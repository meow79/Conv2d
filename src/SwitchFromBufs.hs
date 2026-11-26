module SwitchFromBufs where

import Clash.Prelude

{-
 Module selects kSize of the (kSize + 1) buffers, that currently output row
 data and forwards their outputs to the Conv1d modules.
 (At this time new row is written to the remaining buffer,
 this is controlled by the SwitchToBufs module).
-}

switchFromBufs ::
    (KnownNat kSize, HiddenClockResetEnable dom) =>
    -- Input: (vector of outputs of buffers, last_i)
    Signal dom (Vec (kSize + 1) (Bool, Unsigned 8), Bool) ->
    -- Output: vector of inputs for Conv1d modules
    Signal dom (Vec kSize (Bool, Unsigned 8, Bool))
switchFromBufs = mealy switchFromBufsStep 0

switchFromBufsStep ::
    (KnownNat kSize) =>
    (Index (kSize + 1)) ->
    (Vec (kSize + 1) (Bool, Unsigned 8), Bool) ->
    (Index (kSize + 1), Vec kSize (Bool, Unsigned 8, Bool))
switchFromBufsStep startSlicePtr (bufsOuts, last_i) =
    (nextPtr, inpsForConv1ds)
  where
    (dataValid, _) = bufsOuts !! startSlicePtr
    nextPtr =
        if dataValid && last_i
            then satAdd SatWrap startSlicePtr 1
            else startSlicePtr
    inpsForConv1ds = map (\(validO, dataO) -> (validO, dataO, last_i)) dataFromBufs

    -- cyclic slice of length kSize of bufsOuts
    -- starting at index startSlicePtr
    dataFromBufs = init $ rotateLeft bufsOuts startSlicePtr

module SwitchToBufs where

import Clash.Prelude

{-
 Module sends data_i to the selected buffer with write_en = True.
 To the other buffers it sends write_en = False, read_en = startConv.
 startConv is False at startup and becomes True after kSize
 buffers have been filled.
 Input flag valid_i is simply forwarded to all buffers.
-}

data SwitchToBufsState kSize = SwitchToBufsState
    { writeBufPtr :: Index (kSize + 1)
    , startConv :: Bool
    }
    deriving (NFDataX, Generic)

switchToBufs ::
    (KnownNat kSize, HiddenClockResetEnable dom) =>
    -- Input: (valid_i, data_i, last_i)
    Signal dom (Bool, Unsigned 8, Bool) ->
    -- Output: vector of inputs for line buffers.
    -- i-th element of vector is input (valid_i, data_i, write_en, read_en)
    -- for i-th line buffer
    Signal dom (Vec (kSize + 1) (Bool, Unsigned 8, Bool, Bool))
switchToBufs = mealy switchToBufsStep initState
  where
    initState =
        SwitchToBufsState
            { writeBufPtr = 0
            , startConv = False
            }

switchToBufsStep ::
    (KnownNat kSize) =>
    SwitchToBufsState kSize ->
    (Bool, Unsigned 8, Bool) ->
    ( SwitchToBufsState kSize
    , Vec (kSize + 1) (Bool, Unsigned 8, Bool, Bool)
    )
switchToBufsStep curState (valid_i, data_i, last_i) =
    (nextState, inpToBufs)
  where
    nextState = case valid_i of
        False -> curState
        True ->
            SwitchToBufsState
                { writeBufPtr =
                    if last_i
                        then satAdd SatWrap curWriteBufPtr 1
                        else curWriteBufPtr
                , startConv =
                    curStartConv || (last_i && curWriteBufPtr == maxBound - 1)
                }
    inpToBufs =
        replace curWriteBufPtr (valid_i, data_i, True, False)
            $ repeat (valid_i, 0, False, curStartConv)

    SwitchToBufsState curWriteBufPtr curStartConv = curState

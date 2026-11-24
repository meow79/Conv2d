module SwitchToBufs where

import Clash.Prelude

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
    nextState =
        SwitchToBufsState
            { writeBufPtr =
                if last_i
                    then satAdd SatWrap curWriteBufPtr 1
                    else curWriteBufPtr
            , startConv = curStartConv || (last_i && curWriteBufPtr == maxBound - 1)
            }
    inpToBufs =
        replace curWriteBufPtr (valid_i, data_i, True, False)
            $ repeat (valid_i, 0, False, curStartConv)

    SwitchToBufsState curWriteBufPtr curStartConv = curState

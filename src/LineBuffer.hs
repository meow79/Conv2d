module LineBuffer where

import Clash.Prelude
import Data.Proxy (Proxy)

{-
 lineBuffer is a circular buffer of length bufWidth.

 At any given time, it can either be written to or read from.
 Reads from and writes to the buffer occur at the address stored in the
 buffer's internal state. Initially this address is 0 and is incremented by 1
 on each read or write.

 The buffer has 4 inputs: valid_i, data_i, write_en, read_en
 And 2 outputs: valid_o, data_o

 If in the current clock cycle the flags valid_i and write_en are active,
 the value data_i is written to the buffer. In this case, valid_o is set
 to False in the next clock cycle.
 Otherwise, if the flags valid_i and read_en are active, then in the next
 clock cycle the value at the current address in the buffer is provided on
 output data_o with valid_o = True. In this case, data_i is ignored.
 That is, write has priority over read.
 In all other cases, valid_o = False in the next clock cycle.
-}

lineBuffer ::
    forall bufWidth dom.
    (KnownNat bufWidth, HiddenClockResetEnable dom) =>
    (Proxy bufWidth) ->
    -- Input: (valid_i, data_i, write_en, read_en)
    Signal dom (Bool, Unsigned 8, Bool, Bool) ->
    -- Output: (valid_o, data_o)
    Signal dom (Bool, Unsigned 8)
lineBuffer _ inp = bundle (valid_o, data_o)
  where
    (valid_o, rdAddr, wrM) =
        unbundle
            $ mealy
                lineBufStep
                (0 :: Index bufWidth, False)
                inp
    data_o = blockRam (repeat 0 :: Vec bufWidth (Unsigned 8)) rdAddr wrM

lineBufStep ::
    (KnownNat bufWidth) =>
    -- Current state: (data pointer, valid_o (for current output))
    (Index bufWidth, Bool) ->
    -- Input: (valid_i, data_i, write_en, read_en)
    (Bool, Unsigned 8, Bool, Bool) ->
    ( -- Next state: (next data pointer, valid_o for next output)
      (Index bufWidth, Bool)
    , -- Output:
      ( Bool -- valid_o from current state
      , Index bufWidth -- read address
      , Maybe (Index bufWidth, Unsigned 8) -- (write address, value to write)
      )
    )
lineBufStep (dataPtr, valid_o) (valid_i, data_i, write_en, read_en)
    | not valid_i = ((dataPtr, False), (valid_o, undefined, Nothing))
    | write_en =
        let
            nextPtr = incPtr dataPtr
            nextValid = False
            rd_addr = undefined
            wr_maybe = Just (dataPtr, data_i)
         in
            ((nextPtr, nextValid), (valid_o, rd_addr, wr_maybe))
    | read_en =
        let
            nextPtr = incPtr dataPtr
            nextValid = True
            rd_addr = dataPtr
            wr_maybe = Nothing
         in
            ((nextPtr, nextValid), (valid_o, rd_addr, wr_maybe))
    | otherwise = ((dataPtr, False), (valid_o, undefined, Nothing))
  where
    incPtr ptr = if ptr == maxBound then 0 else ptr + 1

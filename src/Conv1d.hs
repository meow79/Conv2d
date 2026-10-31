module Conv1d where

import Clash.Prelude


{-
 This module implements vector convolution with an odd fixed-size kernel.
 The resulting vector has the same size as the input vector; its first and
 last floor(kernelSize / 2) elements are zero.

 Input vector elements arrive one per clock cycle. Convolution result elements
 are also output one per clock cycle.
 Kernel elements arrive all at once and must be held constant throughout
 the convolution.

 Input vector elements are integers from 0 to 255.
 Kernel elements are integers from -128 to 127.
 Result elements are integers from (-128) * 255 * kernelSize
 to 127 * 255 * kernelSize
-}

-- START_PAD and END_PAD carry the count of remaining padding zeros to be output
data ConvStage n = INITIAL n | START_PAD n | CONV | END_PAD n
    deriving (NFDataX, Generic)

-- kSize --- size of kernel
data Conv1dState (kSize :: Nat) = Conv1dState
    { stage :: ConvStage (Index (kSize `Div` 2 + 1))
    , inputBuf :: Vec kSize (Unsigned 8)
    , weights :: Vec kSize (Signed 8)
    }
    deriving (NFDataX, Generic)

{-
 Multiplying a weight (Signed 8) by a pixel (Unsigned 8) gives a result
 that fits in Signed 16.
 Since convolution sums kSize such products, the final result fits in
 Signed (16 + ceil(log(2, kSize)))
-}
type Conv1dResType kSize = Signed (16 + CLog 2 kSize)

conv1dStep ::
    (KnownNat kSize, 1 <= kSize) =>
    Conv1dState kSize ->
    (Bool, Unsigned 8, Bool, Vec kSize (Signed 8)) ->
    (Conv1dState kSize, (Bool, Conv1dResType kSize, Bool))
conv1dStep state (valid_i, data_i, last_i, weights_i)
    | not valid_i = (state, (False, 0, False))
    | otherwise = case state of
        Conv1dState (INITIAL delayCnt) inputBuf _ ->
            (newState, (valid_o, data_o, last_o))
          where
            valid_o = False
            data_o = 0
            last_o = False
            newState =
                Conv1dState
                    { stage = if delayCnt == 1 then START_PAD maxBound else INITIAL (delayCnt - 1)
                    , inputBuf = inputBuf <<+ data_i
                    , weights = weights_i
                    }
        Conv1dState (START_PAD padCnt) inputBuf _ ->
            (newState, (valid_o, data_o, last_o))
          where
            valid_o = True
            data_o = 0
            last_o = False
            newState =
                Conv1dState
                    { stage = if padCnt == 1 then CONV else START_PAD (padCnt - 1)
                    , inputBuf = inputBuf <<+ data_i
                    , weights = weights_i
                    }
        Conv1dState CONV inputBuf weights ->
            (newState, (valid_o, data_o, last_o))
          where
            valid_o = True
            data_o = scalarProduct weights (inputBuf <<+ data_i)
            last_o = False
            newState =
                Conv1dState
                    { stage = if last_i then END_PAD maxBound else CONV
                    , inputBuf = inputBuf <<+ data_i
                    , weights = weights_i
                    }
        Conv1dState (END_PAD padCnt) inputBuf _ ->
            (newState, (valid_o, data_o, last_o))
          where
            valid_o = True
            data_o = 0
            last_o = (padCnt == 1)
            newState =
                Conv1dState
                    { stage = if padCnt == 1 then START_PAD maxBound else END_PAD (padCnt - 1)
                    , inputBuf = inputBuf <<+ data_i
                    , weights = weights_i
                    }

mealyConv1d ::
    (KnownNat kSize, 1 <= kSize, HiddenClockResetEnable dom) =>
    Signal dom (Bool, Unsigned 8, Bool, Vec kSize (Signed 8)) ->
    Signal dom (Bool, Conv1dResType kSize, Bool)
mealyConv1d = mealy conv1dStep initialState
  where
    initialState =
        Conv1dState
            { stage = INITIAL maxBound
            , inputBuf = repeat 0
            , weights = repeat 0
            }

scalarProduct ::
    forall n.
    (KnownNat n, 1 <= n) =>
    Vec n (Signed 8) ->
    Vec n (Unsigned 8) ->
    Conv1dResType n
scalarProduct weights xs = res
  where
    weightsExtended = map (numConvert :: (Signed 8 -> Signed 16)) weights
    xsExtended = map (numConvert :: (Unsigned 8 -> Signed 16)) xs

    products = zipWith (*) weightsExtended xsExtended
    res = foldl (+) 0 $ map (numConvert :: (Signed 16 -> Conv1dResType n)) products

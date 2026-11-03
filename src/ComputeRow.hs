module ComputeRow where

import Clash.Prelude
import Conv1d (Conv1dResNat, Conv1dResType)

{-
 This module computes one row of 2D convolution result from kernelSize
 1D convolution results and shift coefficient shift_i.

 Module works in streaming mode: every clock cycle it takes one element
 from each 1D convolution result (result of 1D conv is vector); and these elements
 must be at positions with same indices in 1D convolution results.
 Module sums these elements, divides sum by (2 ^ shift_i), clamps result
 to range 0--255 and outputs it. Thus, each cycle produces one element of the
 row being computed.
-}

type Conv1dResultsSumType kSize = Signed (Conv1dResNat kSize + CLog 2 kSize)

computeRow ::
    forall kSize.
    (KnownNat kSize, 3 <= kSize) =>
    (Bool, Vec kSize (Conv1dResType kSize), Bool, Unsigned 4) ->
    (Bool, Unsigned 8, Bool)
computeRow (valid_i, data_i, last_i, shift_i) = (valid_o, data_o, last_o)
  where
    valid_o = valid_i
    last_o = last_i

    conv1dResultsSum :: Conv1dResultsSumType kSize
    conv1dResultsSum =
        fold @(kSize - 1) (+)
            $ map (numConvert :: Conv1dResType kSize -> Conv1dResultsSumType kSize) data_i
    shiftedSum = conv1dResultsSum `shiftR` (numConvert shift_i)
    data_o
        | shiftedSum > 255 = 255
        | shiftedSum < 0 = 0
        | otherwise = resize (bitCoerce shiftedSum)

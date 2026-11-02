module ComputeRow where

import Clash.Prelude
import Conv1d (Conv1dResNat, Conv1dResType)

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

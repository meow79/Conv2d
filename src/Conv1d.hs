module Conv1d where

import Clash.Prelude

{-
 Модуль содержит реализацию свёртки вектора ядром нечётного фиксированного
 размера. Получившийся в результате свёртки вектор того же размера, что и исходный,
 причём первые и последние floor(k / 2) (k - размер ядра) его элементов нулевые.

 Элементы входного вектора поступают по одному за такт. Элементы результата свёртки
 выдаются на выход также один в такт.
 Элементы ядра на вход поступают все одновременно и должны держаться
 постоянными всё время, пока осуществляется свёртка.

 Элементы входного вектора --- целые числа от 0 до 255.
 Элементы ядра --- целые числа от -128 до 127.
 Элементы результата --- целые числа от (-128) * 255 * kernelSize
 до 127 * 255 * kernelSize
-}

-- START_PAD и END_PAD хранят информацию о том,
-- сколько ещё padding нулей нужно выдать
data ConvStage n = INITIAL n | START_PAD n | CONV | END_PAD n
    deriving (NFDataX, Generic)

-- kSize --- размер ядра
data Conv1dState (kSize :: Nat) = Conv1dState
    { stage :: ConvStage (Index (kSize `Div` 2 + 1))
    , inputBuf :: Vec kSize (Unsigned 8)
    , weights :: Vec kSize (Signed 8)
    }
    deriving (NFDataX, Generic)

{-
 Результат умножения веса (Signed 8) на пиксель (Unsigned 8)
 поместится в Signed 16.
 Так как при свёртке складываем n результатов перемножения, итоговый
 результат поместится в Signed (16 + ceil(log(2, n)))
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

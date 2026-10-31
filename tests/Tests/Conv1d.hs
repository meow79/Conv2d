module Tests.Conv1d where

import qualified Clash.Prelude as C
import qualified Clash.Sized.Vector as Vec
import Conv1d (mealyConv1d)
import Data.Maybe (fromJust)
import Data.Proxy
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog
import Test.Tasty.TH
import Prelude

prop_testConv1dWithKernelOfSize3 :: H.Property
prop_testConv1dWithKernelOfSize3 = testConv1dWithKSize (Proxy :: Proxy 3)

prop_testConv1dWithKernelOfSize5 :: H.Property
prop_testConv1dWithKernelOfSize5 = testConv1dWithKSize (Proxy :: Proxy 5)

prop_testConv1dWithKernelOfSize7 :: H.Property
prop_testConv1dWithKernelOfSize7 = testConv1dWithKSize (Proxy :: Proxy 7)

prop_testConv1dWithKernelOfSize9 :: H.Property
prop_testConv1dWithKernelOfSize9 = testConv1dWithKSize (Proxy :: Proxy 9)

testConv1dWithKSize :: forall kSize. (C.KnownNat kSize, 3 C.<= kSize) => Proxy kSize -> H.Property
testConv1dWithKSize proxy = H.property $ do
    let kernelSize = C.snatToNum (C.snatProxy proxy) :: Int
    (dutInput, payloadsWithWeights) <- H.forAll $ genDataForMultipleConvs kernelSize
    let
        expected = modelConv1d (dutInput, payloadsWithWeights)

        dutInputClashTypes = map (convertInputToClashTypes @kSize) dutInput
        dutRes = C.simulateN @C.System (length dutInputClashTypes) mealyConv1d dutInputClashTypes
        actual = map (\(validO, dataO, lastO) -> (validO, fromJust $ C.maybeNumConvert dataO :: Int, lastO)) dutRes
    expected H.=== actual

modelConv1d :: ([(Bool, Int, Bool, [Int])], [([Int], [Int])]) -> [(Bool, Int, Bool)]
modelConv1d (dutInput, payloadsWithWeights) =
    wrapResults (kSize `div` 2) valids convResults
  where
    convResults = map (\(payload, weights) -> conv1d weights payload) payloadsWithWeights
    valids = map (\(validI, _, _, _) -> validI) dutInput
    kSize = length $ snd $ head payloadsWithWeights

    wrapResults :: Int -> [Bool] -> [[Int]] -> [(Bool, Int, Bool)]
    wrapResults startDelay (curValid : nextValids) results@(curResult : nextResults) = case curValid of
        False -> (False, 0, False) : wrapResults startDelay nextValids results
        True ->
            if startDelay > 0
                then (False, 0, False) : wrapResults (startDelay - 1) nextValids results
                else case curResult of
                    [] -> error "Result of convolution is empty"
                    [x] -> (True, x, True) : wrapResults 0 nextValids nextResults
                    (x : xs) -> (True, x, False) : wrapResults 0 nextValids (xs : nextResults)
    wrapResults _ [] _ = []
    wrapResults _ _ [] = error "Length of valid input for DUT is greater than sum of lengths of all convolution results"

conv1d :: [Int] -> [Int] -> [Int]
conv1d weights inpData =
    replicate padSize 0 ++ helper weights inpData ++ replicate padSize 0
  where
    helper ws xs
        | length ws > length xs = []
        | otherwise =
            let
                curSlice = take (length ws) xs
                scalProd as bs = foldl (+) 0 $ zipWith (*) as bs
             in
                scalProd ws curSlice : helper ws (tail xs)
    padSize = length weights `div` 2

-- returns pair: (input for clash module, [(payload for one conv, weights for one conv)])
genDataForMultipleConvs :: Int -> H.Gen ([(Bool, Int, Bool, [Int])], [([Int], [Int])])
genDataForMultipleConvs kSize = do
    oneDataStreamPayloadLen <- Gen.int $ Range.constant kSize (kSize * 5)
    streamsCnt <- Gen.int $ Range.constant 1 10

    pairsForConv <- Gen.list (Range.singleton streamsCnt) $ genDataFor1Conv kSize oneDataStreamPayloadLen
    let
        dutInput = concatMap fst pairsForConv
        payloadsWithWeights = map (\(_, (payload, weights)) -> (payload, weights)) pairsForConv
    return (dutInput, payloadsWithWeights)

-- returns pair: (input for clash module, (data for convolution, weights))
genDataFor1Conv :: Int -> Int -> H.Gen ([(Bool, Int, Bool, [Int])], ([Int], [Int]))
genDataFor1Conv kSize payloadLen = do
    payload <- Gen.list (Range.singleton payloadLen) $ Gen.int $ Range.constant 0 255
    weights <- Gen.list (Range.singleton kSize) $ Gen.int $ Range.constant (-128) 127

    streamLen <- Gen.int $ Range.constant payloadLen (payloadLen * 2)
    validI <- Gen.shuffle $ replicate payloadLen True ++ replicate (streamLen - payloadLen) False

    return (buildDutInput validI payload weights, (payload, weights))
  where
    buildDutInput validI payload weights = case validI of
        [] -> []
        False : xs ->
            (False, 0, False, weights) : buildDutInput xs payload weights
        True : xs ->
            (True, head payload, length payload == 1, weights) : buildDutInput xs (tail payload) weights

convertInputToClashTypes ::
    (C.KnownNat kSize, 3 C.<= kSize) =>
    (Bool, Int, Bool, [Int]) ->
    (Bool, C.Unsigned 8, Bool, Vec.Vec kSize (C.Signed 8))
convertInputToClashTypes (validI, dataI, lastI, kernel) =
    (validI, intToU8 dataI, lastI, Vec.unsafeFromList $ map intToS8 kernel)
  where
    intToU8 = fromJust . (C.maybeNumConvert :: Int -> Maybe (C.Unsigned 8))
    intToS8 = fromJust . (C.maybeNumConvert :: Int -> Maybe (C.Signed 8))

accumTests :: TestTree
accumTests = $(testGroupGenerator)

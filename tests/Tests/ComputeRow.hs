module Tests.ComputeRow where

import qualified Clash.Prelude as C
import qualified Clash.Sized.Vector as Vec
import ComputeRow (computeRow)
import Conv1d (Conv1dResType)
import Data.Maybe (fromJust)
import Data.Proxy (Proxy (Proxy))
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty (TestTree)
import Test.Tasty.Hedgehog (testProperty)
import Test.Tasty.TH (testGroupGenerator)
import Prelude

prop_testComputeRowWithKernelOfSize3 :: H.Property
prop_testComputeRowWithKernelOfSize3 = testComputeRowWithKSize (Proxy :: Proxy 3)

prop_testComputeRowWithKernelOfSize5 :: H.Property
prop_testComputeRowWithKernelOfSize5 = testComputeRowWithKSize (Proxy :: Proxy 5)

prop_testComputeRowWithKernelOfSize7 :: H.Property
prop_testComputeRowWithKernelOfSize7 = testComputeRowWithKSize (Proxy :: Proxy 7)

prop_testComputeRowWithKernelOfSize9 :: H.Property
prop_testComputeRowWithKernelOfSize9 = testComputeRowWithKSize (Proxy :: Proxy 9)

testComputeRowWithKSize ::
    forall kSize.
    (C.KnownNat kSize, 3 C.<= kSize) => Proxy kSize -> H.Property
testComputeRowWithKSize proxy = H.property $ do
    let
        kernelSize = C.snatToNum $ C.snatProxy proxy :: Int
        inpElemMinBound = -128 * 255 * kernelSize
        inpElemMaxBound = 127 * 255 * kernelSize

    validI <- H.forAll Gen.bool
    dataI <-
        H.forAll $
            Gen.list (Range.singleton kernelSize) $
                Gen.int (Range.constant inpElemMinBound inpElemMaxBound)
    lastI <- H.forAll Gen.bool
    shiftI <- H.forAll $ Gen.int $ Range.constant 0 15

    let
        tempSum = foldl1 (+) dataI `div` 2 ^ shiftI
        truncatedSum
            | tempSum > 255 = 255
            | tempSum < 0 = 0
            | otherwise = tempSum
        expected = (validI, truncatedSum, lastI)

        inputForDut = buildInputForDut @kSize validI dataI lastI shiftI
        (validO, dataO, lastO) = computeRow inputForDut
        actual = (validO, fromJust $ C.maybeNumConvert dataO :: Int, lastO)

    expected H.=== actual

buildInputForDut ::
    (C.KnownNat kSize, 3 C.<= kSize) =>
    Bool ->
    [Int] ->
    Bool ->
    Int ->
    (Bool, C.Vec kSize (Conv1dResType kSize), Bool, C.Unsigned 4)
buildInputForDut validI dataI lastI shiftI =
    ( validI
    , Vec.unsafeFromList (map (fromJust . C.maybeNumConvert) dataI)
    , lastI
    , fromJust $ C.maybeNumConvert shiftI
    )

accumTests :: TestTree
accumTests = $(testGroupGenerator)

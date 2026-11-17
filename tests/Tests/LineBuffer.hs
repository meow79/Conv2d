module Tests.LineBuffer where

import qualified Clash.Prelude as C
import Data.List (zip4)
import Data.Maybe (fromJust)
import Data.Proxy (Proxy (..))
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import LineBuffer (lineBuffer)
import Test.Tasty (TestTree)
import Test.Tasty.Hedgehog (testProperty)
import Test.Tasty.TH (testGroupGenerator)
import Prelude

prop_oneWriteThenNReadsBufWidth4 :: H.Property
prop_oneWriteThenNReadsBufWidth4 =
    testLineBufWith (Proxy :: Proxy 4) $ genInpForNCycles 1

prop_oneWriteThenNReadsBufWidth10 :: H.Property
prop_oneWriteThenNReadsBufWidth10 =
    testLineBufWith (Proxy :: Proxy 10) $ genInpForNCycles 1

prop_multipleWriteReadCyclesBufWidth4 :: H.Property
prop_multipleWriteReadCyclesBufWidth4 =
    testLineBufWith (Proxy :: Proxy 4) $
        ( \bW -> do
            cyclesCnt <- Gen.int (Range.constant 2 10)
            genInpForNCycles cyclesCnt bW
        )

prop_multipleWriteReadCyclesBufWidth10 :: H.Property
prop_multipleWriteReadCyclesBufWidth10 =
    testLineBufWith (Proxy :: Proxy 10) $
        ( \bW -> do
            cyclesCnt <- Gen.int (Range.constant 2 10)
            genInpForNCycles cyclesCnt bW
        )

testLineBufWith ::
    (C.KnownNat bufWidth) =>
    Proxy bufWidth ->
    (Int -> H.Gen [(Bool, Int, Bool, Bool)]) ->
    H.Property
testLineBufWith proxyBufW genFun = H.property $ do
    let bufWidth = C.snatToNum $ C.snatProxy proxyBufW

    inp <- H.forAll $ genFun bufWidth

    let
        expected = modelLineBuf proxyBufW inp

        dutInp = convertInputToClashTypes inp
        dutRes = C.simulateN @C.System (length inp) (lineBuffer proxyBufW) dutInp
        actual =
            convertUndefToNothing $
                map (\(validO, dataO) -> (validO, C.numConvert dataO)) dutRes

    expected H.=== actual
  where
    convertUndefToNothing = map $ \(validO, dataO) -> (validO, C.maybeIsX dataO)

modelLineBuf ::
    forall bufWidth.
    (C.KnownNat bufWidth) =>
    (Proxy bufWidth) -> [(Bool, Int, Bool, Bool)] -> [(Bool, Maybe Int)]
modelLineBuf proxyBufW inp =
    helper (False, Nothing) 0 (C.repeat 0 :: C.Vec bufWidth Int) inp
  where
    helper (validD, outD) ptr mem ((validI, dataI, wrEn, rdEn) : xs)
        | not validI =
            (validD, outD) : helper (False, Nothing) ptr mem xs
        | wrEn =
            (validD, outD)
                : helper
                    (False, Nothing)
                    (incPtr ptr)
                    (C.replace ptr dataI mem)
                    xs
        | rdEn =
            (validD, outD)
                : helper
                    (True, Just (mem C.!! ptr))
                    (incPtr ptr)
                    mem
                    xs
        | otherwise =
            (validD, outD) : helper (False, Nothing) ptr mem xs
    helper _ _ _ [] = []

    incPtr ptr = if ptr == maxBnd then 0 else ptr + 1
      where
        maxBnd = C.snatToInteger (C.snatProxy proxyBufW) - 1

genInpForNCycles :: Int -> Int -> H.Gen [(Bool, Int, Bool, Bool)]
genInpForNCycles n bufWidth = do
    readCnt <- Gen.int $ Range.constant 1 10
    inp <- Gen.list (Range.singleton n) $ genInpFor1Cycle bufWidth readCnt
    return $ concat inp
  where
    genInpFor1Cycle :: Int -> Int -> H.Gen [(Bool, Int, Bool, Bool)]
    genInpFor1Cycle bufWidth rdCnt = do
        wrInp <- genInputToWrite bufWidth
        rdInp <- genInputToReadNTimes bufWidth rdCnt
        return (wrInp ++ rdInp)

genInputToWrite :: Int -> H.Gen [(Bool, Int, Bool, Bool)]
genInputToWrite bufWidth = do
    payload <-
        Gen.list (Range.singleton bufWidth) $
            Gen.int (Range.constant 0 255)
    let
        validI = replicate bufWidth True
        wrEn = replicate bufWidth True
        rdEn = replicate bufWidth False

    inpLen <- Gen.int $ Range.constant bufWidth (bufWidth * 2)
    input <-
        Gen.shuffle $
            let
                pad :: forall a. a -> [a] -> [a]
                pad = padWith (inpLen - bufWidth)
             in
                zip4
                    (pad False validI)
                    (pad 0 payload)
                    (pad False wrEn)
                    (pad False rdEn)

    return input

genInputToReadNTimes :: Int -> Int -> H.Gen [(Bool, Int, Bool, Bool)]
genInputToReadNTimes bufWidth n = do
    let
        inpWithTrueValidLen = bufWidth * n
        validI = replicate inpWithTrueValidLen True
        wrEn = replicate inpWithTrueValidLen False
        rdEn = replicate inpWithTrueValidLen True

    -- dataI can be anything. It should not affect the output.
    dataI <-
        Gen.list (Range.singleton inpWithTrueValidLen) $
            Gen.int (Range.constant 0 255)

    inpLen <-
        Gen.int $ Range.constant (inpWithTrueValidLen) (inpWithTrueValidLen * 2)
    input <-
        Gen.shuffle $
            let
                pad :: forall a. a -> [a] -> [a]
                pad = padWith (inpLen - inpWithTrueValidLen)
             in
                zip4
                    (pad False validI)
                    (pad 0 dataI)
                    (pad False wrEn)
                    (pad False rdEn)

    return input

padWith :: Int -> a -> [a] -> [a]
padWith n val xs = xs ++ replicate n val

convertInputToClashTypes ::
    [(Bool, Int, Bool, Bool)] ->
    [(Bool, C.Unsigned 8, Bool, Bool)]
convertInputToClashTypes =
    map
        ( \(validI, dataI, wrEn, rdEn) ->
            (validI, fromJust $ C.maybeNumConvert dataI, wrEn, rdEn)
        )

accumTests :: TestTree
accumTests = $(testGroupGenerator)

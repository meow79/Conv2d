import Prelude

import Test.Tasty

import qualified Tests.ComputeRow
import qualified Tests.Conv1d
import qualified Tests.LineBuffer

main :: IO ()
main =
    defaultMain $
        testGroup
            "."
            [ Tests.Conv1d.accumTests
            , Tests.ComputeRow.accumTests
            , Tests.LineBuffer.accumTests
            ]

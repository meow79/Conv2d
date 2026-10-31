import Prelude

import Test.Tasty

import qualified Tests.Conv1d

main :: IO ()
main =
    defaultMain $
        testGroup
            "."
            [ Tests.Conv1d.accumTests
            ]

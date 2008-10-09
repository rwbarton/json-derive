{-# LANGUAGE TemplateHaskell #-}

import Text.JSON
import Data.DeriveTH
import Text.JSON.Derive
import Control.Monad

data Foo = Foo1 { x :: Int, s :: String } | Foo2 Double
data Bar = Bar { d :: Double }
data Baz = Baz Int Int
data Unit = Unit { }

$(derive makeJSON ''Foo)
$(derive makeJSON ''Bar)
$(derive makeJSON ''Baz)
$(derive makeJSON ''Unit)

tests :: [(JSValue, String)]
tests = [(showJSON $ Foo1 { x = 5, s = "hello" },
          "[\"Foo1\",{\"x\":5,\"s\":\"hello\"}]"),
         (showJSON $ Foo2 29,
          "[\"Foo2\",29]"),
         (showJSON $ Bar { d = 7.5 },
          "{\"d\":7.5}"),
         (showJSON $ Baz 10 10,
          "[10,10]"),
         (showJSON $ Unit,
          "[]")
        ]

check :: (JSValue, String) -> String
check (v, c) = if e == c then "" else "Test failure, expected " ++ c ++ " but got " ++ e ++ "\n"
  where e = encode v

main :: IO ()
main = putStr allResults >> when (null allResults) (putStrLn "All tests passed.")
  where allResults = concatMap check tests

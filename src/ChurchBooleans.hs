{-# LANGUAGE RankNTypes #-}
module ChurchBooleans
    ( demo
    ) where

type Boolean = forall a. (a -> a -> a)

true :: Boolean
true x y = x

false :: Boolean
false x y = y

if' :: Boolean -> a -> a -> a
if' = id

not' :: Boolean -> Boolean
not' = flip

and' :: Boolean -> Boolean -> Boolean
and' a b = a b false

or' :: Boolean -> Boolean -> Boolean
or' a b = a true b

xor' :: Boolean -> Boolean -> Boolean
xor' a b = a (not' b) b

checkTruth :: String -> (Boolean -> Boolean -> Boolean) -> Boolean -> Boolean -> Boolean -> IO ()
checkTruth name op a b r = do
  putStr name
  putStr ": "
  putTruth a
  putTruth b
  putStrLn $ if' (xor' (op a b) r) "FAIL" "ok"
  where
    putTruth x = putStr $ x "true " "false "

demo :: IO ()
demo = do
  putStrLn $ if' true ("ok") ("fail")
  putStrLn $ if' false ("fail") ("ok")

  let checkAnd = checkTruth "and" and'
  checkAnd false false false
  checkAnd false true false
  checkAnd true false false
  checkAnd true true true

  let checkOr = checkTruth "or" or'
  checkOr false false false
  checkOr false true true
  checkOr true false true
  checkOr true true true

  let checkXor = checkTruth "xor" xor'
  checkXor false false false
  checkXor false true true
  checkXor true false true
  checkXor true true false

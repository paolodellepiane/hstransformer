module Main where

import           Data.Aeson
import           Data.Aeson.Types
import           Data.List
import           Data.Maybe
import           Data.Foldable
import           System.FilePath.Glob
import qualified Data.HashMap.Strict           as H

main = do
    paths <- glob "*.json"
    outs  <- mapM readAndTransform paths
    putStrLn $ intercalate "\n" outs


readAndTransform f = do
    d <- decodeFileStrict f
    return $ transform (expectDecoded f d) ""

transform v prefix = case v of
    Object o -> transformObj o (prefix ++ joiner)
    Array  a -> prefix ++ " = " ++ show a
    String s -> prefix ++ " = " ++ show s
    Number n -> prefix ++ " = " ++ show n
    Bool   b -> prefix ++ " = " ++ show b
    where joiner = if prefix == "" then "" else "__"

transformObj :: Object -> String -> String
transformObj o prefix = foldl
    (\a -> \(k, v) -> a ++ (transform v (prefix ++ (show k))) ++ "\n")
    ""
    (H.toList o)

expect err = fromMaybe (error err)
expectDecoded file = expect $ "can't decode " ++ (show file)

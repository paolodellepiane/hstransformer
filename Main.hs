-- #region imports
module Main where

import MyPrelude
import qualified Data.Text                     as Text
import qualified Data.HashMap.Strict           as Hash
-- #endregion

main = do
    paths <- glob "*.json"
    outs  <- mapM readAndTransform paths
    putStrLn $ intercalate "\n" outs

readAndTransform f = do
    d <- decodeFileStrict f
    return $ transform (expectDecoded f d) ""

transform v prefix = case v of
    Object o -> foldr (\(k, v) -> transform' k v) "" $ Hash.toList o
    Array  a -> ifoldr transform' "" a
    _        -> display prefix v
  where
    name = prefix ++ if prefix == "" then "" else "__"
    display prefix s = prefix ++ " = " ++ (toValue s)
    transform' k v a = a ++ transform v (name ++ toKey k) ++ "\n"

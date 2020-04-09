-- #region imports
module Main where

import           MyPrelude
import qualified Data.Text                     as Text
import qualified Data.HashMap.Strict           as Hash
-- #endregion

main = do
    arg <- cmdArgs myargs
    paths <- glob (pattern arg)
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

data Transformer = Transformer {pattern :: String} deriving (Show, Data, Typeable)
myargs = Transformer { pattern = "**/*.json" &= args &= typ "GLOB PATTERN" }
    &= summary "Transforms .net core settings json to docker env files. v0.1"

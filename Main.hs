-- #region imports
module Main where

import           Data.Aeson
import           Data.Aeson.Types
import           Data.List            ( intercalate )
import           Data.Maybe
import           Data.Foldable
import           Data.Vector          ( ifoldr )
import qualified Data.Text                     as Text
import           System.FilePath.Glob
import qualified Data.HashMap.Strict           as Hash
-- #endregion
-- #region utils
expect err = fromMaybe (error err)
expectDecoded file = expect $ "can't decode " ++ (show file)
replace from to = Text.unpack . Text.replace (Text.pack from) (Text.pack to) . Text.pack
strip what = replace what ""
trim what = filter $ (/=) what
toValue = strip "String " . strip "Bool " . strip "Number " . show
toKey s = (trim '\"' . show) s
(|>) a b = b a
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

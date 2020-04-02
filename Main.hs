module Main where

import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.List                     as L
import           System.FilePath.Glob


main =
    glob "*.json" >>= mapM transform >>= \s -> putStrLn (L.intercalate "\n" s)

transform f = do
    d <- decodeFileStrict f :: IO (Maybe Value)
    return
        (case d of
            Just a -> transform2 a ""
            _      -> "can't decode " ++ (show f)
        )

transform2 v prefix = case v of
    Object o -> foldl (transform3 $ prefix ++ if prefix == "" then "" else "__") "" o
    Array  a -> prefix ++ " = " ++ show a
    String s -> prefix ++ " = " ++ show s
    Number n -> prefix ++ " = " ++ show n
    Bool   b -> prefix ++ " = " ++ show b

transform3 :: String -> String -> Value -> String
transform3 prefix acc v = acc ++ (transform2 v ("\n" ++ prefix ++ ("mah")))

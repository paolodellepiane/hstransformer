module MyPrelude
    ( module MyPrelude
    )
where

import           Data.Aeson                    as MyPrelude
import           Data.Aeson.Types              as MyPrelude
import           Data.Maybe                    as MyPrelude
import           Data.Foldable                 as MyPrelude
import           System.FilePath.Glob          as MyPrelude
import           Data.List                     as List
import           Data.Vector                   as Vector
import qualified Data.Text                     as Text
import qualified Data.HashMap.Strict           as Hash


-- #region utils
ifoldr = Vector.ifoldr
intercalate = List.intercalate
expect err = fromMaybe (error err)
expectDecoded file = expect $ "can't decode " List.++ (show file)
replace from to = Text.unpack . Text.replace (Text.pack from) (Text.pack to) . Text.pack
strip what = replace what ""
trim what = List.filter $ (/=) what

toValue s = strip "String " . strip "Bool " . strip "Number " . show $ s
toKey s = (trim '\"' . show) s
(|>) a b = b a
-- #endregion

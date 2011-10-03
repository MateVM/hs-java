
module Java.META.Types where

import qualified Data.Map as M
import Text.Parsec
import Text.Parsec.String

type Section = M.Map String String
type META = [Section]


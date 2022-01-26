module Parser.ParserUtils where
import           Data.List                      ( intercalate
                                                , partition
                                                )

type AttributeValuePair = (String, Int)

data Coach = Coach
    { determination :: Int
    , discipline    :: Int
    , motivating    :: Int
    , fitness       :: Int
    , attacking     :: Int
    , defending     :: Int
    , tactical      :: Int
    , technical     :: Int
    , mental        :: Int
    , distribution  :: Int
    , handling      :: Int
    , shotStopping  :: Int
    }
    deriving Show

data Error
    = AttributeError [AttributeError]
    | ParseFailed
    | MissingFile
    | FileDoesNotExist String
    deriving Show

data AttributeError
    = MissingAttribute String
    | OutOfRangeValue String Int
    deriving Show

eitherFromMaybe :: a -> Maybe b -> Either a b
eitherFromMaybe a = maybe (Left a) Right

isMissingAttributeError :: AttributeError -> Bool
isMissingAttributeError (MissingAttribute _ ) = True
isMissingAttributeError (OutOfRangeValue _ _) = False

extractAttribute :: AttributeError -> String
extractAttribute (MissingAttribute a ) = a
extractAttribute (OutOfRangeValue a v) = a ++ " (" ++ show v ++ ")"

missingAttrErrorsToStr :: [AttributeError] -> [String]
missingAttrErrorsToStr [] = []
missingAttrErrorsToStr ms =
    [ "The following required attributes are missing: "
          ++ intercalate ", " (map extractAttribute ms)
          ++ "."
    ]

outOfRangeValErrorsToStr :: [AttributeError] -> [String]
outOfRangeValErrorsToStr [] = []
outOfRangeValErrorsToStr os =
    [ "Invalid values given for "
          ++ intercalate ", " (map extractAttribute os)
          ++ ". Please note valid values are 1-20."
    ]

prettifyAttributeErrors :: [AttributeError] -> String
prettifyAttributeErrors es =
    intercalate "\n" $ missingAttrErrorsToStr ms ++ outOfRangeValErrorsToStr os
    where (ms, os) = partition isMissingAttributeError es

prettifyError :: Error -> String
prettifyError err = case err of
    AttributeError es  -> prettifyAttributeErrors es
    ParseFailed        -> "Could not parse file."
    MissingFile -> "Please provide a file you want to run this application on."
    FileDoesNotExist p -> "The file " ++ p ++ " does not seem to exist."

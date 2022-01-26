{-# LANGUAGE OverloadedStrings #-}

module Parser.ParserUtils where
import           Data.List                      ( partition )
import           Data.Text                      ( Text
                                                , intercalate
                                                , pack
                                                )

type AttributeValuePair = (Text, Int)

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
    | FileDoesNotExist Text
    deriving Show

data AttributeError
    = MissingAttribute Text
    | OutOfRangeValue Text Int
    deriving Show

eitherFromMaybe :: a -> Maybe b -> Either a b
eitherFromMaybe a = maybe (Left a) Right

isMissingAttributeError :: AttributeError -> Bool
isMissingAttributeError (MissingAttribute _ ) = True
isMissingAttributeError (OutOfRangeValue _ _) = False

extractAttribute :: AttributeError -> Text
extractAttribute (MissingAttribute a ) = a
extractAttribute (OutOfRangeValue a v) = a <> " (" <> pack (show v) <> ")"

missingAttrErrorsToStr :: [AttributeError] -> [Text]
missingAttrErrorsToStr [] = []
missingAttrErrorsToStr ms =
    [ "The following required attributes are missing: "
          <> intercalate ", " (map extractAttribute ms)
          <> "."
    ]

outOfRangeValErrorsToStr :: [AttributeError] -> [Text]
outOfRangeValErrorsToStr [] = []
outOfRangeValErrorsToStr os =
    [ "Invalid values given for "
          <> intercalate ", " (map extractAttribute os)
          <> ". Please note valid values are 1-20."
    ]

prettifyAttributeErrors :: [AttributeError] -> Text
prettifyAttributeErrors es =
    intercalate "\n" $ missingAttrErrorsToStr ms <> outOfRangeValErrorsToStr os
    where (ms, os) = partition isMissingAttributeError es

prettifyError :: Error -> Text
prettifyError err = case err of
    AttributeError es  -> prettifyAttributeErrors es
    ParseFailed        -> "Could not parse file."
    MissingFile -> "Please provide a file you want to run this application on."
    FileDoesNotExist p -> "The file " <> p <> " does not seem to exist."

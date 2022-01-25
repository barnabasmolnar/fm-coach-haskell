module Parser.ParserUtils where

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
    = MissingAttribute String
    | OutOfRangeValue String Int
    | ParseFailed
    | MissingFile
    | FileDoesNotExist String
    deriving Show

eitherFromMaybe :: a -> Maybe b -> Either a b
eitherFromMaybe a = maybe (Left a) Right

prettifyError :: Error -> String
prettifyError err = case err of
    MissingAttribute a -> "Required attribute " ++ a ++ " is missing."
    OutOfRangeValue a v ->
        show v
            ++ " is an invalid value for attribute "
            ++ a
            ++ ". Valid values are 1-20."
    ParseFailed -> "Could not parse file."
    MissingFile -> "Please provide a file you want to run this application on."
    FileDoesNotExist p -> "The file " ++ p ++ " does not seem to exist."

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

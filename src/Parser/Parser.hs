{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ApplicativeDo #-}

module Parser.Parser where
import           Control.Applicative            ( Alternative(empty) )
import           Control.Monad                  ( guard )
import           Data.Ix                        ( Ix(inRange) )
import           Data.Maybe                     ( fromJust
                                                , isJust
                                                )
import           Data.Validation                ( Validation(..)
                                                , toEither
                                                )
import           Parser.ParserUtils             ( AttributeError(..)
                                                , AttributeValuePair
                                                , Coach(..)
                                                , Error(..)
                                                , eitherFromMaybe
                                                )
import           Text.HTML.Scalpel              ( Scraper
                                                , chroots
                                                , inSerial
                                                , scrapeStringLike
                                                , seekNext
                                                , text
                                                )
import           Text.Read                      ( readMaybe )

lookup' :: String -> [AttributeValuePair] -> Validation [AttributeError] Int
lookup' attribute pairs = case lookup attribute pairs of
    Nothing                    -> Failure [MissingAttribute attribute]
    Just n | inRange (1, 20) n -> Success n
    Just n                     -> Failure [OutOfRangeValue attribute n]

parseToCoach :: [AttributeValuePair] -> Validation [AttributeError] Coach
parseToCoach pairs = do
    determination <- lookup' "Determination" pairs
    discipline    <- lookup' "Level of Discipline" pairs
    motivating    <- lookup' "Motivating" pairs
    fitness       <- lookup' "Fitness" pairs
    attacking     <- lookup' "Attacking" pairs
    defending     <- lookup' "Defending" pairs
    tactical      <- lookup' "Tactical" pairs
    technical     <- lookup' "Technical" pairs
    mental        <- lookup' "Mental" pairs
    distribution  <- lookup' "GK Distribution" pairs
    handling      <- lookup' "GK Handling" pairs
    shotStopping  <- lookup' "GK Shot Stopping" pairs
    pure $ Coach { .. }

scrapeTables :: Scraper String [AttributeValuePair]
scrapeTables = concat . take 3 <$> chroots "table" attrValPairScraper

attrValPairScraper :: Scraper String [AttributeValuePair]
attrValPairScraper = chroots "tr" $ inSerial $ do
    attr <- seekNext $ text "td"
    val  <- seekNext $ text "td"
    let val' = readMaybe val
    guard $ isJust val'
    pure (attr, fromJust val')

makeCoach :: String -> Either Error Coach
makeCoach html = do
    pairs <- scrapeStringLike' html scrapeTables
    case parseToCoach pairs of
        Failure err   -> Left $ AttributeError err
        Success coach -> Right coach

scrapeStringLike' :: String -> Scraper String a -> Either Error a
scrapeStringLike' str scraper =
    eitherFromMaybe ParseFailed $ scrapeStringLike str scraper


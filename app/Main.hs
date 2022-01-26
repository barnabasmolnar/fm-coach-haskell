module Main where

import           Control.Exception              ( tryJust )
import           Data.Text                      ( Text
                                                , pack
                                                , unpack
                                                )
import qualified Data.Text.IO                  as IO
import           Formula                        ( Rating(..)
                                                , coachRatings
                                                )
import           Parser.Parser                  ( makeCoach )
import           Parser.ParserUtils             ( Error(..)
                                                , prettifyError
                                                )
import           System.Environment             ( getArgs )
import           System.IO.Error                ( isDoesNotExistError )
import           Text.Layout.Table              ( def
                                                , numCol
                                                , rowG
                                                , tableString
                                                , titlesH
                                                , unicodeRoundS
                                                )
import           Text.Layout.Table.Spec.RowGroup
                                                ( RowGroup )

readFileSafe :: FilePath -> IO (Either Error Text)
readFileSafe path = tryJust handleError $ IO.readFile path  where
    handleError e | isDoesNotExistError e = Just $ FileDoesNotExist $ pack path
                  | otherwise             = Nothing

getFileContent :: IO (Either Error Text)
getFileContent = do
    args <- getArgs
    case args of
        []    -> pure $ Left MissingFile
        x : _ -> readFileSafe x

main :: IO ()
main = do
    result <- getFileContent
    let ratings = do
            content <- result
            coach   <- makeCoach content
            pure $ coachRatings coach
    case ratings of
        Left  er -> IO.putStrLn $ prettifyError er
        Right rs -> printRatings rs

ratingToRow :: Rating -> RowGroup String
ratingToRow r =
    rowG [unpack $ category r, show $ weightedSum r, show $ stars r]

printRatings :: [Rating] -> IO ()
printRatings ratings = putStrLn $ tableString
    [def, numCol, numCol]
    unicodeRoundS
    (titlesH ["Category", "Weighted Sum", "Star Rating"])
    (map ratingToRow ratings)

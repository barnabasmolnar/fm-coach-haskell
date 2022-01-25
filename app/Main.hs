module Main where

import           Control.Exception              ( tryJust )
import           Formula                        ( Rating
                                                    ( Rating
                                                    , category
                                                    , stars
                                                    , weightedSum
                                                    )
                                                , coachRatings
                                                )
import           Parser.ParserUtils             ( Error(..)
                                                , prettifyError
                                                )
import           Parser.ParserV2                ( makeCoach )
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

readFileSafe :: FilePath -> IO (Either [Error] String)
readFileSafe path = tryJust handleError $ readFile path  where
    handleError e | isDoesNotExistError e = Just [FileDoesNotExist path]
                  | otherwise             = Nothing

getFileContent :: IO (Either [Error] String)
getFileContent = do
    args <- getArgs
    case args of
        []    -> pure $ Left [MissingFile]
        x : _ -> readFileSafe x

main :: IO ()
main = do
    result <- getFileContent
    let ratings = do
            content <- result
            coach   <- makeCoach content
            pure $ coachRatings coach
    case ratings of
        Left  er -> mapM_ (putStrLn . prettifyError) er
        Right rs -> printRatings rs

ratingToRow :: Rating -> RowGroup String
ratingToRow r = rowG [category r, show $ weightedSum r, show $ stars r]

printRatings :: [Rating] -> IO ()
printRatings ratings = putStrLn $ tableString
    [def, numCol, numCol]
    unicodeRoundS
    (titlesH ["Category", "Weighted Sum", "Star Rating"])
    (map ratingToRow ratings)

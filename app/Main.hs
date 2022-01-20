module Main where

import           Control.Exception              ( tryJust )
import           Formula                        ( coachRatings )
import           Parser.ParserUtils             ( Error(..) )
import           Parser.ParserV2                ( makeCoach )
import           System.Environment             ( getArgs )
import           System.IO.Error                ( isDoesNotExistError )

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
        Left  er -> print er
        Right rs -> print rs

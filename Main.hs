{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson 
import Network.HTTP.Base (urlEncode)
import Network.HTTP.Conduit (simpleHttp)
import Data.List (sortBy, groupBy)
import Data.Function (on)
import Data.Map (Map, toList)

import RosettaApi

-- Pretty print a single language
showLanguage :: Int -> Bool -> Language -> IO ()
showLanguage rank tie (Language languageName languageQuantity) = 
    let rankStr = show rank
    in putStrLn $ rankStr ++ "." ++ 
                      replicate (4 - length rankStr) ' ' ++
                      (if tie then " (tie)" else "      ") ++
                      " " ++ drop 9 languageName ++
                      " - " ++ show languageQuantity

-- Pretty print languages with common rank
showRanking :: (Int,  [Language]) -> IO ()
showRanking (ranking, languages) = 
    mapM_ (showLanguage ranking $ length languages > 1) languages

-- Sort and group languages by rank, then pretty print them.
showLanguages :: [Language] -> IO ()
showLanguages allLanguages =
    mapM_ showRanking $ 
          zip [1..] $ 
          groupBy ((==) `on` quantity) $
          sortBy (flip compare `on` quantity) allLanguages

-- Issue query to get a list of Language descriptions
runQuery :: [Language] -> String -> IO ()
runQuery ls query = do
    Just (Report continue langs) <- decode <$> simpleHttp query 
    let accLanguages = ls ++ map snd (toList langs)

    -- If there is a continue string, recusively continue the query.
    case continue of
        Nothing -> showLanguages accLanguages
        Just continueStr -> do
            let continueQueryStr = queryStr ++ "&gcmcontinue=" ++ urlEncode continueStr
            runQuery accLanguages continueQueryStr

main :: IO ()
main = runQuery [] queryStr

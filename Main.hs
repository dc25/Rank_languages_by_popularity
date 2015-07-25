{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson 
import Network.HTTP.Base (urlEncode)
import Network.HTTP.Conduit (simpleHttp)
import Data.List (sortBy, groupBy)
import Data.Function (on)
import Data.Map (Map, toList)

-- Record representing a single language.  
data Language =
    Language { 
        name      :: String,
        quantity  :: Int
    } deriving (Show)

-- Make Language an instance of FromJSON for parsing of query response.
instance FromJSON Language where
    parseJSON (Object p) = do
        categoryInfo <- p .:? "categoryinfo" 

        let quantity = case categoryInfo of
                           Just ob -> ob .: "size"
                           Nothing -> return 0

            name = p .: "title"

        Language <$> name <*> quantity

-- Record representing entire response to query.  
-- Contains collection of languages and optional continuation string.
data Report =
    Report { 
        continue    :: Maybe String,
        languages   :: Map String Language
    } deriving (Show)

-- Make Report an instance of FromJSON for parsing of query response.
instance FromJSON Report where
    parseJSON (Object p) = do
        querycontinue <- p .:? "query-continue"

        let continue 
                = case querycontinue of
                      Just ob -> fmap Just $ 
                                     (ob .: "categorymembers") >>= 
                                     (   .: "gcmcontinue")
                      Nothing -> return Nothing

            languages = (p .: "query") >>= (.: "pages") 

        Report <$> continue <*> languages

-- Mediawiki api style query to send to rosettacode.org
queryStr = "http://rosettacode.org/mw/api.php?" ++ 
           "format=json" ++ 
           "&action=query" ++ 
           "&generator=categorymembers" ++ 
           "&gcmtitle=Category:Programming%20Languages" ++ 
           "&gcmlimit=100" ++ 
           "&prop=categoryinfo" 

-- Issue query to get a list of Language descriptions
runQuery :: String -> IO [Language]
runQuery query = do
    Just report <- decode <$> simpleHttp query 

    let res = map snd $ toList $ languages report 

    -- If there is a continue string, recusively continue the query.
    moreRes <- case continue report of
                      Nothing -> return []
                      Just continueStr -> runQuery $ 
                                              queryStr ++ 
                                              "&gcmcontinue=" ++ 
                                              urlEncode continueStr
    return $ res ++ moreRes

-- Pretty print languages and ranks 
showRanking :: (Int,  [Language]) -> IO ()
showRanking languageGroup = do
    let ranking = fst languageGroup
    let languages = snd languageGroup
    let tie = length languages > 1

    mapM_ (showLanguage ranking tie) languages

    where showLanguage r t lang = 
              let rankStr = show r
              in putStrLn $ rankStr ++ "." ++ 
                            replicate (4 - length rankStr) ' ' ++
                            (if t then " (tie)" else "      ") ++
                            " " ++ drop 9 (name lang) ++
                            " - " ++ show  (quantity lang) 

main :: IO ()
main = do 
    allLanguages <- runQuery queryStr

    mapM_ showRanking $ 
          zip [1..] $ 
          groupBy ((==) `on` quantity) $
          sortBy (flip compare `on` quantity) allLanguages


{-# LANGUAGE OverloadedStrings #-}
module RosettaApi (
    queryStr,
    Language(..),
    Report(..)
) where

import Data.Aeson 
import Data.Map (Map, toList)

-- Mediawiki api style query to send to rosettacode.org
queryStr :: String
queryStr = "http://rosettacode.org/mw/api.php?" ++ 
           "format=json" ++ 
           "&action=query" ++ 
           "&generator=categorymembers" ++ 
           "&gcmtitle=Category:Programming%20Languages" ++ 
           "&gcmlimit=100" ++ 
           "&prop=categoryinfo" 

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


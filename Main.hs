{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson 
import Network.HTTP.Base (urlEncode)
import Network.HTTP.Conduit (simpleHttp)
import Data.List (sortBy)
import Data.Function (on)

import qualified Data.Map as M

data Page =
    Page { 
             title     :: String,
             size      :: Int
         } deriving (Show)

instance FromJSON Page where
    parseJSON (Object p) = do
        categoryInfo <- p .:? "categoryinfo" 
        let size = case categoryInfo of
                       Just ob -> ob .: "size"
                       Nothing -> return 0
            title = p .: "title"
        Page <$> title <*> size

data Languages =
    Languages { 
                gcmcontinue    :: Maybe String,
                pages          :: M.Map String Page
              } deriving (Show)

instance FromJSON Languages where
    parseJSON (Object p) = do
        querycontinue <- p .:? "query-continue"
        let gcmcontinue = case querycontinue of
                              Just ob -> fmap Just $ (ob .: "categorymembers") >>=
                                            (.: "gcmcontinue")
                              Nothing -> return Nothing

            pages      = (p .: "query") >>= (.: "pages") 
        Languages <$> gcmcontinue <*> pages

baseQuery = "http://rosettacode.org/mw/api.php?format=json&action=query&generator=categorymembers&gcmtitle=Category:Programming%20Languages&gcmlimit=500&prop=categoryinfo" 

runQuery :: String -> IO [Page]
runQuery query = do
    Just decoded <- decode <$> simpleHttp query 
    let res = map snd $ M.toList $ pages decoded 
    moreRes <- case gcmcontinue decoded of
                      Nothing -> return []
                      Just cs -> runQuery $ 
                                     baseQuery 
                                         ++ "&gcmcontinue=" 
                                         ++ urlEncode cs
    return $ res ++ moreRes

main :: IO ()
main = do 
    allLanguages <- runQuery baseQuery
    mapM_ showPage $ sortBy (flip compare `on` size) allLanguages
        where showPage pg = putStrLn $ drop 9 (title pg) ++ " " ++ show  (size pg)

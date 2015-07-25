{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson 
import Network.HTTP.Base (urlEncode)
import Network.HTTP.Conduit (simpleHttp)
import Data.List (sortBy)
import Data.Function (on)

import qualified Data.Map as M

data Language =
    Language { 
        name      :: String,
        quantity  :: Int
    } deriving (Show)

instance FromJSON Language where
    parseJSON (Object p) = do
        categoryInfo <- p .:? "categoryinfo" 

        let quantity = case categoryInfo of
                           Just ob -> ob .: "size"
                           Nothing -> return 0

            name = p .: "title"

        Language <$> name <*> quantity

data Report =
    Report { 
        continue    :: Maybe String,
        languages   :: M.Map String Language
    } deriving (Show)

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

queryStr = "http://rosettacode.org/mw/api.php?" ++ 
           "format=json" ++ 
           "&action=query" ++ 
           "&generator=categorymembers" ++ 
           "&gcmtitle=Category:Programming%20Languages" ++ 
           "&gcmlimit=500&prop=categoryinfo" 

runQuery :: String -> IO [Language]
runQuery query = do
    Just report <- decode <$> simpleHttp query 

    let res = map snd $ M.toList $ languages report 

    moreRes <- case continue report of
                      Nothing -> return []
                      Just continueStr -> runQuery $ 
                                              queryStr ++ 
                                              "&gcmcontinue=" ++ 
                                              urlEncode continueStr
    return $ res ++ moreRes

main :: IO ()
main = do 
    allLanguages <- runQuery queryStr

    mapM_ showPage $ 
          zip [1..] $ 
          sortBy (flip compare `on` quantity) allLanguages

        where showPage pg 
                  = putStrLn $ 
                        show (fst pg) ++ 
                        " " ++ drop 9 (name $ snd pg) ++ 
                        " " ++ show  (quantity $ snd pg)

{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson 
import Network.HTTP.Base (urlEncode)
import Network.HTTP.Conduit (simpleHttp)
import Data.List (sortBy, groupBy)
import Data.Function (on)
import Data.Map (Map, toList)
import Data.ByteString.Lazy (ByteString)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as TE


import GHCJS.DOM (webViewGetDomDocument, runWebGUI, WebView, currentWindow)
import GHCJS.DOM.Document (documentGetBody, documentCreateElement)
import GHCJS.DOM.HTMLScriptElement (castToHTMLScriptElement, htmlScriptElementSetSrc)
import GHCJS.DOM.Node (nodeAppendChild)
import GHCJS.Foreign (syncCallback1, ForeignRetention(NeverRetain), fromJSString)
import GHCJS.Types (JSFun)

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

-- Mediawiki api style query to send to rosettacode.org
queryStr :: String
queryStr = "http://rosettacode.org/mw/api.php?" ++ 
           "format=json" ++ 
           "&action=query" ++ 
           "&generator=categorymembers" ++ 
           "&gcmtitle=Category:Programming%20Languages" ++ 
           "&gcmlimit=100" ++ 
           "&prop=categoryinfo" ++
           "&callback=cb" 

respondToQuery :: [Language] -> T.Text -> IO ()
respondToQuery ls response = do
    let Just (Report continue langs) = decode  (TE.encodeUtf8 response)
    let accLanguages = ls ++ map snd (toList langs)

    -- If there is a continue string, recusively continue the query.
    case continue of
        Nothing -> showLanguages accLanguages

        Just continueStr -> do
            let continueQueryStr = queryStr ++ 
                   "&gcmcontinue=" ++ urlEncode continueStr
            runQuery accLanguages continueQueryStr

foreign import javascript unsafe "cb_ = $1"
    js_set_cb :: JSFun a -> IO ()

setQueryResponseCallback :: (T.Text -> IO ()) -> IO ()
setQueryResponseCallback cb = do
    cb <- syncCallback1 NeverRetain False (cb.fromJSString)
    js_set_cb cb

-- Issue query to get a list of Language descriptions
runQuery :: [Language] -> String -> IO ()
runQuery ls query = do
    setQueryResponseCallback $ respondToQuery ls
    Just webView <- currentWindow
    Just doc <- webViewGetDomDocument webView
    Just body <- documentGetBody doc
    Just newScript <- fmap castToHTMLScriptElement <$> documentCreateElement doc ("script" :: String)
    htmlScriptElementSetSrc newScript query
    nodeAppendChild body (Just newScript)
    return ()

main :: IO ()
main = runQuery [] queryStr

{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson (decode)
import Network.HTTP.Base (urlEncode)
import Network.HTTP.Conduit (simpleHttp)
import Data.List (sortBy, groupBy)
import Data.Function (on)
import Data.Map (Map, toList)
import Data.ByteString.Lazy (ByteString)
import Data.Text.Lazy (Text)
import Data.Text.Lazy.Encoding (encodeUtf8)

import GHCJS.DOM (webViewGetDomDocument, runWebGUI, WebView, currentWindow)
import GHCJS.DOM.Document (documentGetBody, documentCreateElement,Document)
import GHCJS.DOM.HTMLElement (htmlElementSetInnerText)
import GHCJS.DOM.HTMLDivElement (castToHTMLDivElement)
import GHCJS.DOM.HTMLScriptElement (castToHTMLScriptElement, htmlScriptElementSetSrc)
import GHCJS.DOM.HTMLParagraphElement (castToHTMLParagraphElement)
import GHCJS.DOM.HTMLTableElement (HTMLTableElement, castToHTMLTableElement)
import GHCJS.DOM.HTMLTableCaptionElement (castToHTMLTableCaptionElement)
import GHCJS.DOM.HTMLTableRowElement (castToHTMLTableRowElement)
import GHCJS.DOM.HTMLTableCellElement (castToHTMLTableCellElement)
import GHCJS.DOM.Node (nodeAppendChild)
import GHCJS.Foreign (syncCallback1, ForeignRetention(NeverRetain), fromJSString)
import GHCJS.Types (JSFun, JSString)

import RosettaApi

-- Pretty print a single language
showLanguage :: Document -> HTMLTableElement -> Int -> Bool -> Language -> IO ()
showLanguage doc table rank tie (Language languageName languageQuantity) = do

    Just row <- fmap castToHTMLTableRowElement <$> 
                      documentCreateElement doc ("tr" :: String)

    Just pRank <- fmap castToHTMLTableCellElement <$> 
                      documentCreateElement doc ("td" :: String)

    let rankString = show rank ++ (if tie then " (tie)" else "") :: String
    htmlElementSetInnerText pRank $ rankString

    Just pName <- fmap castToHTMLTableCellElement <$> 
                      documentCreateElement doc ("td" :: String)

    htmlElementSetInnerText pName $ ( drop 9 languageName )

    Just pQuantity <- fmap castToHTMLTableCellElement <$> 
                      documentCreateElement doc ("td" :: String)

    htmlElementSetInnerText pQuantity $ show languageQuantity

    nodeAppendChild row (Just pRank)
    nodeAppendChild row (Just pName)
    nodeAppendChild row (Just pQuantity)

    nodeAppendChild table (Just row)
    return ()

-- Pretty print languages with common rank
showRanking :: Document -> HTMLTableElement -> (Int,  [Language]) -> IO ()
showRanking doc table (ranking, languages) = 
    mapM_ (showLanguage doc table ranking $ length languages > 1) languages

-- Sort and group languages by rank, then pretty print them.
showLanguages :: [Language] -> IO ()
showLanguages allLanguages = do
    Just webView <- currentWindow
    Just doc <- webViewGetDomDocument webView
    Just body <- documentGetBody doc

    Just table <- fmap castToHTMLTableElement <$> 
                      documentCreateElement doc ("table" :: String)

    Just caption <- fmap castToHTMLTableCaptionElement <$> 
                      documentCreateElement doc ("caption" :: String)

    htmlElementSetInnerText caption $ ("Rosetta Code Language Rankings" :: String)

    Just row <- fmap castToHTMLTableRowElement <$> 
                      documentCreateElement doc ("tr" :: String)

    Just pRank <- fmap castToHTMLTableCellElement <$> 
                      documentCreateElement doc ("th" :: String)

    htmlElementSetInnerText pRank $ ("Rank" :: String)

    Just pName <- fmap castToHTMLTableCellElement <$> 
                      documentCreateElement doc ("th" :: String)

    htmlElementSetInnerText pName $ ("Language" :: String)

    Just pQuantity <- fmap castToHTMLTableCellElement <$> 
                      documentCreateElement doc ("th" :: String)

    htmlElementSetInnerText pQuantity $ ("Completed Tasks" :: String)

    nodeAppendChild row (Just pRank)
    nodeAppendChild row (Just pName)
    nodeAppendChild row (Just pQuantity)

    nodeAppendChild table (Just caption)
    nodeAppendChild table (Just row)

    mapM_ (showRanking doc table) $ 
          zip [1..] $ 
          groupBy ((==) `on` quantity) $
          sortBy (flip compare `on` quantity) allLanguages

    nodeAppendChild body (Just table)
    return ()

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

foreign import javascript unsafe 
    "cb = function(json) { $1(JSON.stringify(json)); }"
    js_set_cb :: JSFun a -> IO ()

respondToQuery :: [Language] -> JSString -> IO ()
respondToQuery ls response = do
    let Just (Report continue langs) = decode $ encodeUtf8 $ fromJSString response
    let accLanguages = ls ++ map snd (toList langs)

    case continue of
        -- If there is no continue string we are done so display the accumulated languages.
        Nothing -> showLanguages accLanguages

        -- If there is a continue string, recusively continue the query.
        Just continueStr -> do
            let continueQueryStr = queryStr ++ 
                   "&gcmcontinue=" ++ urlEncode continueStr
            runQuery accLanguages continueQueryStr

runQuery :: [Language] -> String -> IO ()
runQuery ls query = do
    let cb = respondToQuery ls
    responder <- syncCallback1 NeverRetain False cb
    js_set_cb responder
    Just webView <- currentWindow
    Just doc <- webViewGetDomDocument webView
    Just body <- documentGetBody doc
    Just newScript <- fmap castToHTMLScriptElement <$> 
                      documentCreateElement doc ("script" :: String)
    htmlScriptElementSetSrc newScript query
    nodeAppendChild body (Just newScript)
    return ()

main :: IO ()
main = runQuery [] queryStr

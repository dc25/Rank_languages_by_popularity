{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson (decode)
import Network.HTTP.Base (urlEncode)
import Network.HTTP.Conduit (simpleHttp)
import Data.List (sortBy, groupBy)
import Data.Function (on)
import Data.Map (Map, toList)
import Data.ByteString.Lazy (pack, ByteString)
import Data.Text.Lazy (pack, Text)
import Data.Text.Lazy.Encoding (encodeUtf8)

-- import GHCJS.DOM.EventM (on)
-- import Control.Monad.Trans (liftIO)
import Data.JSString (JSString, unpack)

import GHCJS.DOM (webViewGetDomDocument, runWebGUI, WebView, currentWindow)
import GHCJS.DOM.Document (getBody, createElement,Document)
import GHCJS.DOM.Element (setInnerHTML)
-- import GHCJS.DOM.HTMLElement (setInnerHTML)
import GHCJS.DOM.HTMLDivElement (castToHTMLDivElement)
import GHCJS.DOM.HTMLScriptElement (castToHTMLScriptElement, setSrc)
import GHCJS.DOM.HTMLParagraphElement (castToHTMLParagraphElement)
import GHCJS.DOM.HTMLTableElement (HTMLTableElement, castToHTMLTableElement)
import GHCJS.DOM.HTMLTableCaptionElement (castToHTMLTableCaptionElement)
import GHCJS.DOM.HTMLTableRowElement (castToHTMLTableRowElement)
import GHCJS.DOM.HTMLTableCellElement (castToHTMLTableCellElement)
import GHCJS.DOM.Node (appendChild)
import GHCJS.Foreign 
import GHCJS.Foreign.Callback(Callback, syncCallback1, OnBlocked(ContinueAsync))
-- import GHCJS.Foreign (syncCallback1, ForeignRetention(NeverRetain))
import GHCJS.Types (JSVal)
import GHCJS.Marshal(fromJSVal)
-- import GHCJS.Types (JSString)

import RosettaApi

-- Pretty print a single language
showLanguage :: Document -> HTMLTableElement -> Int -> Bool -> Language -> IO ()
showLanguage doc table rank tie (Language languageName languageQuantity) = do

    -- Add a new row to the table.
    Just row <- fmap castToHTMLTableRowElement <$> 
                      createElement doc (Just "tr" :: Maybe String)
    appendChild table (Just row)

    -- Add a ranking entry to the new row.
    Just pRank <- fmap castToHTMLTableCellElement <$> 
                      createElement doc (Just "td" :: Maybe String)

    let rankString = Just $ show rank ++ (if tie then " (tie)" else "")
    setInnerHTML pRank $ rankString

    appendChild row (Just pRank)

    -- Add a name entry to the new row.
    Just pName <- fmap castToHTMLTableCellElement <$> 
                      createElement doc (Just "td" :: Maybe String)

    setInnerHTML pName $ ( Just $ drop 9 languageName )

    appendChild row (Just pName)

    -- Add a quantity entry to the new row.
    Just pQuantity <- fmap castToHTMLTableCellElement <$> 
                      createElement doc (Just "td" :: Maybe String)

    setInnerHTML pQuantity $ Just $ show languageQuantity

    appendChild row (Just pQuantity)

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
    Just body <- getBody doc

    Just table <- fmap castToHTMLTableElement <$> 
                      createElement doc (Just "table" :: Maybe String)

    -- Add a caption to the table
    Just caption <- fmap castToHTMLTableCaptionElement <$> 
                      createElement doc (Just "caption" :: Maybe String)


    setInnerHTML caption $ (Just "Rosetta Code Language Rankings" :: Maybe String)

    appendChild table (Just caption)

    -- Add a header row to the table.
    Just row <- fmap castToHTMLTableRowElement <$> 
                      createElement doc (Just "tr" :: Maybe String)

    appendChild table (Just row)

    -- Add a rank header to the header row.
    Just pRank <- fmap castToHTMLTableCellElement <$> 
                      createElement doc (Just "th" :: Maybe String)

    setInnerHTML pRank $ (Just "Rank" :: Maybe String)

    appendChild row (Just pRank)

    -- Add a language name header to the header row.
    Just pName <- fmap castToHTMLTableCellElement <$> 
                      createElement doc (Just "th" :: Maybe String)

    setInnerHTML pName (Just "Language" :: Maybe String)

    appendChild row (Just pName)

    -- Add a quantity header to the header row.
    Just pQuantity <- fmap castToHTMLTableCellElement <$> 
                      createElement doc (Just "th" :: Maybe String)

    setInnerHTML pQuantity (Just "Completed Tasks" :: Maybe String)

    appendChild row (Just pQuantity)

    -- Sort the languages, group by tasks completed, and add rows
    -- for each group with the same number of completed tasks.
    mapM_ (showRanking doc table) $ 
          zip [1..] $ 
          groupBy ((==) `on` quantity) $
          sortBy (flip compare `on` quantity) allLanguages

    -- Add the completed table to the document body for display.
    appendChild body (Just table)
    return ()

-- Mediawiki api style query to send to rosettacode.org
queryStr :: String
queryStr = "http://rosettacode.org/mw/api.php?" ++ 
           "format=json" ++ 
           "&action=query" ++ 
           "&generator=categorymembers" ++ 
           "&gcmtitle=Category:Programming%20Languages" ++ 
           "&gcmlimit=500" ++ 
           "&prop=categoryinfo" ++
           "&callback=javascriptCallback" -- specify javascript callback function.

respondToQuery :: [Language] -> JSVal -> IO ()
respondToQuery languagesSoFar response = do
    Just str <- fromJSVal response
    let Just (Report continue langs) = decode $ encodeUtf8 $ Data.Text.Lazy.pack $ unpack str
    let accLanguages = languagesSoFar ++ map snd (toList langs)

    case continue of
        -- If there is no continue string we are done so display the accumulated languages.
        Nothing -> showLanguages accLanguages

        -- If there is a continue string, recusively continue the query.
        Just continueStr -> do
            -- append the continuation string to the query string.
            let continueQueryStr = queryStr ++ 
                   "&gcmcontinue=" ++ urlEncode continueStr

            -- continue the query with the languages accumulated so far.
            runQuery accLanguages continueQueryStr

-- assign a haskell function to the javascript callback function
foreign import javascript unsafe 
    "javascriptCallback = function(json) { $1(JSON.stringify(json)); }"
    js_set_javascriptCallback :: Callback a -> IO ()

foreign import javascript unsafe "js_callback_($1)" 
    call_callback :: JSString -> IO ()

foreign import javascript unsafe "js_callback_ = $1"
    set_callback :: Callback a -> IO ()


runQuery :: [Language] -> String -> IO ()
runQuery languagesSoFar query = do
    -- callback is respondToQuery partially applied to languages processed so far.
    let haskellCallback = respondToQuery languagesSoFar

    -- make javascript-accessible version of callback
    responder <- syncCallback1 ContinueAsync haskellCallback

    -- assign callback to javascript variable "javascriptCallback"
    js_set_javascriptCallback responder

    -- create a script element
    Just webView <- currentWindow
    Just doc <- webViewGetDomDocument webView
    Just body <- getBody doc
    Just newElement <- createElement doc $ (Just "script" :: Maybe String)
    let newScript = castToHTMLScriptElement newElement
    -- Just newScript <- fmap castToHTMLScriptElement <$> elem

    -- put the query in the script element.
    setSrc newScript query

    -- put the script element with the query in the browser document body.
    appendChild body (Just newScript)
    return ()

main :: IO ()
main = runQuery [] queryStr

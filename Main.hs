{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson (decode)
import Network.HTTP.Base (urlEncode)
import Data.List (sortBy, groupBy)
import Data.Function (on)
import Data.Map (toList)
import Data.Text.Lazy (pack)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Data.JSString (unpack)

import GHCJS.DOM (webViewGetDomDocument, currentWindow)
import GHCJS.DOM.Document (getBody, createElement,Document)
import GHCJS.DOM.Element (setInnerHTML)
import GHCJS.DOM.HTMLScriptElement (castToHTMLScriptElement, HTMLScriptElement, setSrc)
import GHCJS.DOM.HTMLTableElement (HTMLTableElement, castToHTMLTableElement)
import GHCJS.DOM.HTMLTableCaptionElement (castToHTMLTableCaptionElement, HTMLTableCaptionElement)
import GHCJS.DOM.HTMLTableRowElement (castToHTMLTableRowElement,HTMLTableRowElement)
import GHCJS.DOM.HTMLTableCellElement (castToHTMLTableCellElement, HTMLTableCellElement)
import GHCJS.DOM.Node (appendChild)
import GHCJS.Foreign.Callback(Callback, syncCallback1, OnBlocked(ContinueAsync))
import GHCJS.Types (JSVal)
import GHCJS.Marshal(fromJSVal)

import RosettaApi(Language(..), quantity, Report(..))

tableElement :: Document -> IO (Maybe HTMLTableElement)
tableElement doc = 
    let element = createElement doc $ Just ("table" :: String)
    in fmap (fmap castToHTMLTableElement) element

captionElement :: Document -> IO (Maybe HTMLTableCaptionElement)
captionElement doc = 
    let element = createElement doc $ Just ("caption" :: String)
    in fmap (fmap castToHTMLTableCaptionElement) element

rowElement :: Document -> IO (Maybe HTMLTableRowElement)
rowElement doc = 
    let element = createElement doc $ Just ("tr" :: String)
    in fmap (fmap castToHTMLTableRowElement) element

cellElement :: Document -> IO (Maybe HTMLTableCellElement)
cellElement doc = 
    let element = createElement doc $ Just ("td" :: String)
    in fmap (fmap castToHTMLTableCellElement) element

headerCellElement :: Document -> IO (Maybe HTMLTableCellElement)
headerCellElement doc = 
    let element = createElement doc $ Just ("th" :: String)
    in fmap (fmap castToHTMLTableCellElement) element

scriptElement :: Document -> IO (Maybe HTMLScriptElement)
scriptElement doc = 
    let element = createElement doc $ Just ("script" :: String)
    in fmap (fmap castToHTMLScriptElement) element

-- Pretty print a single language
showLanguage :: Document -> HTMLTableElement -> Int -> Bool -> Language -> IO ()
showLanguage doc table rank tie (Language languageName languageQuantity) = do

    -- Add a new row to the table.

    Just row <- rowElement doc
    appendChild table (Just row)

    -- Add a ranking entry to the new row.
    Just pRank <- cellElement doc

    let rankString = Just $ show rank ++ (if tie then " (tie)" else "")
    setInnerHTML pRank rankString

    appendChild row (Just pRank)

    -- Add a name entry to the new row.
    Just pName <- cellElement doc

    setInnerHTML pName ( Just $ drop 9 languageName )

    appendChild row (Just pName)

    -- Add a quantity entry to the new row.
    Just pQuantity <- cellElement doc

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

    Just table <- tableElement doc

    -- Add a caption to the table
    Just caption <- captionElement doc

    setInnerHTML caption (Just "Rosetta Code Language Rankings" :: Maybe String)

    appendChild table (Just caption)

    -- Add a header row to the table.
    Just row <- rowElement doc

    appendChild table (Just row)

    -- Add a rank header to the header row.
    Just pRank <- headerCellElement doc

    setInnerHTML pRank (Just "Rank" :: Maybe String)

    appendChild row (Just pRank)

    -- Add a language name header to the header row.
    Just pName <- headerCellElement doc

    setInnerHTML pName (Just "Language" :: Maybe String)

    appendChild row (Just pName)

    -- Add a quantity header to the header row.
    Just pQuantity <- headerCellElement doc

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
    Just newElement <- scriptElement doc 

    let newScript = castToHTMLScriptElement newElement
    Just newScript <- scriptElement doc 

    -- put the query in the script element.
    setSrc newScript query

    -- put the script element with the query in the browser document body.
    appendChild body (Just newScript)
    return ()

main :: IO ()
main = runQuery [] queryStr

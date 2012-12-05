{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Arrow ((>>>), arr)
import Data.Monoid (mempty, mconcat)
import System.Locale (iso8601DateFormat)

import Hakyll

main :: IO ()
main = hakyllWith config $ do

    -- Blog posts are under posts/
    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pageCompiler
            >>> arr (renderDateField "date" "%B %e, %Y" "Date unknown")
            >>> arr (renderDateField "machinedate" (iso8601DateFormat Nothing) "")
            >>> renderTagsField "prettytags" tagIdentifier
            >>> applyTemplateCompiler "templates/article.html"
            >>> applyTemplateCompiler "templates/master.html"
            >>> relativizeUrlsCompiler

    -- Blog archive
    match "archive.html" $ do
        route idRoute
        create "archive.html" $ constA mempty
             >>> arr (setField "title" "Blog archive")
             >>> setFieldPageList recentFirst "templates/archive-item.html" "posts" "posts/*"
             >>> applyTemplateCompiler "templates/archive.html"
             >>> applyTemplateCompiler "templates/master.html"
             >>> relativizeUrlsCompiler

    -- Tags
    create "tags" $ requireAll "posts/*" (\_ ps -> readTags ps :: Tags String)

    -- Add a tag list compiler for every tag
    match "tags/*" $ route $ setExtension ".html"
    metaCompile $ require_ "tags"
        >>> arr tagsMap
        >>> arr (map (\(t, p) -> (tagIdentifier t, makeTagList t p)))


     -- Render RSS feed
    match "rss.xml" $ route idRoute
    create "rss.xml" $ requireAll_ "posts/*"
            >>> renderRss feedConfiguration


    -- Static pages are located in pages/
    match "pages/*" $ do
        route $ gsubRoute "pages/" (const "") `composeRoutes` setExtension "html"
        compile $ pageCompiler
            >>> byPattern (arr id)
                [ ("pages/index.md", addRelatedToAs "posts/*" "Recent blog entries")
                , ("pages/research.md", addRelatedToAs "tags/research" "Related recent blog entries")
                ]
            >>> applyTemplateCompiler "templates/master.html"
            >>> relativizeUrlsCompiler

    -- CSS
    match "css/*" $ do
        route idRoute
        compile compressCssCompiler

    -- Templates
    match "templates/*" $ compile templateCompiler

  where
    tagIdentifier = fromCapture "tags/*"


-- * Auxiliary compilers
makeTagList :: String -> [Page String] -> Compiler () (Page String)
makeTagList tag posts = constA posts
        >>> pageListCompiler recentFirst "templates/archive-item.html"
        >>> arr (copyBodyToField "posts" . fromBody)
        >>> arr (setField "title" ("Posts tagged as " ++ tag))
        >>> applyTemplateCompiler "templates/archive.html"
        >>> applyTemplateCompiler "templates/master.html"
        >>> relativizeUrlsCompiler

addRelatedToAs :: Pattern (Page String) -> String -> Compiler (Page String) (Page String)
addRelatedToAs p t = setFieldPageList (take newestEntries . recentFirst) "templates/archive-item.html" "posts" p
        >>> arr (setField "related" t)
        >>> applyTemplateCompiler "templates/related.html"


-- * Configuration
config :: HakyllConfiguration
config = defaultHakyllConfiguration {
    deployCommand = " rsync --checksum -ave 'ssh' \
                    \_site/* jtanguy@jhome.fr:sites/julien.jhome.fr"
    }

newestEntries :: Int
newestEntries = 3

feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
    { feedTitle = "jtanguy :: Blogger"
    , feedDescription = "Thoughts about random cs-related things."
    , feedAuthorName = "Julien Tanguy"
    , feedAuthorEmail = "julien.tanguy@jhome.fr"
    , feedRoot = "http://julien.jhome.fr"
    }


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

     -- Render RSS feed
    match "rss.xml" $ route idRoute
    create "rss.xml" $ requireAll_ "posts/*"
            >>> renderRss feedConfiguration


    -- Static pages are located in pages/
    match "pages/*" $ do
        route $ gsubRoute "pages/" (const "") `composeRoutes` setExtension "html"
        compile $ pageCompiler
            >>> applyTemplateCompiler "templates/master.html"
            >>> relativizeUrlsCompiler

    -- CSS
    match "css/*" $ do
        route idRoute
        compile compressCssCompiler

    -- Templates
    match "templates/*" $ compile templateCompiler

config :: HakyllConfiguration
config = defaultHakyllConfiguration {
    deployCommand = " rsync --checksum -ave 'ssh' \
                    \_site/* jtanguy@jhome.fr:sites/julien.jhome.fr"
    }

feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
    { feedTitle = "jtanguy :: Blogger"
    , feedDescription = "Thoughts about random cs-related things."
    , feedAuthorName = "Julien Tanguy"
    , feedAuthorEmail = "julien.tanguy@jhome.fr"
    , feedRoot = "http://julien.jhome.fr"
    }


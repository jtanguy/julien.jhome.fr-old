{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Monoid (mappend, mconcat)
import System.Locale (iso8601DateFormat)

import Hakyll

main :: IO ()
main = hakyllWith config $ do
    -- Build tags
    tags <- buildTags "posts/*" (fromCapture "tags/*.html")

    -- Compress CSS
    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    -- Copy Files
    match "files/*" $ do
        route   idRoute
        compile copyFileCompiler

    -- Copy static assets
    match "assets/**" $ do
        route   idRoute
        compile copyFileCompiler

    -- Render posts
    match "posts/*" $ do
        route   $ setExtension ".html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html" (tagsCtx tags)
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    -- Render posts list
    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            itemTpl <- loadBody "templates/archive-item.html"
            list <- applyTemplateList itemTpl postCtx posts
            let archiveCtx = constField "title" "All posts" `mappend`
                             defaultContext
            makeItem list
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" defaultContext
                >>= relativizeUrls

    -- Static pages
    match "pages/*" $ do
        route $ gsubRoute "pages/" (const "") `composeRoutes` setExtension "html"
        compile $ pandocCompiler
                >>= loadAndApplyTemplate "templates/default.html" defaultContext
                >>= relativizeUrls

    -- Project pages
    match "projects/**.md" $ do
        route $ setExtension ".html"
        compile $ pandocCompiler
                >>= loadAndApplyTemplate "templates/default.html" defaultContext
                >>= relativizeUrls
    match ("projects/**" .&&. complement "projects/**.md") $ do
        route   idRoute
        compile copyFileCompiler

    -- Post tags
    tagsRules tags $ \tag pattern -> do
        let title = "Posts tagged " ++ tag
        route idRoute
        compile $ do
            list <- postList tags pattern recentFirst
            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" (
                        mconcat [ constField "title" title
                                , constField "body" list
                                , defaultContext
                                ])
                >>= loadAndApplyTemplate "templates/default.html" defaultContext
                >>= relativizeUrls


    -- Render RSS feed
    create ["rss.xml"] $ do
        route idRoute
        compile $ loadAllSnapshots "posts/*" "content"
            >>= fmap (take 10) . recentFirst
            >>= renderRss feedConfiguration feedCtx

    create ["atom.xml"] $ do
        route idRoute
        compile $ loadAllSnapshots "posts/*" "content"
            >>= fmap (take 10) . recentFirst
            >>= renderAtom feedConfiguration feedCtx

    -- Read templates
    match "templates/*" $ compile templateCompiler

postCtx :: Context String
postCtx = mconcat [ dateField "machinedate" (iso8601DateFormat Nothing)
                  , dateField "date" "%B %e, %Y"
                  , dateField "dateday" "%d"
                  , dateField "datemonth" "%b"
                  , dateField "dateyear" "%Y"
                  , defaultContext
                  ]


feedCtx :: Context String
feedCtx = mconcat [ postCtx
                  , metadataField
                  ]

tagsCtx :: Tags -> Context String
tagsCtx tags = mconcat [ tagsField "prettytags" tags
                       , postCtx
                       ]

config :: Configuration
config = defaultConfiguration {
    deployCommand = " rsync --checksum --delete -ave 'ssh' \
                    \_site/* jtanguy@jhome.fr:sites/julien.jhome.fr"
    }

feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
    { feedTitle = "jtanguy - RSS feed"
    , feedDescription = "Thoughts about random cs-related things."
    , feedAuthorName = "Julien Tanguy"
    , feedAuthorEmail = "julien.tanguy@jhome.fr"
    , feedRoot = "http://julien.jhome.fr"
    }

postList :: Tags -> Pattern -> ([Item String] -> Compiler [Item String])
         -> Compiler String
postList tags pattern preprocess' = do
    postItemTpl <- loadBody "templates/archive-item.html"
    posts <- preprocess' =<< loadAll pattern
    applyTemplateList postItemTpl (tagsCtx tags) posts


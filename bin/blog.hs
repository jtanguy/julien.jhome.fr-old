{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (liftM)
import Data.Maybe (fromMaybe)
import Data.Monoid (mappend, mconcat)
import System.Locale (iso8601DateFormat)

import Text.Pandoc.Options
import Hakyll

main :: IO ()
main = hakyllWith config $ do
    -- Build tags
    tags <- buildTags "posts/*" (fromCapture "tags/*.html")

    -- Compress CSS
    match "assets/css/*" $ do
        route   $ gsubRoute "assets/" (const "") `composeRoutes` idRoute
        compile compressCssCompiler

    -- Bibtex entries (for bibliography)
    match "assets/bib/*" $ compile biblioCompiler

    match "assets/csl/*" $ compile cslCompiler

    -- Copy static assets
    match "assets/files/*" $ do
        route   idRoute
        compile copyFileCompiler

    -- Render posts
    match "posts/*" $ do
        route   $ setExtension ".html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html" (tagsCtx tags)
            >>= saveSnapshot "content"
            >>= pageCompiler

    match "posts/*" $ version "toc" $
       compile $ pandocCompilerWith defaultHakyllReaderOptions
                                    defaultHakyllWriterOptions {
                                        writerTableOfContents = True
                                      , writerTemplate = "$toc$"
                                      , writerStandalone = True
                                      }


    -- Render posts list
    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll ("posts/*" .&&. hasNoVersion)
            itemTpl <- loadBody "templates/archive-item.html"
            list <- applyTemplateList itemTpl postCtx posts
            let archiveCtx = constField "title" "All posts" `mappend`
                             defaultContext
            makeItem list
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= pageCompiler

    -- Static pages
    match "pages/*" $ do
        route $ gsubRoute "pages/" (const "") `composeRoutes` setExtension "html"
        compile $ do
            item <- getUnderlying
            title <- liftM (fromMaybe "Related posts") $ getMetadataField item "relatedTitle"
            related <- liftM (fromMaybe "") $ getMetadataField item "related"
            bibFile <- liftM (fromMaybe "") $ getMetadataField item "biblio"
            cslFile <- liftM (fromMaybe "chicago") $ getMetadataField item "csl"
            list <- if related == "*" then
                        postList tags ("posts/*" .&&. hasNoVersion) recentFirst
                    else let items = fromMaybe [] $ lookup related (tagsMap tags)
                         in postList tags (fromList items) recentFirst
            let relatedCtx = constField "related.title" title `mappend`
                             constField "related" list `mappend`
                             defaultContext
            let compiler = if bibFile /= "" then
                                bibtexCompiler cslFile bibFile
                           else pandocCompiler
            compiler
                >>= loadAndApplyTemplate "templates/related.html" relatedCtx
                >>= pageCompiler


    -- Project pages
    match "projects/**.md" $ do
        route $ setExtension ".html"
        compile $ pandocCompiler >>= pageCompiler

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
                >>= pageCompiler

    -- Render RSS feed
    create ["rss.xml"] $ do
        route idRoute
        compile $ loadAllSnapshots ("posts/*" .&&. hasNoVersion) "content"
            >>= fmap (take 10) . recentFirst
            >>= renderRss feedConfiguration feedCtx

    create ["atom.xml"] $ do
        route idRoute
        compile $ loadAllSnapshots ("posts/*" .&&. hasNoVersion) "content"
            >>= fmap (take 10) . recentFirst
            >>= renderAtom feedConfiguration feedCtx

    create ["sitemap.xml"] $ do
        route idRoute
        compile $ do
            posts <- loadAll (("pages/*" .||. "posts/*" .||. "projects/**.md")
                              .&&. hasNoVersion)
            itemTpl <- loadBody "templates/sitemap-item.xml"
            list <- applyTemplateList itemTpl (sitemapCtx feedConfiguration) posts
            makeItem list
                >>= loadAndApplyTemplate "templates/sitemap.xml" defaultContext

    match "assets/txt/*" $ do
        route $ gsubRoute "assets/txt/" (const "")
        compile copyFileCompiler

    -- Read templates
    match "templates/*" $ compile templateCompiler

-- Auxiliary compilers
pageCompiler :: Item String -> Compiler (Item String)
pageCompiler i = loadAndApplyTemplate "templates/default.html" defaultContext i
               >>= relativizeUrls

bibtexCompiler :: String -> String -> Compiler (Item String)
bibtexCompiler cslFileName bibFileName = do 
    csl <- load (fromFilePath $ "assets/csl/"++cslFileName)
    bib <- load (fromFilePath $ "assets/bib/"++bibFileName)
    liftM writePandoc
        (getResourceBody >>= readPandocBiblio def csl bib)

postList :: Tags -> Pattern -> ([Item String] -> Compiler [Item String])
         -> Compiler String
postList tags pattern preprocess' = do
    postItemTpl <- loadBody "templates/archive-item.html"
    posts <- preprocess' =<< loadAll (pattern .&&. hasNoVersion)
    applyTemplateList postItemTpl (tagsCtx tags) posts

-- Contexts
postCtx :: Context String
postCtx = mconcat [ dateField "date.machine" (iso8601DateFormat Nothing)
                  , dateField "date" "%B %e, %Y"
                  , field "toc" $ \item ->
                        loadBody ((itemIdentifier item) { identifierVersion = Just "toc"})
                  , modificationTimeField "updated.machine" (iso8601DateFormat Nothing)
                  , modificationTimeField "updated" "%B %e, %Y"
                  , dateField "date.day" "%d"
                  , dateField "date.month" "%b"
                  , dateField "date.year" "%Y"
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

sitemapCtx :: FeedConfiguration -> Context String
sitemapCtx conf = mconcat [ constField "root" (feedRoot conf)
                          , feedCtx
                          ]

-- Configuration
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


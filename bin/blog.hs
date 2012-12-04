{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Arrow ((>>>), arr)
import Data.Monoid (mempty, mconcat)

import Hakyll

main :: IO ()
main = hakyllWith config $ do

    -- Static pages are located in pages/
    match "pages/*" $ do
        route $ gsubRoute "pages/" (const "") `composeRoutes` setExtension "html"
        compile $ pageCompiler
            >>> applyTemplateCompiler "templates/master.html"
            >>> relativizeUrlsCompiler

    -- css
    match "css/*" $ do
        route idRoute
        compile compressCssCompiler

    match "templates/*" $ compile templateCompiler

config :: HakyllConfiguration
config = defaultHakyllConfiguration {
    deployCommand = " rsync --checksum -ave 'ssh' \
                    \_site/* jtanguy@jhome.fr:sites/julien.jhome.fr"
    }


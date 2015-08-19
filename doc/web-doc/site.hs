--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll


--------------------------------------------------------------------------------
main :: IO ()
main = do
  basicExample <- readFile "examples/basic_example.j"   
  hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "docs/examples.markdown" $ do
        route $ setExtension "html"
        compile $ do
          let ctx =
                constField "basicExample" basicExample `mappend`
                defaultContext
          getResourceBody
            >>= applyAsTemplate ctx
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "docs/installing.markdown" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls
    
    match "docs.markdown" $ do
        route $ setExtension "html"
        compile $ do
            docs <- loadAll "docs/*"
            let docsCtx =
                    listField "docs" defaultContext (return docs) `mappend`
                    defaultContext
            getResourceBody
                >>= applyAsTemplate docsCtx
                >>= loadAndApplyTemplate "templates/default.html" docsCtx
                >>= relativizeUrls

    match "index.markdown" $ do
        route $ setExtension "html"
        compile $ do pandocCompiler
                >>= loadAndApplyTemplate "templates/default.html" defaultContext
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler



--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll
import qualified Data.Set as S
import           Text.Pandoc.Options
import           Control.Monad(liftM)

--------------------------------------------------------------------------------
main :: IO ()
main = do
  basicExample <- readFile "examples/basicExample.j"
  addExample <- readFile "examples/add.j"
  multExample <- readFile "examples/mult.j"
  hakyll $ do
    match "bib/*" $ compile biblioCompiler
    match "csl/*" $ compile cslCompiler
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "docs/implementation.markdown" $ do
        route $ setExtension "html"
        compile $ do
            let ctx = constField "add" addExample `mappend`
                      constField "mult" multExample `mappend`
                      defaultContext
            bibtexMathCompilerCtx ctx "csl/default.csl" "bib/default.bib"
                >>= loadAndApplyTemplate "templates/default.html" defaultContext
                >>= relativizeUrls


    match "docs/examples.markdown" $ do
        route $ setExtension "html"
        compile $ do
          let ctx =
                constField "basicExample" basicExample `mappend`
                defaultContext
          getResourceBody
            >>= applyAsTemplate ctx
            >>= renderPandocMath
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "docs/*" $ do
        route $ setExtension "html"
        compile $ pandocMathCompiler
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

bibtexMathCompilerCtx :: Context String -> String -> String -> Compiler (Item String)
bibtexMathCompilerCtx ctx cslFileName bibFileName = do
    csl <- load $ fromFilePath cslFileName
    bib <- load $ fromFilePath bibFileName
    liftM (writePandocWith mathWriterOptions)
        (getResourceBody
            >>= applyAsTemplate ctx
            >>= readPandocBiblio def csl bib)

renderPandocMath :: Item String -> Compiler (Item String)
renderPandocMath = renderPandocWith defaultHakyllReaderOptions mathWriterOptions

pandocMathCompiler :: Compiler (Item String)
pandocMathCompiler = pandocCompilerWith defaultHakyllReaderOptions mathWriterOptions

mathWriterOptions :: WriterOptions
mathWriterOptions =
    defaultHakyllWriterOptions {
        writerExtensions = newExtensions,
        writerHTMLMathMethod = MathJax "" }
    where mathExtensions = [Ext_tex_math_dollars, Ext_tex_math_double_backslash,
                          Ext_latex_macros]
          defaultExtensions = writerExtensions defaultHakyllWriterOptions
          newExtensions = foldr S.insert defaultExtensions mathExtensions

--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend,mconcat,(<>))
import           Hakyll
import qualified Data.Set as S
import qualified Data.Map as M
import           Text.Pandoc.Options
import           Control.Monad(liftM)
import           System.Directory
import           System.FilePath
import           Control.Applicative((<$>),(<*>))

--------------------------------------------------------------------------------
main :: IO ()
main = do
  exampleContext <- mconcat . map (uncurry constField) <$> getExamples
  hakyll $ do
    match "bib/*" $ compile biblioCompiler
    match "csl/*" $ compile cslCompiler
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "docs/*" $ do
        route $ setExtension "html"
        compile $ do
            let ctx = exampleContext <> defaultContext
            bibtexMathCompilerCtx ctx "csl/default.csl" "bib/default.bib"
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

getExamples :: IO  [(String,String)]
getExamples = do
    filepaths <- map ("examples" </>)
                 . filter ((== ".j") . takeExtension)
                 <$> getDirectoryContents "examples"
    putStrLn $ show filepaths
    contents <- mapM readFile filepaths
    return $ zip (map takeBaseName filepaths) contents

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

{- cabal:
build-depends: base, text, directory, filepath, pandoc
-}

{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text (Text)
import System.Directory (createDirectoryIfMissing, copyFile, listDirectory, doesDirectoryExist, removeDirectoryRecursive)
import System.FilePath ((</>), dropExtension, takeExtension, isExtensionOf)
import Control.Monad (forM_, when)
import Data.List (sort, isSuffixOf)
import Text.Pandoc

-- ==========================================
-- CONFIGURATION
-- ==========================================

outputDir :: FilePath
outputDir = "docs"

-- Explicitly ordered chapter list
orderedChapters :: [String]
orderedChapters = 
    [ "introduction"
    , "starting-out"
    , "types-and-typeclasses"
    , "syntax-in-functions"
    , "recursion"
    , "higher-order-functions"
    , "modules"
    , "making-our-own-types-and-typeclasses"
    , "input-and-output"
    , "functionally-solving-problems"
    , "functors-applicative-functors-and-monoids"
    , "a-fistful-of-monads"
    , "for-a-few-monads-more"
    , "zippers"
    ]

-- ==========================================
-- RENDERING LOGIC
-- ==========================================

-- | Converts Markdown text to HTML using Pandoc.
renderMarkdown :: Text -> IO Text
renderMarkdown input = do
    result <- runIO $ do
        doc <- readMarkdown def input
        writeHtml5String def doc
    case result of
        Left err -> return $ "Error parsing markdown: " <> T.pack (show err)
        Right html -> return html

-- | Main layout matching the original Learn You a Haskell design.
layout :: Maybe String -> Maybe String -> Text -> Text
layout mPrev mNext content = T.unlines
    [ "<!DOCTYPE html>"
    , "<html lang=\"pt-BR\">"
    , "<head>"
    , "    <meta charset=\"UTF-8\">"
    , "    <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">"
    , "    <title>Aprenda Haskell para o Bem de Todos!</title>"
    , "    <link rel=\"stylesheet\" href=\"assets/css/reset.css\" type=\"text/css\" />"
    , "    <link rel=\"stylesheet\" href=\"assets/css/style.css\" type=\"text/css\" />"
    , "    <link rel=\"stylesheet\" href=\"sh/Styles/SyntaxHighlighter.css\" type=\"text/css\" />"
    , "    <link rel=\"shortcut icon\" href=\"assets/images/favicon.png\" type=\"image/png\" />"
    , "</head>"
    , "<body>"
    , "    <div class=\"bgwrapper\">"
    , "        <div id=\"content\">"
    ,           renderFootDiv mPrev mNext
    , "            <article>"
    ,               content
    , "            </article>"
    ,           renderFootDiv mPrev mNext
    , "        </div>"
    , "    </div>"
    , "    <script type=\"text/javascript\" src=\"sh/Scripts/shCore.js\"></script>"
    , "    <script type=\"text/javascript\" src=\"sh/Scripts/shBrushHaskell.js\"></script>"
    , "    <script type=\"text/javascript\" src=\"sh/Scripts/shBrushPlain.js\"></script>"
    , "    <script type=\"text/javascript\" src=\"assets/js/toggleShow.js\"></script>"
    , "    <script type=\"text/javascript\">"
    , "        dp.SyntaxHighlighter.HighlightAll('code');"
    , "    </script>"
    , "</body>"
    , "</html>"
    ]

renderFootDiv :: Maybe String -> Maybe String -> Text
renderFootDiv mPrev mNext = T.unlines
    [ "<div class=\"footdiv\">"
    , "    <ul>"
    ,       case mPrev of
                Nothing -> "<li class=\"prev\"></li>"
                Just p  -> "<li class=\"prev\"><a href=\"" <> T.pack p <> ".html\">Anterior</a></li>"
    , "        <li class=\"toc\"><a href=\"index.html\">Sumário</a></li>"
    ,       case mNext of
                Nothing -> "<li class=\"next\"></li>"
                Just n  -> "<li class=\"next\"><a href=\"" <> T.pack n <> ".html\">Próximo</a></li>"
    , "    </ul>"
    , "</div>"
    ]

-- ==========================================
-- MAIN BUILD STEP
-- ==========================================

main :: IO ()
main = do
    putStrLn "--- Iniciando build simplificado ---"
    createDirectoryIfMissing True outputDir
    
    let total = length orderedChapters
    
    forM_ (zip [0..] orderedChapters) $ \(i, name) -> do
        putStrLn $ "Renderizando [" ++ show (i+1) ++ "/" ++ show total ++ "]: " ++ name
        
        -- Lê MD da raiz
        content <- TIO.readFile (name ++ ".md")
        htmlBody <- renderMarkdown content
        
        let mPrev = if i > 0 then Just (orderedChapters !! (i-1)) else Nothing
        let mNext = if i < total - 1 then Just (orderedChapters !! (i+1)) else Nothing
        
        let finalHtml = layout mPrev mNext htmlBody
        TIO.writeFile (outputDir </> name ++ ".html") finalHtml
    
    -- index.html
    let firstChapter = head orderedChapters
    copyFile (outputDir </> firstChapter ++ ".html") (outputDir </> "index.html")
    putStrLn $ "index.html criado a partir de " ++ firstChapter
    
    -- Copiar Assets
    putStrLn "Copiando ativos..."
    copyDirRecursive "assets" (outputDir </> "assets")
    copyDirRecursive "sh" (outputDir </> "sh")
    
    putStrLn $ "--- Build concluído com sucesso em /" ++ outputDir ++ " ---"

-- | Auxiliar para cópia recursiva
copyDirRecursive :: FilePath -> FilePath -> IO ()
copyDirRecursive src dst = do
    exists <- doesDirectoryExist src
    when exists $ do
        createDirectoryIfMissing True dst
        items <- listDirectory src
        forM_ items $ \item -> do
            let s = src </> item
            let d = dst </> item
            isDir <- doesDirectoryExist s
            if isDir
                then copyDirRecursive s d
                else copyFile s d

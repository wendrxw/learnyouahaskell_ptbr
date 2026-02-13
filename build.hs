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
orderedChapters :: [(String, String)]
orderedChapters = 
    [ ("introduction", "Introdução")
    , ("starting-out", "Começando")
    , ("types-and-typeclasses", "Tipos e Typeclasses")
    , ("syntax-in-functions", "Sintaxe em Funções")
    , ("recursion", "Recursão")
    , ("higher-order-functions", "Funções de Alta Ordem")
    , ("modules", "Módulos")
    , ("making-our-own-types-and-typeclasses", "Criando Nossos Próprios Tipos e Typeclasses")
    , ("input-and-output", "Entrada e Saída")
    , ("functionally-solving-problems", "Resolvendo Problemas Funcionalmente")
    , ("functors-applicative-functors-and-monoids", "Funtores, Funtores Aplicativos e Monoides")
    , ("a-fistful-of-monads", "Por um Punhado de Monads")
    , ("for-a-few-monads-more", "Por mais Alguns Monads")
    , ("zippers", "Zippers")
    ]


-- ==========================================
-- RENDERING LOGIC
-- ==========================================

-- | Pandoc options with attributes enabled and figures disabled
readerOpts :: ReaderOptions
readerOpts = def 
    { readerExtensions = extensionsFromList 
        [ Ext_attributes
        , Ext_link_attributes
        , Ext_header_attributes
        , Ext_fenced_code_attributes
        , Ext_implicit_header_references
        ] <> (disableExtension Ext_implicit_figures pandocExtensions) 
    }

writerOpts :: WriterOptions
writerOpts = def 
    { writerExtensions = extensionsFromList 
        [ Ext_attributes
        , Ext_link_attributes
        , Ext_header_attributes
        , Ext_fenced_code_attributes
        ] <> (disableExtension Ext_implicit_figures pandocExtensions)
    }

-- | Converts Markdown text to HTML using Pandoc.
renderMarkdown :: Text -> IO Text
renderMarkdown input = do
    result <- runIO $ do
        doc <- readMarkdown readerOpts input
        writeHtml5String writerOpts doc
    case result of
        Left err -> return $ "Error parsing markdown: " <> T.pack (show err)
        Right html -> return html

-- | Main layout matching the original Learn You a Haskell design.
layout :: String -> Maybe (String, String) -> Maybe (String, String) -> Text -> Text
layout bodyClass mPrev mNext content = T.unlines
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
    , "<body class=\"" <> T.pack bodyClass <> "\">"
    , "    <div class=\"bgwrapper\">"
    , "        <div id=\"content\">"
    ,           renderFootDiv True mPrev mNext
    , "            <article>"
    ,               content
    , "            </article>"
    ,           renderFootDiv False mPrev mNext
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

renderFootDiv :: Bool -> Maybe (String, String) -> Maybe (String, String) -> Text
renderFootDiv isTop mPrev mNext = T.unlines
    [ "<div class=\"footdiv\"" <> (if isTop then " style=\"margin-bottom:25px;\"" else "") <> ">"
    , "    <ul>"
    , "        <li style=\"text-align:left\">"
    ,           case mPrev of
                    Nothing -> ""
                    Just (f, t) -> "<a href=\"" <> T.pack f <> ".html\">Anterior (" <> T.pack t <> ")</a>"
    , "        </li>"
    , "        <li style=\"text-align:center\">"
    , "            <a href=\"chapters.html\">Índice de capítulos</a>"
    , "        </li>"
    , "        <li style=\"text-align:right\">"
    ,           case mNext of
                    Nothing -> ""
                    Just (f, t) -> "<a href=\"" <> T.pack f <> ".html\" class=\"nxtlink\">Próximo (" <> T.pack t <> ")</a>"
    , "        </li>"
    , "    </ul>"
    , "</div>"
    ]

-- ==========================================
-- TOC GENERATION
-- ==========================================

generateTOC :: IO ()
generateTOC = do
    putStrLn "Gerando chapters.html (TOC)..."
    let tocContent = T.unlines
            [ "<h1>Índice de capítulos</h1>"
            , "<ol class=\"chapters\" type=\"1\">"
            , T.concat (map renderTocItem orderedChapters)
            , "</ol>"
            ]
    let finalHtml = layout "introcontent" Nothing Nothing tocContent
    TIO.writeFile (outputDir </> "chapters.html") finalHtml

renderTocItem :: (String, String) -> Text
renderTocItem (file, title) = 
    "    <li><a href=\"" <> T.pack file <> ".html\">" <> T.pack title <> "</a></li>"

-- ==========================================
-- MAIN BUILD STEP
-- ==========================================

main :: IO ()
main = do
    putStrLn "--- Iniciando build de alta fidelidade ---"
    createDirectoryIfMissing True outputDir
    
    let total = length orderedChapters
    
    forM_ (zip [0..] orderedChapters) $ \(i, (name, chapterTitle)) -> do
        putStrLn $ "Renderizando [" ++ show (i+1) ++ "/" ++ show total ++ "]: " ++ name ++ " (" ++ chapterTitle ++ ")"
        
        -- Lê MD da raiz
        content <- TIO.readFile (name ++ ".md")
        htmlBody <- renderMarkdown content
        
        let mPrev = if i > 0 then Just (orderedChapters !! (i-1)) else Nothing
        let mNext = if i < total - 1 then Just (orderedChapters !! (i+1)) else Nothing
        
        -- Capítulos no LYAH original usam "introcontent" como classe
        let finalHtml = layout "introcontent" mPrev mNext htmlBody
        TIO.writeFile (outputDir </> name ++ ".html") finalHtml
    
    -- Gerar TOC
    generateTOC
    
    -- index.html redireciona para chapters.html ou é uma cópia da intro
    copyFile (outputDir </> "chapters.html") (outputDir </> "index.html")
    putStrLn "index.html criado a partir de chapters.html"
    
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

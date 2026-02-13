{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (createDirectoryIfMissing, copyFile, listDirectory, doesDirectoryExist)
import System.FilePath ((</>), dropExtension, takeExtension)
import Control.Monad (forM_, when)
import Data.Maybe (listToMaybe)

import Files (listOrderedChapters, readChapter)
import Markdown (renderMarkdown)
import Templates (layout)

outputDir :: FilePath
outputDir = "site"

main :: IO ()
main = do
    putStrLn "Iniciando geração do site estático (Layout Original)..."
    createDirectoryIfMissing True outputDir
    
    -- Caminho para os docs
    let docDir = "docs"
    chapters <- listOrderedChapters
    
    if null chapters
        then putStrLn "Nenhum capítulo encontrado."
        else do
            let indexedChapters = zip [0..] chapters
            let total = length chapters
            
            forM_ indexedChapters $ \(i, chapterName) -> do
                putStrLn $ "Renderizando capítulo [" ++ show (i+1) ++ "/" ++ show total ++ "]: " ++ chapterName
                
                content <- readChapter docDir chapterName
                htmlBody <- renderMarkdown content
                
                let mPrev = if i > 0 then Just (chapters !! (i-1)) else Nothing
                let mNext = if i < total - 1 then Just (chapters !! (i+1)) else Nothing
                
                let finalHtml = layout mPrev mNext htmlBody
                TIO.writeFile (outputDir </> chapterName ++ ".html") finalHtml
            
            -- Criar index.html (cópia do primeiro capítulo por enquanto, ou página de capítulos)
            let firstChapter = head chapters
            copyFile (outputDir </> firstChapter ++ ".html") (outputDir </> "index.html")
            putStrLn $ "index.html criado a partir de " ++ firstChapter
            
            -- Copiar diretório de assets
            let assetsSource = docDir </> "assets"
            let assetsTarget = outputDir </> "assets"
            hasAssets <- doesDirectoryExist assetsSource
            when hasAssets $ do
                putStrLn "Copiando assets..."
                copyDir assetsSource assetsTarget
            
            -- Copiar diretório sh (Syntax Highlighter)
            let shSource = docDir </> "sh"
            let shTarget = outputDir </> "sh"
            hasSh <- doesDirectoryExist shSource
            when hasSh $ do
                putStrLn "Copiando syntax highlighter..."
                copyDir shSource shTarget
            
            putStrLn "Geração concluída com sucesso na pasta /site!"

-- Função auxiliar para copiar diretórios recursivamente
copyDir :: FilePath -> FilePath -> IO ()
copyDir src dst = do
    createDirectoryIfMissing True dst
    items <- listDirectory src
    forM_ items $ \item -> do
        let srcPath = src </> item
        let dstPath = dst </> item
        isDir <- doesDirectoryExist srcPath
        if isDir
            then copyDir srcPath dstPath
            else copyFile srcPath dstPath

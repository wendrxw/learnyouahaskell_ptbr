{-# LANGUAGE OverloadedStrings #-}

import qualified Web.Scotty as S

import Network.HTTP.Types (status404)
import Data.Text.Lazy (fromStrict, Text)
import qualified Data.Text.Lazy as TL
import qualified Data.Text as T
import Control.Monad.IO.Class (liftIO)
import System.Directory (getCurrentDirectory)

import Files (listChapters, readChapter)
import Markdown (renderMarkdown)
import Templates (layout)


-- | Path to the Markdown files (now local to the project)
getDocDir :: IO FilePath
getDocDir = do
    cwd <- getCurrentDirectory
    pure (cwd ++ "/docs")

main :: IO ()
main = S.scotty 8080 $ do

    -- Home: redirect to first chapter
    S.get "/" $ do
        dir <- liftIO getDocDir
        chapters <- liftIO $ listChapters dir
        case chapters of
          [] ->
            S.text "Nenhum arquivo .md encontrado na pasta /docs."
          (c:_) ->
            S.redirect (TL.fromStrict (T.pack ("/" ++ c)))

    -- Serve a specific chapter
    S.get "/:chapter" $ do
        chapterParam <- S.pathParam "chapter"  -- Lazy Text
        dir <- liftIO getDocDir
        chapters <- liftIO $ listChapters dir

        let chapterName = T.unpack (TL.toStrict chapterParam)

        if chapterName `notElem` chapters
        then do
            S.status status404
            S.text "Capítulo não encontrado."
        else do
            content <- liftIO $ readChapter dir chapterName
            htmlContent <- liftIO $ renderMarkdown content
            S.html (TL.fromStrict (layout chapters chapterName htmlContent))

    -- Fallback
    S.notFound $ do
        S.status status404
        S.text "404 - O que você está procurando não está aqui."

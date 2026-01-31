{-# LANGUAGE OverloadedStrings #-}
module Markdown (renderMarkdown) where

import Text.Pandoc
import Data.Text (Text)
import qualified Data.Text as T

-- | Converts Markdown text to HTML using Pandoc.
renderMarkdown :: Text -> IO Text
renderMarkdown input = do
    result <- runIO $ do
        doc <- readMarkdown def input
        writeHtml5String def doc
    case result of
        Left err -> return $ "Error parsing markdown: " <> T.pack (show err)
        Right html -> return html

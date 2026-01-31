module Files (listChapters, readChapter) where

import System.Directory (listDirectory)
import System.FilePath (takeExtension, dropExtension)
import Data.List (sort, isSuffixOf)
import qualified Data.Text.IO as TIO
import Data.Text (Text)

-- | List all .md files in the documentation directory, sorted alphabetically.
listChapters :: FilePath -> IO [String]
listChapters path = do
    files <- listDirectory path
    return $ sort [ dropExtension f | f <- files, ".md" `isSuffixOf` f ]

-- | Read the content of a specific chapter.
readChapter :: FilePath -> String -> IO Text
readChapter baseDir name = TIO.readFile (baseDir ++ "/" ++ name ++ ".md")

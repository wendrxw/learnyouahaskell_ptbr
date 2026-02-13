module Files (listChapters, readChapter, listOrderedChapters) where

import System.Directory (listDirectory)
import System.FilePath (takeExtension, dropExtension)
import Data.List (sort, isSuffixOf)
import qualified Data.Text.IO as TIO
import Data.Text (Text)

-- | Explicitly ordered chapter list
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

-- | List all .md files in the documentation directory, sorted alphabetically.
listChapters :: FilePath -> IO [String]
listChapters path = do
    files <- listDirectory path
    return $ sort [ dropExtension f | f <- files, ".md" `isSuffixOf` f ]

-- | List chapters in the correct order.
listOrderedChapters :: IO [String]
listOrderedChapters = pure orderedChapters

-- | Read the content of a specific chapter.
readChapter :: FilePath -> String -> IO Text
readChapter baseDir name = TIO.readFile (baseDir ++ "/" ++ name ++ ".md")

{-# LANGUAGE OverloadedStrings #-}
module Templates (layout) where

import Data.Text (Text)
import qualified Data.Text as T

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

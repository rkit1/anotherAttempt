{-# LANGUAGE OverloadedStrings #-}
module Path where
import qualified Data.Text as T


type Path = [T.Text]
f ("news":xs) = article xs
f [p] | ".htm" `T.isSuffixOf` p = mainPage p
f x = copy x


article = undefined
mainPage = undefined
copy = undefined
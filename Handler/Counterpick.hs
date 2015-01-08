module Handler.Counterpick where

import Import

getCounterpickR :: [String] -> Handler Html
getCounterpickR strings = defaultLayout [whamlet|<h1>#{show strings}|]

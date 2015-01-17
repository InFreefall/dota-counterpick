module Handler.Counterpick where

import Dotabuff
import Import

getCounterpickR :: [String] -> Handler Html
getCounterpickR strings = do
  pair_list <- liftIO $ name_list_to_pair_list strings
  defaultLayout [whamlet|<h1>#{show pair_list}|]

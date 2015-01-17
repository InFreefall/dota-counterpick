{-# LANGUAGE OverloadedStrings #-}

module Dotabuff where

import Control.Applicative
import Control.Arrow
import Control.Lens hiding (element)
import Control.Monad
import qualified Data.ByteString.Lazy.Char8 as L
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid
import qualified Data.Text as T
import Network.HTTP.Conduit
import Prelude
import Text.HTML.DOM (parseLBS)
import Text.XML.Cursor

-- DOTA API request url
-- https://api.steampowered.com/IDOTA2Match_570/GetMatchHistory/V001/?key=5ABFD7C9A3A2754A0724E7B99581F24D

-- Because Emacs is stupid about !!
nth = (!!)

rename_rules = M.fromList [
    ("Outhouse Decorator", "Outworld Devourer")
  , ("Furion", "Nature's Prophet")
  , ("Tree", "Treant Protector")
  , ("Kotl", "Keeper of the Light")
  , ("Buttseeker", "Bloodseeker")
  ]

data Hero = Hero T.Text T.Text
          deriving Show

heroes_url = "http://www.dotabuff.com/heroes"

find_hero :: Cursor -> [Cursor]
find_hero = element "div" >=> attributeIs "class" "hero-grid" >=> child

cursorFor url = do
  page <- simpleHttp url
  return $ fromDocument $ parseLBS page

find_hero_name :: Cursor -> [Cursor]
find_hero_name = element "div" >=> attributeIs "class" "name"

hero_cursor_to_name c = head $ head (c $// find_hero_name) $/ content
hero_cursor_to_url = head . attribute "href"

 -- TEST DATA
_children :: IO [Cursor]
_children = do
  c <- cursorFor heroes_url
  return $ c $// find_hero
  
all_names = ((hero_cursor_to_name &&& hero_cursor_to_url) <$>) <$> _children
all_heroes = (map (\(name, url) -> Hero name ("http://www.dotabuff.com" <> url))) <$> all_names

hero_to_counterpicks :: Hero -> IO [(String, Double)]
hero_to_counterpicks (Hero name url) = do
  c <- cursorFor $ T.unpack (url <> "/_versus")
  let sections = c $// element "section"
  unless (length sections > 1) $ error ("Could not get counterpicks list for " <> T.unpack name)
  let worst_section = sections !! 1
      trs = tail $ worst_section $// element "tr"
      tr_to_counterpick tr = do
        let tds = tr $// element "td"
        if length tds < 2
           then Nothing
          else Just $ (T.unpack . head $ tds !! 1 $// content,
                       read . init . T.unpack . head $ tds !! 2 $/ content)
  return (map fromJust . filter isJust $ map tr_to_counterpick trs)

index_for_name name_before_rename =
  let
    name = case M.lookup name_before_rename rename_rules of
      Nothing -> name_before_rename
      Just n -> n
    matches name' = name == name'
  in do
    names <- map fst <$> all_names
    return . head . map snd $ filter (matches . fst) (names `zip` [0..])

f n = do
  (name, _) <- (!! n) <$> all_names
  putStrLn $ T.unpack name
  counters <- ((!! n) <$> all_heroes) >>= hero_to_counterpicks
  forM_ counters $ \(counterName, chance) ->
    putStrLn ("  " <> counterName <> ": " <> show (abs chance) <> "%")

g name = do
  index <- index_for_name name
  f index

name_list_to_pair_list :: [String] -> IO [(String, Double)]
name_list_to_pair_list enemies = let
  make_enemy_map :: String -> IO (M.Map String Double)
  make_enemy_map enemy = do
    index <- index_for_name (T.pack enemy)
    counters <- ((!! index) <$> all_heroes) >>= hero_to_counterpicks
    return $ M.fromList counters
  enemy_maps :: IO [M.Map String Double]
  enemy_maps = sequence $ map make_enemy_map enemies
  counter_map = return. (foldr (M.unionWith (+)) M.empty) =<< enemy_maps
  counter_list_unsorted = M.toList <$> counter_map
  in sortBy (\x y -> compare (snd x) (snd y)) <$> counter_list_unsorted

enemy_team :: [String] -> IO ()
enemy_team enemies = do
  putStrLn "Counterpicks:"
  name_list_to_pair_list enemies >>= (mapM_ $ \(name, percentage) ->
                                    putStrLn ("  " <> name <> ": " <> show percentage))

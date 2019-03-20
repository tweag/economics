{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Lens (to, only,(^?),ix, toListOf)
import Control.Arrow ((&&&), (***))
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import Data.Text (Text)
import Data.Text.Encoding.Error (lenientDecode)
import Data.Text.Lazy.Encoding (decodeUtf8With)
import Text.Taggy (Node)
import Text.Taggy.Lens (html, elements, children, contents,
                        allNamed, element, attrs)
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as H
import Data.Maybe (catMaybes)


data Upload =
  Upload { ele  :: Text
         , time :: Text
         , temp :: Text
         }
  deriving (Show)

trkpt :: [Node] -> Maybe Upload
trkpt row = do
  ele    <- row ^? ix 0 . contents
  time   <- row ^? ix 1 . contents
  temp   <- row ^? ix 2 . elements . elements . contents
  return $ Upload ele time temp

temperatures :: ByteString -> [Maybe Upload]
temperatures = toListOf
               $ to (decodeUtf8With lenientDecode)
               . html . allNamed (only "trkpt") . children . to trkpt

positions :: ByteString -> [HashMap Text Text]
positions = toListOf
               $ to (decodeUtf8With lenientDecode)
               . html . allNamed (only "trkpt") . attrs

main :: IO ()
main = do
  ts <- BS.readFile "/Users/dom/Downloads/activity_3457805023.gpx"
  let foo = temperatures ts
  let bar = sequence foo
  case bar of
    Nothing -> putStrLn "Nothing"
    Just us -> putStrLn $ show $ maximum $ map temp us
  let baz = positions ts
  let f :: HashMap Text Text -> Maybe (Text, Text)
      f m = do
        lat <- H.lookup "lat" m
        lon <- H.lookup "lon" m
        return (lat, lon)
  let g :: (Maybe (Text, Text), Maybe Upload) -> Maybe ((Text, Text), Upload)
      g (posM, uM) = do
        pos <- posM
        u   <- uM
        return (pos, u)
  let urk :: ByteString -> [((Text, Text), Upload)]
      urk = catMaybes . map g .
            map (f *** id) . uncurry zip . (positions &&& temperatures)
  error $ take 1000 $ show $ urk ts
  return ()

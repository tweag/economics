{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind  #-}
{-# OPTIONS_GHC -fno-warn-orphans         #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing  #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import Control.Lens (to, only,(^?),ix, toListOf, (^.))
import Control.Arrow ((&&&), (***))
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding.Error (lenientDecode)
import Data.Text.Lazy.Encoding (decodeUtf8With)
import Text.Taggy (Node)
import Text.Taggy.Lens (html, elements, children, contents,
                        allNamed, attrs)
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as H
import Data.Maybe (catMaybes)
import Frames
import Frames.CSV
import Frames.ShowCSV
import Data.Vinyl (Rec(..))
import Data.Time.Clock(UTCTime(..))
import Data.Time.Format
import Data.Time.LocalTime hiding (TimeOfDay)
import qualified Language.R as R
import Language.R.QQ


declareColumn "latitude" ''Double
declareColumn "longitude" ''Double
declareColumn "timeOfDay" ''UTCTime
declareColumn "temperature" ''Double

data TimeTemp =
  TimeTemp { ele  :: Text
           , time :: Text
           , temp :: Text
           }
  deriving (Show)

data Pos =
  Pos { lat :: Text
      , lon :: Text
      }
  deriving (Show)

trkpt :: [Node] -> Maybe TimeTemp
trkpt row = do
  ele    <- row ^? ix 0 . contents
  time   <- row ^? ix 1 . contents
  temp   <- row ^? ix 2 . elements . elements . contents
  return $ TimeTemp ele time temp

temperatures :: ByteString -> [Maybe TimeTemp]
temperatures = toListOf
               $ to (decodeUtf8With lenientDecode)
               . html . allNamed (only "trkpt") . children . to trkpt

positions :: ByteString -> [HashMap Text Text]
positions = toListOf
               $ to (decodeUtf8With lenientDecode)
               . html . allNamed (only "trkpt") . attrs

tFormat :: String
tFormat = "%Y-%m-%dT%H:%M:%S%QZ"

tFormatR :: String
tFormatR = "%Y-%m-%d %H:%M:%S"

pTime :: Monad m => String -> m UTCTime
pTime = parseTimeM True defaultTimeLocale tFormat

instance ShowCSV UTCTime where
  showCSV = T.pack . formatTime defaultTimeLocale tFormat

f :: HashMap Text Text -> Maybe Pos
f m = do
  lat <- H.lookup "lat" m
  lon <- H.lookup "lon" m
  return $ Pos lat lon

g :: (Maybe Pos, Maybe TimeTemp) -> Maybe (Pos, TimeTemp)
g (posM, uM) = do
  pos <- posM
  u   <- uM
  return (pos, u)

h :: (Pos, TimeTemp) -> Record '[Latitude, Longitude, TimeOfDay, Temperature]
h (p, tt) = case pTime (T.unpack (time tt)) of
              Nothing -> error "Failed to parse time"
              Just u  -> (read (T.unpack (lat p))) &:
                         (read (T.unpack (lon p))) &:
                         u &: (read (T.unpack (temp tt))) &: RNil

preCSVs :: ByteString -> [Record '[Latitude, Longitude, TimeOfDay, Temperature]]
preCSVs = map h . catMaybes . map g .
          map (f *** id) . uncurry zip . (positions &&& temperatures)

analyseGpx :: String -> String -> String -> IO ()
analyseGpx fName dName cName = do
  ts <- BS.readFile fName
  let csvs = preCSVs ts
  writeCSV cName csvs
  let temps :: [Double]
      temps = fmap (^. temperature) csvs
      tods :: [String]
      tods  = fmap (formatTime defaultTimeLocale tFormatR) $
              fmap (utcToZonedTime (minutesToTimeZone (6 * 60 + 30))) $
              fmap (^. timeOfDay) csvs
  R.runRegion $ do
    [r| library(ggplot2) |]
    fig0 <- [r| ggplot() |]
    df <- [r| x = as.POSIXct(tods_hs, format = tFormatR_hs, tz = "PST")
              data.frame(time = x, temperature = temps_hs) |]
    [r| print(head(df_hs)) |]
    fig <- [r| fig0_hs + geom_path(data = df_hs, aes(x = time, y = temperature)) |]
    [r| png(filename=dName_hs) |]
    [r| print(fig_hs) |]
    [r| dev.off() |]
    return ()


main :: IO ()
main = do
  analyseGpx "/Users/dom/Downloads/activity_3457805023.gpx" "diagrams/temperature.png" "Burma.csv"
  analyseGpx "/Users/dom/Downloads/activity_3439353899.gpx" "diagrams/temperature1.png" "Burma1.csv"




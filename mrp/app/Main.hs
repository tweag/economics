{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DataKinds, FlexibleContexts, TemplateHaskell, TypeOperators, TypeSynonymInstances, FlexibleInstances, QuasiQuotes, UndecidableInstances, ExplicitForAll, ScopedTypeVariables, OverloadedStrings, GADTs #-}

module Main where

import Control.Lens

import Data.Foldable
import Frames hiding ( (:&) )

import Data.Vinyl (Rec(..))
import Data.Vinyl.TypeLevel

import Data.Text as T
import Data.Proxy
import Foo


tableTypesText "StateLevel" "/Users/dom/Dropbox/Tidy/Economics/mrp/data/statelevel.csv"
tableTypesText "Marriage"   "/Users/dom/Dropbox/Tidy/Economics/mrp/data/marriageData.csv"

loadStateLevel :: IO (Frame StateLevel)
loadStateLevel = inCoreAoS (readTable "/Users/dom/Dropbox/Tidy/Economics/mrp/data/statelevel.csv")

loadMarriage :: IO (Frame Marriage)
loadMarriage = inCoreAoS (readTable "/Users/dom/Dropbox/Tidy/Economics/mrp/data/marriageData.csv")

-- FIXME: Maybe use type application?
type MiniMarriage = Record '[ RaceWbh, AgeCat, EduCat, Female, StateInitnum
                            , State, RegionCat, Region, Statename, Poll
                            , YesOfAll
                            ]

miniMarriage :: Marriage -> MiniMarriage
miniMarriage = rcast

type BiggerMarriage = Record '[ RaceWbhI, AgeCatI, EduCatI, StateInitnumI,
                                RaceWbh, AgeCat, EduCat, Female, StateInitnum
                              , State, RegionCat, Region, Statename, Poll
                              , YesOfAll
                              ]

type MiniMarriageTyped = Record '[ RaceWbhI, AgeCatI, EduCatI, Female, StateInitnumI
                                 , State, RegionCat, Region, Statename, Poll
                                 , YesOfAll
                                 ]

miniMarriageTyped :: BiggerMarriage -> MiniMarriageTyped
miniMarriageTyped = rcast


type RaceWbhI = "raceWbhI" :-> Int

raceWbhI :: (Functor f, (RElem RaceWbhI rs (RIndex RaceWbhI rs))) =>
        (Int -> f Int) -> Record rs -> f (Record rs)
raceWbhI = rlens (Proxy :: Proxy RaceWbhI)

getRaceWbh :: MiniMarriage -> Record '[RaceWbhI]
getRaceWbh x = pure (Col ((read $ unpack (x ^. raceWbh)) :: Int)) :& Nil

type AgeCatI = "ageCatI" :-> Int

ageCatI :: (Functor f, (RElem AgeCatI rs (RIndex AgeCatI rs))) =>
        (Int -> f Int) -> Record rs -> f (Record rs)
ageCatI = rlens (Proxy :: Proxy AgeCatI)

getAgeCat :: MiniMarriage -> Record '[AgeCatI]
getAgeCat x = pure (Col ((read $ unpack (x ^. ageCat)) :: Int)) :& Nil

type EduCatI = "eduCatI" :-> Int

eduCatI :: (Functor f, (RElem EduCatI rs (RIndex EduCatI rs))) =>
        (Int -> f Int) -> Record rs -> f (Record rs)
eduCatI = rlens (Proxy :: Proxy EduCatI)

getEduCat :: MiniMarriage -> Record '[EduCatI]
getEduCat x = pure (Col ((read $ unpack (x ^. eduCat)) :: Int)) :& Nil

type StateInitnumI = "stateInitnumI" :-> Int

stateInitnumI :: (Functor f, (RElem StateInitnumI rs (RIndex StateInitnumI rs))) =>
        (Int -> f Int) -> Record rs -> f (Record rs)
stateInitnumI = rlens (Proxy :: Proxy StateInitnumI)

getStateInitnum :: MiniMarriage -> Record '[StateInitnumI]
getStateInitnum x = pure (Col ((read $ unpack (x ^. stateInitnum)) :: Int)) :& Nil

intFieldDoubler :: Record '[StateInitnumI, RaceWbhI, AgeCatI, EduCatI] ->
                   Record '[StateInitnumI, RaceWbhI, AgeCatI, EduCatI]
intFieldDoubler = mapMono (+ (-1))

main :: IO ()
main = do
  ms <- loadMarriage
  let msClean = filterFrame
                (\x -> x ^. raceWbh      /= "NA" &&
                       x ^. ageCat       /= "NA" &&
                       x ^. eduCat       /= "NA" &&
                       x ^. stateInitnum /= "NA")
                ms
  let biggerMarriage :: Frame BiggerMarriage
      biggerMarriage = fmap (\x -> (getRaceWbh x)      `rappend`
                                   (getAgeCat x)       `rappend`
                                   (getEduCat x)       `rappend`
                                   (getStateInitnum x) `rappend` x) $
                       fmap miniMarriage msClean
  mapM_ print $ Prelude.take 6 $ toList $
    fmap (rsubset %~ intFieldDoubler) $
    fmap miniMarriageTyped biggerMarriage
  return ()


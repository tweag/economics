{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveGeneric #-}
import Control.DeepSeq
import qualified Data.Vector.SEXP as SV
import Language.R.Matcher (Matcher)
import qualified Language.R.Matcher as P
import Language.R.HExp as H
import qualified Data.ByteString as B
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Foreign.R as R
import Language.R.Instance as R
import Data.Maybe
import Data.Traversable
import Data.Word
import Control.Applicative

import Language.R.Instance as R
import Language.R.Literal as R
import Language.R.QQ
import H.Prelude

import Control.Monad.IO.Class
import Control.Memory.Region
import Data.Monoid
import Foreign.R.Internal as R
import Foreign.Ptr
import Foreign.Storable
import qualified Foreign.R as R

import GHC.Generics hiding (R)

data Value
 = T Text
 | I Int
 | D Double
 | B Bool
 deriving (Show, Generic)

instance NFData Value

data DF = DF
  { colNames :: [Text]
  , rowNames :: [Text]
  , colValues :: [[Value]]
  } deriving (Show, Generic)

instance NFData DF


toDF :: Matcher s DF
toDF = do
  P.s3 ["data.frame"]
  ns <- fmap T.pack . fromMaybe [] <$> optional P.names
  rs <- fmap T.pack . fromMaybe [] <$> optional P.rownames
  vs <- P.hexp R.SVector $ \(Vector _ v) -> for (SV.toList v) $ \e -> P.with e
          (P.choice [parseFactor, mkVector1])
  pure $ DF ns rs vs

mkVector1 = do
  mdim <-  optional P.dim
  v <- P.choice
         [ P.hexp R.SReal $ \(Real v) -> return $ D <$> SV.toList v
         , P.hexp R.SInt  $ \(Int v) -> return $ I . fromIntegral <$> SV.toList v
         -- , P.hexp R.SLogical $ \(Logical v) -> return $ fromLogical <$> SV.toList v
         , P.hexp R.SString $ \(String v) ->
           return $ (\(hexp -> Char p) -> T (rCharToText p)) <$> SV.toList v
         ]
  pure v

parseFactor :: Matcher s [Value]
parseFactor = do
  P.s3 ["factor"]
  levels <- P.charList <$> P.attribute R.SString "levels"
  P.hexp R.SInt $ \(Int v) ->
    pure $ (\i ->
      if i > 0 then T $ T.pack $ levels !! (fromIntegral i - 1) else T "") <$> SV.toList v


rCharToText :: SV.Vector s 'R.Char Word8 -> Text
rCharToText v = bytesToText $ SV.toList v

bytesToText :: [Word8] -> Text
bytesToText = TE.decodeUtf8 . B.pack


main = R.withEmbeddedR R.defaultConfig $ do
  x <- R.runRegion $ do
    [r| library("foreign") |]
    z <- [r| marriaage.data <- read.dta("http://www.princeton.edu/~jkastell/MRP_primer/gay_marriage_megapoll.dta", convert.underscore = TRUE) |]
    Right r <- P.matchOnly toDF z
    pure r
  print (x::DF)

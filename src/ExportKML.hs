-- Export the places data into KML format
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveGeneric #-} -- for arguments parsing
{-# LANGUAGE DataKinds #-} -- for arguments documentation
{-# LANGUAGE TypeOperators #-} -- for arguments documentation
{-# LANGUAGE FlexibleInstances  #-}  -- To instanciate ParseRecord for Args Wrapped
{-# LANGUAGE StandaloneDeriving #-}  -- To derive Show for Args Unwrapped

import Control.Monad
import Data.Aeson (eitherDecode)
import Data.Either.Combinators (fromRight')
import Data.Maybe (fromJust)
import Data.Text (Text, pack)
import Place (Place(..))
import Prelude hiding (writeFile)
import qualified Data.ByteString.Lazy.UTF8 as BLU
import qualified Data.Map as Map
import Options.Generic
import System.IO (openFile, hSetEncoding, utf8, hGetContents, IOMode(ReadMode))
import Text.Hamlet.XML
import Text.XML

data Args w = Args
    { infile :: w ::: String <?> "Filepath to get places data from (JSON format)"
    , outfile :: w ::: String <?> "Filepath to write map data to (KML format)"
    } deriving (Generic)

instance ParseRecord (Args Wrapped)
deriving instance Show (Args Unwrapped)

main :: IO ()
main = do
  args <- unwrapRecord "Export the places data into a KML format map"
  inputHandle <- openFile (infile args) ReadMode
  hSetEncoding inputHandle utf8
  content <- hGetContents inputHandle
  let contentLines = (lines content) :: [String]
      places = map parseFromJSON contentLines
   in writeFile def (outfile args) $ makeDoc places

parseFromJSON :: String -> Place
parseFromJSON line =
  fromRight' $ (eitherDecode utf8line :: Either String Place)
  where utf8line = BLU.fromString line

-- Creates the XML document with our placemarks
makeDoc places =
  Document (Prologue [] Nothing []) root []
  where kmlAttrs = Map.fromList [("xmlns", "http://www.opengis.net/kml/2.2")]
        root = Element "kml" kmlAttrs [xml|
<Document>
    <name>Highlighted Icon</name>
    <description>Place your mouse over the icon to see it display the new icon</description>
    ^{styleNodes}
    $forall place <- places
          ^{placemarkNodes place}
|]

ratingToStyleData :: Map.Map Integer (String, String)
ratingToStyleData = Map.fromList [
  (5, ("fiveStarsPlacemark", "0212f9")),
  (4, ("fourStarPlacemark",  "025df9")),
  (3, ("threeStarPlacemark", "028ef9")),
  (2, ("twoStarPlacemark",   "02c0f9")),
  (1, ("oneStarPlacemark",   "02f9f9"))
  ]

styleNodes :: [Node]
styleNodes = (Map.toList ratingToStyleData) >>= (\(_, d) -> styleNode d)

styleNode :: (String, String) -> [Node]
styleNode (name, colorCode) = [xml|
<Style id="#{pack $ name}">
  <IconStyle>
    <color>
      ff#{pack $ colorCode}
    <scale>
      1
    <Icon>
      <href>
        http://www.gstatic.com/mapspro/images/stock/503-wht-blank_maps.png
    <hotSpot x="32" xunits="pixels" y="64" yunits="insetPixels">
  <LabelStyle>
    <scale>
      1
|]

placemarkNodes :: Place -> [Node]
placemarkNodes (Place {name=name, rating=rating, lat=lat, lng=lng, url=url, desc=desc}) = [xml|
<Placemark>
  <name> #{pack $ name}
  <description> #{pack $ description}
  <styleUrl> ##{pack $ styleUrl}
  <Point>
    <coordinates>
      #{pack $ coords}
|]
  where description = desc ++ " " ++ url
        styleUrl = fst $ fromJust $ Map.lookup rating ratingToStyleData
        coords = (show lng) ++ "," ++ (show lat) ++ ",0"

-- Export the places data into KML format
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

import Control.Monad
import Data.Aeson (eitherDecode)
import Data.Either.Combinators (fromRight')
import Data.Maybe (fromJust)
import Data.Text (Text, pack)
import Place (Place(..))
import Prelude hiding (writeFile)
import qualified Data.ByteString.Lazy.UTF8 as BLU
import qualified Data.Map as Map
import System.IO (openFile, hSetEncoding, utf8, hGetContents, IOMode(ReadMode))
import Text.Hamlet.XML
import Text.XML

main :: IO ()
main = do
  inputHandle <- openFile "places.json" ReadMode
  hSetEncoding inputHandle utf8
  content <- hGetContents inputHandle
  let contentLines = (lines content) :: [String]
      places = map parseFromJSON contentLines
   in writeFile def "map.kml" $ makeDoc places

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
    <Style id="fiveStarsPlacemark">
      <IconStyle>
        <Icon>
          <href>http://maps.google.com/mapfiles/kml/paddle/red-stars.png</href>
    <Style id="normalPlacemark">
      <IconStyle>
        <Icon>
          <href>http://maps.google.com/mapfiles/kml/paddle/wht-blank.png</href>
    $forall place <- places
          ^{placemarkNodes place}
|]

placemarkNodes :: Place -> [Node]
placemarkNodes (Place {name=name, rating=rating, lat=lat, lng=lng, url=url, desc=desc}) = [xml|
<Placemark>
  <name> #{pack $ name}
  <styleUrl> #{pack $ style}
  <Point>
    <coordinates>
      #{pack $ coords}
|]
  where style = if rating == 5 then "#fiveStarsPlacemark" else "#normalPlacemark"
        coords = (show lng) ++ "," ++ (show lat) ++ ",0"

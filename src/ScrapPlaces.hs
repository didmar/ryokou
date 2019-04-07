-- Dumps the places data in JSON format on stdout
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import Control.Applicative
import Control.Monad
import Data.Aeson (encode)
import Data.List (isPrefixOf, isSuffixOf, nub, intersperse)
import Data.Maybe (fromJust)
import qualified Data.ByteString.Lazy.Char8 as BL
import System.IO (hPutStrLn, stderr)
import Text.HTML.Scalpel
import Text.Regex.TDFA

import Place (Place(..))

siteURL = "https://www.japanhoppers.com"

-- Only scraping one region at a time
region = "kyushu_okinawa"

main :: IO ()
main = do
  cityLinks <- scrapCityLinks
  forM_ cityLinks $ \cityLink -> do
    hPutStrLn stderr $ "Scraping city ... " ++ show cityLink
    placeLinks <- scrapPlaceLinks cityLink
    forM_ placeLinks $ \placeLink ->
      let city = BL.unpack $ last $ BL.split '/'$ fromJust $ BL.stripSuffix "/" $ BL.pack cityLink
       in do
        hPutStrLn stderr $ "Scraping place ... " ++ show placeLink
        place <- scrapPlace city placeLink
        BL.putStrLn (encode place :: BL.ByteString)

-- link prefix will be something like /en/kyushu_okinawa/fukuoka/kanko/1850/
scrapPlace :: String -> String -> IO Place
scrapPlace city linkPrefix = do
  scrapedPlace <- scrapeURL url (placeScraper city url)
  return $ fromJust scrapedPlace
  where url = siteURL ++ linkPrefix

placeScraper :: String -> String -> Scraper String Place
placeScraper city url = do
  (lat, lng) <- gpsScraper
  tableEntries <- tableEntriesScraper
  desc <- attr "content" $ "meta" @: ["name" @= "description"]
  let (name, rating) = extractTableEntries tableEntries
   in return Place {name=name, rating=rating, lat=lat, lng=lng, url=url, desc=desc, city=city}

gpsScraper :: Scraper String (Float, Float)
gpsScraper = do
    ggmapURL <- chroot "iframe" (attr "src" "iframe")
    return $ parseGGMapURL ggmapURL

data TableEntry = SightEntry String | RatingEntry Integer | OtherEntry deriving (Show)

-- There is probable a cleaner way to do this ...
extractTableEntries :: [TableEntry] -> (String, Integer)
extractTableEntries ts = (fromJust maybeName, fromJust maybeRating)
  where
    (maybeName, maybeRating) = foldr extractTableEntries' (Nothing, Nothing) ts

    extractTableEntries' :: TableEntry -> (Maybe String, Maybe Integer) -> (Maybe String, Maybe Integer)
    extractTableEntries' t (name, rating) =
      case t of
        SightEntry name' -> (Just name', rating)
        RatingEntry rating' -> (name, Just rating')
        _ -> (name, rating)

tableEntriesScraper :: Scraper String [TableEntry]
tableEntriesScraper = chroots (("section" @: ["id" @= "spot_basic"]) // "table" // "tr") tableEntryScraper

tableEntryScraper :: Scraper String TableEntry
tableEntryScraper = do
  title <- text "th"
  case title of
    "Sights" -> do
      placeName <- text "td"
      return $ SightEntry placeName

    "Rating" -> do
      ratingStr <- attr "content" "meta"
      let rating = read ratingStr :: Integer
       in return $ RatingEntry rating

    _ -> return OtherEntry

-- example: https://www.google.com/maps/d/embed?mid=1yIZIBqPVxvr8y3yeaGjlmUlmyI2mwNzL&amp;ll=\t\n33.521379, 130.535388&amp;z=16&amp;hl=en
parseGGMapURL :: String -> (Float, Float)
parseGGMapURL url =
  (lat, lng)
  where
      pat = "[0-9]+\\.[0-9]+" :: String
      matches = url =~ ("[0-9]+\\.[0-9]+" :: String) :: AllTextMatches [] String
      latStr:lngStr:_ = getAllTextMatches matches
      lat = read latStr :: Float
      lng = read lngStr :: Float

-- cityLink will be something like /en/kyushu_okinawa/fukuoka/
-- returns a place link such as /en/kyushu_okinawa/fukuoka/kanko/1850/
scrapPlaceLinks :: String -> IO [String]
scrapPlaceLinks cityLink = do
  links <- scrapLinks (cityLink ++ "kanko/")
  -- Filter out /en/kyushu_okinawa/fukuoka/kanko/ from the places url
  return $ filter (not . ("kanko/" `isSuffixOf`)) links

scrapCityLinks :: IO [String]
scrapCityLinks = do
  links <- scrapLinks url
  return $ filter ((/=) url) links
  where url = "/en/" ++ region ++ "/"

-- Finds all the <a href="..."> and filters on a prefix
scrapLinks :: String -> IO [String]
scrapLinks linkPrefix = do
  scrapedLinks <- scrapeURL (siteURL ++ linkPrefix) links
  return $ nub $ fromJust scrapedLinks
  where
      links :: Scraper String [String]
      links = chroots "a" linkScraper

      linkScraper :: Scraper String String
      linkScraper = do
          link <- attr "href" "a"
          guard (linkPrefix `isPrefixOf` link)
          return link

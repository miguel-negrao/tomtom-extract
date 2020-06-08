{--
    Copyright 2019 Miguel Negrão

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>.
--}

{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (replicateM, guard, forM_, void)
import System.Environment (getArgs)
import Data.Word
import Data.Either
import Text.Printf
import qualified Data.List as L

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Builder as BB

import qualified Data.Binary.Get as G

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Encoding as E

import Data.Geo.GPX
import Data.Geo.GPX.Type.Wpt

import Text.XML.HXT.Core

import Text.CSV

import Control.Lens

--"Precision", "Type of Location", "Longitude 1", "Latitude 1", "Longitude 2", "Latitude 2", "Labels"
type TomTomRecord = (Word8, Word8, Double, Double, Double, Double, T.Text, T.Text, T.Text)  

-- https://stackoverflow.com/questions/12876384/grouping-a-list-into-lists-of-n-elements-in-haskell
group :: Int -> [a] -> [[a]]
group _ [] = []
group n l
  | n > 0 = take n l : group n (drop n l)
  | otherwise = error "Negative or zero n"

bytes :: [Word8] -> G.Get ()
bytes xs = do
    p <- replicateM (length  xs) G.getWord8
    guard $ p == xs

getTomTomString :: G.Get T.Text
getTomTomString = do
    initialSize <- G.getWord8
    actualSize <- case initialSize of
        0x08 -> fromIntegral <$> G.getWord16le
        _    -> return $ fromIntegral initialSize
    bs <- G.getByteString (actualSize - 0x1B)
    return $ E.decodeLatin1 bs

mapSettingsCfgLocationParser :: G.Get TomTomRecord
mapSettingsCfgLocationParser = do
    bytes [0x04, 0x00]
    precision <- G.getWord8
    guard $ precision <= 4
    bytes [0x00, 0x00, 0x00, 0x04, 0x00]
    typeOfLoc <- G.getWord8
    bytes [0x00, 0x00, 0x00, 0x08, 0x00]
    long <- G.getInt32le 
    lat <- G.getInt32le
    bytes [0x08, 0x00]
    long2 <- G.getInt32le
    lat2 <- G.getInt32le 
    [a,b,c] <- replicateM 3 getTomTomString
    return (
            precision, typeOfLoc, 
            fromIntegral long / 100000.0, fromIntegral lat / 100000.0,
            fromIntegral long2 / 100000.0, fromIntegral lat2 / 100000.0,
            a,b,c)
    
tomtomToGPX :: TomTomRecord -> Wpt
tomtomToGPX (precision, typeOfLoc, long, lat, _, _, desc1, _, desc3) = wpt (latitude lat) (longitude long) Nothing Nothing Nothing Nothing (Just (showTypeOfLoc typeOfLoc)) Nothing (Just (T.unpack desc3)) Nothing [] Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

--formatBytes :: [Word8] -> String
--formatBytes bytes =  concat $ (\xs -> (concat xs) ++ " ") <$> (group 1 $ printf "%02X" <$> bytes)

showPrecision :: Word8 -> String
showPrecision 1 = "City centre"
showPrecision 2 = "Specified crossing on a street"
showPrecision 3 = "House number or premises"
showPrecision 4 = "Anywhere on a street"
showPrecision _ = "unkown value"

showTypeOfLoc :: Word8 -> String
showTypeOfLoc 1 = "Entered via map or postcode lookup"
showTypeOfLoc 3 = "Favourite"
showTypeOfLoc 4 = "Home location"
showTypeOfLoc 5 = "Entered via address"
showTypeOfLoc 6 = "Entered via POI"
showTypeOfLoc 7 = "Start of last calculated route"

header :: [String]
header = ["Precision", "Type of Location", "Longitude", "Latitude", "Longitude (Nearest road)", "Latitude (Nearest road)", "City", "Street/Place", "Adress/Place"]

toCSV :: [TomTomRecord] -> CSV
toCSV xs = header:rows
    where
        rows = fmap (\(precision, typeOfLoc, long1, lat1, long2, lat2, desc1, desc2, desc3) -> [showPrecision precision, showTypeOfLoc typeOfLoc, show long1, show lat1, show long2, show lat2, T.unpack desc1, T.unpack desc2, T.unpack desc3]) xs

tshow :: Show a => a -> T.Text
tshow = T.pack . show

toSimpleText :: [TomTomRecord] -> T.Text
toSimpleText xs = T.concat $ fmap f xs
    where 
        f (precision, typeOfLoc, long1, lat1, long2, lat2, desc1, desc2, desc3) = T.concat lines <> "\n" where
            lines = zipWith g (T.pack <$> header) [T.pack $ showPrecision precision, T.pack $  showTypeOfLoc typeOfLoc,
                                                    tshow long1, tshow lat1, tshow long2, tshow lat2, desc1, desc2, desc3]
        g a b = a <> ": " <> b <> "\n"

main :: IO ()
main = do
    args <- getArgs
    case args of
        [src, dst] -> do
            contents <- B.readFile src
            let
                starts :: [B.ByteString]
                starts = B.tails contents
                parsedStarts :: [Either (B.ByteString, G.ByteOffset, String) (B.ByteString, G.ByteOffset, TomTomRecord)]
                parsedStarts = G.runGetOrFail mapSettingsCfgLocationParser <$> starts
                locations :: [TomTomRecord]
                locations = parsedStarts ^.. each . _Right . _3
                sortedLocations = L.sortOn (view _2) locations
                gpxData :: GPX
                gpxData = gpx "1.0" "tomtom-extract" Nothing (tomtomToGPX <$> sortedLocations) [] [] Nothing 
            writeFile (dst <> ".csv") $ printCSV $ toCSV sortedLocations
            void $ runX (constA gpxData
                        >>>
                        xpickleDocument xpickle [withIndent yes] (dst <> ".gpx"))
            TIO.writeFile (dst <> ".txt") $ toSimpleText sortedLocations
        _ -> putStrLn "usage: tomtom-extract <src file> <dst file prefix>"
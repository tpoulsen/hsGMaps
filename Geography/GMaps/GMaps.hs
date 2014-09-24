{-# LANGUAGE OverloadedStrings #-}

module Geography.GMaps.GMaps where

import Geography.GMaps.Types
import Control.Applicative      ((<$>))
import Control.Lens             ((^.))
import Data.List                (intercalate)
import Data.Monoid              ((<>))
import qualified Data.Text as T (Text)
import Network.Wreq

--TODO: Graceful error handling.

type APIKey = String

-- For requests with an API key. Function should be partially applied with API key in use.
geocodeAddressKey :: APIKey -> String -> IO GeocodeResponse
geocodeAddressKey k s = do
    let safeAddress = (intercalate "+" . words $ s)
    let q           = "https://maps.googleapis.com/maps/api/geocode/json?address=" <> safeAddress <> "&key=" <> k
    fmap (^. responseBody) . asJSON =<< get q

reverseGeocodeKey :: APIKey -> Location -> IO GeocodeResponse
reverseGeocodeKey k s = do
    let coords = intercalate "," . map show $ (s ^.) <$> [lat, lng]
    let q      = "https://maps.googleapis.com/maps/api/geocode/json?address=" <> coords <> "&key=" <> k
    fmap (^. responseBody) . asJSON =<< get q

-- Requests without API keys.
geocodeAddress :: String -> IO GeocodeResponse
geocodeAddress s = do
    let safeAddress = (intercalate "+" . words $ s)
    let q           = "https://maps.googleapis.com/maps/api/geocode/json?address=" <> safeAddress
    fmap (^. responseBody) . asJSON =<< get q

reverseGeocode :: Location -> IO GeocodeResponse
reverseGeocode s = do
    let coords = intercalate "," . map show $ (s ^.) <$> [lat, lng]
    let q      = "https://maps.googleapis.com/maps/api/geocode/json?address=" <> coords
    fmap (^. responseBody) . asJSON =<< get q

-- Tries to get Lat/Lng for response, if there aren't results, gets the status message.
getLatLng :: GeocodeResponse -> Either T.Text [Location]
getLatLng r = 
    let geocodeResults = map (^. geometry . location) $ r ^. results
    in  case geocodeResults of
            []        -> Left $ r ^. status
            otherwise -> Right geocodeResults 

getStatus :: GeocodeResponse -> T.Text
getStatus r = r ^. status
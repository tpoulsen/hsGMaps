{-# LANGUAGE OverloadedStrings #-}

module Geography.GMaps.Geocoder where

import Geography.GMaps.Types
import Control.Applicative      ((<$>))
import Control.Lens             ((^.), (.~), (&))
import Data.Monoid              ((<>))
import qualified Data.Text as T (Text, intercalate, pack)
import Network.Wreq

--TODO: Graceful error handling.

-- For requests with an API key. A key of "" is still valid.
geocodeAddress :: APIKey -> T.Text -> IO GeocodeResponse
geocodeAddress k s = do
    let p           = defaults & param "address" .~ [s] & param "key" .~ [k]
    let q           = "https://maps.googleapis.com/maps/api/geocode/json?"
    fmap (^. responseBody) . asJSON =<< getWith p q

reverseGeocode :: APIKey -> Location -> IO GeocodeResponse
reverseGeocode k s = do
    let coords = T.intercalate "," . map (T.pack . show) $ (s ^.) <$> [lat, lng]
    let p      = defaults & param "address" .~ [coords] & param "key" .~ [k]
    let q      = "https://maps.googleapis.com/maps/api/geocode/json?"
    fmap (^. responseBody) . asJSON =<< getWith p q

-- Convenience for applications without an API key.
geocodeAddress_ :: T.Text -> IO GeocodeResponse
geocodeAddress_ = geocodeAddress ""

reverseGeocode_ :: Location -> IO GeocodeResponse
reverseGeocode_ = reverseGeocode ""

-- Tries to get Lat/Lng for response, if there aren't results, gets the status message.
getLatLng :: GeocodeResponse -> Either T.Text [Location]
getLatLng r = 
    let geocodeResults = map (^. geometry . location) $ r ^. results
    in  case geocodeResults of
            []        -> Left $ r ^. status
            otherwise -> Right geocodeResults 

getFormattedAddress :: GeocodeResponse -> Either T.Text [T.Text]
getFormattedAddress r = 
    let addresses = map (^. formattedAddress) $ r ^. results
    in case addresses of
            []        -> Left $ r ^. status
            otherwise -> Right addresses
            
getStatus :: GeocodeResponse -> T.Text
getStatus r = r ^. status


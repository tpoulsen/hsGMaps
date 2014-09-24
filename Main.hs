{-# LANGUAGE OverloadedStrings #-}

import Geography.GoogleMapsAPI
import Control.Applicative     ((<$>))
import Control.Lens            ((^.))
import Data.List               (intercalate)
import Data.Monoid             ((<>))
import Network.Wreq

geocodeAddress :: String -> IO (Response GeocodeResponse)
geocodeAddress s = do
    let q = "http://maps.googleapis.com/maps/api/geocode/json?address=" <> (intercalate "+" . words $ s)
    asJSON =<< get q

reverseGeocode :: Location -> IO (Response GeocodeResponse)
reverseGeocode s = do
    let coords = intercalate "," . map show $ (s ^.) <$> [lat, lng]
    let q      = "http://maps.googleapis.com/maps/api/geocode/json?address=" <> coords
    asJSON =<< get q

getLatLng :: Response GeocodeResponse -> [Location]
getLatLng r = 
    let geocodeResults = r ^. responseBody ^. results
    in map (^. geometry . location) geocodeResults
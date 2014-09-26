{-# LANGUAGE OverloadedStrings #-}

module Geography.Geography 
    ( geoDistance
    , geoDistanceGeocoder
    ) where

import Geography.GMaps.Geocoder
import Geography.GMaps.Types
import Control.Applicative   (liftA, liftA2)
import Control.Lens          ((^.))
import qualified Data.Text as T (Text)

locationRadians :: Location -> (Double, Double)
locationRadians l = 
    let lat' = (l ^. lat) * pi/180
        lng' = (l ^. lng) * pi/180
    in (lat', lng')

-- Haversine formula; result in km.
geoDistance :: Location -> Location -> Double
geoDistance p1 p2 =
    let earthRadius    = 6371 --km
        (lat1r, lng1r) = locationRadians p1
        (lat2r, lng2r) = locationRadians p2
        deltaLat       = lat2r - lat1r
        deltaLng       = lng2r - lng1r
        halfDistSquare = sin deltaLat/2 * sin deltaLat/2 + cos lat1r * cos lat2r  * sin deltaLng/2 * sin deltaLng/2
        angularDist    = 2 * atan2 (sqrt halfDistSquare) (sqrt (1-halfDistSquare))
    in earthRadius * angularDist

geoDistanceGeocoder :: GeocodeResponse -> GeocodeResponse -> Either T.Text Double
geoDistanceGeocoder p1 p2 =
    let p1' = liftA head $ getLatLng p1
        p2' = liftA head $ getLatLng p2
    in liftA2 geoDistance p1' p2'
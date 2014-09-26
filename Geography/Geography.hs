{-# LANGUAGE OverloadedStrings #-}

module Geography.Geography 
    ( geoDistance
    ) where

import Geography.GMaps.Types
import Control.Lens ((^.))

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
{-# LANGUAGE OverloadedStrings #-}

import Test.HUnit
import Geography.GMaps.Geocoder
import Geography.GMaps.Types

zeroResultsFormatted = TestCase (do 
    r <- geocodeAddress "" "123 Fake St. Springfield USA"
    assertEqual "123 Fake St. Springfield, USA" (Left "ZERO_RESULTS") (getFormattedAddress r)
    )

oneResultFormatted = TestCase (do 
    r <- geocodeAddress "" "Camden Yards, Baltimore, MD"
    assertEqual "Camden Yards" (Right ["Sports Legends Museum at Camden Yards, 301 West Camden Street, Baltimore, MD 21201, USA"]) (getFormattedAddress r)
    )

tests = TestList [ TestLabel "123 Fake St -> ZERO_RESULTS" zeroResultsFormatted
                 , TestLabel "Camden Yards -> Address" oneResultFormatted
                 ] 

main = runTestTT tests
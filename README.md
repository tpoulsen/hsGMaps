#hsGMaps

Haskell interface for Google Maps API. Current focus is on Geocoding. 

Given an address (or a set of coordinates) and an API key (reccommended by Google; an empty string still works), will return an item of type GeocodeResponse. Convenience functions exist to access several fields, but any field can be accessed using Lenses.


Ex:
```Bash
ghci> :set -XOverloadedStrings
ghci> :m +Geography.GMaps.Geocoder

-- Example of successful request:
ghci> r <- geocodeAddress_ "Camden Yards, Baltimore, MD"

ghci> getLatLng r
Right [Location {_lat = 39.2849786, _lng = -76.6199528}]

ghci> getFormattedAddress r
Right ["Sports Legends Museum at Camden Yards, 301 West Camden Street, Baltimore, MD 21201, USA"]

-- Example of failure:
ghci> r2 <- geocodeAddress_ "123 Fake St. Springfield, USA"

ghci> getLatLng r2
Left "ZERO_RESULTS"

ghci> getFormattedAddress r2
Left "ZERO_RESULTS"

```
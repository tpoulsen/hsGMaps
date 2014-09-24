{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

--module HaskellMaps where

import Control.Applicative ((<$>), (<*>))
import Control.Monad       (mzero)
import Data.Aeson
import Data.Monoid
import GHC.Generics
import qualified Data.ByteString.Lazy.Internal as BS
import qualified Data.Text as T

--type Lat = BS.ByteString
--type Lng = BS.ByteString
--type Coordinates = (Lat, Lng)
type Coordinates = (T.Text, T.Text)

{-
instance FromJSON Ingredient where
  parseJSON (Object o) = Ingredient <$>
                          o .: "amount" <*>
                          o .: "unit"   <*>
                          o .: "ingredient" 
  parseJSON _          = mzero 
-}
data GeocodeResponse = GeocodeResponse
    { results :: [GeocodeResults]
    , status  :: T.Text
    } deriving (Show, Eq, Generic)
instance FromJSON GeocodeResponse 
{-where
    parseJSON (Object o) =
        GeocodeResponse <$>
                o .: "results" <*>
                o .: "status"
    parseJSON _  = mzero
-}
data GeocodeResults = GeocodeResults
    { addressComponents :: [AddressComponents]
    , formattedAddress  :: T.Text
    , geometry           :: [Coordinates]
    , rTypes              :: [T.Text]
    } deriving (Show, Eq, Generic)
instance FromJSON GeocodeResults 
{-where
    parseJSON (Object o) =
        GeocodeResults <$>
                o .: "address_components" <*>
                o .: "formatted_address"  <*>
                o .: "geometry"           <*>
                o .: "types"
    parseJSON _ = mzero
-}
data AddressComponents = AddressComponents
    { longName  :: T.Text
    , shortName :: T.Text
    , aTypes      :: [T.Text]
    } deriving (Show, Eq, Generic)
instance FromJSON AddressComponents 
{-where
    parseJSON (Object o) =
        AddressComponents <$>
                o .: "long_name"  <*>
                o .: "short_name" <*>
                o .: "types"
    parseJSON _ = mzero
-}
data Geometry = Geometry
    { location     :: Location
    , locationType :: T.Text
    , viewport     :: Viewport
    } deriving (Show, Eq, Generic)
instance FromJSON Geometry 
{-where
    parseJSON (Object o) = 
        Geometry <$>
                o .: "location"      <*>
                o .: "location_type" <*>
                o .: "viewport"
    parseJSON _ = mzero
-}
data Location = Location 
    { lat :: T.Text
    , lng :: T.Text
    } deriving (Show, Eq, Generic)
instance FromJSON Location 
{-where
    parseJSON (Object o) =
        Location <$>
                o .: "lat" <*>
                o .: "lng"
    parseJSON _ = mzero 
-}
data Viewport = Viewport
    { northeast :: Location
    , southwest :: Location
    } deriving (Show, Eq, Generic)
instance FromJSON Viewport 
{-where
    parseJSON (Object o) =
        Viewport <$>
                o .: "northeast" <*>
                o .: "southwest"
    parseJSON _ = mzero 
-}
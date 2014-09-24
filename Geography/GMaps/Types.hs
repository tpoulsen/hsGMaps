{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell   #-}

module Geography.GMaps.Types where

import Control.Applicative ((<$>), (<*>))
import Control.Monad       (mzero)
import Control.Lens
import Data.Aeson
import Data.Monoid
import GHC.Generics
import qualified Data.ByteString.Lazy.Internal as BS
import qualified Data.Text as T

type APIKey = T.Text

data AddressComponents = AddressComponents
    {  _longName  :: T.Text
    ,  _shortName :: T.Text
    ,  _aTypes    :: [T.Text]
    } deriving (Show, Eq, Generic)

makeLenses ''AddressComponents

instance FromJSON AddressComponents where
    parseJSON (Object o) =
        AddressComponents <$>
                o .: "long_name"  <*>
                o .: "short_name" <*>
                o .: "types"
    parseJSON _ = mzero
    
-----------------------------------------------------------
data Location = Location 
    {  _lat :: Double
    ,  _lng :: Double
    } deriving (Show, Eq, Generic)

makeLenses ''Location

instance FromJSON Location where
    parseJSON (Object o) =
        Location <$>
                o .: "lat" <*>
                o .: "lng"
    parseJSON _ = mzero 

-----------------------------------------------------------
data Viewport = Viewport
    {  _northeast :: Location
    ,  _southwest :: Location
    } deriving (Show, Eq, Generic)

makeLenses ''Viewport

instance FromJSON Viewport where
    parseJSON (Object o) =
        Viewport <$>
                o .: "northeast" <*>
                o .: "southwest"
    parseJSON _ = mzero 

-----------------------------------------------------------
data Geometry = Geometry
    {  _location     :: Location
    ,  _locationType :: T.Text
    ,  _viewport     :: Viewport
    } deriving (Show, Eq, Generic)

makeLenses ''Geometry

instance FromJSON Geometry where
    parseJSON (Object o) = 
        Geometry <$>
                o .: "location"      <*>
                o .: "location_type" <*>
                o .: "viewport"
    parseJSON _ = mzero

-----------------------------------------------------------
data GeocodeResults = GeocodeResults
    {  _addressComponents :: [AddressComponents]
    ,  _formattedAddress  :: T.Text
    ,  _geometry          :: Geometry
    ,  _rTypes            :: [T.Text]
    } deriving (Show, Eq, Generic)

makeLenses ''GeocodeResults

instance FromJSON GeocodeResults where
    parseJSON (Object o) =
        GeocodeResults <$>
                o .: "address_components" <*>
                o .: "formatted_address"  <*>
                o .: "geometry"           <*>
                o .: "types"
    parseJSON _ = mzero

-----------------------------------------------------------
data GeocodeResponse = GeocodeResponse
    {  _results :: [GeocodeResults]
    ,  _status  :: T.Text
    } deriving (Show, Eq, Generic)

makeLenses ''GeocodeResponse

instance FromJSON GeocodeResponse where
    parseJSON (Object o) =
        GeocodeResponse <$>
                o .: "results" <*>
                o .: "status"
    parseJSON _  = mzero
{-# LANGUAGE
    OverloadedStrings
  , DeriveGeneric #-}

module View
    ( GradesJSON(..)
    , gradesJSONtoLazyByteString
    , gradesJSONToGrades
    ) where

import Model

import GHC.Generics
-- import Data.Maybe
import Data.Aeson
import Database.Persist
import Database.Persist.Class
import Data.ByteString.Lazy

data GradesJSON = GradesJSON {
    name        :: String
  , firstGrade  :: Maybe Double
  , secondGrade :: Maybe Double
} deriving (Show, Generic)


instance FromJSON GradesJSON where
  parseJSON (Object v) =
    GradesJSON <$> v .:   "name"
               <*> v .:?  "firstGrade"
               <*> v .:?  "secondGrade"

instance ToJSON GradesJSON where
  toJSON (GradesJSON nameJSON firstGradeJSON secondGradeJSON)
   = object ["name" .= nameJSON, "firstGrade" .= firstGradeJSON, "secondGrade" .= secondGradeJSON]

---

gradesJSONtoLazyByteString :: Database.Persist.Class.Key Grades -> Grades -> Data.ByteString.Lazy.ByteString
gradesJSONtoLazyByteString key grades = encode . entityIdToJSON $ Entity key grades

gradesJSONToGrades :: GradesJSON -> Grades
gradesJSONToGrades gradesJSON = Grades nameJSONtoGrades firstGradeJSONtoGrades secondGradeJSONtoGrades
  where
    nameJSONtoGrades = name gradesJSON
    firstGradeJSONtoGrades = firstGrade gradesJSON
    secondGradeJSONtoGrades = secondGrade gradesJSON

--- Fluxo dos tipos :
--- ByteString -> GradesJson -> Grades
--- Rever a necessidade do tipo GradesJson. Ideia inicial era utilizar a biblioteca FromJson e ToJson do Aeson. Como recebo
--- Um bytestring talvez dê para pular esta etapa.
--- tojson no proprio grade jã me devolve objeto Json.
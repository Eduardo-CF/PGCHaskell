{-# LANGUAGE
    OverloadedStrings
  , DeriveGeneric #-}

module View
    (
    -- ( GradesJSON(..)
    -- , gradesJSONtoLazyByteString
    -- , gradesJSONToGrades
    ) where

-- import Model

-- import GHC.Generics
-- -- import Data.Maybe
-- import Data.Aeson
-- import Database.Persist
-- import Database.Persist.Class
-- import Data.ByteString.Lazy

--- Modelo de dados para serem utilizados no frontend (ideia original).

-- data GradesJSON = GradesJSON {
--     name        :: String
--   , firstGrade  :: Maybe Double
--   , secondGrade :: Maybe Double
-- } deriving (Show, Generic)


-- instance FromJSON GradesJSON where
--   parseJSON (Object v) =
--     GradesJSON <$> v .:   "name"
--                <*> v .:?  "firstGrade"
--                <*> v .:?  "secondGrade"

-- instance ToJSON GradesJSON where
--   toJSON (GradesJSON nameJSON firstGradeJSON secondGradeJSON)
--    = object ["name" .= nameJSON, "firstGrade" .= firstGradeJSON, "secondGrade" .= secondGradeJSON]

-- --- 
-- gradesJSONToGrades :: GradesJSON -> Grades
-- gradesJSONToGrades gradesJSON = Grades nameJSONtoGrades firstGradeJSONtoGrades secondGradeJSONtoGrades
--   where
--     nameJSONtoGrades = name gradesJSON
--     firstGradeJSONtoGrades = firstGrade gradesJSON
--     secondGradeJSONtoGrades = secondGrade gradesJSON

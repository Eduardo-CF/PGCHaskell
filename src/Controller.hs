{-# LANGUAGE OverloadedStrings #-}

module Controller
    ( mainRouter
    ) where

import Database
import Model
-- import View

import Snap
import Data.Maybe
import Data.Aeson
-- import Data.ByteString
import Data.ByteString.Lazy as LBS
-- import Data.ByteString.Char8
import Control.Monad.IO.Class
import Database.Persist
import Database.Persist.Class as DB

mainRouter :: Snap()
mainRouter =  route [
                  ("", writeBS "Bem vindo ao sistema de notas.")    -- Base / route                    Status
                , ("/grades/:id", method GET    gradesRouteShow)    -- Gets a single Grades by /:id    (200)
                , ("/grades",     method POST   gradesRouteCreate)  -- Creates a single Grades         (205)
                , ("/grades/:id", method DELETE gradesRouteDelete)  -- Deletes a single Grades by /:id (210)
                , ("/grades/:id", method PUT    gradesRouteUpdate)  -- Updates a single Grades by /:id (215)
                , ("/grades",     method GET    gradesAllRouteShow) -- Gets a list of All Grades       (220)
              ]

--
--- Show a Single Grade
--
gradesRouteShow :: Snap()
gradesRouteShow = do
    maybeGradeId <- getParam "id"
    maybeGrade <- liftIO $ getGrade maybeGradeId
    case maybeGrade of
      Nothing -> writeBS "Error, id don't exist."
      Just grades -> response 200 (gradeIdtoKey maybeGradeId) grades


--
--- Show all Grades -- Implementar 
--
gradesAllRouteShow :: Snap()
gradesAllRouteShow = do
    -- Recebe [Entity Grades]. Vem desta maneira pela resposta do GetMany do Persistent
    listGrades <- liftIO $ getManyGrades
    -- response 200 2 (gradesJSONtoLazyByteString $ encode $ Prelude.map entityIdToJSON listGrades)
    modifyResponse $ setResponseCode 200
    writeLBS $ encode $ Prelude.map entityIdToJSON listGrades
--
--- Insert a new Grade - tratamento para casos sem valores ou pelo menos retorno em caso de insert incompleto
--
gradesRouteCreate :: Snap()
gradesRouteCreate = do
  requestBody <- readRequestBody 2048
  let grades = lbsToGrades requestBody
  gradeId <- liftIO $ insertGrade grades
  response 205 gradeId grades


--
--- Delete a Grade - Talvez o tratamento para casos sem valores
--  
gradesRouteDelete :: Snap()
gradesRouteDelete = do
  maybeGradeId <- getParam "id"
  maybeGrade <- liftIO $ getGrade maybeGradeId
  case maybeGrade of
    Nothing -> writeBS "Error, id don't exist. Can't Delete"
    Just grades -> do
      liftIO $ deleteGrade maybeGradeId
      response 210 (gradeIdtoKey maybeGradeId) grades


--
--- Update a Grade - Talvez o tratamento para casos sem valores
--
gradesRouteUpdate :: Snap()
gradesRouteUpdate = do
  maybeGradeId <- getParam "id"
  requestBody <- readRequestBody 2048
  let newGrades = lbsToGrades requestBody
  maybeGrades <- liftIO $ updateGrade maybeGradeId newGrades
  case maybeGrades of 
    Nothing -> writeBS "Error, id don't exist. Can't update"
    Just grades -> response 215 (gradeIdtoKey maybeGradeId) grades

--
--- Delete Where - Para limpar o banco (mais pra testes)

-- Auxiliares


--
--- Generate a response for the route with method
--
response :: Int -> DB.Key Grades -> Grades -> Snap()
response status gradeId grades = do
  modifyResponse $ setResponseCode status
  writeLBS $ gradesToLbs gradeId grades

--
--- With a Lazy Bytestring (The body from our response) parse to Grade
--- Ver a possibilidade de campos opcionais.
--
-- Não consegui fazer diretamente com o Aeson, precisei desta função.(FromJSON)
-- OBS. Decode é função do Aeson 
lbsToGrades :: LBS.ByteString -> Grades
lbsToGrades body = fromMaybe (Grades ("") (Just 0.0) (Just 0.0)) (decode body :: Maybe Grades)

-- Pega valor grade e transforma em Bitestring (ToJSON)
gradesToLbs :: DB.Key Grades -> Grades -> LBS.ByteString
gradesToLbs key grades = encode . entityIdToJSON $ Entity key grades
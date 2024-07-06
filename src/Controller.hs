{-# LANGUAGE OverloadedStrings #-}

module Controller
    ( mainRouter
    ) where

import Database
import Model
-- import View

import Snap
-- import Data.Maybe
import Data.Aeson
-- import Data.ByteString
-- import Data.ByteString.Lazy as LBS
-- import Data.ByteString.Char8
import Control.Monad.IO.Class
import Database.Persist
-- import Database.Persist.Class as DB

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
--- Show Grades - Retorna na resposta o Json com o registro do banco com id informado.
--
gradesRouteShow :: Snap()
gradesRouteShow = do
    maybeGradeId <- getParam "id"
    maybeGrade <- liftIO $ getGrade maybeGradeId
    case maybeGrade of
      Nothing -> writeBS "Error, id don't exist."
      Just grades -> response 200 grades


--
--- Show all Grades -- Apresentar uma lista com todos os registros do banco.
--
gradesAllRouteShow :: Snap()
gradesAllRouteShow = do
    -- Recebe [Entity Grades]. Vem desta maneira pela resposta do GetMany do Persistent
    listGrades <- liftIO $ getManyGrades
    -- response 200 2 (gradesJSONtoLazyByteString $ encode $ Prelude.map entityIdToJSON listGrades)
    modifyResponse $ setResponseCode 220
    writeLBS $ encode $ Prelude.map entityIdToJSON listGrades

--
--- Insert - Insere um novo registro com os dados passados no Json.
-- Melhoria - Talvez o tratamento para casos sem valores (Nothing) ou pelo menos retorno em caso de insert incompleto.
--
gradesRouteCreate :: Snap()
gradesRouteCreate = do
  requestBody <- readRequestBody 2048
  let grades = decode requestBody :: Maybe Grades
  case grades of
    Nothing -> writeBS "Error, grade not correctly formed, can created register."
    Just newGrade -> response 205 newGrade

--
--- Delete - Remove um registro com o id informado.
-- Melhoria - Talvez o tratamento para casos sem valores (Nothing)
--
gradesRouteDelete :: Snap()
gradesRouteDelete = do
  maybeGradeId <- getParam "id"
  maybeGrade <- liftIO $ getGrade maybeGradeId
  case maybeGrade of
    Nothing -> writeBS "Error, id don't exist. Can't Delete"
    Just grades -> do
      liftIO $ deleteGrade maybeGradeId
      response 210 grades

--
--- Update - Atualiza um registro com o id informado e dados passados no Json.
-- Melhoria - Talvez o tratamento para casos sem valores (Nothing)
--
gradesRouteUpdate :: Snap()
gradesRouteUpdate = do
  maybeGradeId <- getParam "id"
  requestBody <- readRequestBody 2048
  let newGrades = decode requestBody :: Maybe Grades
  case newGrades of
    Nothing -> writeBS "Error, grade not correctly formed."
    Just newGradesValue -> do
      maybeGrades <- liftIO $ updateGrade maybeGradeId newGradesValue
      case maybeGrades of
        Nothing -> writeBS "Error, id don't exist. Can't update"
        Just grades -> response 215 grades
  

--
--- Delete Where - Para limpar o banco (mais pra testes)
--


-- Auxiliares


--
--- Response - Gera a resposta do registro com seu status code. Note que é utilizado para retornar único registro.
--
response :: Int -> Grades -> Snap()
response status grades = do
  modifyResponse $ setResponseCode status
  -- writeLBS $ gradesToLbs gradeId grades -- Modo anterior.
  -- Abaixo está uma possibilidade, reduzindo o numero de funções e utilizando o ToJSON de Grades.
  writeLBS . encode $ grades

--
--- LazyByteString to Grades - A partir da leitura do body da request faz o parsing para o datatype utilizado no serviço.
--
-- lbsToGrades :: LBS.ByteString -> Grades
-- lbsToGrades body = fromMaybe (Grades ("") (Just 0.0) (Just 0.0)) (decode body :: Maybe Grades)

--
--- Grades to LazyByteString - A partir do datatype do serviço o transforma em Bytestring para ser enviado como response da request.
-- 
-- gradesToLbs :: DB.Key Grades -> Grades -> LBS.ByteString
-- gradesToLbs key grades = encode . entityIdToJSON $ Entity key grades

-- Não consegui fazer diretamente com o Aeson, precisei desta função.(ToJSON)
-- OBS. Decode, encode e entityIdToJSON é função do Aeson, precisando do ToJSON e FromJSON



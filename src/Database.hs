{-# LANGUAGE OverloadedStrings #-}

module Database
    ( databaseMigration
    , getGrade
    , getManyGrades
    , insertGrade
    , updateGrade
    , deleteGrade
    , gradeIdtoKey
    ) where

import Model
-- import View


import Database.Persist
-- import Database.Persist.Class
import Database.Persist.Sqlite as DBSql
import Control.Monad.Trans.Resource
import Control.Monad.Logger
import Data.Int
import Data.Text
import Data.ByteString as BS
import Data.Maybe
-- import Data.Aeson
import Data.ByteString.Char8 as BSC8

--
--- Abrir conexão
--

sqliteConnection :: IO Data.Text.Text
sqliteConnection = return $ Data.Text.pack $ "default.db"


--
--- Rodar Query
--
dbRunCommand :: SqlPersistT(NoLoggingT (ResourceT IO)) b -> IO b
dbRunCommand query = do
  connString <- sqliteConnection
  runSqlite connString query


--
--- Migration do banco de dados - Cria estrutura caso não exista.
--
databaseMigration :: IO()
databaseMigration = dbRunCommand $ runMigration $ migrate entityDefs $ entityDef (Nothing :: Maybe Grades)

-- Métodos

--
--- Get
--
getGrade :: Maybe BS.ByteString -> IO(Maybe Grades)
getGrade k = do
  maybeGrade <- dbRunCommand $ DBSql.get $ gradeIdtoKey k
  return maybeGrade


  -- getBookmarks :: Maybe Data.ByteString.ByteString -> Maybe Data.ByteString.ByteString -> IO [Entity Bookmark]
  -- getBookmarks maybeLimitTo maybeOffsetBy = do
  --   -- If the limit and offset are `Nothing`, we will use the defaults 10 for the limit and 0 for the offset
  --   let limitToBS  = fromMaybe ("10" :: Data.ByteString.ByteString) maybeLimitTo
  --   let offsetByBS = fromMaybe ("0" :: Data.ByteString.ByteString) maybeOffsetBy
  --   -- Converts the strings to integers
  --   let limitToInt  = read (Data.ByteString.Char8.unpack limitToBS) :: Int
  --   let offsetByInt = read (Data.ByteString.Char8.unpack offsetByBS) :: Int
  --   -- The actual database call
  --   withDbRun $ DbSql.selectList ([] :: [Filter Bookmark]) [LimitTo limitToInt, OffsetBy offsetByInt]

getManyGrades :: IO [Entity Grades]
getManyGrades = dbRunCommand $ DBSql.selectList ([] :: [Filter Grades]) []

--
--- Insert
--
insertGrade :: Grades -> IO(DBSql.Key Grades)
insertGrade grade = dbRunCommand $ DBSql.insert grade

--
--- Update
--
updateGrade :: Maybe BS.ByteString -> Grades -> IO(Maybe Grades)
updateGrade gradeId grade = do
  maybeOldGrade <- getGrade gradeId
  case maybeOldGrade of
    Nothing -> return Nothing
    Just _ -> do
      let oldGradeKey = gradeIdtoKey gradeId
          newGrade = Grades {
            gradesName = gradesName grade
          ,  gradesFirstGrade = gradesFirstGrade grade
          ,  gradesSecondGrade = gradesSecondGrade grade
          }
      dbRunCommand $ DBSql.update oldGradeKey [
            GradesName =. gradesName grade
          , GradesFirstGrade =. gradesFirstGrade grade
          , GradesSecondGrade =. gradesSecondGrade grade
        ]
      return (Just newGrade)
  

--
--- Delete
--
deleteGrade :: Maybe BS.ByteString -> IO ()
deleteGrade gradeId = dbRunCommand $ DBSql.delete $ gradeIdtoKey gradeId


-- Auxiliares

--
--- ByteString para DBSQL Key
--
gradeIdtoKey :: Maybe BS.ByteString -> DBSql.Key Grades
gradeIdtoKey gradeId = toSqlKey $ fromByteString
  where
    invalid = "-1" :: BS.ByteString
    gradeIdInt64 = fromMaybe invalid gradeId
    fromByteString = read (BSC8.unpack gradeIdInt64) :: Int64
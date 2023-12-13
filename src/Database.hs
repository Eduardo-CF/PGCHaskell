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

-- Métodos

--
--- Get - Retorna Json de registro com id informado.
-- Melhoria : Usar Key Grades como entrada.
--
getGrade :: Maybe BS.ByteString -> IO(Maybe Grades)
getGrade key = do
  maybeGrade <- dbRunCommand $ DBSql.get $ gradeIdtoKey key
  return maybeGrade

--
--- GetAll - Retorna lista de Json com todos registros no banco.
--
getManyGrades :: IO [Entity Grades]
getManyGrades = dbRunCommand $ DBSql.selectList ([] :: [Filter Grades]) []

--
--- GetMany - DBSql.getMany recebe lista de ke e retorna lista de maybe grades, talvez. Util caso tenha path para retornar grades especĩficas.
--

--
--- Insert - Insere um novo registro do tipo Grades.
--
insertGrade :: Grades -> IO(DBSql.Key Grades)
insertGrade grade = dbRunCommand $ DBSql.insert grade

--
--- Update - Atualiza um registro com o id informado, passando as informações recebidas no registro do tipo Grades.
-- Melhoria : Usar Key Grades como entrada.
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
--- Delete - Remove registro com o Id informado.
-- Melhoria : Usar Key Grades como entrada.
--
deleteGrade :: Maybe BS.ByteString -> IO ()
deleteGrade gradeId = dbRunCommand $ DBSql.delete $ gradeIdtoKey gradeId


-- Auxiliares

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

--
--- ByteString para DBSQL Key
--
gradeIdtoKey :: Maybe BS.ByteString -> DBSql.Key Grades
gradeIdtoKey gradeId = toSqlKey $ fromByteString
  where
    invalid = "-1" :: BS.ByteString
    gradeIdInt64 = fromMaybe invalid gradeId
    fromByteString = read (BSC8.unpack gradeIdInt64) :: Int64


-- Ideia da melhoria :
--- Ao invés do controller informar o bytestring recebido, ele já faz a conversão para um Key Grades e assim a camada de database trata somente
--- com os tipos suportados por ele.
--- Isso é interessante caso, por exemplo, faça uma alteração no código e não utilize mais Snap, e o novo framework venha a utilizar uma
--- estrutura sem Bytestring. Assim seria feito o processo de conversão do tipo que seja para Key Grades e manteria este arquivo como está.
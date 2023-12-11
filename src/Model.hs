{-# LANGUAGE
    OverloadedStrings
  , GeneralizedNewtypeDeriving -- possibilita o uso do share.
  , MultiParamTypeClasses -- possibilita o uso do share.
  , DeriveGeneric -- possibilita o uso do share.
  , GADTs -- possibilita o uso do share.
  , TypeFamilies
  , TemplateHaskell
  , QuasiQuotes
  , FlexibleInstances -- possibilita o uso do share.
  , DerivingStrategies -- possibilita o uso do share.
  , UndecidableInstances -- possibilita o uso do share.
  , DataKinds -- possibilita o uso do share.
  , StandaloneDeriving #-}

module Model
    ( Grades(..)
    , entityDefs
    , EntityField(..)
    ) where

import GHC.Generics
import Database.Persist
import Database.Persist.TH
import Data.Aeson
-- import Data.Time -- Biblioteca com funções de tempo para implementação do createdAt/updatedAt

-- Estrutura para o Banco de Dados (Biblioteca Persistent)
-- Talvez mudar para usar maybe no meio e o valor ser opcional no lugar do 0.0 quando recebe o objeto. Colocar tambẽm um campo de data do created e updated
--- Maybe foi, default não funciona e created/updated não tentei(necessários ajustes que não deram tempo ainda).
share [mkEntityDefList "entityDefs", mkPersist sqlSettings] [persistLowerCase|
    Grades
      name        String
      firstGrade  Double  Maybe default='0.0'
      secondGrade Double  Maybe default='0.0'
      deriving Show Generic
  |]


-- Definição das funções ToJSON e FromJSON para o datatype Grades.
instance ToJSON Grades where 
  toJSON (Grades name firstGrade secondGrade) = object ["name" .= name, "firstGrade" .= firstGrade, "secondGrade" .= secondGrade]

instance FromJSON Grades where
  parseJSON (Object v) =
    Grades <$> v .:  "name"
           <*> v .:? "firstGrade"
           <*> v .:? "secondGrade"
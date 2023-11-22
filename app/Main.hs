module Main (main) where

import Snap

import Database
import Controller
-- import View
-- import Model

main :: IO ()
main = do
  databaseMigration -- Cria estrutura do banco caso não exista
  quickHttpServe mainRouter -- Sobe o servidor do Snap

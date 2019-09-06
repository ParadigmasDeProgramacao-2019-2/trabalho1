module Main (main, jogoLoop) where

import Data.Matrix
import Menu
import Usuario
import Validacoes
import Movimento
import Tabuleiro

main :: IO()
main = do
    menu
module Validacoes (validaEntradaLinhaColuna, validaEntradaDirecao) where

import Data.List

{--
    Função para validar o intervalo de entrada de linhas e colunas
--}
validaEntradaLinhaColuna :: Int -> Bool
validaEntradaLinhaColuna linhaColunaInput 
    | (linhaColunaInput > 0) && (linhaColunaInput < 8) = True
    | otherwise = False

{--
    Função para validar o intervalo de entrada da direcao
--}
validaEntradaDirecao :: Int -> Bool
validaEntradaDirecao direcaoInput 
    | (direcaoInput > 0) && (direcaoInput < 5) = True
    | otherwise = False
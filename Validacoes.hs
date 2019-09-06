module Validacoes (validaEntradaLinhaColuna, validaEntradaDirecao, verificaLinha, verificaLinhas, haJogadaValida) where

import Data.Matrix
import Data.List hiding (transpose)

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

{--
    Funcao para verificar se ha jogada valida em uma linha
--}
verificaLinha :: [Char] -> Bool
verificaLinha linha = isInfixOf ['O','O','-'] linha || isInfixOf ['-','O','O'] linha

{--
    Funcao para verificar se ha jogada valida em varias linhas
--}
verificaLinhas :: [[Char]] -> Bool
verificaLinhas [] = False
verificaLinhas (xs:xss) = verificaLinha xs || verificaLinhas xss

{--
    Funcao para verificar se ha jogada valida na matriz
--}
haJogadaValida :: Matrix Char -> Bool
haJogadaValida tabuleiro = do
    verificaLinhas (toLists tabuleiro) || verificaLinhas (toLists (transpose tabuleiro))
    
module Movimento (pegaLinha, pegaColuna, pegaDirecao, mapeiaLetraColuna, defineDestinoMatriz) where

import Data.Char
import Validacoes

{--
    Função para capturar o input da linha
--}
pegaLinha :: IO Int
pegaLinha = do
    putStrLn "Digite a linha da peca que vai se movimentar [1..7]:"
    linhaInput <- getLine
    if (validaEntradaLinhaColuna (read linhaInput) == False) 
        then do
            putStrLn "Linha invalida [1..7]" 
            pegaLinha
        else do
            return (read linhaInput)

{--
    Mapeamento das letras das colunas com numeros da matriz
--}
mapeiaLetraColuna :: Char -> Int
mapeiaLetraColuna 'A' = 1
mapeiaLetraColuna 'B' = 2
mapeiaLetraColuna 'C' = 3
mapeiaLetraColuna 'D' = 4
mapeiaLetraColuna 'E' = 5
mapeiaLetraColuna 'F' = 6
mapeiaLetraColuna 'G' = 7
mapeiaLetraColuna  _  = 8

{--
    Função para capturar o input da coluna
--}
pegaColuna :: IO Int
pegaColuna = do
    putStrLn "Digite a coluna da peca que vai se movimentar [A..G]:"
    colunaInput <- getChar
    getChar
    if (validaEntradaLinhaColuna (mapeiaLetraColuna (toUpper colunaInput)) == False) 
        then do
            putStrLn "Coluna invalida [A..G]" 
            pegaColuna
        else do
            return (mapeiaLetraColuna (toUpper colunaInput))

{--
    Função para capturar o input da direcao
--}
pegaDirecao :: IO Int
pegaDirecao = do
    putStrLn "Digite a direção [1 - Para Cima; 2 - Para Baixo; 3 - Para Esquerda; 4 - Para Direita]:"
    direcaoInput <- getLine
    if (validaEntradaDirecao (read direcaoInput) == False) 
        then do
            putStrLn "Direcao Invalida [1..4]" 
            pegaDirecao
        else do
            return (read direcaoInput)

{--
    Função para definir para onde a peça será movida
--}
defineDestinoMatriz :: Int -> Int -> Int -> (Int,Int)
defineDestinoMatriz linha coluna 1 = (linha - 1, coluna) 
defineDestinoMatriz linha coluna 2 = (linha + 1, coluna) 
defineDestinoMatriz linha coluna 3 = (linha, coluna - 1) 
defineDestinoMatriz linha coluna 4 = (linha, coluna + 1) 
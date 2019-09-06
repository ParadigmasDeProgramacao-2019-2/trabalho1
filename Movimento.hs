module Movimento (pegaLinha, pegaColuna, pegaDirecao, mapeiaLetraLinha, defineDestinoMatriz) where

import Data.Char
import Validacoes

{--
    Função para capturar o input da coluna
--}
pegaColuna :: IO Int
pegaColuna = do
    putStrLn "Digite a coluna da peca que vai se movimentar [1..7]:"
    colunaInput <- getLine
    if (validaEntradaLinhaColuna (read colunaInput) == False) 
        then do
            putStrLn "Coluna invalida [1..7]" 
            pegaColuna
        else do
            return (read colunaInput)

{--
    Mapeamento das letras das colunas com numeros da matriz
--}
mapeiaLetraLinha :: Char -> Int
mapeiaLetraLinha 'A' = 1
mapeiaLetraLinha 'B' = 2
mapeiaLetraLinha 'C' = 3
mapeiaLetraLinha 'D' = 4
mapeiaLetraLinha 'E' = 5
mapeiaLetraLinha 'F' = 6
mapeiaLetraLinha 'G' = 7
mapeiaLetraLinha  _  = 8

{--
    Função para capturar o input da linha
--}
pegaLinha :: IO Int
pegaLinha = do
    putStrLn "Digite a linha da peca que vai se movimentar [A..G]:"
    linhaInput <- getChar
    getChar
    if (validaEntradaLinhaColuna (mapeiaLetraLinha (toUpper linhaInput)) == False) 
        then do
            putStrLn "Linha invalida [A..G]" 
            pegaLinha
        else do
            return (mapeiaLetraLinha (toUpper linhaInput))

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
defineDestinoMatriz :: Int -> Int -> Int -> Int -> (Int,Int)
defineDestinoMatriz linha coluna 1 incremento = (linha - incremento, coluna) 
defineDestinoMatriz linha coluna 2 incremento = (linha + incremento, coluna) 
defineDestinoMatriz linha coluna 3 incremento = (linha, coluna - incremento) 
defineDestinoMatriz linha coluna 4 incremento = (linha, coluna + incremento) 

{--
    Função para pegar os movimentos e realizar a jogada
--}

pegaMovimento :: Matrix Char -> IO()
pegaMovimento tabuleiro = do
    printarTabuleiro tabuleiro
    linha <- pegaLinha
    coluna <- pegaColuna
    direcao <- pegaDirecao
    fazJogada linha coluna direcao tabuleiro

{--
    Função para realizar a jogada, alterar os 3 elementos que sao alterados ao fazer uma jogada
--}    
fazJogada :: Int -> Int -> Int -> Matrix Char -> IO()
fazJogada linha coluna direcao tabuleiro = do
    pegaMovimento (setElem '-' (linha, coluna) (setElem '-' (defineDestinoMatriz linha coluna direcao 1) (setElem 'O' (defineDestinoMatriz linha coluna direcao 2) tabuleiro)))
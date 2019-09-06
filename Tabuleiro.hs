module Tabuleiro( 
    printarTabuleiro,
    tabuleiro, 
    guiasVerticais, 
    printarGuiaColuna,
    printarAux1,
    printarAux2,
    printarLinhaAux,
    printarPosicaoAtual,
    printarLinha,
    printarColuna,
    verificaPosicaoOrigem,
    verificaPosicaoIntermediaria,
    verificaPosicaoDestino,
    verificaPosicao,
    checarQuatPecas
) where

import Data.Matrix 
import Data.List hiding (transpose)
import Utils

tabuleiro :: Matrix Char
tabuleiro = fromLists
    [[' ',' ','O','O','O',' ',' '],
     [' ',' ','O','O','O',' ',' '],
     ['O','O','O','O','O','O','O'],
     ['O','O','O','-','O','O','O'],
     ['O','O','O','O','O','O','O'],
     [' ',' ','O','O','O',' ',' '],
     [' ',' ','O','O','O',' ',' ']]

{--
    Função responsável por chamar o print do Tabuleiro,
    primeira função a ser chamada
--}
printarTabuleiro :: Matrix Char -> IO()
printarTabuleiro mat = do
    putStr("  ")
    printarGuiaColuna mat 1

{--
    Letras verticais para suporte de locomoção das peças
--}
guiasVerticais:: [String]
guiasVerticais = ["A ","B ","C ","D ","E ","F ","G "] 

{--
    Printa a linha horizonta contendo a coluna 
    correspondente para locomoção
--}
printarGuiaColuna :: Matrix Char -> Int -> IO()
printarGuiaColuna mat 8 = do
    putStrLn("")
    printarLinha mat 1 1 guiasVerticais

printarGuiaColuna mat cont = do
    putStr("  "++ show cont ++ "  ")
    printarGuiaColuna mat (cont + 1)

{-- 
    Printa as linhas de divisão das casa
    contendo as peças para as que tem 8 casa
--}
printarAux1 :: Int -> IO()
printarAux1 8 = putStrLn("")
printarAux1 cont = do 
    putStr("---- ")
    printarAux1 (cont + 1)

{-- 
    Printa as linhas de divisão das casa
    contendo as peças para as que tem apenas 3 casas
--}
printarAux2 :: Int -> IO()
printarAux2 8 = putStrLn("")
printarAux2 cont
    | cont == 3 || cont == 4 || cont == 5 = do
        putStr("---- ")
        printarAux2 (cont + 1)
    |otherwise = do 
        putStr("     ")
        printarAux2 (cont + 1)
{-- 
    Chama as linhas que printam a divisão das
    casas de acordo com certas regras
--}
printarLinhaAux:: Int -> IO()
printarLinhaAux linha
    |  linha == 3 || linha == 4 || linha == 5 || linha == 6 = printarAux1 1
    | otherwise = printarAux2 1

{-- 
    Verifica qual o simbolo na matriz correspondente
    e printa a casa de acrodo com o simbolo
--}
printarPosicaoAtual :: Matrix Char -> Int -> Int -> IO()
printarPosicaoAtual mat row col 
    | mat ! (row,col) == 'O' = putStr("| .. ")
    | mat ! (row, col) == '-' = putStr("|    ")
    | otherwise = putStr("     ")
{--
    Função principal responsavel por chamar as linhas
    e as colunas na hora da printagem
--}
printarLinha ::  Matrix Char -> Int -> Int -> [String] -> IO()
printarLinha mat 8 _ _ = do
    putStr("   ")
    printarAux2 1
printarLinha mat row col (x:xs) = do
    putStr("   ")
    printarLinhaAux row
    putStr(x)
    printarColuna mat row col
    putStr("  ")
    printarColuna mat row col
    printarLinha mat (row + 1) 1 xs

{--
    Printa as colunas até a 8º posição
--}
printarColuna ::  Matrix Char -> Int -> Int -> IO ()
printarColuna mat _ 8 = putStrLn("")
printarColuna mat row col = do
    printarPosicaoAtual mat row col
    printarColuna mat row (col + 1)

{--
    Verifica se posição de origem contém peça
--}
verificaPosicaoOrigem :: Matrix Char -> Int -> Int -> Char
verificaPosicaoOrigem tabuleiro linha coluna = getElem linha coluna tabuleiro 

{--
    Verifica se posição de destino contém peça
--}
verificaPosicaoDestino :: Matrix Char -> Int -> Int -> Int -> Char
verificaPosicaoDestino tabuleiro linha coluna 1 = getElem (linha - 2) coluna tabuleiro
verificaPosicaoDestino tabuleiro linha coluna 2 = getElem (linha + 2) coluna tabuleiro
verificaPosicaoDestino tabuleiro linha coluna 3 = getElem linha (coluna - 2) tabuleiro 
verificaPosicaoDestino tabuleiro linha coluna 4 = getElem linha (coluna + 2) tabuleiro

{--
    Verifica se posição intermediária contém peça
--}
verificaPosicaoIntermediaria :: Matrix Char -> Int -> Int -> Int -> Char
verificaPosicaoIntermediaria tabuleiro linha coluna 1 = getElem (linha - 1) coluna tabuleiro
verificaPosicaoIntermediaria tabuleiro linha coluna 2 = getElem (linha + 1) coluna tabuleiro
verificaPosicaoIntermediaria tabuleiro linha coluna 3 = getElem linha (coluna - 1) tabuleiro 
verificaPosicaoIntermediaria tabuleiro linha coluna 4 = getElem linha (coluna + 1) tabuleiro

{--
    Verifica se posições são válidas
--}
verificaPosicao :: Matrix Char -> Int -> Int -> Int -> Bool
verificaPosicao tabuleiro linha coluna direcao = naoEhVazio (verificaPosicaoOrigem tabuleiro linha coluna) && naoEhVazio (verificaPosicaoIntermediaria tabuleiro linha coluna direcao) && ehVazio (verificaPosicaoDestino tabuleiro linha coluna direcao)

{--
    Função para calcular quantidade de peças
--}
checarQuatPecas :: Matrix Char -> Int 
checarQuatPecas tabuleiro = 
    let listaTabuleiro = toList tabuleiro in     
    (length [a | a <- listaTabuleiro, a == 'O']) 

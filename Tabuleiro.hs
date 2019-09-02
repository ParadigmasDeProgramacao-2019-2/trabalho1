module Tabuleiro ( novaPartida, renderLinhasAux, linhaCampoBranco, linhaCampoPreto, renderLinha, renderTabuleiro, Tabuleiro, EhDama, Posicao, Peca ) where

import Usuario
import System.Process

type Tabuleiro = [Int]

type EhDama = Bool --Informa se a peça já se tornou dama
type Posicao = (Int, Int) --Localização da peca na matriz
data Peca = Peca Jogador Posicao EhDama deriving (Show)

novaPartida :: Tabuleiro -> IO()
novaPartida tabuleiro = do
    system("clear")
    return ()
    -- Instruções do Jogo

-- Renderizar tabuleiro
renderLinhasAux :: Int -> IO()
renderLinhasAux 9 = putStrLn("")
renderLinhasAux cont = do
    putStr(" -------")
    renderLinhasAux (cont + 1)
    
linhaCampoPreto :: Int -> IO()
linhaCampoPreto 9 = putStrLn("|")
linhaCampoPreto cont = do
    putStr("| . . . ")
    linhaCampoBranco (cont + 1)
    
linhaCampoBranco :: Int -> IO()
linhaCampoBranco 9 = putStrLn("|")
linhaCampoBranco cont = do
    putStr("|       ")
    linhaCampoPreto (cont + 1)

renderLinha :: Int -> IO()
renderLinha cont
    | (mod cont 4 == 0) = do
        linhaCampoPreto 1
        linhaCampoPreto 1
        linhaCampoPreto 1
    | (mod cont 2 == 1) = renderLinhasAux 1 
    | otherwise = do
        linhaCampoBranco 1
        linhaCampoBranco 1
        linhaCampoBranco 1

renderTabuleiro :: Tabuleiro -> Int -> IO()
renderTabuleiro _ 17 = do
    renderLinhasAux 1

renderTabuleiro tabuleiro cont = do
    renderLinha cont
    renderTabuleiro tabuleiro (cont + 1)
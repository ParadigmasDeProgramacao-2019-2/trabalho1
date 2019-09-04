module Tabuleiro( printaTabuleiro, tabuleiro ) where
import Data.Matrix

tabuleiro :: Matrix Char
tabuleiro = fromLists
    [[' ',' ','O','O','O',' ',' '],
     [' ',' ','O','O','O',' ',' '],
     ['O','O','O','O','O','O','O'],
     ['O','O','O','-','O','O','O'],
     ['O','O','O','O','O','O','O'],
     [' ',' ','O','O','O',' ',' '],
     [' ',' ','O','O','O',' ',' ']]

printaTabuleiro :: IO()
printaTabuleiro = do 
    putStrLn "\nO tabuleiro utizado será o tipo inglês. \n"
    putStrLn "Onde (O) representa as bolinhas e (-) representa os espaços vazios. \n"
    putStrLn "Você ganhará o jogo quando restar apenas uma bolinha."
    print tabuleiro

{-novaPartida :: Tabuleiro -> IO()
novaPartida tabuleiro = do
    system("clear")
    return ()
    -- Instruções do Jogo

-- Renderizar tabuleiro
renderLinhasAux :: Int -> IO()
renderLinhasAux 9 = putStrLn("")
renderLinhasAux cont = do
    putStr(" -------")
    renderLinhasAux (cont + O)
    
linhaCampoPreto :: Int -> IO()
linhaCampoPreto 9 = putStrLn("|")
linhaCampoPreto cont = do
    putStr("| . . . ")
    linhaCampoBranco (cont + O)
    
linhaCampoBranco :: Int -> IO()
linhaCampoBranco 9 = putStrLn("|")
linhaCampoBranco cont = do
    putStr("|       ")
    linhaCampoPreto (cont + O)

renderLinha :: Int -> IO()
renderLinha cont
    | (mod cont 4 == 0) = do
        linhaCampoPreto O
        linhaCampoPreto O
        linhaCampoPreto O
    | (mod cont 2 == O) = renderLinhasAux O 
    | otherwise = do
        linhaCampoBranco O
        linhaCampoBranco O
        linhaCampoBranco O

renderTabuleiro :: Tabuleiro -> Int -> IO()
renderTabuleiro _ O7 = do
    renderLinhasAux O

renderTabuleiro tabuleiro cont = do
    renderLinha cont
    renderTabuleiro tabuleiro (cont + O)-}
module Menu ( menu, executar, novoJogo, jogoLoop) where

import System.Process
import Data.Matrix
import Usuario
import Tabuleiro
import Validacoes
import Movimento
import Data.Char

menu :: IO()
menu = do
    system "clear" --Limpa a tela (Apenas para Ubuntu)
    putStrLn "---------------------------------- RESTA 1 ----------------------------------"
    putStr "\n\nDigite 1 para jogar"
    putStr "\nDigite 0 para sair\n\n"
    op <- getChar
    getChar -- descarta o Enter
    executar op
    return ()

executar :: Char -> IO()
executar '1' = do
   
    --Cadastra o jogador
    jogador <- cadastraJogador
    --Inicia um novo jogo
    novoJogo jogador tabuleiro
    return ()

executar '0' = do
    return ()

executar _ = do
    putStrLn ("\nOpção inválida! Tente novamente")
    putStr "\nPressione <Enter> para voltar ao menu\n"
    getChar
    menu

novoJogo :: Jogador -> Matrix Char -> IO()
novoJogo jogador tabuleiro = do
    system("clear")
    putStrLn "\nO tabuleiro utizado será o tipo inglês. \n"
    putStrLn "Onde os campos cheios são as peças e os vazios são os espaços vagos \n"
    putStrLn "Você ganhará o jogo quando restar apenas uma peça."
    putStrLn("")
    putStr "\nPressione <Enter> para iniciar o jogo\n"
    getChar
    jogoLoop jogador tabuleiro

{--
Função para pegar os movimentos e realizar a jogada
--}
pegaMovimento :: Jogador -> Matrix Char -> IO()
pegaMovimento jogador tabuleiro = do
    printarTabuleiro tabuleiro
    linha <- pegaLinha
    coluna <- pegaColuna
    direcao <- pegaDirecao
    if (validaEspacoInvalido linha coluna == False )
        then do
            system("clear")
            putStrLn "Espaço Não ultilizado no tabuleiro"
            getChar 
            pegaMovimento jogador tabuleiro
        else do
            fazJogada jogador linha coluna direcao tabuleiro

{--
    Função para realizar a jogada, alterar os 3 elementos que sao alterados ao fazer uma jogada
--}    
fazJogada :: Jogador -> Int -> Int -> Int -> Matrix Char -> IO()
fazJogada jogador linha coluna direcao tabuleiro = do
    jogoLoop jogador (setElem '-' (linha, coluna) (setElem '-' (defineDestinoMatriz linha coluna direcao 1) (setElem 'O' (defineDestinoMatriz linha coluna direcao 2) tabuleiro)))

jogoLoop :: Jogador -> Matrix Char -> IO()
jogoLoop jogador tabuleiro 
    | haJogadaValida tabuleiro = 
        do
            pegaMovimento jogador tabuleiro
    | otherwise =
        do
            if ((checarQuatPecas tabuleiro) ==  1) then
                print "Parabens, ganhou"
            else
                print "Perdeu"
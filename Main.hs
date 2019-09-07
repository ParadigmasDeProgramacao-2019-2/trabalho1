module Main (main, menu, executar, printaRegras, novoJogo, pegaMovimento, jogoLoop) where

import System.Process
import Data.Matrix
import Usuario
import Validacoes
import Movimento
import Tabuleiro

main :: IO()
main = do
    menu

{--
    Menu inicial do jogo
--}
menu :: IO()
menu = do
    system "clear" --Limpa a tela (Apenas para Ubuntu)
    putStrLn "---------------------------------- RESTA 1 ----------------------------------"
    putStr "\n\nDigite 1 para jogar"
    putStr "\nDigite 2 para ganhar automaticamente\n\n"
    putStr "\nDigite 0 para sair\n\n"
    op <- getChar
    getChar -- descarta o Enter
    executar op
    return ()
sequenciaDeVitoria :: [[Int]]
sequenciaDeVitoria = [[2,4,2], [5,4,1], [4,6,3], [4,3,4], [2,3,2], [5,6,3], [3,1,4], [7,5,1], [3,4,3], [4,5,2], [5,1,1], [7,3,4], [3,1,4], [7,5,1], [4,3,1], [5,4,4], [3,6,3], [1,5,2], [5,7,3], [5,2,4], [5,4,4], [3,7,2], [5,7,3], [6,3,4], [6,5,1], [4,5,1], [1,3,4], [1,5,2], [3,5,3], [2,3,2], [4,2,4]]
    
{--
    Executa a operacao indicada pelo usuario no menu
--}
executar :: Char -> IO()
executar '1' = do
    --Cadastra o jogador
    jogador <- cadastraJogador
    --Inicia um novo jogo
    novoJogo jogador tabuleiro
    return ()
executar '2' = do
    printaRegras
    putStr "\nPressione <Enter> para iniciar o jogo\n"
    getChar
    jogoVitoriaAutomatica sequenciaDeVitoria tabuleiro
    return ()
executar '0' = do
    return ()
executar _ = do
    putStrLn ("\nOpção inválida! Tente novamente")
    putStr "\nPressione <Enter> para voltar ao menu\n"
    getChar
    menu

{--
    Mostra regras para o usuario
--}
printaRegras :: IO()
printaRegras = do
    system("clear")
    putStrLn "\nO tabuleiro utizado será o tipo inglês. \n"
    putStrLn "Onde os campos cheios são as peças e os vazios são os espaços vagos \n"
    putStrLn "Você ganhará o jogo quando restar apenas uma peça."
    putStrLn("")

{--
    Cria um novo jogo mostrando as regras e chamando o loop do jogo
--}
novoJogo :: Jogador -> Matrix Char -> IO()
novoJogo jogador tabuleiro = do
    printaRegras
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
    if ((validaMovimentoDentroMatriz linha coluna direcao) == False || validaEspacoInvalido linha coluna == False)
        then do
            system("clear")
            putStrLn "Espaço Não ultilizado no tabuleiro"
            putStrLn "<enter> para continuar"
            getChar 
            pegaMovimento jogador tabuleiro
        else if ((verificaPosicao tabuleiro linha coluna direcao) == False)
            then do
                system("clear")
                putStrLn "Jogada invalida"
                putStrLn "<enter> para continuar"
                getChar
                pegaMovimento jogador tabuleiro
        else do
            jogoLoop jogador (fazJogada linha coluna direcao tabuleiro)

{--
    Função que realiza o loop do jogo
--}
jogoLoop :: Jogador -> Matrix Char -> IO()
jogoLoop jogador tabuleiro 
    | haJogadaValida tabuleiro = 
        do
            pegaMovimento jogador tabuleiro
    | otherwise =
        do
            if ((checarQuatPecas tabuleiro) ==  1) then 
                do
                    putStrLn ("Parabens companheiro(a), vc ganhou " ++ (show jogador))
                    printarTabuleiro tabuleiro
            else
                do
                    putStrLn ("Perdeu companheiro(a) " ++ (show jogador))
                    printarTabuleiro tabuleiro

jogoVitoriaAutomatica :: [[Int]] -> Matrix Char -> IO()
jogoVitoriaAutomatica [] tabuleiro = do
    printarTabuleiro tabuleiro
    putStrLn ("Parabenssss, voce nao venceu")
jogoVitoriaAutomatica (x:xs) tabuleiro = do
    let linha = x !! 0
    let coluna = x !! 1
    let direcao = x !! 2
    printarTabuleiro tabuleiro
    jogoVitoriaAutomatica xs (fazJogada linha coluna direcao tabuleiro)
module Menu ( menu, executar, novoJogo) where

import System.Process
import Usuario
import Tabuleiro

menu :: IO()
menu = do
    system "clear" --Limpa a tela (Apenas para Ubuntu)
    putStrLn "---------------------------------- Jogo de Dama ----------------------------------"
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
    novoJogo
    return ()

executar '0' = do
    return ()

executar _ = do
    putStrLn ("\nOpção inválida! Tente novamente")
    putStr "\nPressione <Enter> para voltar ao menu\n"
    getChar
    menu

novoJogo :: IO()
novoJogo = do
    system("clear")
    putStrLn("O - Espaços com Peça\n. - Espaços sem Peça")
    -- Comeca com o primeiro jogador
    novaPartida [1..33]
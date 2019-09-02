module Usuario (QtdPecas, Nome, Jogador, cadastraJogador) where

import Utils

type QtdPecas = Int
type Nome = String
type Id = Int
data Jogador = Jogador Nome QtdPecas deriving (Show)

cadastraJogador :: IO Jogador
cadastraJogador = do
    nome<-getString("Digite o seu nome:\n")

    return (Jogador nome 32)
module Usuario (Nome, Jogador, cadastraJogador) where

import Utils

type Nome = String
data Jogador = Jogador Nome deriving (Show)

cadastraJogador :: IO Jogador
cadastraJogador = do
    nome <- getString("Digite o seu nome:\n")
    return (Jogador nome)
module Utils ( getString, naoEVazio, eVazio ) where

getString :: String -> IO String
getString str = do
    putStr str
    res <- getLine
    return res

{--
    Função auxiliar que verifica se posição contém peça
--}
naoEVazio :: Char -> Bool
naoEVazio 'O' = True
naoEVazio _ = False

{--
    Função auxiliar que verifica se posição não contém peça
--}
eVazio :: Char -> Bool
eVazio '-' = True
naoEVazio _ = False
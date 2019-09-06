module Utils ( getString, naoEhVazio, ehVazio ) where

getString :: String -> IO String
getString str = do
    putStr str
    res <- getLine
    return res

{--
    Função auxiliar que verifica se posição contém peça
--}
naoEhVazio :: Char -> Bool
naoEhVazio 'O' = True
naoEhVazio _ = False

{--
    Função auxiliar que verifica se posição não contém peça
--}
ehVazio :: Char -> Bool
ehVazio '-' = True
ehVazio _ = False
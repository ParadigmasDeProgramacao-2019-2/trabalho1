import System.IO
import System.Process
import Data.Char
import Menu

main :: IO()
main = do
    menu

pegaMovimento = do
    linha <- pegaLinha
    coluna <- pegaColuna
    direcao <- pegaDirecao
    print (linha)
    print (coluna)
    print (direcao)

validaEntradaLinhaColuna :: Int -> Bool
validaEntradaLinhaColuna linhaColunaInput 
    | (linhaColunaInput > 0) && (linhaColunaInput < 8) = True
    | otherwise = False

pegaLinha :: IO Int
pegaLinha = do
    putStrLn "Digite a linha da peca que vai se movimentar [1..7]:"
    linhaInput <- getLine
    if (validaEntradaLinhaColuna (read linhaInput) == False) 
        then do
            putStrLn "Linha invalida [1..7]" 
            pegaLinha
        else do
            return (read linhaInput)

mapeiaLetraColuna :: Char -> Int
mapeiaLetraColuna 'A' = 1
mapeiaLetraColuna 'B' = 2
mapeiaLetraColuna 'C' = 3
mapeiaLetraColuna 'D' = 4
mapeiaLetraColuna 'E' = 5
mapeiaLetraColuna 'F' = 6
mapeiaLetraColuna 'G' = 7
mapeiaLetraColuna  _  = 8

pegaColuna :: IO Int
pegaColuna = do
    putStrLn "Digite a coluna da peca que vai se movimentar [A..G]:"
    colunaInput <- getChar
    getChar
    if (validaEntradaLinhaColuna (mapeiaLetraColuna (toUpper colunaInput)) == False) 
        then do
            putStrLn "Coluna invalida [A..G]" 
            pegaColuna
        else do
            return (mapeiaLetraColuna (toUpper colunaInput))

validaEntradaDirecao :: Int -> Bool
validaEntradaDirecao direcaoInput 
    | (direcaoInput > 0) && (direcaoInput < 5) = True
    | otherwise = False

pegaDirecao :: IO Int
pegaDirecao = do
    putStrLn "Digite a direção [1 - Para Cima; 2 - Para Baixo; 3 - Para Esquerda; 4 - Para Direita]:"
    direcaoInput <- getLine
    if (validaEntradaDirecao (read direcaoInput) == False) 
        then do
            putStrLn "Direcao Invalida [1..4]" 
            pegaDirecao
        else do
            return (read direcaoInput)
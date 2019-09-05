import System.IO
import System.Process
import Menu
import Movimento
import Tabuleiro

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
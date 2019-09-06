import System.IO
import System.Process
import Menu
import Movimento
import Tabuleiro
import Data.Matrix

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
    -- verificar a jogada
    -- se for valida ele troca os 3 elementos
    -- setElem '-' (defineDestinoMatriz linha coluna direcao) tabuleiro
    -- printarTabuleiro tabuleiro
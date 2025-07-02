module EventosLoja where
import Type

-- Objetivo: Transformar coordenadas do clique do rato em píxeis (x,y) num índice da matriz do tabuleiro Just (linha,coluna)
-- Exemplo: (125, -75)  -->  Just (2,3)

pixelParaIndice :: Int -> Int -> (Float,Float) -> Maybe (Int,Int) 
pixelParaIndice nCols nRows (x,y) =
  let tile = 50
      metadeLargura = (fromIntegral nCols * tile ) / 2
      metadeAltura = (fromIntegral nRows * tile) / 2

      -- Faz descolacamento para (0,0) ser no canto superior esquerdo  
      colF = (x + metadeLargura) / tile
      rowF = (metadeAltura - y) / tile      
      col  = floor colF -- Arredonda para inteiro
      row  = floor rowF
  in if col >= 0 && col < nCols && row >= 0 && row < nRows
        then Just (row,col)
        else Nothing -- Fora do tabuleiro

clicouRelva :: (Float,Float) -> Jogo -> Bool
clicouRelva pxy jogo =
  case pixelParaIndice (nColunas jogo) (nLinhas  jogo) pxy of
    Just (r,c) -> mapaJogo jogo !! r !! c == Relva -- xs !! i  “dá‑me o elemento da lista xs na posição i”
    Nothing    -> False

module Movimentacao where
import Type
import EventosLoja



terrenoETerra :: (Float,Float) -> EstadoJanela -> Bool
terrenoETerra pxy estado =
  let jogo = jogoatual estado
  in case pixelParaIndice (nColunas jogo) (nLinhas jogo) pxy of
        Just (r,c) -> 
            let linha = mapaJogo jogo !! r
            in if c < length linha then linha !! c == Terra else False
        Nothing -> False


{-
Tenta mover o inimigo na direção atual. 
Verifica se, na direção do movimento, há terra ,ou seja, se o inimigo pode continuar. 
Se houver, move-se , caso contrário, tenta mudar a direção para uma perpendicular.
-}

moverInimigo :: Float -> Inimigo -> EstadoJanela -> Inimigo
moverInimigo tempo inimigo estado = 
    let direcao = direcaoInimigo inimigo
        (x,y) = posicaoInimigo inimigo
        tile = 50
        largura = (tile / 2) + 1
        altura = (tile / 2) + 1
    in case direcao of
        Norte -> if terrenoETerra (x, y + altura) estado then inimigo { posicaoInimigo = (x, y + (velocidadeInimigo inimigo * tempo)) }
                 else if terrenoETerra (x + largura, y) estado then inimigo { direcaoInimigo = Este }
                 else if terrenoETerra (x - largura, y) estado then inimigo { direcaoInimigo = Oeste }
                 else inimigo
        Sul -> if terrenoETerra (x, y - altura) estado then inimigo { posicaoInimigo = (x, y - (velocidadeInimigo inimigo * tempo)) }
               else if terrenoETerra (x + largura, y) estado then inimigo { direcaoInimigo = Este }
               else if terrenoETerra (x - largura, y) estado then inimigo { direcaoInimigo = Oeste }
               else inimigo
        Este -> if terrenoETerra (x + largura, y) estado then inimigo { posicaoInimigo = (x + (velocidadeInimigo inimigo * tempo), y) }
                else if terrenoETerra (x, y - altura) estado then inimigo { direcaoInimigo = Sul }
                else if terrenoETerra (x, y + altura) estado then inimigo { direcaoInimigo = Norte }
                else inimigo
        Oeste -> if terrenoETerra (x - largura, y) estado then inimigo { posicaoInimigo = (x - (velocidadeInimigo inimigo * tempo), y) }
                 else if terrenoETerra (x, y - largura) estado then inimigo { direcaoInimigo = Sul }
                 else if terrenoETerra (x, y + altura) estado then inimigo { direcaoInimigo = Norte }
                 else inimigo



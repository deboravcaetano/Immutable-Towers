module Movimentacao where
import Type

-- FUNÇÕES PROJETO ANTIGO QUE LIDAM COM O MOVIMENTO: MELHORAR!!!!!!!!!!!!!!!!!!!!11


obterPosicoesTerra :: Mapa -- ^ O mapa do jogo, representado como uma lista de listas de terrenos
                   -> [Posicao] -- ^ Lista de posições (x, y) onde o terreno é do tipo __Terra__
obterPosicoesTerra mapa = [(fromIntegral x + 0.5, fromIntegral y + 0.5) | 
    (linha, y) <- zip mapa [0..], 
    (x, terreno) <- zip [0..] linha, 
    terreno == Terra]

estaNoMapa :: Posicao -- ^ A posição a ser verificada 
           -> Mapa -- ^ O mapa do jogo, representado como uma lista de listas de terrenos
           -> Bool -- ^ Retorna 'True' se a posição estiver dentro dos limites do mapa e 'False' caso contrário
estaNoMapa (x, y) mapa = 
    let (xMapa, yMapa) = converterCoordenadas (x,y)
    in floor xMapa >= 0 && floor xMapa < length (head mapa) && floor yMapa >= 0 && floor yMapa < length mapa

terrenoETerra :: Posicao -- ^ A posição a ser verificada
              -> Mapa -- ^ O mapa do jogo
              -> Bool -- ^ Retorna 'True' se a posição estiver dentro dos limites do mapa e o terreno for do tipo 'Terra', caso contrário, retorna 'False'
terrenoETerra (x,y) mapa = 
    let (xMapa, yMapa) = converterCoordenadas (x,y)
    in estaNoMapa (x,y) mapa && ((mapa !! floor yMapa) !! floor xMapa == Terra)

moverInimigo :: Tempo -- ^ Tempo decorrido desde a última atualização, usado para calcular o deslocamento
             -> Inimigo -- ^ O inimigo cuja posição deve ser atualizada
             -> Mapa -- ^ O mapa do jogo, que contém informações sobre os terrenos
             -> Inimigo -- ^ O inimigo atualizado com a nova posição ou direção
moverInimigo tempo inimigo mapa = 
    let direcao = direcaoInimigo inimigo
        (x,y) = posicaoInimigo inimigo
        largura = (larguratile/2) + 1
        altura = (alturatile/2) + 1
    in case direcao of
        Norte -> if terrenoETerra (x, y+altura) mapa then inimigo { posicaoInimigo = (x, y+(velocidadeInimigo inimigo *tempo)) }
                 else if terrenoETerra (x+largura, y) mapa then inimigo { direcaoInimigo = Este }
                 else if terrenoETerra (x-largura, y) mapa then inimigo { direcaoInimigo = Oeste }
                 else inimigo
        Sul -> if terrenoETerra (x, y-altura) mapa then inimigo { posicaoInimigo = (x, y-(velocidadeInimigo inimigo *tempo)) }
               else if terrenoETerra (x+largura, y) mapa then inimigo { direcaoInimigo = Este }
               else if terrenoETerra (x-largura, y) mapa then inimigo { direcaoInimigo = Oeste }
               else inimigo
        Este -> if terrenoETerra (x+largura, y) mapa then inimigo { posicaoInimigo = (x+(velocidadeInimigo inimigo *tempo), y) }
                else if terrenoETerra (x, y-altura) mapa then inimigo { direcaoInimigo = Sul }
                else if terrenoETerra (x, y+altura) mapa then inimigo { direcaoInimigo = Norte }
                else inimigo
        Oeste -> if terrenoETerra (x-largura, y) mapa then inimigo { posicaoInimigo = (x-(velocidadeInimigo inimigo *tempo), y) }
                 else if terrenoETerra (x, y-largura) mapa then inimigo { direcaoInimigo = Sul }
                 else if terrenoETerra (x, y+altura) mapa then inimigo { direcaoInimigo = Norte }
                 else inimigo


converterCoordenadas :: (Float, Float) -- ^ A posição no sistema gráfico do jogo, representada como uma tupla : (xGloss, yGloss)
                     -> (Float, Float) -- ^ Tupla querepresenta as coordenadas convertidas no sistema do mapa
converterCoordenadas (xGloss, yGloss) =
    let xMapa = (xGloss + (650)) / larguratile
        yMapa = (450.5 - yGloss) / alturatile
    in (xMapa, yMapa)


atualizarInimigos :: [Inimigo] -- ^ Lista de inimigos cujas posições precisam ser atualizadas
                  -> Tempo -- ^ Valor que representa o tempo decorrido desde a última atualização
                  -> Mapa -- ^ O mapa atual do jogo, que pode influenciar o movimento dos inimigos
                  -> [Inimigo] -- ^ Retorna uma nova lista de inimigos com suas posições atualizadas
atualizarInimigos inimigos delta mapa =
    map (\inimigo -> moverInimigo delta inimigo mapa) inimigos
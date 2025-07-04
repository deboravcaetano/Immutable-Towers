module Atualizar where
import Type 
import Movimentacao


atualizarInimigos :: [Inimigo] -> Float -> EstadoJanela -> [Inimigo]
atualizarInimigos inimigos delta estado =
    map (\inimigo -> moverInimigo delta inimigo estado) inimigos


atualizarJogo :: Float -> Jogo -> EstadoJanela -> Jogo
atualizarJogo delta jogo estado =
    jogo { inimigosJogo = atualizarInimigos (inimigosJogo jogo) delta estado }
    
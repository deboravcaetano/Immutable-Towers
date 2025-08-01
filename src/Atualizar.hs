module Atualizar where
import Type 
import Movimentacao
import Ataque
import Data.List
import Data.List (partition)


atualizarInimigos :: [Inimigo] -> Float -> EstadoJanela -> [Inimigo]
atualizarInimigos inimigos delta estado =
    map (\inimigo -> moverInimigo delta inimigo estado) inimigos


atualizarPortal :: Float -> Portal -> (Portal, [Inimigo])
atualizarPortal delta portal
    | not (ativo portal) = (portal, [])   -- Portal inativo
    | null (ondasPortal portal) = (portal { ativo = False }, [])  -- Sem ondas restantes
    | otherwise = atualizarOndas delta portal


atualizarOndas :: Float -> Portal -> (Portal, [Inimigo])
atualizarOndas delta portal = 
    let (ondasAtualizadas, novosInimigos) = atualizarListaOndas delta (ondasPortal portal)
    in (portal { ondasPortal = ondasAtualizadas }, novosInimigos)


atualizarListaOndas :: Float -> [Onda] -> ([Onda], [Inimigo])
atualizarListaOndas _ [] = ([], [])
atualizarListaOndas delta (onda:resto)
    | entradaOnda onda > 0 = 
        -- Espera para ativar a onda
        let novaOnda = onda { entradaOnda = entradaOnda onda - delta }
        in (novaOnda : resto, [])
    | tempoOnda onda > 0 = 
        -- Espera para lançar o próximo inimigo
        let novaOnda = onda { tempoOnda = tempoOnda onda - delta }
        in (novaOnda : resto, [])
    | otherwise = 
        -- Hora de lançar inimigo
        case inimigosOnda onda of
            [] -> 
                -- Onda terminada, passa para a próxima
                atualizarListaOndas delta resto
            (inimigo:restoInimigos) ->
                -- Lança próximo inimigo
                let novaOnda = onda { 
                        inimigosOnda = restoInimigos,
                        tempoOnda = cicloOnda onda  -- Reinicia contador
                    }
                    (ondasRestantes, outrosInimigos) = atualizarListaOndas delta (novaOnda : resto)
                in (ondasRestantes, inimigo : outrosInimigos)


{-
Atualiza os portais do jogo e vai "ficando" com os inimigos que vai lançando para depois adicionar
na lista de inimigosJogo

-}
atualizarPortais :: [Portal] -> Float -> ([Portal], [Inimigo])
atualizarPortais portais delta = 
    let resultados = map (atualizarPortal delta) portais -- Aplica a função atualizarPortal delta a cada portal na lista portais. Resultaado: lista de tuplas
        portaisAtualizados = map fst resultados -- fst pega o primeiro elemento de uma tupla, resultado é a lista dos portais atualizados sem inimigos
        inimigosNovos = concatMap snd resultados --  Transforma a lista de tuplas em lista de listas de inimigos
    in (portaisAtualizados, inimigosNovos)


atualizarTorres :: Float -> [Torre] -> [Inimigo] -> ([Torre], [Inimigo])
atualizarTorres _ [] inimigos = ([], inimigos)
atualizarTorres delta (t:ts) inimigos =
    let (torreAtualizada, novosInimigos) = atualizarTorre delta inimigos t
        (torresRestantes, inimigosRestantes) = atualizarTorres delta ts novosInimigos
    in (torreAtualizada : torresRestantes, inimigosRestantes)


atualizarTorre :: Float -> [Inimigo] -> Torre -> (Torre, [Inimigo])
atualizarTorre delta inimigos torre
    | tempoTorre torre > 0 = (torre { tempoTorre = tempoTorre torre - delta }, inimigos)
    | otherwise = 
        let alvos = inimigosNoAlcance torre inimigos 
            idsAlvos = map (\inimigo -> (posicaoInimigo inimigo, vidaInimigo inimigo)) alvos
            novosInimigos = map (atualizarInimigoSeAlvo idsAlvos torre) inimigos
        in (torre { tempoTorre = cicloTorre torre }, novosInimigos)
  where
    atualizarInimigoSeAlvo :: [((Float, Float), Float)] -> Torre -> Inimigo -> Inimigo
    atualizarInimigoSeAlvo alvos torre inimigo
        | any (\(pos, vida) -> pos == posicaoInimigo inimigo && vida == vidaInimigo inimigo) alvos =
            atingeInimigo torre inimigo
        | otherwise = inimigo


atualizarProjeteisInimigos :: Float -> [Inimigo] -> [Inimigo]
atualizarProjeteisInimigos delta = map (atualizarProjeteisInimigo delta)


atualizarJogo :: Float -> Jogo -> EstadoJanela -> Jogo
atualizarJogo delta jogo estado =
  let (portaisAtualizados, inimigosNovos) = atualizarPortais (portaisJogo jogo) delta
      (torresAtualizadas, inimigosAtualizados) = atualizarTorres delta (torresJogo jogo) (inimigosJogo jogo)
      todosInimigos = inimigosAtualizados ++ inimigosNovos
      inimigosComProjeteis = atualizarProjeteisInimigos delta todosInimigos
      inimigosMovidos = atualizarInimigos inimigosComProjeteis delta estado
      

      (novaBase, inimigosQueSobram) = verificarColisoesBase (baseJogo jogo) inimigosMovidos
      
      inimigosComDano = map (aplicarDanoProjeteis delta) inimigosQueSobram
  in jogo { 
      baseJogo = novaBase,  -- base atualizada com dano
      portaisJogo = portaisAtualizados,
      torresJogo = torresAtualizadas,
      inimigosJogo = inimigosComDano  
  }


-- Função auxiliar para aplicar dano de projéteis
aplicarDanoProjeteis :: Float -> Inimigo -> Inimigo
aplicarDanoProjeteis delta inimigo =
    let (_, dano) = atualizarListaProjeteis delta (projeteisInimigo inimigo)
        vidaNova = vidaInimigo inimigo - dano
    in inimigo { vidaInimigo = max 0 vidaNova }
    

verificarColisoesBase :: Base -> [Inimigo] -> (Base, [Inimigo])
verificarColisoesBase base inimigos =
  let (colidiram, restantes) = partition (colidiuComBase base) inimigos -- partition divide em dois, os que verificam a condição e os que não verificam
      danoTotal = sum $ map ataqueInimigo colidiram
      novaBase = base { vidaBase = vidaBase base - danoTotal }
  in (novaBase, restantes)


colidiuComBase :: Base -> Inimigo -> Bool
colidiuComBase base inimigo = 
  distancia (posicaoInimigo inimigo) (posicaoBase base) < 50
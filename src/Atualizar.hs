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
    | tempoTorre torre > 0 = 
        (torre { tempoTorre = tempoTorre torre - delta }, inimigos)
        
    | otherwise = 
        let alvos = take (rajadaTorre torre) $  
                   inimigosNoAlcance torre inimigos
                   
            novosInimigos = map (\inimigo -> 
                if inimigo `elem` alvos  
                    then atingeInimigo torre inimigo
                    else inimigo
                ) inimigos
        in (torre { tempoTorre = cicloTorre torre }, novosInimigos)


atualizarProjeteisInimigos :: Float -> [Inimigo] -> [Inimigo]
atualizarProjeteisInimigos delta = map (atualizarProjeteisInimigo delta)


atualizarJogo :: Float -> Jogo -> EstadoJanela -> Jogo
atualizarJogo delta jogo estado =
  let -- atualiza portais e pega novos inimigos
      (portaisAtualizados, inimigosNovos) = atualizarPortais (portaisJogo jogo) delta
      
      -- junta todos os inimigos (novos e existentes)
      todosInimigos = inimigosJogo jogo ++ inimigosNovos
      
      -- aplica dano das torres
      (torresAtualizadas, inimigosAposTorres) = atualizarTorres delta (torresJogo jogo) todosInimigos
      
      -- atualiza projéteis nos inimigos
      inimigosComProjeteis = atualizarProjeteisInimigos delta inimigosAposTorres
      
      -- move os inimigos
      inimigosMovidos = atualizarInimigos inimigosComProjeteis delta estado
      
      -- verifica colisões com a base
      (novaBase, inimigosQueSobram, butimTotal) = verificarColisoesBase (baseJogo jogo) inimigosMovidos
      
      -- filtra inimigos vivos
      inimigosVivos = filter (\i -> vidaInimigo i > 0) inimigosQueSobram
      
      -- atualiza créditos
      baseFinal = novaBase { creditosBase = creditosBase novaBase + butimTotal }
      
  in jogo { 
      baseJogo = baseFinal,
      portaisJogo = portaisAtualizados,
      torresJogo = torresAtualizadas,
      inimigosJogo = inimigosVivos
  }


aplicarDanoProjeteis :: Float -> Inimigo -> (Inimigo, Creditos)
aplicarDanoProjeteis delta inimigo =
    let (projAtualizados, dano) = atualizarListaProjeteis delta (projeteisInimigo inimigo)
        vidaNova = vidaInimigo inimigo - dano
        butim = if vidaNova <= 0 then butimInimigo inimigo else 0
    in (inimigo { 
        vidaInimigo = max 0 vidaNova,
        projeteisInimigo = projAtualizados
    }, butim)
    

verificarColisoesBase :: Base -> [Inimigo] -> (Base, [Inimigo], Creditos)
verificarColisoesBase base inimigos =
  let (colidiram, naoColidiram) = partition (colidiuComBase base) inimigos -- partition divide em dois, aqueles que verificam a condição e os que não
      
      danoTotal = sum $ map ataqueInimigo colidiram
      
      -- calcula butim dos inimigos que morreram (tanto os que colidiram quanto outros)
      todosInimigos = colidiram ++ naoColidiram
      butimTotal = sum [ butimInimigo i | i <- todosInimigos, vidaInimigo i <= 0 ]
      
      novaBase = base { 
          vidaBase = vidaBase base - danoTotal,
          creditosBase = creditosBase base + butimTotal
      }
      
  in (novaBase, naoColidiram, butimTotal)

colidiuComBase :: Base -> Inimigo -> Bool
colidiuComBase base inimigo = 
  distancia (posicaoInimigo inimigo) (posicaoBase base) < 50
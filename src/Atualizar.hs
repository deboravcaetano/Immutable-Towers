module Atualizar where
import Type 
import Movimentacao


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


atualizarJogo :: Float -> Jogo -> EstadoJanela -> Jogo
atualizarJogo delta jogo estado =
    let (portaisAtualizados, inimigosNovos) = atualizarPortais (portaisJogo jogo) delta
        inimigosAntigos = inimigosJogo jogo
        inimigosAtualizados = atualizarInimigos inimigosAntigos delta estado
        novosInimigos = inimigosAtualizados ++ inimigosNovos
    in jogo { 
        portaisJogo = portaisAtualizados,
        inimigosJogo = novosInimigos 
    }
    
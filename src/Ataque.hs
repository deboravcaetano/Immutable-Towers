module Ataque where
import Type


distancia :: Posicao -> Posicao -> Float
distancia (x1, y1) (x2, y2) = sqrt $ (x1 - x2) ^ 2 + (y1 - y2) ^ 2


inimigosNoAlcance :: Torre -> [Inimigo] -> [Inimigo]
inimigosNoAlcance torre inimigos = filter (\inimigo -> distancia (posicaoTorre torre) (posicaoInimigo inimigo) <= alcanceTorre torre) inimigos


-- Aplicar efeitos sinergia
-- Fogo e Gelo cancelam-se mutuamente
-- Fogo e Resina dobra a duração do fogo 
-- Projéteis iguais somam as suas durações. 
-- As restantes combinações de projéteis não resultam em sinergias 


aplicarSinergia :: Projetil -> [Projetil] -> [Projetil]
aplicarSinergia novo [] = [novo]
aplicarSinergia novo (p:ps)
    | tipoProjetil novo == Fogo && tipoProjetil p == Gelo = ps  
    | tipoProjetil novo == Gelo && tipoProjetil p == Fogo = ps 

    | tipoProjetil novo == Fogo && tipoProjetil p == Resina = 
        novo { duracaoProjetil = dobraDuracao (duracaoProjetil novo) } : filter ((/= Resina) . tipoProjetil) ps
    | tipoProjetil novo == Resina && tipoProjetil p == Fogo =
        novo { duracaoProjetil = dobraDuracao (duracaoProjetil p)} : filter ((/= Resina) . tipoProjetil) ps
        
    | tipoProjetil novo == tipoProjetil p = 
        [p { duracaoProjetil = somaDuracao (duracaoProjetil p) (duracaoProjetil novo) }] ++ ps
    | otherwise = p : aplicarSinergia novo ps

  where
    dobraDuracao :: Duracao -> Duracao
    dobraDuracao (Finita t) = Finita (t * 2)
    dobraDuracao d = d
    
    somaDuracao :: Duracao -> Duracao -> Duracao
    somaDuracao Infinita _ = Infinita
    somaDuracao _ Infinita = Infinita
    somaDuracao (Finita t1) (Finita t2) = Finita (t1 + t2)


atingeInimigo :: Torre -> Inimigo -> Inimigo
atingeInimigo torre inimigo = 
    let vidaNova = (vidaInimigo inimigo) - (danoTorre torre)
        projeteisNovos = aplicarSinergia (projetilTorre torre) (projeteisInimigo inimigo)
    in inimigo { 
        vidaInimigo = max 0 vidaNova,  -- Vida não pode ser negativa
        projeteisInimigo = projeteisNovos 
    }


atualizarProjeteisInimigo :: Float -> Inimigo -> Inimigo
atualizarProjeteisInimigo delta inimigo =
    let (projeteisAtualizados, danoAcumulado) = atualizarListaProjeteis delta (projeteisInimigo inimigo)
        vidaNova = vidaInimigo inimigo - danoAcumulado
    in inimigo { 
        vidaInimigo = max 0 vidaNova,
        projeteisInimigo = projeteisAtualizados 
    }


atualizarListaProjeteis :: Float -> [Projetil] -> ([Projetil], Float)
atualizarListaProjeteis _ [] = ([], 0)
atualizarListaProjeteis delta (p:ps) =
    let (psAtualizados, dano) = atualizarListaProjeteis delta ps
        (pAtualizado, danoP) = atualizarProjetil delta p
    in (if duracaoProjetil pAtualizado > Finita 0 then pAtualizado : psAtualizados else psAtualizados, dano + danoP)


atualizarProjetil :: Float -> Projetil -> (Projetil, Float)
atualizarProjetil delta p = 
    case tipoProjetil p of
        Fogo -> 
            case duracaoProjetil p of
                Finita t | t > 0 -> 
                    let t' = t - delta
                        dano = 10 * delta  -- Dano por segundo
                    in (p { duracaoProjetil = Finita t' }, dano)
        Gelo -> 
            case duracaoProjetil p of
                Finita t | t > 0 -> 
                    let t' = t - delta
                        dano = 10 * delta  -- Dano por segundo
                    in (p { duracaoProjetil = Finita t' }, dano)
        Resina -> 
            case duracaoProjetil p of
                Finita t | t > 0 -> 
                    let t' = t - delta
                        dano = 10 * delta  -- Dano por segundo
                    in (p { duracaoProjetil = Finita t' }, dano)
        _ -> (p, 0)
module Game where
import Type 





jogoInicial :: Jogo
jogoInicial = Jogo {
    baseJogo = Base 100 (325.0, 200.0) 500,
    portaisJogo = [],
    torresJogo = [],
    inimigosJogo = [],
    mapaJogo = [[]],
    lojaJogo = [],
    nLinhas = 0,
    nColunas = 0         
}


iniciarJogo :: Int -> Jogo
iniciarJogo nivel = case nivel of
    1 -> Jogo
        { baseJogo = Base {vidaBase = 100 , posicaoBase = (325.0, 200.0) , creditosBase = 500}
        , portaisJogo = [portal1]
        , torresJogo = []
        , mapaJogo = mapaNivel1
        , inimigosJogo = []
        ,lojaJogo = [ 
        (100, criarTorre Fogo), 
        (150, criarTorre Gelo), 
        (200, criarTorre Resina)
        ]
        , nLinhas = 14
        , nColunas = 14
        }
    _ -> iniciarJogo 1


criarTorre :: TipoProjetil -> Torre
criarTorre tipo = 
    let (dano, alcance, rajada, ciclo, duracao) = case tipo of
                    Fogo  -> (50, 3.0, 1, 1.5, Finita 2.0)
                    Resina    -> (30, 2.5, 2, 2.0, Finita 1.8)
                    Gelo     -> (50, 2.0, 3, 3.0, Finita 1.5)
    in Torre {
    posicaoTorre = (-1650, -950), 
    projetilTorre = Projetil {
        tipoProjetil = tipo,
        duracaoProjetil = duracao 
    },
    danoTorre = dano,
    alcanceTorre = alcance,
    rajadaTorre = rajada,
    cicloTorre = ciclo ,
    tempoTorre = 10

}

mapaNivel1 =
    [[r, r, r, r, r, r, r, r, r, r, r, r, r, r],
     [r, t, t, t, r, r, r, r, r, r, r, r, r, r],
     [t, t, r, t, r, r, r, r, r, r, r, r, r, r],
     [r, r, r, t, r, r, r, r, r, r, r, t, t, t],
     [r, t, t, t, r, r, r, r, r, r, r, t, r, at],
     [r, t, r, r, r, r, r, r, r, r, at, t, at, a],
     [r, t, r, r, r, r, r, r, at, at, a, t, a, a],
     [r, t, t, t, t, at, at, at, a, a, a, t, t, a],
     [r, r, r, r, t, a, a, a, a, a, r, r, t, r],
     [r, r, r, at, t, a, a, a, a, t, t, t, t, r],
     [r, r, at, a, t, t, t, t, t, t, r, r, r, r],
     [r, r, a, a, a, r, r, r, r, r, r, r, r, r],
     [r, at, a, a, r, r, r, r, r, r, r, t, t, t],
     [at, a, a, r, r, r, r, r, r, r, t, t, t, t]
    ] 
    where
        t = Terra
        r = Relva
        a = Agua
        at = AguaTerra


portal1 :: Portal
portal1 = Portal (-325, 225) [onda1, onda2] True
  where
    onda1 = Onda 
        { inimigosOnda = [inimigo1, inimigo2,inimigo3]
        , cicloOnda = 3.0
        , tempoOnda = 0.0
        , entradaOnda = 0.0  -- Começa imediatamente
        }
    
    onda2 = Onda 
        { inimigosOnda = [inimigo2, inimigo1]
        , cicloOnda = 2.0
        , tempoOnda = 0.0
        , entradaOnda = 20.0  -- Inicia após 20 segundos
        }

inimigo1 = Inimigo {posicaoInimigo = (-325, 225), vidaInimigo = 100, direcaoInimigo = Sul, velocidadeInimigo = 30.0, tipoInimigo = Flora, projeteisInimigo = []}
inimigo2 = Inimigo {posicaoInimigo = (-325, 225), vidaInimigo = 100, direcaoInimigo = Sul, velocidadeInimigo = 20.0, tipoInimigo = Stella, projeteisInimigo = []}
inimigo3 = Inimigo {posicaoInimigo = (-325, 225), vidaInimigo = 100, direcaoInimigo = Sul, velocidadeInimigo = 30.0, tipoInimigo = Stella, projeteisInimigo = []}


{-
terminouJogo :: Jogo -> Bool
terminouJogo jogo = ganhouJogo jogo || perdeuJogo jogo


ganhouJogo :: Jogo -> Bool
ganhouJogo jogo = null (inimigosJogo jogo) && vidaBase (baseJogo jogo) > 0


perdeuJogo :: Jogo -> Bool
perdeuJogo jogo = vidaBase (baseJogo jogo) <= 0
-}
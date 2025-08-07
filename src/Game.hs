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
    2 -> Jogo
        { baseJogo = Base {vidaBase = 100 , posicaoBase = (75.0, -300.0) , creditosBase = 500}
        , portaisJogo = [portal2]
        , torresJogo = []
        , mapaJogo = mapaNivel2
        , inimigosJogo = []
        ,lojaJogo = [ 
        (100, criarTorre Fogo), 
        (150, criarTorre Gelo), 
        (200, criarTorre Resina)
        ]
        , nLinhas = 14
        , nColunas = 14
        }
    3 -> Jogo
        { baseJogo = Base {vidaBase = 100 , posicaoBase = (0.0, 0.0) , creditosBase = 500}
        , portaisJogo = [portal3]
        , torresJogo = []
        , mapaJogo = mapaNivel3
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
    let (dano, alcance, rajada, ciclo, duracao, reducao) = case tipo of
                    Fogo   -> (15, 70.8, 1, 1.5, Finita 2.0, 0.0)
                    Resina -> (30, 70.8, 2, 2.0, Finita 1.8, 0.9)
                    Gelo   -> (20, 70.8, 3, 3.0, Finita 1.5, 0.0)
    in Torre {
    posicaoTorre = (-1650, -950), 
    projetilTorre = Projetil {
        tipoProjetil = tipo,
        duracaoProjetil = duracao,
        reducaoVelocidade = reducao 
    },
    danoTorre = dano,
    alcanceTorre = alcance,
    rajadaTorre = rajada,
    cicloTorre = ciclo ,
    tempoTorre = 0

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

inimigo1 = Inimigo {posicaoInimigo = (-325, 225), vidaInimigo = 100, butimInimigo = 10, direcaoInimigo = Sul, velocidadeInimigo = 50.0, ataqueInimigo = 10, tipoInimigo = Flora, projeteisInimigo = []}
inimigo2 = Inimigo {posicaoInimigo = (-325, 225), vidaInimigo = 90, butimInimigo = 10, direcaoInimigo = Sul, velocidadeInimigo = 20.0, ataqueInimigo = 10, tipoInimigo = Stella, projeteisInimigo = []}
inimigo3 = Inimigo {posicaoInimigo = (-325, 225), vidaInimigo = 90, butimInimigo = 10 ,direcaoInimigo = Sul, velocidadeInimigo = 30.0, ataqueInimigo = 10, tipoInimigo = Stella, projeteisInimigo = []}



mapaNivel2 =
    [[a , a , a , a , a , a , r , r , r , r , r , r , t , r],
     [a , a , a , a , a , r , r , r , r , r , r , r , t , r],
     [a , a , a , a , a , at , r , r , r , r , r , t , t , r],
     [a , a , a , a , a , t , t , t , t , t , t , t , r , r],
     [a , a , r , r , r , t , r , r , r , r , r , r , r , r],
     [r , r , r , r , r , t , t , t , r , r , r , r , r , r],
     [r , r , r , r , r , r , r , t , r , r , r , r , r , r],
     [r , r , r , r , r , r , t , t , r , r , r , r , r , r],
     [r , r , t , t , t , t , t , r , r , r , r , r , r , r],
     [r , r , t , r , r , r , r , r , r , r , r , r , r , r],
     [r , r , t , r , r , r , r , r , r , r , r , r , r , r],
     [r , r , t , t , t , t , t , t , r , r , r , r , r , r],
     [r , r , r , r , r , r , r , t , r , r , r , r , r , r],
     [r , r , r , r , r , r , r , t , t , r , r , r , r , r]
     ]
    where
        t = Terra
        r = Relva
        a = Agua
        at = AguaTerra

portal2 :: Portal
portal2 = Portal (275, 325) [onda1, onda2] True
  where
    onda1 = Onda 
        { inimigosOnda = [inimigo4, inimigo5,inimigo6]
        , cicloOnda = 3.0
        , tempoOnda = 0.0
        , entradaOnda = 0.0  -- Começa imediatamente
        }
    
    onda2 = Onda 
        { inimigosOnda = [inimigo5, inimigo6]
        , cicloOnda = 2.0
        , tempoOnda = 0.0
        , entradaOnda = 20.0  -- Inicia após 20 segundos
        }

inimigo4 = Inimigo {posicaoInimigo = (275, 325) , vidaInimigo = 100, butimInimigo = 10, direcaoInimigo = Sul, velocidadeInimigo = 50.0, ataqueInimigo = 10, tipoInimigo = Flora, projeteisInimigo = []}
inimigo5 = Inimigo {posicaoInimigo = (275, 325) , vidaInimigo = 90, butimInimigo = 10, direcaoInimigo = Sul, velocidadeInimigo = 20.0, ataqueInimigo = 10, tipoInimigo = Flora, projeteisInimigo = []}
inimigo6 = Inimigo {posicaoInimigo = (275, 325) , vidaInimigo = 90, butimInimigo = 10 ,direcaoInimigo = Sul, velocidadeInimigo = 30.0, ataqueInimigo = 10, tipoInimigo = Stella, projeteisInimigo = []}

mapaNivel3 =
    [[r , r , r , r , r , r , r , r , r , r , r , r , t , r],
     [r , r , r , r , r , r , r , r , r , r , r , r , t , r],
     [r , r , r , t , t , t , t , t , t , t , t , t , t , r],
     [r , r , t , t , r , r , r , r , r , r , r , r , r , r],
     [r , t , t , r , r , r , r , r , r , r , r , r , r , r],
     [r , t , r , t , t , t , t , t , t , t , t , t , t , t],
     [r , t , r , t , r , r , r , r , r , r , r , r , r , t],
     [r , t , r , t , r , r , r , t , t , t , t , t , t , t],
     [r , t , r , t , r , r , r , r , r , r , r , r , r , r],
     [r , t , r , t , t , t , t , t , t , t , t , t , t , r],
     [r , t , t , r , r , r , r , r , r , r , r , r , t , r],
     [r , r , t , t , t , t , t , t , t , t , t , t , t , r],
     [r , r , r , r , r , r , r , r , r , r , r , r , r , r],
     [r , r , r , r , r , r , r , r , r , r , r , r , r , r]
     ]
    where
        t = Terra
        r = Relva


portal3 :: Portal
portal3 = Portal (275, 325) [onda1, onda2] True
  where
    onda1 = Onda 
        { inimigosOnda = [inimigo7, inimigo8,inimigo9]
        , cicloOnda = 3.0
        , tempoOnda = 0.0
        , entradaOnda = 0.0  -- Começa imediatamente
        }
    
    onda2 = Onda 
        { inimigosOnda = [inimigo7, inimigo9]
        , cicloOnda = 2.0
        , tempoOnda = 0.0
        , entradaOnda = 20.0  -- Inicia após 20 segundos
        }


inimigo7 = Inimigo {posicaoInimigo = (275, 325) , vidaInimigo = 100, butimInimigo = 10, direcaoInimigo = Sul, velocidadeInimigo = 50.0, ataqueInimigo = 10, tipoInimigo = Flora, projeteisInimigo = []}
inimigo8 = Inimigo {posicaoInimigo = (275, 325) , vidaInimigo = 90, butimInimigo = 10, direcaoInimigo = Sul, velocidadeInimigo = 20.0, ataqueInimigo = 10, tipoInimigo = Flora, projeteisInimigo = []}
inimigo9 = Inimigo {posicaoInimigo = (275, 325) , vidaInimigo = 90, butimInimigo = 10 ,direcaoInimigo = Sul, velocidadeInimigo = 30.0, ataqueInimigo = 10, tipoInimigo = Stella, projeteisInimigo = []}

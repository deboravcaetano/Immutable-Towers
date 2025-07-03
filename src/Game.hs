module Game where
import Type 
import Type (Inimigo(direcaoInimigo))




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
criarTorre tipo = Torre {
    posicaoTorre = (-1650, -950), 
    projetilTorre = Projetil {
        tipoProjetil = tipo
    }
}

mapaNivel1 =
    [[r, r, r, r, r, r, r, r, r, r, r, r, r, r],
     [r, t, t, t, r, r, r, r, r, r, r, r, r, r],
     [t, t, r, t, r, r, r, r, r, r, r, r, r, r],
     [r, r, r, t, r, r, r, r, r, r, r, t, t, t],
     [r, t, t, t, r, r, r, r, r, r, r, t, r, a],
     [r, t, r, r, r, r, r, r, r, r, a, t, a, a],
     [r, t, r, r, r, r, r, r, a, a, a, t, a, a],
     [r, t, t, t, t, a, a, a, a, a, a, t, t, a],
     [r, r, r, r, t, a, a, a, a, a, r, r, t, r],
     [r, r, r, a, t, a, a, a, a, t, t, t, t, r],
     [r, r, a, a, t, t, t, t, t, t, r, r, r, r],
     [r, r, a, a, r, r, r, r, r, r, r, r, r, r],
     [r, a, a, r, r, r, r, r, r, r, r, a, a, a],
     [a, a, a, r, r, r, r, r, r, r, r, a, a, a]
    ] 
    where
        t = Terra
        r = Relva
        a = Agua

portal1 :: Portal
portal1 = Portal (-325, 225) [] True



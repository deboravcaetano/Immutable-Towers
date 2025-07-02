module Game where
import Type 

iniciarJogo :: Int -> Jogo
iniciarJogo nivel = case nivel of
    1 -> Jogo
        { baseJogo = Base 100 (325.0, 200.0) 500
        , portaisJogo = [portal1]
        --, torresJogo = []
        , mapaJogo = mapaNivel1
        --, inimigosJogo = []
        --, lojaJogo = lojaBasica
        }
    _ -> iniciarJogo 1


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



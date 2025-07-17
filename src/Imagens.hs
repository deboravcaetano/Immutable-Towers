module Imagens where
import Graphics.Gloss
import Type


carregarImgMenu :: IO Picture
carregarImgMenu = loadBMP "assets/Fundos/MenuPrincipal.bmp"

carregarImgFundoNivel :: IO Picture
carregarImgFundoNivel = loadBMP "assets/Fundos/JanelaNivel.bmp"

carregarImgBotaoJogar :: IO Picture
carregarImgBotaoJogar = loadBMP "assets/Botoes/BotaoJogar.bmp"

carregarImgGoal :: IO Picture
carregarImgGoal = loadBMP "assets/Fundos/GoalPage.bmp"

carregarImgFirePage :: IO Picture
carregarImgFirePage = loadBMP "assets/Fundos/FireTowerPage.bmp"

carregarImgIcePage :: IO Picture
carregarImgIcePage = loadBMP "assets/Fundos/IceTowerPage.bmp"

carregarImgResinPage :: IO Picture
carregarImgResinPage = loadBMP "assets/Fundos/ResinTowerPage.bmp"

carregarImgBotaoVoltar :: IO Picture
carregarImgBotaoVoltar = loadBMP "assets/Botoes/BotaoVoltar.bmp"

carregarImgNivel1 :: IO Picture
carregarImgNivel1 = loadBMP "assets/Botoes/Nivel1.bmp"

carregarImgNivel2 :: IO Picture
carregarImgNivel2 = loadBMP "assets/Botoes/Nivel2.bmp"

carregarImgNivel3 :: IO Picture
carregarImgNivel3 = loadBMP "assets/Botoes/Nivel3.bmp"

carregarImgBotaoRegras :: IO Picture
carregarImgBotaoRegras = loadBMP "assets/Botoes/BotaoRegras.bmp"

carregarImgRelva :: IO Picture
carregarImgRelva = loadBMP "assets/Terrenos/Relva.bmp"

carregarImgTerra :: IO Picture
carregarImgTerra = loadBMP "assets/Terrenos/Terra.bmp"

carregarImgAgua :: IO Picture
carregarImgAgua = loadBMP "assets/Terrenos/Agua.bmp"

carregarImgAguaTerra :: IO Picture
carregarImgAguaTerra = loadBMP "assets/Terrenos/AguaETerra.bmp"

carregarImgBase :: IO Picture
carregarImgBase = loadBMP "assets/Base/Torre/Base.bmp"

carregarImgPortal :: IO Picture
carregarImgPortal = loadBMP "assets/Portal/Portal.bmp"

carregarImgAbaAberta:: IO Picture
carregarImgAbaAberta = loadBMP "assets/Botoes/BotaoAbaAberta.bmp"

carregarImgAbaFechada:: IO Picture
carregarImgAbaFechada = loadBMP "assets/Botoes/BotaoAbaFechada.bmp"

carregarImgFundoMapa:: IO Picture
carregarImgFundoMapa = loadBMP "assets/Fundos/FundoMapa.bmp"

carregarImgLoja :: IO Picture
carregarImgLoja = loadBMP "assets/Loja/Loja.bmp"

carregarImgBotaoTorreFogo :: IO Picture
carregarImgBotaoTorreFogo = loadBMP "assets/Botoes/TorreFogoBotao.bmp"

carregarImgBotaoTorreGelo :: IO Picture
carregarImgBotaoTorreGelo = loadBMP "assets/Botoes/TorreGeloBotao.bmp"

carregarImgBotaoTorreResina :: IO Picture
carregarImgBotaoTorreResina = loadBMP "assets/Botoes/TorreResinaBotao.bmp"

carregarImgTorreResina :: IO Picture
carregarImgTorreResina = loadBMP "assets/Torres/TorreResina.bmp"

carregarImgTorreGelo :: IO Picture
carregarImgTorreGelo = loadBMP "assets/Torres/TorreGelo.bmp"

carregarImgTorreFogo :: IO Picture
carregarImgTorreFogo = loadBMP "assets/Torres/TorreFogo.bmp"

carregarImgsFlora :: IO [[Picture]]
carregarImgsFlora = do
    imgE100      <- loadBMP "assets/Personagens/Flora/FloraEsquerda100.bmp"
    imgE90      <- loadBMP "assets/Personagens/Flora/FloraEsquerda90.bmp"
    imgE80      <- loadBMP "assets/Personagens/Flora/FloraEsquerda80.bmp"
    imgE70      <- loadBMP "assets/Personagens/Flora/FloraEsquerda70.bmp"
    imgE60      <- loadBMP "assets/Personagens/Flora/FloraEsquerda60.bmp"
    imgE50      <- loadBMP "assets/Personagens/Flora/FloraEsquerda50.bmp"
    imgE40      <- loadBMP "assets/Personagens/Flora/FloraEsquerda40.bmp"
    imgE30      <- loadBMP "assets/Personagens/Flora/FloraEsquerda30.bmp"
    imgE20      <- loadBMP "assets/Personagens/Flora/FloraEsquerda20.bmp"
    imgE10      <- loadBMP "assets/Personagens/Flora/FloraEsquerda10.bmp"
    imgE0      <- loadBMP "assets/Personagens/Flora/FloraEsquerda0.bmp"

    imgD100      <- loadBMP "assets/Personagens/Flora/FloraDireita100.bmp"
    imgD90      <- loadBMP "assets/Personagens/Flora/FloraDireita90.bmp"
    imgD80      <- loadBMP "assets/Personagens/Flora/FloraDireita80.bmp"
    imgD70      <- loadBMP "assets/Personagens/Flora/FloraDireita70.bmp"
    imgD60      <- loadBMP "assets/Personagens/Flora/FloraDireita60.bmp"
    imgD50      <- loadBMP "assets/Personagens/Flora/FloraDireita50.bmp"
    imgD40      <- loadBMP "assets/Personagens/Flora/FloraDireita40.bmp"
    imgD30      <- loadBMP "assets/Personagens/Flora/FloraDireita30.bmp"
    imgD20      <- loadBMP "assets/Personagens/Flora/FloraDireita20.bmp"
    imgD10      <- loadBMP "assets/Personagens/Flora/FloraDireita10.bmp"
    imgD0      <- loadBMP "assets/Personagens/Flora/FloraDireita0.bmp"

    imgN100      <- loadBMP "assets/Personagens/Flora/FloraTras100.bmp"
    imgN90      <- loadBMP "assets/Personagens/Flora/FloraTras90.bmp"
    imgN80      <- loadBMP "assets/Personagens/Flora/FloraTras80.bmp"
    imgN70      <- loadBMP "assets/Personagens/Flora/FloraTras70.bmp"
    imgN60      <- loadBMP "assets/Personagens/Flora/FloraTras60.bmp"
    imgN50      <- loadBMP "assets/Personagens/Flora/FloraTras50.bmp"
    imgN40      <- loadBMP "assets/Personagens/Flora/FloraTras40.bmp"
    imgN30      <- loadBMP "assets/Personagens/Flora/FloraTras30.bmp"
    imgN20      <- loadBMP "assets/Personagens/Flora/FloraTras20.bmp"
    imgN10      <- loadBMP "assets/Personagens/Flora/FloraTras10.bmp"
    imgN0      <- loadBMP "assets/Personagens/Flora/FloraTras0.bmp"

    imgS100      <- loadBMP "assets/Personagens/Flora/FloraFrente100.bmp"
    imgS90      <- loadBMP "assets/Personagens/Flora/FloraFrente90.bmp"
    imgS80      <- loadBMP "assets/Personagens/Flora/FloraFrente80.bmp"
    imgS70      <- loadBMP "assets/Personagens/Flora/FloraFrente70.bmp"
    imgS60      <- loadBMP "assets/Personagens/Flora/FloraFrente60.bmp"
    imgS50      <- loadBMP "assets/Personagens/Flora/FloraFrente50.bmp"
    imgS40      <- loadBMP "assets/Personagens/Flora/FloraFrente40.bmp"
    imgS30      <- loadBMP "assets/Personagens/Flora/FloraFrente30.bmp"
    imgS20      <- loadBMP "assets/Personagens/Flora/FloraFrente20.bmp"
    imgS10      <- loadBMP "assets/Personagens/Flora/FloraFrente10.bmp"
    imgS0      <- loadBMP "assets/Personagens/Flora/FloraFrente0.bmp"

    return [[imgE100,imgE90,imgE80,imgE70,imgE60,imgE50,imgE40,imgE30,imgE20,imgE10,imgE0], 
            [imgD100,imgD90,imgD80,imgD70,imgD60,imgD50,imgD40,imgD30,imgD20,imgD10,imgD0], 
            [imgN100,imgN90,imgN80,imgN70,imgN60,imgN50,imgN40,imgN30,imgN20,imgN10,imgN0], 
            [imgS100,imgS90,imgS80,imgS70,imgS60,imgS50,imgS40,imgS30,imgS20,imgS10,imgS0]]

carregarImgsStella :: IO [[Picture]]
carregarImgsStella = do
    imgE100      <- loadBMP "assets/Personagens/Stella/StellaEsquerda100.bmp"
    imgE90      <- loadBMP "assets/Personagens/Stella/StellaEsquerda90.bmp"
    imgE80      <- loadBMP "assets/Personagens/Stella/StellaEsquerda80.bmp"
    imgE70      <- loadBMP "assets/Personagens/Stella/StellaEsquerda70.bmp"
    imgE60      <- loadBMP "assets/Personagens/Stella/StellaEsquerda60.bmp"
    imgE50      <- loadBMP "assets/Personagens/Stella/StellaEsquerda50.bmp"
    imgE40      <- loadBMP "assets/Personagens/Stella/StellaEsquerda40.bmp"
    imgE30      <- loadBMP "assets/Personagens/Stella/StellaEsquerda30.bmp"
    imgE20      <- loadBMP "assets/Personagens/Stella/StellaEsquerda20.bmp"
    imgE10      <- loadBMP "assets/Personagens/Stella/StellaEsquerda10.bmp"
    imgE0      <- loadBMP "assets/Personagens/Stella/StellaEsquerda0.bmp"

    imgD100      <- loadBMP "assets/Personagens/Stella/StellaDireita100.bmp"
    imgD90      <- loadBMP "assets/Personagens/Stella/StellaDireita90.bmp"
    imgD80      <- loadBMP "assets/Personagens/Stella/StellaDireita80.bmp"
    imgD70      <- loadBMP "assets/Personagens/Stella/StellaDireita70.bmp"
    imgD60      <- loadBMP "assets/Personagens/Stella/StellaDireita60.bmp"
    imgD50      <- loadBMP "assets/Personagens/Stella/StellaDireita50.bmp"
    imgD40      <- loadBMP "assets/Personagens/Stella/StellaDireita40.bmp"
    imgD30      <- loadBMP "assets/Personagens/Stella/StellaDireita30.bmp"
    imgD20      <- loadBMP "assets/Personagens/Stella/StellaDireita20.bmp"
    imgD10      <- loadBMP "assets/Personagens/Stella/StellaDireita10.bmp"
    imgD0      <- loadBMP "assets/Personagens/Stella/StellaDireita0.bmp"

    imgN100      <- loadBMP "assets/Personagens/Stella/StellaTras100.bmp"
    imgN90      <- loadBMP "assets/Personagens/Stella/StellaTras90.bmp"
    imgN80      <- loadBMP "assets/Personagens/Stella/StellaTras80.bmp"
    imgN70      <- loadBMP "assets/Personagens/Stella/StellaTras70.bmp"
    imgN60      <- loadBMP "assets/Personagens/Stella/StellaTras60.bmp"
    imgN50      <- loadBMP "assets/Personagens/Stella/StellaTras50.bmp"
    imgN40      <- loadBMP "assets/Personagens/Stella/StellaTras40.bmp"
    imgN30      <- loadBMP "assets/Personagens/Stella/StellaTras30.bmp"
    imgN20      <- loadBMP "assets/Personagens/Stella/StellaTras20.bmp"
    imgN10      <- loadBMP "assets/Personagens/Stella/StellaTras10.bmp"
    imgN0      <- loadBMP "assets/Personagens/Stella/StellaTras0.bmp"

    imgS100      <- loadBMP "assets/Personagens/Stella/StellaFrente100.bmp"
    imgS90      <- loadBMP "assets/Personagens/Stella/StellaFrente90.bmp"
    imgS80      <- loadBMP "assets/Personagens/Stella/StellaFrente80.bmp"
    imgS70      <- loadBMP "assets/Personagens/Stella/StellaFrente70.bmp"
    imgS60      <- loadBMP "assets/Personagens/Stella/StellaFrente60.bmp"
    imgS50      <- loadBMP "assets/Personagens/Stella/StellaFrente50.bmp"
    imgS40      <- loadBMP "assets/Personagens/Stella/StellaFrente40.bmp"
    imgS30      <- loadBMP "assets/Personagens/Stella/StellaFrente30.bmp"
    imgS20      <- loadBMP "assets/Personagens/Stella/StellaFrente20.bmp"
    imgS10      <- loadBMP "assets/Personagens/Stella/StellaFrente10.bmp"
    imgS0      <- loadBMP "assets/Personagens/Stella/StellaFrente0.bmp"


    return [[imgE100,imgE90,imgE80,imgE70,imgE60,imgE50,imgE40,imgE30,imgE20,imgE10,imgE0], 
            [imgD100,imgD90,imgD80,imgD70,imgD60,imgD50,imgD40,imgD30,imgD20,imgD10,imgD0], 
            [imgN100,imgN90,imgN80,imgN70,imgN60,imgN50,imgN40,imgN30,imgN20,imgN10,imgN0], 
            [imgS100,imgS90,imgS80,imgS70,imgS60,imgS50,imgS40,imgS30,imgS20,imgS10,imgS0]]


selecionaImagensDirecaoInimigo :: EstadoJanela -> Inimigo -> Picture
selecionaImagensDirecaoInimigo estado inimigo = 
    case tipoInimigo inimigo of
        Flora -> selecionaImagemInimigo (imagensFlora estado) inimigo
        Stella -> selecionaImagemInimigo (imagensStella estado) inimigo


-- Seleciona a lista de imagens para uma direção específica
selecionarListaPorDirecao :: [[Picture]] -> Direcao -> [Picture]
selecionarListaPorDirecao dirImgs dir = 
    case dir of
        Oeste -> dirImgs !! 0  
        Este  -> dirImgs !! 1  
        Norte -> dirImgs !! 2  
        Sul   -> dirImgs !! 3  


-- Seleciona a imagem específica dentro dessa lista baseada na vida do inimigo
selecionarImagemPorVida :: [Picture] -> Float -> Picture
selecionarImagemPorVida imgs vida
    | vida > 90 = imgs !! 0 
    | vida > 80 = imgs !! 1  
    | vida > 70 = imgs !! 2  
    | vida > 60 = imgs !! 3  
    | vida > 50 = imgs !! 4
    | vida > 40 = imgs !! 5
    | vida > 30 = imgs !! 6
    | vida > 20 = imgs !! 7
    | vida > 10 = imgs !! 8
    | vida > 0 = imgs !! 9  
    | otherwise = imgs !! 10


-- Principal que junta as duas funções
selecionaImagemInimigo :: [[Picture]] -> Inimigo -> Picture
selecionaImagemInimigo imgs inimigo = 
    let listaDirecao = selecionarListaPorDirecao imgs (direcaoInimigo inimigo)
    in selecionarImagemPorVida listaDirecao (vidaInimigo inimigo)
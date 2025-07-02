module CarregarImagens where
import Graphics.Gloss


carregarImgMenu :: IO Picture
carregarImgMenu = loadBMP "assets/Fundos/MenuPrincipal.bmp"

carregarImgFundoNivel :: IO Picture
carregarImgFundoNivel = loadBMP "assets/Fundos/JanelaNivel.bmp"

carregarImgBotaoJogar :: IO Picture
carregarImgBotaoJogar = loadBMP "assets/Botoes/BotaoJogar.bmp"

carregarImgFundoRegras :: IO Picture
carregarImgFundoRegras = loadBMP "assets/Fundos/FundoRegras.bmp"

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

carregarImgBase :: IO Picture
carregarImgBase = loadBMP "assets/Base/Torre/Base.bmp"

carregarImgPortal :: IO Picture
carregarImgPortal = loadBMP "assets/Portal/Portal.bmp"

carregarImgFlora :: IO [Picture]
carregarImgFlora = do
    imgEsq <- loadBMP "assets/Personagens/FloraEsquerda.bmp" 
    imgDir  <- loadBMP "assets/Personagens/FloraDireita.bmp"
    imgN    <- loadBMP "assets/Personagens/FloraTras.bmp"
    imgS      <- loadBMP "assets/Personagens/FloraFrente.bmp"
    return [imgEsq, imgDir, imgN, imgS]
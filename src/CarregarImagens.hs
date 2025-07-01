-- Este módulo contêm todas as funções para carregar imagens
import Graphics.Gloss


carregarImgMenu :: IO Picture
carregarImgMenu = loadBMP "imagens/fundos/MenuPrincipal.bmp"

carregarImgFundoNivel :: IO Picture
carregarImgFundoNivel = loadBMP "imagens/fundos/JanelaNivel.bmp"

carregarImgBotaoJogar :: IO Picture
carregarImgBotaoJogar = loadBMP "imagens/botoes/BotaoJogar.bmp"

carregarImgFundoRegras :: IO Picture
carregarImgFundoRegras = loadBMP "imagens/fundos/FundoRegras.bmp"

carregarImgBotaoVoltar :: IO Picture
carregarImgBotaoVoltar = loadBMP "imagens/botoes/BotaoVoltar.bmp"

carregarImgNivel1 :: IO Picture
carregarImgNivel1 = loadBMP "imagens/botoes/Nivel1.bmp"

carregarImgNivel2 :: IO Picture
carregarImgNivel2 = loadBMP "imagens/botoes/Nivel2.bmp"

carregarImgNivel3 :: IO Picture
carregarImgNivel3 = loadBMP "imagens/botoes/Nivel3.bmp"

carregarImgBotaoRegras :: IO Picture
carregarImgBotaoRegras = loadBMP "imagens/botoes/BotaoRegras.bmp"

carregarImgFlora :: IO [Picture]
carregarImgFlora = do
    imgEsq <- loadBMP "assets/Personagens/FloraEsquerda.bmp" 
    imgDir  <- loadBMP "assets/Personagens/FloraDireita.bmp"
    imgN    <- loadBMP "assets/Personagens/FloraTras.bmp"
    imgS      <- loadBMP "assets/Personagens/FloraFrente.bmp"
    return [imgEsq, imgDir, imgN, imgS]
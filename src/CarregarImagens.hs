module CarregarImagens where
import Graphics.Gloss


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

carregarImgsFlora :: IO [Picture]
carregarImgsFlora = do
    imgEsq <- loadBMP "assets/Personagens/Flora/FloraEsquerda.bmp" 
    imgDir  <- loadBMP "assets/Personagens/Flora/FloraDireita.bmp"
    imgN    <- loadBMP "assets/Personagens/Flora/FloraTras.bmp"
    imgS      <- loadBMP "assets/Personagens/Flora/FloraFrente.bmp"
    return [imgEsq, imgDir, imgN, imgS]

carregarImgsStella :: IO [Picture]
carregarImgsStella = do
    imgEsq <- loadBMP "assets/Personagens/Stella/StellaEsquerda.bmp" 
    imgDir  <- loadBMP "assets/Personagens/Stella/StellaDireita.bmp"
    imgN    <- loadBMP "assets/Personagens/Stella/StellaTras.bmp"
    imgS      <- loadBMP "assets/Personagens/Stella/StellaFrente.bmp"
    return [imgEsq, imgDir, imgN, imgS]
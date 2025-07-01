import Graphics.Gloss



main :: IO ()
main = do

  imgMenu <- carregarImgMenu
  imgEscolhaNivel <- carregarImgFundoNivel
  imgFundoRegras <- carregarImgFundoRegras
  imgBotaoJogar <- carregarImgBotaoJogar
  imgBotaoRegras <- carregarImgBotaoRegras
  imgBotaoVoltar <- carregarImgBotaoVoltar
  imgBotaoNivel1 <- carregarImgNivel1
  imgBotaoNivel2 <- carregarImgNivel2
  imgBotaoNivel3 <- carregarImgNivel3


  play (InWindow "ImmutableTowers" (1550, 900) (300, 70)) 
       white
       60
       estadoInicial
       render
       handleInput
       update














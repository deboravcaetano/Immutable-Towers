module Main where
import Graphics.Gloss.Interface.Pure.Game
import CarregarImagens
import Menu
import Type


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

  relva <- carregarImgRelva
  terra <- carregarImgTerra
  agua  <- carregarImgAgua

  imgBase <- carregarImgBase
  imgPortal <- carregarImgPortal
    

  let estadoInicial = EstadoJanela
        { estadoJanela = Menu
        , imagemJanelaPrincipal = imgMenu
        , imagemJanelaEscolhaNivel = imgEscolhaNivel
        , imagemFundoRegras = imgFundoRegras
        , imagemBotaoJogar = imgBotaoJogar
        , imagemBotaoRegras = imgBotaoRegras
        , imagemBotaoVoltar = imgBotaoVoltar
        , imagemBotaoNivel1 = imgBotaoNivel1
        , imagemBotaoNivel2 = imgBotaoNivel2
        , imagemBotaoNivel3 = imgBotaoNivel3
        , imagemRelva = relva    
        , imagemTerra = terra    
        , imagemAgua = agua  
        , imagemBase = imgBase
        , imagemPortal = imgPortal
        , jogoatual = Menu
        }


  let atualizar _ = id

  play (InWindow "ImmutableTowers" (1550, 900) (300, 70)) 
       white
       60
       estadoInicial
       render
       handleInput
       atualizar 















module Main where
import Graphics.Gloss.Interface.Pure.Game
import CarregarImagens
import Menu
import Type
import Game
import Atualizar
import CarregarImagens (carregarImgAguaTerra)



main :: IO ()
main = do

  imgMenu <- carregarImgMenu
  imgEscolhaNivel <- carregarImgFundoNivel
  imgFundoGoal <- carregarImgGoal
  imgFireTowerInfo <- carregarImgFirePage
  imgIceTowerInfo <- carregarImgIcePage
  imgResinTowerInfo <- carregarImgResinPage
  imgBotaoJogar <- carregarImgBotaoJogar
  imgBotaoRegras <- carregarImgBotaoRegras
  imgBotaoVoltar <- carregarImgBotaoVoltar
  imgBotaoNivel1 <- carregarImgNivel1
  imgBotaoNivel2 <- carregarImgNivel2
  imgBotaoNivel3 <- carregarImgNivel3

  relva <- carregarImgRelva
  terra <- carregarImgTerra
  agua  <- carregarImgAgua
  aguaTerra <- carregarImgAguaTerra

  imgBase <- carregarImgBase
  imgPortal <- carregarImgPortal
    
  imgAbaAberta <- carregarImgAbaAberta
  imgAbaFechada <- carregarImgAbaFechada

  imgFundoMapa <- carregarImgFundoMapa

  imgLoja <- carregarImgLoja

  imgBotaoFogo <- carregarImgBotaoTorreFogo
  imgBotaoGelo <- carregarImgBotaoTorreGelo
  imgBotaoResina <- carregarImgBotaoTorreResina

  imgTorreFogo <- carregarImgTorreFogo
  imgTorreGelo <- carregarImgTorreGelo
  imgTorreResina <- carregarImgTorreResina

  imgsFlora <- carregarImgsFlora
  imgsStella <- carregarImgsStella

  let estadoInicial = EstadoJanela
        { estadoJanela = Menu
        , imagemJanelaPrincipal = imgMenu
        , imagemJanelaEscolhaNivel = imgEscolhaNivel
        , imagemFundoGoal = imgFundoGoal
        , imagemFundoFogo = imgFireTowerInfo
        , imagemFundoGelo = imgIceTowerInfo
        , imagemFundoResina = imgResinTowerInfo
        , imagemFundoMapa = imgFundoMapa
        , imagemBotaoJogar = imgBotaoJogar
        , imagemBotaoRegras = imgBotaoRegras
        , imagemBotaoVoltar = imgBotaoVoltar
        , imagemBotaoNivel1 = imgBotaoNivel1
        , imagemBotaoNivel2 = imgBotaoNivel2
        , imagemBotaoNivel3 = imgBotaoNivel3
        , imagemBotao1 = imgAbaAberta
        , imagemBotao2 = imgAbaFechada
        , imagemBotao3 = imgAbaFechada
        , imagemBotao4 = imgAbaFechada
        , imagemBotao5 = imgAbaFechada
        , imagemRelva = relva    
        , imagemTerra = terra    
        , imagemAgua = agua  
        , imagemAguaTerra = aguaTerra
        , imagemBase = imgBase
        , imagemPortal = imgPortal
        , imagemLoja = imgLoja
        , imagemBotaoFogo = imgBotaoFogo
        , imagemBotaoGelo = imgBotaoGelo
        , imagemBotaoResina = imgBotaoResina
        , relvaSelecionada = Nothing
        , jogoatual = jogoInicial
        , imagemTorreFogo = imgTorreFogo
        , imagemTorreGelo = imgTorreGelo 
        , imagemTorreResina = imgTorreResina
        , imagensFlora = imgsFlora
        , imagensStella = imgsStella
        }

  let atualizar :: Float -> EstadoJanela -> EstadoJanela
      atualizar delta estado = 
          case estadoJanela estado of
              Game -> 
                  let jogoAntigo = jogoatual estado
                      novoJogo = atualizarJogo delta jogoAntigo estado
                  in estado { jogoatual = novoJogo }
              _ -> estado

  play (InWindow "ImmutableTowers" (1600, 900) (300, 70)) 
       white
       60
       estadoInicial
       render
       handleInput
       atualizar 















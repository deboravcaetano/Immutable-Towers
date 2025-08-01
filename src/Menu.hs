module Menu where
import Graphics.Gloss.Interface.Pure.Game
import Type
import Desenhar
import Game
import EventosLoja

render :: EstadoJanela -> Picture
render estado = desenhar estado


handleInput :: Event -> EstadoJanela -> EstadoJanela
handleInput (EventKey (MouseButton LeftButton) Down _ (x, y)) estadoGeral =
  case estadoJanela estadoGeral of -- acede o campo estadoJanela de estadoGeral :: EstadoJanela
    Menu ->
      if estaDentro (x, y) (-25, -180) dimensaoBotaoJogar then
        estadoGeral { estadoJanela = EscolhaNivel }
      else if estaDentro (x, y) (750, 383) dimensaoBotaoRegras then
        estadoGeral { estadoJanela = TorreResinaInfo }
      else
        estadoGeral
    EscolhaNivel ->
      if estaDentro (x, y) (10, -210) (201.6,21.6) then
        estadoGeral { estadoJanela = Menu }
      else if estaDentro (x, y) (10, 0) dimensaoBotaoNivel1 then
        estadoGeral { estadoJanela = Game, jogoatual = iniciarJogo 1 }
      else if estaDentro (x, y) (10, -30) dimensaoBotaoNivel2 then
        estadoGeral { estadoJanela = Menu }
      else if estaDentro (x, y) (10, -70) dimensaoBotaoNivel3 then
        estadoGeral { estadoJanela = Menu }
      else
        estadoGeral
    Goal ->
      if estaDentro (x, y) (0,-410) dimensaoBotaoVoltar then
        estadoGeral { estadoJanela = Menu }
      else if estaDentro (x, y) (-450,-20) dimensaoBotaoSetaEsq then
        estadoGeral { estadoJanela = TorreGeloInfo }
      else 
        estadoGeral
    TorreFogoInfo ->
      if estaDentro (x, y) (0,-410) dimensaoBotaoVoltar then
        estadoGeral { estadoJanela = Menu }
      else if estaDentro (x, y) (-450,-20) dimensaoBotaoSetaEsq then
        estadoGeral { estadoJanela = TorreResinaInfo }
      else if estaDentro (x, y) (488, -20) dimensaoBotaoSetaDir then
        estadoGeral { estadoJanela = TorreGeloInfo } 
      else 
        estadoGeral
    TorreGeloInfo ->
      if estaDentro (x, y)  (0,-410) dimensaoBotaoVoltar then
        estadoGeral { estadoJanela = Menu }
      else if estaDentro (x, y) (-450,-20) dimensaoBotaoSetaEsq then
        estadoGeral { estadoJanela = TorreFogoInfo }
      else if estaDentro (x, y) (488, -20) dimensaoBotaoSetaDir then
        estadoGeral { estadoJanela = Goal } 
      else 
        estadoGeral
    TorreResinaInfo ->
      if estaDentro (x, y) (0,-410) dimensaoBotaoVoltar then
        estadoGeral { estadoJanela = Menu }
      else if estaDentro (x, y) (488,-20) dimensaoBotaoSetaDir then
        estadoGeral { estadoJanela = TorreFogoInfo }
      else 
        estadoGeral
    Game -> 
      if estaDentro (x,y) (0,415) dimensaoBotaoPausa then
        estadoGeral { estadoJanela = Pausa}
      else 
      case relvaSelecionada estadoGeral of
        Just posRelva -> 
            if estaDentro (x, y) (535 ,150) dimensaoBotaoTorre then
                (adicionarTorre estadoGeral posRelva Gelo) 
                    { relvaSelecionada = Nothing }
            else if estaDentro (x, y) (535, 10) dimensaoBotaoTorre then
                (adicionarTorre estadoGeral posRelva Resina) 
                    { relvaSelecionada = Nothing }
            else if estaDentro (x, y) (535 ,-130) dimensaoBotaoTorre then
                (adicionarTorre estadoGeral posRelva Fogo) 
                    { relvaSelecionada = Nothing }
            else estadoGeral
            
        Nothing ->
            if clicouRelva (x, y) estadoGeral then
                estadoGeral { relvaSelecionada = Just (x, y) }
            else estadoGeral
    Perdeu ->
      if estaDentro (x, y) (-101,-133) dimensaoBotaoMenu then
        estadoGeral { estadoJanela = Menu }
      else if estaDentro (x, y) (97,-133)  dimensaoBotaoFindIt then
        estadoGeral { estadoJanela = EscolhaNivel } -- ou recomeçar logo o jogo
      else 
        estadoGeral
    Pausa -> 
      if estaDentro (x,y) (-125,-80) dimensaoBotaoVoltarAJogar then
        estadoGeral { estadoJanela = Game}
      else if estaDentro (x,y) (120,-80) dimensaoBotaoSair then
        estadoGeral { estadoJanela = EscolhaNivel}
      else estadoGeral

handleInput _ estadoGeral = estadoGeral


estaDentro :: (Float, Float) -> (Float, Float) -> (Float, Float) -> Bool
estaDentro (x, y) (bx, by) (width, height) =
  abs (x - bx) < (width / 2) && abs (y - by) < (height / 2)
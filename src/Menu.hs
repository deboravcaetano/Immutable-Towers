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
        estadoGeral { estadoJanela = Goal }
      else
        estadoGeral
    EscolhaNivel ->
      if estaDentro (x, y) (-740, -390) dimensaoBotaoVoltar then
        estadoGeral { estadoJanela = Menu }
      else if estaDentro (x, y) (-350, 0) dimensaoBotaoNivel1 then
        estadoGeral { estadoJanela = Game, jogoatual = iniciarJogo 1 }
      else if estaDentro (x, y) (0, 0) dimensaoBotaoNivel2 then
        estadoGeral { estadoJanela = Menu }
      else if estaDentro (x, y) (350, 0) dimensaoBotaoNivel3 then
        estadoGeral { estadoJanela = Menu }
      else
        estadoGeral
    Goal ->
      if estaDentro (x, y) (-740,-390) dimensaoBotaoVoltar then
        estadoGeral { estadoJanela = Menu }
      else if estaDentro (x, y) (-220, 290) dimensaoBotaoAbaFechada then
        estadoGeral { estadoJanela = TorreFogoInfo }
      else if estaDentro (x, y) (-20, 290) dimensaoBotaoAbaFechada then
        estadoGeral { estadoJanela = TorreGeloInfo } 
      else if estaDentro (x, y) (180, 290) dimensaoBotaoAbaFechada then
        estadoGeral { estadoJanela = TorreResinaInfo }
      else 
        estadoGeral
    TorreFogoInfo ->
      if estaDentro (x, y) (-740,-390) dimensaoBotaoVoltar then
        estadoGeral { estadoJanela = Menu }
      else if estaDentro (x, y) (-420,290) dimensaoBotaoAbaFechada then
        estadoGeral { estadoJanela = Goal }
      else if estaDentro (x, y) (-20, 290) dimensaoBotaoAbaFechada then
        estadoGeral { estadoJanela = TorreGeloInfo } 
      else if estaDentro (x, y) (180, 290) dimensaoBotaoAbaFechada then
        estadoGeral { estadoJanela = TorreResinaInfo }
      else 
        estadoGeral
    TorreGeloInfo ->
      if estaDentro (x, y) (-740,-390) dimensaoBotaoVoltar then
        estadoGeral { estadoJanela = Menu }
      else if estaDentro (x, y) (-420,290) dimensaoBotaoAbaFechada then
        estadoGeral { estadoJanela = Goal }
      else if estaDentro (x, y) (-220, 290) dimensaoBotaoAbaFechada then
        estadoGeral { estadoJanela = TorreFogoInfo } 
      else if estaDentro (x, y) (180, 290) dimensaoBotaoAbaFechada then
        estadoGeral { estadoJanela = TorreResinaInfo }
      else 
        estadoGeral
    TorreResinaInfo ->
      if estaDentro (x, y) (-740,-390) dimensaoBotaoVoltar then
        estadoGeral { estadoJanela = Menu }
      else if estaDentro (x, y) (-420,300) dimensaoBotaoAbaFechada then
        estadoGeral { estadoJanela = Goal }
      else if estaDentro (x, y) (-220, 290) dimensaoBotaoAbaFechada then
        estadoGeral { estadoJanela = TorreFogoInfo } 
      else if estaDentro (x, y) (20, 290) dimensaoBotaoAbaFechada then
        estadoGeral { estadoJanela = TorreGeloInfo }
      else 
        estadoGeral
    Game -> 
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
handleInput _ estadoGeral = estadoGeral


estaDentro :: (Float, Float) -> (Float, Float) -> (Float, Float) -> Bool
estaDentro (x, y) (bx, by) (width, height) =
  abs (x - bx) < (width / 2) && abs (y - by) < (height / 2)
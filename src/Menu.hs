module Menu where
import Graphics.Gloss.Interface.Pure.Game
import Type
import Desenhar
import Game

render :: EstadoJanela -> Picture
render estado = desenhar estado


handleInput :: Event -> EstadoJanela -> EstadoJanela
handleInput (EventKey (MouseButton LeftButton) Down _ (x, y)) jogo =
  case estadoJanela jogo of
    Menu ->
      if estaDentro (x, y) (-25, -165) dimensaoBotaoJogar then
        jogo { estadoJanela = EscolhaNivel }
      else if estaDentro (x, y) (710, 370) dimensaoBotaoRegras then
        jogo { estadoJanela = Goal }
      else
        jogo
    EscolhaNivel ->
      if estaDentro (x, y) (-740, -390) dimensaoBotaoVoltar then
        jogo { estadoJanela = Menu }
      else if estaDentro (x, y) (-350, 0) dimensaoBotaoNivel1 then
        jogo { estadoJanela = Game (iniciarJogo 1) }
      else if estaDentro (x, y) (0, 0) dimensaoBotaoNivel2 then
        jogo { estadoJanela = Menu }
      else if estaDentro (x, y) (350, 0) dimensaoBotaoNivel3 then
        jogo { estadoJanela = Menu }
      else
        jogo
    Goal ->
      if estaDentro (x, y) (-740,-390) dimensaoBotaoVoltar then
        jogo { estadoJanela = Menu }
      else if estaDentro (x, y) (-220, 290) dimensaoBotaoAbaFechada then
        jogo { estadoJanela = TorreFogoInfo }
      else if estaDentro (x, y) (-20, 290) dimensaoBotaoAbaFechada then
        jogo { estadoJanela = TorreGeloInfo } 
      else if estaDentro (x, y) (180, 290) dimensaoBotaoAbaFechada then
        jogo { estadoJanela = TorreResinaInfo }
      else 
        jogo
    TorreFogoInfo ->
      if estaDentro (x, y) (-740,-390) dimensaoBotaoVoltar then
        jogo { estadoJanela = Menu }
      else if estaDentro (x, y) (-420,290) dimensaoBotaoAbaFechada then
        jogo { estadoJanela = Goal }
      else if estaDentro (x, y) (-20, 290) dimensaoBotaoAbaFechada then
        jogo { estadoJanela = TorreGeloInfo } 
      else if estaDentro (x, y) (180, 290) dimensaoBotaoAbaFechada then
        jogo { estadoJanela = TorreResinaInfo }
      else 
        jogo
    TorreGeloInfo ->
      if estaDentro (x, y) (-740,-390) dimensaoBotaoVoltar then
        jogo { estadoJanela = Menu }
      else if estaDentro (x, y) (-420,290) dimensaoBotaoAbaFechada then
        jogo { estadoJanela = Goal }
      else if estaDentro (x, y) (-220, 290) dimensaoBotaoAbaFechada then
        jogo { estadoJanela = TorreFogoInfo } 
      else if estaDentro (x, y) (180, 290) dimensaoBotaoAbaFechada then
        jogo { estadoJanela = TorreResinaInfo }
      else 
        jogo
    TorreResinaInfo ->
      if estaDentro (x, y) (-740,-390) dimensaoBotaoVoltar then
        jogo { estadoJanela = Menu }
      else if estaDentro (x, y) (-420,300) dimensaoBotaoAbaFechada then
        jogo { estadoJanela = Goal }
      else if estaDentro (x, y) (-220, 290) dimensaoBotaoAbaFechada then
        jogo { estadoJanela = TorreFogoInfo } 
      else if estaDentro (x, y) (20, 290) dimensaoBotaoAbaFechada then
        jogo { estadoJanela = TorreGeloInfo }
      else 
        jogo
    Game jogoI->
      if estaDentro (x, y) (526 ,130) dimensaoBotaoTorre then
        jogo { estadoJanela = Menu }
      else if estaDentro (x, y) (526 ,-10) dimensaoBotaoTorre then
        jogo { estadoJanela = TorreGeloInfo }
      else if estaDentro (x, y) (526 ,-150) dimensaoBotaoTorre then
        jogo { estadoJanela = Goal }
      else
        jogo
handleInput _ jogo = jogo


estaDentro :: (Float, Float) -> (Float, Float) -> (Float, Float) -> Bool
estaDentro (x, y) (bx, by) (width, height) =
  abs (x - bx) < (width / 2) && abs (y - by) < (height / 2)
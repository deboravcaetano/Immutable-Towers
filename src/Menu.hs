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
        jogo { estadoJanela = Regras }
      else
        jogo
    EscolhaNivel ->
      if estaDentro (x, y) (0, -350) dimensaoBotaoVoltar then
        jogo { estadoJanela = Menu }
      else if estaDentro (x, y) (-350, 0) dimensaoBotaoNivel1 then
        jogo { estadoJanela = Game (iniciarJogo 1) }
      else if estaDentro (x, y) (0, 0) dimensaoBotaoNivel2 then
        jogo { estadoJanela = Menu }
      else if estaDentro (x, y) (350, 0) dimensaoBotaoNivel3 then
        jogo { estadoJanela = Menu }
      else
        jogo
    Regras ->
      if estaDentro (x, y) (0, -410) dimensaoBotaoVoltar then
        jogo { estadoJanela = Menu }
      else
        jogo
    _ -> jogo

handleInput _ jogo = jogo


estaDentro :: (Float, Float) -> (Float, Float) -> (Float, Float) -> Bool
estaDentro (x, y) (bx, by) (width, height) =
  abs (x - bx) < (width / 2) && abs (y - by) < (height / 2)
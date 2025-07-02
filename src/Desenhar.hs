module Desenhar where
import Type 
import Graphics.Gloss 
import Type (EstadoJanela, Portal (posicaoPortal), Jogo (portaisJogo))
import Graphics.Gloss (Picture)



desenhar :: EstadoJanela -> Picture
desenhar estado = case estadoJanela estado of
    Menu -> desenhaMenu estado
    EscolhaNivel -> desenhaEscolhaNivel estado
    Regras -> desenhaRegras estado
    Game jogo -> desenhaJogo estado jogo


desenhaMenu :: EstadoJanela -> Picture
desenhaMenu estado = pictures [
    imagemJanelaPrincipal estado,
    translate (-25) (-165) (imagemBotaoJogar estado),
    translate 710 370 (imagemBotaoRegras estado)
    ]

desenhaRegras :: EstadoJanela -> Picture
desenhaRegras estado = pictures [
    imagemFundoRegras estado,
    translate 0 (-410) (imagemBotaoVoltar estado)
    ]

desenhaEscolhaNivel :: EstadoJanela -> Picture
desenhaEscolhaNivel estado = pictures [
    imagemJanelaEscolhaNivel estado,
    translate (-350) 0 (imagemBotaoNivel1 estado),
    translate 0 0 (imagemBotaoNivel2 estado),
    translate 350 0 (imagemBotaoNivel3 estado),
    translate 0 (-350) (imagemBotaoVoltar estado)
    ]

desenhaJogo :: EstadoJanela -> Jogo -> Picture
desenhaJogo estado jogo = pictures
    [ desenhaMapa (mapaJogo jogo) [imagemRelva estado, imagemTerra estado, imagemAgua estado]
    , desenhaBase (baseJogo jogo) (imagemBase estado)
    , desenhaPortal (portaisJogo jogo) (imagemPortal estado)
    ]


desenhaMapa :: Mapa -> [Picture] -> Picture 
desenhaMapa mapa imagens = 
    Translate offsetX offsetY $ 
    Pictures [desenhaTile (fromIntegral x, fromIntegral y) terreno imagens | 
              (y, linha) <- zip [0..] (reverse mapa), 
              (x, terreno) <- zip [0..] linha]
  where
    numLinhas = fromIntegral $ length mapa
    numColunas = fromIntegral $ length (head mapa)
    

    offsetX = - (numColunas * 50) / 2
    offsetY = - (numLinhas * 50) / 2


desenhaTile :: (Float, Float) -> Terreno -> [Picture] -> Picture
desenhaTile (x, y) terreno [relva, terra, agua] = 
    Translate (x * 50) (y * 50) img
  where img = case terreno of
                 Relva -> relva
                 Terra -> terra
                 Agua  -> agua

desenhaBase :: Base -> Picture -> Picture
desenhaBase base imgBase = 
    translate x y imgBase
  where
    (x, y) = posicaoBase base

desenhaPortal :: Portal -> Picture -> Picture
desenhaPortal portal imgPortal = 
    translate x y imgPortal
  where
    (x, y) = posicaoPortal portal
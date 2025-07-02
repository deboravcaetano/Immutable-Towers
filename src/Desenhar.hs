module Desenhar where
import Type 
import Graphics.Gloss 



desenhar :: EstadoJanela -> Picture
desenhar estado = case estadoJanela estado of
    Menu -> desenhaMenu estado
    EscolhaNivel -> desenhaEscolhaNivel estado
    Regras -> desenhaRegras estado
    -- Game -> desenharJogo


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

desenhaMapa :: Mapa -> [Picture] -> Picture 
desenhaMapa mapa imagens = 
    Translate (-625) (-425) $ Pictures [desenhaTile (fromIntegral x, fromIntegral y) terreno imagens | 
                                        (y, linha) <- zip [0..] (reverse mapa), 
                                        (x, terreno) <- zip [0..] linha]


desenhaTile :: (Float, Float) -> Terreno -> [Picture] -> Picture
desenhaTile (x, y) terreno [relva, terra, agua] = 
    Translate (x * 50) (y * 50) img
  where img = case terreno of
                 Relva -> relva
                 Terra -> terra
                 Agua  -> agua
module Desenhar where
import Type 
import Graphics.Gloss 
import Type (EstadoJanela, Portal (posicaoPortal), Jogo (portaisJogo))
import Graphics.Gloss (Picture)



desenhar :: EstadoJanela -> Picture
desenhar estado = case estadoJanela estado of
    Menu -> desenhaMenu estado
    EscolhaNivel -> desenhaEscolhaNivel estado
    Goal -> desenhaGoal estado
    TorreFogoInfo -> desenhaFogoInfo estado
    TorreGeloInfo -> desenhaGeloInfo estado
    TorreResinaInfo -> desenhaResinaInfo estado
    Game jogo -> desenhaJogo estado jogo


desenhaMenu :: EstadoJanela -> Picture
desenhaMenu estado = pictures [
    imagemJanelaPrincipal estado,
    translate (-25) (-165) (imagemBotaoJogar estado),
    translate 710 370 (imagemBotaoRegras estado)
    ]

desenhaGoal :: EstadoJanela -> Picture
desenhaGoal estado = pictures [
    imagemFundoGoal estado,
    translate (-740) (-390) (imagemBotaoVoltar estado),
    translate (-420) 300 (imagemBotao1 estado),
    translate (-220) 290 (imagemBotao2 estado),
    translate (-20) 290 (imagemBotao3 estado),
    translate 180 290 (imagemBotao4 estado),
    translate 380 290 (imagemBotao5 estado)
    ]

desenhaFogoInfo :: EstadoJanela -> Picture
desenhaFogoInfo estado = pictures [
    imagemFundoFogo estado,
    translate (-740) (-390) (imagemBotaoVoltar estado),
    translate (-420) 290 (imagemBotao2 estado),
    translate (-220) 300 (imagemBotao1 estado),
    translate (-20) 290 (imagemBotao3 estado),
    translate 180 290 (imagemBotao4 estado),
    translate 380 290 (imagemBotao5 estado)
    ]

desenhaGeloInfo :: EstadoJanela -> Picture
desenhaGeloInfo estado = pictures [
    imagemFundoGelo estado,
    translate (-740) (-390) (imagemBotaoVoltar estado),
    translate (-420) 290 (imagemBotao2 estado),
    translate (-220) 290 (imagemBotao3 estado),
    translate (-20) 300 (imagemBotao1 estado),
    translate 180 290 (imagemBotao4 estado),
    translate 380 290 (imagemBotao5 estado)
    ]

desenhaResinaInfo :: EstadoJanela -> Picture
desenhaResinaInfo estado = pictures [
    imagemFundoResina estado,
    translate (-740) (-390) (imagemBotaoVoltar estado),
    translate (-420) 290 (imagemBotao2 estado),
    translate (-220) 290 (imagemBotao3 estado),
    translate (-20) 290 (imagemBotao4 estado),
    translate 180 300 (imagemBotao1 estado),
    translate 380 290 (imagemBotao5 estado)
    ]

desenhaEscolhaNivel :: EstadoJanela -> Picture
desenhaEscolhaNivel estado = pictures [
    imagemJanelaEscolhaNivel estado,
    translate (-350) 0 (imagemBotaoNivel1 estado),
    translate 0 0 (imagemBotaoNivel2 estado),
    translate 350 0 (imagemBotaoNivel3 estado),
    translate (-740) (-390) (imagemBotaoVoltar estado)
    ]

desenhaJogo :: EstadoJanela -> Jogo -> Picture
desenhaJogo estado jogo = pictures $
    [ desenhaFundoMapa (imagemFundoMapa estado) estado
    , desenhaMapa (mapaJogo jogo) [imagemRelva estado, imagemTerra estado, imagemAgua estado]
    , desenhaBase (baseJogo jogo) (imagemBase estado)
    , desenhaLoja (imagemLoja estado) (imagemBotaoGelo estado) (imagemBotaoResina estado) (imagemBotaoFogo estado) estado
    ]
    ++ map (\p -> desenhaPortal p (imagemPortal estado)) (portaisJogo jogo)

desenhaFundoMapa :: Picture -> EstadoJanela -> Picture
desenhaFundoMapa imgFundo _estado =
    Translate 0 0 imgFundo

desenhaLoja :: Picture -> Picture -> Picture -> Picture -> EstadoJanela -> Picture
desenhaLoja imgLoja imgBotaoGelo imgBotaoResina imgBotaoFogo _estado = pictures [
    translate 80 0 imgLoja,
    translate 526 130 imgBotaoGelo,
    translate 526 (-10) imgBotaoResina,
    translate 526 (-150) imgBotaoFogo
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

    tamTile = 50
    
    offsetX = - (((numColunas * tamTile) / 2 ) - (tamTile / 2))
    offsetY = - (((numLinhas * tamTile) / 2 ) - (tamTile / 2))


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
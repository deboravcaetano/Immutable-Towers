module Desenhar where
import Type 
import Graphics.Gloss 
import Type (EstadoJanela(imagemAgua), Base (creditosBase))
import Graphics.Gloss (Picture)
import Imagens
import Data.Maybe (isJust)
import EventosLoja



desenhar :: EstadoJanela -> Picture
desenhar estado = case estadoJanela estado of
    Menu -> desenhaMenu estado
    EscolhaNivel -> desenhaEscolhaNivel estado
    Goal -> desenhaGoal estado
    TorreFogoInfo -> desenhaFogoInfo estado
    TorreGeloInfo -> desenhaGeloInfo estado
    TorreResinaInfo -> desenhaResinaInfo estado
    Game -> desenhaJogo estado (jogoatual estado)  
    Perdeu -> desenhaPerdeuJanela estado
    Ganhou -> desenhaGanhouJanela estado
    Pausa -> desenhaPausa estado


desenhaMenu :: EstadoJanela -> Picture
desenhaMenu estado = pictures [
    imagemJanelaPrincipal estado,
    translate (-25) (-180) (imagemBotaoJogar estado),
    translate 750 383 (imagemBotaoRegras estado)
    ]

desenhaGoal :: EstadoJanela -> Picture
desenhaGoal estado = pictures [
    imagemFundoGoal estado,
    translate 0 (-410) (imagemBotaoVoltar estado),
    translate (-450) (-20) (imagemBotao1 estado)
    ]

desenhaFogoInfo :: EstadoJanela -> Picture
desenhaFogoInfo estado = pictures [
    imagemFundoFogo estado,
    translate 0 (-410) (imagemBotaoVoltar estado),
    translate (-450) (-20) (imagemBotao1 estado),
    translate 455 (-20) (imagemBotao2 estado)
    ]

desenhaGeloInfo :: EstadoJanela -> Picture
desenhaGeloInfo estado = pictures [
    imagemFundoGelo estado,
    translate 0 (-410) (imagemBotaoVoltar estado),
    translate (-450) (-20) (imagemBotao1 estado),
    translate 455 (-20) (imagemBotao2 estado)
    ]

desenhaResinaInfo :: EstadoJanela -> Picture
desenhaResinaInfo estado = pictures [
    imagemFundoResina estado,
    translate 0 (-410) (imagemBotaoVoltar estado),
    translate 455 (-20) (imagemBotao2 estado)
    ]

desenhaEscolhaNivel :: EstadoJanela -> Picture
desenhaEscolhaNivel estado = pictures [
    imagemJanelaEscolhaNivel estado,
    translate 10 0 (imagemBotaoNivel1 estado),
    translate 10 (-35) (imagemBotaoNivel2 estado),
    translate 10 (-70) (imagemBotaoNivel3 estado),
    translate 10 (-210) $ scale 0.6 0.6  (imagemBotaoVoltar estado)
    ]

desenhaPerdeuJanela :: EstadoJanela -> Picture
desenhaPerdeuJanela estado = pictures [
    imagemJanelaPerdeu estado,
    translate (-101) (-133) (imagemBotaoMenu estado),
    translate 97 (-133) (imagemBotaoFindIt estado)
    ]

desenhaGanhouJanela :: EstadoJanela -> Picture
desenhaGanhouJanela estado = pictures [
    imagemJanelaGanhou estado
    ]

desenhaPausa :: EstadoJanela -> Picture
desenhaPausa estado = pictures [
    desenhaJogo estado (jogoatual estado), 
    color (makeColor 0 0 0 0.9) $ rectangleSolid 1600 9000,
    translate 0 0 (imagemPausa estado)
    ]

desenhaJogo :: EstadoJanela -> Jogo -> Picture
desenhaJogo estado jogo = pictures $
    [ desenhaFundoMapa (imagemFundoMapa estado) estado
    , desenhaMapa (mapaJogo jogo) [imagemRelva estado, imagemTerra estado, imagemAgua estado, imagemAguaTerra estado]
    , desenhaBase (baseJogo jogo) (imagemBase estado)
    , desenhaLoja (imagemLoja estado) (imagemBotaoGelo estado) (imagemBotaoResina estado) (imagemBotaoFogo estado) (imagemBotaoVender estado) estado
    , Translate (-70) (-413) $ Scale 0.2 0.2 $ Text (show $ vidaBase (baseJogo jogo))
    , Translate 70 (-413) $ Scale 0.2 0.2 $ Text (show $ creditosBase (baseJogo jogo))
    , translate 0 415 (imagemBotaoPausa estado)
    ]
    ++ map (\p -> desenhaPortal p (imagemPortal estado)) (portaisJogo jogo)
    ++ [desenhaInimigos (inimigosJogo jogo)  estado]
    ++ map (\t -> desenhaTorre t (escolherImagemTorre estado t)) (torresJogo jogo)
    ++ [desenhaTorreSelecionada estado | isJust (torreSelecionada estado)]
    ++ [desenhaRelvaSelecionada estado | isJust (relvaSelecionada estado)]
  where
    escolherImagemTorre :: EstadoJanela -> Torre -> Picture
    escolherImagemTorre estado torre = 
        case tipoProjetil (projetilTorre torre) of
            Fogo    -> imagemTorreFogo estado    
            Gelo    -> imagemTorreGelo estado
            Resina  -> imagemTorreResina estado


desenhaTorreSelecionada :: EstadoJanela -> Picture
desenhaTorreSelecionada estado =
    case torreSelecionada estado of
        Just torre -> 
            let (x, y) = posicaoTorre torre
            in translate x y $ color (makeColor 0 0 0 0.3) $ rectangleSolid 50 76
        Nothing -> blank

desenhaRelvaSelecionada :: EstadoJanela -> Picture
desenhaRelvaSelecionada estado =
    case relvaSelecionada estado of
        Just (x, y) -> 
            let jogo = jogoatual estado
                tile = 50
                metadeLargura = (fromIntegral (nColunas jogo) * tile) / 2
                metadeAltura = (fromIntegral (nLinhas jogo) * tile) / 2
                
                -- Converte coordenadas de clique para índice da matriz
                maybeIndice = pixelParaIndice (nColunas jogo) (nLinhas jogo) (x, y)
                
                -- Calcula posição central do tile
                posicaoCentral = case maybeIndice of
                    Just (r, c) -> 
                        let centX = fromIntegral c * tile - metadeLargura + tile / 2
                            centY = metadeAltura - fromIntegral r * tile - tile / 2
                        in Just (centX, centY)
                    Nothing -> Nothing
            in case posicaoCentral of
                Just (posX, posY) -> 
                    translate posX posY $ color blue $ rectangleWire tile tile
                Nothing -> blank
        Nothing -> blank

desenhaFundoMapa :: Picture -> EstadoJanela -> Picture
desenhaFundoMapa imgFundo _estado =
    Translate 0 0 imgFundo

desenhaLoja :: Picture -> Picture -> Picture -> Picture -> Picture -> EstadoJanela -> Picture
desenhaLoja imgLoja imgBotaoGelo imgBotaoResina imgBotaoFogo imgBotaoVender _estado = pictures [
    translate (-5) 0 imgLoja,
    translate 535 150 imgBotaoGelo,
    translate 535 10 imgBotaoResina,
    translate 535 (-130) imgBotaoFogo,
    translate 630 (-230) imgBotaoVender
    ]
    
desenhaMapa :: [[Terreno]] -> [Picture] -> Picture 
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
desenhaTile (x, y) terreno [relva, terra, agua, aguaTerra] = 
    Translate (x * 50) (y * 50) img
  where img = case terreno of
                 Relva -> relva
                 Terra -> terra
                 Agua  -> agua
                 AguaTerra -> aguaTerra

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

desenhaTorre :: Torre -> Picture -> Picture  
desenhaTorre torre imgTorre = 
    let (x, y) = posicaoTorre torre
    in translate x y imgTorre

desenhaInimigos :: [Inimigo] -> EstadoJanela -> Picture 
desenhaInimigos inimigos estado =
    if null inimigos then Blank
    else 
    Pictures [Translate x y (selecionaImagemPorInimigo estado inimigo) | inimigo <- inimigos, let (x,y) = posicaoInimigo inimigo]
 


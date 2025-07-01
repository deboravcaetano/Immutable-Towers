module Desenhar where
import Type 
import Graphics.Gloss (Picture)


desenhar :: ImmutableTowers -> Picture
desenhar it = case it of
    Menu -> desenhaMenu 
    Regras -> desenhaRegras
    EscolhaNivel -> desenhaEscolhaNivel
    -- Game -> desenharJogo


desenhaMenu :: Picture
desenhaMenu = pictures [
    imgMenu jogo,
    translate (-25) (-165) (imgBotaoJogar jogo),
    translate 710 370 (imgBotaoRegras jogo)
    ]

desenhaRegras :: Picture
desenhaRegras = pictures [
    imgFundoRegras jogo,
    translate 0 (-410) (imgBotaoVoltar jogo)
    ]

desenhaEscolhaNivel :: Picture
desenhaEscolhaNivel = pictures [
    imgEscolhaNivel jogo,
    translate (-350) 0 (imgBotaoNivel1 jogo),
    translate 0 0 (imgBotaoNivel2 jogo),
    translate 350 0 (imgBotaonivel3 jogo),
    translate 0 (-350) (imgBotaoVoltar jogo)
    ]

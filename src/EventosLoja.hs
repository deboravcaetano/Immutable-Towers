module EventosLoja where
import Type
import Ataque
import Data.List (find)


-- Objetivo: Transformar coordenadas do clique do rato em píxeis (x,y) num índice da matriz do tabuleiro Just (linha,coluna)
-- Exemplo: (125, -75)  -->  Just (2,3)

pixelParaIndice :: Int -> Int -> (Float,Float) -> Maybe (Int,Int) 
pixelParaIndice nCols nRows (x,y) =
  let tile = 50
      metadeLargura = (fromIntegral nCols * tile ) / 2
      metadeAltura = (fromIntegral nRows * tile) / 2

      -- Faz descolacamento para (0,0) ser no canto superior esquerdo  
      colF = (x + metadeLargura) / tile
      rowF = (metadeAltura - y) / tile      
      col  = floor colF -- Arredonda para inteiro
      row  = floor rowF
  in if col >= 0 && col < nCols && row >= 0 && row < nRows
        then Just (row,col)
        else Nothing -- Fora do tabuleiro


clicouRelva :: (Float,Float) -> EstadoJanela -> Bool
clicouRelva pxy estado =
  let jogo = jogoatual estado
  in case pixelParaIndice (nColunas jogo) (nLinhas jogo) pxy of
        Just (r,c) -> 
            let linha = mapaJogo jogo !! r
            in if c < length linha then linha !! c == Relva else False
        Nothing -> False


tileParaPosicaoCentral :: (Float, Float) -> (Int, Int) -> Maybe (Float, Float)
tileParaPosicaoCentral (x, y) (nCols, nRows) = do
    (r, c) <- pixelParaIndice nCols nRows (x, y)
    let tile = 50
        metadeLargura = (fromIntegral nCols * tile) / 2
        metadeAltura = (fromIntegral nRows * tile) / 2

        centX = fromIntegral c * tile - metadeLargura + tile / 2
        centY = metadeAltura - fromIntegral r * tile - tile/ 2

        posX = centX
        posY = centY + 13 -- Ajuste para não ficar parte no bloco abaixo
    return (posX, posY)


procurarCustoTorre :: TipoProjetil -> Loja -> Maybe Creditos
procurarCustoTorre tipo loja = 
    case filter (\(_, torre) -> tipoProjetil (projetilTorre torre) == tipo) loja of
        ((cost, _):_) -> Just cost
        _ -> Nothing

adicionarTorre :: EstadoJanela -> (Float, Float) -> TipoProjetil -> EstadoJanela
adicionarTorre estado posicao tipo =
    let jogo = jogoatual estado
        nCols = nColunas jogo
        nRows = nLinhas jogo

        posCentral = case tileParaPosicaoCentral posicao (nCols, nRows) of
            Just pos -> pos
            Nothing -> posicao

        custo = case procurarCustoTorre tipo (lojaJogo jogo) of
            Just cost -> cost
            Nothing -> 1000

        creditosAtuais = creditosBase (baseJogo jogo)

    in if creditosAtuais >= custo
        then
            let (dano, alcance, rajada, ciclo, duracao, reducao) = case tipo of
                    Fogo   -> (10, 70.8, 2, 1.5, Finita 2.0, 0.0)
                    Resina -> (10, 70.8, 2, 2.0, Finita 1.8, 0.3)
                    Gelo   -> (10, 70.8, 1, 3.0, Finita 1.0, 0.0)

                novaTorre = Torre {
                    posicaoTorre = posCentral,
                    projetilTorre = Projetil {
                        tipoProjetil = tipo,
                        duracaoProjetil = duracao,
                        reducaoVelocidade = reducao
                    },
                    tempoTorre = 0,
                    danoTorre = dano,
                    alcanceTorre = alcance,
                    rajadaTorre = rajada,
                    cicloTorre = ciclo
                }

                novasTorres = novaTorre : torresJogo jogo

                novaBase = (baseJogo jogo) { creditosBase = creditosAtuais - custo }

                novoJogo = jogo {
                    baseJogo = novaBase,
                    torresJogo = novasTorres
                }

            in estado { jogoatual = novoJogo }
        else estado


torreClicada :: EstadoJanela -> (Float, Float) -> Maybe Torre
torreClicada estado (x, y) = 
  find (\torre -> distancia (x,y) (posicaoTorre torre) < 25) (torresJogo $ jogoatual estado)


venderTorre :: EstadoJanela -> Torre -> EstadoJanela
venderTorre estado torre =
  let jogo = jogoatual estado
      custo = case procurarCustoTorre (tipoProjetil (projetilTorre torre)) (lojaJogo jogo) of
                Just cost -> cost
      reembolso = round (fromIntegral custo * 0.75)  -- 75% de reembolso
      
      -- Remover a torre da lista
      novasTorres = filter (\t -> posicaoTorre t /= posicaoTorre torre) (torresJogo jogo)
      
      -- Atualizar créditos
      base = baseJogo jogo
      novaBase = base { creditosBase = creditosBase base + reembolso }
      
      novoJogo = jogo {
          baseJogo = novaBase,
          torresJogo = novasTorres
      }
  in estado { 
      jogoatual = novoJogo,
      torreSelecionada = Nothing  -- deselecionar após vender
  }
module Type where
import Graphics.Gloss.Interface.Pure.Game
import System.Random 


data Janela = Menu | EscolhaNivel | Goal | TorreFogoInfo | TorreGeloInfo | TorreResinaInfo | Game  

data EstadoJanela = EstadoJanela
  { estadoJanela :: Janela              
  , imagemJanelaPrincipal :: Picture     
  , imagemJanelaEscolhaNivel :: Picture  
  , imagemFundoGoal :: Picture  
  , imagemFundoFogo :: Picture   
  , imagemFundoGelo :: Picture 
  , imagemFundoResina :: Picture 
  , imagemFundoMapa :: Picture     
  , imagemBotaoJogar :: Picture          
  , imagemBotaoRegras :: Picture        
  , imagemBotaoVoltar :: Picture        
  , imagemBotaoNivel1 :: Picture         
  , imagemBotaoNivel2 :: Picture        
  , imagemBotaoNivel3 :: Picture
  , imagemBotao1 :: Picture
  , imagemBotao2 :: Picture
  , imagemRelva :: Picture
  , imagemTerra :: Picture
  , imagemAgua :: Picture  
  , imagemAguaTerra 
  , imagemBase :: Picture 
  , imagemPortal :: Picture 
  , imagemLoja :: Picture
  , imagemBotaoFogo :: Picture
  , imagemBotaoGelo :: Picture
  , imagemBotaoResina :: Picture  
  , jogoatual :: Jogo 
  , relvaSelecionada :: Maybe (Float, Float) 
  , imagemTorreFogo :: Picture
  , imagemTorreGelo:: Picture
  , imagemTorreResina :: Picture 
  , imagensFlora :: [[Picture]]
  , imagensStella :: [[Picture]]
  --, imagensInimigo3 :: [Picture]
  --, imagensInimigo4 :: [Picture]
  --, imagensInimigo5 :: [Picture]
  --, imagensInimigo6 :: [Picture] 
  }

data Portal = Portal {
    posicaoPortal :: Posicao,
    ondasPortal :: [Onda],
    ativo :: Bool  
  }
  deriving (Show)

data Jogo = Jogo { 
     baseJogo :: Base
    ,portaisJogo :: [Portal]
    ,torresJogo :: [Torre]
    ,mapaJogo :: [[Terreno]]
    ,inimigosJogo :: [Inimigo]
    ,lojaJogo :: Loja
    ,nLinhas     :: Int
    ,nColunas    :: Int
  }
  deriving (Show)

data Terreno = Relva | Terra | Agua | AguaTerra deriving (Eq, Show)


type Posicao = (Float, Float)


type Creditos = Int


data Base = Base { 
    vidaBase :: Float,
    posicaoBase :: Posicao,
    creditosBase :: Creditos
  }
  deriving (Show)


type Tempo = Float


data Duracao = Finita Tempo | Infinita deriving (Eq, Show, Ord)


data Torre = Torre { 
    posicaoTorre :: Posicao,
    danoTorre :: Float,
    alcanceTorre :: Float,
    -- | Número de máximo de inimigos simultaneamente atingidos por uma rajada de tiros.
    rajadaTorre :: Int,
    -- | Ciclo de tempo entre rajadas de tiros.
    cicloTorre :: Tempo,
    -- | Tempo restante para a próxima rajada de tiros.
    tempoTorre :: Tempo,
    projetilTorre :: Projetil
  }
  deriving (Show)


type Loja = [(Creditos, Torre)]


data TipoProjetil = Fogo | Gelo | Resina deriving (Eq, Show)


data Projetil = Projetil { 
    tipoProjetil :: TipoProjetil,
    duracaoProjetil :: Duracao,
    reducaoVelocidade :: Float  -- (0 para fogo/gelo)
  }
  deriving (Show, Eq)


data Direcao = Norte | Sul | Este | Oeste deriving (Eq, Show)


data Inimigo = Inimigo { 
    posicaoInimigo :: Posicao,
    -- | Direção do último movimento do inimigo.
    direcaoInimigo :: Direcao,
    -- | Vida do inimigo.
    vidaInimigo :: Float,
    -- | Velocidade do inimigo.
    velocidadeInimigo :: Float,
    -- | Dano causado pelo inimigo na base do jogador.
    ataqueInimigo :: Float,
    -- | Créditos que o jogador recebe ao derrotar o inimigo.
    --butimInimigo :: Creditos,
    -- | Efeitos secundários ativos no inimigo.
    projeteisInimigo :: [Projetil],
    tipoInimigo :: TipoInimigo
  }
  deriving (Show, Eq )


data TipoInimigo = Flora | Stella  deriving (Show,Eq)


data Onda = Onda { 
    -- | Inimigos que compõem a onda.
    inimigosOnda :: [Inimigo],
    -- | Tempo em segundos entre a entrada de cada inimigo.
    cicloOnda :: Tempo,
    -- | Tempo restante, em segundos, para a entrada do próximo inimigo da onda.
    tempoOnda :: Tempo,
    -- | Tempo restante, em segundos, para a entrada da onda.
    entradaOnda :: Tempo
  }
  deriving (Show)


-- | Valor inicial que determina a sequência de números pseudo-aleatórios.
type Semente = Int

{-| Função que gera uma lista de números aleatórios a partir de uma 'Semente'.

== Exemplos

>>> geraAleatorios 2425 3
[9108974057934916489,3509742222561512871,1534041518507426227]

>>> geraAleatorios 10 1
[3575835729477015470]
-}
geraAleatorios :: Semente -> Int -> [Int]
geraAleatorios s c = take c $ System.Random.randoms (System.Random.mkStdGen s)


dimensaoBotaoJogar :: (Float, Float)
dimensaoBotaoJogar = (200, 58)

dimensaoBotaoVoltar :: (Float, Float)
dimensaoBotaoVoltar = (336, 36)

dimensaoBotaoNivel1 :: (Float, Float)
dimensaoBotaoNivel1 = (143, 16)

dimensaoBotaoNivel2 :: (Float, Float)
dimensaoBotaoNivel2 = (148, 16)

dimensaoBotaoNivel3 :: (Float, Float)
dimensaoBotaoNivel3 = (148, 16)

dimensaoBotaoRegras :: (Float, Float)
dimensaoBotaoRegras = (77, 100)

dimensaoBotaoSetaEsq :: (Float, Float)
dimensaoBotaoSetaEsq = (80, 87)

dimensaoBotaoSetaDir :: (Float, Float)
dimensaoBotaoSetaDir = (80, 87)

dimensaoBotaoTorre :: (Float, Float)
dimensaoBotaoTorre = (92, 136)
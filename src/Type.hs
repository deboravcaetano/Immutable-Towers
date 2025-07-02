module Type where
import Graphics.Gloss.Interface.Pure.Game
import System.Random 

data Janela = Menu | EscolhaNivel | Goal | TorreFogoInfo | TorreGeloInfo | TorreResinaInfo | Game Jogo -- | Ganhou Jogo | Perdeu Jogo | Pausa Jogo 

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
  , imagemBotao3 :: Picture
  , imagemBotao4 :: Picture
  , imagemBotao5 :: Picture
  , imagemRelva :: Picture
  , imagemTerra :: Picture
  , imagemAgua :: Picture   
  , imagemBase :: Picture 
  , imagemPortal :: Picture 
  , imagemLoja :: Picture
  , imagemBotaoFogo :: Picture
  , imagemBotaoGelo :: Picture
  , imagemBotaoResina :: Picture  
  , jogoatual :: Janela       
  }

data Portal = Portal {
    posicaoPortal :: Posicao,
    ondasPortal :: [Onda],
    ativo :: Bool  
  }
  deriving (Show)

data Jogo = Jogo { 
    baseJogo :: Base,
    portaisJogo :: [Portal],
    --torresJogo :: [Torre],
    mapaJogo :: Mapa--,
    --inimigosJogo :: [Inimigo],
    --lojaJogo :: Loja
  }
  deriving (Show)

  -- | Tipo de terrenno do mapa.
data Terreno
  = -- | Torres constroem-se sobre o relvado do mapa.
    Relva
  | -- | A base e os portais constroem-se sobre caminhos de terra do mapa. Além disso, inimigos movem-se sobre estes terrenos.
    Terra
  | -- | Água para efeito decorativo, mas onde não se pode construir, nem os inimigos se podem mover.
    Agua
  deriving (Eq, Show)

-- | Mapa do jogo composto por uma matriz de terrenos.
type Mapa = [[Terreno]]

-- | Coordenada bilateral de uma entidade no jogo, representante do seu centro.
-- O referencial tem origem no canto superior esquerdo, com eixos x e y positivos para a direita e para baixo, respectivamente.
type Posicao = (Float, Float)

-- | Moeda do jogo.
type Creditos = Int

-- | Base de operações do jogador.
data Base = Base
  { -- | Vida da base. Quando esta chega a zero, o jogador perde o jogo.
    vidaBase :: Float,
    -- | Posição da base no mapa. A base deve estar sobre um terreno de terra.
    posicaoBase :: Posicao,
    -- | Balanço de créditos do jogador.
    creditosBase :: Creditos
  }
  deriving (Show)

-- | Distância entre duas posições.
type Distancia = Float

-- | Tempo em segundos.
type Tempo = Float

-- | Representa uma duração em segundos
data Duracao
  = -- | Duração em segundos
    Finita Tempo
  | -- | Duração infinita
    Infinita
  deriving (Eq, Show, Ord)

-- | Torre que dispara projéteis contra inimigos.
data Torre = Torre
  { -- | Posição da torre no mapa.
    posicaoTorre :: Posicao,
    -- | Redução de vida no inimigo pelo impacto do projétil.
    danoTorre :: Float,
    -- | Alcance circular da torre.
    alcanceTorre :: Float,
    -- | Número de máximo de inimigos simultaneamente atingidos por uma rajada de tiros.
    rajadaTorre :: Int,
    -- | Ciclo de tempo entre rajadas de tiros.
    cicloTorre :: Tempo,
    -- | Tempo restante para a próxima rajada de tiros.
    tempoTorre :: Tempo,
    -- | Efeito secundário associado ao tipo de projétil da torre.
    projetilTorre :: Projetil
  }
  deriving (Show)

-- | Loja de torres disponíveis para construir por uma quantidade de créditos.
type Loja = [(Creditos, Torre)]

-- | Tipo de projétil disparado por uma torre.
data TipoProjetil = Fogo | Gelo | Resina
  deriving (Eq, Show)

-- | Projétil aplicado por uma torre.
data Projetil = Projetil
  { -- | Tipo de projétil.
    tipoProjetil :: TipoProjetil,
    -- | Duração do efeito do projétil no inimigo.
    duracaoProjetil :: Duracao
  }
  deriving (Show)

-- | Direção de movimento de uma entidade no jogo.
data Direcao
  = Norte
  | Sul
  | Este
  | Oeste
  deriving (Eq, Show)

-- | Inimigo que se move em direção à base do jogador.
data Inimigo = Inimigo
  { -- | Posição do inimigo no mapa.
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
    butimInimigo :: Creditos,
    -- | Efeitos secundários ativos no inimigo.
    projeteisInimigo :: [Projetil],
    -- | Tipo de inimigo.
    tipoInimigo :: TipoInimigo
  }
  deriving (Show)

-- | Inimigos que podem existir no jogo.
data TipoInimigo = Zombie | Creeper | Esqueleto1 | Esqueleto2  
  deriving (Show,Eq)

-- | Onda de inimigos que saem de um portal.
data Onda = Onda
  { -- | Inimigos que compõem a onda.
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
dimensaoBotaoJogar = (381, 36)

dimensaoBotaoVoltar :: (Float, Float)
dimensaoBotaoVoltar = (86, 80)

dimensaoBotaoNivel1 :: (Float, Float)
dimensaoBotaoNivel1 = (235, 225)

dimensaoBotaoNivel2 :: (Float, Float)
dimensaoBotaoNivel2 = (235, 225)

dimensaoBotaoNivel3 :: (Float, Float)
dimensaoBotaoNivel3 = (235, 225)

dimensaoBotaoRegras :: (Float, Float)
dimensaoBotaoRegras = (112, 145)

dimensaoBotaoAbaAberta :: (Float, Float)
dimensaoBotaoAbaAberta = (169, 43)

dimensaoBotaoAbaFechada :: (Float, Float)
dimensaoBotaoAbaFechada = (169, 26)

dimensaoBotaoTorre :: (Float, Float)
dimensaoBotaoTorre = (92, 136)
{-| ALUNO 1 - LETICIA - ARQUITETO DE DADOS -}
module Types where

import Data.Map (Map)
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Data.Time.Clock (getCurrentTime)

----------------------------------------------------
{- Definição dos tipos de dados -}
data Item = Item --representa um item no inventario
    { itemID :: String --^ identificador único do item
    , nome :: String --^ nome do item
    , quantidade :: Int --^ quantidade em estoque
    , categoria :: String --^ categoria do item 
    } deriving (Show, Read, Eq, Generic)

type Inventario = Map String Item --inventario é um mapa dos id's dos itens p/ os proprios itens
----------------------------------------------------
{- Definição de tipos de log -}
data AcaoLog --tipo de ação registrada no log de auditoria
    = Add --^ incluir novo Item
    | Remove --^remove item existente
    | Update --^atualiza item existente
    | QueryFail --^falha ao tentar consulta/operação
    deriving(Show, Read, Eq, Generic)
    
data StatusLog --status da operação registrada no log
    = Sucesso --^operacao executada corretamente
    | Falha String --^operacao falhou
    deriving(Show, Read, Eq, Generic)

data LogEntry = LogEntry
    { timestamp :: UTCTime --^momento da operacao
    , acao :: AcaoLog --^tipo de Acao
    , detalhes :: String --^ detalhes da operacao
    , status :: StatusLog --^resultado 
    } deriving(Show, Read, Eq, Generic)
----------------------------------------------------

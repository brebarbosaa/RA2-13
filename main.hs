
import Data.Map (Map, empty, singleton, lookup, member, insert, delete, elems, size, fromList)
import qualified Data.Map as Map
import Data.Time.Clock (getCurrentTime, UTCTime)
import GHC.Generics (Generic)

-- Aluno 1: Arquiteto de Dados - Leticia --
data Item = Item
    { itemID :: String
    , nome :: String
    , quantidade :: Int
    , categoria :: String
    } deriving (Show, Read, Eq, Generic)

type Inventario = Map String Item

data AcaoLog = Add | Remove | Update | QueryFail
    deriving (Show, Read, Eq, Generic)

data StatusLog = Sucesso | Falha String
    deriving (Show, Read, Eq, Generic)

data LogEntry = LogEntry
    { timestamp :: UTCTime
    , acao :: AcaoLog
    , detalhes :: String
    , status :: StatusLog
    } deriving (Show, Read, Eq, Generic)

--  Aluno 2: Lógica de Negócio Pura -  Alana --

type ResultadoOperacao = (Inventario, LogEntry)

-- Função para adicionar item
addItem :: UTCTime -> String -> String -> Int -> String -> Inventario -> Either String ResultadoOperacao
addItem timestamp itemID nome qtd categoria inventario
    | Map.member itemID inventario = 
        Left $ "Erro: ID já existente"
    | qtd <= 0 = 
        Left "Erro: Quantidade não pode ser menor que ou igual a zero"
    | null itemID || null nome || null categoria = 
        Left "Erro: ID, nome e categoria não podem estar vazios."
    | otherwise =
        let novoItem = Item itemID nome qtd categoria
            novoInventario = Map.insert itemID novoItem inventario
            logEntry = LogEntry timestamp Add (show novoItem) Sucesso
        in Right (novoInventario, logEntry)

-- Função remover item
removeItem :: UTCTime -> String -> Int -> Inventario -> Either String ResultadoOperacao
removeItem timestamp itemID qtdRemover inventario =
    case Map.lookup itemID inventario of
        Nothing -> 
            Left $ "Erro: Item " ++ itemID ++ " não foi localizado."
        Just item ->
            if qtdRemover <= 0
            then Left "Erro: a quantidade a remover deve ser positiva."
            else if quantidade item < qtdRemover
                then 
                    let erroMsg = "Estoque insuficiente. Estoque disponível: " ++ show (quantidade item)
                        logEntry = LogEntry timestamp Remove 
                            (itemID ++ " Tentativa: " ++ show qtdRemover) 
                            (Falha erroMsg)
                    in Left erroMsg
                else
                    let novaQtd = quantidade item - qtdRemover
                        (novoInventario, acaoDetalhes) = 
                            if novaQtd == 0
                            then (Map.delete itemID inventario, "Item removido")
                            else (Map.insert itemID (item { quantidade = novaQtd }) inventario, 
                                  "Quantidade atualizada: " ++ show novaQtd)
                        logEntry = LogEntry timestamp Remove 
                            (itemID ++ acaoDetalhes ++ " Quantidade removida: " ++ show qtdRemover) 
                            Sucesso
                    in Right (novoInventario, logEntry)

-- Função atualizar quantidade
updateQty :: UTCTime -> String -> Int -> Inventario -> Either String ResultadoOperacao
updateQty timestamp itemID novaQtd inventario =
    case Map.lookup itemID inventario of
        Nothing -> 
            Left $ "Erro: Item " ++ itemID ++ " não foi localizado."
        Just item ->
            if novaQtd < 0
            then Left "Erro: Quantidade não pode ser negativa."
            else
                let itemAtualizado = item { quantidade = novaQtd }
                    novoInventario = Map.insert itemID itemAtualizado inventario
                    logEntry = LogEntry timestamp Update 
                        (itemID ++ " Nova quantidade: " ++ show novaQtd) 
                        Sucesso
                in Right (novoInventario, logEntry)

-- Funções auxiliares
getItem :: String -> Inventario -> Maybe Item
getItem = Map.lookup

itemExists :: String -> Inventario -> Bool
itemExists = Map.member

getAllItems :: Inventario -> [Item]
getAllItems = Map.elems

getInventorySize :: Inventario -> Int
getInventorySize = Map.size

isEmpty :: Inventario -> Bool
isEmpty = Map.null

-- Testes de Serialização -- 
testSerialization :: (Show a, Read a, Eq a) => String -> a -> IO ()
testSerialization nome valor = do
    let serializado = show valor
        desserializado = read serializado
    if valor == desserializado
        then putStrLn $ nome ++ " passou no teste de serialização!"
        else do
            putStrLn $ nome ++ " falhou!"
            putStrLn $ "  Original: " ++ show valor
            putStrLn $ "  Desserializado: " ++ show desserializado

-- Main
main :: IO ()
main = do
    tempo <- getCurrentTime

    let itemTeste = Item "001" "Teclado" 10 "Periférico"
        logTeste  = LogEntry tempo Add "Adicionado item 001" Sucesso
        acaoTeste = Remove
        statusTeste = Falha "Erro no teste"
    putStrLn " Iniciando testes de serialização do módulo Types..."
    testSerialization "Item" itemTeste
    testSerialization "LogEntry" logTeste
    testSerialization "AcaoLog" acaoTeste
    testSerialization "StatusLog" statusTeste
    putStrLn " Todos os testes concluídos."
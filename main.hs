import Data.Map (Map)
import qualified Data.Map as Map
import Data.Time.Clock (getCurrentTime, UTCTime)
import System.IO (hFlush, stdout, hIsEOF)
import Control.Exception (catch, IOException)
import System.Directory (doesFileExist)
import Data.List (maximumBy)
import Data.Ord (comparing)
import Data.Char (isDigit)
import Text.Read (readMaybe)
import Data.Maybe (mapMaybe)

-- Aluno 1: Arquiteto de Dados - Leticia --
data Item = Item
    { itemID :: String
    , nome :: String
    , quantidade :: Int
    , categoria :: String
    } deriving (Show, Read, Eq)

type Inventario = Map String Item

data AcaoLog = Add String          
             | Remove String Int 
             | Update String Int  
             | QueryFail String  
    deriving (Show, Read, Eq)

data StatusLog = Sucesso | Falha String
    deriving (Show, Read, Eq)

data LogEntry = LogEntry
    { timestamp :: UTCTime
    , acao :: AcaoLog
    , detalhes :: String
    , status :: StatusLog
    } deriving (Show, Read, Eq)

--  Aluno 2: Lógica de Negócio Pura -  Alana --

type ResultadoOperacao = (Inventario, LogEntry)

-- Função para adicionar item
addItem :: UTCTime -> String -> String -> Int -> String -> Inventario -> Either String ResultadoOperacao
addItem tempo iid nomeItem qtd categoria inventario
    | Map.member iid inventario = Left "Erro: ID já existente"
    | qtd <= 0 = Left "Erro: Quantidade deve ser maior que zero"
    | null iid || null nomeItem || null categoria = Left "Erro: ID, nome e categoria não podem estar vazios."
    | otherwise =
        let novoItem = Item iid nomeItem qtd categoria
            novoInventario = Map.insert iid novoItem inventario
            logEntry = LogEntry tempo (Add iid) (show novoItem) Sucesso
        in Right (novoInventario, logEntry)

-- Função remover item
removeItem :: UTCTime -> String -> Int -> Inventario -> Either String ResultadoOperacao
removeItem tempo iid qtdRemover inventario =
    case Map.lookup iid inventario of
        Nothing -> Left $ "Erro: Item " ++ iid ++ " não foi localizado."
        Just item ->
            if qtdRemover <= 0
            then Left "Erro: a quantidade a remover deve ser maior que zero."
            else if quantidade item < qtdRemover
                then
                    let erroMsg = "Estoque insuficiente. Estoque disponível: " ++ show (quantidade item)
                        _logEntry = LogEntry tempo (Remove iid qtdRemover) (itemID item ++ " Tentativa: " ++ show qtdRemover) (Falha erroMsg)
                    in Left erroMsg
                else
                    let novaQtd = quantidade item - qtdRemover
                        (novoInventario, acaoDetalhes) =
                            if novaQtd == 0
                            then (Map.delete iid inventario, " Item removido.")
                            else (Map.insert iid (item { quantidade = novaQtd }) inventario,
                                  " Quantidade atualizada: " ++ show novaQtd)
                        logEntry = LogEntry tempo (Remove iid qtdRemover)
                                   (itemID item ++ acaoDetalhes ++ " Quantidade removida: " ++ show qtdRemover)
                                   Sucesso
                    in Right (novoInventario, logEntry)

-- Função atualizar quantidade
updateQty :: UTCTime -> String -> Int -> Inventario -> Either String ResultadoOperacao
updateQty tempo iid novaQtd inventario =
    case Map.lookup iid inventario of
        Nothing -> Left $ "Erro: Item " ++ iid ++ " não foi localizado."
        Just item ->
            if novaQtd < 0
            then Left "Erro: Quantidade não pode ser negativa."
            else
                let itemAtualizado = item { quantidade = novaQtd }
                    novoInventario = Map.insert iid itemAtualizado inventario
                    logEntry = LogEntry tempo (Update iid novaQtd)
                        (itemID item ++ " Nova quantidade: " ++ show novaQtd) Sucesso
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


-- Aluno 3: Módulo de I/O e Persistência - Leticia --

inventarioFile :: FilePath
inventarioFile = "Inventario.dat"

logFile :: FilePath
logFile = "Auditoria.log"

-- carregar Inventario e tratar exceções --
loadInventory :: IO Inventario
loadInventory = catch (do
    exists <- doesFileExist inventarioFile
    if exists
        then do
            content <- readFile inventarioFile
            if null content
                then return Map.empty
                else case readMaybe content :: Maybe Inventario of
                        Just inv -> return inv
                        Nothing  -> return Map.empty
        else return Map.empty
    ) (\(_ :: IOException) -> return Map.empty)

-- salvar estado atual do inventario --
saveInventory :: Inventario -> IO ()
saveInventory inv = writeFile inventarioFile (show inv)

-- loadLogs
loadLogs :: IO [LogEntry]
loadLogs = catch (do
    exists <- doesFileExist logFile
    if not exists then return []
    else readFile logFile >>= return . mapMaybe readMaybe . lines
    ) (\(_ :: IOException) -> return [])

appendLog :: LogEntry -> IO ()
appendLog logEntry = appendFile logFile (show logEntry ++ "\n")

safeReadInt :: String -> Maybe Int
safeReadInt s = readMaybe s :: Maybe Int

-- Main
main :: IO ()
main = do
    putStrLn "=== Inicializando sistema de inventário ==="
    inv <- loadInventory
    logs <- loadLogs
    putStrLn $ "Inventário carregado. Itens: " ++ show (Map.size inv)
    putStrLn $ "Comandos: "
    putStrLn  $ " add <id> <nome> <quantidade> <categoria>"
    putStrLn $ " remove <id> <quantidade>"
    putStrLn $ " update <id> <novaQuantidade>"
    putStrLn $ " list"
    putStrLn $ " report"
    putStrLn $ " exit"
    loop inv logs

-- loop principal
loop :: Inventario -> [LogEntry] -> IO ()
loop inv logs = do
    putStr "\n> "
    hFlush stdout
    linha <- getLine
    tempo <- getCurrentTime
    let tokens = words linha
    case tokens of

        ("add":iid:nomeItem:qtdStr:categoria:[]) -> 
            case safeReadInt qtdStr of
                Nothing -> do
                    let err = "Quantidade inválida"
                    putStrLn err
                    let logFalha = LogEntry tempo (QueryFail linha) err (Falha err)
                    appendLog logFalha
                    loop inv (logs ++ [logFalha])
                Just qtd ->
                    case addItem(tempo) iid nomeItem qtd categoria inv of
                        Left err -> do
                            putStrLn err
                            let logFalha = LogEntry tempo (QueryFail linha) err (Falha err)
                            appendLog logFalha
                            loop inv (logs ++ [logFalha])
                        Right (novoInv, logEntry) -> do
                            saveInventory novoInv
                            appendLog logEntry
                            putStrLn "Item adicionado com sucesso!"
                            loop novoInv (logs ++ [logEntry])

        ("remove":iid:qtdStr:[]) ->
            case safeReadInt qtdStr of
                Nothing -> do
                    let err = "Quantidade inválida"
                    putStrLn err
                    let logFalha = LogEntry tempo (QueryFail linha) err (Falha err)
                    appendLog logFalha
                    loop inv (logs ++ [logFalha])
                Just qtd ->
                    case removeItem tempo iid qtd inv of
                        Left err -> do
                            putStrLn err
                            let logFalha = LogEntry tempo (Remove iid qtd) err (Falha err)
                            appendLog logFalha
                            loop inv (logs ++ [logFalha])
                        Right (novoInv, logEntry) -> do
                            saveInventory novoInv
                            appendLog logEntry
                            putStrLn "Item removido/atualizado com sucesso!"
                            loop novoInv (logs ++ [logEntry])

        ("update":iid:qtdStr:[]) ->
            case safeReadInt qtdStr of
                Nothing -> do
                    let err = "Quantidade inválida"
                    putStrLn err
                    let logFalha = LogEntry tempo (QueryFail linha) err (Falha err)
                    appendLog logFalha
                    loop inv (logs ++ [logFalha])
                Just qtd ->
                    case updateQty tempo iid qtd inv of
                        Left err -> do
                            putStrLn err
                            let logFalha = LogEntry tempo (Update iid qtd) err (Falha err)
                            appendLog logFalha
                            loop inv (logs ++ [logFalha])
                        Right (novoInv, logEntry) -> do
                            saveInventory novoInv
                            appendLog logEntry
                            putStrLn "Quantidade atualizada com sucesso!"
                            loop novoInv (logs ++ [logEntry])

        ("list":[]) -> do
            putStrLn "\nInventário atual:"
            mapM_ print (Map.elems inv)
            loop inv logs

        ("report":[]) -> do
            let porCategoria = itensPorCategoria inv
            putStrLn "Itens por categoria:\n"
            mapM_ (\(categoria, total) -> putStrLn $ " " ++ categoria ++ ": " ++ show total)
                (Map.toList porCategoria)
            putStrLn "\nLogs de erro recentes:\n"
            let erros = logsDeErro logs
            mapM_ print erros
            putStrLn "\nItens com estoque baixo (qtd < 5):\n"
            mapM_ print (estoqueBaixo inv)
            putStrLn "\nItem mais movimentado:"
            case itemMaisMovimentado logs of
                Nothing -> putStrLn "Nenhum movimento registrado."
                Just (iid,c) -> putStrLn $ iid ++ " com " ++ show c ++ " operações."
            loop inv logs

        ("exit":[]) -> 
            putStrLn "Encerrando o sistema..."

        _ -> do
            putStrLn "Comando inválido!"
            loop inv logs
            
-- Aluno 4: Validação, Documentação e Gerenciamento do Repositório - Brenda --

-- Função histórico por item 
historicoPorItem :: String -> [LogEntry] -> [LogEntry]
historicoPorItem iid logs =
    filter (\le -> case acao le of
                     Add id'        -> id' == iid
                     Remove id' _   -> id' == iid
                     Update id' _   -> id' == iid
                     QueryFail _    -> False) logs

-- Logs que deram Erro
logsDeErro :: [LogEntry] -> [LogEntry]
logsDeErro logs = filter (\logEntry ->
    case status logEntry of
        Falha _ -> True
        _ -> False
    ) logs

-- Item com mais movimentações
itemMaisMovimentado :: [LogEntry] -> Maybe (String, Int)
itemMaisMovimentado logs =
    if null contagemItens
        then Nothing
        else Just (fst itemMax, snd itemMax)
    where
        extrairItens = concatMap (\logEntry -> case acao logEntry of
                                                Add id' -> [id']
                                                Remove id' _ -> [id']
                                                Update id' _ -> [id']
                                                QueryFail _ -> []) logs
        contagemItens = Map.toList $ foldr (\id' acc -> Map.insertWith (+) id' 1 acc) Map.empty extrairItens
        itemMax = maximumBy (comparing snd) contagemItens

-- Relatório estoque baixo
estoqueBaixo :: Inventario -> [Item]
estoqueBaixo inventario = filter (\item -> quantidade item < 5) (Map.elems inventario)

itensPorCategoria :: Inventario -> Map String Int
itensPorCategoria inventario =
    foldr (\item acc ->
        Map.insertWith (+) (categoria item) (quantidade item) acc)
        Map.empty
        (Map.elems inventario)

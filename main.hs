
import Data.Map (Map, empty, singleton, lookup, member, insert, delete, elems, size, fromList)
import qualified Data.Map as Map
import Data.Time.Clock (getCurrentTime, UTCTime)
import GHC.Generics (Generic)
import System.IO (hFlush, stdout) --pt3
import Control.Exception (catch, IOException) --pt3
import System.Directory (doesFileExist) --pt3

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


-- Aluno 3: Módulo de I/O e Persistência - Leticia -- 

inventarioFile :: FilePath
inventarioFile = "Inventario.dat"

logFile :: FilePath
logFile = "Auditoria.log"

-- carregar Inventario salvo e tratar exceções --
loadInventory :: IO Inventario
loadInventory = catch (do exists <- doesFileExist inventarioFile
    if exists then do content <- readFile inventarioFile
        return (read content)
    else return Map.empty)
    (\(_ :: IOException) -> return Map.empty)

-- salvar estado atual do inventario --
saveInventory :: Inventario -> IO ()
saveInventory inv = writeFile inventarioFile (show inv)

-- gravar estrada no log de auditoria --
appendAudit :: LogEntry -> IO ()
appendAudit logEntry = appendFile logFile (show logEntry ++ "\n")

-- exibir menu interativo --
printMenu :: IO ()
printMenu = do
    putStrLn "\n *** Sistema de Inventário ***"
    putStrLn "[1] Adicionar item"
    putStrLn "[2] Remover item"
    putStrLn "[3] Atualizar quantidade"
    putStrLn "[4] Listar itens"
    putStrLn "[0] Sair"
    putStrLn "Escolha: "
    hFlush stdout
    
-- Main -- 
main :: IO ()
main = do
    putStrLn "=== Inicializando sistema de inventário ==="
    inv <- loadInventory
    putStrLn $ "Inventário carregado. Itens: " ++ show(Map.size inv)
    loop inv

-- loop principal interativo -- 
loop :: Inventario -> IO ()
loop inv = do
    printMenu
    opcao <- getLine
    case opcao of
        "1" -> do
            tempo <- getCurrentTime
            putStrLn "ID: " >> hFlush stdout
            itemID <- getLine
            putStrLn "Nome: " >> hFlush stdout
            nome <- getLine
            putStrLn "Quantidade: " >> hFlush stdout
            qtdeStr <- getLine
            putStrLn "Categoria: " >> hFlush stdout
            categoria <- getLine
            let qtd = read qtdeStr :: Int
            case addItem tempo itemID nome qtd categoria inv of
                Left err -> do
                    putStrLn err
                    appendFile logFile (show tempo ++ " - Falha: " ++ err ++ "\n")
                    loop inv
                Right (novoInv, logEntry) -> do
                    saveInventory novoInv
                    appendAudit logEntry
                    putStrLn "Item adicionado com sucesso!"
                    loop novoInv
        "2" -> do
            tempo <- getCurrentTime
            putStrLn "ID do item a ser removido: " >> hFlush stdout
            itemID <- getLine
            putStrLn "Quantidade a ser removida: " >> hFlush stdout
            qtdeStr <- getLine
            let qtd = read qtdeStr :: Int
            case removeItem tempo itemID qtd inv of
                Left err -> do
                    putStrLn err
                    appendFile logFile (show tempo ++ " - Falha: " ++ err ++ "\n")
                    loop inv
                Right (novoInv, logEntry) -> do
                    saveInventory novoInv
                    appendAudit logEntry
                    putStrLn "Item removido/atualizado com sucesso!"
                    loop novoInv
        "3" -> do
            tempo <- getCurrentTime
            putStrLn "ID: " >> hFlush stdout
            itemID <- getLine
            putStrLn "Nova quantidade: " >> hFlush stdout
            qtdeStr <- getLine
            let qtd = read qtdeStr :: Int
            case updateQty tempo itemID qtd inv of 
                Left err -> do
                    putStrLn err
                    appendFile logFile (show tempo ++ " - Falha: " ++ err ++ "\n")
                    loop inv
                Right (novoInv, logEntry) -> do
                    saveInventory novoInv
                    appendAudit logEntry
                    putStrLn "Quantidade atualizada com sucesso!"
                    loop novoInv
        "4" -> do
            putStrLn "\nInventário atual: "
            mapM_ print (Map.elems inv)
            loop inv
        "0" -> putStrLn "Encerrando o sistema..."
        
        _ -> do
            putStrLn "Opção inválida!"
            loop inv

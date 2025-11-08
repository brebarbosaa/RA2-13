{- TestTypes -}
module Main where

import Types
import Data.Time.Clock (getCurrentTime)

-- Função que testa se (read . show) preserva os dados
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

main :: IO ()
main = do
    tempo <- getCurrentTime

    let itemTeste = Item "001" "Teclado" 10 "Periférico"
        logTeste  = LogEntry tempo Add "Adicionado item 001" Sucesso
    putStrLn " Iniciando testes de serialização do módulo Types..."
    testSerialization "Item" itemTeste
    testSerialization "LogEntry" logTeste
    putStrLn " Todos os testes concluídos."
# Sistema de Gerenciamento de Inventário em Haskell

**Disciplina:** Programação Lógica e Funcional  
**Professor:** Frank Coelho de Alcantara  
**Instituição:** Pontifícia Universidade Católica do Paraná 
**Turma:** Turma B

## Integrantes do Grupo (ordem alfabética)

| Nome                     | Papel                        | Usuário GitHub        |
|--------------------------|--------------------------------------------|-----------------------|
| Alana                    | Lógica de Negócio Pura                    | @alanaqroz        |
| Brenda                   | Validação, Relatórios e Documentação      | @brebarbosaa        |
| Leticia                  | Arquiteto de Dados + Módulo de I/O        | @Andrade-Leticia       |

### Link para execução online no OnlineGDB

[ https://onlinegdb.com/SEU_LINK_AQUI](https://www.onlinegdb.com/edit/1KtA91Plzd)

## Descrição do Projeto

Sistema de controle de inventário desenvolvido em Haskell

- Separação total entre lógica pura e operações de I/O
- Persistência do inventário em `Inventario.dat`
- Auditoria completa em `Auditoria.log` (append-only)
- Carregamento automático do estado anterior ao iniciar
- Relatórios e análises a partir dos logs
- Tratamento robusto de erros e exceções

## Comandos disponíveis
add <id> <nome> <quantidade> <categoria>    (adiciona ou cria item)
remove <id> <quantidade>                    (remove quantidade ou item inteiro)
update <id> <novaQuantidade>                (altera quantidade diretamente)
list                                        (exibe todo o inventário)
report                                      (gera relatórios completos)
exit                                        (encerra o programa)

## Cenários de teste manuais

### Cenário 1 – Persistência de estado (Sucesso)

1 - Iniciar o programa (sem arquivos de dados).
2 - Adicionar 3 itens.
3 - Fechar o programa.
4 - Verificar se os arquivos Inventario.dat e Auditoria.log foram criados.
5 - Reiniciar o programa.
6 - Executar um comando de "listar" (a ser criado) ou verificar se o estado carregado em
memória contém os 3 itens.
**Resultado:**

### 2. Cenário 2: Erro de Lógica (Estoque Insuficiente)
1 - Adicionar um item com 10 unidades (ex: "teclado").
2 -  Tentar remover 15 unidades desse item.
3 - Verificar se o programa exibiu uma mensagem de erro clara.
4 - Verificar se o Inventario.dat (e o estado em memória) ainda mostra 10 unidades.
5 - Verificar se o Auditoria.log contém uma LogEntry com StatusLog (Falha ...).
**Resultado:**

### 3. Cenário 3: Geração de Relatório de Erros
1 - Após executar o Cenário 2, executar o comando report.
2 - Verificar se o relatório gerado (especificamente pela função logsDeErro) exibe a
entrada de log referente à falha registrada no Cenário 2 (a tentativa de remover
estoque insuficiente).
**Resultado:**


### Dados mínimos (10 itens distintos)

O sistema foi populado com pelo menos 10 itens diferentes durante os testes (periféricos, eletrodomesticos, etc.).

### Funções puras de lógica de negócio:

| Função        | Assinatura                                                                                         | Descrição principal                                                                                 | Casos de falha (Left)                                         | Comportamento em sucesso (Right)                                      |
|---------------|----------------------------------------------------------------------------------------------------|-----------------------------------------------------------------------------------------------------|----------------------------------------------------------------|-----------------------------------------------------------------------|
| `addItem`     | `UTCTime -> String -> String -> Int -> String -> Inventario -> Either String ResultadoOperacao`   | Adiciona um novo item ao inventário                                                                 | • ID já existe<br>• Quantidade ≤ 0<br>• Campos vazios          | Insere o item e gera LogEntry (Add ...) com Sucesso                   |
| `removeItem`  | `UTCTime -> String -> Int -> Inventario -> Either String ResultadoOperacao`                        | Remove uma quantidade de um item (remove completamente se ficar 0)                                 | • Item não existe<br>• Quantidade a remover ≤ 0<br>• Estoque insuficiente | Atualiza quantidade ou deleta o item + gera LogEntry (Remove ...)    |
| `updateQty`   | `UTCTime -> String -> Int -> Inventario -> Either String ResultadoOperacao`                        | Atualiza diretamente a quantidade de um item existente                                              | • Item não existe<br>• Nova quantidade < 0                    | Substitui a quantidade + gera LogEntry (Update ...) com Sucesso       |

### Funções de I/O e persistência:

| Função            | Assinatura                     | Responsabilidade principal                                                                                                                                |
|-------------------|--------------------------------|------------------------------------------------------------------------------------------------------------|
| `loadInventory`   | `IO Inventario`                | Carrega o inventário do arquivo `Inventario.dat` (cria vazio se não existir ou corrompido)                 |
| `saveInventory`   | `Inventario -> IO ()`          | Sobrescreve `Inventario.dat` com o estado atual (apenas em caso de sucesso)                               |                               
| `loadLogs`        | `IO [LogEntry]`                | Carrega todas as linhas de `Auditoria.log` convertendo com `readMaybe`                                     |
| `appendLog`       | `LogEntry -> IO ()`            | Adiciona uma nova entrada no final do arquivo de log (append-only)                                          
| `loop`            | `Inventario -> [LogEntry] -> IO ()` | Loop principal interativo com parsing de comandos e atualização de estado                                | 
| `main`            | `IO ()`                        | Inicializa o sistema, carrega arquivos e entra no loop                                                     |

### Funções de análise e relatório:

| Função                  | Assinatura                                   | Descrição                                                                                   | Resultado exibido no comando `report`                                      |
|-------------------------|----------------------------------------------|-------------------------------------------------------------------------------------------------------------|----------------------------------------------------------------------------|
| `historicoPorItem`      | `String -> [LogEntry] -> [LogEntry]`         | Retorna todas as entradas de log (Add, Remove, Update) referentes a um determinado `itemID`                | Não exibido diretamente (pode ser usado para consultas futuras)           |
| `logsDeErro`            | `[LogEntry] -> [LogEntry]`                   | Filtra apenas as entradas cujo `status` é `Falha _`                                                         | "Logs de erro recentes:" → lista completa das falhas registradas          |
| `itemMaisMovimentado`   | `[LogEntry] -> Maybe (String, Int)`          | Conta quantas vezes cada item aparece nas ações Add/Remove/Update e retorna o item com maior número de operações | "Item mais movimentado: <id> com <n> operações." (ou "Nenhum movimento")   |
| `estoqueBaixo`          | `Inventario -> [Item]`                       | Retorna todos os itens cujo `quantidade < 5`                                                                | "Itens com estoque baixo (qtd < 5):" → lista dos itens críticos            |
| `itensPorCategoria`     | `Inventario -> Map String Int`               | Agrupa a quantidade total de itens por categoria (soma das quantidades de todos os itens da mesma categoria) | "Itens por categoria:" → categoria + total de unidades                     |




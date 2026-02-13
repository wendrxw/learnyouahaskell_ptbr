# Módulos (Modules)

![modules](assets/images/modules/modules.png){.right width=230 height=162}

Um módulo Haskell é uma coleção de funções, tipos e typeclasses relacionados.
Um programa Haskell é uma coleção de módulos onde o módulo principal carrega os outros módulos e, em seguida, usa as funções definidas neles para fazer algo.
Ter o código dividido em vários módulos tem muitas vantagens.
Se um módulo for genérico o suficiente, as funções que ele exporta podem ser usadas em uma infinidade de programas diferentes.
Se o seu próprio código for separado em módulos independentes que não dependem muito uns dos outros (também dizemos que eles são fracamente acoplados), você poderá reutilizá-los mais tarde.
Isso torna todo o negócio de escrever código mais gerenciável, dividindo-o em várias partes, cada uma das quais tem algum tipo de propósito.

A biblioteca padrão do Haskell é dividida em módulos, cada um deles contém funções e tipos que estão de alguma forma relacionados e servem a algum propósito comum.
Há um módulo para manipular listas, um módulo para programação concorrente, um módulo para lidar com números complexos, etc.
Todas as funções, tipos e typeclasses com os quais lidamos até agora faziam parte do módulo `Prelude`, que é importado por padrão.
Neste capítulo, vamos examinar alguns módulos úteis e as funções que eles têm.
Mas primeiro, vamos ver como importar módulos.

## Carregando módulos (Loading modules) {#loading-modules}

A sintaxe para importar módulos em um script Haskell é `import <nome do módulo>`.
Isso deve ser feito antes de definir quaisquer funções, portanto, as importações geralmente são feitas no topo do arquivo.
Um script pode, é claro, importar vários módulos.
Basta colocar cada instrução de importação em uma linha separada.
Vamos importar o módulo `Data.List`, que tem um monte de funções úteis para trabalhar com listas e usar uma função que ele exporta para criar uma função que nos diz quantos elementos únicos uma lista tem.

```{.haskell:hs}
import Data.List

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub
```

Quando você faz `import Data.List`, todas as funções que `Data.List` exporta tornam-se disponíveis no namespace global, o que significa que você pode chamá-las de qualquer lugar no script.
`nub` é uma função definida em `Data.List` que pega uma lista e elimina elementos duplicados.
Compor `length` e `nub` fazendo `length . nub` produz uma função que é o equivalente a `\xs -> length (nub xs)`.

Você também pode colocar as funções de módulos no namespace global ao usar o GHCI.
Se você estiver no GHCI e quiser chamar as funções exportadas por `Data.List`, faça isso:

```{.haskell:ghci}
ghci> :m + Data.List
```

Se quisermos carregar os nomes de vários módulos dentro do GHCI, não precisamos fazer `:m +` várias vezes, podemos apenas carregar vários módulos de uma vez.

```{.haskell:ghci}
ghci> :m + Data.List Data.Map Data.Set
```

No entanto, se você carregou um script que já importa um módulo, não precisa usar `:m +` para obter acesso a ele.

Se você precisar apenas de algumas funções de um módulo, poderá importar seletivamente apenas essas funções.
Se quiséssemos importar apenas as funções `nub` e `sort` de `Data.List`, faríamos isso:

```{.haskell:hs}
import Data.List (nub, sort)
```

Você também pode optar por importar todas as funções de um módulo, exceto algumas selecionadas.
Isso geralmente é útil quando vários módulos exportam funções com o mesmo nome e você deseja se livrar das ofensivas.
Digamos que já temos nossa própria função chamada `nub` e queremos importar todas as funções de `Data.List`, exceto a função `nub`:

```{.haskell:hs}
import Data.List hiding (nub)
```

Outra maneira de lidar com conflitos de nomes é fazer importações qualificadas.
O módulo `Data.Map`, que oferece uma estrutura de dados para pesquisar valores por chave, exporta um monte de funções com o mesmo nome que as funções do `Prelude`, como `filter` ou `null`.
Portanto, quando importamos `Data.Map` e chamamos `filter`, o Haskell não saberá qual função usar.
Veja como resolvemos isso:

```{.haskell:hs}
import qualified Data.Map
```

Isso faz com que, se quisermos referenciar a função `filter` de `Data.Map`, tenhamos que fazer `Data.Map.filter`, enquanto apenas `filter` ainda se refere ao `filter` normal que todos conhecemos e amamos.
Mas digitar `Data.Map` na frente de cada função desse módulo é meio tedioso.
É por isso que podemos renomear a importação qualificada para algo mais curto:

```{.haskell:hs}
import qualified Data.Map as M
```

Agora, para referenciar a função `filter` de `Data.Map`, apenas usamos `M.filter`.

Use [esta referência útil](https://downloads.haskell.org/~ghc/latest/docs/html/libraries/) para ver quais módulos estão na biblioteca padrão.
Uma ótima maneira de obter novos conhecimentos de Haskell é simplesmente clicar na referência da biblioteca padrão e explorar os módulos e suas funções.
Você também pode visualizar o código-fonte do Haskell para cada módulo.
Ler o código-fonte de alguns módulos é uma maneira muito boa de aprender Haskell e ter uma noção sólida dele.

Para procurar funções ou descobrir onde estão localizadas, use o [Hoogle](https://hoogle.haskell.org/).
É um mecanismo de pesquisa Haskell realmente incrível, você pode pesquisar por nome, nome do módulo ou até assinatura de tipo.

## Data.List {#data-list}

O módulo `Data.List` é tudo sobre listas, obviamente.
Ele fornece algumas funções muito úteis para lidar com elas.
Já conhecemos algumas de suas funções (como `map` e `filter`) porque o módulo `Prelude` exporta algumas funções de `Data.List` por conveniência.
Você não precisa importar `Data.List` por meio de uma importação qualificada porque não entra em conflito com nenhum nome do `Prelude`, exceto aqueles que o `Prelude` já rouba de `Data.List`.
Vamos dar uma olhada em algumas das funções que não conhecemos antes.

`intersperse`{.label .function} pega um elemento e uma lista e, em seguida, coloca esse elemento entre cada par de elementos na lista.
Aqui está uma demonstração:

```{.haskell:ghci}
ghci> intersperse '.' "MONKEY"
"M.O.N.K.E.Y"
ghci> intersperse 0 [1,2,3,4,5,6]
[1,0,2,0,3,0,4,0,5,0,6]
```

`intercalate`{.label .function} pega uma lista e uma lista de listas.
Ele então insere essa lista entre todas essas listas e depois nivela o resultado.

```{.haskell:ghci}
ghci> intercalate " " ["hey","there","folks"]
"hey there folks"
ghci> intercalate [0,0,0] [[1,2,3],[4,5,6],[7,8,9]]
[1,2,3,0,0,0,4,5,6,0,0,0,7,8,9]
```

`transpose`{.label .function} transpõe uma lista de listas.
Se você olhar para uma lista de listas como uma matriz 2D, as colunas se tornarão as linhas e vice-versa.

```{.haskell:ghci}
ghci> transpose [[1,2,3],[4,5,6],[7,8,9]]
[[1,4,7],[2,5,8],[3,6,9]]
ghci> transpose ["hey","there","folks"]
["htf","eho","yel","rk","es"]
```

Digamos que temos os polinômios *3x^2^ + 5x + 9*, *10x^3^ + 9* e *8x^3^ + 5x^2^ + x - 1* e queremos somá-los.
Podemos usar as listas `[0,3,5,9]`, `[10,0,0,9]` e `[8,5,1,-1]` para representá-los em Haskell.
Agora, para adicioná-los, tudo o que temos a fazer é isso:

```{.haskell:ghci}
ghci> map sum $ transpose [[0,3,5,9],[10,0,0,9],[8,5,1,-1]]
[18,8,6,17]
```

Quando transpusemos essas três listas, as terceiras potências estão na primeira linha, as segundas potências na segunda e assim por diante.
Mapear `sum` para isso produz nosso resultado desejado.

![shopping lists](assets/images/modules/legolists.png){.left width=230 height=212}

`foldl'`{.label .function} e `foldl1'`{.label .function} são versões mais estritas de suas respectivas encarnações preguiçosas.
Ao usar dobras preguiçosas em listas realmente grandes, você pode frequentemente obter um erro de estouro de pilha (stack overflow error).
O culpado por isso é que, devido à natureza preguiçosa das dobras, o valor do acumulador não é realmente atualizado à medida que a dobra acontece.
O que realmente acontece é que o acumulador faz uma promessa de que calculará seu valor quando solicitado a realmente produzir o resultado (também chamado de thunk).
Isso acontece para cada acumulador intermediário e todos esses thunks estouram sua pilha.
As dobras estritas não são preguiçosas e realmente calculam os valores intermediários à medida que avançam, em vez de encher sua pilha com thunks.
Portanto, se você receber erros de estouro de pilha ao fazer dobras preguiçosas, tente mudar para suas versões estritas.

`concat`{.label .function} nivela uma lista de listas em apenas uma lista de elementos.

```{.haskell:ghci}
ghci> concat ["foo","bar","car"]
"foobarcar"
ghci> concat [[3,4,5],[2,3,4],[2,1,1]]
[3,4,5,2,3,4,2,1,1]
```

Ele removerá apenas um nível de aninhamento.
Portanto, se você deseja nivelar completamente `[[[2,3],[3,4,5],[2]],[[2,3],[3,4]]]`, que é uma lista de listas de listas, você deve concatená-la duas vezes.

Fazer `concatMap`{.label .function} é o mesmo que primeiro mapear uma função para uma lista e depois concatenar a lista com `concat`.

```{.haskell:ghci}
ghci> concatMap (replicate 4) [1..3]
[1,1,1,1,2,2,2,2,3,3,3,3]
```

`and`{.label .function} pega uma lista de valores booleanos e retorna `True` apenas se todos os valores na lista forem `True`.

```{.haskell:ghci}
ghci> and $ map (>4) [5,6,7,8]
True
ghci> and $ map (==4) [4,4,4,3,4]
False
```

`or`{.label .function} é como `and`, só que retorna `True` se algum dos valores booleanos em uma lista for `True`.

```{.haskell:ghci}
ghci> or $ map (==4) [2,3,4,5,6,1]
True
ghci> or $ map (>4) [1,2,3]
False
```

`any`{.label .function} e `all`{.label .function} pegam um predicado e verificam se algum ou todos os elementos de uma lista satisfazem o predicado, respectivamente.
Geralmente usamos essas duas funções em vez de mapear sobre uma lista e depois fazer `and` ou `or`.

```{.haskell:ghci}
ghci> any (==4) [2,3,5,6,1,4]
True
ghci> all (>4) [6,9,10]
True
ghci> all (`elem` ['A'..'Z']) "HEYGUYSwhatsup"
False
ghci> any (`elem` ['A'..'Z']) "HEYGUYSwhatsup"
True
```

`iterate`{.label .function} pega uma função e um valor inicial.
Ele aplica a função ao valor inicial, depois aplica essa função ao resultado, depois aplica a função a esse resultado novamente, etc.
Ele retorna todos os resultados na forma de uma lista infinita.

```{.haskell:ghci}
ghci> take 10 $ iterate (*2) 1
[1,2,4,8,16,32,64,128,256,512]
ghci> take 3 $ iterate (++ "haha") "haha"
["haha","hahahaha","hahahahahaha"]
```

`splitAt`{.label .function} pega um número e uma lista.
Ele então divide a lista em muitos elementos, retornando as duas listas resultantes em uma tupla.

```{.haskell:ghci}
ghci> splitAt 3 "heyman"
("hey","man")
ghci> splitAt 100 "heyman"
("heyman","")
ghci> splitAt (-3) "heyman"
("","heyman")
ghci> let (a,b) = splitAt 3 "foobar" in b ++ a
"barfoo"
```

`takeWhile`{.label .function} é uma pequena função realmente útil.
Ela pega elementos de uma lista enquanto o predicado se mantém e, quando um elemento é encontrado que não satisfaz o predicado, é cortado.
Acontece que isso é muito útil.

```{.haskell:ghci}
ghci> takeWhile (>3) [6,5,4,3,2,1,2,3,4,5,4,3,2,1]
[6,5,4]
ghci> takeWhile (/=' ') "This is a sentence"
"This"
```

Digamos que queríamos saber a soma de todas as terceiras potências que estão abaixo de 10.000.
Não podemos mapear `(^3)` para `[1..]`, aplicar um filtro e depois tentar somar isso porque filtrar uma lista infinita nunca termina.
Você pode saber que todos os elementos aqui são ascendentes, mas Haskell não.
É por isso que podemos fazer isso:

```{.haskell:ghci}
ghci> sum $ takeWhile (<10000) $ map (^3) [1..]
53361
```

Aplicamos `(^3)` a uma lista infinita e, uma vez que um elemento superior a 10.000 é encontrado, a lista é cortada.
Agora podemos somar facilmente.

`dropWhile`{.label .function} é semelhante, apenas descarta todos os elementos enquanto o predicado é verdadeiro.
Quando o predicado é igual a `False`, ele retorna o restante da lista.
Uma função extremamente útil e adorável!

```{.haskell:ghci}
ghci> dropWhile (/=' ') "This is a sentence"
" is a sentence"
ghci> dropWhile (<3) [1,2,2,2,3,4,5,4,3,2,1]
[3,4,5,4,3,2,1]
```

Recebemos uma lista que representa o valor de uma ação por data.
A lista é composta por tuplas cujo primeiro componente é o valor da ação, o segundo é o ano, o terceiro é o mês e o quarto é a data.
Queremos saber quando o valor das ações excedeu mil dólares!

```{.haskell:ghci}
ghci> let stock = [(994.4,2008,9,1),(995.2,2008,9,2),(999.2,2008,9,3),(1001.4,2008,9,4),(998.3,2008,9,5)]
ghci> head (dropWhile (\(val,y,m,d) -> val < 1000) stock)
(1001.4,2008,9,4)
```

`span`{.label .function} é meio que como `takeWhile`, só que retorna um par de listas.
A primeira lista contém tudo o que a lista resultante de `takeWhile` conteria se fosse chamada com o mesmo predicado e a mesma lista.
A segunda lista contém a parte da lista que teria sido descartada.

```{.haskell:ghci}
ghci> let (fw, rest) = span (/=' ') "This is a sentence" in "First word:" ++ fw ++ ", the rest:" ++ rest
"First word: This, the rest: is a sentence"
```

Considerando que `span` abrange a lista enquanto o predicado é verdadeiro, `break`{.label .function} a quebra quando o predicado é verdadeiro.
Fazer `break p` é o equivalente a fazer `span (not . p)`.

```{.haskell:ghci}
ghci> break (==4) [1,2,3,4,5,6,7]
([1,2,3],[4,5,6,7])
ghci> span (/=4) [1,2,3,4,5,6,7]
([1,2,3],[4,5,6,7])
```

Ao usar `break`, a segunda lista no resultado começará com o primeiro elemento que satisfaz o predicado.

`sort`{.label .function} simplesmente classifica uma lista.
O tipo dos elementos na lista deve fazer parte da typeclass `Ord`, porque se os elementos de uma lista não puderem ser colocados em algum tipo de ordem, a lista não poderá ser classificada.

```{.haskell:ghci}
ghci> sort [8,5,3,2,1,6,4,2]
[1,2,2,3,4,5,6,8]
ghci> sort "This will be sorted soon"
"    Tbdeehiillnooorssstw"
```

`group`{.label .function} pega uma lista e agrupa elementos adjacentes em sublistas se eles forem iguais.

```{.haskell:ghci}
ghci> group [1,1,1,1,2,2,2,2,3,3,2,2,2,5,6,7]
[[1,1,1,1],[2,2,2,2],[3,3],[2,2,2],[5],[6],[7]]
```

Se classificarmos uma lista antes de agrupá-la, podemos descobrir quantas vezes cada elemento aparece na lista.

```{.haskell:ghci}
ghci> map (\l@(x:xs) -> (x,length l)) . group . sort $ [1,1,1,1,2,2,2,2,3,3,2,2,2,5,6,7]
[(1,4),(2,7),(3,2),(5,1),(6,1),(7,1)]
```

`inits`{.label .function} e `tails`{.label .function} são como `init` e `tail`, apenas aplicam recursivamente isso a uma lista até que não resta nada.
Observe.

```{.haskell:ghci}
ghci> inits "w00t"
["","w","w0","w00","w00t"]
ghci> tails "w00t"
["w00t","00t","0t","t",""]
ghci> let w = "w00t" in zip (inits w) (tails w)
[("","w00t"),("w","00t"),("w0","0t"),("w00","t"),("w00t","")]
```

Vamos usar uma dobra para implementar a pesquisa em uma lista por uma sublista.

```{.haskell:hs}
search :: (Eq a) => [a] -> [a] -> Bool
search needle haystack =
    let nlen = length needle
    in  foldl (\acc x -> if take nlen x == needle then True else acc) False (tails haystack)
```

Primeiro, chamamos `tails` com a lista na qual estamos pesquisando.
Então passamos por cima de cada cauda e vemos se ela começa com o que estamos procurando.

Com isso, na verdade, acabamos de fazer uma função que se comporta como `isInfixOf`{.label .function}.
`isInfixOf` procura uma sublista dentro de uma lista e retorna `True` se a sublista que estamos procurando estiver em algum lugar dentro da lista de destino.

```{.haskell:ghci}
ghci> "cat" `isInfixOf` "im a cat burglar"
True
ghci> "Cat" `isInfixOf` "im a cat burglar"
False
ghci> "cats" `isInfixOf` "im a cat burglar"
False
```

`isPrefixOf`{.label .function} e `isSuffixOf`{.label .function} procuram uma sublista no início e no final de uma lista, respectivamente.

```{.haskell:ghci}
ghci> "hey" `isPrefixOf` "hey there!"
True
ghci> "hey" `isPrefixOf` "oh hey there!"
False
ghci> "there!" `isSuffixOf` "oh hey there!"
True
ghci> "there!" `isSuffixOf` "oh hey there"
False
```

`elem`{.label .function} e `notElem`{.label .function} verificam se um elemento está ou não dentro de uma lista.

`partition`{.label .function} pega uma lista e um predicado e retorna um par de listas.
A primeira lista no resultado contém todos os elementos que satisfazem o predicado, a segunda contém todos os que não satisfazem.

```{.haskell:ghci}
ghci> partition (`elem` ['A'..'Z']) "BOBsidneyMORGANeddy"
("BOBMORGAN","sidneyeddy")
ghci> partition (>3) [1,3,5,6,3,2,1,0,3,7]
([5,6,7],[1,3,3,2,1,0,3])
```

É importante entender como isso é diferente de `span` e `break`:

```{.haskell:ghci}
ghci> span (`elem` ['A'..'Z']) "BOBsidneyMORGANeddy"
("BOB","sidneyMORGANeddy")
```

Enquanto `span` e `break` terminam quando encontram o primeiro elemento que não satisfaz e satisfaz o predicado, `partition` passa por toda a lista e a divide de acordo com o predicado.

`find`{.label .function} pega uma lista e um predicado e retorna o primeiro elemento que satisfaz o predicado.
Mas retorna esse elemento envolvido em um valor `Maybe`.
Estaremos cobrindo tipos de dados algébricos mais aprofundadamente no próximo capítulo, mas, por enquanto, é isso que você precisa saber: um valor `Maybe` pode ser `Just something` ou `Nothing`.
Assim como uma lista pode ser uma lista vazia ou uma lista com alguns elementos, um valor `Maybe` pode ser nenhum elemento ou um único elemento.
E como o tipo de uma lista de, digamos, inteiros é `[Int]`, o tipo de maybe ter um inteiro é `Maybe Int`.
De qualquer forma, vamos testar nossa função `find`.

```{.haskell:ghci}
ghci> find (>4) [1,2,3,4,5,6]
Just 5
ghci> find (>9) [1,2,3,4,5,6]
Nothing
ghci> :t find
find :: (a -> Bool) -> [a] -> Maybe a
```

Observe o tipo de `find`.
Seu resultado é `Maybe a`.
É como ter o tipo de `[a]`, apenas um valor do tipo `Maybe` pode conter nenhum elemento ou um elemento, enquanto uma lista pode conter nenhum elemento, um elemento ou vários elementos.

Lembre -se de quando estávamos procurando pela primeira vez que nossas ações ultrapassaram US $ 1.000.
Fizemos `head (dropWhile (\(val,y,m,d) -> val < 1000) stock)`.
Lembre-se de que `head` não é realmente seguro.
O que aconteceria se o nosso estoque nunca ultrapassasse US $ 1000?
Nossa aplicação de `dropWhile` retornaria uma lista vazia e obter o head de uma lista vazia resultaria em um erro.
No entanto, se reescrevêssemos isso como `find (\(val,y,m,d) -> val > 1000) stock`, estaríamos muito mais seguros.
Se nossas ações nunca ultrapassassem US $ 1000 (portanto, se nenhum elemento satisfaz o predicado), receberíamos um `Nothing`.
Mas se houvesse uma resposta válida nessa lista, receberíamos, digamos, `Just (1001.4,2008,9,4)`.

`elemIndex`{.label .function} é meio que como `elem`, só que não retorna um valor booleano.
Talvez retorne o índice do elemento que estamos procurando.
Se esse elemento não estiver em nossa lista, ele retornará `Nothing`.

```{.haskell:ghci}
ghci> :t elemIndex
elemIndex :: (Eq a) => a -> [a] -> Maybe Int
ghci> 4 `elemIndex` [1,2,3,4,5,6]
Just 3
ghci> 10 `elemIndex` [1,2,3,4,5,6]
Nothing
```

`elemIndices`{.label .function} é como `elemIndex`, só que retorna uma lista de índices, caso o elemento que estamos procurando apareça na nossa lista várias vezes.
Como estamos usando uma lista para representar os índices, não precisamos de um tipo `Maybe`, porque a falha pode ser representada como a lista vazia, que é muito sinônimo de `Nothing`.

```{.haskell:ghci}
ghci> ' ' `elemIndices` "Where are the spaces?"
[5,9,13]
```

`findIndex`{.label .function} é como find, mas talvez retorne o índice do primeiro elemento que satisfaz o predicado.
`findIndices`{.label .function} retorna os índices de todos os elementos que satisfazem o predicado na forma de uma lista.

```{.haskell:ghci}
ghci> findIndex (==4) [5,3,2,1,6,4]
Just 5
ghci> findIndex (==7) [5,3,2,1,6,4]
Nothing
ghci> findIndices (`elem` ['A'..'Z']) "Where Are The Caps?"
[0,6,10,14]
```

Já cobrimos `zip` e `zipWith`.
Observamos que eles compactam duas listas, em uma tupla ou com uma função binária (o que significa uma função que aceita dois parâmetros).
Mas e se quisermos compactar três listas?
Ou compactar três listas com uma função que aceita três parâmetros?
Bem, para isso, temos `zip3`{.label .function}, `zip4`{.label .function}, etc. e `zipWith3`{.label .function}, `zipWith4`{.label .function}, etc.
Essas variantes vão até 7.
Embora isso possa parecer uma gambiarra, funciona muito bem, porque não há muitas vezes em que você deseja compactar 8 listas juntas.
Também existe uma maneira muito inteligente de compactar números infinitos de listas, mas não somos avançados o suficiente para cobrir isso ainda.

```{.haskell:ghci}
ghci> zipWith3 (\x y z -> x + y + z) [1,2,3] [4,5,2,2] [2,2,3]
[7,9,8]
ghci> zip4 [2,3,3] [2,2,2] [5,5,3] [2,2,2]
[(2,2,5,2),(3,2,5,2),(3,2,3,2)]
```

Assim como no zipping normal, as listas mais longas que a lista mais curta que está sendo compactada são cortadas no tamanho.

`lines`{.label .function} é uma função útil ao lidar com arquivos ou entrada de algum lugar.
Pega uma string e retorna cada linha dessa string como elemento separado de uma lista.

```{.haskell:ghci}
ghci> lines "first line\nsecond line\nthird line"
["first line","second line","third line"]
```

`'\n'` é o caractere para uma nova linha unix.
As barras invertidas têm significado especial em strings e caracteres de Haskell.

`unlines`{.label .function} é a função inversa de `lines`.
Pega uma lista de strings e as une usando um `'\n'`.

```{.haskell:ghci}
ghci> unlines ["first line", "second line", "third line"]
"first line\nsecond line\nthird line\n"
```

`words`{.label .function} e `unwords`{.label .function} são para dividir uma linha de texto em palavras ou unir uma lista de palavras em um texto.
Muito útil.

```{.haskell:ghci}
ghci> words "hey these are the words in this sentence"
["hey","these","are","the","words","in","this","sentence"]
ghci> words "hey these           are    the words in this\nsentence"
["hey","these","are","the","words","in","this","sentence"]
ghci> unwords ["hey","there","mate"]
"hey there mate"
```

Já mencionamos `nub`{.label .function}.
Pega uma lista e elimina os elementos duplicados, retornando uma lista cujo elemento é um floco de neve único!
A função tem um nome meio estranho.
Acontece que "nub" significa um pequeno pedaço ou parte essencial de algo.
Na minha opinião, eles deveriam usar palavras reais para nomes de funções em vez de palavras de pessoas velhas.

```{.haskell:ghci}
ghci> nub [1,2,3,4,3,2,1,2,3,4,3,2,1]
[1,2,3,4]
ghci> nub "Lots of words and stuff"
"Lots fwrdanu"
```

`delete`{.label .function} pega um elemento e uma lista e exclui a primeira ocorrência desse elemento na lista.

```{.haskell:ghci}
ghci> delete 'h' "hey there ghang!"
"ey there ghang!"
ghci> delete 'h' . delete 'h' $ "hey there ghang!"
"ey tere ghang!"
ghci> delete 'h' . delete 'h' . delete 'h' $ "hey there ghang!"
"ey tere gang!"
```

`\\`{.label .function} é a função de diferença de lista.
Ele age como uma diferença de conjunto, basicamente.
Para cada elemento na lista do lado direito, ele remove um elemento correspondente na esquerda.

```{.haskell:ghci}
ghci> [1..10] \\ [2,5,9]
[1,3,4,6,7,8,10]
ghci> "Im a big baby" \\ "big"
"Im a  baby"
```

Fazer `[1..10] \\ [2,5,9]` é como fazer `delete 2 . delete 5 . delete 9 $ [1..10]`.

`union`{.label .function} também atua como uma função em conjuntos.
Ele retorna a união de duas listas.
Ele praticamente repassa todos os elementos da segunda lista e o acrescenta ao primeiro se ainda não estiver dentro.
Cuidado, porém, as duplicatas são removidas da segunda lista!

```{.haskell:ghci}
ghci> "hey man" `union` "man what's up"
"hey manwt'sup"
ghci> [1..7] `union` [5..10]
[1,2,3,4,5,6,7,8,9,10]
```

`intersect`{.label .function} funciona como interseção de conjuntos.
Ele retorna apenas os elementos encontrados nas duas listas.

```{.haskell:ghci}
ghci> [1..7] `intersect` [5..10]
[5,6,7]
```

`insert`{.label .function} pega um elemento e uma lista de elementos que podem ser classificados e o insere na última posição, onde ainda é menor ou igual ao próximo elemento.
Em outras palavras, `insert` começará no início da lista e continuará até encontrar um elemento igual ou maior que o elemento que estamos inserindo e o inserirá logo antes do elemento.

```{.haskell:ghci}
ghci> insert 4 [3,5,1,2,8,2]
[3,4,5,1,2,8,2]
ghci> insert 4 [1,3,4,4,1]
[1,3,4,4,4,1]
```

O `4` é inserido logo após o `3` e antes do `5` no primeiro exemplo e entre `3` e `4` no segundo exemplo.

Se usarmos `insert` para inserir em uma lista classificada, a lista resultante será mantida classificada.

```{.haskell:ghci}
ghci> insert 4 [1,2,3,5,6,7]
[1,2,3,4,5,6,7]
ghci> insert 'g' $ ['a'..'f'] ++ ['h'..'z']
"abcdefghijklmnopqrstuvwxyz"
ghci> insert 3 [1,2,4,3,2,1]
[1,2,3,4,3,2,1]
```

O que `length`, `take`, `drop`, `splitAt`, `!!` e `replicate` têm em comum é que eles tomam um `Int` como um de seus parâmetros (ou retornam um `Int`), embora pudessem ser mais genéricos e utilizáveis se apenas pegassem qualquer tipo que faça parte das typeclasses `Integral` ou `Num` (dependendo das funções).
Eles fazem isso por razões históricas.
No entanto, corrigir isso provavelmente quebraria muito código existente.
É por isso que `Data.List` tem seus equivalentes mais genéricos, chamado `genericLength`{.label .function}, `genericTake`{.label .function}, `genericDrop`{.label .function}, `genericSplitAt`{.label .function}, `genericIndex`{.label .function} e `genericReplicate`{.label .function}.
Por exemplo, `length` tem uma assinatura de tipo de `length :: [a] -> Int`.
Se tentarmos obter a média de uma lista de números fazendo `let xs = [1..6] in sum xs / length xs`, recebemos um erro de tipo, porque você não pode usar `/` com um `Int`.
`genericLength`, por outro lado, tem uma assinatura de tipo de `genericLength :: (Num a) => [b] -> a`.
Como um `Num` pode agir como um número de ponto flutuante, obter a média fazendo `let xs = [1..6] in sum xs / genericLength xs` funciona muito bem.

As funções `nub`, `delete`, `union`, `intersect` e `group` têm todas as suas contrapartes mais gerais chamadas `nubBy`{.label .function}, `deleteBy`{.label .function}, `unionBy`{.label .function}, `intersectBy`{.label .function} e `groupBy`{.label .function}.
A diferença entre eles é que o primeiro conjunto de funções usa `==` para testar a igualdade, enquanto as *By* também recebem uma função de igualdade e depois as comparam usando essa função de igualdade.
`group` é o mesmo que `groupBy (==)`.

Por exemplo, digamos que temos uma lista que descreve o valor de uma função a cada segundo.
Queremos segmentá-la em sublistas com base em quando o valor estava abaixo de zero e quando subiu.
Se fizéssemos apenas um `group` normal, ele apenas agruparia os valores adjacentes iguais.
Mas o que queremos é agrupá-los se eles são negativos ou não.
É aí que entra o `groupBy`!
A função de igualdade fornecida às funções *By* deve receber dois elementos do mesmo tipo e retornar `True` se os considerar iguais pelos seus padrões.

```{.haskell:ghci}
ghci> let values = [-4.3, -2.4, -1.2, 0.4, 2.3, 5.9, 10.5, 29.1, 5.3, -2.4, -14.5, 2.9, 2.3]
ghci> groupBy (\x y -> (x > 0) == (y > 0)) values
[[-4.3,-2.4,-1.2],[0.4,2.3,5.9,10.5,29.1,5.3],[-2.4,-14.5],[2.9,2.3]]
```

A partir disso, vemos claramente quais seções são positivas e quais são negativas.
A função de igualdade fornecida pega dois elementos e retorna `True` apenas se ambos forem negativos ou se ambos forem positivos.
Essa função de igualdade também pode ser escrita como `\x y -> (x > 0) && (y > 0) || (x <= 0) && (y <= 0)`, embora eu ache que a primeira maneira é mais legível.
Uma maneira ainda mais clara de escrever funções de igualdade para as funções *By* é se você importar a função `on`{.label .function} de `Data.Function`.
`on` é definido assim:

```{.haskell:ghci}
on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
f `on` g = \x y -> f (g x) (g y)
```

Portanto, fazer `(==) `on` (> 0)` retorna uma função de igualdade semelhante a `\x y -> (x > 0) == (y > 0)`.
`on` é usado muito com as funções *By* porque com ela, podemos fazer:

```{.haskell:ghci}
ghci> groupBy ((==) `on` (> 0)) values
[[-4.3,-2.4,-1.2],[0.4,2.3,5.9,10.5,29.1,5.3],[-2.4,-14.5],[2.9,2.3]]
```

Muito legível de fato!
Você pode ler em voz alta: agrupe isso por igualdade se os elementos forem maiores que zero.

Da mesma forma, `sort`, `insert`, `maximum` e `minimum` também têm seus equivalentes mais gerais.
Funções como `groupBy` pegam uma função que determina quando dois elementos são iguais.
`sortBy`{.label .function}, `insertBy`{.label .function}, `maximumBy`{.label .function} e `minimumBy`{.label .function} pegam uma função que determina se um elemento é maior, menor ou igual ao outro.
A assinatura de tipo de `sortBy` é `sortBy :: (a -> a -> Ordering) -> [a] -> [a]`.
Se você se lembra de antes, o tipo `Ordering` pode ter um valor de `LT`, `EQ` ou `GT`.
`sort` é o equivalente a `sortBy compare`, porque compare apenas pega dois elementos cujo tipo está na typeclass `Ord` e retorna seu relacionamento de pedidos.

Listas podem ser comparadas, mas quando são, são comparadas lexicograficamente.
E se tivermos uma lista de listas e queremos classificá-la não com base no conteúdo das listas internas, mas em seus comprimentos?
Bem, como você provavelmente adivinhou, usaremos a função `sortBy`.

```{.haskell:ghci}
ghci> let xs = [[5,4,5,4,4],[1,2,3],[3,5,4,3],[],[2],[2,2]]
ghci> sortBy (compare `on` length) xs
[[],[2],[2,2],[1,2,3],[3,5,4,3],[5,4,5,4,4]]
```

Incrível!
`compare `on` length` ... cara, isso lê quase como inglês real!
Se você não tem certeza de como exatamente o `on` funciona aqui, `compare `on` length` é o equivalente a `\x y -> length x `compare` length y`.
Quando você está lidando com funções *By* que assumem uma função de igualdade, geralmente faz `(==) `on` something` e quando está lidando com funções *By* que executam uma função de ordenação, geralmente faz `compare `on` something`.

## Data.Char {#data-char}

![lego char](assets/images/modules/legochar.png){.right width=230 height=323}

O módulo `Data.Char` faz o que o nome sugere.
Ele exporta funções que lidam com caracteres.
Também é útil ao filtrar e mapear strings, porque elas são apenas listas de caracteres.

`Data.Char` exporta um monte de predicados sobre caracteres.
Ou seja, funções que pegam um caractere e nos dizem se alguma suposição sobre ele é verdadeira ou falsa.
Aqui está o que eles são:

`isControl`{.label .function} verifica se um caractere é um caractere de controle.

`isSpace`{.label .function} verifica se um caractere é um caractere de espaço em branco.
Isso inclui espaços, caracteres de tabulação, novas linhas, etc.

`isLower`{.label .function} verifica se um caractere é minúsculo.

`isUpper`{.label .function} verifica se um caractere é maiúsculo.

`isAlpha`{.label .function} verifica se um caractere é uma letra.

`isAlphaNum`{.label .function} verifica se um caractere é uma letra ou um número.

`isPrint`{.label .function} verifica se um caractere é imprimível.
Caracteres de controle, por exemplo, não são imprimíveis.

`isDigit`{.label .function} verifica se um caractere é um dígito.

`isOctDigit`{.label .function} verifica se um caractere é um dígito octal.

`isHexDigit`{.label .function} verifica se um caractere é um dígito hexadecimal.

`isLetter`{.label .function} verifica se um caractere é uma letra.

`isMark`{.label .function} verifica se há caracteres de marca Unicode.
Esses são caracteres que combinam com as letras anteriores para formar letras com sotaques.
Use isso se você for francês.

`isNumber`{.label .function} verifica se um caractere é numérico.

`isPunctuation`{.label .function} verifica se um caractere é pontuação.

`isSymbol`{.label .function} verifica se um caractere é um símbolo matemático ou de moeda extravagante.

`isSeparator`{.label .function} verifica se há espaços e separadores Unicode.

`isAscii`{.label .function} verifica se um caractere cai nos primeiros 128 caracteres do conjunto de caracteres Unicode.

`isLatin1`{.label .function} verifica se um caractere cai nos primeiros 256 caracteres do Unicode.

`isAsciiUpper`{.label .function} verifica se um caractere é ASCII e maiúsculo.

`isAsciiLower`{.label .function} verifica se um caractere é ASCII e minúsculo.

Todos esses predicados têm uma assinatura de tipo de `Char -> Bool`.
Na maioria das vezes, você usará isso para filtrar strings ou algo assim.
Por exemplo, digamos que estamos fazendo um programa que pega um nome de usuário que consiste apenas em caracteres alfanuméricos.
Podemos usar a função `Data.List` `all` em combinação com os predicados `Data.Char` para determinar se o nome de usuário está bem.

```{.haskell:ghci}
ghci> all isAlphaNum "bobby283"
True
ghci> all isAlphaNum "eddy the fish!"
False
```

Kewl.
Caso você não se lembre, `all` pega um predicado e uma lista e retorna `True` apenas se esse predicado for válido para cada elemento da lista.

Também podemos usar `isSpace` para simular a função `Data.List` `words`.

```{.haskell:ghci}
ghci> words "hey folks its me"
["hey","folks","its","me"]
ghci> groupBy ((==) `on` isSpace) "hey folks its me"
["hey"," ","folks"," ","its"," ","me"]
ghci>
```

Hmmm, bem, isso faz o que `words` faz, mas ficamos com elementos de apenas espaços.
Hmm, o que faremos?
Eu sei, vamos filtrar esse otário.

```{.haskell:ghci}
ghci> filter (not . any isSpace) . groupBy ((==) `on` isSpace) $ "hey folks its me"
["hey","folks","its","me"]
```

Ah.

O `Data.Char` também exporta um tipo de dados que é meio que o `Ordering`.
O tipo `Ordering` pode ter um valor de `LT`, `EQ` ou `GT`.
É uma espécie de enumeração.
Ele descreve alguns resultados possíveis que podem surgir da comparação de dois elementos.
O tipo `GeneralCategory` também é uma enumeração.
Ele nos apresenta algumas categorias possíveis em que um caractere pode cair.
A principal função para obter a categoria geral de um caractere é `generalCategory`.
Tem um tipo de `generalCategory :: Char -> GeneralCategory`.
Existem cerca de 31 categorias, então não vamos listá-las todas aqui, mas vamos brincar com a função.

```{.haskell:ghci}
ghci> generalCategory ' '
Space
ghci> generalCategory 'A'
UppercaseLetter
ghci> generalCategory 'a'
LowercaseLetter
ghci> generalCategory '.'
OtherPunctuation
ghci> generalCategory '9'
DecimalNumber
ghci> map generalCategory " \t\nA9?|"
[Space,Control,Control,UppercaseLetter,DecimalNumber,OtherPunctuation,MathSymbol]
```

Como o tipo `GeneralCategory` faz parte da typeclass `Eq`, também podemos testar coisas como `generalCategory c == Space`.

`toUpper`{.label .function} converte um caractere em maiúsculas.
Espaços, números e afins permanecem inalterados.

`toLower`{.label .function} converte um caractere em minúsculas.

`toTitle`{.label .function} converte um caractere para title-case.
Para a maioria dos caracteres, title-case é o mesmo que maiúsculas.

`digitToInt`{.label .function} converte um caractere em um `Int`.
Para ter sucesso, o caractere deve estar nos intervalos `'0'..'9'`, `'a'..'f'` ou `'A'..'F'`.

```{.haskell:ghci}
ghci> map digitToInt "34538"
[3,4,5,3,8]
ghci> map digitToInt "FF85AB"
[15,15,8,5,10,11]
```

`intToDigit`{.label .function} é a função inversa de `digitToInt`.
Pega um `Int` na faixa de `0..15` e o converte em um caractere minúsculo.

```{.haskell:ghci}
ghci> intToDigit 15
'f'
ghci> intToDigit 5
'5'
```

As funções `ord`{.label .function} e `chr` convertem caracteres em seus números correspondentes e vice-versa:

```{.haskell:ghci}
ghci> ord 'a'
97
ghci> chr 97
'a'
ghci> map ord "abcdefgh"
[97,98,99,100,101,102,103,104]
```

A diferença entre os valores `ord` de dois caracteres é igual à distância entre eles na tabela Unicode.

A cifra de César é um método primitivo de codificar mensagens, deslocando cada caractere nelas por um número fixo de posições no alfabeto.
Podemos criar facilmente uma espécie de cifra de César, só que não nos restringiremos ao alfabeto.

```{.haskell:hs}
encode :: Int -> String -> String
encode shift msg =
    let ords = map ord msg
        shifted = map (+ shift) ords
    in  map chr shifted
```

Aqui, primeiro convertemos a string em uma lista de números.
Em seguida, adicionamos o valor de deslocamento a cada número antes de converter a lista de números de volta aos caracteres.
Se você é um cowboy de composição, pode escrever o corpo desta função como `map (chr . (+ shift) . ord) msg`.
Vamos tentar codificar algumas mensagens.

```{.haskell:ghci}
ghci> encode 3 "Heeeeey"
"Khhhhh|"
ghci> encode 4 "Heeeeey"
"Liiiii}"
ghci> encode 1 "abcd"
"bcde"
ghci> encode 5 "Marry Christmas! Ho ho ho!"
"Rfww~%Hmwnxyrfx&%Mt%mt%mt&"
```

Isso está codificado bem.
A decodificação de uma mensagem é basicamente apenas deslocá-la de volta pelo número de lugares que foi deslocado em primeiro lugar.

```{.haskell:hs}
decode :: Int -> String -> String
decode shift msg = encode (negate shift) msg
```

```{.haskell:ghci}
ghci> encode 3 "Im a little teapot"
"Lp#d#olwwoh#whdsrw"
ghci> decode 3 "Lp#d#olwwoh#whdsrw"
"Im a little teapot"
ghci> decode 5 . encode 5 $ "This is a sentence"
"This is a sentence"
```

## Data.Map {#data-map}

Listas de associação (também chamadas de dicionários) são listas usadas para armazenar pares de chave-valor onde a ordem não importa.
Por exemplo, podemos usar uma lista de associação para armazenar números de telefone, onde os números de telefone seriam os valores e os nomes das pessoas seriam as chaves.
Não nos importamos em que ordem eles são armazenados, apenas queremos obter o número de telefone certo para a pessoa certa.

A maneira mais óbvia de representar listas de associação em Haskell seria ter uma lista de pares.
O primeiro componente no par seria a chave, o segundo componente o valor.
Aqui está um exemplo de uma lista de associação com números de telefone:

```{.haskell:hs}
phoneBook =
    [("amelia","555-2938")
    ,("freya","452-2928")
    ,("isabella","493-2928")
    ,("neil","205-2928")
    ,("roald","939-8282")
    ,("tenzing","853-2492")
    ]
```

Apesar dessa indentação aparentemente estranha, essa é apenas uma lista de pares de strings.
A tarefa mais comum ao lidar com listas de associação é procurar algum valor por chave.
Vamos fazer uma função que procure algum valor dada uma chave.

```{.haskell:hs}
findKey :: (Eq k) => k -> [(k,v)] -> v
findKey key xs = snd . head . filter (\(k,v) -> key == k) $ xs
```

Bem simples.
A função que pega uma chave e uma lista, filtra a lista para que apenas as chaves correspondentes permaneçam, pega o primeiro par chave-valor que corresponde e retorna o valor.
Mas o que acontece se a chave que estamos procurando não estiver na lista de associação?
Hmm.
Aqui, se uma chave não estiver na lista de associação, acabaremos tentando obter a cabeça de uma lista vazia, o que gera um erro de tempo de execução.
No entanto, devemos evitar tornar nossos programas tão fáceis de travar; portanto, vamos usar o tipo de dados `Maybe`.
Se não encontrarmos a chave, retornaremos um `Nothing`.
Se o encontrarmos, retornaremos `Just something`, onde algo é o valor correspondente a essa chave.

```{.haskell:hs}
findKey :: (Eq k) => k -> [(k,v)] -> Maybe v
findKey key [] = Nothing
findKey key ((k,v):xs) = if key == k
                            then Just v
                            else findKey key xs
```

Olhe para a declaração de tipo.
Ele pega uma chave que pode ser equiparada, uma lista de associação e talvez produz um valor.
Parece certo.

Esta é uma função recursiva de livro didático que opera em uma lista.
Caso de borda, dividindo uma lista em uma cabeça e uma cauda, chamadas recursivas, estão todas lá.
Esse é o padrão clássico de dobra, então vamos ver como isso seria implementado como uma dobra.

```{.haskell:hs}
findKey :: (Eq k) => k -> [(k,v)] -> Maybe v
findKey key = foldr (\(k,v) acc -> if key == k then Just v else acc) Nothing
```

::: {.hintbox}
**Nota:** Geralmente, é melhor usar dobras para esse padrão de recursão de lista padrão, em vez de escrever explicitamente a recursão, porque são mais fáceis de ler e identificar.
Todo mundo sabe que é uma dobra quando vê a chamada `foldr`, mas é preciso pensar um pouco mais para ler a recursão explícita.
:::

```{.haskell:ghci}
ghci> findKey "tenzing" phoneBook
Just "853-2492"
ghci> findKey "amelia" phoneBook
Just "555-2938"
ghci> findKey "christopher" phoneBook
Nothing
```

![legomap](assets/images/modules/legomap.png){.left width=214 height=240}

Funciona como um encanto!
Se tivermos o número de telefone do amigo, `Just` obtemos o número, caso contrário, obtemos `Nothing`.

Acabamos de implementar a função `lookup` de `Data.List`.
Se quisermos encontrar o valor correspondente a uma chave, temos que percorrer todos os elementos da lista até encontrá-la.
O módulo `Data.Map` oferece listas de associação muito mais rápidas (porque são implementadas internamente com árvores) e também fornece muitas funções utilitárias.
A partir de agora, diremos que estamos trabalhando com mapas (maps) em vez de listas de associação.

Como o `Data.Map` exporta funções que conflitam com as do `Prelude` e `Data.List`, faremos uma importação qualificada.

```{.haskell:hs}
import qualified Data.Map as Map
```

Coloque esta declaração de importação em um script e, em seguida, carregue o script via GHCI.

Vamos em frente e ver o que `Data.Map` tem reservado para nós!
Aqui está o resumo básico de suas funções.

A função `fromList`{.label .function} pega uma lista de associação (na forma de uma lista) e retorna um mapa com as mesmas associações.

```{.haskell:ghci}
ghci> Map.fromList [("amelia","555-2938"),("freya","452-2928"),("neil","205-2928")]
fromList [("amelia","555-2938"),("freya","452-2928"),("neil","205-2928")]
ghci> Map.fromList [(1,2),(3,4),(3,2),(5,5)]
fromList [(1,2),(3,2),(5,5)]
```

Se houver chaves duplicadas na lista de associação original, as duplicatas serão descartadas.
Esta é a assinatura de tipo de `fromList`

```{.haskell:hs}
Map.fromList :: (Ord k) => [(k, v)] -> Map.Map k v
```

Ele diz que pega uma lista de pares do tipo `k` e `v` e retorna um mapa que mapeia de chaves do tipo `k` para o tipo `v`.
Observe que, quando estávamos fazendo listas de associação com listas normais, as chaves só precisavam ser equiparáveis (seu tipo pertencente à typeclass `Eq`), mas agora elas precisam ser ordenáveis.
Essa é uma restrição essencial no módulo `Data.Map`.
Ele precisa que as chaves sejam ordenáveis para que possa organizá-las em uma árvore.

Você deve sempre usar `Data.Map` para associações de valores-chave, a menos que tenha chaves que não fazem parte da typeclass `Ord`.

`empty`{.label .function} representa um mapa vazio.
Não aceita argumentos, apenas retorna um mapa vazio.

```{.haskell:ghci}
ghci> Map.empty
fromList []
```

`insert`{.label .function} pega uma chave, um valor e um mapa e retorna um novo mapa que é exatamente como o antigo, apenas com a chave e o valor inseridos.

```{.haskell:ghci}
ghci> Map.empty
fromList []
ghci> Map.insert 3 100 Map.empty
fromList [(3,100)]
ghci> Map.insert 5 600 (Map.insert 4 200 ( Map.insert 3 100  Map.empty))
fromList [(3,100),(4,200),(5,600)]
ghci> Map.insert 5 600 . Map.insert 4 200 . Map.insert 3 100 $ Map.empty
fromList [(3,100),(4,200),(5,600)]
```

Podemos implementar nosso próprio `fromList` usando o mapa vazio, `insert` e uma dobra.
Assista:

```{.haskell:ghci}
fromList' :: (Ord k) => [(k,v)] -> Map.Map k v
fromList' = foldr (\(k,v) acc -> Map.insert k v acc) Map.empty
```

É uma dobra bastante direta.
Começamos com um mapa vazio e o dobramos da direita, inserindo os pares de valores-chave no acumulador à medida que avançamos.

`null`{.label .function} verifica se um mapa está vazio.

```{.haskell:ghci}
ghci> Map.null Map.empty
True
ghci> Map.null $ Map.fromList [(2,3),(5,5)]
False
```

`size`{.label .function} relata o tamanho de um mapa.

```{.haskell:ghci}
ghci> Map.size Map.empty
0
ghci> Map.size $ Map.fromList [(2,4),(3,3),(4,2),(5,4),(6,4)]
5
```

`singleton`{.label .function} pega uma chave e um valor e cria um mapa que possui exatamente um mapeamento.

```{.haskell:ghci}
ghci> Map.singleton 3 9
fromList [(3,9)]
ghci> Map.insert 5 9 $ Map.singleton 3 9
fromList [(3,9),(5,9)]
```

`lookup`{.label .function} funciona como o `lookup` de `Data.List`, apenas opera em mapas.
Ele retorna `Just something` se encontrar algo para a chave e `Nothing` se não encontrar.

`member`{.label .function} é um predicado que pega uma chave e um mapa e relata se a chave está no mapa ou não.

```{.haskell:ghci}
ghci> Map.member 3 $ Map.fromList [(3,6),(4,3),(6,9)]
True
ghci> Map.member 3 $ Map.fromList [(2,5),(4,5)]
False
```

`map`{.label .function} e `filter`{.label .function} funcionam muito como seus equivalentes de lista.

```{.haskell:ghci}
ghci> Map.map (*100) $ Map.fromList [(1,1),(2,4),(3,9)]
fromList [(1,100),(2,400),(3,900)]
ghci> Map.filter isUpper $ Map.fromList [(1,'a'),(2,'A'),(3,'b'),(4,'B')]
fromList [(2,'A'),(4,'B')]
```

`toList`{.label .function} é o inverso de `fromList`.

```{.haskell:ghci}
ghci> Map.toList . Map.insert 9 2 $ Map.singleton 4 3
[(4,3),(9,2)]
```

`keys`{.label .function} e `elems`{.label .function} retornam listas de chaves e valores, respectivamente.
`keys` é o equivalente a `map fst . Map.toList` e `elems` é o equivalente a `map snd . Map.toList`.

`fromListWith`{.label .function} é uma pequena função legal.
Ele age como `fromList`, mas não descarta chaves duplicadas, mas usa uma função fornecida a ele para decidir o que fazer com elas.
Digamos que um amigo possa ter vários números e tenhamos uma lista de associação configurada assim.

```{.haskell:hs}
phoneBook =
    [("amelia","555-2938")
    ,("amelia","342-2492")
    ,("freya","452-2928")
    ,("isabella","493-2928")
    ,("isabella","943-2929")
    ,("isabella","827-9162")
    ,("neil","205-2928")
    ,("roald","939-8282")
    ,("tenzing","853-2492")
    ,("tenzing","555-2111")
    ]
```

Agora, se apenas usarmos `fromList` para colocar isso em um mapa, perderemos alguns números!
Então, aqui está o que faremos:

```{.haskell:hs}
phoneBookToMap :: (Ord k) => [(k, String)] -> Map.Map k String
phoneBookToMap xs = Map.fromListWith (\number1 number2 -> number1 ++ ", " ++ number2) xs
```

```{.haskell:hs}
ghci> Map.lookup "isabella" $ phoneBookToMap phoneBook
"827-9162, 943-2929, 493-2928"
ghci> Map.lookup "roald" $ phoneBookToMap phoneBook
"939-8282"
ghci> Map.lookup "amelia" $ phoneBookToMap phoneBook
"342-2492, 555-2938"
```

Se uma chave duplicada for encontrada, a função que passamos será usada para combinar os valores dessas chaves em algum outro valor.
Também poderíamos fazer com que todos os valores na lista de associação fossem listas singleton e, em seguida, podemos usar `++` para combinar os números.

```{.haskell:hs}
phoneBookToMap :: (Ord k) => [(k, a)] -> Map.Map k [a]
phoneBookToMap xs = Map.fromListWith (++) $ map (\(k,v) -> (k,[v])) xs
```

```{.haskell:ghci}
ghci> Map.lookup "isabella" $ phoneBookToMap phoneBook
["827-9162","943-2929","493-2928"]
```

Muito legal!
Outro caso de uso é se estamos criando um mapa a partir de uma lista de associação de números e, quando uma chave duplicada é encontrada, queremos que o maior valor da chave seja mantido.

```{.haskell:ghci}
ghci> Map.fromListWith max [(2,3),(2,5),(2,100),(3,29),(3,22),(3,11),(4,22),(4,15)]
fromList [(2,100),(3,29),(4,22)]
```

Ou poderíamos optar por somar valores nas mesmas chaves.

```{.haskell:ghci}
ghci> Map.fromListWith (+) [(2,3),(2,5),(2,100),(3,29),(3,22),(3,11),(4,22),(4,15)]
fromList [(2,108),(3,62),(4,37)]
```

`insertWith`{.label .function} está para `insert` assim como `fromListWith` está para `fromList`.
Ele insere um par de chave-valor em um mapa, mas se esse mapa já contiver a chave, ele usa a função passada para determinar o que fazer.

```{.haskell:ghci}
ghci> Map.insertWith (+) 3 100 $ Map.fromList [(3,4),(5,103),(6,339)]
fromList [(3,104),(5,103),(6,339)]
```

Essas foram apenas algumas funções do `Data.Map`.
Você pode ver uma lista completa de funções na [documentação](https://hackage.haskell.org/package/containers/docs/Data-Map.html).

## Data.Set {#data-set}

![legosets](assets/images/modules/legosets.png){.right width=150 height=236}

O módulo `Data.Set` nos oferece, bem, conjuntos.
Como conjuntos da matemática.
Conjuntos são como um cruzamento entre listas e mapas.
Todos os elementos de um conjunto são únicos.
E como eles são implementados internamente com árvores (como mapas em `Data.Map`), eles são ordenados.
Verificar a associação, inserir, excluir etc. é muito mais rápido do que fazer a mesma coisa com as listas.
A operação mais comum ao lidar com conjuntos é inserir em um conjunto, verificar a associação e converter um conjunto em uma lista.

Como os nomes em `Data.Set` entram em conflito com muitos nomes de `Prelude` e `Data.List`, fazemos uma importação qualificada.

Coloque esta declaração de importação em um script:

```{.haskell:ghci}
import qualified Data.Set as Set
```

E então carregue o script via GHCI.

Digamos que temos dois pedaços de texto.
Queremos descobrir quais caracteres foram usados em ambos.

```{.haskell:ghci}
text1 = "I just had an anime dream. Anime... Reality... Are they so different?"
text2 = "The old man left his garbage can out and now his trash is all over my lawn!"
```

A função `fromList`{.label .function} funciona muito como você esperaria.
Pega uma lista e a converte em um conjunto.

```{.haskell:ghci}
ghci> let set1 = Set.fromList text1
ghci> let set2 = Set.fromList text2
ghci> set1
fromList " .?AIRadefhijlmnorstuy"
ghci> set2
fromList " !Tabcdefghilmnorstuvwy"
```

Como você pode ver, os itens são ordenados e cada elemento é único.
Agora vamos usar a função `intersection`{.label .function} para ver quais elementos ambos compartilham.

```{.haskell:ghci}
ghci> Set.intersection set1 set2
fromList " adefhilmnorstuy"
```

Podemos usar a função `difference`{.label .function} para ver quais letras estão no primeiro conjunto, mas não estão no segundo e vice-versa.

```{.haskell:ghci}
ghci> Set.difference set1 set2
fromList ".?AIRj"
ghci> Set.difference set2 set1
fromList "!Tbcgvw"
```

Ou podemos ver todas as letras únicas usadas nas duas frases usando `union`{.label .function}.

```{.haskell:ghci}
ghci> Set.union set1 set2
fromList " !.?AIRTabcdefghijlmnorstuvwy"
```

As funções `null`{.label .function}, `size`{.label .function}, `member`{.label .function}, `empty`{.label .function}, `singleton`{.label .function}, `insert`{.label .function} e `delete`{.label .function} funcionam todas como você esperaria.

```{.haskell:ghci}
ghci> Set.null Set.empty
True
ghci> Set.null $ Set.fromList [3,4,5,5,4,3]
False
ghci> Set.size $ Set.fromList [3,4,5,3,4,5]
3
ghci> Set.singleton 9
fromList [9]
ghci> Set.insert 4 $ Set.fromList [9,3,8,1]
fromList [1,3,4,8,9]
ghci> Set.insert 8 $ Set.fromList [5..10]
fromList [5,6,7,8,9,10]
ghci> Set.delete 4 $ Set.fromList [3,4,5,4,3,4,5]
fromList [3,5]
```

Também podemos verificar subconjuntos ou subconjunto próprio.
O conjunto A é um subconjunto do conjunto B se B contiver todos os elementos que A possui.
O conjunto A é um subconjunto próprio do conjunto B se B contiver todos os elementos que A possui, mas tiver mais elementos.

```{.haskell:ghci}
ghci> Set.fromList [2,3,4] `Set.isSubsetOf` Set.fromList [1,2,3,4,5]
True
ghci> Set.fromList [1,2,3,4,5] `Set.isSubsetOf` Set.fromList [1,2,3,4,5]
True
ghci> Set.fromList [1,2,3,4,5] `Set.isProperSubsetOf` Set.fromList [1,2,3,4,5]
False
ghci> Set.fromList [2,3,4,8] `Set.isSubsetOf` Set.fromList [1,2,3,4,5]
False
```

Também podemos mapear `map`{.label .function} sobre conjuntos e filtrar `filter`{.label .function} eles.

```{.haskell:ghci}
ghci> Set.filter odd $ Set.fromList [3,4,5,6,7,2,3,4]
fromList [3,5,7]
ghci> Set.map (+1) $ Set.fromList [3,4,5,6,7,2,3,4]
fromList [3,4,5,6,7,8]
```

Os conjuntos são frequentemente usados para eliminar uma lista de duplicatas de uma lista, transformando-a primeiro em um conjunto com `fromList` e depois convertendo-o de volta para uma lista com `toList`{.label .function}.
A função `Data.List` `nub` já faz isso, mas eliminar duplicatas para listas grandes é muito mais rápido se você as colocar em um conjunto e depois convertê-las novamente em uma lista do que usar o `nub`.
Mas usar `nub` exige apenas que o tipo dos elementos da lista faça parte da typeclass `Eq`, ao passo que, se você deseja colocar elementos em um conjunto, o tipo da lista deve estar em `Ord`.

```{.haskell:ghci}
ghci> let setNub xs = Set.toList $ Set.fromList xs
ghci> setNub "HEY WHATS CRACKALACKIN"
" ACEHIKLNRSTWY"
ghci> nub "HEY WHATS CRACKALACKIN"
"HEY WATSCRKLIN"
```

`setNub` geralmente é mais rápido que `nub` em grandes listas, mas como você pode ver, `nub` preserva a ordem dos elementos da lista, enquanto `setNub` não.

## Fazendo nossos próprios módulos (Making our own modules) {#making-our-own-modules}

![making modules](assets/images/modules/making_modules.png){.right width=345 height=224}

Vimos alguns módulos legais até agora, mas como fazemos nosso próprio módulo?
Quase todas as linguagens de programação permitem dividir seu código em vários arquivos e o Haskell não é diferente.
Ao fazer programas, é uma boa prática pegar funções e tipos que trabalham para um objetivo semelhante e colocá-los em um módulo.
Dessa forma, você pode reutilizar facilmente essas funções em outros programas apenas importando seu módulo.

Vamos ver como podemos criar nossos próprios módulos criando um pequeno módulo que fornece algumas funções para calcular o volume e a área de alguns objetos geométricos.
Começaremos criando um arquivo chamado `Geometry.hs`.

Dizemos que um módulo *exporta* funções.
O que isso significa é que, quando importo um módulo, posso usar as funções que ele exporta.
Ele pode definir funções que suas funções chamam internamente, mas só podemos ver e usar as que ele exporta.

No início de um módulo, especificamos o nome do módulo.
Se tivermos um arquivo chamado `Geometry.hs`, devemos nomear nosso módulo `Geometry`.
Em seguida, especificamos as funções que ele exporta e, depois disso, podemos começar a escrever as funções.
Então começaremos com isso.

```{.haskell:ghci}
module Geometry
( sphereVolume
, sphereArea
, cubeVolume
, cubeArea
, cuboidArea
, cuboidVolume
) where
```

Como você pode ver, faremos áreas e volumes para esferas, cubos e cubóides.
Vamos em frente e definir nossas funções então:

```{.haskell:ghci}
module Geometry
( sphereVolume
, sphereArea
, cubeVolume
, cubeArea
, cuboidArea
, cuboidVolume
) where

sphereVolume :: Float -> Float
sphereVolume radius = (4.0 / 3.0) * pi * (radius ^ 3)

sphereArea :: Float -> Float
sphereArea radius = 4 * pi * (radius ^ 2)

cubeVolume :: Float -> Float
cubeVolume side = cuboidVolume side side side

cubeArea :: Float -> Float
cubeArea side = cuboidArea side side side

cuboidVolume :: Float -> Float -> Float -> Float
cuboidVolume a b c = rectangleArea a b * c

cuboidArea :: Float -> Float -> Float -> Float
cuboidArea a b c = rectangleArea a b * 2 + rectangleArea a c * 2 + rectangleArea c b * 2

rectangleArea :: Float -> Float -> Float
rectangleArea a b = a * b
```

Geometria bastante padrão aqui.
Há algumas coisas a serem observadas.
Como um cubo é apenas um caso especial de cubóide, definimos sua área e volume, tratando-o como um cubóide cujos lados são todos do mesmo comprimento.
Também definimos uma função auxiliar chamada `rectangleArea`, que calcula a área de um retângulo com base no comprimento de seus lados.
É bastante trivial, porque é apenas multiplicação.
Observe que o usamos em nossas funções no módulo (a saber `cuboidArea` e `cuboidVolume`), mas não o exportamos!
Como queremos que nosso módulo apresente apenas funções para lidar com objetos tridimensionais, usamos `rectangleArea`, mas não o exportamos.

Ao criar um módulo, geralmente exportamos apenas as funções que atuam como uma espécie de interface para o nosso módulo, para que a implementação seja ocultada.
Se alguém estiver usando nosso módulo `Geometry`, não precisará se preocupar com funções que não exportamos.
Podemos decidir mudar essas funções completamente ou excluí-las em uma versão mais recente (poderíamos excluir `rectangleArea` e apenas usar `*` em vez disso) e ninguém se importará, porque não as exportávamos em primeiro lugar.

Para usar nosso módulo, basta fazer:

```{.haskell:ghci}
import Geometry
```

`Geometry.hs` deve estar na mesma pasta em que está o programa que o importa.

Os módulos também podem ter uma estrutura hierárquica.
Cada módulo pode ter vários submódulos e eles podem ter seus próprios submódulos.
Vamos separar essas funções para que `Geometry` seja um módulo que tenha três submódulos, um para cada tipo de objeto.

Primeiro, faremos uma pasta chamada `Geometry`.
Cuidado com o G maiúsculo.
Nele, colocaremos três arquivos: `Sphere.hs`, `Cuboid.hs` e `Cube.hs`.
Aqui está o que os arquivos conterão:

`Sphere.hs`

```{.haskell:ghci}
module Geometry.Sphere
( volume
, area
) where

volume :: Float -> Float
volume radius = (4.0 / 3.0) * pi * (radius ^ 3)

area :: Float -> Float
area radius = 4 * pi * (radius ^ 2)
```

`Cuboid.hs`

```{.haskell:ghci}
module Geometry.Cuboid
( volume
, area
) where

volume :: Float -> Float -> Float -> Float
volume a b c = rectangleArea a b * c

area :: Float -> Float -> Float -> Float
area a b c = rectangleArea a b * 2 + rectangleArea a c * 2 + rectangleArea c b * 2

rectangleArea :: Float -> Float -> Float
rectangleArea a b = a * b
```

`Cube.hs`

```{.haskell:ghci}
module Geometry.Cube
( volume
, area
) where

import qualified Geometry.Cuboid as Cuboid

volume :: Float -> Float
volume side = Cuboid.volume side side side

area :: Float -> Float
area side = Cuboid.area side side side
```

Tudo certo!
Então, primeiro é `Geometry.Sphere`.
Observe como o colocamos em uma pasta chamada `Geometry` e depois definimos o nome do módulo como `Geometry.Sphere`.
Fizemos o mesmo para o cubóide.
Observe também como nos três submódulos definimos funções com os mesmos nomes.
Podemos fazer isso porque são módulos separados.
Queremos usar funções de `Geometry.Cuboid` em `Geometry.Cube`, mas não podemos simplesmente fazer `import Geometry.Cuboid` porque exporta funções com os mesmos nomes que `Geometry.Cube`.
É por isso que fazemos uma importação qualificada e está tudo bem.

Então, agora, se estivermos em um arquivo que está no mesmo nível que a pasta `Geometry`, podemos fazer, digamos:

```{.haskell:ghci}
import Geometry.Sphere
```

E então podemos chamar `area` e `volume` e eles nos darão a área e o volume para uma esfera.
E se quisermos fazer malabarismos com dois ou mais desses módulos, temos que fazer importações qualificadas porque eles exportam funções com os mesmos nomes.
Então, fazemos algo como:

```{.haskell:ghci}
import qualified Geometry.Sphere as Sphere
import qualified Geometry.Cuboid as Cuboid
import qualified Geometry.Cube as Cube
```

E então podemos chamar `Sphere.area`, `Sphere.volume`, `Cuboid.area`, etc. e cada um calculará a área ou o volume para seu objeto correspondente.

Na próxima vez que você se encontrar escrevendo um arquivo realmente grande e com muitas funções, tente ver quais funções atendem a algum objetivo comum e depois veja se você pode colocá-las em seu próprio módulo.
Você poderá importar seu módulo na próxima vez em que estiver escrevendo um programa que requer a mesma funcionalidade.


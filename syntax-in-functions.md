# Sintaxe em Funções

## Pattern matching (Correspondência de Padrões) {#pattern-matching}

![four!](assets/images/syntax-in-functions/pattern.png){.right width=162 height=250}

Este capítulo cobrirá algumas das construções sintáticas legais do Haskell e começaremos com pattern matching.
Pattern matching consiste em especificar padrões aos quais alguns dados devem estar em conformidade e, em seguida, verificar se eles estão e desconstruir os dados de acordo com esses padrões.

Ao definir funções, você pode definir corpos de função separados para diferentes padrões.
Isso leva a um código muito limpo, simples e legível.
Você pode fazer pattern matching em qualquer tipo de dado --- números, caracteres, listas, tuplas, etc.
Vamos fazer uma função realmente trivial que verifica se o número que fornecemos a ela é um sete ou não.

```{.haskell:hs}
lucky :: (Integral a) => a -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky x = "Sorry, you're out of luck, pal!"
```

Quando você chama `lucky`, os padrões serão verificados de cima para baixo e, quando estiver em conformidade com um padrão, o corpo da função correspondente será usado.
A única maneira de um número estar em conformidade com o primeiro padrão aqui é se for 7.
Se não for, ele cai para o segundo padrão, que corresponde a qualquer coisa e o vincula a `x`.
Esta função também poderia ter sido implementada usando uma instrução if.
Mas e se quiséssemos uma função que diga os números de 1 a 5 e diga `"Not between 1 and 5"` para qualquer outro número?
Sem pattern matching, teríamos que fazer uma árvore if then else bastante complicada.
No entanto, com ele:

```{.haskell:hs}
sayMe :: (Integral a) => a -> String
sayMe 1 = "One!"
sayMe 2 = "Two!"
sayMe 3 = "Three!"
sayMe 4 = "Four!"
sayMe 5 = "Five!"
sayMe x = "Not between 1 and 5"
```

Observe que, se movêssemos o último padrão (o padrão catch-all) para o topo, ele sempre diria `"Not between 1 and 5"`, porque pegaria todos os números e eles não teriam a chance de cair e ser verificados por outros padrões.

Lembra da função fatorial que implementamos anteriormente?
Definimos o fatorial de um número `n` como `product [1..n]`.
Também podemos definir uma função fatorial *recursivamente*, da maneira como é geralmente definida em matemática.
Começamos dizendo que o fatorial de 0 é 1.
Em seguida, afirmamos que o fatorial de qualquer número inteiro positivo é esse número inteiro multiplicado pelo fatorial de seu antecessor.
Veja como isso se parece traduzido em termos de Haskell.

```{.haskell:hs}
factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)
```

Esta é a primeira vez que definimos uma função recursivamente.
A recursão é importante em Haskell e daremos uma olhada mais de perto nela mais tarde.
Mas, em poucas palavras, é isso que acontece se tentarmos obter o fatorial de, digamos, 3.
Ele tenta calcular `3 * factorial 2`.
O fatorial de 2 é `2 * factorial 1`, então, por enquanto, temos `3 * (2 * factorial 1)`.
`factorial 1` é `1 * factorial 0`, então temos `3 * (2 * (1 * factorial 0))`.
Agora vem o truque --- definimos o fatorial de 0 como apenas 1 e, como ele encontra esse padrão antes do catch-all, ele apenas retorna 1.
Portanto, o resultado final é equivalente a `3 * (2 * (1 * 1))`.
Se tivéssemos escrito o segundo padrão em cima do primeiro, ele pegaria todos os números, incluindo 0, e nosso cálculo nunca terminaria.
É por isso que a ordem é importante ao especificar padrões e é sempre melhor especificar os mais específicos primeiro e depois os mais gerais mais tarde.

O pattern matching também pode falhar.
Se definirmos uma função como esta:

```{.haskell:hs}
charName :: Char -> String
charName 'a' = "Albert"
charName 'b' = "Broseph"
charName 'c' = "Cecil"
```

e depois tentarmos chamá-la com uma entrada que não esperávamos, é isso que acontece:

```{.haskell: .ghci}
ghci> charName 'a'
"Albert"
ghci> charName 'b'
"Broseph"
ghci> charName 'h'
"*** Exception: tut.hs:(53,0)-(55,21): Non-exhaustive patterns in function charName
```

Ele reclama que temos padrões não exaustivos, e com razão.
Ao criar padrões, devemos sempre incluir um padrão catch-all para que nosso programa não falhe se recebermos alguma entrada inesperada.

O pattern matching também pode ser usado em tuplas.
E se quiséssemos fazer uma função que pega dois vetores em um espaço 2D (que estão na forma de pares) e os soma?
Para somar dois vetores, somamos seus componentes x separadamente e depois seus componentes y separadamente.
Veja como teríamos feito se não soubéssemos sobre pattern matching:

```{.haskell:hs}
addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors a b = (fst a + fst b, snd a + snd b)
```

Bem, isso funciona, mas há uma maneira melhor de fazer isso.
Vamos modificar a função para que ela use pattern matching.

```{.haskell:hs}
addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)
```

Aí está!
Muito melhor.
Observe que este já é um padrão catch-all.
O tipo de `addVectors` (em ambos os casos) é `addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)`, portanto, temos a garantia de obter dois pares como parâmetros.

`fst` e `snd` extraem os componentes de pares.
Mas e os trios?
Bem, não há funções fornecidas que façam isso, mas podemos fazer as nossas.

```{.haskell:hs}
first :: (a, b, c) -> a
first (x, _, _) = x

second :: (a, b, c) -> b
second (_, y, _) = y

third :: (a, b, c) -> c
third (_, _, z) = z
```

O `_` significa a mesma coisa que nas compreensões de lista.
Isso significa que realmente não nos importamos com o que é essa parte, então apenas escrevemos um `_`.

O que me lembra, você também pode usar pattern matching em compreensões de lista.
Confira isso:

```{.haskell: .ghci}
ghci> let xs = [(1,3), (4,3), (2,4), (5,3), (5,6), (3,1)]
ghci> [a+b | (a,b) <- xs]
[4,7,6,8,11,4]
```

Se um pattern match falhar, ele apenas passará para o próximo elemento.

As próprias listas também podem ser usadas em pattern matching.
Você pode combinar com a lista vazia `[]` ou qualquer padrão que envolva `:` e a lista vazia.
Mas como `[1,2,3]` é apenas açúcar sintático para `1:2:3:[]`, você também pode usar o padrão anterior.
Um padrão como `x:xs` vinculará a cabeça da lista a `x` e o restante dela a `xs`, mesmo que haja apenas um elemento, de modo que `xs` acabe sendo uma lista vazia.

::: {.hintbox}
**Nota**: O padrão `x:xs` é muito usado, especialmente com funções recursivas.
Mas os padrões que têm `:` neles correspondem apenas a listas de comprimento 1 ou mais.
:::

Se você quiser vincular, digamos, os três primeiros elementos a variáveis e o restante da lista a outra variável, poderá usar algo como `x:y:z:zs`.
Ele corresponderá apenas a listas que tenham três elementos ou mais.

Agora que sabemos como fazer pattern matching em listas, vamos fazer nossa própria implementação da função `head`.

```{.haskell:hs}
head' :: [a] -> a
head' [] = error "Can't call head on an empty list, dummy!"
head' (x:_) = x
```

Verificando se funciona:

```{.haskell: .ghci}
ghci> head' [4,5,6]
4
ghci> head' "Hello"
'H'
```

Legal!
Observe que se você quiser vincular a várias variáveis (mesmo se uma delas for apenas `_` e não vincular a nada), temos que cercá-las com parênteses.
Observe também a função `error` que usamos.
Ele pega uma string e gera um erro de tempo de execução, usando essa string como informação sobre que tipo de erro ocorreu.
Faz o programa travar, então não é bom usá-lo muito.
Mas chamar `head` em uma lista vazia não faz sentido.

Vamos fazer uma função trivial que nos diz alguns dos primeiros elementos da lista em inglês (in)conveniente.

```{.haskell:hs}
tell :: (Show a) => [a] -> String
tell [] = "The list is empty"
tell (x:[]) = "The list has one element: " ++ show x
tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y
tell (x:y:_) = "This list is long. The first two elements are: " ++ show x ++ " and " ++ show y
```

Esta função é segura porque cuida da lista vazia, uma lista singleton, uma lista com dois elementos e uma lista com mais de dois elementos.
Observe que `(x:[])` e `(x:y:[])` podem ser reescritos como `[x]` e `[x,y]` (porque é açúcar sintático, não precisamos dos parênteses).
Não podemos reescrever `(x:y:_)` com colchetes porque corresponde a qualquer lista de comprimento 2 ou mais.

Já implementamos nossa própria função `length` usando compreensão de lista.
Agora faremos isso usando pattern matching e um pouco de recursão:

```{.haskell:hs}
length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs
```

Isso é semelhante à função fatorial que escrevemos anteriormente.
Primeiro, definimos o resultado de uma entrada conhecida --- a lista vazia.
Isso também é conhecido como condição de borda (edge condition).
Então, no segundo padrão, desmontamos a lista dividindo-a em cabeça e cauda.
Dizemos que o comprimento é igual a 1 mais o comprimento da cauda.
Usamos `_` para combinar a cabeça porque não nos importamos com o que ela é.
Observe também que cuidamos de todos os padrões possíveis de uma lista.
O primeiro padrão corresponde a uma lista vazia e o segundo corresponde a qualquer coisa que não seja uma lista vazia.

Vamos ver o que acontece se chamarmos `length'` em `"ham"`.
Primeiro, ele verificará se é uma lista vazia.
Como não é, ele cai para o segundo padrão.
Ele corresponde ao segundo padrão e lá diz que o comprimento é `1 + length' "am"`, porque o dividimos em uma cabeça e uma cauda e descartamos a cabeça.
Ok.
O `length'` de `"am"` é, da mesma forma, `1 + length' "m"`.
Então, agora temos `1 + (1 + length' "m")`.
`length' "m"` é `1 + length' ""` (também pode ser escrito como `1 + length' []`).
E definimos `length' []` como `0`.
Então, no final, temos `1 + (1 + (1 + 0))`.

Vamos implementar `sum`.
Sabemos que a soma de uma lista vazia é 0.
Escrevemos isso como um padrão.
E também sabemos que a soma de uma lista é a cabeça mais a soma do restante da lista.
Então, se Escrevemos isso, obtemos:

```{.haskell:nogutter:nocontrols:hs}
sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs
```

Há também uma coisa chamada *padrões as* (as patterns).
Essas são uma maneira prática de dividir algo de acordo com um padrão e vinculá-lo a nomes, mantendo uma referência à coisa toda.
Você faz isso colocando um nome e um `@` na frente de um padrão.
Por exemplo, o padrão `xs@(x:y:ys)`.
Este padrão corresponderá exatamente à mesma coisa que `x:y:ys`, mas você pode obter facilmente a lista inteira via `xs` em vez de se repetir digitando `x:y:ys` no corpo da função novamente.
Aqui está um exemplo rápido e sujo:

```{.haskell:nogutter:nocontrols:hs}
capital :: String -> String
capital "" = "Empty string, whoops!"
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]
```

```{.haskell:ghci}
ghci> capital "Dracula"
"The first letter of Dracula is D"
```

Normalmente, usamos *padrões as* para evitar nos repetir ao combinar com um padrão maior, quando temos que usar a coisa toda novamente no corpo da função.

Mais uma coisa --- você não pode usar `++` em pattern matches.
Se você tentasse fazer pattern match contra `(xs ++ ys)`, o que estaria na primeira e o que estaria na segunda lista?
Não faz muito sentido.
Faria sentido combinar coisas contra `(xs ++ [x,y,z])` ou apenas `(xs ++ [x])`, mas devido à natureza das listas, você não pode fazer isso.

## Guardas, guardas! (Guards, guards!) {#guards-guards}

![guards](assets/images/syntax-in-functions/guards.png){.left width=83 height=180}

Considerando que os padrões são uma maneira de garantir que um valor esteja em conformidade com alguma forma e desconstruí-lo, as guardas (guards) são uma maneira de testar se alguma propriedade de um valor (ou vários deles) é verdadeira ou falsa.
Isso soa muito como uma instrução if e é muito semelhante.
Acontece que as guardas são muito mais legíveis quando você tem várias condições e elas jogam muito bem com padrões.

Em vez de explicar sua sintaxe, vamos mergulhar e fazer uma função usando guardas.
Vamos fazer uma função simples que responda de maneira diferente, dependendo da [densidade](https://en.wikipedia.org/wiki/Density) fornecida.
Densidade (ou massa específica) é a massa de uma substância por unidade de volume (aqui, gramas por litro).
Se uma substância tiver uma densidade inferior a 1.2, ela flutuará no ar, pois 1.2g/L é a densidade do ar.
Se tiver mais de 1000g/L (a densidade da água), afundará na água.
Entre eles estão coisas (como pessoas, geralmente) que nem flutuarão nem afundarão na água.
Então aqui está a função (não calcularemos a densidade agora, essa função apenas recebe uma densidade e responde)

```{.haskell:hs}
densityTell :: (RealFloat a) => a -> String
densityTell density
    | density < 1.2 = "Wow! You're going for a ride in the sky!"
    | density <= 1000.0 = "Have fun swimming, but watch out for sharks!"
    | otherwise   = "If it's sink or swim, you're going to sink."
```

Guardas são indicadas por pipes que seguem o nome de uma função e seus parâmetros.
Geralmente, eles são recuados um pouco para a direita e alinhados.
Uma guarda pode ser uma de duas coisas.
A primeira é basicamente uma expressão booleana.
Se for avaliada como `True`, o corpo da função correspondente será usado.
Se for avaliada como `False`, a verificação cairá para a próxima guarda e assim por diante.
Se chamarmos essa função com `24.3`, ela verificará primeiro se é menor ou igual a `1.2`.
Porque não é, cai para a próxima guarda.
A verificação é realizada com a segunda guarda e, como `24.3` é menor que `1000.0`, a segunda string é retornada.

Isso lembra muito uma grande árvore if else em linguagens imperativas, só que isso é muito melhor e mais legível.
Embora grandes árvores if else geralmente sejam desaprovadas, às vezes um problema é definido de maneira tão discreta que você não pode contorná-las.
Guardas são uma alternativa muito boa para isso.

Muitas vezes, a última guarda é `otherwise`.
`otherwise` é definido simplesmente como `otherwise = True` e pega tudo.
Isso é muito semelhante aos padrões, apenas eles verificam se a entrada satisfaz um padrão, mas as guardas booleanas verificam as condições booleanas.
Se todas as guardas de uma função forem avaliadas como `False` (e não tivermos fornecido uma guarda catch-all `otherwise`), a avaliação cairá para o próximo **padrão**.
É assim que padrões e guardas jogam bem juntos.
Se nenhuma guarda ou padrão adequado for encontrado, um erro será lançado.

Claro que podemos usar guardas com funções que recebem quantos parâmetros quisermos.
Em vez de fazer o usuário calcular a densidade da substância por conta própria antes de chamar a função, vamos modificar essa função para que ela leve uma massa (em gramas) e volume (em litros).

```{.haskell:hs}
densityTell :: (RealFloat a) => a -> a -> String
densityTell mass volume
    | mass / volume < 1.2 = "Wow! You're going for a ride in the sky!"
    | mass / volume <= 1000.0 = "Have fun swimming, but watch out for sharks!"
    | otherwise   = "If it's sink or swim, you're going to sink."
```

Vamos ver se comida de gato flutua...

```{.haskell:ghci}
ghci> densityTell 400 1
"Have fun swimming, but watch out for sharks!"
```

Parece que vai!
Pelo menos até se dissolver na piscina...
Eca!

Observe que não há `=` logo após o nome da função e seus parâmetros, antes da primeira guarda.
Muitos novatos recebem erros de sintaxe porque às vezes o colocam lá.

Outro exemplo muito simples: vamos implementar nossa própria função `max`.
Se você se lembra, leva duas coisas que podem ser comparadas e retorna a maior delas.

```{.haskell:hs}
max' :: (Ord a) => a -> a -> a
max' a b
    | a > b     = a
    | otherwise = b
```

Guardas também podem ser escritas inline, embora eu desaconselhe isso porque é menos legível, mesmo para funções muito curtas.
Mas, para demonstrar, poderíamos escrever `max'` assim:

```{.haskell:hs}
max' :: (Ord a) => a -> a -> a
max' a b | a > b = a | otherwise = b
```

Ugh!
Nem um pouco legível!

Continuando: vamos implementar nosso próprio `compare` usando guardas.

```{.haskell:hs}
myCompare :: (Ord a) => a -> a -> Ordering
a `myCompare` b
    | a > b     = GT
    | a == b    = EQ
    | otherwise = LT
```

```{.haskell:hs}
ghci> 3 `myCompare` 2
GT
```

::: {.hintbox}
**Nota:** Não apenas podemos chamar funções como infixas com crases, também podemos defini-las usando crases.
Às vezes é mais fácil ler dessa maneira.
:::

Mas espere! Não é o único tipo de guarda.
Às vezes, você deseja verificar não se um argumento satisfaz algum padrão, mas se o resultado de alguma função satisfaz (e pattern match no resultado, é claro).
É para isso que servem as guardas de padrão (pattern guards):

```{.haskell:hs}
densityTell :: String -> String  
densityTell input  
    | Just density <- readMaybe input, density < 1.2 = "Wow! You're going for a ride in the sky!"  
    | Just density <- readMaybe input, density <= 1000.0 = "Have fun swimming, but watch out for sharks!"  
    | Nothing <- readMaybe input :: (RealFloat a => Maybe a) = "You know I need a density, right?"  
    | otherwise   = "If it's sink or swim, you're going to sink."  
```

A sintaxe completa das guardas é uma série de expressões booleanas ou guardas de padrão, separadas por vírgulas.

::: {.hintbox}
**Nota:** Haskell foi projetado para ser uma linguagem que evolui e inclui os resultados de pesquisa e experimentação.
As guardas de padrão não foram incluídas na primeira versão estável do Haskell (chamada **Haskell98**), mas foram adicionadas à próxima (chamada **Haskell2010**).
:::

## Onde!? (Where!?) {#where}

Na seção anterior, definimos uma função de calculadora de densidade e respondedor como esta:

```{.haskell:hs}
densityTell :: (RealFloat a) => a -> a -> String
densityTell mass volume
    | mass / volume < 1.2 = "Wow! You're going for a ride in the sky!"
    | mass / volume <= 1000.0 = "Have fun swimming, but watch out for sharks!"
    | otherwise   = "If it's sink or swim, you're going to sink."
```

Observe que nos repetimos aqui duas vezes.
Nós nos repetimos duas vezes.
Repetir-se (duas vezes) durante a programação é tão desejável quanto levar um chute na cabeça.
Como repetimos a mesma expressão duas vezes, seria ideal se pudéssemos calculá-la uma vez, vinculá-la a um nome e depois usar esse nome em vez da expressão.
Bem, podemos modificar nossa função assim:

```{.haskell:hs}
densityTell :: (RealFloat a) => a -> a -> String
densityTell mass volume
    | density < 1.2 = "Wow! You're going for a ride in the sky!"
    | density <= 1000.0 = "Have fun swimming, but watch out for sharks!"
    | otherwise   = "If it's sink or swim, you're going to sink."
    where density = mass / volume
```

Colocamos a palavra-chave `where` após as guardas (geralmente é melhor recuá-la tanto quanto os pipes são recuados) e depois definimos vários nomes ou funções.
Esses nomes são visíveis nas guardas e nos dão a vantagem de não ter que nos repetir.
Se decidirmos que queremos calcular a densidade de maneira um pouco diferente, só precisamos alterá-la uma vez.
Também melhora a legibilidade ao dar nomes às coisas e pode tornar nossos programas mais rápidos, já que coisas como nossa variável `density` aqui são calculadas apenas uma vez.
Poderíamos exagerar um pouco e apresentar nossa função assim:

```{.haskell:hs}
densityTell :: (RealFloat a) => a -> a -> String
densityTell mass volume
    | density < air = "Wow! You're going for a ride in the sky!"
    | density <= water = "Have fun swimming, but watch out for sharks!"
    | otherwise   = "If it's sink or swim, you're going to sink."
    where density = mass / volume
          air = 1.2
          water = 1000.0
```

Os nomes que definimos na seção where de uma função são visíveis apenas para essa função, portanto, não precisamos nos preocupar com eles poluindo o namespace de outras funções.
Observe que todos os nomes estão alinhados em uma única coluna.
Se não os alinharmos bem e corretamente, o Haskell ficará confuso porque não sabe que todos fazem parte do mesmo bloco.

As ligações *where* não são compartilhadas entre corpos de função de padrões diferentes.
Se você deseja que vários padrões de uma função acessem algum nome compartilhado, deve defini-lo globalmente.

Você também pode usar ligações where para fazer **pattern match**!
Poderíamos ter reescrito a seção where da nossa função anterior como:

```{.haskell:hs}
...
    where density = mass / volume
          (air, water) = (1.2, 1000.0)
```

Vamos fazer outra função bastante trivial, onde obtemos um nome e um sobrenome e devolvemos a alguém suas iniciais.

```{.haskell:hs}
initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
    where (f:_) = firstname
          (l:_) = lastname
```

Poderíamos ter feito esse pattern matching diretamente nos parâmetros da função (teria sido mais curto e claro, na verdade), mas isso apenas mostra que é possível fazê-lo nas ligações where também.

Assim como definimos constantes em blocos where, você também pode definir funções.
Mantendo-nos fiéis ao nosso tema de programação de sólidos, vamos fazer uma função que pega uma lista de pares de massa-volume e retorna uma lista de densidades.

```{.haskell:hs}
calcDensities :: (RealFloat a) => [(a, a)] -> [a]
calcDensities xs = [density m v | (m, v) <- xs]
    where density mass volume = mass / volume
```

E só isso!
A razão pela qual tivemos que introduzir `density` como uma função neste exemplo é porque não podemos calcular apenas uma densidade a partir dos parâmetros da função.
Temos que examinar a lista passada para a função e há uma densidade diferente para cada par lá.

As ligações *where* também podem ser aninhadas.
É um idioma comum fazer uma função e definir alguma função auxiliar em sua cláusula *where* e, em seguida, fornecer a essas funções funções auxiliares também, cada uma com sua própria cláusula *where*.

## Deixe estar (Let it be) {#let-it-be}

Muito semelhantes às ligações where são as ligações let.
Ligações where são uma construção sintática que permite vincular a variáveis no final de uma função e toda a função pode vê-las, incluindo todas as guardas.
Ligações let permitem vincular a variáveis em qualquer lugar e são expressões em si mesmas, mas são muito locais, portanto não abrangem guardas.
Assim como qualquer construção em Haskell usada para vincular valores a nomes, as ligações let podem ser usadas para pattern matching.
Vamos vê-las em ação!
É assim que poderíamos definir uma função que nos dá a área de superfície de um cilindro com base em sua altura e raio:

```{.haskell:hs}
cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
    let sideArea = 2 * pi * r * h
        topArea = pi * r ^2
    in  sideArea + 2 * topArea
```

![let it be](assets/images/syntax-in-functions/letitbe.png){.right width=215 height=240}

A forma é `let <bindings> in <expression>`.
Os nomes que você define na parte *let* são acessíveis à expressão após a parte *in*.
Como você pode ver, também poderíamos ter definido isso com uma ligação *where*.
Observe que os nomes também estão alinhados em uma única coluna.
Então, qual é a diferença entre os dois?
Por enquanto, parece que *let* coloca as ligações primeiro e a expressão que as usa depois, enquanto *where* é o contrário.

A diferença é que as ligações *let* são expressões em si mesmas.
Ligações *where* são apenas construções sintáticas.
Lembra quando fizemos a instrução if e foi explicado que uma instrução if else é uma expressão e você pode encaixá-la em quase qualquer lugar?

```{.haskell:ghci}
ghci> [if 5 > 3 then "Woo" else "Boo", if 'a' > 'b' then "Foo" else "Bar"]
["Woo", "Bar"]
ghci> 4 * (if 10 > 5 then 10 else 0) + 2
42
```

Você também pode fazer isso com ligações let.

```{.haskell:ghci}
ghci> 4 * (let a = 9 in a + 1) + 2
42
```

Eles também podem ser usados para introduzir funções em um escopo local:

```{.haskell:ghci}
ghci> [let square x = x * x in (square 5, square 3, square 2)]
[(25,9,4)]
```

Se quisermos vincular a várias variáveis inline, obviamente não podemos alinhá-las em colunas.
É por isso que podemos separá-las com ponto e vírgula.

```{.haskell:ghci}
ghci> (let a = 100; b = 200; c = 300 in a*b*c, let foo="Hey "; bar = "there!" in foo ++ bar)
(6000000,"Hey there!")
```

Você não precisa colocar um ponto e vírgula após a última ligação, mas pode se quiser.
Como dissemos antes, você pode fazer pattern match com ligações *let*.
Elas são muito úteis para desmantelar rapidamente uma tupla em componentes e vinculá-las a nomes e assim por diante.

```{.haskell:ghci}
ghci> (let (a,b,c) = (1,2,3) in a+b+c) * 100
600
```

Você também pode colocar ligações *let* dentro de compreensões de lista.
Vamos reescrever nosso exemplo anterior de cálculo de listas de pares massa-volume para usar um *let* dentro de uma compreensão de lista em vez de definir uma função auxiliar com um *where*.

```{.haskell:hs}
calcDensities :: (RealFloat a) => [(a, a)] -> [a]
calcDensities xs = [density | (m, v) <- xs, let density = m / v]
```

Incluímos um *let* dentro de uma compreensão de lista da mesma forma que faríamos com um predicado, só que ele não filtra a lista, apenas vincula a nomes.
Os nomes definidos em um *let* dentro de uma compreensão de lista são visíveis para a função de saída (a parte anterior ao `|`) e todos os predicados e seções que vêm após a ligação.
Para que pudéssemos fazer nossa função retornar apenas as densidades que flutuarão no ar:

```{.haskell:hs}
calcDensities :: (RealFloat a) => [(a, a)] -> [a]
calcDensities xs = [density | (m, v) <- xs, let density = m / v, density < 1.2]
```

Não podemos usar o nome `density` na parte `(m, v) <- xs` porque é definido antes da ligação *let*.

Omitimos a parte *in* da ligação *let* quando as usamos em compreensões de lista porque a visibilidade dos nomes já está predefinida lá.
No entanto, poderíamos usar uma ligação *let in* em um predicado e os nomes definidos seriam visíveis apenas para esse predicado.
A parte *in* também pode ser omitida ao definir funções e constantes diretamente no GHCi.
Se fizermos isso, os nomes ficarão visíveis durante toda a sessão interativa.

```{.haskell:ghci}
ghci> let zoot x y z = x * y + z
ghci> zoot 3 9 2
29
ghci> let boot x y z = x * y + z in boot 3 4 2
14
ghci> boot
<interactive>:1:0: Not in scope: `boot'
```

Se as ligações *let* são tão legais, por que não usá-las o tempo todo em vez de ligações *where*, você pergunta?
Bem, como as ligações *let* são expressões e são bastante locais em seu escopo, elas não podem ser usadas entre guardas.
Algumas pessoas preferem ligações *where* porque os nomes vêm depois da função em que estão sendo usados.
Dessa forma, o corpo da função está mais próximo de seu nome e declaração de tipo e para alguns isso é mais legível.

## Expressões case (Case expressions) {#case-expressions}

![case](assets/images/syntax-in-functions/case.png){.right width=185 height=164}

Muitas linguagens imperativas (C, C++, Java, etc.) têm sintaxe case e, se você já programou nelas, provavelmente sabe do que se trata.
Trata-se de pegar uma variável e executar blocos de código para valores específicos dessa variável e, em seguida, talvez incluir um bloco de código catch-all no caso de a variável ter algum valor para o qual não configuramos um caso.

Haskell pega esse conceito e o supera.
Como o nome indica, expressões case são, bem, expressões, muito parecidas com expressões if else e ligações *let*.
Não apenas podemos avaliar expressões com base nos casos possíveis do valor de uma variável, mas também podemos fazer pattern matching.
Hmmm, pegar uma variável, fazer pattern matching nela, avaliar pedaços de código com base em seu valor, onde já ouvimos isso antes?
Ah, sim, pattern matching em parâmetros nas definições de função!
Bem, isso é na verdade apenas açúcar sintático para expressões case.
Esses dois pedaços de código fazem a mesma coisa e são intercambiáveis:

```{.haskell:hs}
head' :: [a] -> a
head' [] = error "No head for empty lists!"
head' (x:_) = x
```

```{.haskell:hs}
head' :: [a] -> a
head' xs = case xs of [] -> error "No head for empty lists!"
                      (x:_) -> x
```

Como você pode ver, a sintaxe para expressões case é bastante simples:

```{.haskell:hs}
case expression of pattern -> result
                   pattern -> result
                   pattern -> result
                   ...
```

`expression` é correspondida com os padrões.
A ação de pattern matching é a mesma esperada: o primeiro padrão que corresponde à expressão é usado.
Se cair por toda a expressão case e nenhum padrão adequado for encontrado, ocorrerá um erro de tempo de execução.

Considerando que o pattern matching nos parâmetros de função pode ser feito apenas ao definir funções, as expressões case podem ser usadas em praticamente qualquer lugar.
Por exemplo:

```{.haskell:hs}
describeList :: [a] -> String
describeList xs = "The list is " ++ case xs of [] -> "empty."
                                               [x] -> "a singleton list."
                                               xs -> "a longer list."
```

Elas são úteis para fazer pattern matching em algo no meio de uma expressão.
Como o pattern matching nas definições de função é açúcar sintático para expressões case, também poderíamos ter definido isso assim:

```{.haskell:hs}
describeList :: [a] -> String
describeList xs = "The list is " ++ what xs
    where what [] = "empty."
          what [x] = "a singleton list."
          what xs = "a longer list."
```

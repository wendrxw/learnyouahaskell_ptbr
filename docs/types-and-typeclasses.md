# Tipos e Typeclasses

## Acredite no tipo (Believe the type) {#believe-the-type}

![moo](assets/images/types-and-typeclasses/cow.png){.left width=180 height=127}

Anteriormente, mencionamos que o Haskell possui um sistema de tipos estático.
O tipo de cada expressão é conhecido em tempo de compilação, o que leva a um código mais seguro.
Se você escrever um programa onde tenta dividir um tipo booleano por algum número, ele nem compilará.
Isso é bom porque é melhor detectar esses erros em tempo de compilação do que ter seu programa travando.
Tudo em Haskell tem um tipo, então o compilador pode raciocinar bastante sobre seu programa antes de compilá-lo.

Ao contrário de Java ou Pascal, Haskell possui inferência de tipo.
Se escrevermos um número, não precisamos dizer a Haskell que é um número.
Ele pode *inferir* isso por conta própria, portanto, não precisamos escrever explicitamente os tipos de nossas funções e expressões para fazer as coisas.
Cobrimos alguns dos fundamentos do Haskell com apenas uma visão muito superficial dos tipos.
No entanto, entender o sistema de tipos é uma parte muito importante do aprendizado de Haskell.

Um tipo é uma espécie de rótulo que toda expressão possui.
Ele nos diz em qual categoria de coisas essa expressão se encaixa.
A expressão `True` é um booleano, `"hello"` é uma string, etc.

Agora usaremos o GHCI para examinar os tipos de algumas expressões.
Faremos isso usando o comando `:t` que, seguido por qualquer expressão válida, nos diz seu tipo.
Vamos dar uma volta.

```{.haskell: .ghci}
ghci> :t 'a'
'a' :: Char
ghci> :t True
True :: Bool
ghci> :t "HELLO!"
"HELLO!" :: [Char]
ghci> :t (True, 'a')
(True, 'a') :: (Bool, Char)
ghci> :t 4 == 5
4 == 5 :: Bool
```

![bomb](assets/images/types-and-typeclasses/bomb.png){.right width=171 height=144}
Aqui vemos que fazer `:t` em uma expressão imprime a expressão seguida por `::` e seu tipo.
`::` é lido como "tem o tipo de".
Tipos explícitos são sempre denotados com a primeira letra em maiúscula.
`'a'`, ao que parece, tem um tipo de `Char`.
Não é difícil concluir que significa *caractere*.
`True` é do tipo `Bool`.
Isso faz sentido.
Mas o que é isso?
Examinar o tipo de `"HELLO!"` produz um `[Char]`.
Os colchetes denotam uma lista.
Então lemos isso como sendo *uma lista de caracteres*.
Ao contrário das listas, cada tamanho de tupla tem seu próprio tipo.
Portanto, a expressão `(True, 'a')` tem um tipo de `(Bool, Char)`, enquanto uma expressão como `('a', 'b', 'c')` teria o tipo de `(Char, Char, Char)`.
`4 == 5` sempre retornará `False`, então seu tipo é `Bool`.

Funções também têm tipos.
Ao escrever nossas próprias funções, podemos optar por dar-lhes uma declaração de tipo explícita.
Isso geralmente é considerado uma boa prática, exceto ao escrever funções muito curtas.
A partir daqui, daremos a todas as funções que fizermos declarações de tipo.
Lembra da compreensão de lista que fizemos anteriormente que filtra uma string para que apenas as maiúsculas permaneçam?
Veja como fica com uma declaração de tipo.

```{.haskell:hs}
removeNonUppercase :: [Char] -> [Char]
removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]
```

`removeNonUppercase` tem um tipo de `[Char] -> [Char]`, o que significa que mapeia de uma string para uma string.
Isso ocorre porque recebe uma string como parâmetro e retorna outra como resultado.
O tipo `[Char]` é sinônimo de `String`, então é mais claro se escrevermos `removeNonUppercase :: String -> String`.
Não precisávamos dar a essa função uma declaração de tipo porque o compilador pode inferir por si mesmo que é uma função de uma string para uma string, mas fizemos assim mesmo.
Mas como escrevemos o tipo de uma função que recebe vários parâmetros?
Aqui está uma função simples que pega três inteiros e os soma:

```{.haskell:hs}
addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z
```

Os parâmetros são separados por `->` e não há distinção especial entre os parâmetros e o tipo de retorno.
O tipo de retorno é o último item na declaração e os parâmetros são os três primeiros.
Mais tarde, veremos por que todos eles são apenas separados por `->` em vez de ter alguma distinção mais explícita entre os tipos de retorno e os parâmetros, como `Int, Int, Int -> Int` ou algo assim.

Se você quiser dar à sua função uma declaração de tipo, mas não tiver certeza de qual deve ser, sempre poderá apenas escrever a função sem ela e depois verificá-la com `:t`.
Funções são expressões também, então `:t` funciona nelas sem problemas.

Aqui está uma visão geral de alguns tipos comuns.

`Int`{.label .type} significa inteiro.
É usado para números inteiros.
`7` pode ser um `Int`, mas `7.2` não pode.

## Acredite no tipo {#believe-the-type}

`Int` é limitado, o que significa que tem um valor mínimo e um máximo.
Geralmente em máquinas de 32 bits o máximo `Int` possível é 2147483647 e o mínimo é -2147483648.

`Integer`{.label .type} significa, er... também inteiro.
A principal diferença é que não é limitado, então pode ser usado para representar números realmente muito grandes.
Quero dizer, muito grandes mesmo.
`Int`, no entanto, é mais eficiente.

```{.haskell:hs}
factorial :: Integer -> Integer
factorial n = product [1..n]
```

```{.haskell: .ghci}
ghci> factorial 50
30414093201713378043612608166064768844377641568960512000000000000
```

`Float`{.label .type} é um ponto flutuante real com precisão simples.

```{.haskell:hs}
circumference :: Float -> Float
circumference r = 2 * pi * r
```

```{.haskell: .ghci}
ghci> circumference 4.0
25.132742
```

`Double`{.label .type} é um ponto flutuante real com o dobro da precisão!

```{.haskell:hs}
circumference' :: Double -> Double
circumference' r = 2 * pi * r
```

```{.haskell: .ghci}
ghci> circumference' 4.0
25.132741228718345
```

`Bool`{.label .type} é um tipo booleano.
Pode ter apenas dois valores: `True` e `False`.

`Char`{.label .type} representa um caractere.
É denotado por aspas simples.
Uma lista de caracteres é uma string.

Tuplas são tipos, mas dependem de seu comprimento, bem como dos tipos de seus componentes, portanto, teoricamente, há um número infinito de tipos de tuplas, o que é demais para cobrir neste tutorial.
Observe que a tupla vazia `()`{.label .type} também é um tipo que pode ter apenas um único valor: `()`

## Variáveis de tipo (Type variables) {#type-variables}

Qual você acha que é o tipo da função `head`?
Como `head` pega uma lista de qualquer tipo e retorna o primeiro elemento, o que poderia ser?
Vamos verificar!

```{.haskell: .ghci}
ghci> :t head
head :: [a] -> a
```

![box](assets/images/types-and-typeclasses/box.png){.left width=130 height=93}
Hmmm!
O que é esse `a`?
É um tipo?
Lembre-se de que declaramos anteriormente que os tipos são escritos em letras maiúsculas, portanto, não pode ser exatamente um tipo.
Como não está em maiúsculas, na verdade é uma **variável de tipo**.
Isso significa que `a` pode ser de qualquer tipo.
Isso é muito parecido com genéricos em outras linguagens, só que em Haskell é muito mais poderoso porque nos permite escrever facilmente funções muito gerais se elas não usarem nenhum comportamento específico dos tipos nelas.
Funções que possuem variáveis de tipo são chamadas de **funções polimórficas**.
A declaração de tipo de `head` afirma que ela recebe uma lista de qualquer tipo e retorna um elemento desse tipo.

Embora as variáveis de tipo possam ter nomes com mais de um caractere, geralmente damos a elas nomes como a, b, c, d ...

Lembra de `fst`?
Retorna o primeiro componente de um par.
Vamos examinar seu tipo.

```{.haskell: .ghci}
ghci> :t fst
fst :: (a, b) -> a
```

Vemos que `fst` recebe uma tupla que contém dois tipos e retorna um elemento que é do mesmo tipo que o primeiro componente do par.
É por isso que podemos usar `fst` em um par que contém quaisquer dois tipos.
Observe que apenas porque `a` e `b` são variáveis de tipo diferentes, eles não precisam ser tipos diferentes.
Apenas afirma que o tipo do primeiro componente e o tipo do valor de retorno são os mesmos.

## Typeclasses 101 {#typeclasses-101}

![class](assets/images/types-and-typeclasses/classes.png){.right width=210 height=158}

Uma typeclass é uma espécie de interface que define algum comportamento.
Se um tipo faz parte de uma typeclass, isso significa que ele suporta e implementa o comportamento que a typeclass descreve.
Muitas pessoas vindas de OOP (Programação Orientada a Objetos) ficam confusas com typeclasses porque acham que são como classes em linguagens orientadas a objetos.
Bem, elas não são.
Você pode pensar nelas como interfaces Java, só que melhores.

Qual é a assinatura de tipo da função `==`?

```{.haskell: .ghci}
ghci> :t (==)
(==) :: (Eq a) => a -> a -> Bool
```

::: {.hintbox}
**Nota**: o operador de igualdade, `==` é uma função.
Assim como `+`, `*`, `-`, `/` e praticamente todos os operadores.
Se o nome de uma função for composto apenas por caracteres especiais, ela é considerada uma função infixa por padrão.
Se quisermos examinar seu tipo, passá-la para outra função ou chamá-la como uma função prefixa, temos que cercá-la entre parênteses.
:::

Interessante.
Vemos uma coisa nova aqui, o símbolo `=>`.
Tudo antes do símbolo `=>` é chamado de **restrição de classe** (class constraint).
Podemos ler a declaração de tipo anterior assim: a função de igualdade recebe quaisquer dois valores que sejam do mesmo tipo e retorna um `Bool`.
O tipo desses dois valores deve ser um membro da classe `Eq` (esta era a restrição de classe).

A typeclass `Eq` fornece uma interface para testar a igualdade.
Qualquer tipo em que faça sentido testar a igualdade entre dois valores desse tipo deve ser um membro da classe `Eq`.
Todos os tipos padrão de Haskell, exceto IO (o tipo para lidar com entrada e saída) e funções, fazem parte da typeclass `Eq`.

A função `elem` tem um tipo de `(Eq a) => a -> [a] -> Bool` porque usa `==` sobre uma lista para verificar se algum valor que estamos procurando está nela.

Algumas typeclasses básicas:

`Eq`{.label .class} é usado para tipos que suportam teste de igualdade.
As funções que seus membros implementam são `==` e `/=`.
Portanto, se houver uma restrição de classe `Eq` para uma variável de tipo em uma função, ela usa `==` ou `/=` em algum lugar dentro de sua definição.
Todos os tipos que mencionamos anteriormente, exceto funções, fazem parte de `Eq`, então eles podem ser testados quanto à igualdade.

```{.haskell: .ghci}
ghci> 5 == 5
True
ghci> 5 /= 5
False
ghci> 'a' == 'a'
True
ghci> "Ho Ho" == "Ho Ho"
True
ghci> 3.432 == 3.432
True
```

`Ord`{.label .class} é para tipos que têm uma ordenação.

```{.haskell: .ghci}
ghci> :t (>)
(>) :: (Ord a) => a -> a -> Bool
```

Todos os tipos que cobrimos até agora, exceto funções, fazem parte de `Ord`.
`Ord` cobre todas as funções de comparação padrão, como `>`, `<`, `>=` e `<=`.
A função `compare` pega dois membros `Ord` do mesmo tipo e retorna uma ordenação.
`Ordering`{.label .type} é um tipo que pode ser `GT`, `LT` ou `EQ`, significando *maior que* (greater than), *menor que* (lesser than) e *igual* (equal), respectivamente.

Para ser um membro de `Ord`, um tipo deve primeiro ter associação no prestigioso e exclusivo clube `Eq`.

```{.haskell: .ghci}
ghci> "Abrakadabra" < "Zebra"
True
ghci> "Abrakadabra" `compare` "Zebra"
LT
ghci> 5 >= 2
True
ghci> 5 `compare` 3
GT
```

Membros de `Show`{.label .class} podem ser apresentados como strings.
Todos os tipos cobertos até agora, exceto funções, fazem parte de `Show`.
A função mais usada que lida com a typeclass `Show` é `show`.
Ela pega um valor cujo tipo é um membro de `Show` e o apresenta para nós como uma string.

```{.haskell: .ghci}
ghci> show 3
"3"
ghci> show 5.334
"5.334"
ghci> show True
"True"
```

`Read`{.label .class} é uma espécie de typeclass oposta a `Show`.
A função `read` pega uma string e retorna um tipo que é membro de `Read`.

```{.haskell: .ghci}
ghci> read "True" || False
True
ghci> read "8.2" + 3.8
12.0
ghci> read "5" - 2
3
ghci> read "[1,2,3,4]" ++ [3]
[1,2,3,4,3]
```

Até aqui tudo bem.
Novamente, todos os tipos cobertos até agora estão nesta typeclass.
Mas o que acontece se tentarmos fazer apenas `read "4"`?

```{.haskell: .ghci}
ghci> read "4"
<interactive>:1:0:
    Ambiguous type variable `a' in the constraint:
      `Read a' arising from a use of `read' at <interactive>:1:0-7
    Probable fix: add a type signature that fixes these type variable(s)
```

O que o GHCI está nos dizendo aqui é que ele não sabe o que queremos em troca.
Observe que nos usos anteriores de `read` fizemos algo com o resultado depois.
Dessa forma, o GHCI poderia inferir que tipo de resultado queríamos de nosso `read`.
Se o usássemos como booleano, ele sabia que tinha que retornar um `Bool`.

Mas agora, ele sabe que queremos algum tipo que faça parte da classe `Read`, apenas não sabe qual.
Vamos dar uma olhada na assinatura de tipo de `read`.

```{.haskell: .ghci}
ghci> :t read
read :: (Read a) => String -> a
```

Viu?
Ele retorna um tipo que faz parte de `Read`, mas se não tentarmos usá-lo de alguma forma mais tarde, ele não tem como saber qual tipo.
É por isso que podemos usar **anotações de tipo** explícitas.
Anotações de tipo são uma maneira de dizer explicitamente qual deve ser o tipo de uma expressão.
Fazemos isso adicionando `::` no final da expressão e depois especificando um tipo.
Observe:

```{.haskell: .ghci}
ghci> read "5" :: Int
5
ghci> read "5" :: Float
5.0
ghci> (read "5" :: Float) * 4
20.0
ghci> read "[1,2,3,4]" :: [Int]
[1,2,3,4]
ghci> read "(3, 'a')" :: (Int, Char)
(3, 'a')
```

A maioria das expressões é tal que o compilador pode inferir qual é o seu tipo por si mesmo.
Mas às vezes, o compilador não sabe se deve retornar um valor do tipo `Int` ou `Float` para uma expressão como `read "5"`.
Para ver qual é o tipo, Haskell teria que realmente avaliar `read "5"`.
Mas como Haskell é uma linguagem estaticamente tipada, ele precisa saber todos os tipos antes que o código seja compilado (ou no caso do GHCI, avaliado).
Então temos que dizer a Haskell: "Ei, essa expressão deve ter esse tipo, caso você não saiba!".

Membros de `Enum`{.label .class} são tipos ordenados sequencialmente --- eles podem ser enumerados.
A principal vantagem da typeclass `Enum` é que podemos usar seus tipos em intervalos de lista.
Eles também têm sucessores e antecessores definidos, que você pode obter com as funções `succ` e `pred`.
Tipos nesta classe: `()`, `Bool`, `Char`, `Ordering`, `Int`, `Integer`, `Float` e `Double`.

```{.haskell: .ghci}
ghci> ['a'..'e']
"abcde"
ghci> [LT .. GT]
[LT,EQ,GT]
ghci> [3 .. 5]
[3,4,5]
ghci> succ 'B'
'C'
```

Membros de `Bounded`{.label .class} têm um limite superior e um inferior.

```{.haskell: .ghci}
ghci> minBound :: Int
-2147483648
ghci> maxBound :: Char
'\1114111'
ghci> maxBound :: Bool
True
ghci> minBound :: Bool
False
```

`minBound` e `maxBound` são interessantes porque têm um tipo de `(Bounded a) => a`.
Em certo sentido, são constantes polimórficas.

Todas as tuplas também fazem parte de `Bounded` se os componentes também estiverem nela.

```{.haskell: .ghci}
ghci> maxBound :: (Bool, Int, Char)
(True,2147483647,'\1114111')
```

`Num`{.label .class} é uma typeclass numérica.
Seus membros têm a propriedade de poder agir como números.
Vamos examinar o tipo de um número.

```{.haskell: .ghci}
ghci> :t 20
20 :: (Num t) => t
```

Parece que números inteiros também são constantes polimórficas.
Eles podem agir como qualquer tipo que seja um membro da typeclass `Num`.

```{.haskell: .ghci}
ghci> 20 :: Int
20
ghci> 20 :: Integer
20
ghci> 20 :: Float
20.0
ghci> 20 :: Double
20.0
```

Esses são tipos que estão na typeclass `Num`.
Se examinarmos o tipo de `*`, veremos que ele aceita todos os números.

```{.haskell: .ghci}
ghci> :t (*)
(*) :: (Num a) => a -> a -> a
```

Ele pega dois números do mesmo tipo e retorna um número desse tipo.
É por isso que `(5 :: Int) * (6 :: Integer)` resultará em um erro de tipo, enquanto `5 * (6 :: Integer)` funcionará bem e produzirá um `Integer` porque `5` pode agir como um `Integer` ou um `Int`.

Para ingressar em `Num`, um tipo já deve ser amigo de `Show` e `Eq`.

`Integral`{.class .label} também é uma typeclass numérica.
`Num` inclui todos os números, incluindo números reais e números inteiros, `Integral` inclui apenas números inteiros (whole).
Nesta typeclass estão `Int` e `Integer`.

`Floating`{.class .label} inclui apenas números de ponto flutuante, então `Float` e `Double`.

Uma função muito útil para lidar com números é `fromIntegral`{.label .function}.
Ela tem uma declaração de tipo de `fromIntegral :: (Num b, Integral a) => a -> b`.
Pela sua assinatura de tipo, vemos que ela pega um número integral e o transforma em um número mais geral.
Isso é útil quando você quer que tipos inteiros e de ponto flutuante funcionem bem juntos.
Por exemplo, a função `length` tem uma declaração de tipo de `length :: [a] -> Int` em vez de ter um tipo mais geral de `(Num b) => length :: [a] -> b`.
Se tentarmos obter o comprimento de uma lista e depois adicioná-lo a `3.2`, obteremos um erro porque tentamos adicionar um `Int` e um número de ponto flutuante.
Portanto, para contornar isso, fazemos `fromIntegral (length [1,2,3,4]) + 3.2` e tudo funciona.

Observe que `fromIntegral` possui várias restrições de classe em sua assinatura de tipo.
Isso é completamente válido e, como você pode ver, as restrições de classe são separadas por vírgulas dentro dos parênteses.

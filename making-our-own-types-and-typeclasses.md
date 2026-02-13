# Criando Nossos Próprios Tipos e Typeclasses (Making Our Own Types and Typeclasses)

Nos capítulos anteriores, cobrimos alguns tipos e typeclasses existentes em Haskell.
Neste capítulo, aprenderemos como criar os nossos e como colocá-los para trabalhar!

## Introdução a tipos de dados algébricos (Algebraic data types intro) {#algebraic-data-types}

Até agora, encontramos muitos tipos de dados.
`Bool`, `Int`, `Char`, `Maybe`, etc.
Mas como fazemos o nosso?
Bem, uma maneira é usar a palavra-chave **data** para definir um tipo.
Vamos ver como o tipo `Bool` é definido na biblioteca padrão.

```{.haskell:hs}
data Bool = False | True
```

`data` significa que estamos definindo um novo tipo de dados.
A parte antes do `=` denota o tipo, que é `Bool`.
As partes após o `=` são **construtores de valor** (value constructors).
Eles especificam os diferentes valores que esse tipo pode ter.
O `|` é lido como *ou*.
Então, podemos ler isso como: o tipo `Bool` pode ter um valor de `True` ou `False`.
Tanto o nome do tipo quanto os construtores de valor devem ser iniciados com letra maiúscula.

De maneira semelhante, podemos pensar no tipo `Int` sendo definido assim:

```{.haskell:hs}
data Int = -2147483648 | -2147483647 | ... | -1 | 0 | 1 | 2 | ... | 2147483647
```

![caveman](assets/images/making-our-own-types-and-typeclasses/caveman.png){.left width=220 height=215}

O primeiro e o último valor de construtores são os valores mínimos e máximos possíveis de `Int`.
Na verdade, não é definido assim, as reticências estão aqui porque omitimos uma carga de números, então isso é apenas para fins ilustrativos.

Agora, vamos pensar em como representaríamos uma forma em Haskell.
Uma maneira seria usar tuplas.
Um círculo poderia ser indicado como `(43.1, 55.0, 10.4)`, onde o primeiro e o segundo campos são as coordenadas do centro do círculo e o terceiro campo é o raio.
Parece bom, mas isso também pode representar um vetor 3D ou qualquer outra coisa.
Uma solução melhor seria criar nosso próprio tipo para representar uma forma.
Digamos que uma forma pode ser um círculo ou um retângulo.
Aqui está:

```{.haskell:hs}
data Shape = Circle Float Float Float | Rectangle Float Float Float Float
```

Agora o que é isso?
Pense assim.
O construtor de valor `Circle` possui três campos, que recebem floats.
Portanto, quando escrevemos um construtor de valor, podemos, opcionalmente, adicionar alguns tipos depois dele e esses tipos definem os valores que ele conterá.
Aqui, os dois primeiros campos são as coordenadas de seu centro, o terceiro seu raio.
O construtor de valor `Rectangle` possui quatro campos que aceitam floats.
Os dois primeiros são as coordenadas para o canto superior esquerdo e os dois segundos são coordenadas para o canto inferior direito.

Agora, quando digo campos, na verdade quero dizer parâmetros.
Construtores de valor são na verdade funções que, em última análise, retornam um valor de um tipo de dados.
Vamos dar uma olhada nas assinaturas de tipo para esses dois construtores de valor.

```{.haskell:hs}
ghci> :t Circle
Circle :: Float -> Float -> Float -> Shape
ghci> :t Rectangle
Rectangle :: Float -> Float -> Float -> Float -> Shape
```

Legal, então construtores de valor são funções como todo o resto.
Quem teria pensado?
Vamos fazer uma função que pega uma forma e retorna sua superfície.

```{.haskell:hs}
surface :: Shape -> Float
surface (Circle _ _ r) = pi * r ^ 2
surface (Rectangle x1 y1 x2 y2) = (abs $ x2 - x1) * (abs $ y2 - y1)
```

A primeira coisa notável aqui é a declaração de tipo.
Diz que a função assume uma forma e retorna um float.
Não poderíamos escrever uma declaração de tipo de `Circle -> Float` porque `Circle` não é um tipo, `Shape` é.
Assim como não podemos escrever uma função com uma declaração de tipo de `True -> Int`.
A próxima coisa que notamos aqui é que podemos fazer pattern matching com construtores.
Fizemos correspondência de padrões com construtores antes (o tempo todo, na verdade) quando combinamos um padrão com valores como `[]` ou `False` ou `5`, apenas esses valores não tinham campos.
Apenas escrevemos um construtor e depois vinculamos seus campos a nomes.
Como estamos interessados no raio, na verdade não nos importamos com os dois primeiros campos, que nos dizem onde está o círculo.

```{.haskell:hs}
ghci> surface $ Circle 10 20 10
314.15927
ghci> surface $ Rectangle 0 0 100 100
10000.0
```

Yay, funciona!
Mas se tentarmos apenas imprimir `Circle 10 20 5` no prompt, receberemos um erro.
Isso ocorre porque o Haskell não sabe como exibir nosso tipo de dados como uma string (ainda).
Lembre-se, quando tentamos imprimir um valor no prompt, Haskell primeiro executa a função `show` para obter a representação de string do nosso valor e depois imprime isso no terminal.
Para tornar nosso tipo `Shape` parte da typeclass `Show`, nós o modificamos assim:

```{.haskell:hs}
data Shape = Circle Float Float Float | Rectangle Float Float Float Float deriving (Show)
```

Não vamos nos preocupar muito com a derivação por enquanto.
Vamos apenas dizer que, se adicionarmos `deriving (Show)` no final de uma declaração *data*, o Haskell torna automagicamente esse tipo parte da typeclass `Show`.
Então agora, podemos fazer isso:

```{.haskell:hs}
ghci> Circle 10 20 5
Circle 10.0 20.0 5.0
ghci> Rectangle 50 230 60 90
Rectangle 50.0 230.0 60.0 90.0
```

Os construtores de valor são funções, para que possamos mapeá-los e aplicá-los parcialmente e tudo mais.
Se queremos uma lista de círculos concêntricos com raios diferentes, podemos fazer isso.

```{.haskell:hs}
ghci> map (Circle 10 20) [4,5,6,6]
[Circle 10.0 20.0 4.0,Circle 10.0 20.0 5.0,Circle 10.0 20.0 6.0,Circle 10.0 20.0 6.0]
```

Nosso tipo de dados é bom, embora possa ser melhor.
Vamos criar um tipo de dados intermediário que defina um ponto no espaço bidimensional.
Então podemos usar isso para tornar nossas formas mais compreensíveis.

```{.haskell:hs}
data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)
```

Observe que, ao definir um ponto, usamos o mesmo nome para o tipo de dados e o construtor de valor.
Isso não tem significado especial, embora seja comum usar o mesmo nome que o tipo se houver apenas um construtor de valor.
Então agora o `Circle` tem dois campos, um é do tipo `Point` e o outro do tipo `Float`.
Isso facilita a compreensão do que é o quê.
O mesmo vale para o retângulo.
Temos que ajustar nossa função `surface` para refletir essas mudanças.

```{.haskell:hs}
surface :: Shape -> Float
surface (Circle _ r) = pi * r ^ 2
surface (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)
```

A única coisa que tivemos que mudar foram os padrões.
Desconsideramos o ponto inteiro no padrão do círculo.
No padrão do retângulo, acabamos de usar uma correspondência de padrão aninhada para obter os campos dos pontos.
Se quiséssemos referenciar os pontos para algum motivo, poderíamos ter usado as-patterns.

```{.haskell:hs}
ghci> surface (Rectangle (Point 0 0) (Point 100 100))
10000.0
ghci> surface (Circle (Point 0 0) 24)
1809.5574
```

Que tal uma função que empurra uma forma?
Pega uma forma, a quantidade para movê-la no eixo x e a quantidade para movê-la no eixo y e depois retorna uma nova forma que tem as mesmas dimensões, apenas está localizado em outro lugar.

```{.haskell:hs}
nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) r) a b = Circle (Point (x+a) (y+b)) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) a b = Rectangle (Point (x1+a) (y1+b)) (Point (x2+a) (y2+b))
```

Bastante simples.
Adicionamos as quantidades de empurrão aos pontos que denotam a posição da forma.

```{.haskell:hs}
ghci> nudge (Circle (Point 34 34) 10) 5 10
Circle (Point 39.0 44.0) 10.0
```

Se não queremos lidar diretamente com pontos, podemos criar algumas funções auxiliares que criam formas de algum tamanho nas coordenadas zero e depois empurram elas.

```{.haskell:hs}
baseCircle :: Float -> Shape
baseCircle r = Circle (Point 0 0) r

baseRect :: Float -> Float -> Shape
baseRect width height = Rectangle (Point 0 0) (Point width height)
```

```{.haskell:hs}
ghci> nudge (baseRect 40 100) 60 23
Rectangle (Point 60.0 23.0) (Point 100.0 123.0)
```

Você pode, é claro, exportar seus tipos de dados em seus módulos.
Para fazer isso, basta escrever seu tipo junto com as funções que você está exportando e adicionar alguns parênteses e, neles, especificar os construtores de valor que deseja exportar para ele, separados por vírgulas.
Se você deseja exportar todos os construtores de valor para um determinado tipo, basta escrever `..`.

Se quiséssemos exportar as funções e tipos que definimos aqui em um módulo, poderíamos começar assim:

```{.haskell:hs}
module Shapes
( Point(..)
, Shape(..)
, surface
, nudge
, baseCircle
, baseRect
) where
```

Ao fazer `Shape(..)`, exportamos todos os construtores de valor para `Shape`, o que significa que quem importa nosso módulo pode fazer formas usando os construtores de valor `Rectangle` e `Circle`.
É o mesmo que escrever `Shape (Rectangle, Circle)`.

Também poderíamos optar por não exportar nenhum construtor de valor para `Shape`, apenas escrevendo `Shape` na instrução de exportação.
Dessa forma, alguém importando nosso módulo só poderia fazer formas usando as funções auxiliares `baseCircle` e `baseRect`.
`Data.Map` usa essa abordagem.
Você não pode criar um mapa fazendo `Map.Map [(1,2),(3,4)]` porque ele não exporta esse construtor de valor.
No entanto, você pode fazer um mapeamento usando uma das funções auxiliares como `Map.fromList`.

Lembre-se, construtores de valor são apenas funções que tomam os campos como parâmetros e retornam um valor de algum tipo (como `Shape`) como resultado.
Portanto, quando escolhemos não exportá-los, apenas impedimos que a pessoa importe nosso módulo de usar essas funções, mas se algumas outras funções exportadas retornarem um tipo, podemos usá-las para criar valores de nossos tipos de dados personalizados.

Não exportar os construtores de valor de um tipo de dados os torna mais abstratos de tal maneira que ocultamos a implementação deles.
Além disso, quem usa nosso módulo não pode fazer pattern matching contra os construtores de valor.

## Sintaxe de registro (Record syntax) {#record-syntax}

![record](assets/images/making-our-own-types-and-typeclasses/record.png){.right width=208 height=97}

OK, fomos encarregados de criar um tipo de dados que descreva uma pessoa.
As informações que queremos armazenar sobre essa pessoa são: primeiro nome, sobrenome, idade, altura, número de telefone e sabor de sorvete favorito.
Não sei sobre você, mas isso é tudo que eu quero saber sobre uma pessoa.
Vamos tentar!

```{.haskell:hs}
data Person = Person String String Int Float String String deriving (Show)
```

O-kay.
O primeiro campo é o primeiro nome, o segundo é o sobrenome, o terceiro é a idade e assim por diante.
Vamos criar uma pessoa.

```{.haskell:hs}
ghci> let guy = Person "Buddy" "Finklestein" 43 184.2 "526-2928" "Chocolate"
ghci> guy
Person "Buddy" "Finklestein" 43 184.2 "526-2928" "Chocolate"
```

Isso é meio legal, embora um pouco ilegível.
E se quisermos criar uma função para obter informações separadas de uma pessoa?
Uma função que recebe o primeiro nome de uma pessoa, uma função que recebe o sobrenome de uma pessoa, etc.
Bem, teríamos que defini-las mais ou menos assim.

```{.haskell:hs}
firstName :: Person -> String
firstName (Person firstname _ _ _ _ _) = firstname

lastName :: Person -> String
lastName (Person _ lastname _ _ _ _) = lastname

age :: Person -> Int
age (Person _ _ age _ _ _) = age

height :: Person -> Float
height (Person _ _ _ height _ _) = height

phoneNumber :: Person -> String
phoneNumber (Person _ _ _ _ number _) = number

flavor :: Person -> String
flavor (Person _ _ _ _ _ flavor) = flavor
```

Ufa!
Eu certamente não gostei de escrever isso!
Apesar de ser muito complicado e CHATO de escrever, esse método funciona.

```{.haskell:hs}
ghci> let guy = Person "Buddy" "Finklestein" 43 184.2 "526-2928" "Chocolate"
ghci> firstName guy
"Buddy"
ghci> height guy
184.2
ghci> flavor guy
"Chocolate"
```

Deve haver uma maneira melhor, você diz!
Bem não, não há, desculpe.

Brincadeirinha, existe.
Hahaha!
Os criadores de Haskell eram muito inteligentes e anteciparam esse cenário.
Eles incluíram uma maneira alternativa de escrever tipos de dados.
Veja como poderíamos alcançar a funcionalidade acima com a sintaxe de registro.

```{.haskell:hs}
data Person = Person { firstName :: String
                     , lastName :: String
                     , age :: Int
                     , height :: Float
                     , phoneNumber :: String
                     , flavor :: String
                     } deriving (Show)
```

Portanto, em vez de apenas nomear os tipos de campo um após o outro e separá-los com espaços, usamos colchetes.
Primeiro, escrevemos o nome do campo, por exemplo, `firstName` e depois escrevemos dois pontos duplos `::` (também chamados de Paamayim Nekudotayim, haha) e depois especificamos o tipo.
O tipo de dados resultante é exatamente o mesmo.
O principal benefício disso é que ele cria funções que procuram campos no tipo de dados.
Ao usar sintaxe de registro para criar esse tipo de dados, o Haskell criou automaticamente estas funções: `firstName`, `lastName`, `age`, `height`, `phoneNumber` e `flavor`.

```{.haskell:hs}
ghci> :t flavor
flavor :: Person -> String
ghci> :t firstName
firstName :: Person -> String
```

Há outro benefício em usar a sintaxe de registro.
Quando derivamos `Show` para o tipo, ele o exibe de maneira diferente se usarmos a sintaxe de registro para definir e instanciar o tipo.
Digamos que temos um tipo que representa um carro.
Queremos acompanhar a empresa que o fez, o nome do modelo e seu ano de produção.
Veja.

```{.haskell:hs}
data Car = Car String String Int deriving (Show)
```

```{.haskell:hs}
ghci> Car "Ford" "Mustang" 1967
Car "Ford" "Mustang" 1967
```

Se o definirmos usando a sintaxe de registro, podemos fazer um carro novo como este.

```{.haskell:hs}
data Car = Car {company :: String, model :: String, year :: Int} deriving (Show)
```

```{.haskell:hs}
ghci> Car {company="Ford", model="Mustang", year=1967}
Car {company = "Ford", model = "Mustang", year = 1967}
```

Ao fazer um carro novo, não precisamos necessariamente colocar os campos na ordem adequada, desde que listemos todos eles.
Mas se não usarmos a sintaxe de registro, temos que especificá-los em ordem.

Use sintaxe de registro quando um construtor tiver vários campos e não for óbvio qual campo é qual.
Se fizermos um tipo de dados vetoriais em 3D fazendo `data Vector = Vector Int Int Int`, é bastante óbvio que os campos são os componentes de um vetor.
No entanto, em nossos tipos `Person` e `Car`, não era tão óbvio e nos beneficiamos muito do uso da sintaxe de registro.

## Parâmetros de tipo (Type parameters) {#type-parameters}

Um construtor de valor pode receber alguns parâmetros de valores e produzir um novo valor.
Por exemplo, o construtor `Car` recebe três valores e produz um valor de carro.
De maneira semelhante, **construtores de tipos** podem receber tipos como parâmetros para produzir novos tipos.
Isso pode parecer um pouco meta demais no começo, mas não é tão complicado.
Se você estiver familiarizado com templates em C++, verá alguns paralelos.
Para obter uma imagem clara de como os parâmetros de tipo funcionam em ação, vamos dar uma olhada em como um tipo que já conhecemos é implementado.

```{.haskell:hs}
data Maybe a = Nothing | Just a
```

![yeti](assets/images/making-our-own-types-and-typeclasses/yeti.png){.left width=209 height=260}

O `a` aqui é o parâmetro de tipo.
E como há um parâmetro de tipo envolvido, chamamos `Maybe` de construtor de tipos.
Dependendo do que queremos que esse tipo de dados mantenha quando não for `Nothing`, esse construtor de tipos pode acabar produzindo um tipo de `Maybe Int`, `Maybe Car`, `Maybe String`, etc.
Nenhum valor pode ter um tipo apenas `Maybe`, porque esse não é um tipo por si só, é um construtor de tipos.
Para que isso seja um tipo real de que um valor pode fazer parte, ele deve ter todos os seus parâmetros de tipo preenchidos.

Portanto, se passarmos `Char` como o parâmetro de tipo para `Maybe`, obteremos um tipo de `Maybe Char`.
O valor `Just 'a'` tem um tipo de `Maybe Char`, por exemplo.

Você pode não saber, mas usamos um tipo que possui um parâmetro de tipo antes de usarmos `Maybe`.
Esse tipo é o tipo de lista.
Embora haja algum açúcar sintático em jogo, o tipo de lista leva um parâmetro para produzir um tipo concreto.
Os valores podem ter um tipo `[Int]`, um tipo `[Char]`, um tipo `[[String]]`, mas você não pode ter um valor que apenas tenha um tipo de `[]`.

Vamos brincar com o tipo `Maybe`.

```{.haskell:hs}
ghci> Just "Haha"
Just "Haha"
ghci> Just 84
Just 84
ghci> :t Just "Haha"
Just "Haha" :: Maybe [Char]
ghci> :t Just 84
Just 84 :: (Num t) => Maybe t
ghci> :t Nothing
Nothing :: Maybe a
ghci> Just 10 :: Maybe Double
Just 10.0
```

Parâmetros de tipo são úteis porque podemos criar tipos diferentes com eles, dependendo do tipo de tipos que queremos contidos em nosso tipo de dados.
Quando fazemos `:t Just "Haha"`, o mecanismo de inferência de tipo descobre que é do tipo `Maybe [Char]`, porque se o `a` no `Just a` for uma string, o `a` em `Maybe a` também deve ser uma string.

Observe que o tipo de `Nothing` é `Maybe a`.
Seu tipo é polimórfico.
Se alguma função exigir um `Maybe Int` como parâmetro, podemos dar um `Nothing`, porque um `Nothing` não contém um valor de qualquer maneira e, portanto, não importa.
O tipo `Maybe a` pode agir como `Maybe Int` se for necessário, assim como `5` pode agir como `Int` ou `Double`.
Da mesma forma, o tipo de lista vazia é `[a]`.
Uma lista vazia pode agir como uma lista de qualquer coisa.
É por isso que podemos fazer `[1,2,3] ++ []` e `["ha","ha","ha"] ++ []`.

O uso de parâmetros de tipo é muito benéfico, mas apenas quando usá-los faz sentido.
Geralmente os usamos quando nosso tipo de dados funcionaria independentemente do tipo de valor que ele mantém dentro dele, como com nosso tipo `Maybe a`.
Se o nosso tipo age como algum tipo de caixa, é bom usá-los.
Poderíamos mudar nosso tipo de dados `Car` disso:

```{.haskell:hs}
data Car = Car { company :: String
               , model :: String
               , year :: Int
               } deriving (Show)
```

Para isso:

```{.haskell:hs}
data Car a b c = Car { company :: a
                     , model :: b
                     , year :: c
                     } deriving (Show)
```

Mas nós realmente nos beneficiaríamos?
A resposta é: provavelmente não, porque acabaríamos definindo funções que funcionam apenas no tipo `Car String String Int`.
Por exemplo, dada nossa primeira definição de `Car`, poderíamos fazer uma função que exibe as propriedades do carro em um pequeno texto agradável.

```{.haskell:hs}
tellCar :: Car -> String
tellCar (Car {company = c, model = m, year = y}) = "This " ++ c ++ " " ++ m ++ " was made in " ++ show y
```

```{.haskell:hs}
ghci> let stang = Car {company="Ford", model="Mustang", year=1967}
ghci> tellCar stang
"This Ford Mustang was made in 1967"
```

Uma pequena função fofa!
A declaração de tipo é fofa e funciona muito bem.
Agora, se `Car` fosse `Car a b c`?

```{.haskell:hs}
tellCar :: (Show a) => Car String String a -> String
tellCar (Car {company = c, model = m, year = y}) = "This " ++ c ++ " " ++ m ++ " was made in " ++ show y
```

Teríamos que forçar essa função a pegar um tipo `Car` de `(Show a) => Car String String a`.

Você pode ver que a assinatura do tipo é mais complicada e o único benefício que realmente obteríamos seria que podemos usar qualquer tipo que seja uma instância da typeclass `Show` como o tipo para `c`.

```{.haskell:hs}
ghci> tellCar (Car "Ford" "Mustang" 1967)
"This Ford Mustang was made in 1967"
ghci> tellCar (Car "Ford" "Mustang" "nineteen sixty seven")
"This Ford Mustang was made in \"nineteen sixty seven\""
ghci> :t Car "Ford" "Mustang" 1967
Car "Ford" "Mustang" 1967 :: (Num t) => Car [Char] [Char] t
ghci> :t Car "Ford" "Mustang" "nineteen sixty seven"
Car "Ford" "Mustang" "nineteen sixty seven" :: Car [Char] [Char] [Char]
```

![meekrat](assets/images/making-our-own-types-and-typeclasses/meekrat.png){.right width=150 height=267}

Na vida real, porém, acabaríamos usando `Car String String Int` na maioria das vezes e, portanto, pareceria que parametrizar o tipo `Car` não vale realmente a pena.
Geralmente usamos parâmetros de tipo quando o tipo contido nos vários construtores de valor do tipo de dados não é realmente tão importante para o tipo funcionar.
Uma lista de coisas é uma lista de coisas e não importa qual é o tipo dessas coisas, ela ainda pode funcionar.
Se queremos somar uma lista de números, podemos especificar mais tarde na função de soma que queremos especificamente uma lista de números.
O mesmo vale para `Maybe`.
`Maybe` representa uma opção de não ter nada ou ter um de algo.
Não importa qual é o tipo de algo.

Outro exemplo de um tipo parametrizado que já encontramos é `Map k v` de `Data.Map`.
O `k` é o tipo das chaves em um mapa e o `v` é o tipo dos valores.
Este é um bom exemplo de onde os parâmetros de tipo são muito úteis.
Ter mapas parametrizados nos permite ter mapeamentos de qualquer tipo para qualquer outro tipo, desde que o tipo da chave faça parte da typeclass `Ord`.
Se estivéssemos definindo um tipo de mapeamento, poderíamos adicionar uma restrição de typeclass na declaração *data*:

```{.haskell:hs}
data (Ord k) => Map k v = ...
```

No entanto, é uma convenção muito forte em Haskell **nunca adicionar restrições de typeclass nas declarações de dados.**
Por quê?
Bem, porque não nos beneficiamos muito, mas acabamos escrevendo mais restrições de classe, mesmo quando não precisamos delas.
Se colocarmos ou não colocarmos a restrição `Ord k` na declaração *data* de `Map k v`, teremos que colocar a restrição em funções que assumem que as chaves em um mapa podem ser ordenadas.
Mas se não colocarmos a restrição na declaração de dados, não precisamos colocar `(Ord k) =>` nas declarações de tipo de funções que não se importam se as chaves podem ser ordenadas ou não.
Um exemplo de tal função é `toList`, que apenas pega um mapeamento e o converte em uma lista associativa.
Sua assinatura de tipo é `toList :: Map k a -> [(k, a)]`.
Se `Map k v` tivesse uma restrição de tipo em sua declaração *data*, o tipo para `toList` teria que ser `toList :: (Ord k) => Map k a -> [(k, a)]`, embora a função não faça nenhuma comparação de chaves por ordem.

Portanto, não coloque restrições de tipo nas declarações *data*, mesmo que pareça fazer sentido, porque você terá que colocá-las nas declarações de tipo de função de qualquer maneira.

Vamos implementar um tipo de vetor 3D e adicionar algumas operações para ele.
Usaremos um tipo parametrizado porque, embora geralmente contenha tipos numéricos, ele ainda suportará vários deles.

```{.haskell:hs}
data Vector a = Vector a a a deriving (Show)

vplus :: (Num t) => Vector t -> Vector t -> Vector t
(Vector i j k) `vplus` (Vector l m n) = Vector (i+l) (j+m) (k+n)

vectMult :: (Num t) => Vector t -> t -> Vector t
(Vector i j k) `vectMult` m = Vector (i*m) (j*m) (k*m)

scalarMult :: (Num t) => Vector t -> Vector t -> t
(Vector i j k) `scalarMult` (Vector l m n) = i*l + j*m + k*n
```

`vplus` é para adicionar dois vetores juntos.
Dois vetores são adicionados apenas adicionando seus componentes correspondentes.
`scalarMult` é para o produto escalar de dois vetores e `vectMult` é para multiplicar um vetor com um escalar.
Essas funções podem operar em tipos de `Vector Int`, `Vector Integer`, `Vector Float`, o que for, desde que o `a` de `Vector a` seja da typeclass `Num`.
Além disso, se você examinar a declaração de tipo para essas funções, verá que elas podem operar apenas em vetores do mesmo tipo e os números envolvidos também devem ser do tipo contido nos vetores.
Observe que não colocamos uma restrição de classe `Num` na declaração *data*, porque teríamos que repeti-la nas funções de qualquer maneira.

Mais uma vez, é muito importante distinguir entre o construtor de tipos e o construtor de valor.
Ao declarar um tipo de dados, a parte antes do `=` é o construtor de tipos e os construtores após ele (possivelmente separados por `|`s) são construtores de valor.
Dar a uma função um tipo de `Vector t t t -> Vector t t t -> t` estaria errado, porque temos que colocar tipos na declaração de tipo e o construtor de **tipo** vetorial leva apenas um parâmetro, enquanto o construtor de valor leva três.
Vamos brincar com nossos vetores.

```{.haskell:hs}
ghci> Vector 3 5 8 `vplus` Vector 9 2 8
Vector 12 7 16
ghci> Vector 3 5 8 `vplus` Vector 9 2 8 `vplus` Vector 0 2 3
Vector 12 9 19
ghci> Vector 3 9 7 `vectMult` 10
Vector 30 90 70
ghci> Vector 4 9 5 `scalarMult` Vector 9.0 2.0 4.0
74.0
ghci> Vector 2 9 3 `vectMult` (Vector 4 9 5 `scalarMult` Vector 9 2 4)
Vector 148 666 222
```

## Instâncias derivadas (Derived instances) {#derived-instances}

![gob](assets/images/making-our-own-types-and-typeclasses/gob.png){.right width=112 height=350}

Na seção [Typeclasses 101](types-and-typeclasses.html#typeclasses-101), explicamos o básico das typeclasses.
Explicamos que uma typeclass é uma espécie de interface que define algum comportamento.
Um tipo pode ser feito uma **instância** de uma typeclass, se suportar esse comportamento.
Exemplo: o tipo `Int` é uma instância da typeclass `Eq` porque a typeclass `Eq` define o comportamento para coisas que podem ser comparadas.
E como inteiros podem ser comparados, `Int` faz parte da typeclass `Eq`.
A utilidade real vem com as funções que atuam como a interface para `Eq`, ou seja, `==` e `/=`.
Se um tipo faz parte da typeclass `Eq`, podemos usar as funções `==` com valores desse tipo.
É por isso que expressões como `4 == 4` e `"foo" /= "bar"` verificam os tipos.

Também mencionamos que elas costumam ser confundidas com classes em linguagens como Java, Python, C++ e afins, o que confunde muitas pessoas.
Nessas linguagens, as classes são um plano a partir do qual criamos objetos que contêm estado e podem executar algumas ações.
Typeclasses são mais como interfaces.
Não criamos dados a partir de typeclasses.
Em vez disso, primeiro criamos nosso tipo de dados e depois pensamos no que ele pode agir.
Se ele pode agir como algo que pode ser comparado, tornamos isto uma instância da typeclass `Eq`.
Se ele pode agir como algo que pode ser ordenado, tornamos isto uma instância da typeclass `Ord`.

Na próxima seção, veremos como podemos fazer manualmente nossos tipos de instâncias de typeclasses implementando as funções definidas pelas typeclasses.
Mas agora, vamos ver como o Haskell pode fazer automaticamente nosso tipo uma instância de uma das seguintes typeclasses: `Eq`, `Ord`, `Enum`, `Bounded`, `Show`, `Read`.
Haskell pode derivar o comportamento de nossos tipos nesses contextos se usarmos a palavra-chave *deriving* ao criar nosso tipo de dados.

Considere este tipo de dados:

```{.haskell:hs}
data Person = Person { firstName :: String
                     , lastName :: String
                     , age :: Int
                     }
```

Descreve uma pessoa.
Vamos supor que duas pessoas não tenham a mesma combinação de nome, sobrenome e idade.
Agora, se tivermos registros para duas pessoas, faz sentido ver se elas representam a mesma pessoa?
Claro que sim.
Podemos tentar compará-las e ver se são iguais ou não.
É por isso que faria sentido para esse tipo fazer parte da typeclass `Eq`.
Vamos derivar a instância.

```{.haskell:hs}
data Person = Person { firstName :: String
                     , lastName :: String
                     , age :: Int
                     } deriving (Eq)
```

Quando derivamos a instância `Eq` para um tipo e depois tentamos comparar dois valores desse tipo com `==` ou `/=`, o Haskell verá se os construtores de valor correspondem (há apenas um construtor de valor aqui) e então verificará se todos os dados contidos no interior correspondem testando cada par de campos com `==`.
Há apenas uma ressalva, porém, os tipos de todos os campos também devem fazer parte da typeclass `Eq`.
Mas como `String` e `Int` são, estamos bem.
Vamos testar nossa instância de `Eq`.

```{.haskell:hs}
ghci> let mikeD = Person {firstName = "Michael", lastName = "Diamond", age = 43}
ghci> let adRock = Person {firstName = "Adam", lastName = "Horovitz", age = 41}
ghci> let mca = Person {firstName = "Adam", lastName = "Yauch", age = 44}
ghci> mca == adRock
False
ghci> mikeD == adRock
False
ghci> mikeD == mikeD
True
ghci> mikeD == Person {firstName = "Michael", lastName = "Diamond", age = 43}
True
```

Obviamente, como `Person` está agora em `Eq`, podemos usá-lo como o `a` para todas as funções que possuem uma restrição de classe de `Eq a` em sua assinatura de tipo, como `elem`.

```{.haskell:hs}
ghci> let beastieBoys = [mca, adRock, mikeD]
ghci> mikeD `elem` beastieBoys
True
```

As typeclasses `Show` e `Read` são para coisas que podem ser convertidas para ou de strings, respectivamente.
Como com `Eq`, se os construtores de um tipo tiverem campos, seu tipo deve ser parte de `Show` ou `Read` se queremos tornar nosso tipo uma instância deles.
Vamos fazer do nosso tipo de dados `Person` uma parte de `Show` e `Read` também.

```{.haskell:hs}
data Person = Person { firstName :: String
                     , lastName :: String
                     , age :: Int
                     } deriving (Eq, Show, Read)
```

Agora podemos imprimir uma pessoa no terminal.

```{.haskell:hs}
ghci> let mikeD = Person {firstName = "Michael", lastName = "Diamond", age = 43}
ghci> mikeD
Person {firstName = "Michael", lastName = "Diamond", age = 43}
ghci> "mikeD is: " ++ show mikeD
"mikeD is: Person {firstName = \"Michael\", lastName = \"Diamond\", age = 43}"
```

Se tentássemos imprimir uma pessoa no terminal antes de tornar o tipo de dados `Person` da typeclass `Show`, Haskell teria reclamado conosco, alegando que não sabe como representar uma pessoa como uma string.
Mas agora que derivamos uma instância `Show` para ele, ele sabe.

`Read` é praticamente a typeclass inversa de `Show`.
`Show` é para converter valores de um tipo em uma string, `Read` é para converter strings em valores de nosso tipo.
Lembre-se, porém, quando usamos a função `read`, precisamos usar uma anotação de tipo explícita para dizer ao Haskell qual tipo queremos obter como resultado.
Se não tornarmos o tipo que queremos como resultado explícito, Haskell não sabe qual tipo queremos.

```{.haskell:hs}
ghci> read "Person {firstName =\"Michael\", lastName =\"Diamond\", age = 43}" :: Person
Person {firstName = "Michael", lastName = "Diamond", age = 43}
```

Se usarmos o resultado de nossa leitura `read` mais tarde, de uma maneira que Haskell possa inferir que ele deve lê-lo como uma pessoa, não precisamos usar a anotação do tipo.

```{.haskell:hs}
ghci> read "Person {firstName =\"Michael\", lastName =\"Diamond\", age = 43}" == mikeD
True
```

Também podemos ler tipos parametrizados, mas temos que preencher os parâmetros do tipo.
Portanto, não podemos fazer `read "Just 't'" :: Maybe a`, mas podemos fazer `read "Just 't'" :: Maybe Char`.

Podemos derivar instâncias para a classe de tipo `Ord`, que é para tipos que têm valores que podem ser ordenados.
Se compararmos dois valores do mesmo tipo que foram feitos usando construtores diferentes, o valor que foi feito com um construtor definido primeiro é considerado menor.
Por exemplo, considere o tipo `Bool`, que pode ter um valor de `False` ou `True`.
Com o objetivo de ver como ele se comporta quando comparado, podemos pensar nele como sendo implementado assim:

```{.haskell:hs}
data Bool = False | True deriving (Ord)
```

Como o construtor de valor `False` é especificado primeiro e o construtor de valor `True` é especificado depois dele, podemos considerar `True` como maior que `False`.

```{.haskell:hs}
ghci> True `compare` False
GT
ghci> True > False
True
ghci> True < False
False
```

No tipo de dados `Maybe a`, o construtor de valor `Nothing` é especificado antes do construtor de valor `Just`, portanto, um valor de `Nothing` é sempre menor que um valor de `Just something`, mesmo que esse algo seja menos um bilhão de trilhões.
Mas se compararmos dois valores `Just`, então ele compara o que está dentro deles.

```{.haskell:hs}
ghci> Nothing < Just 100
True
ghci> Nothing > Just (-49999)
False
ghci> Just 3 `compare` Just 2
GT
ghci> Just 100 > Just 50
True
```

Mas não podemos fazer algo como `Just (*3) > Just (*2)`, porque `(*3)` e `(*2)` são funções, que não são instâncias de `Ord`.

Podemos usar facilmente tipos de dados algébricos para fazer enumerações e as typeclasses `Enum` e `Bounded` nos ajudam com isso.
Considere o seguinte tipo de dados:

```{.haskell:hs}
data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
```

Como todos os construtores de valor são nullary (não aceitam parâmetros, ou seja, campos), podemos torná-lo parte da typeclass `Enum`.
A typeclass `Enum` é para coisas que têm predecessores e sucessores.
Também podemos torná-lo parte da typeclass `Bounded`, que é para coisas que têm o menor valor possível e o valor mais alto possível.
E enquanto estamos nisso, vamos torná-lo também uma instância de todos as outras typeclasses deriváveis e ver o que podemos fazer com isso.

```{.haskell:hs}
data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
           deriving (Eq, Ord, Show, Read, Bounded, Enum)
```

Como faz parte das typeclasses `Show` e `Read`, podemos converter valores desse tipo para e de strings.

```{.haskell:hs}
ghci> Wednesday
Wednesday
ghci> show Wednesday
"Wednesday"
ghci> read "Saturday" :: Day
Saturday
```

Como faz parte das typeclasses `Eq` e `Ord`, podemos comparar ou igualar dias.

```{.haskell:hs}
ghci> Saturday == Sunday
False
ghci> Saturday == Saturday
True
ghci> Saturday > Friday
True
ghci> Monday `compare` Wednesday
LT
```

Também faz parte de `Bounded`, para que possamos obter o dia mais baixo e o mais alto.

```{.haskell:hs}
ghci> minBound :: Day
Monday
ghci> maxBound :: Day
Sunday
```

Também é uma instância de `Enum`.
Podemos obter predecessores e sucessores de dias e podemos fazer faixas de lista a partir deles!

```{.haskell:hs}
ghci> succ Monday
Tuesday
ghci> pred Saturday
Friday
ghci> [Thursday .. Sunday]
[Thursday,Friday,Saturday,Sunday]
ghci> [minBound .. maxBound] :: [Day]
[Monday,Tuesday,Wednesday,Thursday,Friday,Saturday,Sunday]
```

Isso é incrível.

## Sinônimos de tipo (Type synonyms) {#type-synonyms}

Anteriormente, mencionamos que, ao escrever tipos, os tipos `[Char]` e `String` são equivalentes e intercambiáveis.
Isso é implementado com **sinônimos de tipo**.
Sinônimos de tipo realmente não fazem nada por si, são apenas para dar a alguns tipos nomes diferentes, para que façam mais sentido para alguém lendo nosso código e documentação.
Veja como a biblioteca padrão define `String` como sinônimo de `[Char]`.

```{.haskell:hs}
type String = [Char]
```

![chicken](assets/images/making-our-own-types-and-typeclasses/chicken.png){.left width=169 height=225}

Introduzimos a palavra-chave *type*.
A palavra-chave pode ser enganosa para alguns, porque não estamos realmente criando nada novo (fizemos isso com a palavra-chave *data*), mas estamos apenas criando um sinônimo para um tipo já existente.

Se fizermos uma função que converte uma string para maiúsculas e a chamarmos `toUpperString` ou algo assim, podemos dar a ela uma declaração de tipo de `toUpperString :: [Char] -> [Char]` ou `toUpperString :: String -> String`.
Ambos são essencialmente os mesmos, apenas o último é mais agradável de ler.

Quando estávamos lidando com o módulo `Data.Map`, primeiro representamos uma lista telefônica com uma lista de associação antes de convertê-la em um mapa.
Como já descobrimos, uma lista de associação é uma lista de pares de valores-chave.
Vamos ver uma lista telefônica que tínhamos.

```{.haskell:hs}
phoneBook :: [(String,String)]
phoneBook =
    [("amelia","555-2938")
    ,("freya","452-2928")
    ,("isabella","493-2928")
    ,("neil","205-2928")
    ,("roald","939-8282")
    ,("tenzing","853-2492")
    ]
```

Vemos que o tipo de `phoneBook` é `[(String,String)]`.
Isso nos diz que é uma lista de associação que mapeia de strings para strings, mas não muito mais.
Vamos criar um sinônimo de tipo para transmitir mais informações na declaração de tipo.

```{.haskell:hs}
type PhoneBook = [(String,String)]
```

Agora, a declaração de tipo para nossa lista telefônica pode ser `phoneBook :: PhoneBook`.
Vamos criar um sinônimo de tipo para `String` também.

```{.haskell:hs}
type PhoneNumber = String
type Name = String
type PhoneBook = [(Name,PhoneNumber)]
```

Dar sinônimos de tipo para `String` é algo que os programadores de Haskell fazem quando querem transmitir mais informações sobre o que as strings em suas funções devem ser usadas e o que elas representam.

Então agora, quando implementamos uma função que pega um nome e um número e vê se essa combinação de nome e número está em nossa lista telefônica, podemos dar uma declaração de tipo muito bonita e descritiva.

```{.haskell:hs}
inPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool
inPhoneBook name pnumber pbook = (name,pnumber) `elem` pbook
```

Se decidíssemos não usar sinônimos de tipo, nossa função teria um tipo de `String -> String -> [(String,String)] -> Bool`.
Nesse caso, a declaração de tipo que aproveitou os sinônimos do tipo é mais fácil de entender.
No entanto, você não deve exagerar com eles.
Introduzimos sinônimos de tipo para descrever o que algum tipo existente representa em nossas funções (e, portanto, nossas declarações de tipo se tornam uma documentação melhor) ou quando algo tem um tipo longo que é repetido muito (como `[(String,String)]`), mas representa algo mais específico no contexto de nossas funções.

Os sinônimos de tipo também podem ser parametrizados.
Se queremos um tipo que represente um tipo de lista de associação, mas ainda queremos que seja geral para que possa usar qualquer tipo como chaves e valores, podemos fazer isso:

```{.haskell:hs}
type AssocList k v = [(k,v)]
```

Agora, uma função que obtém o valor por uma chave em uma lista de associação pode ter um tipo de `(Eq k) => k -> AssocList k v -> Maybe v`.
`AssocList` é um construtor de tipos que recebe dois tipos e produz um tipo concreto, como `AssocList Int String`, por exemplo.

::: {.hintbox}
**Fonzie diz:** Aaay!
Quando falo em *tipos concretos*, quero dizer tipos totalmente aplicados, como `Map Int String` ou se estamos lidando com uma delas funções polimórficas, `[a]` ou `(Ord a) => Maybe a` e outras coisas.
E tipo, às vezes eu e meus amigos dizemos que `Maybe` é um tipo, mas não queremos dizer isso, porque todo idiota sabe que `Maybe` é um construtor de tipos.
Quando aplico um tipo extra a `Maybe`, como `Maybe String`, tenho um tipo concreto.
Você sabe, valores só podem ter tipos que são tipos concretos!
Portanto, em conclusão, viva rápido, ame intensamente e não deixe ninguém usar seu pente!
:::

Assim como podemos aplicar parcialmente funções para obter novas funções, podemos aplicar parcialmente parâmetros de tipo e obter novos construtores de tipos a partir deles.

Assim como chamamos uma função com poucos parâmetros para obter uma nova função, podemos especificar um construtor de tipos com poucos parâmetros de tipo e obter um construtor de tipos parcialmente aplicado.
Se quiséssemos um tipo que representasse um mapa (de `Data.Map`) de inteiros para algo, poderíamos fazer isso:

```{.haskell:hs}
type IntMap v = Map Int v
```

Ou poderíamos fazer assim:

```{.haskell:hs}
type IntMap = Map Int
```

De qualquer maneira, o construtor de tipos `IntMap` recebe um parâmetro e esse é o tipo do que os inteiros apontarão.

::: {.hintbox}
**Ah sim**.
Se você tentar implementar isso, provavelmente precisará fazer uma importação qualificada de `Data.Map`.
Quando você faz uma importação qualificada, os construtores de tipos também devem ser precedidos por um nome de módulo.
Então você escreveria `type IntMap = Map.Map Int`.
:::

Certifique-se de realmente entender a distinção entre construtores de tipos e construtores de valor.
Só porque criamos um sinônimo de tipo chamado `IntMap` ou `AssocList` não significa que podemos fazer coisas como `AssocList [(1,2),(4,5),(7,9)]`.
Tudo o que significa é que podemos nos referir ao seu tipo usando nomes diferentes.
Podemos fazer `[(1,2),(3,5),(8,9)] :: AssocList Int Int`, que fará com que os números assumam um tipo de `Int`, mas ainda podemos usar essa lista como faríamos com qualquer lista normal que tenha pares de inteiros dentro.
Sinônimos de tipo (e tipos geralmente) só podem ser usados na parte de tipo do Haskell.
Estamos na parte de tipos do Haskell sempre que estamos definindo novos tipos (portanto, em declarações *data* e *type*) ou quando estamos localizados após um `::`.
O `::` está em declarações de tipo ou em anotações de tipo.

Outro tipo de dados muito legal que leva dois tipos como parâmetros é o tipo `Either a b`.
É mais ou menos assim que ele é definido:

```{.haskell:hs}
data Either a b = Left a | Right b deriving (Eq, Ord, Read, Show)
```

Tem dois construtores de valor.
Se o `Left` for usado, seu conteúdo será do tipo `a` e se o `Right` for usado, seu conteúdo será do tipo `b`.
Portanto, podemos usar esse tipo para encapsular um valor de um tipo ou outro e, quando obtemos um valor do tipo `Either a b`, geralmente fazemos pattern matching em `Left` e `Right` e fazemos coisas diferentes com base em qual deles era.

```{.haskell:hs}
ghci> Right 20
Right 20
ghci> Left "w00t"
Left "w00t"
ghci> :t Right 'a'
Right 'a' :: Either a Char
ghci> :t Left True
Left True :: Either Bool b
```

Até agora, vimos que `Maybe a` era usado principalmente para representar os resultados de cálculos que poderiam ter falhado ou não.
Mas às vezes, `Maybe a` não é bom o suficiente porque `Nothing` não transmite muita informação além de que algo falhou.
Isso é legal para funções que podem falhar de apenas uma maneira ou se simplesmente não estamos interessados em como e por que falharam.
Uma pesquisa em `Data.Map` falha apenas se a chave que estávamos procurando não estava no mapa, então sabemos exatamente o que aconteceu.
No entanto, quando estamos interessados em como alguma função falhou ou por que, geralmente usamos o tipo de resultado de `Either a b`, onde `a` é algum tipo de tipo que pode nos dizer algo sobre a possível falha e `b` é o tipo de um cálculo bem-sucedido.
Portanto, os erros usam o construtor de valor `Left`, enquanto os resultados usam `Right`.

Um exemplo: uma escola secundária tem armários para que os alunos tenham algum lugar para colocar seus pôsteres do Guns'n'Roses.
Cada armário tem uma combinação de código.
Quando um aluno deseja um novo armário, ele diz ao supervisor do armário qual número de armário deseja e ele lhes dá o código.
No entanto, se alguém já estiver usando esse armário, ele não poderá dizer o código do armário e eles terão que escolher um diferente.
Usaremos um mapa de `Data.Map` para representar os armários.
Ele mapeará dos números de armários para um par de se o armário está em uso ou não e o código do armário.

```{.haskell:hs}
import qualified Data.Map as Map

data LockerState = Taken | Free deriving (Show, Eq)

type Code = String

type LockerMap = Map.Map Int (LockerState, Code)
```

Coisas simples.
Introduzimos um novo tipo de dados para representar se um armário é tomado ou gratuito e fazemos um sinônimo de tipo para o código do armário.
Também criamos um sinônimo de tipo para o tipo que mapeia de números inteiros para pares de estado e código de armário.
E agora, vamos criar uma função que procure o código em um mapa de armário.
Vamos usar um tipo `Either String Code` para representar nosso resultado, porque nossa pesquisa pode falhar de duas maneiras --- o armário pode ser tomado, caso em que não podemos contar o código ou o número do armário pode não existir.

Se a pesquisa falhar, vamos usar uma `String` para contar o que aconteceu.

```{.haskell:hs}
lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNumber map =
    case Map.lookup lockerNumber map of
        Nothing -> Left $ "Locker number " ++ show lockerNumber ++ " doesn't exist!"
        Just (state, code) -> if state /= Taken
                                then Right code
                                else Left $ "Locker " ++ show lockerNumber ++ " is already taken!"
```

Fazemos uma pesquisa normal no mapa.
Se recebermos um `Nothing`, retornaremos um valor do tipo `Left String`, dizendo que o armário não existe.
Se o encontrarmos, fazemos uma verificação adicional para ver se o armário é levado.
Se for, retorne um `Left` dizendo que já foi tomado.
Ao contrário, retornamos um valor do tipo `Right Code`, no qual damos ao aluno o código correto para o armário.
Na verdade, é um `Right String`, mas introduzimos esse sinônimo de tipo para introduzir alguma documentação adicional na declaração do tipo.
Aqui está um mapa de exemplo:

```{.haskell:hs}
lockers :: LockerMap
lockers = Map.fromList
    [(100,(Taken,"ZD39I"))
    ,(101,(Free,"JAH3I"))
    ,(103,(Free,"IQSA9"))
    ,(105,(Free,"QOTSA"))
    ,(109,(Taken,"893JJ"))
    ,(110,(Taken,"99292"))
    ]
```

Agora vamos tentar procurar alguns códigos de armário.

```{.haskell:hs}
ghci> lockerLookup 101 lockers
Right "JAH3I"
ghci> lockerLookup 100 lockers
Left "Locker 100 is already taken!"
ghci> lockerLookup 102 lockers
Left "Locker number 102 doesn't exist!"
ghci> lockerLookup 110 lockers
Left "Locker 110 is already taken!"
ghci> lockerLookup 105 lockers
Right "QOTSA"
```

Poderíamos ter usado um `Maybe a` para representar o resultado, mas não saberíamos por que não conseguimos o código.
Mas agora temos informações sobre a falha no nosso tipo de resultado.

## Estruturas de dados recursivas (Recursive data structures) {#recursive-data-structures}

![the fonz](assets/images/making-our-own-types-and-typeclasses/thefonz.png){.right width=168 height=301}

Como vimos, um construtor em um tipo de dado algébrico pode ter vários campos (ou nenhum) e cada campo deve ser de algum tipo concreto.
Com isso em mente, podemos criar tipos cujos construtores têm campos do mesmo tipo!
Usando isso, podemos criar tipos de dados recursivos, onde um valor de algum tipo contém valores desse tipo, que por sua vez contêm mais valores do mesmo tipo e assim por diante.

Pense nesta lista: `[5]`.
Isso é apenas açúcar sintático para `5:[]`.
No lado esquerdo do `:`, há um valor e no lado direito, há uma lista.
E, neste caso, é uma lista vazia.
Agora, e a lista `[4,5]`?
Bem, isso desaçucara para `4:(5:[])`.
Olhando para o primeiro `:`, vemos que ele também tem um elemento no lado esquerdo e uma lista (`5:[]`) no lado direito.
O mesmo vale para uma lista como `3:(4:(5:6:[]))`, que pode ser escrita assim ou como `3:4:5:6:[]` (porque `:` é associativo à direita) ou `[3,4,5,6]`.

Poderíamos dizer que uma lista pode ser uma lista vazia ou pode ser um elemento unido a um `:` com outra lista (que pode ser a lista vazia ou não).

Vamos usar tipos de dados algébricos para implementar nossa própria lista então!

```{.haskell:hs}
data List a = Empty | Cons a (List a) deriving (Show, Read, Eq, Ord)
```

Isso soa exatamente como a nossa definição de listas de um dos parágrafos anteriores.
É uma lista vazia ou uma combinação de uma cabeça com algum valor e uma lista.
Se você está confuso sobre isso, pode achar mais fácil entender na sintaxe de registro.

```{.haskell:hs}
data List a = Empty | Cons { listHead :: a, listTail :: List a} deriving (Show, Read, Eq, Ord)
```

Você também pode estar confuso sobre o construtor `Cons` aqui.
*cons* é outra palavra para `:`.
Você vê, em listas, `:` é na verdade um construtor que assume um valor e outra lista e retorna uma lista.
Já podemos usar nosso novo tipo de lista!
Em outras palavras, possui dois campos.
Um campo é do tipo de `a` e o outro é do tipo `[a]`.

```{.haskell:hs}
ghci> Empty
Empty
ghci> 5 `Cons` Empty
Cons 5 Empty
ghci> 4 `Cons` (5 `Cons` Empty)
Cons 4 (Cons 5 Empty)
ghci> 3 `Cons` (4 `Cons` (5 `Cons` Empty))
Cons 3 (Cons 4 (Cons 5 Empty))
```

Chamamos nosso construtor `Cons` de maneira infixa para que você possa ver como é exatamente como `:`.
`Empty` é como `[]` e ``4 `Cons` (5 `Cons` Empty)`` é como `4:(5:[])`.

Podemos definir funções para serem infixas automaticamente, fazendo com que consistam apenas em caracteres especiais.
Também podemos fazer o mesmo com construtores, pois são apenas funções que retornam um tipo de dados.
Então confira isso.

```{.haskell:hs}
infixr 5 :-:
data List a = Empty | a :-: (List a) deriving (Show, Read, Eq, Ord)
```

Primeiro de tudo, notamos uma nova construção sintática, as declarações de fixidade.
Quando definimos funções como operadores, podemos usar isso para dar a elas uma fixidade (mas não precisamos).
Uma fixidade afirma o quão fortemente o operador se liga e se é associativo à esquerda ou à direita.
Por exemplo, a fixidade de `*` é `infixl 7 *` e a fixidade de `+` é `infixl 6`.
Isso significa que ambos são associativos à esquerda (`4 * 3 * 2` é `(4 * 3) * 2`), mas `*` liga mais forte que `+`, porque tem uma fixidade maior, então `5 * 4 + 3` é `(5 * 4) + 3`.

Caso contrário, acabamos de escrever `a :-: (List a)` em vez de `Cons a (List a)`.
Agora, podemos escrever listas em nosso tipo de lista assim:

```{.haskell:hs}
ghci> 3 :-: 4 :-: 5 :-: Empty
(:-:) 3 ((:-:) 4 ((:-:) 5 Empty))
ghci> let a = 3 :-: 4 :-: 5 :-: Empty
ghci> 100 :-: a
(:-:) 100 ((:-:) 3 ((:-:) 4 ((:-:) 5 Empty)))
```

Ao derivar `Show` para o nosso tipo, o Haskell ainda o exibirá como se o construtor fosse uma função de prefixo, daí os parênteses ao redor do operador (lembre-se, `4 + 3` é `(+) 4 3`).

Vamos fazer uma função que adicione duas de nossas listas juntas.
É assim que `++` é definido para listas normais:

```{.haskell:hs}
infixr 5  ++
(++) :: [a] -> [a] -> [a]
[]     ++ ys = ys
(x:xs) ++ ys = x : (xs ++ ys)
```

Então, vamos roubar isso para nossa própria lista.
Nomearemos a função `.++`.

```{.haskell:hs}
infixr 5  .++
(.++) :: List a -> List a -> List a
Empty .++ ys = ys
(x :-: xs) .++ ys = x :-: (xs .++ ys)
```

E vamos ver se funciona ...

```{.haskell:hs}
ghci> let a = 3 :-: 4 :-: 5 :-: Empty
ghci> let b = 6 :-: 7 :-: Empty
ghci> a .++ b
(:-:) 3 ((:-:) 4 ((:-:) 5 ((:-:) 6 ((:-:) 7 Empty))))
```

Legal.

Se quiséssemos, poderíamos implementar todas as funções que operam em listas em nosso próprio tipo de lista.

Observe como fizemos pattern matching em `(x :-: xs)`.
Isso funciona porque a correspondência de padrões é realmente sobre construtores correspondentes.
Podemos combinar em `:-:` porque é um construtor para o nosso próprio tipo de lista e também podemos corresponder a `:` porque é um construtor para o tipo de lista interna.
O mesmo vale para `[]`.
Como a correspondência de padrões funciona (apenas) nos construtores, podemos corresponder a coisas assim, construtores prefixados normais ou coisas como `8` ou `'a'`, que são basicamente construtores para os tipos numéricos e de caracteres, respectivamente.

![binary search tree](assets/images/making-our-own-types-and-typeclasses/binarytree.png){.left width=323 height=225}

Agora, vamos implementar uma **árvore de busca binária**.
Se você não está familiarizado com as árvores de busca binária de idiomas como C, aqui está o que elas são: um elemento aponta para dois elementos, um à esquerda e outro à direita.
O elemento à esquerda é menor, o elemento à direita é maior.
Cada um desses elementos também pode apontar para dois elementos (ou um ou nenhum).
Com efeito, cada elemento tem até duas subárvores.
E uma coisa legal sobre as árvores de busca binária é que sabemos que todos os elementos na subárvore esquerda de, digamos, 5 serão menores que 5.
Elementos em sua subárvore certa serão maiores.
Portanto, se precisarmos descobrir se 8 está em nossa árvore, começaríamos com 5 e, porque 8 é maior que 5, iríamos para a direita.
Estamos agora no 7 e porque 8 é maior que 7, vamos para a direita novamente.
E encontramos nosso elemento em três saltos!
Agora, se fosse uma lista normal (ou uma árvore, mas realmente desequilibrada), levaria sete lúpulos em vez de três para ver se 8 está lá.

Conjuntos e mapas de `Data.Set` e `Data.Map` são implementados usando árvores, apenas em vez de árvores de busca binária normais, eles usam árvores de busca binária equilibradas, que são sempre equilibradas.
Mas agora, estaremos apenas implementando árvores de busca binária normais.

Aqui está o que vamos dizer: uma árvore é uma árvore vazia ou é um elemento que contém algum valor e duas árvores.
Parece um ajuste perfeito para um tipo de dados algébrico!

```{.haskell:hs}
data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)
```

Ok, bom, isso é bom.
Em vez de construir manualmente uma árvore, faremos uma função que pega uma árvore e um elemento e insere um elemento.
Fazemos isso comparando o valor que queremos inserir no nó raiz e, se for menor, vamos para a esquerda, se for maior, vamos para a direita.
Fazemos o mesmo para cada nó subsequente até chegarmos a uma árvore vazia.
Depois de chegarmos a uma árvore vazia, apenas inserimos um nó com esse valor em vez da árvore vazia.

Em linguagens como C, faríamos isso modificando os ponteiros e valores dentro da árvore.
No Haskell, não podemos realmente modificar nossa árvore, por isso temos que criar uma nova subárvore a cada vez que decidimos ir para a esquerda ou direita e, no final, a função de inserção retorna uma árvore completamente nova, porque o Haskell realmente não tem um conceito de ponteiro, apenas valores.
Portanto, o tipo para nossa função de inserção será algo como `a -> Tree a -> Tree a`.
Pega um elemento e uma árvore e retorna uma nova árvore que tem esse elemento dentro.
Isso pode parecer ineficiente, mas a preguiça cuida desse problema.

Então, aqui estão duas funções.
Uma é uma função de utilidade para criar uma árvore singleton (uma árvore com apenas um nó) e uma função para inserir um elemento em uma árvore.

```{.haskell:hs}
singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right)
    | x == a = Node x left right
    | x < a  = Node a (treeInsert x left) right
    | x > a  = Node a left (treeInsert x right)
```

A função `singleton` é apenas um atalho para criar um nó que tenha algo e depois duas subárvores vazias.
Na função de inserção, primeiro temos a condição de borda como um padrão.
Se chegamos a uma subárvore vazia, isso significa que estamos onde queremos e, em vez da árvore vazia, colocamos uma árvore singleton com nosso elemento.
Se não estamos inserindo em uma árvore vazia, temos que verificar algumas coisas.
Primeiro, se o elemento que estamos inserindo for igual ao elemento raiz, basta retornar uma árvore que seja a mesma.
Se for menor, retorne uma árvore que tenha o mesmo valor da raiz, a mesma subárvore direita, mas em vez de sua subárvore esquerda, coloque uma árvore que tenha nosso valor inserido nela.

O mesmo (mas o contrário) acontece se nosso valor for maior que o elemento raiz.

Em seguida, faremos uma função que verifique se algum elemento está na árvore.
Primeiro, vamos definir a condição de borda.
Se estamos procurando um elemento em uma árvore vazia, certamente não está lá.
OK.
Observe como isso é o mesmo que a condição de borda ao procurar elementos nas listas.
Se estamos procurando um elemento em uma lista vazia, não está lá.
De qualquer forma, se não estamos procurando um elemento em uma árvore vazia, verificamos algumas coisas.
Se o elemento no nó raiz é o que estamos procurando, ótimo!
Se não for, então o que?
Bem, podemos tirar proveito de saber que todos os elementos esquerdos são menores que o nó raiz.
Portanto, se o elemento que estamos procurando é menor que o nó raiz, verifique se está na subárvore esquerda.
Se for maior, verifique se está na subárvore direita.

```{.haskell:hs}
treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node a left right)
    | x == a = True
    | x < a  = treeElem x left
    | x > a  = treeElem x right
```

Tudo o que tínhamos que fazer era escrever o parágrafo anterior em código.
Vamos nos divertir com nossas árvores!
Em vez de construir manualmente uma (embora pudéssemos), usaremos uma dobra (fold) para construir uma árvore a partir de uma lista.
Lembre-se, praticamente tudo o que atravessa uma lista um por um e depois retorna algum tipo de valor pode ser implementado com uma dobra!
Vamos começar com a árvore vazia e depois abordar uma lista da direita e apenas inserir elemento após elemento em nossa árvore acumuladora.

```{.haskell:hs}
ghci> let nums = [8,6,4,1,7,3,5]
ghci> let numsTree = foldr treeInsert EmptyTree nums
ghci> numsTree
Node 5 (Node 3 (Node 1 EmptyTree EmptyTree) (Node 4 EmptyTree EmptyTree)) (Node 7 (Node 6 EmptyTree EmptyTree) (Node 8 EmptyTree EmptyTree))
```

Nesse `foldr`, `treeInsert` era a função de dobra (leva uma árvore e um elemento de lista e produz uma nova árvore) e `EmptyTree` era o acumulador inicial.
`nums`, é claro, era a lista sobre a qual estávamos dobrando.

Quando imprimimos nossa árvore no console, não é muito legível, mas se tentarmos, podemos distinguir sua estrutura.
Vemos que o nó raiz é 5 e, em seguida, possui duas subárvores, uma das quais tem o nó raiz de 3 e a outra a 7, etc.

```{.haskell:hs}
ghci> 8 `treeElem` numsTree
True
ghci> 100 `treeElem` numsTree
False
ghci> 1 `treeElem` numsTree
True
ghci> 10 `treeElem` numsTree
False
```

A verificação de associação também funciona muito bem.
Legal.

Como você pode ver, as estruturas de dados algébricos são um conceito muito legal e poderoso em Haskell.
Podemos usá-las para fazer qualquer coisa, desde valores booleanos e enumerações durante a semana até árvores de busca binária e muito mais!

## Typeclasses 102 {#typeclasses-102}

![tweet](assets/images/making-our-own-types-and-typeclasses/trafficlight.png){.right width=175 height=480}

Até agora, aprendemos sobre algumas das typeclasses padrão de Haskell e vimos quais tipos estão nelas.
Também aprendemos como tornar automaticamente nossos próprios tipos instâncias das typeclasses padrão, pedindo ao Haskell para derivar as instâncias para nós.
Nesta seção, aprenderemos como fazer nossas próprias typeclasses e como fazer com que as instâncias de tipos delas manualmente.

Uma rápida recapitulação sobre typeclasses: typeclasses são como interfaces.
Uma typeclass define algum comportamento (como comparar a igualdade, comparar pedidos, enumeração) e, em seguida, tipos que podem se comportar dessa maneira são feitos instâncias dessa typeclass.
O comportamento das typeclasses é alcançado definindo funções ou apenas declarações de tipo que implementamos.
Portanto, quando dizemos que um tipo é uma instância de uma typeclass, queremos dizer que podemos usar as funções que a typeclass define com esse tipo.

Typeclasses não têm quase nada a ver com classes em linguagens como Java ou Python.
Isso confunde muitas pessoas, então eu quero que você esqueça tudo o que sabe sobre as classes em linguagens imperativas agora.

Por exemplo, a typeclass `Eq` é para coisas que podem ser equiparadas.
Define as funções `==` e `/=`.
Se tivermos um tipo (digamos, `Car`) e comparar dois carros com a função da igualdade `==` faz sentido, faz sentido que `Car` seja uma instância de `Eq`.

É assim que a classe `Eq` é definida no prelude padrão:

```{.haskell:hs}
class Eq a where
    (==) :: a -> a -> Bool
    (/=) :: a -> a -> Bool
    x == y = not (x /= y)
    x /= y = not (x == y)
```

Woah, woah, woah!
Alguma sintaxe e palavras-chave estranhas novas lá!
Não se preocupe, tudo isso ficará claro em um segundo.
Primeiro, quando escrevemos `class Eq a where`, isso significa que estamos definindo uma nova typeclass e que se chama `Eq`.
O `a` é a variável de tipo e significa que `a` desempenhará o papel do tipo que em breve estaremos criando uma instância de `Eq`.
Não precisa ser chamado de `a`, nem precisa ser uma letra, apenas precisa ser uma palavra minúscula.
Em seguida, definimos várias funções.
Não é obrigatório implementar os próprios corpos de função, apenas precisamos especificar as declarações de tipo para as funções.

::: {.hintbox}
Algumas pessoas podem entender isso melhor se escrevêssemos `class Eq equatable where` e depois especifiquemos as declarações de tipo como `(==) :: equatable -> equatable -> Bool`.
:::

De qualquer forma, *implementamos* os corpos de função para as funções que definem `Eq`, apenas as definimos em termos de recursão mútua.
Dissemos que duas instâncias de `Eq` são iguais se não forem diferentes e são diferentes se não forem iguais.
Nós não precisávamos fazer isso, na verdade, mas fizemos e veremos como isso nos ajuda em breve.

::: {.hintbox}
Se dissermos `class Eq a where` e, em seguida, definirmos uma declaração de tipo dentro dessa classe como `(==) :: a -> a -> Bool`, então, quando examinarmos o tipo dessa função mais tarde, ele terá o tipo de `(Eq a) => a -> a -> Bool`.
:::

Então, uma vez que temos uma classe, o que podemos fazer com ela?
Bem, não muito, realmente.
Mas assim que começamos a fazer instâncias de tipos dessa classe, começamos a obter alguma funcionalidade agradável.
Então confira esse tipo:

```{.haskell:hs}
data TrafficLight = Red | Yellow | Green
```

Ele define os estados de um semáforo.
Observe como não derivamos nenhuma instância de classe para ela.
Isso porque vamos escrever algumas instâncias manualmente, mesmo que pudéssemos derivá-las para tipos como `Eq` e `Show`.
Veja como tornamos isso uma instância de `Eq`.

```{.haskell:hs}
instance Eq TrafficLight where
    Red == Red = True
    Green == Green = True
    Yellow == Yellow = True
    _ == _ = False
```

Fizemos isso usando a palavra-chave *instance*.
Portanto, *class* é para definir novas typeclasses e *instance* é para tornar nossos tipos instâncias de typeclasses.
Quando estávamos definindo `Eq`, escrevemos `class Eq a where` e dissemos que `a` desempenha o papel de qualquer tipo que será feito uma instância posteriormente.
Podemos ver claramente aqui, porque quando estamos fazendo uma instância, escrevemos `instance Eq TrafficLight where`.
Substituímos o `a` pelo tipo real.

Como `==` foi definido em termos de `/=` e vice-versa na declaração de *class*, apenas tivemos que substituir um deles na declaração da instância.
Isso é chamado de definição completa mínima para a typeclass --- o mínimo de funções que temos que implementar para que nosso tipo possa se comportar como a classe anuncia.
Para cumprir a definição completa mínima para `Eq`, temos que substituir um de `==` ou `/=`.
Se `Eq` fosse definido simplesmente assim:

```{.haskell:hs}
class Eq a where
    (==) :: a -> a -> Bool
    (/=) :: a -> a -> Bool
```

teríamos que implementar ambas as funções ao tornar um tipo uma instância dela, porque Haskell não saberia como essas duas funções estão relacionadas.
A definição completa mínima seria então: `==` e `/=`.

Você pode ver que implementamos `==` simplesmente fazendo correspondência de padrões.
Como há muito mais casos em que duas luzes não são iguais, especificamos as que são iguais e depois fizemos um padrão geral, dizendo que, se não for nenhuma das combinações anteriores, duas luzes não serão iguais.

Vamos tornar isso uma instância de `Show` à mão também.
Para satisfazer a definição completa mínima de `Show`, só precisamos implementar sua função `show`, que assume um valor e o transforma em uma string.

```{.haskell:hs}
instance Show TrafficLight where
    show Red = "Red light"
    show Yellow = "Yellow light"
    show Green = "Green light"
```

Mais uma vez, usamos a correspondência de padrões para alcançar nossos objetivos.
Vamos ver como funciona em ação:

```{.haskell:hs}
ghci> Red == Red
True
ghci> Red == Yellow
False
ghci> Red `elem` [Red, Yellow, Green]
True
ghci> [Red, Yellow, Green]
[Red light,Yellow light,Green light]
```

Legal.
Poderíamos ter acabado de derivar `Eq` e isso teria o mesmo efeito (mas não o fizemos para fins educacionais).
No entanto, derivar `Show` teria traduzido diretamente os construtores de valor em strings.
Mas se queremos que as luzes pareçam `"Red light"`, precisamos fazer a declaração da instância manualmente.

Você também pode criar typeclasses que são subclasses de outras typeclasses.
A declaração de *class* para `Num` é um pouco longa, mas aqui está a primeira parte:

```{.haskell:hs}
class (Eq a) => Num a where
   ...
```

Como mencionamos anteriormente, existem muitos lugares onde podemos agrupar restrições de classe.
Portanto, isso é exatamente como escrever `class Num a where`, apenas afirmamos que nosso tipo `a` deve ser uma instância de `Eq`.
Estamos essencialmente dizendo que temos que fazer de um tipo uma instância de `Eq` antes de podermos torná-lo uma instância de `Num`.
Antes que algum tipo possa ser considerado um número, faz sentido que possamos determinar se os valores desse tipo podem ser equiparados ou não.
Isso é tudo o que existe para subclassificação, é apenas uma restrição de classe em uma declaração *class*!
Ao definir corpos de função na declaração *class* ou ao defini-los nas declarações *instance*, podemos supor que `a` faz parte de `Eq` e, portanto, podemos usar `==` nos valores desse tipo.

Mas como os tipos `Maybe` ou lista são feitos como instâncias de typeclasses?
O que torna o `Maybe` diferente de, digamos, `TrafficLight` é que `Maybe` em si não é um tipo concreto, é um construtor de tipos que leva um parâmetro de tipo (como `Char` ou algo assim) para produzir um tipo concreto (como `Maybe Char`).
Vamos dar uma olhada na typeclass `Eq` novamente:

```{.haskell:hs}
class Eq a where
    (==) :: a -> a -> Bool
    (/=) :: a -> a -> Bool
    x == y = not (x /= y)
    x /= y = not (x == y)
```

Das declarações de tipo, vemos que o `a` é usado como um tipo concreto porque todos os tipos em funções precisam ser concretos (lembre-se, você não pode ter uma função do tipo `a -> Maybe`, mas você pode ter uma função de `a -> Maybe a` ou `Maybe Int -> Maybe String`).
É por isso que não podemos fazer algo como

```{.haskell:hs}
instance Eq Maybe where
    ...
```

Porque, como vimos, o `a` tem que ser um tipo concreto, mas `Maybe` não é um tipo concreto.
É um construtor de tipos que recebe um parâmetro e depois produz um tipo concreto.
Também seria tedioso escrever `instance Eq (Maybe Int) where`, `instance Eq (Maybe Char) where`, etc. para todos os tipos de todos.
Então, poderíamos escrever assim:

```{.haskell:hs}
instance Eq (Maybe m) where
    Just x == Just y = x == y
    Nothing == Nothing = True
    _ == _ = False
```

Isso é como dizer que queremos tornar todos os tipos do formulário `Maybe something` uma instância de `Eq`.
Na verdade, poderíamos ter escrito `(Maybe something)`, mas geralmente optamos por letras únicas para serem fiéis ao estilo Haskell.
O `(Maybe m)` aqui desempenha o papel de `a` de `class Eq a where`.
Embora `Maybe` não seja um tipo concreto, `Maybe m` é.

Ao especificar um parâmetro de tipo (`m`, que está em minúsculas), dissemos que queremos que todos os tipos que estejam na forma de `Maybe m`, onde `m` seja qualquer tipo, seja uma instância de `Eq`.

Há um problema com isso.
Você pode identificá-lo?
Usamos `==` no conteúdo de `Maybe`, mas não temos garantia de que o que o `Maybe` contém pode ser usado com `Eq`!
É por isso que temos que modificar nossa declaração *instance* assim:

```{.haskell:hs}
instance (Eq m) => Eq (Maybe m) where
    Just x == Just y = x == y
    Nothing == Nothing = True
    _ == _ = False
```

Tivemos que adicionar uma restrição de classe!
Com esta declaração *instance*, dizemos o seguinte: queremos que todos os tipos da forma `Maybe m` façam parte da typeclass `Eq`, mas apenas esses tipos onde o `m` (o que está contido dentro do `Maybe`) também faz parte de `Eq`.
É assim que Haskell derivaria a instância também.

Na maioria das vezes, as restrições de classe nas declarações *class* são usadas para tornar uma typeclass uma subclasse de outra typeclass e restrições de classe nas declarações *instance* são usadas para expressar requisitos sobre o conteúdo de algum tipo.
Por exemplo, aqui exigimos que o conteúdo de `Maybe` também faça parte da typeclass `Eq`.

Ao fazer instâncias, se você vir que um tipo é usado como um tipo concreto nas declarações de tipo (como o `a` em `a -> a -> Bool`), você deve fornecer parâmetros de tipo e adicionar parênteses para que você acabe com um tipo concreto.

::: {.hintbox}
Leve em consideração que o tipo que você está tentando criar uma instância substituirá o parâmetro na declaração *class*.
O `a` de `class Eq a where` será substituído por um tipo real quando você fizer uma instância; portanto, tente colocar mentalmente seu tipo nas declarações de tipo de função também.
`(==) :: Maybe -> Maybe -> Bool` não faz muito sentido, mas `(==) :: (Eq m) => Maybe m -> Maybe m -> Bool` faz.
Mas isso é apenas algo para se pensar, porque `==` sempre terá um tipo de `(==) :: (Eq a) => a -> a -> Bool`, não importa quais instâncias façamos.
:::

Ooh, mais uma coisa, confira isso!
Se você quiser ver quais são as instâncias de uma typeclass, basta fazer `:info YourTypeClass` no GHCI.
Portanto, digitar `:info Num` mostrará quais funções a typeclass define e fornecerá uma lista dos tipos na typeclass.
`:info` funciona para tipos e construtores de tipos também.
Se você fizer `:info Maybe`, ele mostrará todas as typeclasses das quais `Maybe` é uma instância.
Também `:info` pode mostrar a declaração de tipo de uma função.
Eu acho isso muito legal.

## Uma typeclass sim-não (A yes-no typeclass) {#a-yes-no-typeclass}

![yesno](assets/images/making-our-own-types-and-typeclasses/yesno.png){.left width=201 height=111}

Em JavaScript e algumas outras linguagens de tipo fraco, você pode colocar quase tudo dentro de uma expressão `if`.
Por exemplo, você pode fazer todas as seguintes opções: `if (0) alert("YEAH!") else alert("NO!")`, `if ("") alert ("YEAH!") else alert("NO!")`, `if (false) alert("YEAH") else alert("NO!)`, etc. e todos estes vão lançar um alerta de `NO!`.
Se você fizer `if ("WHAT") alert ("YEAH") else alert("NO!")`, ele alertará um `"YEAH!"` porque o JavaScript considera que strings não vazias são uma espécie de valor verdadeiro.

Embora o uso estritamente de `Bool` para a semântica booleana funcione melhor em Haskell, vamos tentar implementar esse comportamento JavaScript-ish de qualquer maneira.
Por diversão!
Vamos começar com uma declaração de *class*.

```{.haskell:hs}
class YesNo a where
    yesno :: a -> Bool
```

Muito simples.
A typeclass `YesNo` define uma função.
Essa função assume um valor de um tipo que pode ser considerado para manter algum conceito de veracidade e nos diz com certeza se é verdade ou não.
Observe que, da maneira como usamos o `a` na função, `a` deve ser um tipo concreto.

Em seguida, vamos definir algumas instâncias.
Para números, assumiremos que (como em JavaScript) qualquer número que não seja 0 é verdadeiro e 0 é falso.

```{.haskell:hs}
instance YesNo Int where
    yesno 0 = False
    yesno _ = True
```

Listas vazias (e, por extensões, strings) são um valor não-ish, enquanto listas não vazias são um valor yes-ish.

```{.haskell:hs}
instance YesNo [a] where
    yesno [] = False
    yesno _ = True
```

Observe como apenas colocamos um parâmetro de tipo `a` lá para tornar a lista um tipo concreto, mesmo que não façamos suposições sobre o tipo contido na lista.
O que mais, hmm ...
Eu sei, `Bool` em si também mantém a verdade e a falsidade e é bastante óbvio qual é qual.

```{.haskell:hs}
instance YesNo Bool where
    yesno = id
```

Hã?
O que é `id`?
É apenas uma função da biblioteca padrão que aceita um parâmetro e retorna a mesma coisa, que é o que estaríamos escrevendo aqui de qualquer maneira.

Vamos fazer de `Maybe a` uma instância também.

```{.haskell:hs}
instance YesNo (Maybe a) where
    yesno (Just _) = True
    yesno Nothing = False
```

Não precisávamos de uma restrição de classe porque não fizemos suposições sobre o conteúdo do `Maybe`.
Acabamos de dizer que é verdadeiro se for um valor `Just` e falso se for um `Nothing`.
Ainda tínhamos que escrever `(Maybe a)` em vez de apenas `Maybe`, porque se você pensar sobre isso, uma função `Maybe -> Bool` não pode existir (porque `Maybe` não é um tipo concreto), enquanto uma `Maybe a -> Bool` é bom e elegante.
Ainda assim, isso é muito legal, porque agora, qualquer tipo de forma `Maybe something` faz parte de `YesNo` e não importa o que seja `something`.

Anteriormente, definimos um tipo `Tree a`, que representava uma árvore de busca binária.
Podemos dizer que uma árvore vazia é falsa e qualquer coisa que não seja uma árvore vazia é verdadeira.

```{.haskell:hs}
instance YesNo (Tree a) where
    yesno EmptyTree = False
    yesno _ = True
```

Um semáforo pode ser um valor sim ou não?
Certo.
Se estiver vermelho, pare.
Se estiver verde, vá.
Se for amarelo?
Eh, eu costumo correr nos amarelos porque vivo pela adrenalina.

```{.haskell:hs}
instance YesNo TrafficLight where
    yesno Red = False
    yesno _ = True
```

Legal, agora que temos alguns exemplos, vamos brincar!

```{.haskell:hs}
ghci> yesno $ length []
False
ghci> yesno "haha"
True
ghci> yesno ""
False
ghci> yesno $ Just 0
True
ghci> yesno True
True
ghci> yesno EmptyTree
False
ghci> yesno []
False
ghci> yesno [0,0,0]
True
ghci> :t yesno
yesno :: (YesNo a) => a -> Bool
```

Certo, funciona!
Vamos criar uma função que imite a instrução `if`, mas funciona com valores `YesNo`.

```{.haskell:hs}
yesnoIf :: (YesNo y) => y -> a -> a -> a
yesnoIf yesnoVal yesResult noResult = if yesno yesnoVal then yesResult else noResult
```

Bastante simples.
Pega um valor sim-não-ish e duas coisas.
Se o valor sim-não-ish for mais um sim, ele retornará a primeira das duas coisas, caso contrário, retornará a segunda delas.

```{.haskell:hs}
ghci> yesnoIf [] "YEAH!" "NO!"
"NO!"
ghci> yesnoIf [2,3,4] "YEAH!" "NO!"
"YEAH!"
ghci> yesnoIf True "YEAH!" "NO!"
"YEAH!"
ghci> yesnoIf (Just 500) "YEAH!" "NO!"
"YEAH!"
ghci> yesnoIf Nothing "YEAH!" "NO!"
"NO!"
```

## A typeclass Functor (The Functor typeclass) {#the-functor-typeclass}

Até agora, encontramos muitas das typeclasses na biblioteca padrão.
Brincamos com `Ord`, que é para coisas que podem ser encomendadas.
Conversamos com `Eq`, que é para coisas que podem ser equiparadas.
Vimos `Show`, que apresenta uma interface para tipos cujos valores podem ser exibidos como strings.
Nosso bom amigo `Read` está lá sempre que precisamos converter uma string em um valor de algum tipo.
E agora, vamos dar uma olhada na typeclass `Functor`{.label .class}, que é basicamente para coisas que podem ser mapeadas.
Você provavelmente está pensando em listas agora, já que o mapeamento sobre listas é um idioma dominante em Haskell.
E você está certo, o tipo de lista faz parte da typeclass `Functor`.

Qual a melhor maneira de conhecer a typeclass `Functor` do que ver como é implementada?
Vamos dar uma espiada.

```{.haskell:hs}
class Functor f where
    fmap :: (a -> b) -> f a -> f b
```

![I AM FUNCTOOOOR!!!](assets/images/making-our-own-types-and-typeclasses/functor.png){.right width=220 height=441}

Bem.
Vemos que ele define uma função, `fmap`, e não fornece nenhuma implementação padrão para ela.
O tipo de `fmap` é interessante.
Nas definições de typeclasses até agora, a variável de tipo que desempenhou o papel do tipo na typeclass era um tipo concreto, como o `a` em `(==) :: (Eq a) => a -> a -> Bool`.
Mas agora, o `f` não é um tipo concreto (um tipo que um valor pode conter, como `Int`, `Bool` ou `Maybe String`), mas um construtor de tipos que leva um parâmetro de tipo.
Um exemplo rápido de atualização: `Maybe Int` é um tipo concreto, mas `Maybe` é um construtor de tipos que usa um tipo como parâmetro.
De qualquer forma, vemos que `fmap` assume uma função de um tipo para outro e um functor aplicado com um tipo e retorna um functor aplicado com outro tipo.

Se isso parecer um pouco confuso, não se preocupe.
Tudo será revelado em breve quando verificarmos alguns exemplos.
Hmm, essa declaração de tipo para `fmap` me lembra algo.
Se você não sabe qual é a assinatura de tipo de `map`, é: `map :: (a -> b) -> [a] -> [b]`.

Ah, interessante!
É preciso uma função de um tipo para outro e uma lista de um tipo e retorna uma lista de outro tipo.
Meus amigos, acho que temos um functor!
De fato, `map` é apenas um `fmap` que funciona apenas em listas.
Veja como a lista é uma instância da typeclass `Functor`.

```{.haskell:hs}
instance Functor [] where
    fmap = map
```

É isso!
Observe como não escrevemos `instance Functor [a] where`, porque a partir de `fmap :: (a -> b) -> f a -> f b`, vemos que o `f` deve ser um construtor de tipos que leva um tipo.
`[a]` já é um tipo concreto (de uma lista com qualquer tipo dentro dela), enquanto `[]` é um construtor de tipos que leva um tipo e pode produzir tipos como `[Int]`, `[String]` ou até `[[String]]`.

Como para as listas, `fmap` é apenas `map`, obtemos os mesmos resultados ao usá-los nas listas.

```{.haskell:hs}
map :: (a -> b) -> [a] -> [b]
ghci> fmap (*2) [1..3]
[2,4,6]
ghci> map (*2) [1..3]
[2,4,6]
```

O que acontece quando fazemos `map` ou `fmap` sobre uma lista vazia?
Bem, é claro, recebemos uma lista vazia.
Apenas transforma uma lista vazia do tipo `[a]` em uma lista vazia do tipo `[b]`.

Tipos que podem agir como uma caixa podem ser functors.
Você pode pensar em uma lista como uma caixa que tem uma quantidade infinita de pequenos compartimentos e todos eles podem estar vazios, um pode estar cheio e os outros vazios ou vários deles podem estar cheios.
Então, o que mais tem as propriedades de ser como uma caixa?
Por um lado, o tipo `Maybe a`.
De certa forma, é como uma caixa que pode conter nada, em cujo caso tem o valor de `Nothing`, ou pode conter um item, como `"HAHA"`, caso em que ele tem um valor de `Just "HAHA"`.
Veja como `Maybe` é um functor.

```{.haskell:hs}
instance Functor Maybe where
    fmap f (Just x) = Just (f x)
    fmap f Nothing = Nothing
```

Mais uma vez, observe como escrevemos `instance Functor Maybe where` em vez de `instance Functor (Maybe m) where`, como fizemos quando estávamos lidando com `Maybe` e `YesNo`.
`Functor` quer um construtor de tipos que leve um tipo e não um tipo concreto.
Se você substituir mentalmente os `f`s por `Maybe`s, `fmap` age como um `(a -> b) -> Maybe a -> Maybe b` para esse tipo específico, o que parece OK.
Mas se você substituir `f` por `(Maybe m)`, pareceria agir como um `(a -> b) -> Maybe m a -> Maybe m b`, o que não faz nenhum sentido, porque `Maybe` leva apenas um parâmetro de tipo.

De qualquer forma, a implementação do `fmap` é bastante simples.

Se for um valor vazio de `Nothing`, basta retornar um `Nothing`.
Se mapearmos sobre uma caixa vazia, receberemos uma caixa vazia.
Faz sentido.
Assim como se mapearmos sobre uma lista vazia, recebemos uma lista vazia.
Se não é um valor vazio, mas um único valor acumulado em um `Just`, aplicamos a função no conteúdo do `Just`.

```{.haskell:hs}
ghci> fmap (++ " HEY GUYS IM INSIDE THE JUST") (Just "Something serious.")
Just "Something serious. HEY GUYS IM INSIDE THE JUST"
ghci> fmap (++ " HEY GUYS IM INSIDE THE JUST") Nothing
Nothing
ghci> fmap (*2) (Just 200)
Just 400
ghci> fmap (*2) Nothing
Nothing
```

Outra coisa que pode ser mapeada e feita uma instância de `Functor` é o nosso tipo `Tree a`.
Pode ser pensado como uma caixa (contém vários ou nenhum valor) e o construtor de tipos `Tree` recebe exatamente um parâmetro de tipo.
Se você observar `fmap` como se fosse uma função criada apenas para `Tree`, sua assinatura de tipo seria `(a -> b) -> Tree a -> Tree b`.
Vamos usar a recursão neste.
O mapeamento sobre uma árvore vazia produzirá uma árvore vazia.
O mapeamento sobre uma árvore não vazia será uma árvore que consiste em nossa função aplicada ao valor da raiz e suas subárvores esquerda e direita serão as subárvores anteriores, apenas nossa função será mapeada sobre elas.

```{.haskell:hs}
instance Functor Tree where
    fmap f EmptyTree = EmptyTree
    fmap f (Node x leftsub rightsub) = Node (f x) (fmap f leftsub) (fmap f rightsub)
```

```{.haskell:hs}
ghci> fmap (*2) EmptyTree
EmptyTree
ghci> fmap (*4) (foldr treeInsert EmptyTree [5,7,3,2,1,7])
Node 28 (Node 4 EmptyTree (Node 8 EmptyTree (Node 12 EmptyTree (Node 20 EmptyTree EmptyTree)))) EmptyTree
```

Legal!
Agora, e o `Either a b`?
Isso pode ser feito um functor?
A typeclass `Functor` deseja um construtor de tipos que leve apenas um parâmetro de tipo, mas `Either` leva dois.
Hmmm!
Eu sei, aplicaremos parcialmente o `Either`, alimentando apenas um parâmetro, para que ele tenha um parâmetro livre.
Veja como `Either a` é um functor nas bibliotecas padrão:

```{.haskell:hs}
instance Functor (Either a) where
    fmap f (Right x) = Right (f x)
    fmap f (Left x) = Left x
```

Bem, o que fizemos aqui?
Você pode ver como fizemos `Either a` uma instância em vez de apenas `Either`.
Isso ocorre porque `Either a` é um construtor de tipos que leva um parâmetro, enquanto `Either` leva dois.
Se `fmap` fosse especificamente para `Either a`, a assinatura do tipo seria `(b -> c) -> Either a b -> Either a c` porque é o mesmo que `(b -> c) -> (Either a) b -> (Either a) c`.
Na implementação, mapeamos no caso de um construtor de valor `Right`, mas não no caso de um `Left`.
Por que é que?
Bem, se olharmos para trás como o tipo `Either a b` é definido, é como:

```{.haskell:hs}
data Either a b = Left a | Right b
```

Bem, se quiséssemos mapear uma função sobre os dois, `a` e `b` teriam que ser do mesmo tipo.
Quero dizer, se tentássemos mapear uma função que pega uma string e retorna uma string e o `b` fosse uma string, mas o `a` fosse um número, isso realmente não funcionaria.
Além disso, ao ver o que o tipo `fmap` seria se operasse apenas nos valores `Either`, vemos que o primeiro parâmetro deve permanecer o mesmo, enquanto o segundo pode mudar e o primeiro parâmetro é atualizado pelo construtor de valor `Left`.

Isso também acompanha bem nossa analogia de caixa se pensarmos na parte `Left` como uma espécie de caixa vazia com uma mensagem de erro escrita ao lado, dizendo-nos por que está vazia.

Os mapas de `Data.Map` também podem ser feitos um functor porque possuem valores (ou não!).
No caso de `Map k v`, `fmap` mapeará uma função `v -> v'` sobre um mapa do tipo `Map k v` e retornará um mapa do tipo `Map k v'`.

::: {.hintbox}
Observe que o `'` não tem um significado especial em tipos, assim como não tem significado especial ao nomear valores.
É usado para denotar coisas semelhantes, apenas ligeiramente alteradas.
:::

Tente descobrir como `Map k` é feito uma instância de `Functor` por si mesmo!

Com a typeclass `Functor`, vimos como as typeclasses podem representar conceitos de ordem superior muito legais.
Também tivemos um pouco mais de prática aplicando parcialmente tipos e fazendo instâncias.
Em um dos próximos capítulos, também daremos uma olhada em algumas leis que se aplicam aos functors.

::: {.hintbox}
**Apenas mais uma coisa!**
Functors devem obedecer a algumas leis para que possam ter algumas propriedades das quais possamos depender e não pensar muito.
Se usarmos `fmap (+1)` sobre a lista `[1,2,3,4]`, esperamos que o resultado seja `[2,3,4,5]` e não o inverso, `[5,4,3,2]`.
Se usarmos `fmap (\a -> a)` (a função identidade, que apenas retorna seu parâmetro) em alguma lista, esperamos recuperar a mesma lista como resultado.
Por exemplo, se dermos a instância incorreta do functor aos nosso tipo `Tree`, usando `fmap` sobre uma árvore onde a subárvore esquerda de um nó possui apenas elementos menores que o nó e a subárvore direita possui apenas nós que são maiores que o nó pode produzir uma árvore onde esse não é o caso.
Vamos repassar as leis de functor com mais detalhes em um dos próximos capítulos.
:::

## Kinds e um pouco de type-foo (Kinds and some type-foo) {#kinds-and-some-type-foo}

![TYPE FOO MASTER](assets/images/making-our-own-types-and-typeclasses/typefoo.png){.right width=287 height=400}

Construtores de tipos usam outros tipos como parâmetros para, eventualmente, produzir tipos concretos.
Isso me lembra as funções, que tomam valores como parâmetros para produzir valores.
Vimos que os construtores de tipos podem ser parcialmente aplicados (`Either String` é um tipo que leva um tipo e produz um tipo concreto, como `Either String Int`), assim como as funções podem ser.
Isso tudo é muito interessante, de fato.
Nesta seção, examinaremos a definição formal de como os tipos são aplicados aos construtores de tipos, assim como analisamos a definição formal de como os valores são aplicados às funções usando declarações de tipo.
**Você realmente não precisa ler esta seção para continuar em sua busca mágica de Haskell** e se você não entende, não se preocupe com isso.
No entanto, entender isso lhe dará uma compreensão muito completa do sistema de tipos.

Portanto, valores como `3`, `"YEAH"` ou `takeWhile` (funções também são valores, porque podemos passá-las e tal) cada um tem seu próprio tipo.
Os tipos são pequenos rótulos que os valores carregam para que possamos raciocinar sobre os valores.
Mas os tipos têm seus próprios pequenos rótulos, chamados **kinds**.
Um kind é mais ou menos o tipo de um tipo.
Isso pode parecer um pouco estranho e confuso, mas na verdade é um conceito muito legal.

O que são kinds e para que servem?
Bem, vamos examinar o tipo de um tipo usando o comando `:k` no GHCI.

```{.haskell:hs}
ghci> :k Int
Int :: *
```

Uma estrela?
Que pitoresco.
O que isso significa?
Um `*` significa que o tipo é um tipo concreto.
Um tipo concreto é um tipo que não aceita parâmetros de tipo e os valores só podem ter tipos que são tipos concretos.
Se eu tivesse que ler `*` em voz alta (não tive que fazer isso até agora), eu diria *star* ou apenas *type*.

Ok, agora vamos ver qual é o kind de `Maybe`.

```{.haskell:hs}
ghci> :k Maybe
Maybe :: * -> *
```

O construtor de tipos `Maybe` usa um tipo concreto (como `Int`) e depois retorna um tipo concreto como `Maybe Int`.
E é isso que esse kind nos diz.
Assim como `Int -> Int` significa que uma função pega um `Int` e retorna um `Int`, `* -> *` significa que o construtor de tipos pega um tipo concreto e retorna um tipo concreto.
Vamos aplicar o parâmetro de tipo para `Maybe` e ver qual é o kind desse tipo.

```{.haskell:hs}
ghci> :k Maybe Int
Maybe Int :: *
```

Assim como eu esperava!
Aplicamos o parâmetro de tipo em `Maybe` e recebemos um tipo concreto (é isso que `* -> *` significa).
Um paralelo (embora não equivalente, tipos e tipos são duas coisas diferentes) para isso é se fizermos `:t isUpper` e `:t isUpper 'A'`.
`isUpper` tem um tipo de `Char -> Bool` e `isUpper 'A'` tem um tipo de `Bool`, porque seu valor é basicamente `True`.
Ambos os tipos, no entanto, têm um kind de `*`.

Usamos `:k` em um tipo para obter seu kind, assim como podemos usar `:t` em um valor para obter seu tipo.
Como dissemos, os tipos são os rótulos de valores e kinds são os rótulos de tipos e existem paralelos entre os dois.

Vamos ver outro kind.

```{.haskell:hs}
ghci> :k Either
Either :: * -> * -> *
```

Aha, isso nos diz que `Either` requer dois tipos concretos como parâmetros de tipo para produzir um tipo concreto.
Também se parece com uma declaração de tipo de uma função que assume dois valores e retorna algo.
Os construtores de tipos são curried (assim como as funções), para que possamos aplicá-los parcialmente.

```{.haskell:hs}
ghci> :k Either String
Either String :: * -> *
ghci> :k Either String Int
Either String Int :: *
```

Quando queríamos fazer de `Either` uma parte da typeclass `Functor`, tivemos que aplicá-lo parcialmente, porque `Functor` quer tipos que aceitem apenas um parâmetro enquanto `Either` aceita dois.
Em outras palavras, `Functor` quer tipos de kind `* -> *` e, portanto, tivemos que aplicar parcialmente o `Either` para obter um tipo de kind `* -> *` em vez de seu kind original `* -> * -> *`.
Se olharmos para a definição de `Functor` novamente

```{.haskell:hs}
class Functor f where
    fmap :: (a -> b) -> f a -> f b
```

vemos que a variável de tipo `f` é usada como um tipo que usa um tipo concreto para produzir um tipo concreto.
Sabemos que ele deve produzir um tipo concreto porque é usado como o tipo de valor em uma função.
E a partir disso, podemos deduzir que os tipos que querem ser amigos de `Functor` devem ser do tipo `* -> *`.

Agora, vamos fazer um pouco de type-foo.
Dê uma olhada nesta typeclass que vou inventar agora:

```{.haskell:hs}
class Tofu t where
    tofu :: j a -> t a j
```

Cara, isso parece estranho.
Como faríamos um tipo que poderia ser uma instância dessa estranha typeclass?
Bem, vamos ver o que teria que ser seu kind.
Como `j a` é usado como o tipo de valor que a função `tofu` assume como parâmetro, `j a` deve ter um kind de `*`.
Assumimos `*` para `a` e, portanto, podemos inferir que `j` deve ter um kind de `* -> *`.
Vemos que `t` também deve produzir um valor concreto e que são necessários dois tipos.
E sabendo que `a` tem um kind de `*` e `j` tem um kind de `* -> *`, inferimos que `t` deve ter um kind de `* -> (* -> *) -> *`.
Portanto, é necessário um tipo concreto (`a`), um construtor de tipos que recebe um tipo concreto (`j`) e produz um tipo concreto.
Uau.

OK, então vamos criar um tipo com um kind de `* -> (* -> *) -> *`.
Aqui está uma maneira de fazer isso.

```{.haskell:hs}
data Frank a b  = Frank {frankField :: b a} deriving (Show)
```

Como sabemos que esse tipo tem um kind de `* -> (* -> *) -> *`?
Bem, os campos nos ADTs são feitos para manter valores, por isso devem ser do kind `*`, obviamente.
Assumimos `*` para `a`, o que significa que `b` usa um parâmetro de tipo e, portanto, seu kind é `* -> *`.
Agora conhecemos os kinds de `a` e `b` e, porque são parâmetros para `Frank`, vemos que `Frank` tem um kind de `* -> (* -> *) -> *`. O primeiro `*` representa `a` e o `(* -> *)` representa `b`.
Vamos criar alguns valores `Frank` e verificar seus tipos.

```{.haskell:hs}
ghci> :t Frank {frankField = Just "HAHA"}
Frank {frankField = Just "HAHA"} :: Frank [Char] Maybe
ghci> :t Frank {frankField = Node 'a' EmptyTree EmptyTree}
Frank {frankField = Node 'a' EmptyTree EmptyTree} :: Frank Char Tree
ghci> :t Frank {frankField = "YES"}
Frank {frankField = "YES"} :: Frank Char []
```

Hmm.
Como `frankField` tem um tipo de formulário `a b`, seus valores devem ter tipos de uma forma semelhante também.
Portanto, eles podem ser `Just "HAHA"`, que tem um tipo de `Maybe [Char]` ou pode ter um valor de `['Y','E','S']`, que tem um tipo de `[Char]` (se usássemos nosso próprio tipo de lista para isso, ele teria um tipo de `List Char`).
E vemos que os tipos dos valores `Frank` correspondem ao kind para `Frank`.
`[Char]` tem um kind de `*` e `Maybe` tem um kind de `* -> *`.
Porque, para ter um valor, ele deve ser um tipo concreto e, portanto, deve ser totalmente aplicado, todo valor de `Frank blah blaah` tem um kind de `*`.

Tornar `Frank` uma instância de `Tofu` é bastante simples.
Vemos que `tofu` recebe um `j a` (portanto, um tipo de exemplo desse formulário seria `Maybe Int`) e retorna um `t a j`.
Portanto, se substituirmos `Frank` por `j`, o tipo de resultado seria `Frank Int Maybe`.

```{.haskell:hs}
instance Tofu Frank where
    tofu x = Frank x
```

```{.haskell:hs}
ghci> tofu (Just 'a') :: Frank Char Maybe
Frank {frankField = Just 'a'}
ghci> tofu ["HELLO"] :: Frank [Char] []
Frank {frankField = ["HELLO"]}
```

Não é muito útil, mas flexionamos nossos músculos de tipo.
Vamos fazer mais type-foo.
Temos este tipo de dados:

```{.haskell:hs}
data Barry t k p = Barry { yabba :: p, dabba :: t k }
```

E agora queremos torná-lo uma instância de `Functor`.
`Functor` quer tipos de kind `* -> *` mas `Barry` não parece ter esse kind.
Qual é o kind de `Barry`?
Bem, vemos que são necessários três parâmetros de tipo, então será `something -> something -> something -> *`.
É seguro dizer que `p` é um tipo concreto e, portanto, tem um kind de `*`.
Para `k`, assumimos `*` e, por extensão, `t` tem um kind de `* -> *`.
Agora vamos substituir esses kinds pelos *somethings* que usamos como espaços reservados e vemos que ele tem um kind de `(* -> *) -> * -> * -> *`.
Vamos verificar isso com o GHCI.

```{.haskell:hs}
ghci> :k Barry
Barry :: (* -> *) -> * -> * -> *
```

Ah, nós estávamos certos.
Que satisfatório.
Agora, para tornar esse tipo uma parte de `Functor`, temos que aplicar parcialmente os dois primeiros parâmetros de tipo para ficarmos com `* -> *`.
Isso significa que o início da declaração da instância será: `instance Functor (Barry a b) where`.
Se olharmos para `fmap` como se fosse feito especificamente para `Barry`, ele teria um tipo de `fmap :: (a -> b) -> Barry c d a -> Barry c d b`, porque apenas substituímos o `f` do `Functor` por `Barry c d`.
O terceiro parâmetro de tipo de `Barry` terá que mudar e vemos que está convenientemente em seu próprio campo.

```{.haskell:hs}
instance Functor (Barry a b) where
    fmap f (Barry {yabba = x, dabba = y}) = Barry {yabba = f x, dabba = y}
```

Aí vamos nós!
Acabamos de mapear o `f` sobre o primeiro campo.

Nesta seção, examinamos bem como os parâmetros de tipo funcionam e os formalizamos com kinds, assim como formalizamos os parâmetros de função com declarações de tipo.
Vimos que existem paralelos interessantes entre funções e construtores de tipos.
Eles são, no entanto, duas coisas completamente diferentes.
Ao trabalhar em Haskell real, geralmente você não precisará mexer com kinds e fazer inferência de kind manualmente como fizemos agora.
Geralmente, você apenas precisa aplicar parcialmente seu próprio tipo a `* -> *` ou `*` ao torná-lo uma instância de uma das typeclasses padrão, mas é bom saber como e por que isso realmente funciona.
Também é interessante ver que os tipos têm pequenos tipos próprios.
Mais uma vez, você realmente não precisa entender tudo o que fizemos aqui para ler, mas se você entender como os kinds funcionam, provavelmente terá uma compreensão muito sólida do sistema de tipos de Haskell.



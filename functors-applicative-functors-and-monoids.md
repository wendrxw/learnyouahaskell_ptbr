# Functors, Applicative Functors e Monoids

A combinação de pureza, funções de alta ordem, tipos de dados algébricos parametrizados e typeclasses do Haskell nos permite implementar polimorfismo em um nível muito mais alto do que é possível em outras linguagens.
Não precisamos pensar em tipos pertencentes a uma grande hierarquia de tipos.
Em vez disso, pensamos sobre como os tipos podem agir e então os conectamos com as typeclasses apropriadas.
Um `Int` pode agir como muitas coisas.
Ele pode agir como algo que pode ser igualado, como algo ordenado, como algo enumerável, etc.

Typeclasses são abertas, o que significa que podemos definir nosso próprio tipo de dados, pensar sobre como ele pode agir e conectá-lo com as typeclasses que definem seus comportamentos.
Por causa disso e por causa do ótimo sistema de tipos do Haskell que nos permite saber muito sobre uma função apenas conhecendo sua declaração de tipo, podemos definir typeclasses que definem um comportamento que é muito geral e abstrato.
Conhecemos typeclasses que definem operações para ver se duas coisas são iguais ou comparar duas coisas por alguma ordem.
Esses são comportamentos muito abstratos e elegantes, mas não pensamos neles como algo muito especial porque lidamos com eles na maior parte de nossas vidas.
Recentemente conhecemos Functors, que são basicamente coisas que podem ser mapeadas.
Esse é um exemplo de uma propriedade útil e ainda assim bastante abstrata que as typeclasses podem descrever.
Neste capítulo, examinaremos mais de perto os Functors, juntamente com versões ligeiramente mais fortes e úteis de Functors chamadas Applicative Functors.
Também daremos uma olhada em Monoids, que são meio que como meias.

## Functors redux {#functors-redux}

![frogs dont even need money](assets/images/functors-applicative-functors-and-monoids/frogtor.png){.right width=369 height=243}

Ainda assim, aqui vai uma rápida recapitulação: Functors são coisas que podem ser mapeadas, como listas, `Maybe`s, árvores e afins.
Em Haskell, eles são descritos pela typeclass `Functor`, que tem apenas um método de typeclass, a saber `fmap`, que tem um tipo de `fmap :: (a -> b) -> f a -> f b`.
Ele diz: me dê uma função que pega um `a` e retorna um `b` e uma caixa com um `a` (ou vários deles) dentro dela e eu te darei uma caixa com um `b` (ou vários deles) dentro dela.
Ele meio que aplica a função ao elemento dentro da caixa.

::: {.hintbox}
**Um conselho.**
Muitas vezes a analogia da caixa é usada para ajudar você a ter alguma intuição de como os Functors funcionam, e mais tarde, provavelmente usaremos a mesma analogia para Applicative Functors e Monads.
É uma analogia ok que ajuda as pessoas a entender Functors no início, apenas não leve muito ao pé da letra, porque para alguns Functors a analogia da caixa tem que ser esticada muito para ainda manter alguma verdade.
Um termo mais correto para o que é um Functor seria *contexto computacional*.
O contexto pode ser que a computação pode ter um valor ou pode ter falhado (`Maybe` e `Either a`) ou que pode haver mais valores (listas), coisas assim.
:::

Se quisermos fazer de um construtor de tipo uma instância de `Functor`, ele tem que ter um kind de `* -> *`, o que significa que ele tem que tomar exatamente um tipo concreto como parâmetro de tipo.
Por exemplo, `Maybe` pode ser feito uma instância porque ele pega um parâmetro de tipo para produzir um tipo concreto, como `Maybe Int` ou `Maybe String`.
Se um construtor de tipo pega dois parâmetros, como `Either`, temos que aplicar parcialmente o construtor de tipo até que ele pegue apenas um parâmetro de tipo.
Então não podemos escrever `instance Functor Either where`, mas podemos escrever `instance Functor (Either a) where` e então se imaginarmos que `fmap` é apenas para `Either a`, ele teria uma declaração de tipo de `fmap :: (b -> c) -> Either a b -> Either a c`.
Como você pode ver, a parte `Either a` é fixa, porque `Either a` pega apenas um parâmetro de tipo, enquanto apenas `Either` pega dois, então `fmap :: (b -> c) -> Either b -> Either c` não faria muito sentido.

Aprendemos até agora como muitos tipos (bem, construtores de tipos na verdade) são instâncias de `Functor`, como `[]`, `Maybe`, `Either a` e um tipo `Tree` que fizemos por conta própria.
Vimos como podemos mapear funções sobre eles para um grande bem.
Nesta seção, daremos uma olhada em mais duas instâncias de functor, a saber `IO` e `(->) r`.

Se algum valor tem um tipo de, digamos, `IO String`, isso significa que é uma ação de E/S que, quando executada, sairá para o mundo real e pegará alguma string para nós, que ela produzirá como resultado.
Podemos usar `<-` na sintaxe *do* para vincular esse resultado a um nome.
Mencionamos que ações de E/S são como caixas com pezinhos que saem e buscam algum valor do mundo exterior para nós.
Podemos inspecionar o que elas buscaram, mas depois de inspecionar, temos que embrulhar o valor de volta em `IO`.
Ao pensar nessa analogia da caixa com pezinhos, podemos ver como `IO` age como um functor.

Vamos ver como `IO` é uma instância de `Functor`.
Quando damos `fmap` de uma função sobre uma ação de E/S, queremos obter de volta uma ação de E/S que faz a mesma coisa, mas tem nossa função aplicada sobre seu valor resultante.

```{.haskell:hs}
instance Functor IO where
    fmap f action = do
        result <- action
        return (f result)
```

O resultado de mapear algo sobre uma ação de E/S será uma ação de E/S, então logo de cara usamos a sintaxe *do* para colar duas ações e fazer uma nova.
Na implementação para `fmap`, fazemos uma nova ação de E/S que primeiro executa a ação de E/S original e chama seu resultado de `result`.
Então, fazemos `return (f result)`.
`return` é, como você sabe, uma função que faz uma ação de E/S que não faz nada além de apresentar algo como seu resultado.
A ação que um bloco *do* produz sempre terá o valor resultante de sua última ação.
É por isso que usamos return para fazer uma ação de E/S que realmente não faz nada, ela apenas apresenta `f result` como o resultado da nova ação de E/S.

Podemos brincar com isso para ganhar alguma intuição.
É muito simples, na verdade.
Confira este pedaço de código:

```{.haskell:hs}
main = do line <- getLine
          let line' = reverse line
          putStrLn $ "You said " ++ line' ++ " backwards!"
          putStrLn $ "Yes, you really said" ++ line' ++ " backwards!"
```

O usuário é solicitado por uma linha e nós a devolvemos ao usuário, só que invertida.
Aqui está como reescrever isso usando `fmap`:

```{.haskell:hs}
main = do line <- fmap reverse getLine
          putStrLn $ "You said " ++ line ++ " backwards!"
          putStrLn $ "Yes, you really said" ++ line ++ " backwards!"
```

![w00ooOoooOO](assets/images/functors-applicative-functors-and-monoids/alien.png){.left width=262 height=212}

Assim como quando damos `fmap` de `reverse` sobre `Just "blah"` para obter `Just "halb"`, podemos dar `fmap` de `reverse` sobre `getLine`.
`getLine` é uma ação de E/S que tem um tipo de `IO String` e mapear `reverse` sobre ela nos dá uma ação de E/S que sairá para o mundo real e pegará uma linha e então aplicará `reverse` ao seu resultado.
Como podemos aplicar uma função a algo que está dentro de uma caixa `Maybe`, podemos aplicar uma função ao que está dentro de uma caixa `IO`, só que ela tem que sair para o mundo real para pegar algo.
Então, quando a vinculamos a um nome usando `<-`, o nome refletirá o resultado que já tem `reverse` aplicado a ele.

A ação de E/S `fmap (++"!") getLine` se comporta exatamente como `getLine`, só que seu resultado sempre tem `"!"` anexado a ele!

Se olharmos para qual seria o tipo de `fmap` se fosse limitado a `IO`, seria `fmap :: (a -> b) -> IO a -> IO b`.
`fmap` pega uma função e uma ação de E/S e retorna uma nova ação de E/S que é como a antiga, exceto que a função é aplicada ao seu resultado contido.

Se você se encontrar vinculando o resultado de uma ação de E/S a um nome, apenas para aplicar uma função a isso e chamar isso de outra coisa, considere usar `fmap`, porque parece mais bonito.
Se você quiser aplicar múltiplas transformações a alguns dados dentro de um functor, você pode declarar sua própria função no nível superior, fazer uma função lambda ou, idealmente, usar composição de funções:

```{.haskell:hs}
import Data.Char
import Data.List

main = do line <- fmap (intersperse '-' . reverse . map toUpper) getLine
          putStrLn line
```

```{.plain}
$ runhaskell fmapping_io.hs
hello there
E-R-E-H-T- -O-L-L-E-H
```

Como você provavelmente sabe, `intersperse '-' . reverse . map toUpper` é uma função que pega uma string, mapeia `toUpper` sobre ela, aplica `reverse` a esse resultado e então aplica `intersperse '-'` a esse resultado.
É como escrever `(\xs -> intersperse '-' (reverse (map toUpper xs)))`, só que mais bonito.

Outra instância de `Functor` com a qual estamos lidando o tempo todo, mas não sabíamos que era um `Functor` é `(->) r`.
Você provavelmente está um pouco confuso agora, já que o que diabos `(->) r` significa?
O tipo de função `r -> a` pode ser reescrito como `(->) r a`, muito parecido com como podemos escrever `2 + 3` como `(+) 2 3`.
Quando olhamos para isso como `(->) r a`, podemos ver `(->)` sob uma luz ligeiramente diferente, porque vemos que é apenas um construtor de tipo que pega dois parâmetros de tipo, assim como `Either`.
Mas lembre-se, dissemos que um construtor de tipo tem que pegar exatamente um parâmetro de tipo para que possa ser feito uma instância de `Functor`.
É por isso que não podemos fazer `(->)` uma instância de `Functor`, mas se aplicarmos parcialmente a `(->) r`, não há problemas.
Se a sintaxe permitisse que construtores de tipo fossem parcialmente aplicados com seções (como podemos aplicar parcialmente `+` fazendo `(2+)`, que é o mesmo que `(+) 2`), você poderia escrever `(->) r` como `(r ->)`.
Como as funções são Functors?
Bem, vamos dar uma olhada na implementação, que fica em `Control.Monad.Instances`

::: {.hintbox}
Geralmente marcamos funções que pegam qualquer coisa e retornam qualquer coisa como `a -> b`.
`r -> a` é a mesma coisa, apenas usamos letras diferentes para as variáveis de tipo.
:::

```{.haskell:hs}
instance Functor ((->) r) where
    fmap f g = (\x -> f (g x))
```

Se a sintaxe permitisse, poderia ter sido escrito como

```{.haskell:hs}
instance Functor (r ->) where
    fmap f g = (\x -> f (g x))
```

Mas não permite, então temos que escrever da maneira anterior.

Primeiro de tudo, vamos pensar sobre o tipo de `fmap`.
É `fmap :: (a -> b) -> f a -> f b`.
Agora o que faremos é substituir mentalmente todos os `f`s, que são o papel que nossa instância de Functor desempenha, por `(->) r`s.
Faremos isso para ver como `fmap` deve se comportar para esta instância em particular.
Obtemos `fmap :: (a -> b) -> ((->) r a) -> ((->) r b)`.
Agora o que podemos fazer é escrever os tipos `(->) r a` e `(-> r b)` como infixos `r -> a` e `r -> b`, como normalmente fazemos com funções.
O que obtemos agora é `fmap :: (a -> b) -> (r -> a) -> (r -> b)`.

Hmmm OK.
Mapear uma função sobre uma função tem que produzir uma função, assim como mapear uma função sobre um `Maybe` tem que produzir um `Maybe` e mapear uma função sobre uma lista tem que produzir uma lista.
O que o tipo `fmap :: (a -> b) -> (r -> a) -> (r -> b)` para esta instância nos diz?
Bem, vemos que pega uma função de `a` para `b` e uma função de `r` para `a` e retorna uma função de `r` para `b`.
Isso te lembra alguma coisa?
Sim!
Composição de funções!
Canalizamos a saída de `r -> a` para a entrada de `a -> b` para obter uma função `r -> b`, que é exatamente do que se trata a composição de funções.
Se você olhar como a instância é definida acima, verá que é apenas composição de funções.
Outra maneira de escrever esta instância seria:

```{.haskell:hs}
instance Functor ((->) r) where
    fmap = (.)
```

Isso torna a revelação de que usar `fmap` sobre funções é apenas composição meio óbvia.
Faça `:m + Control.Monad.Instances`, já que é onde a instância é definida e então tente brincar mapeando sobre funções.

```{.haskell:hs}
ghci> :t fmap (*3) (+100)
fmap (*3) (+100) :: (Num a) => a -> a
ghci> fmap (*3) (+100) 1
303
ghci> (*3) `fmap` (+100) $ 1
303
ghci> (*3) . (+100) $ 1
303
ghci> fmap (show . (*3)) (*100) 1
"300"
```

Podemos chamar `fmap` como uma função infixa para que a semelhança com `.` seja clara.
Na segunda linha de entrada, estamos mapeando `(*3)` sobre `(+100)`, o que resulta em uma função que pegará uma entrada, chamará `(+100)` nela e então chamará `(*3)` nesse resultado.
Chamamos essa função com `1`.

Como a analogia da caixa se sustenta aqui?
Bem, se você esticar, ela se sustenta.
Quando usamos `fmap (+3)` sobre `Just 3`, é fácil imaginar o `Maybe` como uma caixa que tem algum conteúdo no qual aplicamos a função `(+3)`.
Mas e quando estamos fazendo `fmap (*3) (+100)`?
Bem, você pode pensar na função `(+100)` como uma caixa que contém seu eventual resultado.
Meio como uma ação de E/S que pode ser pensada como uma caixa que sairá para o mundo real e buscará algum resultado.
Usar `fmap (*3)` em `(+100)` criará outra função que age como `(+100)`, só que antes de produzir um resultado, `(*3)` será aplicado a esse resultado.
Agora podemos ver como `fmap` age exatamente como `.` para funções.

O fato de que `fmap` é composição de funções quando usado em funções não é tão terrivelmente útil agora, mas pelo menos é muito interessante.
Também dobra um pouco nossas mentes e nos permite ver como coisas que agem mais como computações do que caixas (`IO` e `(->) r`) podem ser Functors.
A função sendo mapeada sobre uma computação resulta na mesma computação, mas o resultado dessa computação é modificado com a função.

![lifting a function is easier than lifting a million pounds](assets/images/functors-applicative-functors-and-monoids/lifter.png){.right width=443 height=450}

Antes de prosseguirmos para as regras que `fmap` deve seguir, vamos pensar sobre o tipo de `fmap` mais uma vez.
Seu tipo é `fmap :: (a -> b) -> f a -> f b`.
Estamos perdendo a restrição de classe `(Functor f) =>`, mas a deixamos de fora aqui por brevidade, porque estamos falando sobre Functors de qualquer maneira, então sabemos o que o `f` significa.
Quando aprendemos pela primeira vez sobre [funções curried](higher-order-functions.html#curried-functions), dissemos que todas as funções Haskell na verdade pegam um parâmetro.
Uma função `a -> b -> c` na verdade pega apenas um parâmetro de tipo `a` e então retorna uma função `b -> c`, que pega um parâmetro e retorna um `c`.
É assim que se chamarmos uma função com poucos parâmetros (ou seja, parcialmente aplicá-la), obtemos de volta uma função que pega o número de parâmetros que deixamos de fora (se estivermos pensando em funções como pegando vários parâmetros novamente).
Então `a -> b -> c` pode ser escrito como `a -> (b -> c)`, para tornar o currying mais aparente.

Na mesma veia, se escrevermos `fmap :: (a -> b) -> (f a -> f b)`, podemos pensar em `fmap` não como uma função que pega uma função e um Functor e retorna um Functor, mas como uma função que pega uma função e retorna uma nova função que é exatamente como a antiga, só que pega um Functor como parâmetro e retorna um Functor como resultado.
Ela pega uma função `a -> b` e retorna uma função `f a -> f b`.
Isso é chamado de *levantar* (lifting) uma função.
Vamos brincar com essa ideia usando o comando `:t` do GHCI:

```{.haskell:hs}
ghci> :t fmap (*2)
fmap (*2) :: (Num a, Functor f) => f a -> f a
ghci> :t fmap (replicate 3)
fmap (replicate 3) :: (Functor f) => f a -> f [a]
```

A expressão `fmap (*2)` é uma função que pega um functor `f` sobre números e retorna um functor sobre números.
Esse functor pode ser uma lista, um `Maybe`, um `Either String`, o que for.
A expressão `fmap (replicate 3)` pegará um functor sobre qualquer tipo e retornará um functor sobre uma lista de elementos desse tipo.

::: {.hintbox}
Quando dizemos *um functor sobre números*, você pode pensar nisso como *um functor que tem números nele*.
O primeiro é um pouco mais chique e tecnicamente mais correto, mas o último é geralmente mais fácil de entender.
:::

Isso é ainda mais aparente se aplicarmos parcialmente, digamos, `fmap (++"!")` e então vincularmos a um nome no GHCI.

Você pode pensar em `fmap` como uma função que pega uma função e um Functor e então mapeia essa função sobre o Functor, ou você pode pensar nisso como uma função que pega uma função e levanta essa função para que ela opere em Functors.
Ambas as visões estão corretas e em Haskell, equivalentes.

O tipo `fmap (replicate 3) :: (Functor f) => f a -> f [a]` significa que a função funcionará em qualquer Functor.
O que exatamente ela fará depende de qual Functor a usamos.
Se usarmos `fmap (replicate 3)` em uma lista, a implementação da lista para `fmap` será escolhida, que é apenas `map`.
Se usarmos em um `Maybe a`, ela aplicará `replicate 3` ao valor dentro do `Just`, ou se for `Nothing`, então permanece `Nothing`.

```{.haskell:hs}
ghci> fmap (replicate 3) [1,2,3,4]
[[1,1,1],[2,2,2],[3,3,3],[4,4,4]]
ghci> fmap (replicate 3) (Just 4)
Just [4,4,4]
ghci> fmap (replicate 3) (Right "blah")
Right ["blah","blah","blah"]
ghci> fmap (replicate 3) Nothing
Nothing
ghci> fmap (replicate 3) (Left "foo")
Left "foo"
```

A seguir, vamos olhar para as **leis dos Functors**.
Para que algo seja um Functor, ele deve satisfazer algumas leis.
Todos os Functors devem exibir certos tipos de propriedades e comportamentos semelhantes a Functors.
Eles devem se comportar de forma confiável como coisas que podem ser mapeadas.
Chamar `fmap` em um Functor deve apenas mapear uma função sobre o Functor, nada mais.
Esse comportamento é descrito nas leis dos Functors.
Existem duas que todas as instâncias de `Functor` devem seguir.
Elas não são aplicadas pelo Haskell automaticamente, então você tem que testá-las você mesmo.

**A primeira lei dos Functors afirma que se mapearmos a função `id` sobre um Functor, o Functor que obtemos de volta deve ser o mesmo que o Functor original.**
Se escrevermos isso um pouco mais formalmente, significa que `fmap id = id`{.label .law}.
Então, essencialmente, isso diz que se fizermos `fmap id` sobre um Functor, deve ser o mesmo que apenas chamar `id` no Functor.
Lembre-se, `id` é a função identidade, que apenas retorna seu parâmetro não modificado.
Também pode ser escrita como `\x -> x`.
Se virmos o functor como algo que pode ser mapeado, a lei `fmap id = id`{.label .law} parece meio trivial ou óbvia.

Vamos ver se essa lei se sustenta para alguns valores de functores.

```{.haskell:hs}
ghci> fmap id (Just 3)
Just 3
ghci> id (Just 3)
Just 3
ghci> fmap id [1..5]
[1,2,3,4,5]
ghci> id [1..5]
[1,2,3,4,5]
ghci> fmap id []
[]
ghci> fmap id Nothing
Nothing
```

Se olharmos para a implementação de `fmap` para, digamos, `Maybe`, podemos descobrir por que a primeira lei dos functores se sustenta.

```{.haskell:hs}
instance Functor Maybe where
    fmap f (Just x) = Just (f x)
    fmap f Nothing = Nothing
```

Imaginamos que `id` desempenha o papel do parâmetro `f` na implementação.
Vemos que se dermos `fmap id` sobre `Just x`, o resultado será `Just (id x)`, e como `id` apenas retorna seu parâmetro, podemos deduzir que `Just (id x)` é igual a `Just x`.
Então agora sabemos que se mapearmos `id` sobre um valor `Maybe` com um construtor de valor `Just`, obtemos o mesmo valor de volta.

Ver que mapear `id` sobre um valor `Nothing` retorna o mesmo valor é trivial.
Então, a partir dessas duas equações na implementação para `fmap`, vemos que a lei `fmap id = id` se sustenta.

![justice is blind, but so is my dog](assets/images/functors-applicative-functors-and-monoids/justice.png){.left width=345 height=428}

**A segunda lei diz que compor duas funções e então mapear a função resultante sobre um Functor deve ser o mesmo que primeiro mapear uma função sobre o Functor e então mapear a outra.**
Formalmente escrito, isso significa que `fmap (f . g) = fmap f . fmap g`{.label .law}.
Ou para escrever de outra maneira, para qualquer Functor *F*, o seguinte deve ser válido: `fmap (f . g) F = fmap f (fmap g F)`{.label .law}.

Se pudermos mostrar que algum tipo obedece a ambas as leis dos Functors, podemos confiar que ele terá os mesmos comportamentos fundamentais que outros Functors quando se trata de mapeamento.
Podemos saber que quando usamos `fmap` nele, não haverá nada além de mapeamento acontecendo nos bastidores e que ele agirá como uma coisa que pode ser mapeada, ou seja, um Functor.
Você descobre como a segunda lei se sustenta para algum tipo olhando para a implementação de `fmap` para esse tipo e então usando o método que usamos para verificar se `Maybe` obedece à primeira lei.
Se você quiser, podemos verificar como a segunda lei dos Functors se sustenta para `Maybe`.
Se fizermos `fmap (f . g)` sobre `Nothing`, obtemos `Nothing`, porque fazer um `fmap` com qualquer função sobre `Nothing` retorna `Nothing`.
Se fizermos `fmap f (fmap g Nothing)`, obtemos `Nothing`, pela mesma razão.
OK, ver como a segunda lei se sustenta para `Maybe` se for um valor `Nothing` é muito fácil, quase trivial.

E se for um valor <code>Just *algo*</code>?
Bem, se fizermos `fmap (f . g) (Just x)`, vemos pela implementação que é implementado como `Just ((f . g) x)`, que é, claro, `Just (f (g x))`.
Se fizermos `fmap f (fmap g (Just x))`, vemos pela implementação que `fmap g (Just x)` é `Just (g x)`.
Logo, `fmap f (fmap g (Just x))` é igual a `fmap f (Just (g x))` e pela implementação vemos que isso é igual a `Just (f (g x))`.

Se você está um pouco confuso com essa prova, não se preocupe.
Certifique-se de entender como [composição de funções](higher-order-functions.html#composition) funciona.
Muitas vezes, você pode ver intuitivamente como essas leis se sustentam porque os tipos agem como contêineres ou funções.
Você também pode simplesmente testá-las em um monte de valores diferentes de um tipo e ser capaz de dizer com alguma certeza que um tipo de fato obedece às leis.

Vamos dar uma olhada em um exemplo patológico de um construtor de tipo sendo uma instância da typeclass `Functor`, mas não sendo realmente um functor, porque ele não satisfaz as leis.
Vamos dizer que temos um tipo:

```{.haskell:hs}
data CMaybe a = CNothing | CJust Int a deriving (Show)
```

O C aqui significa *contador* (counter).
É um tipo de dados que se parece muito com `Maybe a`, só que a parte `Just` contém dois campos em vez de um.
O primeiro campo no construtor de valor `CJust` sempre terá um tipo de `Int`, e será algum tipo de contador e o segundo campo é do tipo `a`, que vem do parâmetro de tipo e seu tipo, claro, dependerá do tipo concreto que escolhermos para `CMaybe a`.
Vamos brincar com nosso novo tipo para ter alguma intuição sobre ele.

```{.haskell:hs}
ghci> CNothing
CNothing
ghci> CJust 0 "haha"
CJust 0 "haha"
ghci> :t CNothing
CNothing :: CMaybe a
ghci> :t CJust 0 "haha"
CJust 0 "haha" :: CMaybe [Char]
ghci> CJust 100 [1,2,3]
CJust 100 [1,2,3]
```

Se usarmos o construtor `CNothing`, não há campos, e se usarmos o construtor `CJust`, o primeiro campo é um inteiro e o segundo campo pode ser qualquer tipo.
Vamos fazer disso uma instância de `Functor` para que toda vez que usarmos `fmap`, a função seja aplicada ao segundo campo, enquanto o primeiro campo é incrementado em 1.

```{.haskell:hs}
instance Functor CMaybe where
    fmap f CNothing = CNothing
    fmap f (CJust counter x) = CJust (counter+1) (f x)
```

Isso é meio que como a implementação da instância para `Maybe`, exceto que quando damos `fmap` sobre um valor que não representa uma caixa vazia (um valor `CJust`), não aplicamos apenas a função ao conteúdo, também aumentamos o contador em 1.
Tudo parece legal até agora, podemos até brincar com isso um pouco:

```{.haskell:hs}
ghci> fmap (++"ha") (CJust 0 "ho")
CJust 1 "hoha"
ghci> fmap (++"he") (fmap (++"ha") (CJust 0 "ho"))
CJust 2 "hohahe"
ghci> fmap (++"blah") CNothing
CNothing
```

Isso obedece às leis dos functores?
Para ver que algo não obedece a uma lei, é suficiente encontrar apenas um contra-exemplo.

```{.haskell:hs}
ghci> fmap id (CJust 0 "haha")
CJust 1 "haha"
ghci> id (CJust 0 "haha")
CJust 0 "haha"
```

Ah!
Sabemos que a primeira lei dos functores afirma que se mapearmos `id` sobre um functor, deve ser o mesmo que apenas chamar `id` com o mesmo functor, mas como vimos neste exemplo, isso não é verdade para nosso functor `CMaybe`.
Mesmo que seja parte da typeclass `Functor`, não obedece às leis dos functores e, portanto, não é um functor.
Se alguém usasse nosso tipo `CMaybe` como um functor, esperaria que ele obedecesse às leis dos functores como um bom functor.
Mas `CMaybe` falha em ser um functor, embora finja ser um, então usá-lo como um functor pode levar a algum código defeituoso.
Quando usamos um functor, não deveria importar se primeiro compomos algumas funções e depois as mapeamos sobre o functor ou se apenas mapeamos cada função sobre um functor sucessivamente.
Mas com `CMaybe`, importa, porque ele acompanha quantas vezes foi mapeado.
Não é legal!
Se quiséssemos que `CMaybe` obedecesse às leis dos functores, teríamos que fazer de modo que o campo `Int` permanecesse o mesmo quando usássemos `fmap`.

A princípio, as leis dos functores podem parecer um pouco confusas e desnecessárias, mas então vemos que se soubermos que um tipo obedece a ambas as leis, podemos fazer certas suposições sobre como ele agirá.
Se um tipo obedece às leis dos functores, sabemos que chamar `fmap` em um valor desse tipo apenas mapeará a função sobre ele, nada mais.
Isso leva a um código que é mais abstrato e extensível, porque podemos usar leis para raciocinar sobre comportamentos que qualquer functor deve ter e fazer funções que operam de forma confiável em qualquer functor.

Todas as instâncias de `Functor` na biblioteca padrão obedecem a essas leis, mas você pode verificar por si mesmo se não acredita em mim.
E da próxima vez que você fizer de um tipo uma instância de `Functor`, reserve um minuto para garantir que ele obedeça às leis dos functores.
Depois de lidar com functores suficientes, você meio que vê intuitivamente as propriedades e comportamentos que eles têm em comum e não é difícil ver intuitivamente se um tipo obedece às leis dos functores.
Mas mesmo sem a intuição, você sempre pode apenas passar pela implementação linha por linha e ver se as leis se sustentam ou tentar encontrar um contra-exemplo.

Também podemos olhar para functores como coisas que produzem valores em um contexto.
Por exemplo, `Just 3` produz o valor `3` no contexto de que pode ou não produzir quaisquer valores.
`[1,2,3]` produz três valores---`1`, `2` e `3`, o contexto é que pode haver múltiplos valores ou nenhum valor.
A função `(+3)` produzirá um valor, dependendo de qual parâmetro for dada.

## Applicative Functors {#applicative-functors}

![disregard this analogy](assets/images/functors-applicative-functors-and-monoids/present.png){.right width=302 height=284}

Nesta seção, daremos uma olhada em Applicative Functors, que são Functors reforçados, representados em Haskell pela typeclass `Applicative`, encontrada no módulo `Control.Applicative`.

Como você sabe, as funções em Haskell são curried por padrão, o que significa que uma função que parece pegar vários parâmetros na verdade pega apenas um parâmetro e retorna uma função que pega o próximo parâmetro e assim por diante.
Se uma função é do tipo `a -> b -> c`, costumamos dizer que ela pega dois parâmetros e retorna um `c`, mas na verdade ela pega um `a` e retorna uma função `b -> c`.
É por isso que podemos chamar uma função como `f x y` ou como `(f x) y`.
Esse mecanismo é o que nos permite aplicar parcialmente funções apenas chamando-as com poucos parâmetros, o que resulta em funções que podemos passar para outras funções.

Até agora, quando estávamos mapeando funções sobre functores, geralmente mapeávamos funções que pegam apenas um parâmetro.
Mas o que acontece quando mapeamos uma função como `*`, que pega dois parâmetros, sobre um functor?
Vamos dar uma olhada em alguns exemplos concretos disso.
Se tivermos `Just 3` e fizermos `fmap (*) (Just 3)`, o que obtemos?
Pela implementação da instância de `Maybe` para `Functor`, sabemos que se for um valor <code>Just *algo*</code>, ela aplicará a função ao <code>*algo*</code> dentro do `Just`.
Portanto, fazer `fmap (*) (Just 3)` resulta em `Just ((*) 3)`, que também pode ser escrito como `Just (* 3)` se usarmos seções.
Interessante!
Obtemos uma função embrulhada em um `Just`!

```{.haskell:hs}
ghci> :t fmap (++) (Just "hey")
fmap (++) (Just "hey") :: Maybe ([Char] -> [Char])
ghci> :t fmap compare (Just 'a')
fmap compare (Just 'a') :: Maybe (Char -> Ordering)
ghci> :t fmap compare "A LIST OF CHARS"
fmap compare "A LIST OF CHARS" :: [Char -> Ordering]
ghci> :t fmap (\x y z -> x + y / z) [3,4,5,6]
fmap (\x y z -> x + y / z) [3,4,5,6] :: (Fractional a) => [a -> a -> a]
```

Se mapearmos `compare`, que tem um tipo de `(Ord a) => a -> a -> Ordering` sobre uma lista de caracteres, obtemos uma lista de funções do tipo `Char -> Ordering`, porque a função `compare` é parcialmente aplicada com os caracteres na lista.
Não é uma lista de função `(Ord a) => a -> Ordering`, porque o primeiro `a` que foi aplicado era um `Char` e então o segundo `a` tem que decidir ser do tipo `Char`.

Vemos como mapeando funções de "vários parâmetros" sobre functores, obtemos functores que contêm funções dentro deles.
Então, o que podemos fazer com eles?
Bem, para começar, podemos mapear funções que pegam essas funções como parâmetros sobre eles, porque o que estiver dentro de um functor será dado à função que estamos mapeando sobre ele como um parâmetro.

```{.haskell:hs}
ghci> let a = fmap (*) [1,2,3,4]
ghci> :t a
a :: [Integer -> Integer]
ghci> fmap (\f -> f 9) a
[9,18,27,36]
```

Mas o que acontece se tivermos um valor de functor `Just (3 *)` e um valor de functor `Just 5` e quisermos tirar a função de `Just (3 *)` e mapeá-la sobre `Just 5`?
Com functores normais, estamos sem sorte, porque tudo o que eles suportam é apenas mapear funções normais sobre functores existentes.
Mesmo quando mapeamos `\f -> f 9` sobre um functor que continha funções dentro dele, estávamos apenas mapeando uma função normal sobre ele.
Mas não podemos mapear uma função que está dentro de um functor sobre outro functor com o que `fmap` nos oferece.
Poderíamos fazer pattern matching no construtor `Just` para tirar a função dele e então mapeá-la sobre `Just 5`, mas estamos procurando uma maneira mais geral e abstrata de fazer isso, que funcione em functores.

Conheça a typeclass `Applicative`.
Ela fica no módulo `Control.Applicative` e define dois métodos, `pure` e `<*>`.
Ela não fornece uma implementação padrão para nenhum deles, então temos que definir ambos se quisermos que algo seja um functor aplicativo.
A classe é definida assim:

```{.haskell:hs}
class (Functor f) => Applicative f where
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b
```

Essa definição de classe simples de três linhas nos diz muito!
Vamos começar na primeira linha.
Ela inicia a definição da classe `Applicative` e também introduz uma restrição de classe.
Ela diz que se quisermos fazer de um construtor de tipo parte da typeclass `Applicative`, ele tem que estar em `Functor` primeiro.
É por isso que se sabemos que se um construtor de tipo é parte da typeclass `Applicative`, ele também está em `Functor`, então podemos usar `fmap` nele.

O primeiro método que ela define é chamado `pure`.
Sua declaração de tipo é `pure :: a -> f a`.
`f` desempenha o papel de nossa instância de functor aplicativo aqui.
Como Haskell tem um sistema de tipos muito bom e porque tudo o que uma função pode fazer é pegar alguns parâmetros e retornar algum valor, podemos dizer muito a partir de uma declaração de tipo e isso não é exceção.
`pure` deve pegar um valor de qualquer tipo e retornar um functor aplicativo com esse valor dentro dele.
Quando dizemos *dentro dele*, estamos usando a analogia da caixa novamente, embora tenhamos visto que ela nem sempre resiste ao escrutínio.
Mas a declaração de tipo `a -> f a` ainda é bastante descritiva.
Pegamos um valor e o embrulhamos em um functor aplicativo que tem esse valor como resultado dentro dele.

Uma maneira melhor de pensar sobre `pure` seria dizer que ele pega um valor e o coloca em algum tipo de contexto padrão (ou puro) - um contexto mínimo que ainda produz esse valor.

A função `<*>` é realmente interessante.
Ela tem uma declaração de tipo de `f (a -> b) -> f a -> f b`.
Isso te lembra alguma coisa?
Claro, `fmap :: (a -> b) -> f a -> f b`.
É uma espécie de `fmap` reforçado.
Enquanto `fmap` pega uma função e um functor e aplica a função dentro do functor, `<*>` pega um functor que tem uma função nele e outro functor e meio que extrai essa função do primeiro functor e então a mapeia sobre o segundo.
Quando digo *extrair*, na verdade quero dizer *executar* e então extrair, talvez até *sequenciar*.
Veremos o porquê em breve.

Vamos dar uma olhada na implementação da instância `Applicative` para `Maybe`.

```{.haskell:hs}
instance Applicative Maybe where
    pure = Just
    Nothing <*> _ = Nothing
    (Just f) <*> something = fmap f something
```

Novamente, da definição da classe vemos que o `f` que desempenha o papel do Applicative Functor deve pegar um tipo concreto como parâmetro, então escrevemos `instance Applicative Maybe where` em vez de escrever `instance Applicative (Maybe a) where`.

Primeiro, `pure`.
Dissemos anteriormente que ele deve pegar algo e embrulhá-lo em um Applicative Functor.
Escrevemos `pure = Just`, porque construtores de valor como `Just` são funções normais.
Poderíamos ter escrito também `pure x = Just x`.

Em seguida, temos a definição para `<*>`.
Não podemos extrair uma função de um `Nothing`, porque ele não tem nenhuma função dentro dele.
Então dizemos que se tentarmos extrair uma função de um `Nothing`, o resultado é um `Nothing`.
Se você olhar para a definição da classe para `Applicative`, verá que há uma restrição de classe `Functor`, o que significa que podemos assumir que ambos os parâmetros de `<*>` são functores.
Se o primeiro parâmetro não for um `Nothing`, mas um `Just` com alguma função dentro dele, dizemos que queremos então mapear essa função sobre o segundo parâmetro.
Isso também cuida do caso em que o segundo parâmetro é `Nothing`, porque fazer `fmap` com qualquer função sobre um `Nothing` retornará um `Nothing`.

Então, para `Maybe`, `<*>` extrai a função do valor da esquerda se for um `Just` e a mapeia sobre o valor da direita.
Se algum dos parâmetros for `Nothing`, `Nothing` é o resultado.

OK, legal, ótimo.
Vamos dar uma volta com isso.

```{.haskell:hs}
ghci> Just (+3) <*> Just 9
Just 12
ghci> pure (+3) <*> Just 10
Just 13
ghci> pure (+3) <*> Just 9
Just 12
ghci> Just (++"hahah") <*> Nothing
Nothing
ghci> Nothing <*> Just "woot"
Nothing
```

Vemos como fazer `pure (+3)` e `Just (+3)` é o mesmo neste caso.
Use `pure` se estiver lidando com valores `Maybe` em um contexto aplicativo (ou seja, usando-os com `<*>`), caso contrário, fique com `Just`.
As primeiras quatro linhas de entrada demonstram como a função é extraída e então mapeada, mas neste caso, elas poderiam ter sido alcançadas apenas mapeando funções desembrulhadas sobre Functors.
A última linha é interessante, porque tentamos extrair uma função de um `Nothing` e então mapeá-la sobre algo, o que, claro, resulta em um `Nothing`.

Com Functors normais, você pode apenas mapear uma função sobre um Functor e então não pode tirar o resultado de nenhuma maneira geral, mesmo que o resultado seja uma função parcialmente aplicada.
Applicative Functors, por outro lado, permitem operar em vários Functors com uma única função.
Confira este pedaço de código:

```{.haskell:hs}
ghci> pure (+) <*> Just 3 <*> Just 5
Just 8
ghci> pure (+) <*> Just 3 <*> Nothing
Nothing
ghci> pure (+) <*> Nothing <*> Just 5
Nothing
```

![whaale](assets/images/functors-applicative-functors-and-monoids/whale.png){.right width=214 height=177}

O que está acontecendo aqui?
Vamos dar uma olhada, passo a passo.
`<*>` é associativo à esquerda, o que significa que `pure (+) <*> Just 3 <*> Just 5` é o mesmo que `(pure (+) <*> Just 3) <*> Just 5`.
Primeiro, a função `+` é colocada em um functor, que é neste caso um valor `Maybe` que contém a função.
Então, a princípio, temos `pure (+)`, que é `Just (+)`.
Em seguida, `Just (+) <*> Just 3` acontece.
O resultado disso é `Just (3+)`.
Isso é devido à aplicação parcial.
Apenas aplicar `3` à função `+` resulta em uma função que pega um parâmetro e adiciona 3 a ele.
Finalmente, `Just (3+) <*> Just 5` é realizado, o que resulta em um `Just 8`.

Isso não é incrível?!
Applicative Functors e o Applicative style de fazer `pure f <*> x <*> y <*> ...` nos permitem pegar uma função que espera parâmetros que não estão necessariamente embrulhados em Functors e usar essa função para operar em vários valores que estão em contextos de Functor.
A função pode pegar quantos parâmetros quisermos, porque é sempre parcialmente aplicada passo a passo entre ocorrências de `<*>`.

Isso se torna ainda mais prático e aparente se considerarmos o fato de que `pure f <*> x` é igual a `fmap f x`.
Esta é uma das leis dos Applicatives.
Daremos uma olhada mais de perto nelas mais tarde, mas por enquanto, podemos meio que intuitivamente ver que isso é verdade.
Pense sobre isso, faz sentido.
Como dissemos antes, `pure` coloca um valor em um contexto padrão.
Se apenas colocarmos uma função em um contexto padrão e então extrairmos e a aplicarmos a um valor dentro de outro Applicative Functor, fizemos o mesmo que apenas mapear essa função sobre esse Applicative Functor.
Em vez de escrever `pure f <*> x <*> y <*> ...`, podemos escrever `fmap f x <*> y <*> ...`.
É por isso que `Control.Applicative` exporta uma função chamada `<$>`, que é apenas `fmap` como um operador infixo.
Aqui está como ela é definida:

```{.haskell:hs}
(<$>) :: (Functor f) => (a -> b) -> f a -> f b
f <$> x = fmap f x
```

::: {.hintbox}
**Yo!**
Lembrete rápido: variáveis de tipo são independentes de nomes de parâmetros ou outros nomes de valor.
O `f` na declaração de função aqui é uma variável de tipo com uma restrição de classe dizendo que qualquer construtor de tipo que substitua `f` deve estar na typeclass `Functor`.
O `f` no corpo da função denota uma função que mapeamos sobre `x`.
O fato de termos usado `f` para representar ambos não significa que eles de alguma forma representem a mesma coisa.
:::

Ao usar `<$>`, o Applicative style realmente brilha, porque agora se quisermos aplicar uma função `f` entre três Applicative Functors, podemos escrever `f <$> x <*> y <*> z`.
Se os parâmetros não fossem Applicative Functors, mas valores normais, escreveríamos `f x y z`.

Vamos dar uma olhada mais de perto em como isso funciona.
Temos um valor de `Just "johntra"` e um valor de `Just "volta"` e queremos juntá-los em uma `String` dentro de um functor `Maybe`.
Fazemos isso:

```{.haskell:hs}
ghci> (++) <$> Just "johntra" <*> Just "volta"
Just "johntravolta"
```

Antes de vermos como isso acontece, compare a linha acima com esta:

```{.haskell:hs}
ghci> (++) "johntra" "volta"
"johntravolta"
```

Demais!
Para usar uma função normal em Applicative Functors, apenas espalhe alguns `<$>` e `<*>` por aí e a função operará em Applicatives e retornará um Applicative.
Quão legal é isso?

De qualquer forma, quando fazemos `(++) <$> Just "johntra" <*> Just "volta"`, primeiro `(++)`, que tem um tipo de `(++) :: [a] -> [a] -> [a]` é mapeado sobre `Just "johntra"`, resultando em um valor que é o mesmo que `Just ("johntra"++)` e tem um tipo de `Maybe ([Char] -> [Char])`.
Observe como o primeiro parâmetro de `(++)` foi comido e como os `a`s se transformaram em `Char`s.
E agora `Just ("johntra"++) <*> Just "volta"` acontece, o que tira a função do `Just` e a mapeia sobre `Just "volta"`, resultando em `Just "johntravolta"`.
Se algum dos dois valores tivesse sido `Nothing`, o resultado também teria sido `Nothing`.

Até agora, usamos apenas `Maybe` em nossos exemplos e você pode estar pensando que functores aplicativos são todos sobre `Maybe`.
Existem muitas outras instâncias de `Applicative`, então vamos conhecê-las!

Listas (na verdade o construtor de tipo de lista, `[]`) são Applicative Functors.
Que surpresa!
Aqui está como `[]` é uma instância de `Applicative`:

```{.haskell:hs}
instance Applicative [] where
    pure x = [x]
    fs <*> xs = [f x | f <- fs, x <- xs]
```

Anteriormente, dissemos que `pure` pega um valor e o coloca em um contexto padrão.
Ou em outras palavras, um contexto mínimo que ainda produz esse valor.
O contexto mínimo para listas seria a lista vazia, `[]`, mas a lista vazia representa a falta de um valor, então ela não pode conter em si mesma o valor em que usamos `pure`.
É por isso que `pure` pega um valor e o coloca em uma lista singleton.
Da mesma forma, o contexto mínimo para o functor aplicativo `Maybe` seria um `Nothing`, mas ele representa a falta de um valor em vez de um valor, então `pure` é implementado como `Just` na implementação da instância para `Maybe`.

```{.haskell:hs}
ghci> pure "Hey" :: [String]
["Hey"]
ghci> pure "Hey" :: Maybe String
Just "Hey"
```

E quanto a `<*>`?
Se olharmos para qual seria o tipo de `<*>` se fosse limitado apenas a listas, obtemos `(<*>) :: [a -> b] -> [a] -> [b]`.
É implementado com uma [list comprehension](starting-out.html#im-a-list-comprehension).
`<*>` tem que de alguma forma extrair a função de seu parâmetro esquerdo e então mapeá-la sobre o parâmetro direito.
Mas a questão aqui é que a lista da esquerda pode ter zero funções, uma função ou várias funções dentro dela.
A lista da direita também pode conter vários valores.
É por isso que usamos uma compreensão de lista para extrair de ambas as listas.
Aplicamos todas as funções possíveis da lista esquerda a todos os valores possíveis da lista direita.
A lista resultante tem todas as combinações possíveis de aplicar uma função da lista esquerda a um valor na direita.

```{.haskell:hs}
ghci> [(*0),(+100),(^2)] <*> [1,2,3]
[0,0,0,101,102,103,1,4,9]
```

A lista da esquerda tem três funções e a lista da direita tem três valores, então a lista resultante terá nove elementos.
Cada função na lista da esquerda é aplicada a cada valor na da direita.
Se tivermos uma lista de funções que pegam dois parâmetros, podemos aplicar essas funções entre duas listas.

```{.haskell:hs}
ghci> [(+),(*)] <*> [1,2] <*> [3,4]
[4,5,5,6,3,4,6,8]
```

Como `<*>` é associativo à esquerda, `[(+),(*)] <*> [1,2]` acontece primeiro, resultando em uma lista que é a mesma que `[(1+),(2+),(1*),(2*)]`, porque cada função à esquerda é aplicada a cada valor à direita.
Então, `[(1+),(2+),(1*),(2*)] <*> [3,4]` acontece, o que produz o resultado final.

Usar o estilo aplicativo com listas é divertido!
Veja:

```{.haskell:hs}
ghci> (++) <$> ["ha","heh","hmm"] <*> ["?","!","."]
["ha?","ha!","ha.","heh?","heh!","heh.","hmm?","hmm!","hmm."]
```

Novamente, veja como usamos uma função normal que pega duas strings entre dois functores aplicativos de strings apenas inserindo os operadores aplicativos apropriados.

Você pode ver listas como computações não determinísticas.
Um valor como `100` ou `"what"` pode ser visto como uma computação determinística que tem apenas um resultado, enquanto uma lista como `[1,2,3]` pode ser vista como uma computação que não consegue decidir qual resultado quer ter, então ela nos apresenta todos os resultados possíveis.
Então, quando você faz algo como `(+) <$> [1,2,3] <*> [4,5,6]`, pode pensar nisso como adicionar duas computações não determinísticas com `+`, apenas para produzir outra computação não determinística que é ainda menos certa sobre seu resultado.

Usar o Applicative style em listas é muitas vezes um bom substituto para compreensões de lista.
No segundo capítulo, queríamos ver todos os produtos possíveis de `[2,5,10]` e `[8,10,11]`, então fizemos isso:

```{.haskell:hs}
ghci> [ x*y | x <- [2,5,10], y <- [8,10,11]]
[16,20,22,40,50,55,80,100,110]
```

Estamos apenas extraindo de duas listas e aplicando uma função entre cada combinação de elementos.
Isso pode ser feito no Applicative style também:

```{.haskell:hs}
ghci> (*) <$> [2,5,10] <*> [8,10,11]
[16,20,22,40,50,55,80,100,110]
```

Isso parece mais claro para mim, porque é mais fácil ver que estamos apenas chamando `*` entre duas computações não determinísticas.
Se quiséssemos todos os produtos possíveis dessas duas listas que são maiores que 50, apenas faríamos:

```{.haskell:hs}
ghci> filter (>50) $ (*) <$> [2,5,10] <*> [8,10,11]
[55,80,100,110]
```

É fácil ver como `pure f <*> xs` é igual a `fmap f xs` com listas.
`pure f` é apenas `[f]` e `[f] <*> xs` aplicará cada função na lista esquerda a cada valor na direita, mas há apenas uma função na lista esquerda, então é como mapear.

Outra instância de `Applicative` que já encontramos é `IO`.
É assim que a instância é implementada:

```{.haskell:hs}
instance Applicative IO where
    pure = return
    a <*> b = do
        f <- a
        x <- b
        return (f x)
```

![ahahahah!](assets/images/functors-applicative-functors-and-monoids/knight.png){.left width=195 height=458}

Já que `pure` é tudo sobre colocar um valor em um contexto mínimo que ainda o mantém como seu resultado, faz sentido que `pure` seja apenas `return`, porque `return` faz exatamente isso; ele faz uma ação de E/S que não faz nada, apenas produz algum valor como seu resultado, mas realmente não faz nenhuma operação de E/S como imprimir no terminal ou ler de um arquivo.

Se `<*>` fosse especializado para `IO`, teria um tipo de `(<*>) :: IO (a -> b) -> IO a -> IO b`.
Ele pegaria uma ação de E/S que produz uma função como seu resultado e outra ação de E/S e criaria uma nova ação de E/S a partir dessas duas que, quando executada, primeiro executa a primeira para obter a função e depois executa a segunda para obter o valor e então produziria essa função aplicada ao valor como seu resultado.
Usamos a sintaxe *do* para implementá-lo aqui.
Lembre-se, a sintaxe *do* é sobre pegar várias ações de E/S e colá-las em uma, que é exatamente o que fazemos aqui.

Com `Maybe` e `[]`, poderíamos pensar em `<*>` como simplesmente extraindo uma função de seu parâmetro esquerdo e então meio que aplicando-a sobre o direito.
Com `IO`, extrair ainda está no jogo, mas agora também temos uma noção de *sequenciamento*, porque estamos pegando duas ações de E/S e estamos sequenciando, ou colando-as em uma.
Temos que extrair a função da primeira ação de E/S, mas para extrair um resultado de uma ação de E/S, ela tem que ser executada.

Considere isto:

```{.haskell:hs}
myAction :: IO String
myAction = do
    a <- getLine
    b <- getLine
    return $ a ++ b
```

Esta é uma ação de E/S que solicitará ao usuário duas linhas e produzirá como resultado essas duas linhas concatenadas.
Conseguimos isso colando duas ações de E/S `getLine` e um `return`, porque queríamos que nossa nova ação de E/S colada contivesse o resultado de `a ++ b`.
Outra maneira de escrever isso seria usar o Applicative style.

```{.haskell:hs}
myAction :: IO String
myAction = (++) <$> getLine <*> getLine
```

O que estávamos fazendo antes era fazer uma ação de E/S que aplicava uma função entre os resultados de duas outras ações de E/S, e isso é a mesma coisa.
Lembre-se, `getLine` é uma ação de E/S com o tipo `getLine :: IO String`.
Quando usamos `<*>` entre dois Applicative Functors, o resultado é um Applicative Functor, então tudo isso faz sentido.

Se regredirmos à analogia da caixa, podemos imaginar `getLine` como uma caixa que sairá para o mundo real e nos buscará uma string.
Fazer `(++) <$> getLine <*> getLine` faz uma caixa nova e maior que envia essas duas caixas para buscar linhas do terminal e então apresenta a concatenação dessas duas linhas como seu resultado.

O tipo da expressão `(++) <$> getLine <*> getLine` é `IO String`, o que significa que essa expressão é uma ação de E/S completamente normal como qualquer outra, que também contém um valor resultante dentro dela, assim como outras ações de E/S.
É por isso que podemos fazer coisas como:

```{.haskell:hs}
main = do
    a <- (++) <$> getLine <*> getLine
    putStrLn $ "The two lines concatenated turn out to be: " ++ a
```

Se você se encontrar vinculando algumas ações de E/S a nomes e então chamando alguma função nelas e apresentando isso como resultado usando `return`, considere usar o Applicative style porque é indiscutivelmente um pouco mais conciso e terso.

Outra instância de `Applicative` é `(->) r`, então funções.
Elas são raramente usadas com o Applicative style fora do code golf, mas ainda são interessantes como aplicativos, então vamos dar uma olhada em como a instância da função é implementada.

::: {.hintbox}
Se você está confuso sobre o que `(->) r` significa, confira a seção anterior onde explicamos como `(->) r` é um Functor.
:::

```{.haskell:hs}
instance Applicative ((->) r) where
    pure x = (\_ -> x)
    f <*> g = \x -> f x (g x)
```

Quando embrulhamos um valor em um Applicative Functor com `pure`, o resultado que ele produz sempre tem que ser esse valor.
Um contexto padrão mínimo que ainda produz esse valor como resultado.
É por isso que na implementação da instância de função, `pure` pega um valor e cria uma função que ignora seu parâmetro e sempre retorna esse valor.
Se olharmos para o tipo de `pure`, mas especializado para a instância `(->) r`, é `pure :: a -> (r -> a)`.

```{.haskell:hs}
ghci> (pure 3) "blah"
3
```

Por causa do currying, a aplicação de função é associativa à esquerda, então podemos omitir os parênteses.

```{.haskell:hs}
ghci> pure 3 "blah"
3
```

A implementação da instância para `<*>` é um pouco enigmática, então é melhor apenas dar uma olhada em como usar funções como Applicative Functors no Applicative style.

```{.haskell:hs}
ghci> :t (+) <$> (+3) <*> (*100)
(+) <$> (+3) <*> (*100) :: (Num a) => a -> a
ghci> (+) <$> (+3) <*> (*100) $ 5
508
```

Chamar `<*>` com dois Applicative Functors resulta em um Applicative Functor, então se o usarmos em duas funções, obtemos de volta uma função.
Então o que acontece aqui?
Quando fazemos `(+) <$> (+3) <*> (*100)`, estamos criando uma função que usará `+` nos resultados de `(+3)` e `(*100)` e retornará isso.
Para demonstrar em um exemplo real, quando fizemos `(+) <$> (+3) <*> (*100) $ 5`, o `5` primeiro foi aplicado a `(+3)` e `(*100)`, resultando em `8` e `500`.
Então, `+` é chamado com `8` e `500`, resultando em `508`.

```{.haskell:hs}
ghci> (\x y z -> [x,y,z]) <$> (+3) <*> (*2) <*> (/2) $ 5
[8.0,10.0,2.5]
```

![SLAP](assets/images/functors-applicative-functors-and-monoids/jazzb.png){.right width=400px height=230px}

O mesmo aqui.
Criamos uma função que chamará a função `\x y z -> [x,y,z]` com os eventuais resultados de `(+3)`, `(*2)` e `(/2)`.
O `5` é alimentado para cada uma das três funções e então `\x y z -> [x, y, z]` é chamado com esses resultados.

Você pode pensar em funções como caixas que contêm seus eventuais resultados, então fazer `k <$> f <*> g` cria uma função que chamará `k` com os eventuais resultados de `f` e `g`.
Quando fazemos algo como `(+) <$> Just 3 <*> Just 5`, estamos usando `+` em valores que podem ou não estar lá, o que também resulta em um valor que pode ou não estar lá.
Quando fazemos `(+) <$> (+10) <*> (+5)`, estamos usando `+` nos valores de retorno futuros de `(+10)` e `(+5)` e o resultado também é algo que produzirá um valor apenas quando chamado com um parâmetro.

Não usamos funções como Applicatives com frequência, mas isso ainda é muito interessante.
Não é muito importante que você entenda como a instância `(->) r` para `Applicative` funciona, então não se desespere se não estiver entendendo isso agora.
Tente brincar com o Applicative style e funções para construir uma intuição para funções como Applicatives.

Uma instância de `Applicative` que ainda não encontramos é `ZipList`, e ela vive em `Control.Applicative`.

Acontece que na verdade existem mais maneiras de listas serem Applicative Functors.
Uma maneira é a que já cobrimos, que diz que chamar `<*>` com uma lista de funções e uma lista de valores resulta em uma lista que tem todas as combinações possíveis de aplicar funções da lista esquerda aos valores na lista direita.
Se fizermos `[(+3),(*2)] <*> [1,2]`, `(+3)` será aplicado tanto a `1` quanto a `2` e `(*2)` também será aplicado tanto a `1` quanto a `2`, resultando em uma lista que tem quatro elementos, ou seja, `[4,5,2,4]`.

No entanto, `[(+3),(*2)] <*> [1,2]` também poderia funcionar de tal maneira que a primeira função na lista esquerda é aplicada ao primeiro valor na direita, a segunda função é aplicada ao segundo valor, e assim por diante.
Isso resultaria em uma lista com dois valores, ou seja, `[4,4]`.
Você poderia ver isso como `[1 + 3, 2 * 2]`.

Como um tipo não pode ter duas instâncias para a mesma typeclass, o tipo `ZipList a` foi introduzido, que tem um construtor `ZipList` que tem apenas um campo, e esse campo é uma lista.
Aqui está a instância:

```{.haskell:hs}
instance Applicative ZipList where
        pure x = ZipList (repeat x)
        ZipList fs <*> ZipList xs = ZipList (zipWith (\f x -> f x) fs xs)
```

`<*>` faz exatamente o que dissemos.
Aplica a primeira função ao primeiro valor, a segunda função ao segundo valor, etc.
Isso é feito com `zipWith (\f x -> f x) fs xs`.
Por causa de como `zipWith` funciona, a lista resultante será tão longa quanto a mais curta das duas listas.

`pure` também é interessante aqui.
Ele pega um valor e o coloca em uma lista que apenas tem esse valor repetindo indefinidamente.
`pure "haha"` resulta em `ZipList (["haha","haha","haha"...`.
Isso pode ser um pouco confuso, já que dissemos que `pure` deveria colocar um valor em um contexto mínimo que ainda produz esse valor.
E você pode estar pensando que uma lista infinita de algo dificilmente é mínima.
Mas faz sentido com zip lists, porque tem que produzir o valor em cada posição.
Isso também satisfaz a lei de que `pure f <*> xs` deve ser igual a `fmap f xs`.
Se `pure 3` apenas retornasse `ZipList [3]`, `pure (*2) <*> ZipList [1,5,10]` resultaria em `ZipList [2]`, porque a lista resultante de duas listas zipadas tem o comprimento da mais curta das duas.
Se ziparmos uma lista finita com uma lista infinita, o comprimento da lista resultante será sempre igual ao comprimento da lista finita.

Então, como zip lists funcionam em um Applicative style?
Vamos ver.
Ah, o tipo `ZipList a` não tem uma instância `Show`, então temos que usar a função `getZipList`{.label .function} para extrair uma lista bruta de uma zip list.

```{.haskell:hs}
ghci> getZipList $ (+) <$> ZipList [1,2,3] <*> ZipList [100,100,100]
[101,102,103]
ghci> getZipList $ (+) <$> ZipList [1,2,3] <*> ZipList [100,100..]
[101,102,103]
ghci> getZipList $ max <$> ZipList [1,2,3,4,5,3] <*> ZipList [5,3,1,2]
[5,3,3,4]
ghci> getZipList $ (,,) <$> ZipList "dog" <*> ZipList "cat" <*> ZipList "rat"
[('d','c','r'),('o','a','a'),('g','t','t')]
```

::: {.hintbox}
A função `(,,)` é a mesma que `\x y z -> (x,y,z)`.
Além disso, a função `(,)` é a mesma que `\x y -> (x,y)`.
:::

Além de `zipWith`, a biblioteca padrão tem funções como `zipWith3`, `zipWith4`, até 7.
`zipWith` pega uma função que pega dois parâmetros e zipa duas listas com ela.
`zipWith3` pega uma função que pega três parâmetros e zipa três listas com ela, e assim por diante.
Nada de especial, apenas aplica uma função entre dois Applicatives, escondendo o Applicative style com o qual nos familiarizamos.
A razão pela qual estamos olhando para isso é porque ela mostra claramente por que Applicative Functors são mais poderosos do que apenas Functors comuns.
Com Functors comuns, podemos apenas mapear funções sobre um Functor.
Mas com Applicative Functors, podemos aplicar uma função entre vários Functors.
Também é interessante olhar para o tipo desta função como `(a -> b -> c) -> (f a -> f b -> f c)`.
Quando olhamos para ela assim, podemos dizer que `liftA2` pega uma função binária normal e a promove para uma função que opera em dois Functors.

Aqui está um conceito interessante: podemos pegar dois Applicative Functors e combiná-los em um Applicative Functor que tem dentro dele os resultados desses dois Applicative Functors em uma lista.
Por exemplo, temos `Just 3` e `Just 4`.
Vamos assumir que o segundo tem uma lista singleton dentro dele, porque isso é realmente fácil de conseguir:

```{.haskell:hs}
ghci> fmap (\x -> [x]) (Just 4)
Just [4]
```

OK, então digamos que temos `Just 3` e `Just [4]`.
Como obtemos `Just [3,4]`?
Fácil.

```{.haskell:hs}
ghci> liftA2 (:) (Just 3) (Just [4])
Just [3,4]
ghci> (:) <$> Just 3 <*> Just [4]
Just [3,4]
```

Lembre-se, `:` é uma função que pega um elemento e uma lista e retorna uma nova lista com esse elemento no início.
Agora que temos `Just [3,4]`, poderíamos combinar isso com `Just 2` para produzir `Just [2,3,4]`?
Claro que poderíamos.
Parece que podemos combinar qualquer quantidade de Applicatives em um Applicative que tem uma lista dos resultados desses Applicatives dentro dele.
Vamos tentar implementar uma função que pega uma lista de Applicatives e retorna um Applicative que tem uma lista como seu valor resultante.
Vamos chamá-la de `sequenceA`.

```{.haskell:hs}
sequenceA :: (Applicative f) => [f a] -> f [a]
sequenceA [] = pure []
sequenceA (x:xs) = (:) <$> x <*> sequenceA xs
```

Ah, recursão!
Primeiro, olhamos para o tipo.
Ela transformará uma lista de Applicatives em um Applicative com uma lista.
A partir disso, podemos estabelecer algumas bases para uma condição de borda.
Se quisermos transformar uma lista vazia em um Applicative com uma lista de resultados, bem, apenas colocamos uma lista vazia em um contexto padrão.
Agora vem a recursão.
Se tivermos uma lista com uma cabeça e uma cauda (lembre-se, `x` é um Applicative e `xs` é uma lista deles), chamamos `sequenceA` na cauda, o que resulta em um Applicative com uma lista.
Então, apenas prefixamos the valor dentro do Applicative `x` nessa lista dentro do Applicative, e é isso!

Então se fizermos `sequenceA [Just 1, Just 2]`, isso é `(:) <$> Just 1 <*> sequenceA [Just 2]`.
Isso é igual a `(:) <$> Just 1 <*> ((:) <$> Just 2 <*> sequenceA [])`.
Ah!
Sabemos que `sequenceA []` acaba sendo `Just []`, então esta expressão agora é `(:) <$> Just 1 <*> ((:) <$> Just 2 <*> Just [])`, que é `(:) <$> Just 1 <*> Just [2]`, que é `Just [1,2]`!

Outra maneira de implementar `sequenceA` é com um fold.
Lembre-se, praticamente qualquer função onde passamos por uma lista elemento por elemento e acumulamos um resultado ao longo do caminho pode ser implementada com um fold.

```{.haskell:hs}
sequenceA :: (Applicative f) => [f a] -> f [a]
sequenceA = foldr (liftA2 (:)) (pure [])
```

Abordamos a lista pela direita e começamos com um valor acumulador de `pure []`.
Fazemos `liftA2 (:)` entre o acumulador e o último elemento da lista, o que resulta em um aplicativo que tem um singleton nele.
Então fazemos `liftA2 (:)` com o agora último elemento e o acumulador atual e assim por diante, até ficarmos apenas com o acumulador, que contém uma lista dos resultados de todos os aplicativos.

Vamos dar uma volta com nossa função em alguns Applicatives.

```{.haskell:hs}
ghci> sequenceA [Just 3, Just 2, Just 1]
Just [3,2,1]
ghci> sequenceA [Just 3, Nothing, Just 1]
Nothing
ghci> sequenceA [(+3),(+2),(+1)] 3
[6,5,4]
ghci> sequenceA [[1,2,3],[4,5,6]]
[[1,4],[1,5],[1,6],[2,4],[2,5],[2,6],[3,4],[3,5],[3,6]]
ghci> sequenceA [[1,2,3],[4,5,6],[3,4,4],[]]
[]
```

Ah!
Muito legal.
Quando usado em valores `Maybe`, `sequenceA` cria um valor `Maybe` com todos os resultados dentro dele como uma lista.
Se um dos valores fosse `Nothing`, então o resultado também é um `Nothing`.
Isso é legal quando você tem uma lista de valores `Maybe` e está interessado nos valores apenas se nenhum deles for um `Nothing`.

Quando usado com funções, `sequenceA` pega uma lista de funções e retorna uma função que retorna uma lista.
Em nosso exemplo, fizemos uma função que pegava um número como parâmetro e o aplicava a cada função na lista e então retornava uma lista de resultados.
`sequenceA [(+3),(+2),(+1)] 3` chamará `(+3)` com `3`, `(+2)` com `3` e `(+1)` com `3` e apresentará todos esses resultados como uma lista.

Fazer `(+) <$> (+3) <*> (*2)` criará uma função que pega um parâmetro, o alimenta para `(+3)` e `(*2)` e então chama `+` com esses dois resultados.
Na mesma veia, faz sentido que `sequenceA [(+3),(*2)]` faz uma função que pega um parâmetro e o alimenta para todas as funções na lista.
Em vez de chamar `+` com os resultados das funções, uma combinação de `:` e `pure []` é usada para reunir esses resultados em uma lista, que é o resultado dessa função.

Usar `sequenceA` é legal quando temos uma lista de funções e queremos alimentar a mesma entrada para todas elas e então ver a lista de resultados.
Por exemplo, temos um número e estamos nos perguntando se ele satisfaz todos os predicados em uma lista.
Uma maneira de fazer isso seria assim:

```{.haskell:hs}
ghci> map (\f -> f 7) [(>4),(<10),odd]
[True,True,True]
ghci> and $ map (\f -> f 7) [(>4),(<10),odd]
True
```

Lembre-se, `and` pega uma lista de booleanos e retorna `True` se todos forem `True`.
Outra maneira de alcançar a mesma coisa seria com `sequenceA`:

```{.haskell:hs}
ghci> sequenceA [(>4),(<10),odd] 7
[True,True,True]
ghci> and $ sequenceA [(>4),(<10),odd] 7
True
```

`sequenceA [(>4),(<10),odd]` cria uma função que pegará um número e o alimentará para todos os predicados em `[(>4),(<10),odd]` e retornará uma lista de booleanos.
Ela transforma uma lista com o tipo `(Num a) => [a -> Bool]` em uma função com o tipo `(Num a) => a -> [Bool]`.
Muito legal, hein?

Porque listas são homogêneas, todas as funções na lista têm que ser funções do mesmo tipo, claro.
Você não pode ter uma lista como `[ord, (+3)]`, porque `ord` pega um caractere e retorna um número, enquanto `(+3)` pega um número e retorna um número.

Quando usado com `[]`, `sequenceA` pega uma lista de listas e retorna uma lista de listas.
Hmm, interessante.
Ela na verdade cria listas que têm todas as combinações possíveis de seus elementos.
Para ilustração, aqui está o acima feito com `sequenceA` e depois feito com uma compreensão de lista:

```{.haskell:hs}
ghci> sequenceA [[1,2,3],[4,5,6]]
[[1,4],[1,5],[1,6],[2,4],[2,5],[2,6],[3,4],[3,5],[3,6]]
ghci> [[x,y] | x <- [1,2,3], y <- [4,5,6]]
[[1,4],[1,5],[1,6],[2,4],[2,5],[2,6],[3,4],[3,5],[3,6]]
ghci> sequenceA [[1,2],[3,4]]
[[1,3],[1,4],[2,3],[2,4]]
ghci> [[x,y] | x <- [1,2], y <- [3,4]]
[[1,3],[1,4],[2,3],[2,4]]
ghci> sequenceA [[1,2],[3,4],[5,6]]
[[1,3,5],[1,3,6],[1,4,5],[1,4,6],[2,3,5],[2,3,6],[2,4,5],[2,4,6]]
ghci> [[x,y,z] | x <- [1,2], y <- [3,4], z <- [5,6]]
[[1,3,5],[1,3,6],[1,4,5],[1,4,6],[2,3,5],[2,3,6],[2,4,5],[2,4,6]]
```

Isso pode ser um pouco difícil de entender, mas se você brincar com isso por um tempo, verá como funciona.
Digamos que estamos fazendo `sequenceA [[1,2],[3,4]]`.
Para ver como isso acontece, vamos usar a definição `sequenceA (x:xs) = (:) <$> x <*> sequenceA xs` de `sequenceA` e a condição de borda `sequenceA [] = pure []`.
Você não precisa seguir esta avaliação, mas pode ajudar se tiver problemas para imaginar como `sequenceA` funciona em listas de listas, porque pode ser um pouco confuso.

* Começamos com `sequenceA [[1,2],[3,4]]`
* Isso avalia para `(:) <$> [1,2] <*> sequenceA [[3,4]]`
* Avaliando o `sequenceA` interno ainda mais, obtemos `(:) <$> [1,2] <*> ((:) <$> [3,4] <*> sequenceA [])`
* Atingimos a condição de borda, então isso agora é `(:) <$> [1,2] <*> ((:) <$> [3,4] <*> [[]])`
* Agora, avaliamos a parte `(:) <$> [3,4] <*> [[]]`, que usará `:` com todos os valores possíveis na lista esquerda (valores possíveis são `3` e `4`) com todos os valores possíveis na lista direita (único valor possível é `[]`), o que resulta em `[3:[], 4:[]]`, que é `[[3],[4]]`.
  Então agora temos `(:) <$> [1,2] <*> [[3],[4]]`
* Agora, `:` é usado com todos os valores possíveis da lista esquerda (`1` e `2`) com todos os valores possíveis da lista direita (`[3]` e `[4]`), o que resulta em `[1:[3], 1:[4], 2:[3], 2:[4]]`, que é `[[1,3],[1,4],[2,3],[2,4]`

Fazer `(+) <$> [1,2] <*> [4,5,6]` resulta em uma computação não determinística `x + y` onde `x` assume todos os valores de `[1,2]` e `y` assume todos os valores de `[4,5,6]`.
Representamos isso como uma lista que contém todos os resultados possíveis.
Da mesma forma, quando fazemos `sequence [[1,2],[3,4],[5,6],[7,8]]`, o resultado é uma computação não determinística `[x,y,z,w]`, onde `x` assume todos os valores de `[1,2]`, `y` assume todos os valores de `[3,4]` e assim por diante.
Para representar o resultado dessa computação não determinística, usamos uma lista, onde cada elemento na lista é uma lista possível.
É por isso que o resultado é uma lista de listas.

Quando usado com ações de E/S, `sequenceA` é a mesma coisa que `sequence`!
Pega uma lista de ações de E/S e retorna uma ação de E/S que executará cada uma dessas ações e terá como resultado uma lista dos resultados dessas ações de E/S.
Isso porque para transformar um valor `[IO a]` em um valor `IO [a]`, para fazer uma ação de E/S que produz uma lista de resultados quando executada, todas essas ações de E/S têm que ser sequenciadas para que sejam executadas uma após a outra quando a avaliação for forçada.
Você não pode obter o resultado de uma ação de E/S sem executá-la.

```{.haskell:hs}
ghci> sequenceA [getLine, getLine, getLine]
heyh
ho
woo
["heyh","ho","woo"]
```

Como Functors normais, Applicative Functors vêm com algumas leis.
A mais importante é a que já mencionamos, ou seja, que `pure f <*> x = fmap f x`{.label .law} se mantém.
Como exercício, você pode provar esta lei para alguns dos Applicative Functors que encontramos neste capítulo. As outras leis dos Applicative Functors são:

* `pure id <*> v = v`{.label .law}
* `pure (.) <*> u <*> v <*> w = u <*> (v <*> w)`{.label .law}
* `pure f <*> pure x = pure (f x)`{.label .law}
* `u <*> pure y = pure ($ y) <*> u`{.label .law}

Não vamos repasá-las em detalhes agora porque isso ocuparia muitas páginas e provavelmente seria meio chato, mas se você estiver pronto para a tarefa, pode dar uma olhada mais de perto nelas e ver se elas se mantêm para algumas das instâncias.

Em conclusão, Applicative Functors não são apenas interessantes, eles também são úteis, porque nos permitem combinar diferentes computações, como computações de E/S, computações não determinísticas, computações que podem ter falhado, etc. usando o Applicative style.
Apenas usando `<$>` e `<*>` podemos usar funções normais para operar uniformemente em qualquer número de Applicative Functors e tirar proveito da semântica de cada um.

## A palavra-chave newtype {#the-newtype-keyword}

![why so serious?](assets/images/functors-applicative-functors-and-monoids/maoi.png){.left width=107 height=202}

Até agora, aprendemos como fazer nossos próprios tipos de dados algébricos usando a palavra-chave **data**.
Também aprendemos como dar sinônimos a tipos existentes com a palavra-chave **type**.
Nesta seção, daremos uma olhada em como fazer novos tipos a partir de tipos de dados existentes usando a palavra-chave **newtype** e por que gostaríamos de fazer isso em primeiro lugar.

Na seção anterior, vimos que há na verdade mais maneiras para o tipo de lista ser um Applicative Functor.
Uma maneira é ter `<*>` pegando cada função da lista que é seu parâmetro esquerdo e aplicando-a a cada valor na lista que está à direita, resultando em todas as combinações possíveis de aplicar uma função da lista esquerda a um valor na lista direita.

```{.haskell:hs}
ghci> [(+1),(*100),(*5)] <*> [1,2,3]
[2,3,4,100,200,300,5,10,15]
```

A segunda maneira é pegar a primeira função no lado esquerdo de `<*>` e aplicá-la ao primeiro valor à direita, depois pegar a segunda função da lista no lado esquerdo e aplicá-la ao segundo valor à direita, e assim por diante.
Basicamente, é meio que como zipar as duas listas juntas.
Mas as listas já são uma instância de `Applicative`, então como também fizemos das listas uma instância de `Applicative` dessa segunda maneira?
Se você se lembra, dissemos que o tipo `ZipList a` foi introduzido por esse motivo, que tem um construtor de valor, `ZipList`, que tem apenas um campo.
Colocamos a lista que estamos embrulhando nesse campo.
Então, `ZipList` foi feita uma instância de `Applicative`, de modo que quando queremos usar listas como Applicatives na maneira de zipar, apenas as embrulhamos com o construtor `ZipList` e então, depois que terminamos, as desembrolhamos com `getZipList`:

```{.haskell:hs}
ghci> getZipList $ ZipList [(+1),(*100),(*5)] <*> ZipList [1,2,3]
[2,200,15]
```

Então, o que isso tem a ver com essa palavra-chave *newtype*?
Bem, pense em como poderíamos escrever a declaração de dados para nosso tipo `ZipList a`.
Uma maneira seria fazer assim:

```{.haskell:hs}
data ZipList a = ZipList [a]
```

Um tipo que tem apenas um construtor de valor e esse construtor de valor tem apenas um campo que é uma lista de coisas.
Também poderíamos querer usar a sintaxe de registro para obter automaticamente uma função que extrai uma lista de uma `ZipList`:

```{.haskell:hs}
data ZipList a = ZipList { getZipList :: [a] }
```

Isso parece bom e na verdade funcionaria muito bem.
Tínhamos duas maneiras de tornar um tipo existente uma instância de uma typeclass, então usamos a palavra-chave *data* para apenas embrulhar esse tipo em outro tipo e fizemos o outro tipo uma instância da segunda maneira.

A palavra-chave *newtype* em Haskell é feita exatamente para esses casos quando queremos apenas pegar um tipo e embrulhá-lo em algo para apresentá-lo como outro tipo.
Nas bibliotecas reais, `ZipList a` é definido assim:

```{.haskell:hs}
newtype ZipList a = ZipList { getZipList :: [a] }
```

Em vez da palavra-chave *data*, a palavra-chave *newtype* é usada.
Agora por que isso?
Bem, para começar, *newtype* é mais rápido.
Se você usar a palavra-chave *data* para embrulhar um tipo, há alguma sobrecarga para todo esse embrulho e desembrulho quando seu programa está em execução.
Mas se você usar *newtype*, o Haskell sabe que você está apenas usando isso para embrulhar um tipo existente em um novo tipo (daí o nome), porque você quer que seja o mesmo internamente, mas tenha um tipo diferente.
Com isso em mente, o Haskell pode se livrar do embrulho e desembrulho assim que resolver qual valor é de que tipo.

Então, por que não usar apenas *newtype* o tempo todo em vez de *data* então?
Bem, quando você faz um novo tipo a partir de um tipo existente usando a palavra-chave *newtype*, você pode ter apenas um construtor de valor e esse construtor de valor pode ter apenas um campo.
Mas com *data*, você pode criar tipos de dados que têm vários construtores de valor e cada construtor pode ter zero ou mais campos:

```{.haskell:hs}
data Profession = Fighter | Archer | Accountant

data Race = Human | Elf | Orc | Goblin

data PlayerCharacter = PlayerCharacter Race Profession
```

Ao usar *newtype*, você está restrito a apenas um construtor com um campo.

Também podemos usar a palavra-chave *deriving* com *newtype* assim como faríamos com *data*.
Podemos derivar instâncias para `Eq`, `Ord`, `Enum`, `Bounded`, `Show` e `Read`.
Se derivarmos a instância para uma typeclass, o tipo que estamos embrulhando deve estar nessa typeclass para começar.
Faz sentido, porque *newtype* apenas embrulha um tipo existente.
Então agora se fizermos o seguinte, podemos imprimir e igualar valores de nosso novo tipo:

```{.haskell:hs}
newtype CharList = CharList { getCharList :: [Char] } deriving (Eq, Show)
```

Vamos dar uma chance a isso:

```{.haskell:hs}
ghci> CharList "this will be shown!"
CharList {getCharList = "this will be shown!"}
ghci> CharList "benny" == CharList "benny"
True
ghci> CharList "benny" == CharList "oysters"
False
```

Neste *newtype* em particular, o construtor de valor tem o seguinte tipo:

```{.haskell:hs}
CharList :: [Char] -> CharList
```

Ele pega um valor `[Char]`, como `"my sharona"` e retorna um valor `CharList`.
A partir dos exemplos acima, onde usamos o construtor de valor `CharList`, vemos que realmente é o caso.
Por outro lado, a função `getCharList`, que foi gerada para nós porque usamos sintaxe de registro em nosso *newtype*, tem esse tipo:

```{.haskell:hs}
getCharList :: CharList -> [Char]
```

Ela pega um valor `CharList` e o converte para um valor `[Char]`.
Você pode pensar nisso como embrulhar e desembrulhar, mas também pode pensar nisso como converter valores de um tipo para o outro.

### Usando newtype para fazer instâncias de typeclass 

Muitas vezes, queremos tornar nossos tipos instâncias de certas typeclasses, mas os parâmetros de tipo simplesmente não coincidem para o que queremos fazer.
É fácil fazer de `Maybe` uma instância de `Functor`, porque a typeclass `Functor` é definida assim:

```{.haskell:hs}
class Functor f where
    fmap :: (a -> b) -> f a -> f b
```

Então apenas começamos com:

```{.haskell:hs}
instance Functor Maybe where
```

E então implementamos `fmap`.
Todos os parâmetros de tipo batem porque o `Maybe` toma o lugar de `f` na definição da typeclass `Functor` e assim se olharmos para `fmap` como se ele funcionasse apenas em `Maybe`, ele acaba se comportando como:

```{.haskell:hs}
fmap :: (a -> b) -> Maybe a -> Maybe b
```

![wow, very evil](assets/images/functors-applicative-functors-and-monoids/krakatoa.png){.right width=322px height=280px}

Não é uma beleza?
Agora, e se quiséssemos tornar a tupla uma instância de `Functor` de tal maneira que quando fazemos `fmap` de uma função sobre uma tupla, ela é aplicada ao primeiro componente da tupla?
Dessa forma, fazer `fmap (+3) (1,1)` resultaria em `(4,1)`.
Acontece que escrever a instância para isso é meio difícil.
Com `Maybe`, apenas dizemos `instance Functor Maybe where` porque apenas construtores de tipo que pegam exatamente um parâmetro podem ser feitos uma instância de `Functor`.
Mas parece que não há maneira de fazer algo assim com `(a,b)` para que o parâmetro de tipo `a` acabe sendo o que muda quando usamos `fmap`.
Para contornar isso, podemos dar *newtype* em nossa tupla de tal maneira que o segundo parâmetro de tipo represente o tipo do primeiro componente na tupla:

```{.haskell:hs}
newtype Pair b a = Pair { getPair :: (a,b) }
```

E agora, podemos torná-lo uma instância de `Functor` para que a função seja mapeada sobre o primeiro componente:

```{.haskell:hs}
instance Functor (Pair c) where
    fmap f (Pair (x,y)) = Pair (f x, y)
```

Como você pode ver, podemos fazer pattern matching em tipos definidos com *newtype*.
Fazemos pattern matching para obter a tupla subjacente, então aplicamos a função `f` ao primeiro componente na tupla e então usamos o construtor de valor `Pair` para converter a tupla de volta para nosso `Pair b a`.
Se imaginarmos qual seria o tipo de `fmap` se ele funcionasse apenas em nossos novos pares, seria:

```{.haskell:hs}
fmap :: (a -> b) -> Pair c a -> Pair c b
```

Novamente, dissemos `instance Functor (Pair c) where` e então `Pair c` tomou o lugar do `f` na definição da typeclass para `Functor`:

```{.haskell:hs}
class Functor f where
    fmap :: (a -> b) -> f a -> f b
```

Então agora, se convertermos uma tupla em um `Pair b a`, podemos usar `fmap` sobre ela e a função será mapeada sobre o primeiro componente:

```{.haskell:hs}
ghci> getPair $ fmap (*100) (Pair (2,3))
(200,3)
ghci> getPair $ fmap reverse (Pair ("london calling", 3))
("gnillac nodnol",3)
```

### Sobre a preguiça do newtype 

Mencionamos que *newtype* geralmente é mais rápido que *data*.
A única coisa que pode ser feita com *newtype* é transformar um tipo existente em um novo tipo, então, internamente, o Haskell pode representar os valores dos tipos definidos com *newtype* exatamente como os originais, apenas lembrando que seus tipos agora são distintos.
Esse fato significa que *newtype* não é apenas mais rápido, é também mais preguiçoso.
Vamos dar uma olhada no que isso significa.

Como dissemos antes, o Haskell é preguiçoso por padrão, o que significa que apenas quando tentamos imprimir os resultados de nossas funções é que qualquer cálculo ocorrerá.
Além disso, apenas os cálculos necessários para nossa função nos dizer o resultado serão realizados.
O valor `undefined` (indefinido) em Haskell representa um cálculo errôneo.
Se tentarmos avaliá-lo (ou seja, forçar o Haskell a realmente calculá-lo) imprimindo-o no terminal, o Haskell terá um ataque de pelanca (tecnicamente referido como uma exceção):

```{.haskell:hs}
ghci> undefined
*** Exception: Prelude.undefined
```

No entanto, se fizermos uma lista que contém alguns valores `undefined`, mas solicitarmos apenas a cabeça da lista, que não é `undefined`, tudo correrá bem porque o Haskell realmente não precisa avaliar nenhum outro elemento em uma lista se quisermos ver apenas qual é o primeiro elemento:

```{.haskell:hs}
ghci> head [3,4,5,undefined,2,undefined]
3
```

Agora considere o seguinte tipo:

```{.haskell:hs}
data CoolBool = CoolBool { getCoolBool :: Bool }
```

É o seu tipo de dados algébrico comum que foi definido com a palavra-chave *data*.
Ele tem um construtor de valor, que tem um campo cujo tipo é `Bool`.
Vamos fazer uma função que faça pattern matching em um `CoolBool` e retorne o valor `"hello"`, independentemente de o `Bool` dentro do `CoolBool` ser `True` ou `False`:

```{.haskell:hs}
helloMe :: CoolBool -> String
helloMe (CoolBool _) = "hello"
```

Em vez de aplicar essa função a um `CoolBool` normal, vamos jogar uma bola curva e aplicá-la a `undefined`!

```{.haskell:hs}
ghci> helloMe undefined
"*** Exception: Prelude.undefined
```

Nossa!
Uma exceção!
Agora, por que essa exceção aconteceu?
Tipos definidos com a palavra-chave *data* podem ter vários construtores de valor (embora `CoolBool` tenha apenas um).
Portanto, para verificar se o valor atribuído à nossa função está em conformidade com o padrão `(CoolBool _)`, o Haskell deve avaliar o valor apenas o suficiente para ver qual construtor de valor foi usado quando fizemos o valor.
E quando tentamos avaliar um valor `undefined`, mesmo que um pouco, uma exceção é lançada.

Em vez de usar a palavra-chave *data* para `CoolBool`, vamos tentar usar *newtype*:

```{.haskell:hs}
newtype CoolBool = CoolBool { getCoolBool :: Bool }
```

Não precisamos alterar nossa função `helloMe`, porque a sintaxe de pattern matching é a mesma se você usa *newtype* ou *data* para definir seu tipo.
Vamos fazer a mesma coisa aqui e aplicar `helloMe` a um valor `undefined`:

```{.haskell:hs}
ghci> helloMe undefined
"hello"
```

![top of the mornin to ya!!!](assets/images/functors-applicative-functors-and-monoids/shamrock.png){.right width=184 height=230}

Funcionou!
Hmmm, por que isso?
Bem, como dissemos, quando usamos *newtype*, o Haskell pode representar internamente os valores do novo tipo da mesma maneira que os valores originais.
Ele não precisa adicionar outra caixa ao redor deles, apenas precisa estar ciente de que os valores são de tipos diferentes.
E como o Haskell sabe que os tipos feitos com a palavra-chave *newtype* só podem ter um construtor, ele não precisa avaliar o valor passado para a função para garantir que esteja em conformidade com o padrão `(CoolBool _)` porque tipos *newtype* só podem ter um construtor de valor possível e um campo!

Essa diferença no comportamento pode parecer trivial, mas na verdade é muito importante porque nos ajuda a perceber que, embora os tipos definidos com *data* e *newtype* se comportem da mesma forma do ponto de vista do programador, porque ambos têm construtores de valor e campos, eles são na verdade dois mecanismos diferentes.
Enquanto *data* pode ser usado para criar seus próprios tipos do zero, *newtype* é para criar um tipo completamente novo a partir de um tipo existente.
O pattern matching em valores *newtype* não é como tirar algo de uma caixa (como é com *data*), é mais sobre fazer uma conversão direta de um tipo para outro.

### `type` vs. `newtype` vs. `data` 

Nesse ponto, você pode estar um pouco confuso sobre qual exatamente é a diferença entre *type*, *data* e *newtype*, então vamos refrescar nossa memória um pouco.

A palavra-chave **type** é para criar sinônimos de tipo.
O que isso significa é que apenas damos outro nome a um tipo já existente para que seja mais fácil se referir a ele.
Digamos que fizemos o seguinte:

```{.haskell:hs}
type IntList = [Int]
```

Tudo o que isso faz é nos permitir referir ao tipo `[Int]` como `IntList`.
Eles podem ser usados ​​de forma intercambiável.
Não obtemos um construtor de valor `IntList` ou algo assim.
Como `[Int]` e `IntList` são apenas duas maneiras de se referir ao mesmo tipo, não importa qual nome usamos em nossas anotações de tipo:

```{.haskell:hs}
ghci> ([1,2,3] :: IntList) ++ ([1,2,3] :: [Int])
[1,2,3,1,2,3]
```

Usamos sinônimos de tipo quando queremos tornar nossas assinaturas de tipo mais descritivas, dando aos tipos nomes que nos dizem algo sobre seu objetivo no contexto das funções em que estão sendo usados.
Por exemplo, quando usamos uma lista de associação do tipo `[(String,String)]` para representar uma lista telefônica, damos a ela o sinônimo de tipo `PhoneBook` para que as assinaturas de tipo de nossas funções fossem mais fáceis de ler.

A palavra-chave **newtype** é para pegar tipos existentes e embrulhá-los em novos tipos, principalmente para que seja mais fácil torná-los instâncias de certas typeclasses.
Quando usamos *newtype* para embrulhar um tipo existente, o tipo que obtemos é separado do tipo original.
Se fizermos o seguinte *newtype*:

```{.haskell:hs}
newtype CharList = CharList { getCharList :: [Char] }
```

Não podemos usar `++` para juntar uma `CharList` e uma lista do tipo `[Char]`.
Não podemos nem usar `++` para juntar duas `CharList`s, porque `++` funciona apenas em listas e o tipo `CharList` não é uma lista, embora possa ser dito que contém uma.
Podemos, no entanto, converter duas `CharList`s em listas, usar `++` nelas e depois converter isso de volta para uma `CharList`.

Quando usamos sintaxe de registro em nossas declarações *newtype*, obtemos funções para converter entre o novo tipo e o tipo original: ou seja, o construtor de valor do nosso *newtype* e a função para extrair o valor em seu campo.
O novo tipo também não é feito automaticamente uma instância das typeclasses às quais o tipo original pertence, então temos que derivá-las ou escrevê-las manualmente.

Na prática, você pode pensar nas declarações *newtype* como declarações *data* que só podem ter um construtor e um campo.
Se você se pegar escrevendo tal declaração *data*, considere usar *newtype*.

A palavra-chave **data** é para fazer seus próprios tipos de dados e com eles você pode enlouquecer.
Eles podem ter quantos construtores e campos você desejar e podem ser usados para implementar qualquer tipo de dados algébrico por conta própria.
Tudo, desde listas e tipos semelhantes a `Maybe` até árvores.

Se você quer apenas que suas assinaturas de tipo pareçam mais limpas e sejam mais descritivas, provavelmente você quer sinônimos de tipo.
Se você quer pegar um tipo existente e embrulhá-lo em um novo tipo para torná-lo uma instância de uma typeclass, é provável que você esteja procurando por um *newtype*.
E se você quer fazer algo completamente novo, as chances são boas de que você esteja procurando pela palavra-chave *data*.

## Monoids {#monoids}

![wow this is pretty much the gayest pirate ship
ever](assets/images/functors-applicative-functors-and-monoids/pirateship.png){.right width=460 height=417}

Começamos com typeclasses simples como `Eq`, que é para tipos cujos valores podem ser igualados, e `Ord`, que é para coisas que podem ser colocadas em uma ordem e depois passamos para outras mais interessantes, como `Functor` e `Applicative`.

Quando criamos um tipo, pensamos sobre quais comportamentos ele suporta, ou seja, como ele pode agir e, com base nisso, decidimos de quais typeclasses torná-lo uma instância.
Se faz sentido que os valores de nosso tipo sejam igualados, nós o tornamos uma instância da typeclass `Eq`.
Se vemos que nosso tipo é algum tipo de Functor, nós o tornamos uma instância de `Functor`, e assim por diante.

Agora considere o seguinte: `*` é uma função que pega dois números e os multiplica.
Se multiplicarmos algum número por `1`, o resultado é sempre igual a esse número.
Não importa se fazemos `1 * x` ou `x * 1`, o resultado é sempre `x`.
Da mesma forma, `++` é também uma função que pega duas coisas e retorna uma terceira.
Só que em vez de multiplicar números, ela pega duas listas e as concatena.
E muito parecido com `*`, ela também tem um certo valor que não muda o outro quando usado com `++`.
Esse valor é a lista vazia: `[]`.

```{.haskell:hs}
ghci> 4 * 1
4
ghci> 1 * 9
9
ghci> [1,2,3] ++ []
[1,2,3]
ghci> [] ++ [0.5, 2.5]
[0.5,2.5]
```

Parece que ambos `*` junto com `1` e `++` junto com `[]` compartilham algumas propriedades comuns:

* A função pega dois parâmetros.
* Os parâmetros e o valor retornado têm o mesmo tipo.
* Existe tal valor que não muda outros valores quando usado com a função binária.

Há outra coisa que essas duas operações têm em comum que pode não ser tão óbvia quanto nossas observações anteriores: quando temos três ou mais valores e queremos usar a função binária para reduzi-los a um único resultado, a ordem em que aplicamos a função binária aos valores não importa.
Não importa se fazemos `(3 * 4) * 5` ou `3 * (4 * 5)`.
De qualquer maneira, o resultado é `60`.
O mesmo vale para `++`:

```{.haskell:hs}
ghci> (3 * 2) * (8 * 5)
240
ghci> 3 * (2 * (8 * 5))
240
ghci> "la" ++ ("di" ++ "da")
"ladida"
ghci> ("la" ++ "di") ++ "da"
"ladida"
```

Chamamos essa propriedade de *associatividade*.
`*` é associativo, e `++` também, mas `-`, por exemplo, não é.
As expressões `(5 - 3) - 4` e `5 - (3 - 4)` resultam em números diferentes.

Ao perceber e anotar essas propriedades, nós encontramos Monoids!
Um Monoid é quando você tem uma função binária associativa e um valor que age como uma identidade em relação a essa função.
Quando algo age como uma identidade em relação a uma função, significa que quando chamado com essa função e algum outro valor, o resultado é sempre igual a esse outro valor.
`1` é a identidade em relação a `*` e `[]` é a identidade em relação a `++`.
Existem muitos outros Monoids para serem encontrados no mundo de Haskell, e é por isso que a typeclass `Monoid` existe.
É para tipos que podem agir como Monoids.
Vamos ver como a typeclass é definida:

```{.haskell:hs}
class Monoid m where
    mempty :: m
    mappend :: m -> m -> m
    mconcat :: [m] -> m
    mconcat = foldr mappend mempty
```

![woof dee do!!!](assets/images/functors-applicative-functors-and-monoids/balloondog.png){.right width=260 height=326}

A typeclass `Monoid` é definida em `import Data.Monoid`.
Vamos levar algum tempo e nos familiarizar adequadamente com ela.

Primeiro de tudo, vemos que apenas tipos concretos podem ser feitos instâncias de `Monoid`, porque o `m` na definição da typeclass não pega nenhum parâmetro de tipo.
Isso é diferente de `Functor` e `Applicative`, que exigem que suas instâncias sejam construtores de tipo que pegam um parâmetro.

A primeira função é `mempty`.
Não é realmente uma função, já que não pega parâmetros, então é uma constante polimórfica, meio que como `minBound` de `Bounded`.
`mempty` representa o valor de identidade para um Monoid específico.

Em seguido, temos `mappend`, que, como você provavelmente adivinhou, é a função binária.
Ela pega dois valores do mesmo tipo e retorna um valor desse tipo também.
Vale a pena notar que a decisão de nomear `mappend` como é nomeado foi meio infeliz, porque implica que estamos anexando duas coisas de alguma forma.
Embora `++` realmente pegue duas listas e anexe uma à outra, `*` realmente não faz nenhum anexo, apenas multiplica dois números.
Quando encontrarmos outras instâncias de `Monoid`, veremos que a maioria delas também não anexa valores, então evite pensar em termos de anexar e apenas pense em termos de `mappend` ser uma função binária que pega dois valores de monóide e retorna um terceiro.

A última função nesta definição de typeclass é `mconcat`.
Ela pega uma lista de valores de monóide e os reduz a um único valor fazendo `mappend` entre os elementos da lista.
Ela tem uma implementação padrão, que apenas pega `mempty` como valor inicial e dobra (fold) a lista da direita com `mappend`.
Como a implementação padrão é boa para a maioria das instâncias, não nos preocuparemos muito com `mconcat` de agora em diante.
Ao tornar um tipo uma instância de `Monoid`, basta implementar `mempty` e `mappend`.
A razão pela qual `mconcat` está lá é porque para algumas instâncias, pode haver uma maneira mais eficiente de implementar `mconcat`, mas para a maioria das instâncias a implementação padrão é boa o suficiente.

Antes de passar para instâncias específicas de `Monoid`, vamos dar uma breve olhada nas leis dos Monoids.
Mencionamos que deve haver um valor que atue como identidade em relação à função binária e que a função binária deve ser associativa.
É possível fazer instâncias de `Monoid` que não seguem essas regras, mas tais instâncias não servem para ninguém, porque ao usar a typeclass `Monoid`, confiamos em suas instâncias agindo como Monoids.
Caso contrário, qual é o ponto?
É por isso que ao fazer instâncias, temos que ter certeza de que elas seguem essas leis:

* ``mempty `mappend` x = x``{.label .law}
* ``x `mappend` mempty = x``{.label .law}
* ``(x `mappend` y) `mappend` z = x `mappend` (y `mappend` z)``{.label .law}

As duas primeiras afirmam que `mempty` tem que agir como a identidade em relação a `mappend` e a terceira diz que `mappend` tem que ser associativa, ou seja, que a ordem em que usamos `mappend` para reduzir vários valores de Monoid em um não importa.
O Haskell não impõe essas leis, então nós, como programadores, temos que ter cuidado para que nossas instâncias de fato as obedeçam.

### Listas são Monoids 

Sim, listas são Monoids!
Como vimos, a função `++` e a lista vazia `[]` formam um Monoid.
A instância é muito simples:

```{.haskell:hs}
instance Monoid [a] where
    mempty = []
    mappend = (++)
```

Listas são uma instância da typeclass `Monoid` independentemente do tipo dos elementos que elas contêm.
Observe que escrevemos `instance Monoid [a]` e não `instance Monoid []`, porque `Monoid` requer um tipo concreto para uma instância.

Dando a isso um teste, não encontramos surpresas:

```{.haskell:hs}
ghci> [1,2,3] `mappend` [4,5,6]
[1,2,3,4,5,6]
ghci> ("one" `mappend` "two") `mappend` "tree"
"onetwotree"
ghci> "one" `mappend` ("two" `mappend` "tree")
"onetwotree"
ghci> "one" `mappend` "two" `mappend` "tree"
"onetwotree"
ghci> "pang" `mappend` mempty
"pang"
ghci> mconcat [[1,2],[3,6],[9]]
[1,2,3,6,9]
ghci> mempty :: [a]
[]
```

![smug as hell](assets/images/functors-applicative-functors-and-monoids/smug.png){.left width=157 height=144}

Observe que na última linha, tivemos que escrever uma anotação de tipo explícita, porque se apenas fizéssemos `mempty`, o GHCi não saberia qual instância usar, então tivemos que dizer que queríamos a instância da lista.
Fomos capazes de usar o tipo geral de `[a]` (em oposição a especificar `[Int]` ou `[String]`) porque a lista vazia pode agir como se contivesse qualquer tipo.

Como `mconcat` tem uma implementação padrão, nós a obtemos de graça quando tornamos algo uma instância de `Monoid`.
No caso da lista, `mconcat` acaba sendo apenas `concat`.
Ela pega uma lista de listas e a achata, porque esse é o equivalente a fazer `++` entre todas as listas adjacentes em uma lista.

As leis dos Monoids realmente se mantêm para a instância da lista.
Quando temos várias listas e as `mappend` (ou `++`) juntas, não importa quais fazemos primeiro, porque elas são apenas unidas nas extremidades de qualquer maneira.
Além disso, a lista vazia age como a identidade, então tudo está bem.
Observe que os Monoids não exigem que ``a `mappend` b`` seja igual a ``b `mappend` a``.
No caso da lista, eles claramente não são:

```{.haskell:hs}
ghci> "one" `mappend` "two"
"onetwo"
ghci> "two" `mappend` "one"
"twoone"
```

E tudo bem.
O fato de que para multiplicação `3 * 5` e `5 * 3` são os mesmos é apenas uma propriedade da multiplicação, mas não se mantém para todos (e de fato, a maioria) os Monoids.

### `Product` e `Sum` 

Já examinamos uma maneira de os números serem considerados Monoids.
Basta ter a função binária sendo `*` e o valor de identidade `1`.
Acontece que essa não é a única maneira de os números serem Monoids.
Outra maneira é ter a função binária sendo `+` e o valor de identidade `0`:

```{.haskell:hs}
ghci> 0 + 4
4
ghci> 5 + 0
5
ghci> (1 + 3) + 5
9
ghci> 1 + (3 + 5)
9
```

As leis dos Monoids se mantêm, porque se você adicionar 0 a qualquer número, o resultado é esse número.
E a adição também é associativa, então não temos problemas lá.
Então, agora que existem duas maneiras igualmente válidas para os números serem Monoids, qual maneira escolher?
Bem, não precisamos escolher.
Lembre-se, quando existem várias maneiras de um tipo ser uma instância da mesma typeclass, podemos embrulhar esse tipo em um *newtype* e então tornar o novo tipo uma instância da typeclass de uma maneira diferente.
Podemos ter nosso bolo e comê-lo também.

O módulo `Data.Monoid` exporta dois tipos para isso, a saber `Product` e `Sum`.
`Product` é definido assim:

```{.haskell:hs}
newtype Product a =  Product { getProduct :: a }
    deriving (Eq, Ord, Read, Show, Bounded)
```

Simples, apenas um embrulho *newtype* com um parâmetro de tipo juntamente com algumas instâncias derivadas.
Sua instância para `Monoid` vai mais ou menos assim:

```{.haskell:hs}
instance Num a => Monoid (Product a) where
    mempty = Product 1
    Product x `mappend` Product y = Product (x * y)
```

`mempty` é apenas `1` embrulhado em um construtor `Product`.
`mappend` faz pattern matching no construtor `Product`, multiplica os dois números e então embrulha o número resultante de volta.
Como você pode ver, há uma restrição de classe `Num a`.
Então isso significa que `Product a` é uma instância de `Monoid` para todos os `a`s que já são uma instância de `Num`.
Para usar `Product a` como um monóide, temos que fazer algum embrulho e desembrulho de *newtype*:

```{.haskell:hs}
ghci> getProduct $ Product 3 `mappend` Product 9
27
ghci> getProduct $ Product 3 `mappend` mempty
3
ghci> getProduct $ Product 3 `mappend` Product 4 `mappend` Product 2
24
ghci> getProduct . mconcat . map Product $ [3,4,2]
24
```

Isso é legal como uma vitrine da typeclass `Monoid`, mas ninguém em sã consciência usaria essa maneira de multiplicar números em vez de apenas escrever `3 * 9` e `3 * 1`.
Mas um pouco mais tarde, veremos como essas instâncias de `Monoid` que podem parecer triviais agora podem ser úteis.

`Sum` é definido como `Product` e a instância é semelhante também.
Nós o usamos da mesma maneira:

```{.haskell:hs}
ghci> getSum $ Sum 2 `mappend` Sum 9
11
ghci> getSum $ mempty `mappend` Sum 3
3
ghci> getSum . mconcat . map Sum $ [1,2,3]
6
```

### `Any` e `All` 

Outro tipo que pode agir como um Monoid de duas maneiras distintas, mas igualmente válidas, é `Bool`.
A primeira maneira é fazer a função *ou* `||` agir como a função binária juntamente com `False` como o valor de identidade.
A maneira como o *ou* funciona na lógica é que se qualquer um de seus dois parâmetros for `True`, ele retorna `True`, caso contrário, retorna `False`.
Então, se usarmos `False` como valor de identidade, ele retornará `False` quando fizermos *ou* com `False` e `True` quando fizermos *ou* com `True`.
O construtor *newtype* `Any` é uma instância de `Monoid` dessa maneira.
É definido assim:

```{.haskell:hs}
newtype Any = Any { getAny :: Bool }
    deriving (Eq, Ord, Read, Show, Bounded)
```

Sua instância parece ser assim:

```{.haskell:hs}
instance Monoid Any where
        mempty = Any False
        Any x `mappend` Any y = Any (x || y)
```

A razão pela qual é chamado `Any` é porque ``x `mappend` y`` será `True` se *qualquer* um desses dois for `True`.
Mesmo que três ou mais `Bool`s embrulhados em `Any` sejam `mappend` juntos, o resultado será `True` se qualquer um deles for `True`:

```{.haskell:hs}
ghci> getAny $ Any True `mappend` Any False
True
ghci> getAny $ mempty `mappend` Any True
True
ghci> getAny . mconcat . map Any $ [False, False, False, True]
True
ghci> getAny $ mempty `mappend` mempty
False
```

A outra maneira de `Bool` ser uma instância de `Monoid` é fazer meio que o oposto: ter `&&` sendo a função binária e então fazer `True` o valor de identidade.
O *e* lógico retornará `True` apenas se ambos os seus parâmetros forem `True`.
Esta é a declaração *newtype*, nada chique:

```{.haskell:hs}
newtype All = All { getAll :: Bool }
        deriving (Eq, Ord, Read, Show, Bounded)
```

E esta é a instância:

```{.haskell:hs}
instance Monoid All where
        mempty = All True
        All x `mappend` All y = All (x && y)
```

Quando fazemos `mappend` em valores do tipo `All`, o resultado será `True` apenas se *todos* os valores usados nas operações `mappend` forem `True`:

```{.haskell:hs}
ghci> getAll $ mempty `mappend` All True
True
ghci> getAll $ mempty `mappend` All False
False
ghci> getAll . mconcat . map All $ [True, True, True]
True
ghci> getAll . mconcat . map All $ [True, True, False]
False
```

Assim como com multiplicação e adição, geralmente indicamos explicitamente as funções binárias em vez de embrulhá-las em *newtype*s e depois usar `mappend` e `mempty`.
`mconcat` parece ser útil para `Any` e `All`, mas geralmente é mais fácil usar as funções `or` e `and`, que pegam listas de `Bool`s e retornam `True` se qualquer um deles for `True` ou se todos eles forem `True`, respectivamente.

### The Ordering Monoid 

Ei, lembra do tipo `Ordering`?
Ele é usado como o resultado ao comparar coisas e pode ter três valores: `LT`, `EQ` e `GT`, que significam *less than* (menor que), *equal* (igual) e *greater than* (maior que), respectivamente:

```{.haskell:hs}
ghci> 1 `compare` 2
LT
ghci> 2 `compare` 2
EQ
ghci> 3 `compare` 2
GT
```

Com listas, números e valores booleanos, encontrar Monoids foi apenas uma questão de olhar para funções comumente usadas já existentes e ver se elas exibem algum tipo de comportamento de Monoid.
Com `Ordering`, temos que olhar um pouco mais para reconhecer um Monoid, mas acontece que sua instância `Monoid` é tão intuitiva quanto as que encontramos até agora e também bastante útil:

```{.haskell:hs}
instance Monoid Ordering where
    mempty = EQ
    LT `mappend` _ = LT
    EQ `mappend` y = y
    GT `mappend` _ = GT
```

![did anyone ORDER pizza?!?! I can't BEAR these puns!](assets/images/functors-applicative-functors-and-monoids/bear.png){.right width=330 height=339}

A instância é configurada assim: quando fazemos `mappend` em dois valores `Ordering`, o da esquerda é mantido, a menos que o valor da esquerda seja `EQ`, caso em que o da direita é o resultado.
A identidade é `EQ`.
A princípio, isso pode parecer meio arbitrário, mas na verdade se assemelha à maneira como comparamos alfabeticamente palavras.
Comparamos as duas primeiras letras e, se forem diferentes, já podemos decidir qual palavra iria primeiro em um dicionário.
No entanto, se as duas primeiras letras forem iguais, passamos a comparar o próximo par de letras e repetimos o processo.

Por exemplo, se fôssemos comparar alfabeticamente as palavras `"ox"` e `"on"`, primeiro compararíamos as primeiras duas letras de cada palavra, veríamos que são iguais e então passaríamos a comparar a segunda letra de cada palavra.
Vemos que `'x'` é alfabeticamente maior que `'n'`, e assim sabemos como as palavras se comparam.
Para ganhar alguma intuição para `EQ` sendo a identidade, podemos notar que se fôssemos enfiar a mesma letra na mesma posição em ambas as palavras, isso não mudaria sua ordem alfabética.
`"oix"` ainda é alfabeticamente maior que `"oin"`.

É importante notar que na instância `Monoid` para `Ordering`, ``x `mappend` y`` não é igual a ``y `mappend` x``.
Como o primeiro parâmetro é mantido a menos que seja `EQ`, ``LT `mappend` GT`` resultará em `LT`, enquanto ``GT `mappend` LT`` resultará em `GT`:

```{.haskell:hs}
ghci> LT `mappend` GT
LT
ghci> GT `mappend` LT
GT
ghci> mempty `mappend` LT
LT
ghci> mempty `mappend` GT
GT
```

OK, então como esse monóide é útil?
Digamos que você estivesse escrevendo uma função que pega duas strings, compara seus comprimentos e retorna um `Ordering`.
Mas se as strings forem do mesmo comprimento, então em vez de retornar `EQ` imediatamente, queremos compará-las alfabeticamente.
Uma maneira de escrever isso seria assim:

```{.haskell:hs}
lengthCompare :: String -> String -> Ordering
lengthCompare x y = let a = length x `compare` length y
                        b = x `compare` y
                    in  if a == EQ then b else a
```

Nomeamos o resultado da comparação dos comprimentos de `a` e o resultado da comparação alfabética de `b` e então se acontecer de os comprimentos serem iguais, retornamos sua ordem alfabética.

Mas ao empregar nosso entendimento de como `Ordering` é um Monoid, podemos reescrever essa função de uma maneira muito mais simples:

```{.haskell:hs}
import Data.Monoid

lengthCompare :: String -> String -> Ordering
lengthCompare x y = (length x `compare` length y) `mappend`
                    (x `compare` y)
```

Podemos experimentar isso:

```{.haskell:hs}
ghci> lengthCompare "zen" "ants"
LT
ghci> lengthCompare "zen" "ant"
GT
```

Lembre-se, quando usamos `mappend`, seu parâmetro esquerdo é sempre mantido a menos que seja `EQ`, caso em que o direito é mantido.
É por isso que colocamos a comparação que consideramos ser o primeiro, mais importante critério como o primeiro parâmetro.
Se quiséssemos expandir essa função para também comparar pelo número de vogais e definir isso como o segundo critério mais importante para comparação, apenas a modificaríamos assim:

```{.haskell:hs}
import Data.Monoid

lengthCompare :: String -> String -> Ordering
lengthCompare x y = (length x `compare` length y) `mappend`
                    (vowels x `compare` vowels y) `mappend`
                    (x `compare` y)
    where vowels = length . filter (`elem` "aeiou")
```

Fizemos uma função auxiliar, que pega uma string e nos diz quantas vogais ela tem, primeiro filtrando-a apenas para letras que estão na string `"aeiou"` e depois aplicando `length` a isso.

```{.haskell:hs}
ghci> lengthCompare "zen" "anna"
LT
ghci> lengthCompare "zen" "ana"
LT
ghci> lengthCompare "zen" "ann"
GT
```

Muito legal.
Aqui, vemos como no primeiro exemplo os comprimentos são considerados diferentes e então `LT` é retornado, porque o comprimento de `"zen"` é menor que o comprimento de `"anna"`.
No segundo exemplo, os comprimentos são os mesmos, mas a segunda string tem mais vogais, então `LT` é retornado novamente.
No terceiro exemplo, ambas têm o mesmo comprimento e o mesmo número de vogais, então são comparadas alfabeticamente e `"zen"` ganha.

### The Maybe Monoid 

Vamos dar uma olhada nas várias maneiras que `Maybe a` pode ser feito uma instância de `Monoid` e para que essas instâncias são úteis.

Uma maneira é tratar `Maybe a` como um Monoid apenas se seu parâmetro de tipo `a` for um Monoid também e então implementar `mappend` de tal maneira que use a operação `mappend` dos valores que estão embrulhados com `Just`.
Usamos `Nothing` como a identidade, e então se um dos dois valores que estamos fazendo `mappend` for `Nothing`, mantemos o outro valor.
Aqui está a declaração da instância:

```{.haskell:hs}
instance Monoid a => Monoid (Maybe a) where
    mempty = Nothing
    Nothing `mappend` m = m
    m `mappend` Nothing = m
    Just m1 `mappend` Just m2 = Just (m1 `mappend` m2)
```

Observe a restrição de classe.
Ela diz que `Maybe a` é uma instância de `Monoid` apenas se `a` for uma instância de `Monoid`.
Se fizermos `mappend` de algo com um `Nothing`, o resultado é esse algo.
Se fizermos `mappend` de dois valores `Just`, o conteúdo dos `Just`s é `mappended` e então embrulhado de volta em um `Just`.
Podemos fazer isso porque a restrição de classe garante que o tipo do que está dentro do `Just` é uma instância de `Monoid`.

```{.haskell:hs}
ghci> Nothing `mappend` Just "andy"
Just "andy"
ghci> Just LT `mappend` Nothing
Just LT
ghci> Just (Sum 3) `mappend` Just (Sum 4)
Just (Sum {getSum = 7})
```

Isso é útil quando você está lidando com Monoids como resultados de cálculos que podem ter falhado.
Por causa dessa instância, não temos que verificar se os cálculos falharam vendo se são um valor `Nothing` ou `Just`; podemos apenas continuar a tratá-los como Monoids normais.

Mas e se o tipo do conteúdo de `Maybe` não for uma instância de `Monoid`?
Observe que na declaração de instância anterior, o único caso em que temos que confiar que o conteúdo são Monoids é quando ambos os parâmetros de `mappend` são valores `Just`.
Mas se não sabemos se o conteúdo são Monoids, não podemos usar `mappend` entre eles, então o que devemos fazer?
Bem, uma coisa que podemos fazer é apenas descartar o segundo valor e manter o primeiro.
Para isso, o tipo `First a` existe e esta é a sua definição:

```{.haskell:hs}
newtype First a = First { getFirst :: Maybe a }
    deriving (Eq, Ord, Read, Show)
```

Pegamos um `Maybe a` e o embrulhamos com um *newtype*.
A instância `Monoid` é a seguinte:

```{.haskell:hs}
instance Monoid (First a) where
    mempty = First Nothing
    First (Just x) `mappend` _ = First (Just x)
    First Nothing `mappend` x = x
```

Assim como dissemos.
`mempty` é apenas um `Nothing` embrulhado com o construtor *newtype* `First`.
Se o primeiro parâmetro de `mappend` for um valor `Just`, ignoramos o segundo.
Se o primeiro for um `Nothing`, então apresentamos o segundo parâmetro como resultado, independentemente de ser um `Just` ou um `Nothing`:

```{.haskell:hs}
ghci> getFirst $ First (Just 'a') `mappend` First (Just 'b')
Just 'a'
ghci> getFirst $ First Nothing `mappend` First (Just 'b')
Just 'b'
ghci> getFirst $ First (Just 'a') `mappend` First Nothing
Just 'a'
```

`First` é útil quando temos um monte de valores `Maybe` e queremos apenas saber se algum deles é um `Just`.
A função `mconcat` vem a calhar:

```{.haskell:hs}
ghci> getFirst . mconcat . map First $ [Nothing, Just 9, Just 10]
Just 9
```

Se quisermos um Monoid sobre `Maybe a` de tal forma que o segundo parâmetro seja mantido se ambos os parâmetros de `mappend` forem valores `Just`, `Data.Monoid` fornece um tipo `Last a`, que funciona como `First a`, só que o último valor não-`Nothing` é mantido ao fazer `mappend` e usar `mconcat`:

```{.haskell:hs}
ghci> getLast . mconcat . map Last $ [Nothing, Just 9, Just 10]
Just 10
ghci> getLast $ Last (Just "one") `mappend` Last (Just "two")
Just "two"
```

### Usando Monoids para dobrar estruturas de dados 

Uma das maneiras mais interessantes de colocar Monoids para trabalhar é fazê-los nos ajudar a definir dobras (folds) sobre várias estruturas de dados.
Até agora, fizemos apenas dobras sobre listas, mas listas não são a única estrutura de dados que pode ser dobrada.
Podemos definir dobras sobre quase qualquer estrutura de dados.
Árvores especialmente se prestam bem a dobras.

Como existem tantas estruturas de dados que funcionam bem com dobras, a typeclass `Foldable`{.label .class} foi introduzida.
Muito parecido com `Functor` é para coisas que podem ser mapeadas, `Foldable` é para coisas que podem ser dobradas!
Ela pode ser encontrada em `Data.Foldable` e porque exporta funções cujos nomes entram em conflito com os do `Prelude`, é melhor importada qualificada (e servida com manjericão):

```{.haskell:hs}
import qualified Foldable as F
```

Para economizar preciosos toques de tecla, escolhemos importá-la qualificada como `F`.
Certo, então quais são algumas das funções que essa typeclass define?
Bem, entre elas estão `foldr`, `foldl`, `foldr1` e `foldl1`.
Hã?
Mas já conhecemos essas funções, o que há de tão novo nisso?
Vamos comparar os tipos de `foldr` de `Foldable` e o `foldr` do `Prelude` para ver como eles diferem:

```{.haskell:hs}
ghci> :t foldr
foldr :: (a -> b -> b) -> b -> [a] -> b
ghci> :t F.foldr
F.foldr :: (F.Foldable t) => (a -> b -> b) -> b -> t a -> b
```

Ah!
Então, enquanto `foldr` pega uma lista e a dobra, o `foldr` de `Data.Foldable` aceita qualquer tipo que possa ser dobrado, não apenas listas!
Como esperado, ambas as funções `foldr` fazem o mesmo para listas:

```{.haskell:hs}
ghci> foldr (*) 1 [1,2,3]
6
ghci> F.foldr (*) 1 [1,2,3]
6
```

Ok então, quais são algumas outras estruturas de dados que suportam dobras?
Bem, tem o `Maybe` que todos conhecemos e amamos!

```{.haskell:hs}
ghci> F.foldl (+) 2 (Just 9)
11
ghci> F.foldr (||) False (Just True)
True
```

Mas dobrar sobre um valor `Maybe` não é terrivelmente interessante, porque quando se trata de dobrar, ele apenas age como uma lista com um elemento se for um valor `Just` e como uma lista vazia se for `Nothing`.
Então, vamos examinar uma estrutura de dados um pouco mais complexa então.

Lembre-se da estrutura de dados de árvore do capítulo [Making Our Own Types and Typeclasses](making-our-own-types-and-typeclasses.html#recursive-data-structures)?
Nós a definimos assim:

```{.haskell:hs}
data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)
```

Dissemos que uma árvore é ou uma árvore vazia que não contém nenhum valor ou é um nó que contém um valor e também duas outras árvores.
Depois de defini-la, nós a tornamos uma instância de `Functor` e com isso ganhamos a habilidade de `fmap` funções sobre ela.
Agora, vamos torná-la uma instância de `Foldable` para que tenhamos a habilidade de dobrá-la.
Uma maneira de tornar um construtor de tipo uma instância de `Foldable` é apenas implementar diretamente `foldr` para ele.
Mas outra maneira, muitas vezes muito mais fácil, é implementar a função `foldMap`, que também faz parte da typeclass `Foldable`.
A função `foldMap` tem o seguinte tipo:

```{.haskell:hs}
foldMap :: (Monoid m, Foldable t) => (a -> m) -> t a -> m
```

Seu primeiro parâmetro é uma função que pega um valor do tipo que nossa estrutura dobrável contém (denotado aqui com `a`) e retorna um valor de Monoid.
Seu segundo parâmetro é uma estrutura dobrável que contém valores do tipo `a`.
Ela mapeia essa função sobre a estrutura dobrável, produzindo assim uma estrutura dobrável que contém valores de Monoid.
Então, fazendo `mappend` entre esses valores de Monoid, ela os une todos em um único valor de Monoid.
Essa função pode soar meio estranha no momento, mas veremos que é muito fácil de implementar.
O que também é legal é que implementar essa função é tudo o que é preciso para o nosso tipo ser feito uma instância de `Foldable`.
Então, se apenas implementarmos `foldMap` para algum tipo, obtemos `foldr` e `foldl` nesse tipo de graça!

É assim que fazemos `Tree` uma instância de `Foldable`:

```{.haskell:hs}
instance F.Foldable Tree where
    foldMap f Empty = mempty
    foldMap f (Node x l r) = F.foldMap f l `mappend`
                             f x           `mappend`
                             F.foldMap f r
```

![find the visual pun or whatever](assets/images/functors-applicative-functors-and-monoids/accordion.png){.right width=366 height=280}

Pensamos assim: se nos for fornecida uma função que pega um elemento de nossa árvore e retorna um valor de Monoid, como reduzimos nossa árvore inteira a um único valor de Monoid?
Quando estávamos fazendo `fmap` sobre nossa árvore, aplicávamos a função que estávamos mapeando a um nó e então mapeávamos recursivamente a função sobre a subárvore esquerda, bem como a direita.
Aqui, somos encarregados de não apenas mapear uma função, mas também de unir os resultados em um único valor de Monoid usando `mappend`.
Primeiro consideramos o caso da árvore vazia --- uma árvore triste e solitária que não tem valores ou subárvores.
Ela não contém nenhum valor que possamos dar à nossa função criadora de Monoids, então apenas dizemos que se nossa árvore estiver vazia, o valor de Monoid em que ela se torna é `mempty`.

O caso de um nó não vazio é um pouco mais interessante.
Ele contém duas subárvores, bem como um valor.
Neste caso, recursivamente `foldMap` a mesma função `f` sobre as subárvores esquerda e direita.
Lembre-se, nosso `foldMap` resulta em um único valor de Monoid.
Também aplicamos nossa função `f` ao valor no nó.
Agora temos três valores de Monoid (dois de nossas subárvores e um da aplicação de `f` ao valor no nó) e apenas temos que juntá-los em um único valor.
Para esse propósito, usamos `mappend`, e naturalmente a subárvore esquerda vem primeiro, depois o valor do nó e então a subárvore direita.

Observe que não tivemos que fornecer a função que pega um valor e retorna um valor de Monoid.
Recebemos essa função como parâmetro para `foldMap` e tudo o que temos a decidir é onde aplicar essa função e como unir os Monoids resultantes dela.

Agora que temos uma instância `Foldable` para nosso tipo de árvore, obtemos `foldr` e `foldl` de graça!
Considere esta árvore:

```{.haskell:hs}
testTree = Node 5
            (Node 3
                (Node 1 Empty Empty)
                (Node 6 Empty Empty)
            )
            (Node 9
                (Node 8 Empty Empty)
                (Node 10 Empty Empty)
            )
```

Ela tem `5` em sua raiz e então seu nó esquerdo tem `3` com `1` à esquerda e `6` à direita.
O nó direito da raiz tem um `9` e então um `8` à sua esquerda e um `10` no lado direito distante.
Com uma instância `Foldable`, podemos fazer todas as dobras que podemos fazer em listas:

```{.haskell:hs}
ghci> F.foldl (+) 0 testTree
42
ghci> F.foldl (*) 1 testTree
64800
```

E também, `foldMap` não é útil apenas para fazer novas instâncias de `Foldable`; ele vem a calhar para reduzir nossa estrutura a um único valor de Monoid.
Por exemplo, se quisermos saber se algum número em nossa árvore é igual a `3`, podemos fazer isso:

```{.haskell:hs}
ghci> getAny $ F.foldMap (\x -> Any $ x == 3) testTree
True
```

Aqui, `\x -> Any $ x == 3` é uma função que pega um número e retorna um valor de Monoid, ou seja, um `Bool` embrulhado em `Any`.
`foldMap` aplica essa função a cada elemento em nossa árvore e então reduz os Monoids resultantes em um único Monoid com `mappend`.
Se fizermos isso:

```{.haskell:hs}
ghci> getAny $ F.foldMap (\x -> Any $ x > 15) testTree
False
```

Todos os nós em nossa árvore conteriam o valor `Any False` depois de ter a função lambda aplicada a eles.
Mas para acabar `True`, `mappend` para `Any` tem que ter pelo menos um valor `True` como parâmetro.
É por isso que o resultado final é `False`, o que faz sentido porque nenhum valor em nossa árvore é maior que `15`.

Também podemos transformar facilmente nossa árvore em uma lista fazendo um `foldMap` com a função `\x -> [x]`.
Ao primeiro projetar essa função em nossa árvore, cada elemento se torna uma lista singleton.
A ação `mappend` que ocorre entre todos esses resultados de lista singleton resulta em uma única lista que contém todos os elementos que estão em nossa árvore:

```{.haskell:hs}
ghci> F.foldMap (\x -> [x]) testTree
[1,3,6,5,8,9,10]
```

O que é legal é que todos esses truques não se limitam a árvores, eles funcionam em qualquer instância de `Foldable`.




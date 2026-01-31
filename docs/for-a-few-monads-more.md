# Por Mais Algumas Mônadas

![existem dois tipos de pessoas no mundo, meu amigo. aquelas que aprendem haskell e aquelas que têm o trabalho de programar em java](assets/images/for-a-few-monads-more/clint.png){.right width=189 height=400}

Vimos como as mônadas podem ser usadas para pegar valores com contextos e aplicá-los a funções, e como o uso de `>>=` ou da notação `do` nos permite focar nos próprios valores enquanto o contexto é tratado para nós.

Conhecemos a mônada `Maybe` e vimos como ela adiciona um contexto de possível falha aos valores. Aprendemos sobre a mônada de lista e vimos como ela nos permite introduzir facilmente o não-determinismo em nossos programas. Também aprendemos como trabalhar na mônada `IO`, antes mesmo de sabermos o que era uma mônada!

Neste capítulo, aprenderemos sobre algumas outras mônadas. Veremos como elas podem tornar nossos programas mais claros, permitindo-nos tratar todos os tipos de valores como valores monádicos. Explorar mais algumas mônadas também solidificará nossa intuição sobre elas.

As mônadas que exploraremos fazem parte do pacote `mtl`. Um pacote Haskell é uma coleção de módulos. O pacote `mtl` vem com a Haskell Platform, então você provavelmente já o tem. Para verificar se você o tem, digite `ghc-pkg list` na linha de comando. Isso mostrará quais pacotes Haskell você tem instalados e um deles deve ser `mtl`, seguido por um número de versão.

## Writer? Quase nem a conheço! {#writer}

Carregamos nossa arma com a mônada `Maybe`, a mônada de lista e a mônada `IO`. Agora vamos colocar a mônada `Writer` na câmara e ver o que acontece quando disparamos!

Enquanto `Maybe` é para valores com um contexto adicionado de falha e a lista é para valores não-determinísticos, a mônada `Writer` é para valores que possuem outro valor anexado que atua como uma espécie de valor de log (registro). O `Writer` nos permite fazer computações enquanto garante que todos os valores de log sejam combinados em um único valor de log que é então anexado ao resultado.

Por exemplo, podemos querer equipar nossos valores com strings que expliquem o que está acontecendo, provavelmente para fins de depuração. Considere uma função que recebe um número de bandidos em uma gangue e nos diz se essa é uma gangue grande ou não. Essa é uma função muito simples:

```{.haskell:hs}
isBigGang :: Int -> Bool
isBigGang x = x > 9
```

Agora, e se em vez de apenas nos dar um valor `True` ou `False`, quisermos que ela também retorne uma string de log dizendo o que fez? Bem, nós apenas criamos essa string e a retornamos junto com o nosso `Bool`:

```{.haskell:hs}
isBigGang :: Int -> (Bool, String)
isBigGang x = (x > 9, "Comparei o tamanho da gangue com 9.")
```

Então agora, em vez de apenas retornar um `Bool`, retornamos uma tupla onde o primeiro componente da tupla é o valor real e o segundo componente é a string que acompanha esse valor. Há algum contexto adicionado ao nosso valor agora. Vamos testar isso:

```{.haskell:hs}
ghci> isBigGang 3
(False,"Comparei o tamanho da gangue com 9.")
ghci> isBigGang 30
(True,"Comparei o tamanho da gangue com 9.")
```

![quando tiver que fazer cocô, faça, não fale](assets/images/for-a-few-monads-more/tuco.png){.left width=196 height=280}

Até aqui tudo bem. `isBigGang` recebe um valor normal e retorna um valor com um contexto. Como acabamos de ver, alimentá-la com um valor normal não é um problema. Agora, e se já tivermos um valor que tenha uma string de log anexada a ele, como `(3, "Gangue pequena.")`, e quisermos alimentá-lo para `isBigGang`? Parece que, mais uma vez, nos deparamos com esta questão: se temos uma função que recebe um valor normal e retorna um valor com um contexto, como pegamos um valor com um contexto e o alimentamos para a função?

Quando estávamos explorando a mônada `Maybe`, criamos uma função `applyMaybe`, que recebia um valor `Maybe a` e uma função do tipo `a -> Maybe b` e alimentava esse valor `Maybe a` na função, embora a função receba um `a` normal em vez de um `Maybe a`. Ela fazia isso cuidando do contexto que vem com os valores `Maybe a`, que é o fato de serem valores com falha possível. Mas dentro da função `a -> Maybe b`, fomos capazes de tratar esse valor como apenas um valor normal, porque `applyMaybe` (que mais tarde se tornou `>>=`) cuidou de verificar se era um valor `Nothing` or `Just`.

Na mesma linha, vamos criar uma função que receba um valor com um log anexado, ou seja, um valor `(a, String)` e uma função do tipo `a -> (b, String)` e alimente esse valor na função. Chamaremos de `applyLog`. Mas como um valor `(a, String)` não carrega consigo um contexto de falha possível, mas sim um contexto de um valor de log adicional, `applyLog` vai garantir que o log do valor original não seja perdido, mas sim unido ao log do valor que resulta da função. Aqui está a implementação de `applyLog`:

```{.haskell:hs}
applyLog :: (a,String) -> (a -> (b,String)) -> (b,String)
applyLog (x,log) f = let (y,newLog) = f x in (y,log ++ newLog)
```

Quando temos um valor com um contexto e queremos alimentá-lo para uma função, geralmente tentamos separar o valor real do contexto e então tentamos aplicar a função ao valor para depois ver se o contexto é cuidado. Na mônada `Maybe`, verificamos se o valor era um `Just x` e, se fosse, pegávamos esse `x` e aplicávamos a função a ele. Neste caso, é muito fácil encontrar o valor real, porque estamos lidando com um par onde um componente é o valor e o outro um log. Então, primeiro pegamos apenas o valor, que é `x`, e aplicamos a função `f` a ele. Obtemos um par de `(y, newLog)`, onde `y` é o novo resultado e `newLog` o novo log. Mas se retornássemos isso como resultado, o valor do log antigo não seria incluído no resultado, então retornamos um par de `(y, log ++ newLog)`. Usamos `++` para anexar o novo log ao antigo.

Aqui está o `applyLog` em ação:

```{.haskell:hs}
ghci> (3, "Gangue pequena.") `applyLog` isBigGang
(False,"Gangue pequena.Comparei o tamanho da gangue com 9.")
ghci> (30, "Um pelotão assustador.") `applyLog` isBigGang
(True,"Um pelotão assustador.Comparei o tamanho da gangue com 9.")
```

Os resultados são semelhantes aos de antes, só que agora o número de pessoas na gangue tinha seu log acompanhante e ele foi incluído no log de resultado. Aqui estão mais alguns exemplos de uso do `applyLog`:

```{.haskell:hs}
ghci> ("Tobin","Recebeu nome de fora da lei.") `applyLog` (\x -> (length x, "Aplicou length."))
(5,"Recebeu nome de fora da lei.Aplicou length.")
ghci> ("Bathcat","Recebeu nome de fora da lei.") `applyLog` (\x -> (length x, "Aplicou length"))
(7,"Recebeu nome de fora da lei.Aplicou length")
```

Veja como dentro do lambda, `x` é apenas uma string normal e não uma tupla, e como o `applyLog` cuida de anexar os logs.

### Monóides ao resgate

::: {.hintbox}
Certifique-se de saber o que são [monóides](functors-applicative-functors-and-monoids.html#monoids) a esta altura! Saudações.
:::

Até agora, `applyLog` recebe valores do tipo `(a, String)`, mas existe alguma razão para o log ter que ser uma `String`? Ele usa `++` para anexar os logs, então isso não funcionaria para qualquer tipo de lista, e não apenas uma lista de caracteres? Com certeza funcionaria. Podemos mudar o seu tipo para isto:

```{.haskell:hs}
applyLog :: (a,[c]) -> (a -> (b,[c])) -> (b,[c])
```

Agora, o log é uma lista. O tipo de valores contidos na lista tem que ser o mesmo para a lista original, bem como para a lista que a função retorna, caso contrário não seríamos capazes de usar `++` para juntá-los.

Isso funcionaria para bytestrings? Não há razão para não funcionar. No entanto, o tipo que temos agora funciona apenas para listas. Parece que teríamos que fazer um `applyLog` separado para bytestrings. Mas espere! Tanto listas quanto bytestrings são monóides. Como tal, ambos são instâncias da typeclass `Monoid`, o que significa que implementam a função `mappend`. E tanto para listas quanto para bytestrings, `mappend` serve para concatenar. Veja:

```{.haskell:hs}
ghci> [1,2,3] `mappend` [4,5,6]
[1,2,3,4,5,6]
ghci> B.pack [99,104,105] `mappend` B.pack [104,117,97,104,117,97]
Chunk "chi" (Chunk "huahua" Empty)
```

Legal! Agora nosso `applyLog` pode funcionar para qualquer monóide. Temos que mudar o tipo para refletir isso, assim como a implementação, porque temos que mudar `++` para `mappend`:

```{.haskell:hs}
applyLog :: (Monoid m) => (a,m) -> (a -> (b,m)) -> (b,m)
applyLog (x,log) f = let (y,newLog) = f x in (y,log `mappend` newLog)
```

Como o valor acompanhante pode agora ser qualquer valor monóide, não precisamos mais pensar na tupla como um valor e um log, mas agora podemos pensar nela como um valor com um valor monóide acompanhante. Por exemplo, podemos ter uma tupla que tenha um nome de item e um preço de item como o valor monóide. Apenas usamos o `newtype` `Sum` para garantir que os preços sejam somados à medida que operamos com os itens. Aqui está uma função que adiciona bebida a alguma comida de cowboy:

```{.haskell:hs}
import Data.Monoid

type Comida = String
type Preco = Sum Int

addDrink :: Comida -> (Comida,Preco)
addDrink "feijao" = ("leite", Sum 25)
addDrink "carne-seca" = ("uisque", Sum 99)
addDrink _ = ("cerveja", Sum 30)
```

Usamos strings para representar as comidas e um `Int` em um wrapper `newtype` `Sum` para acompanhar quantos centavos algo custa. Apenas um lembrete, fazer `mappend` com `Sum` resulta na soma dos valores envolvidos:

```{.haskell:hs}
ghci> Sum 3 `mappend` Sum 9
Sum {getSum = 12}
```

A função `addDrink` é bem simples. Se estivermos comendo feijão, ela retorna `"leite"` junto com `Sum 25`, ou seja, 25 centavos envolvidos em `Sum`. Se estivermos comendo carne seca, bebemos uísque e se estivermos comendo qualquer outra coisa, bebemos cerveja. Simplesmente aplicar esta função a uma comida não seria terrivelmente interessante agora, mas usar `applyLog` para alimentar uma comida que vem com um preço próprio nesta função é interessante:

```{.haskell:hs}
ghci> ("feijao", Sum 10) `applyLog` addDrink
("leite",Sum {getSum = 35})
ghci> ("carne-seca", Sum 25) `applyLog` addDrink
("uisque",Sum {getSum = 124})
ghci> ("carne-de-cachorro", Sum 5) `applyLog` addDrink
("cerveja",Sum {getSum = 35})
```

O leite custa 25 centavos, mas se o tomarmos com feijão que custa 10 centavos, acabaremos pagando 35 centavos. Agora está claro como o valor anexado nem sempre tem que ser um log, pode ser qualquer valor monóide e como dois desses valores são combinados em um depende do monóide. Quando estávamos fazendo logs, eles eram concatenados, mas agora, os números estão sendo somados.

Como o valor que `addDrink` retorna é uma tupla do tipo `(Comida,Preco)`, podemos alimentar esse resultado para `addDrink` novamente, para que ele nos diga o que devemos beber junto com nossa bebida e quanto isso nos custará. Vamos tentar:

```{.haskell:hs}
ghci> ("carne-de-cachorro", Sum 5) `applyLog` addDrink `applyLog` addDrink
("cerveja",Sum {getSum = 65})
```

Adicionar uma bebida a alguma carne de cachorro resulta em uma cerveja e 30 centavos adicionais, então `("cerveja", Sum 35)`. E se usarmos `applyLog` para alimentar isso para `addDrink`, pegamos outra cerveja e o resultado é `("cerveja", Sum 65)`.

### O tipo Writer

Agora que vimos que um valor com um monóide anexado age como um valor monádico, vamos examinar a instância `Monad` para tipos de tais valores. O módulo `Control.Monad.Writer` exporta o tipo `Writer w a` junto com sua instância `Monad` e algumas funções úteis para lidar com valores deste tipo.

Primeiro, vamos examinar o próprio tipo. Para anexar um monóide a um valor, basta colocá-los juntos em uma tupla. O tipo `Writer w a` é apenas um wrapper `newtype` para isso. Sua definição é muito simples:

```{.haskell:hs}
newtype Writer w a = Writer { runWriter :: (a, w) }
```

Ele é envolvido em um `newtype` para que possa ser feito uma instância de `Monad` e para que seu tipo seja separado de uma tupla normal. O parâmetro de tipo `a` representa o tipo do valor e o parâmetro de tipo `w` o tipo do valor monóide anexado.

Sua instância `Monad` é definida desta forma:

```{.haskell:hs}
instance (Monoid w) => Monad (Writer w) where
    return x = Writer (x, mempty)
    (Writer (x,v)) >>= f = let (Writer (y, v')) = f x in Writer (y, v `mappend` v')
```

![quando tiver que fazer cocô, faça, não fale](assets/images/for-a-few-monads-more/angeleyes.png){.right width=383 height=248}

Primeiro de tudo, vamos examinar o `>>=`. Sua implementação é essencialmente a mesma que a do `applyLog`, só que agora que nossa tupla está envolvida no `newtype` `Writer`, temos que desempacotá-la ao fazer o pattern matching. Pegamos o valor `x` e aplicamos a função `f` a ele. Isso nos dá um valor `Writer w a` e usamos uma expressão `let` para fazer o pattern matching sobre ele. Apresentamos `y` como o novo resultado e usamos `mappend` para combinar o valor monóide antigo com o novo. Empacotamos isso com o valor resultante em uma tupla e então envolvemos com o construtor `Writer` para que nosso resultado seja um valor `Writer` em vez de apenas uma tupla desempacotada.

Então, e quanto ao `return`? Ele tem que receber um valor e colocá-lo em um contexto mínimo padrão que ainda apresente esse valor como resultado. Então, qual seria esse contexto para os valores `Writer`? Se quisermos que o valor monóide acompanhante afete outros valores monóides o mínimo possível, faz sentido usar `mempty`. `mempty` é usado para apresentar valores de identidade monóide, como `""`, `Sum 0` e bytestrings vazias. Sempre que usamos `mappend` entre `mempty` e algum outro valor monóide, o resultado é esse outro valor monóide. Portanto, se usarmos `return` para criar um valor `Writer` e usarmos `>>=` para alimentar esse valor em uma função, o valor monóide resultante será apenas o que a função retornar. Vamos usar `return` no número `3` algumas vezes, só que o associaremos a um monóide diferente a cada vez:

```{.haskell:hs}
ghci> runWriter (return 3 :: Writer String Int)
(3,"")
ghci> runWriter (return 3 :: Writer (Sum Int) Int)
(3,Sum {getSum = 0})
ghci> runWriter (return 3 :: Writer (Product Int) Int)
(3,Product {getProduct = 1})
```

Como o `Writer` não possui uma instância `Show`, tivemos que usar `runWriter` para converter nossos valores `Writer` em tuplas normais que podem ser exibidas. Para `String`, o valor monóide é a string vazia. Com `Sum`, é `0`, porque se somarmos 0 a algo, esse algo permanece o mesmo. Para `Product`, a identidade é `1`.

A instância `Writer` não apresenta uma implementação para `fail`, por isso, se um pattern match falhar na notação `do`, o `error` é chamado.

### Usando a notação do com o Writer

Agora que temos uma instância `Monad`, estamos livres para usar a notação `do` para valores `Writer`. É útil para quando temos vários valores `Writer` e queremos fazer coisas com eles. Assim como com outras mônadas, podemos tratá-los como valores normais e o contexto será cuidado para nós. Neste caso, todos os valores monóides que vêm anexados são combinados com `mappend` e, portanto, são refletidos no resultado final. Aqui está um exemplo simples de uso da notação `do` com `Writer` para multiplicar dois números:

```{.haskell:hs}
import Control.Monad.Writer

logNumber :: Int -> Writer [String] Int
logNumber x = Writer (x, ["Peguei o numero: " ++ show x])

multWithLog :: Writer [String] Int
multWithLog = do
    a <- logNumber 3
    b <- logNumber 5
    return (a*b)
```

`logNumber` recebe um número e cria um valor `Writer` a partir dele. Para o monóide, usamos uma lista de strings e equipamos o número com uma lista unitária que apenas diz que temos esse número. `multWithLog` é um valor `Writer` que multiplica `3` e `5` e garante que seus logs anexados sejam incluídos no log final. Usamos `return` para apresentar `a*b` como resultado. Como o `return` apenas recebe algo e o coloca em um contexto mínimo, podemos ter certeza de que ele não adicionará nada ao log. Aqui está o que vemos se executarmos isso:

```{.haskell:hs}
ghci> runWriter multWithLog
(15,["Peguei o numero: 3","Peguei o numero: 5"])
```

Às vezes, queremos apenas que algum valor monóide seja incluído em algum ponto específico. Para isso, a função `tell` é útil. Ela faz parte da typeclass `MonadWriter` e, no caso do `Writer`, ela recebe um valor monóide, como `["Isso esta acontecendo"]`, e cria um valor `Writer` que apresenta o valor fictício `()` como seu resultado, mas tem o valor monóide desejado anexado. Quando temos um valor monádico que tem `()` como resultado, não o vinculamos a uma variável. Aqui está o `multWithLog` mas com algum relatório extra incluído:

```{.haskell:hs}
multWithLog :: Writer [String] Int
multWithLog = do
    a <- logNumber 3
    b <- logNumber 5
    tell ["Vou multiplicar esses dois"]
    return (a*b)
```

É importante que `return (a*b)` seja a última linha, porque o resultado da última linha de uma expressão `do` é o resultado de toda a expressão `do`. Se tivéssemos colocado o `tell` como a última linha, `()` teria sido o resultado desta expressão `do`. Perderíamos o resultado da multiplicação. No entanto, o log seria o mesmo. Aqui está isso em ação:

```{.haskell:hs}
ghci> runWriter multWithLog
(15,["Peguei o numero: 3","Peguei o numero: 5","Vou multiplicar esses dois"])

### Adicionando registros aos programas

O algoritmo de Euclides é um algoritmo que recebe dois números e calcula o seu máximo divisor comum. Ou seja, o maior número que ainda divide ambos de forma inteira. O Haskell já possui a função `gcd`, que faz exatamente isso, mas vamos implementar a nossa própria e então equipá-la com capacidades de log. Aqui está o algoritmo normal:

```{.haskell:hs}
gcd' :: Int -> Int -> Int
gcd' a b
    | b == 0    = a
    | otherwise = gcd' b (a `mod` b)
```

O algoritmo é muito simples. Primeiro, ele verifica se o segundo número é 0. Se for, o resultado é o primeiro número. Se não for, o resultado é o máximo divisor comum do segundo número e do resto da divisão do primeiro número pelo segundo. Por exemplo, se quisermos saber qual é o máximo divisor comum de 8 e 3, basta seguir o algoritmo descrito. Como 3 não é 0, temos que encontrar o máximo divisor comum de 3 e 2 (se dividirmos 8 por 3, o resto é 2). Em seguida, encontramos o máximo divisor comum de 3 e 2. 2 ainda não é 0, então agora temos 2 e 1. O segundo número não é 0, então executamos o algoritmo novamente para 1 e 0, já que a divisão de 2 por 1 nos dá um resto de 0. E finalmente, como o segundo número é agora 0, o resultado final é 1. Vamos ver se o nosso código concorda:

```{.haskell:hs}
ghci> gcd' 8 3
1
```

Ele concorda. Muito bom! Agora, queremos equipar nosso resultado com um contexto, e o contexto será um valor monóide que atua como um log. Como antes, usaremos uma lista de strings como nosso monóide. Portanto, o tipo da nossa nova função `gcd'` deve ser:

```{.haskell:hs}
gcd' :: Int -> Int -> Writer [String] Int
```

Tudo o que resta agora é equipar nossa função com valores de log. Aqui está o código:

```{.haskell:hs}
import Control.Monad.Writer

gcd' :: Int -> Int -> Writer [String] Int
gcd' a b
    | b == 0 = do
        tell ["Terminei com " ++ show a]
        return a
    | otherwise = do
        tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]
        gcd' b (a `mod` b)
```

Esta função recebe dois valores `Int` normais e retorna um `Writer [String] Int`, ou seja, um `Int` que tem um contexto de log. No caso em que `b` é `0`, em vez de apenas dar `a` como resultado, usamos uma expressão `do` para compor um valor `Writer` como resultado. Primeiro usamos o `tell` para relatar que terminamos e depois usamos o `return` para apresentar `a` como o resultado da expressão `do`. Em vez dessa expressão `do`, poderíamos também ter escrito isto:

```{.haskell:hs}
Writer (a, ["Terminei com " ++ show a])
```

No entanto, acho que a expressão `do` é mais fácil de ler. Em seguida, temos o caso em que `b` não é `0`. Neste caso, registramos que estamos usando o `mod` para descobrir o resto da divisão de `a` e `b`. Então, a segunda linha da expressão `do` simplesmente chama recursivamente o `gcd'`. Lembre-se, o `gcd'` agora retorna finalmente um valor `Writer`, então é perfeitamente válido que ``gcd' b (a `mod` b)`` seja uma linha em uma expressão `do`.

Embora possa ser útil rastrear a execução deste novo `gcd'` manualmente para ver como os logs são anexados, acho que é mais esclarecedor apenas olhar para o panorama geral e ver estes como valores com um contexto e, a partir daí, ganhar uma visão de qual será o resultado final.

Vamos testar o nosso novo `gcd'`. O seu resultado é um valor `Writer [String] Int` e, se o desempacotarmos do seu `newtype`, obteremos uma tupla. A primeira parte da tupla é o resultado. Vamos ver se está tudo bem:

```{.haskell:hs}
ghci> fst $ runWriter (gcd' 8 3)
1
```

Bom! E quanto ao log? Como o log é uma lista de strings, vamos usar `mapM_ putStrLn` para imprimir essas strings na tela:

```{.haskell:hs}
ghci> mapM_ putStrLn $ snd $ runWriter (gcd' 8 3)
8 mod 3 = 2
3 mod 2 = 1
2 mod 1 = 0
Terminei com 1
```

Acho incrível como conseguimos mudar nosso algoritmo comum para um que relata o que faz à medida que avança, apenas mudando valores normais para valores monádicos e deixando que a implementação do `>>=` para o `Writer` cuide dos logs para nós. Podemos adicionar um mecanismo de registro a quase qualquer função. Basta substituir os valores normais por valores `Writer` onde quisermos e mudar a aplicação normal de funções para `>>=` (ou expressões `do` se isso aumentar a legibilidade).

### Construção de lista ineficiente

Ao usar a mônada `Writer`, você deve ter cuidado com qual monóide usar, porque usar listas pode às vezes tornar-se muito lento. Isso ocorre porque as listas usam o `++` para o `mappend`, e usar o `++` para adicionar algo ao final de uma lista é lento se essa lista for muito longa.

Em nossa função `gcd'`, o registro é rápido porque a anexação da lista acaba parecendo com isto:

```{.haskell:hs}
a ++ (b ++ (c ++ (d ++ (e ++ f))))
```

As listas são uma estrutura de dados que é construída da esquerda para a direita, e isso é eficiente porque primeiro construímos totalmente a parte esquerda de uma lista e só então adicionamos uma lista mais longa à direita. Mas se não formos cuidadosos, usar a mônada `Writer` pode produzir uma anexação de lista que se parece com isto:

```{.haskell:hs}
((((a ++ b) ++ c) ++ d) ++ e) ++ f
```

Isso se associa à esquerda em vez de à direita. Isso é ineficiente porque cada vez que queremos adicionar a parte direita à esquerda, temos que reconstruir a parte esquerda desde o início!

A função a seguir funciona como o `gcd'`, apenas registra as coisas de forma inversa. Primeiro ela produz o log para o resto do procedimento e depois adiciona o passo atual ao final do log.

```{.haskell:hs}
import Control.Monad.Writer

gcdReverse :: Int -> Int -> Writer [String] Int
gcdReverse a b
    | b == 0 = do
        tell ["Terminei com " ++ show a]
        return a
    | otherwise = do
        result <- gcdReverse b (a `mod` b)
        tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]
        return result
```

Ela faz a recursão primeiro e vinculá seu valor de resultado a `result`. Em seguida, adiciona o passo atual ao log, mas o passo atual vai para o final do log que foi produzido pela recursão. Finalmente, apresenta o resultado da recursão como o resultado final. Aqui está ela em ação:

```{.haskell:hs}
ghci> mapM_ putStrLn $ snd $ runWriter (gcdReverse 8 3)
Terminei com 1
2 mod 1 = 0
3 mod 2 = 1
8 mod 3 = 2
```

É ineficiente porque acaba associando o uso de `++` à esquerda em vez de à direita.

### Listas de diferença

![cactos](assets/images/for-a-few-monads-more/cactus.png){.left width=147 height=300}

Como as listas podem ser ineficientes às vezes quando anexadas repetidamente dessa maneira, é melhor usar uma estrutura de dados que suporte sempre uma anexação eficiente. Uma dessas estruturas de dados é a lista de diferença. Uma lista de diferença é semelhante a uma lista, só que em vez de ser uma lista normal, é uma função que recebe uma lista e anexa outra lista a ela. O equivalente em lista de diferença de uma lista como `[1,2,3]` seria a função `\xs -> [1,2,3] ++ xs`. Uma lista vazia normal é `[]`, enquanto uma lista de diferença vazia é a função `\xs -> [] ++ xs`.

O legal das listas de diferença é que elas suportam uma anexação eficiente. Quando anexadas duas listas normais com `++`, temos que percorrer todo o caminho até o final da lista à esquerda de `++` e então colar a outra ali. Mas e se adotarmos a abordagem de lista de diferença e representarmos nossas listas como funções? Bem, então, anexar duas listas de diferença pode ser feito desta forma:

```{.haskell:hs}
f `append` g = \xs -> f (g xs)
```

Lembre-se, `f` e `g` são funções que recebem listas e anexam algo a elas. Então, por exemplo, se `f` for a função `("cao"++)` (outra forma de escrever `\xs -> "cao" ++ xs`) e `g` for a função `("carne"++)`, então ``f `append` g`` cria uma nova função que é equivalente à seguinte:

```{.haskell:hs}
\xs -> "cao" ++ ("carne" ++ xs)
```

Anexamos duas listas de diferença apenas criando uma nova função que primeiro aplica uma lista de diferença a alguma lista e depois a outra.

Vamos criar um wrapper `newtype` para listas de diferença para que possamos facilmente dar a elas instâncias de monóide:

```{.haskell:hs}
newtype DiffList a = DiffList { getDiffList :: [a] -> [a] }
```

O tipo que envolvemos é `[a] -> [a]` porque uma lista de diferença é apenas uma função que recebe uma lista e retorna outra. Converter listas normais em listas de diferença e vice-versa é fácil:

```{.haskell:hs}
toDiffList :: [a] -> DiffList a
toDiffList xs = DiffList (xs++)

fromDiffList :: DiffList a -> [a]
fromDiffList (DiffList f) = f []
```

Para transformar uma lista normal em uma lista de diferença, basta fazermos o que fizemos antes e transformá-la em uma função que a anexa a outra lista. Como uma lista de diferença é uma função que anexa algo a outra lista, se quisermos apenas esse "algo", aplicamos a função a uma lista vazia!

Aqui está a instância `Monoid`:

```{.haskell:hs}
instance Monoid (DiffList a) where
    mempty = DiffList (\xs -> [] ++ xs)
    (DiffList f) `mappend` (DiffList g) = DiffList (\xs -> f (g xs))
```

Observe como para listas, o `mempty` é apenas a função `id` e o `mappend` é na verdade apenas a composição de funções. Vamos ver se isto funciona:

```{.haskell:hs}
ghci> fromDiffList (toDiffList [1,2,3,4] `mappend` toDiffList [1,2,3])
[1,2,3,4,1,2,3]
```

Ponta de linha! Agora podemos aumentar a eficiência da nossa função `gcdReverse` fazendo-a usar listas de diferença em vez de listas normais:

```{.haskell:hs}
import Control.Monad.Writer

gcd' :: Int -> Int -> Writer (DiffList String) Int
gcd' a b
    | b == 0 = do
        tell (toDiffList ["Terminei com " ++ show a])
        return a
    | otherwise = do
        result <- gcd' b (a `mod` b)
        tell (toDiffList [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)])
        return result
```

Tivemos apenas que mudar o tipo do monóide de `[String]` para `DiffList String` e depois, ao usar o `tell`, converter nossas listas normais em listas de diferença com o `toDiffList`. Vamos ver se o log é montado corretamente:

```{.haskell:hs}
ghci> mapM_ putStrLn . fromDiffList . snd . runWriter $ gcdReverse 110 34
Terminei com 2
8 mod 2 = 0
34 mod 8 = 2
110 mod 34 = 8
```

Fazemos `gcdReverse 110 34`, depois usamos o `runWriter` para desempacotá-lo do seu `newtype`, depois aplicamos o `snd` a isso para obter apenas o log, depois aplicamos o `fromDiffList` para convertê-lo em uma lista normal e, finalmente, imprimimos suas entradas na tela.

### Comparando o desempenho

Para ter uma ideia de quanto as listas de diferença podem melhorar o desempenho, considere esta função que apenas faz uma contagem regressiva de algum número até zero, mas produz seu log de forma reversa, como `gcdReverse`, de modo que os números no log serão contados de forma progressiva:

```{.haskell:hs}
finalCountDown :: Int -> Writer (DiffList String) ()
finalCountDown 0 = do
    tell (toDiffList ["0"])
finalCountDown x = do
    finalCountDown (x-1)
    tell (toDiffList [show x])
```

Se dermos `0`, ela apenas registra. Para qualquer outro número, ela primeiro faz a contagem regressiva do seu antecessor até `0` e depois anexa esse número ao log. Portanto, se aplicarmos `finalCountDown` a `100`, a string `"100"` virá por último no log.

De qualquer modo, se você carregar esta função no GHCi e aplicá-la a um número grande, como `500000`, verá que ela começa a contar rapidamente de `0` em diante:

```{.haskell:hs}
ghci> mapM_ putStrLn . fromDiffList . snd . runWriter $ finalCountDown 500000
0
1
2
...
```

No entanto, se mudarmos para usar listas normais em vez de listas de diferença, desta forma:

```{.haskell:hs}
finalCountDown :: Int -> Writer [String] ()
finalCountDown 0 = do
    tell ["0"]
finalCountDown x = do
    finalCountDown (x-1)
    tell [show x]
```

E então pedirmos para o GHCi começar a contar:

```{.haskell:hs}
ghci> mapM_ putStrLn . snd . runWriter $ finalCountDown 500000
```

Veremos que a contagem é muito lenta.

Claro, esta não é a maneira correta e científica de testar a rapidez dos nossos programas, mas fomos capazes de ver que, neste caso, usar listas de diferença começa a produzir resultados imediatamente, enquanto as listas normais levam uma eternidade.

Ah, a propósito, a música Final Countdown do Europe agora está grudada na sua cabeça. Divirta-se!

## Reader? Ugh, essa piada de novo não. {#reader}

![bang voce esta morto](assets/images/for-a-few-monads-more/revolver.png){.left width=280 height=106}

No [capítulo sobre aplicativos](functors-applicative-functors-and-monoids.html), vimos que o tipo de função, `(->) r` é uma instância de `Functor`. Mapear uma função `f` sobre uma função `g` criará uma função que recebe a mesma coisa que `g`, aplica `g` a ela e então aplica `f` a esse resultado. Basicamente, estamos criando uma nova função que é como `g`, só que antes de retornar seu resultado, `f` também é aplicado a esse resultado. Por exemplo:

```{.haskell:hs}
ghci> let f = (*5)
ghci> let g = (+3)
ghci> (fmap f g) 8
55
```

Também vimos que as funções são functores aplicativos. Elas nos permitem operar nos resultados eventuais das funções como se já tivéssemos seus resultados. Aqui está um exemplo:

```{.haskell:hs}
ghci> let f = (+) <$> (*2) <*> (+10)
ghci> f 3
19
```

A expressão `(+) <$> (*2) <*> (+10)` cria uma função que recebe um número, dá esse número para `(*2)` e `(+10)` e depois soma os resultados. Por exemplo, se aplicarmos esta função a `3`, ela aplica tanto `(*2)` quanto `(+10)` a `3`, dando `6` e `13`. Então, ela chama `(+)` com `6` e `13` e o resultado é `19`.

O tipo de função `(->) r` não é apenas um functor e um functor aplicativo, mas também é uma mônada. Assim como outros valores monádicos que conhecemos até agora, uma função também pode ser considerada um valor com um contexto. O contexto para funções é que esse valor ainda não está presente e que temos que aplicar essa função a algo para obter seu valor de resultado.

Como já estamos familiarizados com o funcionamento das funções como functores e functores aplicativos, vamos mergulhar de cabeça e ver como é a sua instância `Monad`. Ela está localizada em `Control.Monad.Instances` e funciona mais ou menos assim:

```{.haskell:hs}
instance Monad ((->) r) where
    return x = \_ -> x
    h >>= f = \w -> f (h w) w
```

Já vimos como o `pure` é implementado para funções, e o `return` é basicamente a mesma coisa que o `pure`. Ele recebe um valor e o coloca em um contexto mínimo que sempre tem esse valor como resultado. E a única maneira de criar uma função que sempre tenha um determinado valor como resultado é fazê-la ignorar completamente o seu parâmetro.

A implementação para o `>>=` parece um pouco enigmática, mas não é tudo isso. Quando usamos o `>>=` para alimentar um valor monádico para uma função, o resultado é sempre um valor monádico. Portanto, neste caso, quando alimentamos uma função para outra função, o resultado também é uma função. É por isso que o resultado começa como um lambda. Todas as implementações do `>>=` até agora sempre isolaram de alguma forma o resultado do valor monádico e depois aplicaram a função `f` a esse resultado. A mesma coisa acontece aqui. Para obter o resultado de uma função, temos que aplicá-la a algo, e é por isso que fazemos `(h w)` aqui para obter o resultado da função e depois aplicamos `f` a isso. O `f` retorna um valor monádico, que é uma função no nosso caso, por isso também o aplicamos a `w`.

Se você não entender como o `>>=` funciona neste momento, não se preocupe, porque com exemplos veremos que esta é uma mônada muito simples. Aqui está uma expressão `do` que utiliza esta mônada:

```{.haskell:hs}
import Control.Monad.Instances

addStuff :: Int -> Int
addStuff = do
    a <- (*2)
    b <- (+10)
    return (a+b)
```

Isso é a mesma coisa que a expressão aplicativa que escrevemos anteriormente, só que agora ela depende das funções serem mônadas. Uma expressão `do` sempre resulta em um valor monádico e esta não é diferente. O resultado deste valor monádico é uma função. O que acontece aqui é que ela recebe um número e então o `(*2)` é aplicado a esse número e o resultado se torna `a`. O `(+10)` é aplicado ao mesmo número que o `(*2)` foi aplicado e o resultado se torna `b`. O `return`, como em outras mônadas, não tem nenhum outro efeito a não ser criar um valor monádico que apresente algum resultado. Isso apresenta `a+b` como o resultado desta função. Se testarmos, obteremos o mesmo resultado de antes:

```{.haskell:hs}
ghci> addStuff 3
19
```

Tanto o `(*2)` quanto o `(+10)` são aplicados ao número `3` neste caso. `return (a+b)` também, mas ele o ignora e sempre apresenta `a+b` como o resultado. Por esta razão, a mônada de função também é chamada de mônada reader (leitor). Todas as funções lêem de uma fonte comum. Para ilustrar isto ainda melhor, podemos reescrever o `addStuff` desta forma:

```{.haskell:hs}
addStuff :: Int -> Int
addStuff x = let
    a = (*2) x
    b = (+10) x
    in a+b
```

Vemos que a mônada reader nos permite tratar funções como valores com um contexto. Podemos agir como se já soubéssemos o que as funções retornarão. Ela faz isso colando as funções em uma única função e dando o parâmetro dessa função a todas as funções a partir das quais ela foi colada. Portanto, se tivermos muitas funções que estão todas sentindo falta de apenas um parâmetro e que eventualmente seriam aplicadas à mesma coisa, podemos usar a mônada reader para extrair os seus resultados futuros e a implementação do `>>=` garantirá que tudo corra bem.

## Computações com estado de bom gosto {#state}

![não brinque com o texas](assets/images/for-a-few-monads-more/texas.png){.left width=244 height=230}

Haskell é uma linguagem pura e, por causa disso, nossos programas são feitos de funções que não podem alterar nenhum estado global ou variáveis; elas podem apenas fazer algumas computações e retornar seus resultados. Essa restrição na verdade torna mais fácil pensar sobre nossos programas, pois nos livra de nos preocuparmos com o valor de cada variável em algum momento no tempo. No entanto, alguns problemas são inerentemente estatais, no sentido de que dependem de algum estado que muda ao longo do tempo. Embora tais problemas não sejam um problema para o Haskell, eles podem ser um pouco tediosos de modelar às vezes. É por isso que o Haskell apresenta uma coisa chamada mônada state (estado), que torna a lida com problemas estatais uma brisa, mantendo tudo agradável e puro.

[Quando estávamos lidando com números aleatórios](input-and-output.html#randomness), lidamos com funções que recebiam um gerador aleatório como parâmetro e retornavam um número aleatório e um novo gerador aleatório. Se quisermos gerar vários números aleatórios, sempre tínhamos que usar o gerador aleatório que uma função anterior retornou junto com seu resultado. Ao fazer uma função que recebe um `StdGen` e joga uma moeda três vezes com base nesse gerador, tínhamos que fazer isto:

```{.haskell:hs}
threeCoins :: StdGen -> (Bool, Bool, Bool)
threeCoins gen =
    let (firstCoin, newGen) = random gen
        (secondCoin, newGen') = random newGen
        (thirdCoin, newGen'') = random newGen'
    in  (firstCoin, secondCoin, thirdCoin)
```

Ela recebia um gerador `gen` e então `random gen` retornava um valor `Bool` junto com um novo gerador. Para jogar a segunda moeda, usamos o novo gerador, e assim por diante. Na maioria das outras linguagens, não teríamos que retornar um novo gerador junto com um número aleatório. Poderíamos apenas modificar o já existente! Mas como o Haskell é puro, não podemos fazer isso, então tivemos que pegar algum estado, criar um resultado a partir dele e um novo estado, e então usar esse novo estado para gerar novos resultados.

Você pensaria que, para evitar lidar manualmente com computações estatais dessa maneira, teríamos que abrir mão da pureza do Haskell. Bem, não precisamos, pois existe uma pequena mônada especial chamada mônada state que cuida de todo esse negócio de estado para nós e sem abrir mão de nada da pureza que torna a programação em Haskell tão legal.

Então, para nos ajudar a entender melhor esse conceito de computações estatais, vamos em frente e dar a elas um tipo. Diremos que uma computação estatal é uma função que recebe algum estado e retorna um valor junto com algum novo estado. Essa função teria o seguinte tipo:

```{.haskell:hs}
s -> (a,s)
```

`s` é o tipo do estado e `a` o resultado das computações estatais.

::: {.hintbox}
A atribuição na maioria das outras linguagens poderia ser pensada como uma computação estatal. Por exemplo, quando fazemos `x = 5` em uma linguagem imperativa, ela geralmente atribui o valor `5` à variável `x` e também terá o valor `5` como uma expressão. Se você olhar para isso funcionalmente, poderia vê-lo como uma função que recebe um estado (ou seja, todas as variáveis que foram atribuídas anteriormente) e retorna um resultado (neste caso `5`) e um novo estado, que seria todos os mapeamentos de variáveis anteriores mais a variável recém-atribuída.
:::

Esta computação estatal, uma função que recebe um estado e retorna um resultado e um novo estado, pode ser pensada como um valor com um contexto também. O valor real é o resultado, enquanto o contexto é que temos que fornecer algum estado inicial para realmente obter esse resultado e que, além de obter um resultado, também obtemos um novo estado.

### Pilhas e pedras

Digamos que queiramos modelar a operação de uma pilha (stack). Você tem uma pilha de coisas uma em cima da outra e pode colocar coisas em cima dessa pilha ou pode tirar coisas do topo da pilha. Quando você está colocando um item no topo da pilha, dizemos que você o está empilhando (*pushing*) e quando você está tirando coisas do topo, dizemos que o está desempilhando (*popping*). Se você quiser algo que está no fundo da pilha, você tem que desempilhar tudo o que está acima dele.

Usaremos uma lista para representar nossa pilha e a cabeça da lista será o topo da pilha. Para nos ajudar em nossa tarefa, faremos duas funções: `pop` e `push`. `pop` receberá uma pilha, desempilhará um item e retornará esse item como resultado e também retornará uma nova pilha, sem esse item. `push` receberá um item e uma pilha e então empilhará esse item na pilha. Ela retornará `()` como seu resultado, junto com uma nova pilha. Aqui vai:

```{.haskell:hs}
type Stack = [Int]

pop :: Stack -> (Int,Stack)
pop (x:xs) = (x,xs)

push :: Int -> Stack -> ((),Stack)
push a xs = ((),a:xs)
```

Usamos `()` como o resultado ao empurrar para a pilha porque empurrar um item na pilha não tem nenhum valor de resultado importante, seu trabalho principal é mudar a pilha. Observe como, apenas ao aplicarmos o primeiro parâmetro de `push`, obtemos uma computação estatal. `pop` já é uma computação estatal por causa do seu tipo.

Vamos escrever um pequeno pedaço de código para simular uma pilha usando estas funções. Pegaremos uma pilha, empilharemos `3` nela e depois desempilharemos dois itens, apenas por diversão. Aqui está ele:

```{.haskell:hs}
stackManip :: Stack -> (Int, Stack)
stackManip stack = let
    ((),newStack1) = push 3 stack
    (a ,newStack2) = pop newStack1
    in pop newStack2
```

Pegamos uma `stack` e então fazemos `push 3 stack`, o que resulta em uma tupla. A primeira parte da tupla é um `()` e a segunda é uma nova pilha e a chamamos de `newStack1`. Então, desempilhamos um número de `newStack1`, o que resulta em um número `a` (que é o `3` que empilhamos) e uma nova pilha que chamamos de `newStack2`. Então, desempilhamos um número da `newStack2` e obtemos um número que é `b` e uma `newStack3`. Retornamos uma tupla com esse número e essa pilha. Vamos testar:

```{.haskell:hs}
ghci> stackManip [5,8,2,1]
(5,[8,2,1])
```

Legal, o resultado é `5` e a nova pilha é `[8,2,1]`. Observe como `stackManip` é por si só uma computação estatal. Pegamos um punhado de computações estatais e as colamos juntas, por assim dizer. Hmm, parece familiar.

O código acima para o `stackManip` é meio tedioso, pois estamos dando manualmente o estado para cada computação estatal e armazenandoo para depois passá-lo para a próxima. Não seria mais legal se, em vez de dar a pilha manualmente para cada função, pudéssemos escrever algo como isto:

```{.haskell:hs}
stackManip = do
    push 3
    a <- pop
    pop
```

Bem, usar a mônada state nos permitirá fazer exatamente isso. Com ela, poderemos pegar computações estatais como estas e usá-las sem ter que gerenciar o estado manualmente.

### A mônada State

O módulo `Control.Monad.State` fornece um `newtype` que envolve computações estatais. Aqui está sua definição:

```{.haskell:hs}
newtype State s a = State { runState :: s -> (a,s) }
```

Um `State s a` é uma computação estatal que manipula um estado do tipo `s` e tem um resultado do tipo `a`.

Agora que vimos do que se trata as computações estatais e como elas podem até ser pensadas como valores com contextos, vamos conferir a sua instância `Monad`:

```{.haskell:hs}
instance Monad (State s) where
    return x = State $ \s -> (x,s)
    (State h) >>= f = State $ \s -> let (a, newState) = h s
                                        (State g) = f a
                                    in  g newState
```

Vamos dar uma olhada no `return` primeiro. Nosso objetivo com o `return` é pegar um valor e criar uma computação estatal que sempre tenha esse valor como resultado. É por isso que apenas criamos um lambda `\s -> (x,s)`. Sempre apresentamos `x` como o resultado da computação estatal e o estado permanece inalterado, porque o `return` tem que colocar um valor em um contexto mínimo. Então, o `return` criará uma computação estatal que apresenta um determinado valor como resultado e mantém o estado inalterado.

![eu sou um guarda](assets/images/for-a-few-monads-more/badge.png){.right width=182 height=160}

E o `>>=`? Bem, o resultado de alimentar uma computação estatal a uma função com `>>=` tem que ser uma computação estatal, certo? Então começamos com o wrapper `newtype` `State` e depois escrevemos um lambda. Este lambda será nossa nova computação estatal. Mas o que acontece nele? Bem, de alguma forma temos que extrair o valor do resultado da primeira computação estatal. Como estamos em uma computação estatal agora, podemos dar à computação estatal `h` o nosso estado atual `s`, o que resulta em um par de resultado e um novo estado: `(a, newState)`. Toda vez que implementamos o `>>=` até agora, uma vez que tínhamos extraído o resultado do valor monádico, aplicávamos a função `f` a ele para obter o novo valor monádico. No `Writer`, depois de fazer isso e obter o novo valor monádico, ainda tínhamos que garantir que o contexto fosse cuidado fazendo o `mappend` do valor monóide antigo com o novo. Aqui, fazemos `f a` e obtemos uma nova computação estatal `g`. Agora que temos uma nova computação estatal e um novo estado (que atende pelo nome de `newState`), apenas aplicamos essa computação estatal `g` ao `newState`. O resultado é uma tupla de resultado final e estado final!

Portanto, com o `>>=`, colamos duas computações estatais juntas, porém a segunda está escondida dentro de uma função que recebe o resultado da anterior. Como `pop` e `push` já são computações estatais, é fácil envolvê-las em um wrapper `State`. Observe:

```{.haskell:hs}
import Control.Monad.State

pop :: State Stack Int
pop = State $ \(x:xs) -> (x,xs)

push :: Int -> State Stack ()
push a = State $ \xs -> ((),a:xs)
```

`pop` já é uma computação estatal e `push` recebe um `Int` e retorna uma computação estatal. Agora podemos reescrever nosso exemplo anterior de empilhar `3` na pilha e depois desempilhar dois números desta forma:

```{.haskell:hs}
import Control.Monad.State

stackManip :: State Stack Int
stackManip = do
    push 3
    a <- pop
    pop
```

Viu como colamos um push e dois pops em uma única computação estatal? Quando a desempacotamos de seu wrapper `newtype`, obtemos uma função para a qual podemos fornecer algum estado inicial:

```{.haskell:hs}
ghci> runState stackManip [5,8,2,1]
(5,[8,2,1])
```

Não tivemos que vincular o segundo `pop` a `a` porque não usamos esse `a` para nada. Então poderíamos ter escrito desta forma:

```{.haskell:hs}
stackManip :: State Stack Int
stackManip = do
    push 3
    pop
    pop
```

Bem legal. Mas e se quisermos fazer isto: desempilhar um número da pilha e, se esse número for `5`, apenas colocá-lo de volta na pilha e parar, mas se não for `5`, empilhar `3` e `8` de volta? Bem, aqui está o código:

```{.haskell:hs}
stackStuff :: State Stack ()
stackStuff = do
    a <- pop
    if a == 5
        then push 5
        else do
            push 3
            push 8
```

Isto é bem direto. Vamos executá-lo com uma pilha inicial.

```{.haskell:hs}
ghci> runState stackStuff [9,0,2,1,0]
((),[8,3,0,2,1,0])
```

Lembre-se, as expressões `do` resultam em valores monádicos e, com a mônada `State`, uma única expressão `do` também é uma função estatal. Como `stackManip` e `stackStuff` são computações estatais comuns, podemos colá-las para produzir novas computações estatais.

```{.haskell:hs}
moreStack :: State Stack ()
moreStack = do
    a <- stackManip
    if a == 100
        then stackStuff
        else return ()
```

Se o resultado de `stackManip` na pilha atual for `100`, executamos `stackStuff`, caso contrário não fazemos nada. `return ()` apenas mantém o estado como está e não faz nada.

O módulo `Control.Monad.State` fornece uma typeclass chamada `MonadState` que apresenta duas funções bem úteis, chamadas `get` e `put`. Para o `State`, a função `get` é implementada desta forma:

```{.haskell:hs}
get = State $ \s -> (s,s)
```

Então ela apenas pega o estado atual e o apresenta como o resultado. A função `put` recebe algum estado e cria uma função estatal que substitui o estado atual por ele:

```{.haskell:hs}
put newState = State $ \s -> ((),newState)
```

Com estas funções, podemos ver qual é a pilha atual ou podemos substituí-la por uma pilha completamente diferente. Assim:

```{.haskell:hs}
stackyStack :: State Stack ()
stackyStack = do
    stackNow <- get
    if stackNow == [1,2,3]
        then put [8,3,1]
        else put [9,2,1]
```

Vale a pena examinar qual seria o tipo de `>>=` se ele funcionasse apenas para valores `State`:

```{.haskell:hs}
(>>=) :: State s a -> (a -> State s b) -> State s b
```

Viu como o tipo do estado `s` permanece o mesmo, mas o tipo do resultado pode mudar de `a` para `b`? Isso significa que podemos colar várias computações estatais cujos resultados são de tipos diferentes, mas o tipo do estado tem que permanecer o mesmo. E por que isso? Bem, por exemplo, para o `Maybe`, o `>>=` tem este tipo:

```{.haskell:hs}
(>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
```

Faz sentido que a própria mônada, `Maybe`, não mude. Não faria sentido usar `>>=` entre duas mônadas diferentes. Bem, para a mônada state, a mônada é na verdade `State s`, então se esse `s` fosse diferente, estaríamos usando `>>=` entre duas mônadas diferentes.

### Aleatoriedade e a mônada state

No início desta seção, vimos como gerar números pode às vezes ser incômodo porque cada função aleatória recebe um gerador e retorna um número aleatório junto com um novo gerador, que deve ser usado no lugar do antigo se quisermos gerar outro número aleatório. A mônada state torna a lida com isso muito mais fácil.

A função `random` de `System.Random` tem o seguinte tipo:

```{.haskell:hs}
random :: (RandomGen g, Random a) => g -> (a, g)
```

Ou seja, ela recebe um gerador aleatório e produz um número aleatório junto com um novo gerador. Podemos ver que se trata de uma computação estatal, então podemos envolvê-la no construtor `newtype` `State` e então usá-la como um valor monádico para que a passagem do estado seja tratada para nós:

```{.haskell:hs}
import System.Random
import Control.Monad.State

randomSt :: (RandomGen g, Random a) => State g a
randomSt = State random
```

Então, agora, se quisermos jogar três moedas (`True` é coroa, `False` é cara) basta fazer o seguinte:

```{.haskell:hs}
import System.Random
import Control.Monad.State

threeCoins :: State StdGen (Bool,Bool,Bool)
threeCoins = do
    a <- randomSt
    b <- randomSt
    c <- randomSt
    return (a,b,c)
```

`threeCoins` é agora uma computação estatal e, após receber um gerador aleatório inicial, ela o passa para o primeiro `randomSt`, que produz um número e um novo gerador, o qual é passado para o próximo e assim por diante. Usamos `return (a,b,c)` para apresentar `(a,b,c)` como o resultado sem alterar o gerador mais recente. Vamos testar:

```{.haskell:hs}
ghci> runState threeCoins (mkStdGen 33)
((True,False,True),680029187 2103410263)
```

Legas. Fazer essas coisas que exigem que algum estado seja mantido entre as etapas tornou-se muito menos trabalhoso!

## Erro, erro meu... {#error}

Já sabemos que o `Maybe` é usado para adicionar um contexto de possível falha aos valores. Um valor pode ser um `Just algo` ou um `Nothing`. No entanto, por mais útil que isso seja, quando temos um `Nothing`, tudo o que sabemos é que houve algum tipo de falha, mas não há como enfiar mais informações ali dizendo que tipo de falha foi ou por que falhou.

O tipo `Either e a`, por outro lado, nos permite incorporar um contexto de possível falha aos nossos valores e também anexar valores à falha, para que eles possam descrever o que deu errado ou fornecer outras informações úteis sobre a falha. Um valor `Either e a` pode ser um valor `Right`, significando a resposta correta e o sucesso, ou pode ser um valor `Left`, significando falha. Por exemplo:

```{.haskell:hs}
ghci> :t Right 4
Right 4 :: (Num t) => Either a t
ghci> :t Left "erro de falta de queijo"
Left "erro de falta de queijo" :: Either [Char] b
```

Isso é praticamente um `Maybe` aprimorado, então faz sentido que seja uma mônada, porque também pode ser visto como um valor com um contexto adicional de falha possível, só que agora também há um valor anexado quando há um erro.

Sua instância `Monad` é semelhante à do `Maybe` e pode ser encontrada em `Control.Monad.Error`:

```{.haskell:hs}
instance (Error e) => Monad (Either e) where
    return x = Right x
    Right x >>= f = f x
    Left err >>= f = Left err
    fail msg = Left (strMsg msg)
```

O `return`, como sempre, recebe um valor e o coloca em um contexto mínimo padrão. Ele envolve o nosso valor no construtor `Right` porque estamos usando o `Right` para representar uma computação bem-sucedida onde um resultado está presente. Isso é muito parecido com o `return` para o `Maybe`.

O `>>=` examina dois casos possíveis: um `Left` e um `Right`. No caso de um `Right`, a função `f` é aplicada ao valor dentro dele, de forma semelhante a como, no caso de um `Just`, a função é aplicada ao seu conteúdo. No caso de um erro, o valor `Left` é mantido, junto com o seu conteúdo, que descreve a falha.

A instância `Monad` para `Either e` faz uma exigência adicional: o tipo do valor contido em um `Left`, aquele que é indexado pelo parâmetro de tipo `e`, tem que ser uma instância da typeclass `Error`. A typeclass `Error` é para tipos cujos valores podem agir como mensagens de erro. Ela define a função `strMsg`, que recebe um erro na forma de uma string e retorna um valor desse tipo. Um bom exemplo de uma instância `Error` é, bem, o tipo `String`! No caso da `String`, a função `strMsg` apenas retorna a string que recebeu:

```{.haskell:hs}
ghci> :t strMsg
strMsg :: (Error a) => String -> a
ghci> strMsg "boom!" :: String
"boom!"
```

Mas, como geralmente usamos `String` para descrever o erro ao usar `Either`, não precisamos nos preocupar muito com isso. Quando um pattern match falha na notação `do`, um valor `Left` é usado para significar essa falha.

De qualquer forma, aqui estão alguns exemplos de uso:

```{.haskell:hs}
ghci> Left "boom" >>= \x -> return (x+1)
Left "boom"
ghci> Right 100 >>= \x -> Left "de jeito nenhum!"
Left "de jeito nenhum!"
```

Quando usamos o `>>=` para alimentar um valor `Left` para uma função, a função é ignorada e um valor `Left` idêntico é retornado. Quando alimentamos um valor `Right` para uma função, a função é aplicada ao que está dentro, mas no nosso caso essa função produziu um valor `Left` de qualquer maneira!

Quando tentamos alimentar um valor `Right` para uma função que também tem sucesso, somos surpreendidos por um erro de tipo peculiar! Hmmm.

```{.haskell:hs}
ghci> Right 3 >>= \x -> return (x + 100)

<interactive>:1:0:
    Ambiguous type variable `a' in the constraints:
      `Error a' arising from a use of `it' at <interactive>:1:0-33
      `Show a' arising from a use of `print' at <interactive>:1:0-33
    Probable fix: add a type signature that fixes these type variable(s)
```

O Haskell diz que não sabe qual tipo escolher para a parte `e` do nosso valor de tipo `Either e a`, embora estejamos apenas imprimindo a parte `Right`. Isso se deve à restrição `Error e` na instância `Monad`. Portanto, se você receber erros de tipo como este ao usar o `Either` como uma mônada, basta adicionar uma assinatura de tipo explícita:

```{.haskell:hs}
ghci> Right 3 >>= \x -> return (x + 100) :: Either String Int
Right 103
```

Tudo bem, agora funciona!

Além desse pequeno contratempo, usar esta mônada é muito semelhante a usar o `Maybe` como uma mônada. No capítulo anterior, usamos os aspectos monádicos do `Maybe` para simular pássaros pousando na vara de equilíbrio de um equilibrista. Como exercício, você pode reescrever isso com a mônada de erro para que, quando o equilibrista escorregue e caia, lembremos de quantos pássaros estavam em cada lado da vara quando ele caiu.

## Algumas funções monádicas úteis {#useful-monadic-functions}

Nesta seção, exploraremos algumas funções que operam em valores monádicos ou retornam valores monádicos como resultados (ou ambos!). Tais funções são geralmente chamadas de funções monádicas. Embora algumas delas sejam completamente novas, outras serão contrapartes monádicas de funções que já conhecemos, como `filter` e `foldl`. Vamos ver quais são!

### liftM e amigos

![eu também sou um guarda](assets/images/for-a-few-monads-more/wolf.png){.right width=394 height=222}

Quando começamos nossa jornada rumo ao topo da Montanha Monada, primeiro olhamos para os functores, que são para coisas sobre as quais se pode mapear. Em seguida, aprendemos sobre functores aprimorados chamados functores aplicativos, que nos permitiram aplicar funções normais entre vários valores aplicativos, bem como pegar um valor normal e colocá-lo em algum contexto padrão. Finalmente, introduzimos as mônadas como functores aplicativos aprimorados, que adicionaram a capacidade desses valores com contexto de serem de alguma forma alimentados em funções normais.

Portanto, toda mônada é um functor aplicativo e todo functor aplicativo é um functor. A typeclass `Applicative` tem uma restrição de classe tal que o nosso tipo tem que ser uma instância de `Functor` antes de podermos torná-lo uma instância de `Applicative`. Mas, embora a `Monad` devesse ter a mesma restrição para `Applicative`, já que toda mônada é um functor aplicativo, não tem, porque a typeclass `Monad` foi introduzida no Haskell muito antes da `Applicative`.

Mas apesar de toda mônada ser um functor, não precisamos depender de que ela tenha uma instância de `Functor` por causa da função `liftM`. O `liftM` recebe uma função e um valor monádico e o mapeia sobre o valor monádico. Portanto, é praticamente a mesma coisa que o `fmap`! Este é o tipo do `liftM`:

```{.haskell:hs}
liftM :: (Monad m) => (a -> b) -> m a -> m b
```

E este é o tipo do `fmap`:

```{.haskell:hs}
fmap :: (Functor f) => (a -> b) -> f a -> f b
```

Se as instâncias de `Functor` e `Monad` para um tipo obedecerem às leis dos functores e das mônadas, estas duas coisas resultam na mesma coisa (e todas as mônadas que conhecemos até agora obedecem a ambas). Isso é parecido com o `pure` e o `return` fazendo a mesma coisa, só que um tem uma restrição de classe `Applicative` enquanto o outro tem uma `Monad`. Vamos testar o `liftM`:

```{.haskell:hs}
ghci> liftM (*3) (Just 8)
Just 24
ghci> fmap (*3) (Just 8)
Just 24
ghci> runWriter $ liftM not $ Writer (True, "grao-de-bico")
(False,"grao-de-bico")
ghci> runWriter $ fmap not $ Writer (True, "grao-de-bico")
(False,"grao-de-bico")
ghci> runState (liftM (+100) pop) [1,2,3,4]
(101,[2,3,4])
ghci> runState (fmap (+100) pop) [1,2,3,4]
(101,[2,3,4])
```

Já sabemos muito bem como o `fmap` funciona com valores `Maybe`. E o `liftM` faz a mesma coisa. Para valores `Writer`, a função é mapeada sobre o primeiro componente da tupla, que é o resultado. Fazer `fmap` ou `liftM` sobre uma computação estatal resulta em outra computação estatal, apenas seu resultado eventual é modificado pela função fornecida. Se não tivéssemos mapeado `(+100)` sobre o `pop` neste caso antes de executá-lo, ele teria retornado `(1,[2,3,4])`.

É assim que o `liftM` é implementado:

```{.haskell:hs}
liftM :: (Monad m) => (a -> b) -> m a -> m b
liftM f m = m >>= (\x -> return (f x))
```

Ou com a notação `do`:

```{.haskell:hs}
liftM :: (Monad m) => (a -> b) -> m a -> m b
liftM f m = do
    x <- m
    return (f x)
```

Alimentamos o valor monádico `m` na função e então aplicamos a função `f` ao seu resultado antes de colocá-lo de volta em um contexto padrão. Por causa das leis das mônadas, isso garante não alterar o contexto, apenas o resultado que o valor monádico apresenta. Vemos que o `liftM` é implementado sem referenciar a typeclass `Functor` de forma alguma. Isso significa que podemos implementar o `fmap` (ou `liftM`, como você preferir chamar) apenas usando as utilidades que as mônadas nos oferecem. Por isso, podemos concluir que as mônadas são mais fortes que os simples e velhos functores.

A typeclass `Applicative` nos permite aplicar funções entre valores com contextos como se fossem valores normais. Assim:

```{.haskell:hs}
ghci> (+) <$> Just 3 <*> Just 5
Just 8
ghci> (+) <$> Just 3 <*> Nothing
Nothing
```

Usar este estilo aplicativo torna as coisas bem fáceis. O `<$>` é apenas o `fmap` e o `<*>` é uma função da typeclass `Applicative` que tem o seguinte tipo:

```{.haskell:hs}
(<*>) :: (Applicative f) => f (a -> b) -> f a -> f b
```

Portanto, é parecido com o `fmap`, só que a própria função está em um contexto. Temos que de alguma forma extraí-la do contexto e mapeá-la sobre o valor `f a` para depois montar o contexto novamente. Como todas as funções são curried no Haskell por padrão, podemos usar a combinação de `<$>` e `<*>` para aplicar funções que recebem vários parâmetros entre valores aplicativos.

Acontece que, assim como o `fmap`, o `<*>` também pode ser implementado usando apenas o que a typeclass `Monad` nos dá. A função `ap` é basicamente o `<*>`, só que tem uma restrição `Monad` em vez de uma `Applicative`. Aqui está a sua definição:

```{.haskell:hs}
ap :: (Monad m) => m (a -> b) -> m a -> m b
ap mf m = do
    f <- mf
    x <- m
    return (f x)
```

`mf` é um valor monádico cujo resultado é uma função. Como a função está em um contexto, assim como o valor, pegamos a função do contexto e a chamamos de `f`, depois pegamos o valor e o chamamos de `x` e então finalmente aplicamos a função ao valor e apresentamos isso como resultado. Aqui está uma demonstração rápida:

```{.haskell:hs}
ghci> Just (+3) <*> Just 4
Just 7
ghci> Just (+3) `ap` Just 4
Just 7
ghci> [(+1),(+2),(+3)] <*> [10,11]
[11,12,12,13,13,14]
ghci> [(+1),(+2),(+3)] `ap` [10,11]
[11,12,12,13,13,14]
```

Agora vemos que as mônadas também são mais fortes que os aplicativos, porque podemos usar as funções da `Monad` para implementar as da `Applicative`. Na verdade, muitas vezes, quando se descobre que um tipo é uma mônada, as pessoas primeiro escrevem uma instância de `Monad` e depois criam uma instância de `Applicative` apenas dizendo que o `pure` é o `return` e o `<*>` é o `ap`. Da mesma forma, se você já tem uma instância de `Monad` para algo, pode dar a ela uma instância de `Functor` apenas dizendo que o `fmap` é o `liftM`.

A função `liftA2` é uma função de conveniência para aplicar uma função entre dois valores aplicativos. Ela é definida simplesmente assim:

```{.haskell:hs}
liftA2 :: (Applicative f) => (a -> b -> c) -> f a -> f b -> f c
liftA2 f x y = f <$> x <*> y
```

A função `liftM2` faz a mesma coisa, só que tem uma restrição `Monad`. Também existem o `liftM3`, `liftM4` e `liftM5`.

Vimos como as mônadas são mais fortes que os aplicativos e functores e como, apesar de toda mônada ser um functor e um functor aplicativo, elas não têm necessariamente instâncias de `Functor` e `Applicative`, por isso examinamos os equivalentes monádicos das funções que os functores e functores aplicativos usam.

### A função join

Aqui está algo para se pensar: se o resultado de um valor monádico é outro valor monádico, ou seja, se um valor monádico está aninhado dentro de outro, você pode achatá-los para apenas um único valor monádico normal? Por exemplo, se tivermos `Just (Just 9)`, podemos transformar isso em `Just 9`? Acontece que qualquer valor monádico aninhado pode ser achatado e que essa é, na verdade, uma propriedade única das mônadas. Para isso, existe a função `join`. O seu tipo é este:

```{.haskell:hs}
join :: (Monad m) => m (m a) -> m a
```

Portanto, ela recebe um valor monádico dentro de um valor monádico e nos dá apenas um valor monádico; ou seja, ela o achata de certa forma. Aqui está ela com alguns valores `Maybe`:

```{.haskell:hs}
ghci> join (Just (Just 9))
Just 9
ghci> join (Just Nothing)
Nothing
ghci> join Nothing
Nothing
```

A primeira linha tem uma computação bem-sucedida como resultado de uma computação bem-sucedida, então ambas são apenas unidas em uma única computação bem-sucedida. A segunda linha apresenta um `Nothing` como resultado de um valor `Just`. Sempre que estávamos lidando com valores `Maybe` antes e queríamos combinar vários deles em um só, fosse com `<*>` ou `>>=`, todos tinham que ser valores `Just` para que o resultado fosse um valor `Just`. Se houvesse qualquer falha ao longo do caminho, o resultado era uma falha, e o mesmo acontece aqui. Na terceira linha, tentamos achatar o que é, desde o início, uma falha, de modo que o resultado é uma falha também.

Achatar listas é bem intuitivo:

```{.haskell:hs}
ghci> join [[1,2,3],[4,5,6]]
[1,2,3,4,5,6]
```

Como você pode ver, para listas, `join` é apenas `concat`. Para achatar um valor `Writer` cujo resultado é um valor `Writer` por si só, temos que usar o `mappend` no valor monóide.

```{.haskell:hs}
ghci> runWriter $ join (Writer (Writer (1,"aaa"),"bbb"))
(1,"bbbaaa")
```

O valor monóide externo `"bbb"` vem primeiro e depois `"aaa"` é anexado a éle. Intuitivamente falando, quando você quer examinar qual é o resultado de um valor `Writer`, você tem que escrever seu valor monóide no log primeiro e só então pode examinar o que ele tem dentro.

Achatar valores `Either` é muito semelhante a achatar valores `Maybe`:

```{.haskell:hs}
ghci> join (Right (Right 9)) :: Either String Int
Right 9
ghci> join (Right (Left "erro")) :: Either String Int
Left "erro"
ghci> join (Left "erro") :: Either String Int
Left "erro"
```

Se aplicarmos o `join` a uma computação estatal cujo resultado é uma computação estatal por si só, o resultado é uma computação estatal que executa primeiro a computação estatal externa e depois a resultante. Veja:

```{.haskell:hs}
ghci> runState (join (State $ \s -> (push 10,1:2:s))) [0,0,0]
((),[10,1,2,0,0,0])
```

O lambda aqui recebe um estado e coloca `2` e `1` na pilha e apresenta `push 10` como seu resultado. Portanto, quando tudo isso é achatado com `join` e depois executado, ele primeiro coloca `2` e `1` na pilha e depois o `push 10` é realizado, colocando um `10` no topo.

A implementação do `join` é a seguinte:

```{.haskell:hs}
join :: (Monad m) => m (m a) -> m a
join mm = do
    m <- mm
    m
```

Como o resultado de `mm` é um valor monádico, pegamos esse resultado e o colocamos em uma linha própria, pois se trata de um valor monádico. O truque aqui é que, quando fazemos `m <- mm`, o contexto da mônada em que estamos é cuidado. É por isso que, por exemplo, valores `Maybe` resultam em valores `Just` apenas se os valores externo e interno forem ambos valores `Just`. Aqui está como ficaria se o valor `mm` fosse definido antecipadamente como `Just (Just 8)`:

```{.haskell:hs}
joinedMaybes :: Maybe Int
joinedMaybes = do
    m <- Just (Just 8)
    m
```

![eu também sou um guarda também](assets/images/for-a-few-monads-more/tipi.png){.right width=253 height=379}

Talvez a coisa mais interessante sobre a `join` é que, para toda mônada, alimentar um valor monádico a uma função com `>>=` é a mesma coisa que apenas mapear essa função sobre o valor e depois usar o `join` para achatar o valor monádico aninhado resultante! Em outras palavras, `m >>= f` é sempre a mesma coisa que `join (fmap f m)`! Faz sentido quando você pensa sobre isso. Com o `>>=`, estamos sempre pensando em como alimentar um valor monádico para uma função que recebe um valor normal mas retorna um valor monádico. Se apenas mapearmos essa função sobre o valor monádico, teremos um valor monádico dentro de outro valor monádico. Por exemplo, digamos que tenhamos `Just 9` e a função `\x -> Just (x+1)`. Se mapearmos esta função sobre `Just 9`, ficaremos com `Just (Just 10)`.

O fato de `m >>= f` ser sempre igual a `join (fmap f m)` é muito útil se estivermos criando nossa própria instância de `Monad` para algum tipo, porque muitas vezes é mais fácil descobrir como achatar um valor monádico aninhado do que descobrir como implementar o `>>=`.

### filterM

A função `filter` é praticamente o pão de cada dia da programação Haskell (`map` sendo a manteiga). Ela recebe um predicado e uma lista para filtrar e então retorna uma nova lista onde apenas os elementos que satisfazem o predicado são mantidos. O seu tipo é este:

```{.haskell:hs}
filter :: (a -> Bool) -> [a] -> [a]
```

O predicado recebe um elemento da lista e retorna um valor `Bool`. Agora, e se o valor `Bool` que ele retornasse fosse na verdade um valor monádico? Uau! Ou seja, e se ele viesse com um contexto? Poderia funcionar? Por exemplo, e se cada valor `True` ou `False` que o predicado produzisse também tivesse um valor monóide acompanhante, como `["Aceitou o numero 5"]` ou `["3 e muito pequeno"]`? Parece que poderia funcionar. Se fosse esse o caso, esperaríamos que a lista resultante também viesse com um log de todos os valores de log que foram produzidos ao longo do caminho. Portanto, se o `Bool` que o predicado retornasse viesse com um contexto, esperaríamos que a lista final resultante também tivesse algum contexto anexado, caso contrário o contexto com o qual cada `Bool` veio seria perdido.

A função `filterM` de `Control.Monad` faz exatamente o que queremos! O seu tipo é este:

```{.haskell:hs}
filterM :: (Monad m) => (a -> m Bool) -> [a] -> m [a]
```

O predicado retorna um valor monádico cujo resultado é um `Bool`, mas como é um valor monádico, seu contexto pode ser qualquer coisa, desde uma falha possível até o não-determinismo e muito mais! Para garantir que o contexto seja refletido no resultado final, o resultado também é um valor monádico.

Vamos pegar uma lista e manter apenas os valores menores que 4. Para começar, usaremos apenas a função `filter` normal:

```{.haskell:hs}
ghci> filter (\x -> x < 4) [9,1,5,2,10,3]
[1,2,3]
```

Isso é bem fácil. Agora, vamos criar um predicado que, além de apresentar um resultado `True` ou `False`, também forneça um log do que fez. É claro que usaremos a mônada `Writer` para isso:

```{.haskell:hs}
keepSmall :: Int -> Writer [String] Bool
keepSmall x
    | x < 4 = do
        tell ["Mantendo " ++ show x]
        return True
    | otherwise = do
        tell [show x ++ " e muito grande, jogando fora"]
        return False
```

Em vez de apenas retornar um `Bool`, esta função retorna um `Writer [String] Bool`. É um predicado monádico. Parece sofisticado, não é? Se o número for menor que `4`, relatamos que o estamos mantendo e depois damos um `return True`.

Agora, vamos passá-lo para o `filterM` junto com uma lista. Como o predicado retorna um valor `Writer`, a lista resultante também será um valor `Writer`.

```{.haskell:hs}
ghci> fst $ runWriter $ filterM keepSmall [9,1,5,2,10,3]
[1,2,3]
```

Examinando o resultado do valor `Writer` resultante, vemos que está tudo em ordem. Agora, vamos imprimir o log e ver o que obtivemos:

```{.haskell:hs}
ghci> mapM_ putStrLn $ snd $ runWriter $ filterM keepSmall [9,1,5,2,10,3]
9 e muito grande, jogando fora
Mantendo 1
5 e muito grande, jogando fora
Mantendo 2
10 e muito grande, jogando fora
Mantendo 3
```

Incrível. Assim, apenas fornecendo um predicado monádico para o `filterM`, fomos capazes de filtrar uma lista aproveitando o contexto monádico que usamos.

Um truque de Haskell muito legal é usar `filterM` para obter o conjunto das partes (*powerset*) de uma lista (se pensarmos nela como conjuntos por enquanto). O conjunto das partes de algum conjunto é o conjunto de todos os subconjuntos desse conjunto. Portanto, se tivermos um conjunto como `[1,2,3]`, seu conjunto das partes incluiria os seguintes conjuntos:

```{.haskell:hs}
[1,2,3]
[1,2]
[1,3]
[1]
[2,3]
[2]
[3]
[]
```

Em outras palavras, obter um conjunto das partes é como obter todas as combinações de manter e descartar elementos de um conjunto. `[2,3]` é como o conjunto original, só que excluímos o número `1`.

Para fazer uma função que retorna o conjunto das partes de uma lista, vamos contar com o não-determinismo. Pegamos a lista `[1,2,3]` e então olhamos para o primeiro elemento, que é `1`, e nos perguntamos: devemos mantê-lo ou descartá-lo? Bem, gostaríamos de fazer ambos na verdade. Portanto, simularemos a filtragem de uma lista e usaremos um predicado que, de forma não-determinística, tanto mantém quanto descarta cada elemento da lista. Aqui está a nossa função `powerset`:

```{.haskell:hs}
powerset :: [a] -> [[a]]
powerset xs = filterM (\x -> [True, False]) xs
```

Espere, é só isso? Sim. Escolhemos descartar e manter cada elemento, independentemente de qual seja esse elemento. Temos um predicado não-determinístico, de modo que a lista resultante também será um valor não-determinístico e será, portanto, uma lista de listas. Vamos testar:

```{.haskell:hs}
ghci> powerset [1,2,3]
[[1,2,3],[1,2],[1,3],[1],[2,3],[2],[3],[]]
```

Isso exige um pouco de reflexão para entender, mas se você apenas considerar as listas como valores não-determinísticos que não sabem o que ser e então decidem ser tudo de uma vez, fica um pouco mais fácil.

### foldM

A contraparte monádica do `foldl` é o `foldM`. Se você se lembra do seu `foldl`, ele recebia uma função binária, um acumulador inicial e uma lista para dobrar e então reduzia a lista a um único valor dobrando a função binária sobre a lista a partir da esquerda. O `foldM` faz a mesma coisa, só que recebe uma função binária que produz um resultado monádico e então dobra a lista com essa função. O seu tipo é este:

```{.haskell:hs}
foldM :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m a
```

O valor retornado pela função binária é monádico e, portanto, o resultado de toda a dobra também é monádico. Vamos pegar uma lista e dobrá-la, mas em vez de apenas somar os números, vamos garantir que a soma nunca ultrapasse `100`. Se a soma ultrapassar `100`, falharemos.

```{.haskell:hs}
binSmalls :: Int -> Int -> Maybe Int
binSmalls acc x
    | x > 9 = Nothing
    | otherwise = Just (acc + x)
```

Nossa função binária recebe um acumulador e um elemento da lista e, se o elemento for maior que `9`, ela falha retornando `Nothing`, caso contrário, ela retorna a soma envolta em um `Just`. Agora, vamos usá-la com o `foldM`:

```{.haskell:hs}
ghci> foldM binSmalls 0 [2,8,3,1]
Just 14
ghci> foldM binSmalls 0 [2,11,3,1]
Nothing
```

Se qualquer etapa da dobra falhar, todo o `foldM` resulta em uma falha.

Logicamente, se usarmos um `Writer` como a mônada para o `foldM`, o log resultante conterá tudo o que foi registrado em cada etapa da dobra. E se usarmos uma lista como a mônada, o `foldM` pode ser usado para dobras não-determinísticas, o que é bem legal.

## Criando uma calculadora RPN segura {#safe-rpn-calculator}

Quando resolvemos o problema de [implementar uma calculadora RPN](functionally-solving-problems.html#reverse-polish-notation-calculator), mencionamos que ela funcionava bem desde que a entrada fizesse sentido. No entanto, se algo desse errado, todo o nosso programa falharia. Agora que aprendemos sobre as mônadas e o `Maybe` em particular, vamos ver como podemos adicionar tratamento de erro à nossa calculadora RPN.

Nós a implementamos fazendo um `foldl` sobre uma lista de itens. No início, a pilha estava vazia e cada item da entrada era processado um por um. Se fosse um número, ele era empilhado; se fosse um operador, os dois números do topo eram desempilhados e opados e o resultado era empilhado de volta.

Lembre-se da nossa implementação original:

```{.haskell:hs}
import Data.List

solveRPN :: String -> Double
solveRPN = head . foldl foldingFunction [] . words
```

E a função de dobra:

```{.haskell:hs}
foldingFunction :: [Double] -> String -> [Double]
foldingFunction (x:y:ys) "*" = (x * y):ys
foldingFunction (x:y:ys) "+" = (x + y):ys
foldingFunction (x:y:ys) "-" = (y - x):ys
foldingFunction xs numberString = read numberString:xs
```

Dobragem da esquerda para a direita, acumulador sendo a pilha. Agora, vamos torná-la segura! Primeiro, vamos fazer de modo que a `foldingFunction` possa falhar graciosamente. O seu tipo mudará de `[Double] -> String -> [Double]` para `[Double] -> String -> Maybe [Double]`. Isso significa que ela retornará `Nothing` se algo der errado ou `Just novaPilha` se tudo correr bem.

A função `read`, que usamos para converter strings em números, falha e trava o nosso programa se a string não for um número válido. Para corrigir isso, usaremos `reads`, que é como o `read`, só que retorna uma lista com o resultado da leitura e o resto da string se a leitura foi bem-sucedida ou uma lista vazia se falhou. Podemos usá-la para fazer uma função que tenta ler uma string como um número e, se conseguir, retorna um `Just numero`:

```{.haskell:hs}
readMaybe :: (Read a) => String -> Maybe a
readMaybe st = case reads st of [(x,"")] -> Just x
                                _ -> Nothing
```

Vamos testar:

```{.haskell:hs}
ghci> readMaybe "1" :: Maybe Int
Just 1
ghci> readMaybe "GO TO HELL" :: Maybe Int
Nothing
```

Parece que funciona. Agora, vamos reescrever nossa `foldingFunction` para ser uma função monádica:

```{.haskell:hs}
foldingFunction :: [Double] -> String -> Maybe [Double]
foldingFunction (x:y:ys) "*" = return ((x * y):ys)
foldingFunction (x:y:ys) "+" = return ((x + y):ys)
foldingFunction (x:y:ys) "-" = return ((y - x):ys)
foldingFunction xs numberString = liftM (:xs) (readMaybe numberString)
```

As três primeiras linhas, que cuidam dos operadores, agora retornam resultados monádicos (usamos o `return` para envolver a nova pilha em um `Just`). A última linha tenta ler a string como um número e, se conseguir, coloca esse número no topo da pilha. Usamos o `liftM` para aplicar o operador de construção de lista ao resultado de `readMaybe`.

Agora que temos uma função de dobra monádica, usaremos o `foldM` em vez do `foldl`. Veja como fica:

```{.haskell:hs}
solveRPN :: String -> Maybe Double
solveRPN st = do
    [result] <- foldM foldingFunction [] (words st)
    return result
```

Usamos a notação `do` para obter o resultado da dobra monádica. Como a dobra retorna um `Maybe [Double]`, se tudo correu bem, terminamos com uma lista contendo um único elemento, que é o resultado. Usamos o pattern matching `[result]` para extrair esse elemento. Se a dobra resultar em um `Nothing`, o resultado geral também será um `Nothing`. Se o resultado da dobra for uma lista que não tem exatamente um elemento, o pattern match falhará e a mônada `Maybe` cuidará disso para nós, retornando um `Nothing`.

```{.haskell:hs}
ghci> solveRPN "1 2 * 4 +"
Just 6.0
ghci> solveRPN "1 2 * 4 + 5 *"
Just 30.0
ghci> solveRPN "1 2 * 4"
Nothing
ghci> solveRPN "1 8 blabla"
Nothing
```

A primeira falha acontece porque a pilha final tinha dois elementos (`[4.0,2.0]`), então o pattern match falhou. A segunda falha acontece porque "blabla" não é um número.

Nossa calculadora RPN agora é muito mais robusta! Ela pode lidar com erros de entrada sem explodir. E tudo isso graças ao `foldM` e ao contexto de falha possível oferecido pela mônada `Maybe`.

## Composição de funções monádicas {#composing-monadic-functions}

Ao aprender sobre as leis das mônadas, aprendemos sobre a função `<=<`, que é como a composição normal de funções, porém em vez de funcionar com funções normais do tipo `a -> b`, ela funciona com funções monádicas do tipo `a -> m b`. Por exemplo:

```{.haskell:hs}
ghci> let f = (+1) . (*100)
ghci> f 4
401
ghci> let g = (\x -> return (x+1)) <=< (\x -> return (x*100))
ghci> Just 4 >>= g
Just 401
```

Neste exemplo, primeiro compusemos duas funções normais, aplicamos o resultado a `4` e depois fizemos o mesmo com funções monádicas usando `<=<` e alimentamos o resultado monádico com `>>=`.

Se tivermos um monte de funções em uma lista, podemos compô-las todas em uma única função gigante usando o `foldr` e a função de composição normal.

```{.haskell:hs}
ghci> let f = foldr (.) id [(+1),(*100),(+5)]
ghci> f 1
106
```

A função `f` recebe um número, soma `5` a ele, multiplica o resultado por `100` e depois soma `1`. Podemos fazer o mesmo para funções monádicas, só que usamos o `<=<` em vez de `.` e o `return` em vez de `id`. Não precisamos nem mesmo de uma contraparte monádica do `foldr`, porque o `foldr` funciona para qualquer tipo, e o tipo das nossas funções monádicas é `a -> m a`.

```{.haskell:hs}
ghci> let f = foldr (<=<) return [(\x -> [x,x]),(\x -> [x,x,x])]
ghci> f 4
[4,4,4,4,4,4]
```

A função `f` recebe um número e cria uma lista que o contém três vezes, e depois essa lista é alimentada na função que duplica os elementos.

No capítulo sobre as mônadas, usamos esta técnica de compor muitas funções monádicas para resolver o problema de se o nosso cavalo de xadrez poderia chegar a uma certa posição em três movimentos. Lá, tínhamos uma função chamada `moveKnight`, que retornava todas as posições possíveis onde o cavalo poderia estar após um movimento. Então, para calcular todas as posições possíveis após três movimentos, criamos a seguinte função:

```{.haskell:hs}
in3 start = return start >>= moveKnight >>= moveKnight >>= moveKnight
```

Para verificar se ele poderia chegar a uma posição `end` começando em `start`, apenas verificávamos se `end` estava na lista resultante.

Agora, e se quiséssemos fazer uma função que nos dissesse se o cavalo pode chegar em `n` movimentos? Gostaríamos de uma função que recebesse `n` e retornasse uma função monádica como `moveKnight`, mas que em vez de um movimento, fizesse `n` movimentos. O `foldr` e o `<=<` são perfeitos para isso:

```{.haskell:hs}
import Control.Monad

canReachIn :: Int -> KnightPos -> KnightPos -> Bool
canReachIn n start end = end `elem` res
    where res = res (foldr (<=<) return (replicate n moveKnight)) start
```

Primeiro usamos o `replicate` para criar uma lista de tamanho `n` contendo a função `moveKnight`. Em seguida, compusemos todas essas funções monádicas em uma só e aplicamos a posição inicial à função resultante.

## Criando mônadas {#making-monads}

Nesta seção, vamos ver um exemplo de como um tipo é criado, identificado como um functor e, em seguida, recebe instâncias de `Applicative` e `Monad`.

Digamos que queiramos modelar valores não-determinísticos como listas, mas queiramos deixar claro que alguns resultados são mais prováveis que outros. Se pensarmos que `[3,5,9]` é um valor não-determinístico, poderíamos vê-lo como um valor que é `3`, `5` e `9` ao mesmo tempo. Mas e se quiséssemos dizer que há 50% de chance de ele ser `3` e 25% de chance de ser `5` ou `9`?

![probabilidade](assets/images/for-a-few-monads-more/dice.png){.left width=200 height=200}

Para modelar isso, representaremos um valor com sua probabilidade como um par, onde o primeiro componente do par é o valor e o segundo é a probabilidade. Para representar uma lista de probabilidades, basta termos uma lista de tais pares.

```{.haskell:hs}
[(3,0.5),(5,0.25),(9,0.25)]
```

Em Haskell, as probabilidades são representadas por números de ponto flutuante, mas os números racionais (como os do módulo `Data.Ratio`) são muito adequados para probabilidades porque são precisos e não apresentam os erros de arredondamento inerentes aos `Double`. Por isso, usaremos `Rational`.

```{.haskell:hs}
import Data.Ratio

ghci> 1%4
1 % 4
ghci> 1%2 + 1%2
1 % 1
ghci> 1%3 + 5%4
19 % 12
```

Como já temos listas para representar o não-determinismo, e o que estamos fazendo aqui é apenas adicionar uma probabilidade a cada item da lista, vamos dar um nome ao nosso novo tipo.

```{.haskell:hs}
newtype Prob a = Prob { getProb :: [(a,Rational)] } deriving Show
```

Tudo bem, este é o nosso tipo. Ele é um functor? Bem, a lista é um functor, então este provavelmente também é, já que apenas adicionamos algo aos elementos da lista. Quando mapeamos uma função sobre uma lista, nós a aplicamos a cada elemento. Aqui, faremos o mesmo, mas manteremos a probabilidade intacta.

```{.haskell:hs}
instance Functor Prob where
    fmap f (Prob xs) = Prob $ map (\(x,p) -> (f x,p)) xs
```

Mapeamos a função sobre os valores, deixando as probabilidades como estão. Vamos ver se funciona:

```{.haskell:hs}
ghci> fmap negate (Prob [(3,1%2),(5,1%4),(9,1%4)])
Prob {getProb = [(-3,1 % 2),(-5,1 % 4),(-9,1 % 4)]}
```

Agora, e quanto à sua instância de `Monad`? Bem, antes de podermos torná-la uma `Monad`, ela também precisa ser um functor aplicativo. Como já aprendemos, poderíamos apenas usar o `return` e o `ap` para isso, mas como vamos implementar as funções da mônada de qualquer maneira, vamos fazê-lo agora.

O `return` é fácil. Ele tem que pegar um valor e colocá-lo em um contexto mínimo. O que seria uma probabilidade mínima para um valor? Se o valor tem que ser exatamente esse valor, sua probabilidade deve ser `1` (ou seja, 100%).

```{.haskell:hs}
return x = Prob [(x,1%1)]
```

E o `>>=`? O `>>=` recebe um valor monádico e uma função que retorna um valor monádico e tem que nos dar um novo valor monádico. No nosso caso, o valor monádico é uma lista de resultados com probabilidades. A função pegará um resultado e o transformará em outra lista de resultados com probabilidades.

Considere este exemplo: temos um valor que tem 25% de chance de ser `'a'` e 75% de chance de ser `'b'`. Se for `'a'`, há 10% de chance de que ele se torne `1` e 90% de chance de se tornar `2`. Se for `'b'`, há 50% de chance de se tornar tanto `3` quanto `4`. Qual é a probabilidade total de obter cada um destes números?

Para descobrir isso, só temos que multiplicar as probabilidades. Se tivermos 25% de chance de obter `'a'`, ficaremos com 2,5% (0,25 * 0,10) de chance de obter `1`. O mesmo vale para o restante.

O `>>=` para a nossa mônada de probabilidade será agora fácil. Primeiro mapeamos a função sobre o nosso valor `Prob a`. Isso nos dá um `Prob (Prob b)`. Se apenas achatar isso, ou seja, se calcularmos o `join` dele, teremos o nosso resultado.

Mas antes de escrever o `>>=`, vamos escrever o `flatten`, que achata um `Prob (Prob a)` em um `Prob a`.

```{.haskell:hs}
flatten :: Prob (Prob a) -> Prob a
flatten (Prob xs) = Prob $ concat $ map multAll xs
    where multAll (Prob innerxs,p) = map (\(x,r) -> (x,p*r)) innerxs
```

Recebemos uma lista de probabilidades de probabilidades (`xs`). Para cada uma delas, chamamos o par `(Prob innerxs, p)`. Então, multiplicamos a probabilidade `p` por cada uma das probabilidades internas em `innerxs` e retornamos essa lista. Ao final, apenas concatenamos tudo isso em uma única lista grande.

Agora podemos escrever a nossa instância de `Monad`:

```{.haskell:hs}
instance Monad Prob where
    return x = Prob [(x,1%1)]
    m >>= f = flatten (fmap f m)
    fail _ = Prob []
```

Como já tínhamos o `flatten`, o `>>=` foi apenas uma questão de usá-lo junto com o `fmap`, conforme aprendemos anteriormente. Agora que temos a mônada, vamos ver o que podemos fazer com ela!

Digamos que tenhamos duas moedas viciadas. Uma tem 10% de chance de dar cara e a outra tem 75% de chance. Se jogarmos ambas, qual a probabilidade de ambas darem cara? Primeiro, vamos representar as moedas:

```{.haskell:hs}
data Coin = Heads | Tails deriving (Show, Eq)

coin :: Prob Coin
coin = Prob [(Heads,1%10),(Tails,9%10)]

loadedCoin :: Prob Coin
loadedCoin = Prob [(Heads,1%2),(Tails,1%2)]
```

E agora, o lançamento:

```{.haskell:hs}
import Data.List (all)

flipTwo :: Prob Bool
flipTwo = do
    a <- coin
    b <- loadedCoin
    return (all (==Heads) [a,b])
```

Vamos ver o resultado:

```{.haskell:hs}
ghci> getProb flipTwo
[(True,1 % 20),(False,1 % 20),(False,9 % 20),(False,9 % 20)]
```

Se somarmos todos os resultados `False`, veremos que a probabilidade de não obtermos duas caras é de 19/20, que é 95%. Portanto, a probabilidade de ambas serem cara é de 5%.

Vimos como as mônadas podem nos ajudar a modelar problemas complexos de uma maneira que parece muito natural e elegante. O poder de abstração das mônadas nos permite focar na lógica do nosso problema enquanto o contexto (seja ele falha, estado, log ou probabilidade) é tratado automaticamente por baixo dos panos.

Esperamos que, depois deste capítulo (e do anterior), você tenha uma compreensão sólida do que as mônadas são, de como elas funcionam e de por que elas são tão importantes no mundo do Haskell. Elas não são tão assustadoras quanto parecem à primeira vista, e assim que você começa a usá-las, fica difícil imaginar a programação funcional sem elas!











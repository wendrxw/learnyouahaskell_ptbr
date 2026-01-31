# Entrada e Saída (Input and Output)

![poor dog](assets/images/input-and-output/dognap.png){.right width=261 height=382}

Já mencionamos que Haskell é uma linguagem puramente funcional.
Considerando que, em linguagens imperativas, você geralmente faz as coisas dando ao computador uma série de etapas para executar, a programação funcional é mais sobre definir o que as coisas são.
Em Haskell, uma função não pode alterar algum estado, como alterar o conteúdo de uma variável (quando uma função muda de estado, dizemos que a função tem *efeitos colaterais* ou *side-effects*).
A única coisa que uma função pode fazer em Haskell é nos devolver algum resultado com base nos parâmetros que demos a ela.
Se uma função for chamada duas vezes com os mesmos parâmetros, ela deverá retornar o mesmo resultado.
Embora isso possa parecer um pouco limitador quando você vem de um mundo imperativo, vimos que é realmente muito legal.
Em uma linguagem imperativa, você não tem garantia de que uma função simples que deve apenas processar alguns números não queimará sua casa, sequestrará seu cachorro e arranhará seu carro com uma batata enquanto processa esses números.
Por exemplo, quando estávamos fazendo uma árvore de busca binária, não inserimos um elemento em uma árvore modificando alguma árvore no lugar.
Nossa função para inserir em uma árvore de busca binária realmente retornou uma nova árvore, porque não pode mudar a antiga.

Embora as funções serem incapazes de mudar o estado seja bom, porque nos ajuda a raciocinar sobre nossos programas, há um problema com isso.
Se uma função não pode mudar nada no mundo, como é que ela deve nos dizer o que calculou?
Para nos dizer o que calculou, ela tem que mudar o estado de um dispositivo de saída (geralmente o estado da tela), que então emite fótons que viajam para o nosso cérebro e mudam o estado de nossa mente, cara.

Não se desespere, nem tudo está perdido.
Acontece que Haskell realmente tem um sistema muito inteligente para lidar com funções que têm efeitos colaterais que separam perfeitamente a parte de nosso programa que é pura e a parte do nosso programa que é impura, que faz todo o trabalho sujo, como conversar com o teclado e a tela.
Com essas duas partes separadas, ainda podemos raciocinar sobre nosso programa puro e tirar proveito de todas as coisas que a pureza oferece, como preguiça, robustez e modularidade, enquanto nos comunicamos eficientemente com o mundo exterior.

## Olá, mundo! (Hello, world!) {#hello-world}

![HELLO!](assets/images/input-and-output/helloworld.png){.left width=223 height=179}

Até agora, sempre carregamos nossas funções no GHCI para testá-las e brincar com elas.
Também exploramos as funções da biblioteca padrão dessa maneira.
Mas agora, depois de oito ou mais capítulos, finalmente escreveremos nosso primeiro programa Haskell *real*!
Yay!
E com certeza, vamos fazer o bom e velho esquema `"hello, world"`.

::: {.hintbox}
**Ei!**
Para os fins deste capítulo, vou assumir que você está usando um ambiente unix-y para aprender Haskell.
Se você estiver no Windows, sugiro que faça o download do [Cygwin](https://www.cygwin.com/), que é um ambiente semelhante ao Linux para Windows, A.k.A. exatamente o que você precisa.
:::

Portanto, para começar, coloque o seguinte em seu editor de texto favorito:

```{.haskell:hs}
main = putStrLn "hello, world"
```

Acabamos de definir um nome chamado `main` e nele chamamos uma função chamada `putStrLn` com o parâmetro `"hello, world"`.
Parece bastante comum, mas não é, como veremos em apenas alguns instantes.
Salve esse arquivo como `helloworld.hs`.

E agora, vamos fazer algo que nunca fizemos antes.
Na verdade, vamos compilar nosso programa!
Estou tão animado!
Abra seu terminal e navegue até o diretório onde `helloworld.hs` está localizado e faça o seguinte:

```{.plain}
$ ghc --make helloworld
[1 of 1] Compiling Main             ( helloworld.hs, helloworld.o )
Linking helloworld ...
```

Ok!
Com alguma sorte, você conseguiu algo assim e agora pode executar seu programa fazendo `./helloworld`.

```{.haskell:hs}
$ ./helloworld
hello, world
```

E lá vamos nós, nosso primeiro programa compilado que imprimiu algo no terminal.
Quão extraordinariamente chato!

Vamos examinar o que escrevemos.
Primeiro, vamos ver o tipo da função `putStrLn`.

```{.haskell:hs}
ghci> :t putStrLn
putStrLn :: String -> IO ()
ghci> :t putStrLn "hello, world"
putStrLn "hello, world" :: IO ()
```

Podemos ler o tipo de `putStrLn` assim: `putStrLn` pega uma string e retorna uma **ação de E/S (I/O action)** que tem um tipo de resultado de `()` (ou seja, a tupla vazia, também conhecida como `unit`).
Uma ação de E/S é algo que, quando realizado, realizará uma ação com um efeito colateral (geralmente lendo da entrada ou imprimindo coisas na tela) e também conterá algum tipo de valor de retorno dentro dele.
A impressão de uma string no terminal realmente não possui nenhum tipo de valor de retorno significativo, portanto, um valor fictício de `()` é usado.

::: {.hintbox}
A tupla vazia é um valor de `()` e também tem um tipo de `()`.
:::

Então, quando uma ação de E/S será executada?
Bem, é aqui que `main` entra.
Uma ação de E/S será realizada quando dermos um nome de `main` e executarmos nosso programa.

Ter todo o seu programa sendo apenas uma ação de E/S parece meio limitador.
É por isso que podemos usar a sintaxe *do* para colar várias ações de E/S em uma.
Dê uma olhada no seguinte exemplo:

```{.haskell:hs}
main = do
    putStrLn "Hello, what's your name?"
    name <- getLine
    putStrLn ("Hey " ++ name ++ ", you rock!")
```

Ah, interessante, nova sintaxe!
E isso parece muito com um programa imperativo.
Se você compilá-lo e testá-lo, provavelmente se comportará exatamente como você espera.
Observe que dissemos *do* e depois definimos uma série de etapas, como faríamos em um programa imperativo.
Cada uma dessas etapas é uma ação de E/S.
Ao juntá-los com a sintaxe *do*, nós os colamos em uma ação de E/S.
A ação que obtivemos tem um tipo de `IO ()`, porque esse é o tipo da última ação de E/S dentro.

Por esse motivo, `main` sempre tem uma assinatura de tipo de <code>main :: IO *something*</code>, onde <code>*something*</code> é algum tipo concreto.
Por convenção, geralmente não especificamos uma declaração de tipo para `main`.

Uma coisa interessante que não encontramos antes é a terceira linha, que afirma `name <- getLine`.
Parece que lê uma linha da entrada e a armazena em uma variável chamada `name`.
Realmente?
Bem, vamos examinar o tipo de `getLine`.

```{.haskell:hs}
ghci> :t getLine
getLine :: IO String
```

![luggage](assets/images/input-and-output/luggage.png){.left width=204 height=200}

Aha, o-kay.
`getLine` é uma ação de E/S que contém um tipo de resultado de `String`.
Isso faz sentido, porque aguardará o usuário inserir algo no terminal e, em seguida, esse algo será representado como uma string.
Então, o que há com `name <- getLine` então?
Você pode ler esse pedaço de código assim: **execute a ação de E/S `getLine` e vincule seu valor de resultado a `name`**.
`getLine` tem um tipo de `IO String`, então `name` terá um tipo de `String`.
Você pode pensar em uma ação de E/S como uma caixa com pezinhos que sairão para o mundo real e farão algo lá (como escrever algum grafite em uma parede) e talvez trazer de volta alguns dados.
Depois de buscar esses dados para você, a única maneira de abrir a caixa e obter os dados dentro dela é usar a construção `<-`.
E se estamos tirando dados de uma ação de E/S, só podemos retirá-los quando estamos dentro de outra ação de E/S.
É assim que Haskell consegue separar perfeitamente as partes puras e impuras do nosso código.
`getLine` é, em certo sentido, impuro porque seu valor de resultado não é garantido ser o mesmo quando executado duas vezes.
É por isso que é uma espécie de *contaminado* (*tainted*) com o construtor de tipo `IO` e só podemos extrair esses dados no código de E/S.
E como o código de E/S também está contaminado, qualquer computação que depende de dados de E/S contaminados terá um resultado contaminado.

Quando digo *contaminado*, não quero dizer contaminado de tal maneira que nunca possamos usar o resultado contido em uma ação de E/S novamente em código puro.
Não, nós temporariamente *descontaminamos* os dados dentro de uma ação de E/S quando os vinculamos a um nome.
Quando fazemos `name <- getLine`, `name` é apenas uma string normal, porque representa o que está dentro da caixa.
Podemos ter uma função realmente complicada que, digamos, leva seu nome (uma string normal) como um parâmetro e diz sua sorte e todo o futuro da sua vida com base em seu nome.
Nós podemos fazer isso:

```{.haskell:hs}
main = do
    putStrLn "Hello, what's your name?"
    name <- getLine
    putStrLn $ "Read this carefully, because this is your future: " ++ tellFortune name
```

e `tellFortune` (ou qualquer uma das funções para as quais passa `name`) não precisa saber nada sobre E/S, é apenas uma função normal `String -> String`!

Dê uma olhada neste trecho de código.
É válido?

```{.haskell:hs}
nameTag = "Hello, my name is " ++ getLine
```

Se você disse não, vá comer um biscoito.
Se você disse sim, beba uma tigela de lava derretida.
Brincadeira, não!
A razão pela qual isso não funciona é que `++` exige que seus dois parâmetros sejam listas sobre o mesmo tipo.
O parâmetro esquerdo tem um tipo de `String` (ou `[Char]`, se quiser), enquanto `getLine` tem um tipo de `IO String`.
Você não pode concatenar uma string e uma ação de E/S.
Primeiro temos que tirar o resultado da ação de E/S para obter um valor do tipo `String` e a única maneira de fazer isso é dizer algo como `name <- getLine` dentro de outra ação de E/S.
Se queremos lidar com dados impuros, temos que fazê-lo em um ambiente impuro.
Portanto, a mancha da impureza se espalha muito como o flagelo dos mortos-vivos (undead scourge) e é do nosso interesse manter as partes de E/S do nosso código o menor possível.

Toda ação de E/S que é executada tem um resultado encapsulado nela.
É por isso que nosso programa de exemplo anterior também poderia ter sido escrito assim:

```{.haskell:hs}
main = do
    foo <- putStrLn "Hello, what's your name?"
    name <- getLine
    putStrLn ("Hey " ++ name ++ ", you rock!")
```

No entanto, `foo` teria apenas um valor de `()`, portanto, fazer isso seria meio inútil.
Observe que não vinculamos o último `putStrLn` a nada.
Isso ocorre porque em um bloco *do*, **a última ação não pode ser vinculada a um nome** como os dois primeiros foram.
Veremos exatamente por que isso é um pouco mais tarde, quando nos aventurarmos no mundo das mônadas.
Por enquanto, você pode pensar nisso da maneira que o bloco *do* extrai automaticamente o valor da última ação e o liga ao seu próprio resultado.

Exceto a última linha, todas as linhas em um bloco *do* que não se ligam também podem ser escritas com um *bind*.
Então `putStrLn "BLAH"` pode ser escrito como `_ <- putStrLn "BLAH"`.
Mas isso é inútil, então deixamos o `<-` para ações de E/S que não contêm um resultado importante, como <code>putStrLn *something*</code>.

Os iniciantes às vezes pensam que fazer

```{.haskell:hs}
name = getLine
```

lerá a entrada e depois ligará o valor de `name`.
Bem, isso não acontecerá, tudo o que isso faz é dar à ação de E/S `getLine` um nome diferente chamado, bem, `name`.
Lembre-se, para obter o valor de uma ação de E/S, você deve executá-la dentro de outra ação de E/S, ligando-a a um nome com `<-`.

As ações de E/S serão executadas apenas quando receberem um nome de `main` ou quando estiverem dentro de uma ação de E/S maior que compusemos com um bloco *do*.
Também podemos usar um bloco *do* para colar algumas ações de E/S e, em seguida, podemos usar essa ação de E/S em outro bloco *do* e assim por diante.
De qualquer maneira, elas serão executadas apenas se eventualmente caírem no `main`.

Ah, certo, também há mais um caso em que as ações de E/S serão executadas.
Quando digitamos uma ação de E/S no GHCI e pressionamos enter, ela será executada.

```{.haskell:hs}
ghci> putStrLn "HEEY"
HEEY
```

Mesmo quando apenas digitamos um número ou chamamos uma função no GHCI e pressionamos enter, ele o avaliará (tanto quanto necessário) e, em seguida, chamará `show` nele e depois imprimirá essa string no terminal usando `putStrLn` implicitamente.

Lembra das ligações *let*?
Se você não, atualize sua memória sobre elas lendo [esta seção](syntax-in-functions.html#let-it-be).
Elas precisam ter a forma de <code>let *bindings* in *expression*</code>, onde <code>*bindings*</code> são nomes a serem dados às expressões e <code>*expression*</code> é a expressão que deve ser avaliada que as vê.
Também dissemos que, nas compreesões de lista, a parte *in* não é necessária.
Bem, você pode usá-las em blocos *do* exatamente como você as usa nas compreensões de lista.
Verifique isso:

```{.haskell:hs}
import Data.Char

main = do
    putStrLn "What's your first name?"
    firstName <- getLine
    putStrLn "What's your last name?"
    lastName <- getLine
    let bigFirstName = map toUpper firstName
        bigLastName = map toUpper lastName
    putStrLn $ "hey " ++ bigFirstName ++ " " ++ bigLastName ++ ", how are you?"
```

Veja como as ações de E/S no bloco *do* estão alinhadas?
Observe também como o *let* está alinhado com as ações de E/S e os nomes do *let* estão alinhados um com o outro?
Essa é uma boa prática, porque a indentação é importante em Haskell.
Agora, fizemos `map toUpper firstName`, o que transforma algo como `"John"` em uma string muito mais legal como `"JOHN"`.
Vinculamos essa string maiúscula a um nome e depois a usamos em uma string posteriormente impressa no terminal.

Você deve estar se perguntando quando usar `<-` e quando usar *let* bindings?
Bem, lembre-se, `<-` é (por enquanto) para executar ações de E/S e vincular seus resultados a nomes.
`map toUpper firstName`, no entanto, não é uma ação de E/S.
É uma expressão pura em Haskell.
Então use `<-` quando quiser vincular os resultados das ações de E/S aos nomes e você pode usar *let* bindings para ligar expressões puras aos nomes.
Se tivéssemos feito algo como `let firstName = getLine`, teríamos apenas chamado a ação de E/S `getLine` de um nome diferente e ainda teríamos que executá-la através de um `<-` para executá-la.

Agora vamos fazer um programa que lê continuamente uma linha e imprime a mesma linha com as palavras invertidas.
A execução do programa parará quando inserirmos uma linha em branco.
Este é o programa:

```{.haskell:hs}
main = do
    line <- getLine
    if null line
        then return ()
        else do
            putStrLn $ reverseWords line
            main

reverseWords :: String -> String
reverseWords = unwords . map reverse . words
```

Para sentir o que ele faz, você pode executá-lo antes de examinarmos o código.

::: {.hintbox}
**Dica profissional**: Para executar um programa, você pode compilá-lo e executar o arquivo executável produzido fazendo `ghc --make helloworld` e depois `./helloworld` ou pode usar o comando `runhaskell` assim: `runhaskell helloworld.hs` e seu programa será executado em tempo real (*on the fly*).
:::

Primeiro, vamos dar uma olhada na função `reverseWords`.
É apenas uma função normal que pega uma string como `"hey there man"` e depois chama `words` com ela para produzir uma lista de palavras como `["hey","there","man"]`.
Em seguida, mapeamos `reverse` na lista, obtendo `["yeh","ereht","nam"]` e depois colocamos isso de volta em uma string usando `unwords` e o resultado final é `"yeh ereht nam"`.
Veja como usamos a composição de funções aqui.
Sem composição de funções, teríamos que escrever algo como `reverseWords st = unwords (map reverse (words st))`.

E sobre o `main`?
Primeiro, recebemos uma linha do terminal executando `getLine` e chamamos essa linha de `line`.
E agora, temos uma expressão condicional.
Lembre-se de que em Haskell, todo *if* deve ter um *else* correspondente, porque toda expressão precisa ter algum tipo de valor.
Fazemos o *if* para que, quando uma condição seja verdadeira (no nosso caso, a linha em que entramos está em branco), executamos uma ação de E/S e, quando não é, a ação de E/S no *else* é executada.
É por isso que em um bloco *do* de E/S, *if*s devem ter uma forma de <code>if *condition* then *I/O action* else *I/O action*.</code>

Vamos primeiro dar uma olhada no que acontece sob a cláusula *else*.
Porque, temos que ter exatamente uma ação de E/S após o *else*, usamos um bloco *do* para colar duas ações de E/S em uma.
Você também pode escrever essa parte como:

```{.haskell:hs}
else (do
    putStrLn $ reverseWords line
    main)
```

Isso torna mais explícito que o bloco *do* pode ser visto como uma ação de E/S, mas é mais feio.
De qualquer forma, dentro do bloco *do*, chamamos `reverseWords` na linha que obtivemos de `getLine` e depois imprimimos no terminal.
Depois disso, apenas executamos `main`.
É chamado recursivamente e tudo bem, porque `main` é uma ação de E/S.
Então, em certo sentido, voltamos ao início do programa.

Agora, o que acontece quando `null line` é verdadeiro?
O que está depois do *then* é realizado nesse caso.
Se olharmos para cima, veremos que diz `then return ()`.
Se você já fez linguagens imperativas como C, Java ou Python, provavelmente está pensando que sabe o que esse `return` faz e é provável que você já tenha pulado esse parágrafo realmente longo.
Bem, aqui está: **o `return` em Haskell não é nada como o `return` na maioria das outras linguagens!**
Tem o mesmo nome, o que confunde muitas pessoas, mas na realidade é bem diferente.
Em linguagens imperativas, `return` geralmente encerra a execução de um método ou sub-rotina e faz com que ele relate algum tipo de valor a quem o chamou.
Em Haskell (em ações de E/S especificamente), ele faz uma ação de E/S a partir de um valor puro.
Se você pensar na analogia da caixa de antes, é preciso um valor e a embrulha em uma caixa.
A ação de E/S resultante não faz nada, apenas tem esse valor encapsulado como seu resultado.
Portanto, em um contexto de E/S, `return "haha"` terá um tipo de `IO String`.
Qual é o ponto de apenas transformar um valor puro em uma ação de E/S que não faz nada?
Por que incomodar nosso programa com `IO` mais do que é necessário?
Bem, precisávamos de alguma ação de E/S para realizar no caso de uma linha de entrada vazia.
É por isso que acabamos de fazer uma ação de E/S falsa que não faz nada escrevendo `return ()`.

O uso do `return` não faz com que o bloco *do* de E/S termine em execução ou algo assim.
Por exemplo, este programa realizará tudo alegremente até a última linha:

```{.haskell:hs}
main = do
    return ()
    return "HAHAHA"
    line <- getLine
    return "BLAH BLAH BLAH"
    return 4
    putStrLn line
```

Tudo o que esses `return`s fazem é criar ações de E/S que realmente não fazem nada, exceto ter um resultado encapsulado e esse resultado é jogado fora porque não está vinculado a um nome.
Podemos usar `return` em combinação com `<-` para vincular coisas a nomes.

```{.haskell:hs}
main = do
    a <- return "hell"
    b <- return "yeah!"
    putStrLn $ a ++ " " ++ b
```

Então, como você vê, `return` é o oposto de `<-`.
Enquanto `return` pega um valor e o envolve em uma caixa, `<-` pega uma caixa (e a executa) e tira o valor dela, ligando-o a um nome.
Mas fazer isso é meio redundante, especialmente porque você pode usar *let* bindings em blocos *do* para vincular a nomes, assim:

```{.haskell:hs}
main = do
    let a = "hell"
        b = "yeah"
    putStrLn $ a ++ " " ++ b
```

Ao lidar com blocos *do* de E/S, usamos principalmente `return` porque precisamos criar uma ação de E/S que não faça nada ou porque não queremos a ação de E/S composta de um bloco *do* tenha o valor do resultado de sua última ação, mas queremos que ele tenha um valor de resultado diferente; portanto, usamos `return` para fazer uma ação de E/S que sempre tem o resultado desejado contido e a colocamos no final.

::: {.hintbox}
Um bloco *do* também pode ter apenas uma ação de E/S.
Nesse caso, é o mesmo que apenas escrever a ação de E/S.
Algumas pessoas preferem escrever `then do return ()` nesse caso, porque o *else* também tem um *do*.
:::

Antes de passarmos para arquivos, vamos dar uma olhada em algumas funções úteis ao lidar com E/S.

`putStr`{.label .function} é muito parecido com `putStrLn`, pois é preciso uma string como um parâmetro e retorna uma ação de E/S que imprimirá essa string no terminal, apenas `putStr` não pula para uma nova linha depois de imprimir a string enquanto `putStrLn` faz.

```{.haskell:hs}
main = do   putStr "Hey, "
            putStr "I'm "
            putStrLn "Andy!"
```

```{.plain}
$ runhaskell putstr_test.hs
Hey, I'm Andy!
```

Sua assinatura de tipo é `putStr :: String -> IO ()`, portanto, o resultado encapsulado na ação de E/S resultante é a unit.
Um valor inútil, por isso não faz sentido vinculá-lo.

`putChar`{.label .function} pega um caractere e retorna uma ação de E/S que o imprimirá no terminal.

```{.haskell:hs}
main = do   putChar 't'
            putChar 'e'
            putChar 'h'
```

```{.plain}
$ runhaskell putchar_test.hs
teh
```

`putStr` é realmente definida recursivamente com a ajuda de `putChar`.
A condição de borda de `putStr` é a string vazia; portanto, se estivermos imprimindo uma string vazia, basta retornar uma ação de E/S que não faz nada usando `return ()`.
Se não estiver vazia, imprima o primeiro caractere da string fazendo `putChar` e depois imprima o resto deles usando `putStr`.

```{.haskell:hs}
putStr :: String -> IO ()
putStr [] = return ()
putStr (x:xs) = do
    putChar x
    putStr xs
```

Veja como podemos usar a recursão na E/S, exatamente como podemos usá-la no código puro.
Assim como no código puro, definimos o caso de borda e, em seguida, pensamos qual é realmente o resultado.
É uma ação que primeiro produz o primeiro caractere e depois produz o restante da string.

`print`{.label .function} pega um valor de qualquer tipo que seja uma instância de `Show` (o que significa que sabemos como representá-lo como uma string), chama `show` com esse valor para "stringificá-lo" e, em seguida, envia essa string para o terminal.
Basicamente, é apenas `putStrLn . show`.
Ele primeiro executa `show` em um valor e depois alimenta isso para `putStrLn`, que retorna uma ação de E/S que imprimirá nosso valor.

```{.haskell:hs}
main = do   print True
            print 2
            print "haha"
            print 3.2
            print [3,4,3]
```

```{.haskell:hs}
$ runhaskell print_test.hs
True
2
"haha"
3.2
[3,4,3]
```

Como você pode ver, é uma função muito útil.
Lembra de como falamos sobre como as ações de E/S são executadas apenas quando caem em `main` ou quando tentamos avaliá-las no prompt do GHCI?
Quando digitamos um valor (como `3` ou `[1,2,3]`) e pressionamos a tecla de retorno, o GHCI realmente usa `print` nesse valor para exibi-lo em nosso terminal!

```{.haskell:hs}
ghci> 3
3
ghci> print 3
3
ghci> map (++"!") ["hey","ho","woo"]
["hey!","ho!","woo!"]
ghci> print (map (++"!") ["hey","ho","woo"])
["hey!","ho!","woo!"]
```

Quando queremos imprimir strings, geralmente usamos `putStrLn` porque não queremos as aspas ao redor delas, mas para imprimir valores de outros tipos no terminal, `print` é a mais usada.

`getChar`{.function .label} é uma ação de E/S que lê um caractere da entrada.
Assim, sua assinatura de tipo é `getChar :: IO Char`, porque o resultado contido na ação de E/S é um `Char`.
Observe que, devido ao buffer, a leitura dos caracteres não acontecerá até que o usuário esmague a tecla de retorno (Enter).

```{.haskell:hs}
main = do
    c <- getChar
    if c /= ' '
        then do
            putChar c
            main
        else return ()
```

Este programa parece que deve ler um caractere e depois verificar se é um espaço.
Se for, interrompa a execução e, se não, imprima-o no terminal e faça a mesma coisa tudo de novo.
Bem, meio que faz, mas não da maneira que você pode esperar.
Verifique isso:

```{.plain}
$ runhaskell getchar_test.hs
hello sir
hello
```

A segunda linha é a entrada.
Nós inserimos `hello sir` e depois pressionamos return.
Devido ao buffer, a execução do programa começará somente após termos pressionado return e não após cada caractere inserido.
Mas assim que pressionamos o return, ele atua sobre o que temos colocado até agora.
Tente brincar com este programa para sentir isso!

A função `when`{.label .function} é encontrada em `Control.Monad` (para obter acesso a ela, faça `import Control.Monad`).
É interessante porque em um bloco *do* parece uma instrução de fluxo de controle, mas na verdade é uma função normal.
Leva um valor booleano e uma ação de E/S, se esse valor booleano for `True`, ele retornará a mesma ação de E/S que fornecemos a ela.
No entanto, se for `False`, ele retornará a ação `return ()`, ou seja, uma ação de E/S que não faz nada.
Aqui está como poderíamos reescrever o pedaço de código anterior com o qual demonstramos `getChar` usando `when`:

```{.haskell:hs}
import Control.Monad

main = do
    c <- getChar
    when (c /= ' ') $ do
        putChar c
        main
```

Portanto, como você pode ver, é útil para encapsular o padrão <code>if *something* then do *some I/O action* else return ()</code>.

`sequence`{.label .function} uma lista de ações de E/S e retorna uma ação de E/S que executará essas ações uma após a outra.
O resultado contido nessa ação de E/S será uma lista dos resultados de todas as ações de E/S que foram executadas.
Sua assinatura de tipo é `sequence :: [IO a] -> IO [a]`.
Fazendo isso:

```{.haskell:hs}
main = do
    a <- getLine
    b <- getLine
    c <- getLine
    print [a,b,c]
```

É exatamente o mesmo que fazer isso:.

```{.haskell:hs}
main = do
    rs <- sequence [getLine, getLine, getLine]
    print rs
```

Então `sequence [getLine, getLine, getLine]` cria uma ação de E/S que executará `getLine` três vezes.
Se vincularmos essa ação a um nome, o resultado será uma lista de todos os resultados, portanto, no nosso caso, uma lista de três coisas que o usuário inseriu no prompt.

Um padrão comum com `sequence` é quando mapeamos funções como `print` ou `putStrLn` sobre listas.
Fazer `map print [1,2,3,4]` não criará uma ação de E/S.
Ele criará uma lista de ações de E/S, porque é como escrever `[print 1, print 2, print 3, print 4]`.
Se queremos transformar essa lista de ações de E/S em uma ação de E/S, temos que sequenciá-la.

```{.haskell:hs}
ghci> sequence (map print [1,2,3,4,5])
1
2
3
4
5
[(),(),(),(),()]
```

O que há com `[(),(),(),(),()]` no fim?
Bem, quando avaliamos uma ação de E/S no GHCI, ela é executada e, em seguida, seu resultado é impresso, a menos que esse resultado seja `()`, caso em que não é impresso.
É por isso que avaliar `putStrLn "hehe"` no GHCI apenas imprime `hehe` (porque o resultado contido em `putStrLn "hehe"` é `()`).
Mas quando fazemos `getLine` no GHCI, o resultado dessa ação de E/S é impresso, porque `getLine` tem um tipo de `IO String`.

Como o mapeamento de uma função que retorna uma ação de E/S sobre uma lista e depois sequenciá-la é tão comum, as funções utilitárias `mapM`{.label .function} e `mapM_`{.label .function} foram introduzidas.
`mapM` pega uma função e uma lista, mapeia a função sobre a lista e depois a sequencia.
`mapM_` faz o mesmo, apenas joga fora o resultado mais tarde.
Geralmente usamos `mapM_` quando não nos importamos com o resultado de nossas ações sequenciadas de E/S.

```{.haskell:hs}
ghci> mapM print [1,2,3]
1
2
3
[(),(),()]
ghci> mapM_ print [1,2,3]
1
2
3
```

`forever`{.label .function} pega uma ação de E/S e retorna uma ação de E/S que apenas repete a ação de E/S que recebeu para sempre.
Está localizada em `Control.Monad`.
Este pequeno programa pedirá indefinidamente ao usuário alguma entrada e cuspirá de volta para ele, em CAPSLOCK:

```{.haskell:hs}
import Control.Monad
import Data.Char

main = forever $ do
    putStr "Give me some input: "
    l <- getLine
    putStrLn $ map toUpper l
```

`forM`{.label .function} (localizada em `Control.Monad`) é como `mapM`, só que tem seus parâmetros trocados.
O primeiro parâmetro é a lista e a segunda é a função a ser mapeada sobre essa lista, que é então sequenciada.
Por que isso é útil?
Bem, com algum uso criativo de lambdas e notação *do*, podemos fazer coisas assim:

```{.haskell:hs}
import Control.Monad

main = do
    colors <- forM [1,2,3,4] (\a -> do
        putStrLn $ "Which color do you associate with the number " ++ show a ++ "?"
        color <- getLine
        return color)
    putStrLn "The colors that you associate with 1, 2, 3 and 4 are: "
    mapM putStrLn colors
```

O `(\a -> do ... )` é uma função que pega um número e retorna uma ação de E/S.
Temos que cercá-lo com parênteses, caso contrário, o lambda acha que as duas últimas ações de E/S pertencem a ela.
Observe que fazemos `return color` no bloco *do* interno.
Fazemos isso para que a ação de E/S que o bloco *do* define tenha o resultado de nossa cor contida nele.
Na verdade, não tivemos que fazer isso, porque `getLine` já tem isso contido nele.
Fazer `color <- getLine` e depois `return color` é apenas desembalar o resultado de `getLine` e reempacotá-lo novamente, portanto é o mesmo que apenas fazer `getLine`.
O `forM` (chamado com seus dois parâmetros) produz uma ação de E/S, cujo resultado nos vinculamos a `colors`.
`colors` é apenas uma lista normal que contém strings.
No final, imprimimos todas essas cores fazendo `mapM putStrLn colors`.

Você pode pensar em `forM` como significa: faça uma ação de E/S para cada elemento nesta lista.
O que cada ação de E/S fará pode depender do elemento usado para realizar a ação.
Finalmente, execute essas ações e vincule seus resultados a algo.
Não precisamos vinculá-lo, também podemos simplesmente jogá-lo fora.

```{.plain}
$ runhaskell form_test.hs
Which color do you associate with the number 1?
white
Which color do you associate with the number 2?
blue
Which color do you associate with the number 3?
red
Which color do you associate with the number 4?
orange
The colors that you associate with 1, 2, 3 and 4 are:
white
blue
red
orange
```

Poderíamos realmente ter feito isso sem `forM`, apenas com `forM` é mais legível.
Normalmente, escrevemos `forM` quando queremos mapear e sequenciar algumas ações que definimos lá no local usando a notação *do*.
Na mesma veia, poderíamos ter substituído a última linha por `forM colors putStrLn`.

Nesta seção, aprendemos o básico de entrada e saída.
Também descobrimos quais são as ações de E/S, como elas nos permitem fazer entrada e saída e quando elas são realmente executadas.
Para reiterar, as ações de E/S são valores muito parecidos com qualquer outro valor em Haskell.
Podemos passá-las como parâmetros para funções e as funções podem retornar ações de E/S como resultados.
O que há de especial nelas é que, se caírem na função `main` (ou são o resultado em uma linha no GHCI), elas são executadas.
E é aí que elas conseguem escrever coisas na tela ou tocar Yakety Sax através de seus alto-falantes.
Cada ação de E/S também pode encapsular um resultado com o qual diz o que obteve do mundo real.

Não pense em uma função como `putStrLn` como uma função que pega uma string e a imprime na tela.
Pense nisso como uma função que pega uma string e retorna uma ação de E/S.
Essa ação de E/S, quando realizada, imprimirá belas poesias em seu terminal.

## Arquivos e fluxos (Files and streams) {#files-and-streams}

![streams](assets/images/input-and-output/streams.png){.right width=464 height=322}

`getChar` é uma ação de E/S que lê um único caractere do terminal.
`getLine` é uma ação de E/S que lê uma linha do terminal.
Esses dois são bastante diretos e a maioria das linguagens de programação tem algumas funções ou declarações paralelas a eles.
Mas agora, vamos conhecer `getContents`{.label .function}.
`getContents` é uma ação de E/S que lê tudo da entrada padrão até encontrar um caractere de fim de arquivo.
Seu tipo é `getContents :: IO String`.
O legal de `getContents` é que ele faz a E/S preguiçosa (lazy I/O).
Quando fazemos `foo <- getContents`, ele não lê toda a entrada de uma só vez, armazena na memória e depois liga para `foo`.
Não, é preguiçoso!
Vai dizer: *"Sim, sim, vou ler a entrada do terminal mais tarde, à medida que avançamos, quando você realmente presisar!"*.

`getContents` é realmente útil quando estamos canalizando (piping) a saída de um programa para a entrada de nosso programa.
Caso você não saiba como o encanamento (piping) funciona nos sistemas Unix-y, aqui está uma introdução rápida.
Vamos criar um arquivo de texto que contenha o seguinte pequeno haiku:

```{.plain}
I'm a lil' teapot
What's with that airplane food, huh?
It's so small, tasteless
```

Sim, o haiku é péssimo, e daí?
Se alguém souber de bons tutoriais de haiku, me avise.

Agora, lembre-se do pequeno programa que escrevemos quando estávamos apresentando a função `forever`.
Ele pedia ao usuário uma linha, devolvia a ele em CAPSLOCK e, em seguida, fazia tudo de novo, indefinidamente.
Apenas para que você não precise rolar todo o caminho de volta, aqui está novamente:

```{.haskell:hs}
import Control.Monad
import Data.Char

main = forever $ do
    putStr "Give me some input: "
    l <- getLine
    putStrLn $ map toUpper l
```

Vamos salvar esse programa como `capslocker.hs` ou algo assim e compilar.
E então, vamos usar um pipe unix para alimentar nosso arquivo de texto diretamente ao nosso pequeno programa.
Usaremos a ajuda do programa GNU *cat*, que imprime um arquivo que é dado a ele como argumento.
Confira, booyaka!

```{.plain}
$ ghc --make capslocker
[1 of 1] Compiling Main             ( capslocker.hs, capslocker.o )
Linking capslocker ...
$ cat haiku.txt
I'm a lil' teapot
What's with that airplane food, huh?
It's so small, tasteless
$ cat haiku.txt | ./capslocker
I'M A LIL' TEAPOT
WHAT'S WITH THAT AIRPLANE FOOD, HUH?
IT'S SO SMALL, TASTELESS
capslocker <stdin>: hGetLine: end of file
```

Como você pode ver, a canalização da saída de um programa (no nosso caso era *cat*) para a entrada de outro (*capslocker*) é feita com o caractere `|`.
O que fizemos é praticamente equivalente a apenas executar *capslocker*, digitar nosso haiku no terminal e depois emitir um caractere de fim de arquivo (que geralmente é feito pressionando Ctrl-D).
É como executar *cat haiku.txt* e dizer: "Espere, não imprima isso no terminal, diga para o *capslocker* em vez disso!".

Então, o que estamos fazendo essencialmente com esse uso de `forever` está pegando a entrada e a transformando em alguma saída.
É por isso que podemos usar `getContents` para tornar nosso programa ainda mais curto e melhor:

```{.haskell:hs}
import Data.Char

main = do
    contents <- getContents
    putStr (map toUpper contents)
```

Executamos a ação de E/S `getContents` e nomeamos a string que ela produz de `contents`.
Então, mapeamos `toUpper` sobre essa string e imprimimos isso no terminal.
Lembre-se de que, como strings são basicamente listas, que são preguiçosas e `getContents` é I/O lazy, ele não tentará ler todo o conteúdo de uma só vez e armazená-lo na memória antes de imprimir a versão em caixa alta.
Em vez disso, imprimirá a versão em caixa alta enquanto a lê, porque apenas lerá uma linha da entrada quando realmente precisar.

```{.plain}
$ cat haiku.txt | ./capslocker
I'M A LIL' TEAPOT
WHAT'S WITH THAT AIRPLANE FOOD, HUH?
IT'S SO SMALL, TASTELESS
```

Legal, funciona.
E se apenas executarmos *capslocker* e tentarmos digitar as linhas nós mesmos?

```{.plain}
$ ./capslocker
hey ho
HEY HO
lets go
LETS GO
```

Saímos disso pressionando Ctrl-D.
Muito legal!
Como você pode ver, imprimi nossa entrada em caixa alta de volta para nós, linha por linha.
Quando o resultado de `getContents` está vinculado a `contents`, ele não é representado na memória como uma string real, mas mais como uma promessa de que produzirá a string eventualmente.
Quando mapeamos `toUpper` sobre `contents`, essa também é uma promessa de mapear essa função sobre os conteúdos eventuais.
E, finalmente, quando `putStr` acontece, diz à promessa anterior: *"Ei, preciso de uma linha em caixa alta!"*.
Não tem nenhuma linha ainda, então diz a `contents`: *"Ei, que tal realmente obter uma linha do terminal?"*.
Então é aí que `getContents` realmente lê do terminal e fornece uma linha para o código que solicitou que ele produzisse algo tangível.
Esse código mapeia `toUpper` sobre essa linha e o entrega a `putStr`, que a imprime.
E então, `putStr` diz: *"Ei, preciso da próxima linha, vamos lá!"* e isso se repete até que não haja mais entrada, o que é significado por um caractere de fim de arquivo.

Vamos criar programas que recebam algumas entradas e imprimam apenas as linhas que são menores que 10 caracteres.
Observe:

```{.haskell:hs}
main = do
    contents <- getContents
    putStr (shortLinesOnly contents)

shortLinesOnly :: String -> String
shortLinesOnly input =
    let allLines = lines input
        shortLines = filter (\line -> length line < 10) allLines
        result = unlines shortLines
    in  result
```

Fizemos nossa parte de E/S do programa o mais curta possível.
Como nosso programa deve obter alguma entrada e imprimir alguma saída com base na entrada, podemos implementá-lo lendo o conteúdo da entrada, executando uma função neles e depois imprimindo o que a função devolveu.

A função `shortLinesOnly` funciona assim: pega uma string, como `"short\nlooooooooooooooong\nshort again"`.
Essa string tem três linhas, duas delas são curtas e a do meio é longa.
Ele executa a função `lines` nessa string, que a converte em `["short", "looooooooooooooong", "short again"]`, à qual nos ligamos ao nome `allLines`.
Essa lista de strings é filtrada para que apenas as linhas com menos de 10 caracteres permaneçam na lista, produzindo `["short", "short again"]`.
E, finalmente, `unlines` une essa lista em uma única string delimitada por novas linhas (*newlines*), dando `"short\nshort again"`.
Vamos tentar.

```{.plain:hs}
i'm short
so am i
i am a loooooooooong line!!!
yeah i'm long so what hahahaha!!!!!!
short line
loooooooooooooooooooooooooooong
short
```

```{.plain:hs}
$ ghc --make shortlinesonly
[1 of 1] Compiling Main             ( shortlinesonly.hs, shortlinesonly.o )
Linking shortlinesonly ...
$ cat shortlines.txt | ./shortlinesonly
i'm short
so am i
short
```

Nós canalizamos (pipe) o conteúdo de *shortlines.txt* para *shortlinesonly*, e a saída contém apenas as linhas curtas.

Esse padrão de obter alguma string da entrada, transformá-la com uma função e a saída é tão comum que existe uma função que facilita isso ainda mais fácil, chamada `interact`{.label .function}.
`interact` leva uma função do tipo `String -> String` como um parâmetro e retorna uma ação de E/S que levará alguma entrada, executará essa função nela e depois imprimirá o resultado da função.
Vamos modificar nosso programa para usar isso.

```{.haskell:hs}
main = interact shortLinesOnly

shortLinesOnly :: String -> String
shortLinesOnly input =
    let allLines = lines input
        shortLines = filter (\line -> length line < 10) allLines
        result = unlines shortLines
    in  result
```

Apenas para mostrar que isso pode ser alcançado com muito menos código (mesmo que seja menos legível) e demonstrar nossa habilidade de composição de funções, vamos reformular um pouco mais.

```{.haskell:hs}
main = interact $ unlines . filter ((<10) . length) . lines
```

Uau, na verdade reduzimos isso a apenas uma linha, o que é muito legal!

`interact` pode ser usado para fazer programas em que são canalizados (piped) alguns conteúdos e depois despejam algum resultado ou pode ser usado para criar programas que parecem levar uma linha de entrada do usuário, devolver algum resultado com base nessa linha e depois pegar outra linha e assim por diante.
Na verdade, não há uma distinção real entre os dois, depende apenas de como o usuário deve usá-los.

Vamos fazer um programa que leia continuamente uma linha e depois nos diga se a linha é um palíndromo ou não.
Poderíamos apenas usar `getLine` para ler uma linha, dizer ao usuário se é um palíndromo e depois executar `main` tudo de novo.
Mas é mais simples se usarmos `interact`.
Ao usar `interact`, pense no que você precisa fazer para transformar alguma entrada na saída desejada.
No nosso caso, temos que substituir cada linha da entrada por `"palindrome"` ou `"not a palindrome"`.
Portanto, temos que escrever uma função que transforme algo como `"elephant\nABCBA\nwhatever"` em `"not a palindrome\npalindrome\nnot a palindrome"`.
Vamos fazer isso!

```{.haskell:hs}
respondPalindromes contents = unlines (map (\xs -> if isPalindrome xs then "palindrome" else "not a palindrome") (lines contents))
    where   isPalindrome xs = xs == reverse xs
```

Vamos escrever isso em point-free.

```{.haskell:hs}
respondPalindromes = unlines . map (\xs -> if isPalindrome xs then "palindrome" else "not a palindrome") . lines
    where   isPalindrome xs = xs == reverse xs
```

Bastante simples.
Primeiro, transforma algo como `"elephant\nABCBA\nwhatever"` em `["elephant", "ABCBA", "whatever"]` e depois mapeia esse lambda sobre ele, dando `["not a palindrome", "palindrome", "not a palindrome"]` e depois `unlines` une essa lista em uma única string delimitada por novas linhas.
Agora podemos fazer

```{.haskell:hs}
main = interact respondPalindromes
```

Vamos testar isso:

```{.plain}
$ runhaskell palindromes.hs
hehe
not a palindrome
ABCBA
palindrome
cookie
not a palindrome
```

Mesmo que tenhamos criado um programa que transforma uma grande série de entradas em outra, ele age como se tivéssemos criado um programa que o faz linha por linha.
Isso ocorre porque Haskell é preguiçoso e quer imprimir a primeira linha da string de resultado, mas não pode porque ainda não tem a primeira linha da entrada.
Assim que damos a primeira linha de entrada, ele imprime a primeira linha da saída.
Saímos do programa emitindo um caractere de fim de linha.

Também podemos usar este programa apenas canalizando (piping) um arquivo para ele.
Digamos que temos este arquivo:

```{.plain}
dogaroo
radar
rotor
madam
```

e nós salvamos como `words.txt`.
É isso que obtemos canalizando-o para o nosso programa:

```{.plain}
$ cat words.txt | runhaskell palindromes.hs
not a palindrome
palindrome
palindrome
palindrome
```

Novamente, obtemos a mesma saída como se tivéssemos executado o programa e digitado as palavras nós mesmos na entrada padrão.
A diferença é que não vemos a entrada, porque foi fornecida a `palindromes.hs` de um arquivo em vez de inserida diretamente no teclado.

Então, agora você provavelmente vê como a E/S preguiçosa funciona e como podemos usá-la em nossa vantagem.
Você pode pensar em termos de qual deve ser a saída para alguma entrada e escrever uma função para fazer essa transformação.
Na E/S preguiçosa, nada é comido da entrada até que seja absolutamente necessário, porque o que queremos imprimir agora depende dessa entrada.

Até agora, trabalhamos com E/S imprimindo coisas no terminal e lendo dele.
Mas e a leitura e escrita de arquivos?
Bem, de certa forma, já estamos fazendo isso.
Uma maneira de pensar sobre a leitura do terminal é imaginar que é como ler de um arquivo (um pouco especial).
O mesmo vale para escrever no terminal, é como escrever em um arquivo.
Podemos chamar esses dois arquivos de `stdout` e `stdin`, significando *standard output* (saída padrão) e *standard input* (entrada padrão), respectivamente.
Mantendo isso em mente, veremos que escrever e ler arquivos é muito parecido com escrever na saída padrão e na leitura da entrada padrão.

Começaremos com um programa realmente simples que abre um arquivo chamado *girlfriend.txt*, que contém um verso do hit número 1 de Avril Lavigne, *Girlfriend*, e apenas o imprime para o terminal.
Aqui está *girlfriend.txt*:

```{.plain}
Hey! Hey! You! You!
I don't like your girlfriend!
No way! No way!
I think you need a new one!
```

E aqui está o nosso programa:

```{.haskell:hs}
import System.IO

main = do
    handle <- openFile "girlfriend.txt" ReadMode
    contents <- hGetContents handle
    putStr contents
    hClose handle
```

Executando-o, obtemos o resultado esperado:

```{.plain}
$ runhaskell girlfriend.hs
Hey! Hey! You! You!
I don't like your girlfriend!
No way! No way!
I think you need a new one!
```

Vamos repassar isso linha por linha.
A primeira linha são apenas quatro exclamações, para chamar nossa atenção.
Na segunda linha, Avril nos diz que ela não gosta de nossa parceira romântica atual.
A terceira linha serve para enfatizar essa desaprovação, enquanto a quarta linha sugere que devemos procurar uma nova namorada.

Vamos também passar pelo programa linha por linha!
Nosso programa são várias ações de E/S coladas com um bloco *do*.
Na primeira linha do bloco *do*, notamos uma nova função chamada `openFile`{.label .function}.
Esta é a assinatura do seu tipo: `openFile :: FilePath -> IOMode -> IO Handle`.
Se você ler em voz alta, ele afirma: `openFile` pega um caminho de arquivo e um `IOMode` e retorna uma ação de E/S que abrirá um arquivo e terá o identificador (*handle*) associado do arquivo encapsulado como seu resultado.

`FilePath` é apenas um [sinônimo de tipo](making-our-own-types-and-typeclasses.html#type-synonyms) para `String`, simplesmente definido como:

```{.haskell:hs}
type FilePath = String
```

`IOMode` é um tipo que é definido assim:

```{.haskell:hs}
data IOMode = ReadMode | WriteMode | AppendMode | ReadWriteMode
```

![A FILE IN A CAKE!!!](assets/images/input-and-output/file.png){.left width=232 height=340}

Assim como nosso tipo que representa os sete valores possíveis para os dias da semana, esse tipo é uma enumeração que representa o que queremos fazer com nosso arquivo aberto.
Muito simples.
Observe apenas que esse tipo é `IOMode` e não `IO Mode`.
`IO Mode` seria o tipo de ação de E/S que tem um valor de algum tipo `Mode` como seu resultado, mas `IOMode` é apenas uma enumeração simples.

Por fim, retorna uma ação de E/S que abrirá o arquivo especificado no modo especificado.
Se ligarmos essa ação a algo, obtemos um `Handle`.
Um valor do tipo `Handle` representa onde está o nosso arquivo.
Usaremos esse identificador para que saibamos de qual arquivo ler.
Seria estúpido ler um arquivo, mas não vincular essa leitura a um identificador, porque não seríamos capazes de fazer nada com o arquivo.
Então, no nosso caso, vinculamos o identificador a `handle`.

Na próxima linha, vemos uma função chamada `hGetContents`{.label .function}.
É preciso um `Handle`, de modo que sabe de qual arquivo obter o conteúdo e retorna um `IO String` --- uma ação de E/S que mantém como resultado o conteúdo do arquivo.
Esta função é muito parecida com `getContents`.
A única diferença é que `getContents` lerá automaticamente a entrada padrão (ou seja, do terminal), enquanto `hGetContents` recebe um identificador de arquivos que diz a partir de qual arquivo ler.
Em todos os outros aspectos, elas funcionam da mesma maneira.
E assim como `getContents`, `hGetContents` não tentará ler o arquivo de uma só vez e armazená-lo na memória, mas o lerá conforme necessário.
Isso é muito legal, porque podemos tratar `contents` como todo o conteúdo do arquivo, mas ele não está realmente carregado na memória.
Portanto, se esse fosse um arquivo realmente enorme, fazer `hGetContents` não engasgaria nossa memória, mas leria apenas o que precisava do arquivo, quando precisava.

Observe a diferença entre o identificador usado para identificar um arquivo e o conteúdo do arquivo, vinculado ao nosso programa a `handle` e `contents`.
O identificador é apenas algo pelo qual sabemos qual é o nosso arquivo.
Se você imaginar todo o seu sistema de arquivos como um livro realmente grande e cada arquivo é um capítulo no livro, o identificador é um marcador que mostra onde você está lendo (ou escrevendo) um capítulo, enquanto o conteúdo é o capítulo real.

Com `putStr contents`, apenas imprimimos o conteúdo para a saída padrão e depois fazemos `hClose`{.label .function}, que pega um identificador e retorna uma ação de E/S que fecha o arquivo.
Você tem que fechar o arquivo depois de abri-lo com `openFile`!

Outra maneira de fazer o que acabamos de fazer é usar a função `withFile`{.label .function}, que tem uma assinatura de tipo de `withFile :: FilePath -> IOMode -> (Handle -> IO a) -> IO a`.
É preciso um caminho para um arquivo, um `IOMode` e depois é necessária uma função que pegue um identificador e retorne alguma ação de E/S.
O que ele retorna é uma ação de E/S que abrirá esse arquivo, fará algo que queremos com o arquivo e depois o fechará.
O resultado encapsulado na ação final de E/S que é retornada é o mesmo que o resultado da ação de E/S que a função que damos a retorna.
Isso pode parecer um pouco complicado, mas é realmente simples, especialmente com lambdas, aqui está o nosso exemplo anterior reescrito para usar `withFile`:

```{.haskell:hs}
import System.IO

main = do
    withFile "girlfriend.txt" ReadMode (\handle -> do
        contents <- hGetContents handle
        putStr contents)
```

Como você pode ver, é muito semelhante ao pedaço de código anterior.
`(\handle -> ... )` é a função que pega um identificador e retorna uma ação de E/S e geralmente é feita assim, com um lambda.
A razão pela qual ele deve tomar uma função que retorna uma ação de E/S em vez de apenas tomar uma ação de E/S para fazer e depois fechar o arquivo é porque a ação de E/S que passamos para ele não saberia em qual arquivo operar.
Dessa forma, `withFile` abre o arquivo e passa o identificador para a função que demos a ele.
Ele recebe uma ação de E/S de volta dessa função e, em seguida, faz uma ação de E/S exatamente assim, apenas fecha o arquivo depois.
Veja como podemos criar nossa própria função `withFile`:

```{.haskell:hs}
withFile' :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
withFile' path mode f = do
    handle <- openFile path mode
    result <- f handle
    hClose handle
    return result
```

![butter toast](assets/images/input-and-output/edd.png){.right width=246 height=360}

Sabemos que o resultado será uma ação de E/S, para que possamos começar com um *do*.
Primeiro abrimos o arquivo e obtemos um identificador dele.
Então, aplicamos `handle` à nossa função para recuperar a ação de E/S que faz todo o trabalho.
Vinculamos essa ação a `result`, fechamos o manipulador e depois fazemos `return result`.
Ao retornar (`return`) o resultado encapsulado na ação de E/S que obtivemos de `f`, fazemos com que nossa ação de E/S encapsule o mesmo resultado que a que recebemos de `f handle`.
Portanto, se `f handle` retornar uma ação que lerá várias linhas da entrada padrão e as escreverá em um arquivo e terá como resultado encapsulado o número de linhas que leu, se a usássemos com `withFile'`, a ação de E/S resultante também teria como resultado o número de linhas lidas.

Assim como temos `hGetContents` que funciona como `getContents`, mas para um arquivo específico, também há `hGetLine`{.label .function}, `hPutStr`{.label .function}, `hPutStrLn`{.label .function}, `hGetChar`{.label .function}, etc.
Eles funcionam exatamente como seus colegas sem o *h*, apenas eles usam um identificador como um parâmetro e operam nesse arquivo específico, em vez de operar na entrada padrão ou na saída padrão.
Exemplo: `putStrLn` é uma função que pega uma string e retorna uma ação de E/S que imprimirá essa string no terminal e uma nova linha depois dela.
`hPutStrLn` pega um identificador e uma string e retorna uma ação de E/S que gravará essa string no arquivo associado ao identificador e depois colocará uma nova linha depois dela.
Na mesma veia, `hGetLine` pega um identificador e retorna uma ação de E/S que lê uma linha de seu arquivo.

Carregar arquivos e tratar seu conteúdo como strings é tão comum que temos estas três pequenas funções legais para facilitar ainda mais nosso trabalho:

`readFile`{.label .function} tem uma assinatura de tipo de `readFile :: FilePath -> IO String`.
Lembre-se, `FilePath` é apenas um nome chique para `String`.
`readFile` pega um caminho para um arquivo e retorna uma ação de E/S que lerá esse arquivo (preguiçosamente, é claro) e vinculará seu conteúdo a algo como uma string.
Geralmente é mais útil do que fazer `openFile` e vinculá-lo a um identificador e depois fazer `hGetContents`.
Veja como poderíamos ter escrito nosso exemplo anterior com `readFile`:

```{.haskell:hs}
import System.IO

main = do
    contents <- readFile "girlfriend.txt"
    putStr contents
```

Como não obtemos um identificador com o qual identificar nosso arquivo, não podemos fechá-lo manualmente, então Haskell faz isso por nós quando usamos `readFile`.

`writeFile`{.label .function} tem um tipo de `writeFile :: FilePath -> String -> IO ()`.
É preciso um caminho para um arquivo e uma string para gravar nesse arquivo e retorna uma ação de E/S que fará a escrita.
Se esse arquivo já existir, ele será reduzido para o comprimento zero antes de ser escrito.
Veja como transformar *girlfriend.txt* em uma versão em CAPSLOCK e escrevê-la para *girlfriendcaps.txt*:

```{.haskell:hs}
import System.IO
import Data.Char

main = do
    contents <- readFile "girlfriend.txt"
    writeFile "girlfriendcaps.txt" (map toUpper contents)
```

```{.plain}
$ runhaskell girlfriendtocaps.hs
$ cat girlfriendcaps.txt
HEY! HEY! YOU! YOU!
I DON'T LIKE YOUR GIRLFRIEND!
NO WAY! NO WAY!
I THINK YOU NEED A NEW ONE!
```

`appendFile`{.label .function} tem uma assinatura de tipo exatamente como `writeFile`, apenas `appendFile` não trunca o arquivo para o comprimento zero se ele já existir, mas anexa coisas a ele.

Digamos que temos um arquivo *todo.txt* que tem uma tarefa por linha que temos que fazer.
Agora, vamos fazer um programa que adote uma linha da entrada padrão e a adicione à nossa lista de tarefas.

```{.haskell:hs}
import System.IO

main = do
    todoItem <- getLine
    appendFile "todo.txt" (todoItem ++ "\n")
```

```{.plain}
$ runhaskell appendtodo.hs
Iron the dishes
$ runhaskell appendtodo.hs
Dust the dog
$ runhaskell appendtodo.hs
Take salad out of the oven
$ cat todo.txt
Iron the dishes
Dust the dog
Take salad out of the oven
```

Precisávamos adicionar o `"\n"` ao final de cada linha, porque `getLine` não nos dá um caractere de nova linha no final.

Ooh, mais uma coisa.
Conversamos sobre como fazer `contents <- hGetContents handle` não faz com que todo o arquivo seja lido de uma só vez e armazenado na memória.
É I/O lazy, então fazer isso:

```{.haskell:hs}
main = do
    withFile "something.txt" ReadMode (\handle -> do
        contents <- hGetContents handle
        putStr contents)
```

é realmente como conectar um tubo (pipe) do arquivo à saída.
Assim como você pode pensar em listas como fluxos (streams), também pode pensar em arquivos como fluxos.
Isso lerá uma linha de cada vez e a imprimirá no terminal à medida que avança.
Então você pode estar perguntando, qual a largura desse tubo então?
Com que frequência o disco será acessado?
Bem, para arquivos de texto, o buffer padrão é geralmente o buffer de linha (line-buffering).
Isso significa que a menor parte do arquivo a ser lida de uma vez é uma linha.
É por isso que, nesse caso, ele realmente lê uma linha, a imprime na saída, lê a próxima linha, a imprime, etc.
Para arquivos binários, o buffer padrão geralmente é o buffer de bloco (block-buffering).
Isso significa que ele lerá o arquivo pedaço por pedaço.
O tamanho do pedaço é de algum tamanho que seu sistema operacional acha legal.

Você pode controlar exatamente como o buffer é feito usando a função `hSetBuffering`.
É preciso um identificador e um `BufferMode` e retorna uma ação de E/S que define o buffer.
`BufferMode` é um tipo de dados de enumeração simples e os valores possíveis que ele pode conter são: `NoBuffering`, `LineBuffering` ou `BlockBuffering (Maybe Int)`.
O `Maybe Int` é para o tamanho do pedaço, em bytes.
Se for `Nothing`, o sistema operacional determina o tamanho do pedaço.
`NoBuffering` significa que será lido um caractere de cada vez.
`NoBuffering` geralmente é uma merda como um modo de buffer, porque tem que acessar muito o disco.

Aqui está o nosso pedaço de código anterior, só que não o lê linha por linha, mas lê todo o arquivo em pedaços de 2048 bytes.

```{.haskell:hs}
main = do
    withFile "something.txt" ReadMode (\handle -> do
        hSetBuffering handle $ BlockBuffering (Just 2048)
        contents <- hGetContents handle
        putStr contents)
```

A leitura de arquivos em pedaços maiores pode ajudar se quisermos minimizar o acesso ao disco ou quando nosso arquivo for realmente um recurso de rede lento.

Também podemos usar `hFlush`{.label .function}, que é uma função que usa um identificador e retorna uma ação de E/S que liberará o buffer do arquivo associado ao identificador.
Quando estamos fazendo o buffer de linha, o buffer é liberado (flushed) após cada linha.
Quando estamos fazendo buffer de bloco, é depois de lermos um pedaço.
Também é liberado depois de fechar um identificador.
Isso significa que quando chegamos a um caractere de nova linha, o mecanismo de leitura (ou gravação) relata todos os dados até agora.
Mas podemos usar `hFlush` para forçar esse relatório de dados que foram lidos até agora.
Após a liberação, os dados estão disponíveis para outros programas que estão sendo executados ao mesmo tempo.

Pense em ler um arquivo em buffer de bloco como este: o vaso sanitário está programado para se liberar (dar descarga) depois de ter um galão de água dentro dele.
Então você começa a derramar água e, uma vez atingida a marca de galão, essa água é lavada automaticamente e os dados na água que você derramou até agora são lidos.
Mas você também pode lavar o banheiro manualmente pressionando o botão no banheiro.
Isso faz com que o banheiro dê descarga e toda a água (dados) dentro do banheiro seja lida.
Caso você não tenha notado, liberar (dar descarga d') o banheiro manualmente é uma metáfora para `hFlush`.
Essa não é uma analogia muito boa para os padrões de analogia de programação, mas eu queria um objeto do mundo real que possa ser liberado (flushed) para a piada (*punchline*).

Já fizemos um programa para adicionar um novo item à nossa lista de tarefas em *todo.txt*, agora vamos fazer um programa para remover um item.
Apenas colarei o código e depois passaremos pelo programa juntos para que você veja que é realmente fácil.
Usaremos algumas novas funções de `System.Directory` e uma nova função de `System.IO`, mas todas serão explicadas.

De qualquer forma, aqui está o programa para remover um item de *todo.txt*:

```{.haskell:hs}
import System.IO
import System.Directory
import Data.List

main = do
    handle <- openFile "todo.txt" ReadMode
    (tempName, tempHandle) <- openTempFile "." "temp"
    contents <- hGetContents handle
    let todoTasks = lines contents
        numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks
    putStrLn "These are your TO-DO items:"
    putStr $ unlines numberedTasks
    putStrLn "Which one do you want to delete?"
    numberString <- getLine
    let number = read numberString
        newTodoItems = delete (todoTasks !! number) todoTasks
    hPutStr tempHandle $ unlines newTodoItems
    hClose handle
    hClose tempHandle
    removeFile "todo.txt"
    renameFile tempName "todo.txt"
```

No início, abrimos *todo.txt* no modo de leitura e vinculamos seu identificador a `handle`.

Em seguida, usamos uma função que não encontramos antes, que é de `System.IO` --- `openTempFile`{.label .function}.
Seu nome é bastante auto-explicativo.
É preciso um caminho para um diretório temporário e um nome de modelo (template) para um arquivo e abre um arquivo temporário.
Usamos `"."` para o diretório temporário, porque `.` denota o diretório atual em praticamente qualquer sistema operacional.
Usamos `"temp"` como o nome do modelo para o arquivo temporário, o que significa que o arquivo temporário será nomeado *temp* mais alguns caracteres aleatórios.
Ele retorna uma ação de E/S que cria o arquivo temporário e o resultado nessa ação de E/S é um par de valores: o nome do arquivo temporário e um identificador.
Poderíamos apenas abrir um arquivo normal chamado *todo2.txt* ou algo assim, mas é melhor praticar usar `openTempFile` para que você saiba que provavelmente não está sobrescrevendo nada.

A razão pela qual não usamos `getCurrentDirectory` para obter o diretório atual e depois passá-lo para `openTempFile`, mas apenas passamos `"."` para `openTempFile` é porque `.` refere-se ao diretório atual no sistema semelhante ao Unix e Windows

Em seguida, vinculamos o conteúdo de *todo.txt* a `contents`.
Em seguida, divida essa string em uma lista de strings, cada string uma linha.
Portanto, `todoTasks` agora é algo como `["Iron the dishes", "Dust the dog", "Take salad out of the oven"]`.
Nós compactamos (zip) os números de 0 em diante e essa lista com uma função que pega um número, como 3, e uma string, como `"hey"` e retorna `"3 - hey"`, então `numberedTasks` é `["0 - Iron the dishes", "1 - Dust the dog" ...`.
Juntamos essa lista de strings em uma única string delimitada por novas linhas com `unlines` e imprimimos essa string no terminal.
Observe que, em vez de fazer isso, também poderíamos ter feito `mapM putStrLn numberedTasks`

Perguntamos ao usuário qual deles eles querem excluir e esperar que eles insira um número.
Digamos que eles desejam excluir o número 1, que é `Dust the dog`, então eles digitam `1`.
`numberString` agora é `"1"` e porque queremos um número, não uma string, corremos `read` para obter `1` e vinculamos isso a `number`.

Lembre-se das funções `delete` e `!!` de `Data.List`.
`!!` retorna um elemento de uma lista com algum índice e `delete` exclui a primeira ocorrência de um elemento em uma lista e retorna uma nova lista sem essa ocorrência.
`(todoTasks !! number)` (number agora é `1`) retorna `"Dust the dog"`.
Vinculamos `todoTasks` sem a primeira ocorrência de `"Dust the dog"` a `newTodoItems` e depois juntamos tudo em uma única string com `unlines` antes de grava-lá no arquivo temporário que abrimos.
O arquivo antigo agora está inalterado e o arquivo temporário contém todas as linhas que o antigo, exceto a que excluímos.

Depois disso, fechamos os arquivos originais e temporários e depois removemos o original com `removeFile`{.label .function}, que, como você pode ver, pega um caminho para um arquivo e o exclui.
Depois de excluir o antigo *todo.txt*, usamos `renameFile`{.label .function} para renomear o arquivo temporário para *todo.txt*.
Tenha cuidado, `removeFile` e `renameFile` (que estão ambos em `System.Directory` a propósito) usam caminhos de arquivo como seus parâmetros, não identificadores.

E é isso!
Poderíamos ter feito isso em ainda menos linhas, mas tomamos muito cuidado para não substituir nenhum arquivo existente e educadamente pedimos ao sistema operacional que nos dissesse onde podemos colocar nosso arquivo temporário.
Vamos tentar!

```{.plain}
$ runhaskell deletetodo.hs
These are your TO-DO items:
0 - Iron the dishes
1 - Dust the dog
2 - Take salad out of the oven
Which one do you want to delete?
1

$ cat todo.txt
Iron the dishes
Take salad out of the oven

$ runhaskell deletetodo.hs
These are your TO-DO items:
0 - Iron the dishes
1 - Take salad out of the oven
Which one do you want to delete?
0

$ cat todo.txt
Take salad out of the oven
```

## Argumentos de linha de comando (Command line arguments) {#command-line-arguments}

![arguments](assets/images/input-and-output/arg.png){.right width=221 height=338}

Se você deseja executar um script ou aplicativo de um terminal, geralmente é um pouco desajeitado percorrer as etapas de executar o programa, navegando um pouco nos menus (ou digitando em um prompt) e depois saindo.
Em vez disso, o que você quer fazer é alimentar os parâmetros do aplicativo ao executá-lo, para que ele já saiba o que deve fazer quando for lançado.
Se você é o tipo de pessoa que evita a dor de usar o Windows batendo a cabeça no terminal linux, você sabe do que estou falando.
Diferentemente de apenas `mv`, que renomearia o arquivo atual ou o mudaria para outro diretório, fazemos `mv file source destination` e todo o trabalho é tratado naquele instante.
`file`, `source` e `destination` são chamados de argumentos de linha de comando.
A função `CommandLineArgument` permite acessar esses argumentos de linha de comando.

A função `System.Environment` possui duas funções I/O legais e interessantes.
Uma é `getArgs`{.label .function}, que tem um tipo de `getArgs :: IO [String]` e é uma ação de E/S que obterá os argumentos que o programa foi executado e os terá como resultado contido.
A outra função é `getProgName`{.label .function}, que tem um tipo de `getProgName :: IO String` e é uma ação de E/S que contém o nome do programa.

Aqui está um pequeno programa que demonstra como isso funciona:

```{.haskell:hs}
import System.Environment
import Data.List

main = do
    args <- getArgs
    progName <- getProgName
    putStrLn "The arguments are:"
    mapM putStrLn args
    putStrLn "The program name is:"
    putStrLn progName
```

Nós vinculamos `getArgs` a `args` e `getProgName` a `progName`.
`args` é apenas uma lista de strings, nas quais podemos usar mapear as funções de processamento de listas, `mapM` e o que mais você quiser.
Aqui nós apenas imprimimos os argumentos um por um e também o nome do programa.
Vamos compilar isso como `arg-test`:

```{.plain}
$ ghc --make arg-test
[1 of 1] Compiling Main             ( arg-test.hs, arg-test.o )
Linking arg-test ...
$ ./arg-test first second w00t "multi word arg"
The arguments are:
first
second
w00t
multi word arg
The program name is:
arg-test
```

Legal.
Armados com esse conhecimento, poderíamos criar alguns aplicativos de linha de comando legais.
No momento, vamos fazer um programa que adote dois argumentos da linha de comando.
O primeiro argumento será o nome do arquivo da nossa lista de tarefas, e o segundo será uma ação que queremos realizar nesse arquivo, o que pode ser: visualizar (`view`), adicionar (`add`) ou remover (`remove`).
Se a ação for `add`, o programa aceitará outro argumento, que é a tarefa a adicionar a nossa lista.
Se a ação for `remove`, ela pegará outro argumento, que é o índice da entrada a remover.

A coisa legal sobre Haskell é que, se você deseja criar um programa que faça uma tarefa específica, digamos, processando a entrada, geralmente pode começar escrevendo uma função que faz exatamente isso, mas tudo em código puro.
Somente depois disso você escreve a parte suja de E/S que lê a entrada, chama sua função nos dados e imprime o resultado.
Mas aqui, como estamos lidando com a linha de comando, não podemos ter uma função pura agradável processando a entrada, porque o que fazemos depende dos argumentos de linha de comando.
No entanto, podemos criar algumas funções para lidar com as ações antes mesmo de fazermos a parte que processa a linha de comando.

Começaremos fazendo algumas importações.

```{.haskell:hs}
import System.Environment
import System.Directory
import System.IO
import Data.List
```

Agora, vamos fazer as funções que queremos ter.
Todos elas seguirão o trabalho com nossa lista de tarefas: visualizar itens, adicionar itens e excluir itens.
A função `view` fará a visualização.

```{.haskell:hs}
view :: [String] -> IO ()
view [fileName] = do
    contents <- readFile fileName
    let todoTasks = lines contents
        numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks
    putStr $ unlines numberedTasks
```

Se tentarmos avaliar isso no GHCI sem os argumentos, dirá que falhou a correspondência de padrões na função `view`.
Isso ocorre porque a função espera uma lista que contenha exatamente um elemento, o nome do arquivo.
No nosso programa, leremos argumentos na linha de comando e os aplicaremos a essa função, por isso, garantiremos que essa lista de argumentos tenha apenas um elemento.
O que o `view` faz é bastante simples.
Ele lê o arquivo, imprime suas linhas (depois de numerá-las apropriadamente) e pronto.

Agora, para a função `add` adicionar tarefas:

```{.haskell:hs}
add :: [String] -> IO ()
add [fileName, todoItem] = appendFile fileName (todoItem ++ "\n")
```

Também aceita uma lista de argumentos, que esperamos que tenham exatamente dois elementos: um que seja o nome do arquivo no qual queremos adicionar a tarefa e a que é a própria tarefa.
Ele usa `appendFile` para empurrar essa linha até o final do arquivo.

Por fim, remover:

```{.haskell:hs}
remove :: [String] -> IO ()
remove [fileName, numberString] = do
    handle <- openFile fileName ReadMode
    (tempName, tempHandle) <- openTempFile "." "temp"
    contents <- hGetContents handle
    let number = read numberString
        todoTasks = lines contents
        newTodoItems = delete (todoTasks !! number) todoTasks
    hPutStr tempHandle $ unlines newTodoItems
    hClose handle
    hClose tempHandle
    removeFile fileName
    renameFile tempName fileName
```

Isso é basicamente o mesmo programa que usamos na remoção anteriormente, apenas encapsulado em uma função.
Recebe uma lista de argumentos e espera que ela contenha o nome do arquivo para excluir um item e a string que representa o índice do item.

Então, definimos nossas três funções.
Agora, vamos fazer uma lista de associação que mapeie argumentos de linha de comando para essas funções.

```{.haskell:hs}
dispatch :: [(String, [String] -> IO ())]
dispatch =  [ ("add", add)
            , ("view", view)
            , ("remove", remove)
            ]
```

Isso é tudo, apenas uma lista simples de associações.
O tipo da lista e seus valores podem ser um pouco difíceis a princípio, mas uma vez que você pensa sobre a definição, ela faz sentido.
Lembre-se, `add`, `view` e `remove` têm um tipo de `[String] -> IO ()`, portanto, os elementos da lista `dispatch` são pares (tuplas de 2 elementos): o primeiro elemento é uma string (nossos comandos) e o segundo elemento é uma função.

Por fim, a função principal do nosso aplicativo:

```{.haskell:hs}
main = do
    (command:args) <- getArgs
    let (Just action) = lookup command dispatch
    action args
```

A primeira coisa que fazemos é obter os argumentos e vinculá-los a `(command:args)`.
Se chamarmos nosso aplicativo como `./todo view todo.txt`, `command` será `"view"` e `args` será `["todo.txt"]`.
A próxima linha procura nosso comando na lista `dispatch`.
Como `"view"` aponta para `view`, temos `Just view`.
Se usarmos correspondência de padrões para extrair a ação de `Just`, ela retornará `view`.
Finalmente, chamamos o `action args`, que é o mesmo que chamar `view args`, que é o mesmo que chamar `view ["todo.txt"]`.
Massa!

Se você executar o `todo`, mas não fornecer nenhum argumento, o tempo de execução gritará com você.
Isso porque, quando correspondemos ao padrão `(command:args)` com o resultado de `getArgs`, ele espera uma lista com pelo menos um elemento.
Se essa lista estiver vazia, o padrão falhará.

Vamos ver se funciona.

```{.plain}
$ ghc --make todo
[1 of 1] Compiling Main             ( todo.hs, todo.o )
Linking todo ...
$ ./todo view todo.txt
Iron the dishes
Dust the dog
Take salad out of the oven
$ ./todo add todo.txt "Pick up children from drycleaners"
$ ./todo view todo.txt
Iron the dishes
Dust the dog
Take salad out of the oven
Pick up children from drycleaners
$ ./todo remove todo.txt 2
$ ./todo view todo.txt
Iron the dishes
Dust the dog
Pick up children from drycleaners
```

Tudo parece estar funcionando!
Note que nós não validamos os argumentos do usuário, por exemplo.
Se alguém executar `./todo blorg 1`, nosso programa falhará, porque `blorg` não está na lista `dispatch`.
Mas ei, esta é apenas uma demonstração, não um software de nível empresarial que pode falhar quando você tenta se dividir por zero (apenas brincando, os programas Haskell nunca falham dessa maneira!).

## Aleatoriedade (Randomness) {#randomness}

![dice](assets/images/input-and-output/dice.png){.right width=254 height=243}

Muitas vezes, enquanto programa, você precisa obter alguns dados aleatórios.
Talvez você esteja construindo um jogo em que os dados precisem ser lançados ou precise gerar alguns dados de teste para testar seu programa.
Existem várias maneiras de fazer com que o computador gere números aleatórios para nós, mas a maioria deles depende de nós obter algumas sementes (random seeds) físicas e de sistema.
Se definirmos a semente desse gerador aleatório para a mesma coisa, nosso programa poderá gerar os mesmos "números aleatórios" toda vez que for executado.
Em outras linguagens, as funções que nos dão certa aleatoriedade mudam algum tipo de estado oculto (como uma semente aleatória global), assim como faz E/S e, portanto, porque é impuro e não pode ser feito no código funcional puro.
Haskell tem uma função que retorna um número aleatório?
Bem, pense sobre isso.
Se tivermos um gerador de números aleatórios como uma função, ele teria que ter um tipo de `foo :: () -> Int` (ou algum outro tipo de número).
Mas isso significa que ele sempre retornaria o mesmo número e não seria aleatório.
Portanto, para obter um número aleatório, temos a ideia de obter um objeto que representa um gerador de números aleatórios e passar isso para alguma função e obter de volta um número e um novo gerador de números aleatórios que usaremos para obter o próximo número e o próximo gerador e assim por diante.
Isso significa que, se usarmos duas vezes a mesma função com o mesmo gerador, obteremos o mesmo resultado.
E isso é incrível porque significa que nosso código pode ser puro, mesmo que geremos números aleatórios!
Para a mesma semente, sempre obteremos a mesma "aleatoriedade".
Se quisermos que dois números diferentes sejam gerados, basta obter o primeiro número e o novo gerador e passaremos o novo gerador para a função novamente.
Isso também significa que podemos reproduzir situações aleatórias em nosso programa (como reproduzir uma falha que apenas acontece em determinadas situações com números aleatórios específicos), o que é impossível em outras linguagens, porque não há garantia de que você obtenha os mesmos números aleatórios duas vezes consecutivas.

O módulo `System.Random` tem todas as funções que satisfazem nossa necessidade de aleatoriedade.
Vamos mergulhar em uma função que é a chave para gerar números aleatórios.
Chame a polícia, é `random`{.label .function}!
Seu tipo é `random :: (RandomGen g, Random a) => g -> (a, g)`.
Uau, algumas novas typeclasses nesta declaração de tipo!
A classe de tipo `RandomGen` é para tipos que podem atuar como geradores de aleatoriedade.
A classe de tipo `Random` é para coisas que podem assumir valores aleatórios.
Valores booleanos, caracteres, inteiros, duplos, etc.
Se tentarmos traduzir a declaração de tipo para o inglês, seria algo como: pega um gerador aleatório (que é uma instância de `RandomGen`) e retorna um valor aleatório e um novo gerador aleatório.
Por que ele retorna um novo gerador aleatório, além do valor aleatório?
Bem, como dissemos anteriormente, o gerador de números aleatórios representa o estado da computação de números aleatórios.
Se apenas tivéssemos uma função que gerasse um número aleatório, teria que alterar o único gerador de números aleatórios existente e, portanto, teria que ser uma função impura, porque Haskell é puro.
A função `random` usa um gerador existente e retorna um novo gerador estado.

O tipo padrão que é instância de `RandomGen` do `System.Random` é o `StdGen`.
Para fazer manualmente um `StdGen`, podemos usar a função `mkStdGen`{.label .function}, que tem um tipo de `mkStdGen :: Int -> StdGen`.
Pega um número inteiro e com base nisso, nos dá um gerador aleatório.
Ok, vamos tentar usar `random` e `mkStdGen` em conjunto para obter um número aleatório.

```{.haskell:hs}
ghci> random (mkStdGen 100)
```

![oops](assets/images/input-and-output/oops.png){.left width=253 height=226}

O que é isso?
Ah, certo, a função `random` pode retornar um valor de qualquer tipo que faz parte da classe de tipos `Random`, então temos que informar a Haskell que tipo de tipo queremos.
Não assumirá que queremos um `Int` ou um `Double`.
Lembre-se também de que ele retorna um par que consiste no valor aleatório e em outro gerador.

```{.haskell:hs}
ghci> random (mkStdGen 100) :: (Int, StdGen)
(-1352021624,651872571 1655838864)
ghci> random (mkStdGen 100) :: (Int, StdGen)
(-1352021624,651872571 1655838864)
```

Finalmente!
Um número que parece meio aleatório!
Como você pode ver, a representação de texto `StdGen` não consegue ensinar muito sobre o que está por trás disso.
Mas se chamarmos a função duas vezes com os mesmos parâmetros (o mesmo gerador), obtemos o mesmo resultado, conforme esperado.
Agora vamos tentar simular um arremesso de uma moeda com nossos dados aleatórios.

```{.haskell:hs}
ghci> random (mkStdGen 100) :: (Int, StdGen)
(-1352021624,651872571 1655838864)
ghci> random (mkStdGen 100) :: (Bool, StdGen)
(False,4041414 40692)
ghci> random (mkStdGen 100) :: (Float, StdGen)
(0.7915302,159796065 40692)
ghci> random (mkStdGen 949494) :: (Int, StdGen)
(539963926,466647808 1655838864)
ghci> random (mkStdGen 949488) :: (Float, StdGen)
(0.8938442,1597348957 1655838864)
ghci> random (mkStdGen 949488) :: (Bool, StdGen)
(False,1485632275 40692)
```

Por que `random` não sabe que deve retornar um booleano quando dizemos que queremos um arremesso de moeda?
Basicamente, porque não sabe que chamamos isso de "moeda".
Uma moeda tem dois valores, `heads` (cara) e `tails` (coroa), que mapeia bem para `Bool`.
E já temos `Bool`s como instâncias de `Random`.

Vamos fazer uma função que simula atirar uma moeda três vezes.

```{.haskell:hs}
threeCoins :: StdGen -> (Bool, Bool, Bool)
threeCoins gen =
    let (firstCoin, newGen) = random gen
        (secondCoin, newGen') = random newGen
        (thirdCoin, newGen'') = random newGen'
    in  (firstCoin, secondCoin, thirdCoin)
```

Nós chamamos de `random` com o gerador que recebemos como parâmetro para obter uma moeda e um novo gerador.
Então nós chamamos novamente, apenas com o novo gerador, para obter a segunda moeda.
Fazemos a mesma coisa com a terceira moeda.
Se tivéssemos chamado de "random" com o mesmo gerador todas as 3 vezes, todas as 3 moedas teriam o mesmo valor e seriam todas iguais.

```{.haskell:hs}
ghci> threeCoins (mkStdGen 21)
(True,True,True)
ghci> threeCoins (mkStdGen 22)
(True,False,True)
ghci> threeCoins (mkStdGen 943)
(True,False,True)
ghci> threeCoins (mkStdGen 944)
(True,True,True)
```

E se quisermos virar 4 moedas?
Ou 5?
Bem, existe uma função chamada `randoms`{.label .function} que assume um gerador e retorna uma lista infinita de valores com base nesse gerador.

```{.haskell:hs}
ghci> take 5 $ randoms (mkStdGen 11) :: [Int]
[-1807975507,545092095,-1015194702,-1622477312,-502893664]
ghci> take 5 $ randoms (mkStdGen 11) :: [Bool]
[True,True,True,True,False]
ghci> take 5 $ randoms (mkStdGen 11) :: [Float]
[7.904792e-2,0.62566846,0.25612664,0.46784747,0.3687355]
```

Por que `randoms` não retorna um novo gerador, além de uma lista?
Poderíamos implementar a função `randoms` com muita facilidade, assim:

```{.haskell:hs}
randoms' :: (RandomGen g, Random a) => g -> [a]
randoms' gen = let (value, newGen) = random gen in value : randoms' newGen
```

Uma definição recursiva.
Obtemos um valor aleatório e um novo gerador do gerador atual e, em seguida, fazemos uma lista que tem o valor como *cabeça* e números aleatórios gerados com o novo gerador como sua *cauda*.
Como temos que ser capazes de gerar uma quantidade infinita de números, não podemos devolver o novo gerador.

Também há a função `randomR`{.label .function}, que tem o tipo `randomR :: (RandomGen g, Random a) => (a, a) -> g -> (a, g)`.
É como `random`, mas leva como seu primeiro parâmetro um par de valores que estabelecem limites inferior e superior e o valor final produzido cai dentro desses limites.

```{.haskell:hs}
ghci> randomR (1,6) (mkStdGen 359353)
(6,1494289578 40692)
ghci> randomR (1,6) (mkStdGen 35935335)
(1,1250031057 40692)
```

Há também `randomRs`, que produz um fluxo de valores aleatórios dentro de um intervalo.
Confira isso:

```{.haskell:hs}
ghci> take 10 $ randomRs ('a','z') (mkStdGen 3) :: [Char]
"ndkxbvmomg"
```

Legal, parece uma senha super secreta ou algo assim.

Então você pode estar se perguntando: o que isso tem a ver com I/O?
Nós não produzimos nada até agora.
Bem, até agora, sempre criamos nosso gerador de números aleatórios manualmente com alguma semente (semente nada mais é do que um inteiro).
O problema é que, se fizermos isso em nossos programas reais, eles sempre devolverão os mesmos números aleatórios, o que não é bom para nós.
É por isso que `System.Random` oferece a ação de E/S `getStdGen`{.label .function}, que tem um tipo de `getStdGen :: IO StdGen`.
Quando seu programa começa, ele pede ao sistema um bom gerador de números aleatórios e o armazena no chamado gerador global.
`getStdGen` busca esse gerador global para você vinculá-lo a alguma coisa.

Aqui está um programa simples que gera uma string aleatória.

```{.haskell:hs}
import System.Random

main = do
    gen <- getStdGen
    putStr $ take 20 (randomRs ('a','z') gen)
```

```{.plain}
$ runhaskell random_string.hs
pybphhzzhuepknbykxoo
$ runhaskell random_string.hs
pybphhzzhuepknbykxoo
```

Epa!
Aqui na segunda execução imprimiu a mesma coisa.
Isso ocorre porque fizemos `getStdGen` duas vezes.
Na primeira vez, obtém o gerador global, na segunda vez recebe o gerador global.
O mesmo gerador, portanto, a mesma saída.
Para obter dois resultados diferentes, teríamos que atualizar o gerador global.
Fazemos isso, chocantemente, usando a função `newStdGen`{.label .function}, que divide nosso gerador atual em dois geradores novos (*splitting*).
Ele atualiza o gerador global com um deles e encapsula o outro como resultado.

```{.haskell:hs}
import System.Random

main = do
    gen <- getStdGen
    putStrLn $ take 20 (randomRs ('a','z') gen)
    gen2 <- newStdGen
    putStr $ take 20 (randomRs ('a','z') gen2)
```

```{.plain}
$ runhaskell random_string.hs
zjy svdvnlllguykhsae
ncsvkphonxryktfhbmct
```

Não apenas obtemos uma nova string aleatória quando vinculamos `newStdGen` a `gen2`, mas o gerador global é atualizado; portanto, se fizermos `getStdGen` novamente e a vincularmos a algo, teremos um gerador que não é o mesmo que `gen`.

Aqui está um pequeno programa que fará com que o usuário adivinhe qual é o número que está pensando.

```{.haskell:hs}
import System.Random
import Control.Monad(when)

main = do
    gen <- getStdGen
    askForNumber gen

askForNumber :: StdGen -> IO ()
askForNumber gen = do
    let (randomNumber, newGen) = randomR (1,10) gen :: (Int, StdGen)
    putStr "Which number in the range from 1 to 10 am I thinking of? "
    numberString <- getLine
    when (not $ null numberString) $ do
        let number = read numberString
        if randomNumber == number
            then putStrLn "You are correct!"
            else putStrLn $ "Sorry, it was " ++ show randomNumber
        askForNumber newGen
```

Nós criamos uma função `askForNumber`, que recebe um gerador e retorna uma ação de E/S que solicitará um número ao usuário e dirá se ele adivinhou certo.
Nessa função, primeiro geramos um número aleatório e um novo gerador com base no gerador que obtivemos como parâmetro e os chamamos de `randomNumber` e `newGen`.
Dizemos que é um `Int`, porque `read` não saberia de outra forma para o que ler.
Em seguida, obtemos a entrada do usuário e a transformamos em um número.
Verificamos se o número que eles inseriram é igual ao número gerado aleatoriamente e damos a mensagem apropriada.
E então chamamos `askForNumber` recursivamente, apenas desta vez com o novo gerador que recebemos, o que nos dá um novo número.
No `main`, apenas obtemos um gerador padrão e chamamos `askForNumber`.
Observe que apenas verificamos se a entrada do usuário é nula (`null`) ou não.

```{.plain}
$ runhaskell guess_the_number.hs
Which number in the range from 1 to 10 am I thinking of? 4
Sorry, it was 3
Which number in the range from 1 to 10 am I thinking of? 10
You are correct!
Which number in the range from 1 to 10 am I thinking of? 2
Sorry, it was 4
Which number in the range from 1 to 10 am I thinking of?
```

Outra maneira de fazer o mesmo é assim:

```{.haskell:hs}
import System.Random
import Control.Monad(when)

main = do
    gen <- getStdGen
    let (randomNumber, _) = randomR (1,10) gen :: (Int, StdGen)
    putStr "Which number in the range from 1 to 10 am I thinking of? "
    numberString <- getLine
    when (not $ null numberString) $ do
        let number = read numberString
        if randomNumber == number
            then putStrLn "You are correct!"
            else putStrLn $ "Sorry, it was " ++ show randomNumber
        newStdGen
        main
```

É muito semelhante ao anterior, apenas em vez de fazer uma função que pega um gerador e depois se chama recursivamente com o novo gerador, fazemos todo o trabalho no `main`.
Depois de informar ao usuário se eles estão corretos ou não, atualizamos o gerador global e depois chamamos o `main` novamente.
Tanto a abordagem anterior quanto essa estão bem.

`newStdGen` é legal porque não precisamos passar um gerador para as funções ou fazer as coisas no `main` apenas para garantir que obteremos um novo número aleatório na próxima vez.

## Bytestrings {#bytestrings}

Listas são uma estrutura de dados legal e útil.
A maioria que encontramos até agora é uma lista.
Haskell `String`s são apenas listas de caracteres.
Seus tipos de declarações são as mesmas e também as funções para processamento.
Então, `String` é apenas uma lista preguiçosa.
Isso é útil porque podemos interagir com essas strings por quanto tempo quisermos sem carregá-las de uma vez na memória...
Mas também há uma sobrecarga associada a isso.
Isso porque um `Int` em uma lista não é apenas um número, mas, na verdade, uma promessa (*thunk*) para um número (porque Haskell é preguiçoso).
Essa promessa pode ser avaliada para o número, apenas não sabemos quando.

Muitas vezes, essa sobrecarga não nos incomoda, mas acontece quando lemos arquivos grandes ou manipulamos grandes strings, isso nos deixa mais lentos.
É aí que os **Bytestrings** entram.
Bytestrings são parecidos com uma lista, mas cada elemento é um byte (ou um número de 8 bits), e a maneira como eles lidam com a preguiça é diferente.

Bytestrings vêm em dois sabores: estritos (strict) e preguiçosos (lazy).
Bytestrings estritos residem em `Data.ByteString` e são uma série de bytes em uma matriz.
Você não pode avaliá-los preguiçosamente.
Não haveria thunks (promessas de computação).
A vantagem é que há muito menos sobrecarga; desvantagem é que eles preenchem sua memória mais rapidamente.

O outro tipo de bytestrings reside em `Data.ByteString.Lazy`.
Eles são preguiçosos, mas não tão preguiçosos quanto as listas.
Em uma lista, não há tanta diferença entre ter 1 elemento ou 200 elementos: eles são apenas promessas.
Uma bytestring preguiçosa é armazenada em pedaços (64K), onde cada pedaço tem um tamanho fixo.
Bytestrings preguiçosos são listas de pedaços de 64K.
Quando você avalia um byte em uma string preguiçosa (pelo `L.head`, por exemplo), o primeiro pedaço é avaliado.
Depois disso, é apenas uma lista de pedaços!
Portanto, a estrutura de dados é um pouco diferente da das listas, mas não muito.
Isso permite processar um arquivo muito rapidamente e também usá-lo preguiçosamente.
A sobrecarga também não é tão ruim, pois os dados são armazenados em pedaços e, portanto, há apenas uma sobrecarga para o "ponteiro de dados" por pedaço.

Se você olhar a documentação em `Data.ByteString.Lazy`, verá que ele tem muitas funções com os mesmos nomes que as de `Data.List`, apenas as assinaturas de tipo têm `ByteString` e `Word8` em vez de `[a]` e `a`.
Essas funções têm os mesmos nomes que as funções de lista, então vamos importá-lo como `qualfied`.

```{.haskell:hs}
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString as S
```

`B` contém tipos e funções de bytestring preguiçosos, enquanto `S` contém os estritos.
Usaremos principalmente a versão preguiçosa.

A função `pack`{.label .function} tem a assinatura de tipo `pack :: [Word8] -> ByteString`.
Isso significa que é preciso uma lista de bytes do tipo `Word8` e retorna uma `ByteString`.
Você pode pensar nela como uma função que pega uma lista, que é preguiçosa, e a torna menos preguiçosa, de modo que é preguiçosa apenas em intervalos de 64k.
Qual é o tipo `Word8`?
Bem, é como `Int`, só que tem um intervalo muito menor, ou seja, 0-255.
Representa um número de 8 bits.
E, assim como `Int`, está na classe de tipos `Num`.
Por exemplo, sabemos que o valor `5` é polimórfico, pois pode ser qualquer tipo numérico.
Bem, pode ser `Word8` também.

```{.haskell:hs}
ghci> B.pack [99,97,110]
Chunk "can" Empty
ghci> B.pack [98..120]
Chunk "bcdefghijklmnopqrstuvwx" Empty
```

Como você pode ver, geralmente não precisamos nos preocupar com o `Word8`, pois o sistema de tipos pode fazer com que os números escolham esse tipo.
Se você tentar criar esse caractere como um número inteiro maior que 255, ele dará a volta (*wrap around*).

```{.haskell:hs}
ghci> B.pack [336, 337, 338]
Chunk "PQR" Empty
```

Também podemos descompactar (`unpack`{.label .function}) uma `ByteString` em uma lista de bytes.

```{.haskell:hs}
ghci> B.unpack $ B.pack [97..122]
[97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122]
```

`fromChunks`{.label .function} pega uma lista de bytestrings estritas e a converte em uma bytestring preguiçosa.
`toChunks`{.label .function} pega uma bytestring preguiçosa e a converte em uma lista de estritas.

```{.haskell:hs}
ghci> B.fromChunks [S.pack [40,41,42], S.pack [43,44,45], S.pack [46,47,48]]
Chunk "()*" (Chunk "+,-" (Chunk "./0" Empty))
```

Isso é bom se você tiver muitos bytestrings pequenos e deseja processá-los com eficiência (colocando-os em pedaços em uma única bytestring preguiçosa).

A versão bytestring de `:` é chamada `cons`{.label .function}.
Leva um byte e uma bytestring e coloca o byte no começo.
É preguiçoso, por isso fará um novo pedaço, mesmo que o primeiro pedaço na cadeia não esteja cheio.
É por isso que é melhor usar a versão estrita do `cons`, `cons'`{.label .function} se você estiver inserindo muitos bytes.

```{.haskell:hs}
ghci> B.cons 85 $ B.pack [80,81,82,84]
Chunk "U" (Chunk "PQRT" Empty)
ghci> B.cons' 85 $ B.pack [80,81,82,84]
Chunk "UPQRT" Empty
ghci> foldr B.cons B.empty [50..60]
Chunk "2" (Chunk "3" (Chunk "4" (Chunk "5" (Chunk "6" (Chunk "7" (Chunk "8" (Chunk "9" (Chunk ":" (Chunk ";" (Chunk "<" Empty))))))))))
ghci> foldr B.cons' B.empty [50..60]
Chunk "23456789:;<" Empty
```

Como você pode ver, `empty` faz uma bytestring vazia.
Veja a diferença entre `cons` e `cons'`?
Com o `cons`, temos um pedaço para cada byte.
Com o `cons'`, temos apenas um pedaço com todos os bytes.

O módulo bytestring tem uma carga de funções que são análogas às em `Data.List`, incluindo, mas não se limitando a, `head`, `tail`, `init`, `null`, `length`, `map`, `reverse`, `foldl`, `foldr`, `concat`, `takeWhile`, `filter`, etc.

Ele também tem funções que têm o mesmo nome e se comportam da mesma forma que algumas funções encontradas em `System.IO`, apenas as `Strings` são substituídas por `ByteString`s.
Por exemplo, o `readFile` em `System.IO` é do tipo `readFile :: FilePath -> IO String`, enquanto o do módulo de bytestrings é do tipo `readFile :: FilePath -> IO ByteString`.
Cuidado, se você usar uma versão estrita de bytestring, ele tentará ler um arquivo na memória de uma só vez!
Com o bytestrings preguiçosos, ele lerá em pedaços.

Vamos fazer um programa simples que aceite dois nomes de arquivo como argumentos de linha de comando e copia o primeiro arquivo para o segundo arquivo.
Observe que `System.Directory` já tem uma função chamada `copyFile`, mas vamos implementar nossa própria função e usar bytreeStrings para que seja mais eficiente.

```{.haskell:hs}
import System.Environment
import qualified Data.ByteString.Lazy as B

main = do
    (fileName1:fileName2:_) <- getArgs
    copyFile fileName1 fileName2

copyFile :: FilePath -> FilePath -> IO ()
copyFile source dest = do
    contents <- B.readFile source
    B.writeFile dest contents
```

Fazemos nossa própria função que recebe dois `FilePath`s (lembre-se, `FilePath` é apenas sinônimo de `String`) e retorna uma ação de E/S que copiará um arquivo para outro usando bytestring.
Na função `main`, apenas obtemos os argumentos e chamamos nossa função com eles.

```{.plain}
$ ghc --make bytestringcopy.hs
[1 of 1] Compiling Main             ( bytestringcopy.hs, bytestringcopy.o )
Linking bytestringcopy ...
$ ./bytestringcopy something.txt ../../something.txt
```

Observe que um programa que não usa bytestrings poderia se parecer com isso:

```{.haskell:hs}
import System.Environment
import System.IO

main = do
    (fileName1:fileName2:_) <- getArgs
    copyFile fileName1 fileName2

copyFile :: FilePath -> FilePath -> IO ()
copyFile source dest = do
    contents <- readFile source
    writeFile dest contents
```

A diferença é que usamos `B.readFile` e `B.writeFile` em vez de `readFile` e `writeFile`.
Muitas vezes, você pode converter um programa que usa strings para um programa que usa bytestrings, apenas importando os módulos necessários e, em seguida, colocando o nome do módulo qualificado na frente de algumas funções.
Às vezes, você precisa converter strings para bytestrings e vice-versa, mas isso não é difícil.

Sempre que precisar ler muitos dados de um arquivo binário (como um arquivo de som ou uma imagem) ou um arquivo de texto enorme e a implementação normal da lista estiver lhe dando problemas de desempenho, tente usar bytestrings.

## Exceções (Exceptions) {#exceptions}

![bomb](assets/images/input-and-output/bomb.png){.right width=223 height=293}

Todas as linguagens têm procedimentos, funções e pedaços de código que podem falhar.
Isso é um fato da vida.
As coisas diferentes têm maneiras diferentes de lidar com essas falhas.
Em C, geralmente usamos algum valor de retorno anormal (como `-1` ou um ponteiro nulo) para indicar que o que tentamos fazer falhou.
Java e C#, por outro lado, suportam exceções.
Quando uma exceção é lançada, o fluxo de controle salta para algum código de tratamento de exceção que definimos e faz alguma limpeza (cleanup), e talvez relance a exceção para que algum outro código de manipulação de erro possa cuidar disso também.

Haskell tem um sistema de tratamento de exceções muito bom.
Exceções podem ser lançadas de código puro ou de código impuro.
No caso de código puro, as exceções só podem ser capturadas na parte de E/S do nosso código (porque não sabemos quando (ou se) algo será avaliado no código puro, pois é preguiçoso e não tem uma ordem de execução bem definida, enquanto o código de E/S tem).
As exceções lançadas no código de E/S podem ser capturadas no código de E/S.
No entanto, na maioria das vezes, não temos muito o que fazer quando nosso código puro lança uma exceção.
Pegue a função `div`, por exemplo: ela gera como resultado a divisão inteira de seus argumentos.
O `div 4 2` retorna `2`, mas o `div 4 0` lançará uma exceção dizendo que a divisão por zero aconteceu.
Como o `div` é puro, não existe uma ordem bem definida de quando ele será avaliado, portanto, não sabemos quando e onde pegar essa exceção.
Se tivermos que verificar erros, geralmente usamos tipos como `Maybe` e `Either` para indicar valores que são computados com sucesso ou com falha.

Portanto, em Haskell, geralmente usamos exceções para as partes de E/S em nosso programa.
Muitas coisas podem dar errado ao lidar com o mundo exterior, porque não é confiável.
Por exemplo, tentamos abrir um arquivo e acontece que ele foi excluído ou não temos permissões para abri-lo e assim por diante.

Vimos como podemos apenas usar funções como `doesFileExist` de `System.Directory` para verificar se um arquivo existe, então não precisamos lidar com a abertura de um arquivo que não existe.
Usamos `doesFileExist`, verificamos se é `True` ou `False` e depois fazemos a coisa certa.
Mas e se esse arquivo realmente existir e quando vamos a abri-lo, ele não existe mais?
Não há certeza de que algum outro processo não o excluiu no tempo entre a verificação de existência e a abertura do arquivo.
É por isso que é melhor usar exceções nesse caso.
As exceções também são boas se o procedimento puder falhar de várias maneiras e queremos apenas ter um manipulador que pegue (catch) todas elas e faça alguma coisa.

Aqui está um código que abre um arquivo e conta quantas linhas há nele.

```{.haskell:hs}
import System.Environment
import System.IO

main = do (fileName:_) <- getArgs
          contents <- readFile fileName
          putStrLn $ "The file has " ++ show (length (lines contents)) ++ " lines!"
```

Isso funciona bem, a menos que o arquivo não exista ou tenhamos apenas uma lista vazia de argumentos.

```{.plain}
$ ghc --make linecount
[1 of 1] Compiling Main             ( linecount.hs, linecount.o )
Linking linecount ...
$ ./linecount linecount.hs
The file has 7 lines!
$ ./linecount i_dont_exist.txt
linecount: i_dont_exist.txt: openFile: does not exist (No such file or directory)
```

Nós temos uma mensagem de erro do GHC dizendo que o arquivo não existe.
O erro aconteceu quando tentamos abrir o arquivo usando `readFile`, mas note que `readFile` não abre o arquivo quando a chamamos, mas apenas retorna uma ação de E/S que abrirá o arquivo.
A exceção foi lançada quando essa ação de E/S foi realizada.
Para corrigir esse erro, podemos usar a função `catch`{.label .function} do `System.IO.Error`.
Seu tipo é `catch :: IO a -> (IOError -> IO a) -> IO a`.
Leva duas ações de E/S.
A primeira é a ação que tentaremos executar.
A segunda é o *manipulador* (*handler*).
Se a primeira ação lançar uma exceção, ela será passada para o manipulador, que então decide o que fazer.
Portanto, a ação que `catch` retorna fará o mesmo que a primeira ação que passamos a ela, mas se algo der errado, executará o manipulador.
Vamos modificar nosso programa para usar `catch`.

```{.haskell:hs}
import System.Environment
import System.IO
import System.IO.Error

main = toTry `catch` handler

toTry :: IO ()
toTry = do (fileName:_) <- getArgs
           contents <- readFile fileName
           putStrLn $ "The file has " ++ show (length (lines contents)) ++ " lines!"

handler :: IOError -> IO ()
handler e = putStrLn "Whoops, had some trouble!"
```

Como você pode ver, em vez de fazer o que fizemos normalmente, chamamos a função principal `catch` com `toTry` e `handler`.
`toTry` é a ação de E/S que tentamos executar e `handler` é a função que recebe um `IOError` e retorna uma ação a ser executada no caso de uma exceção.
Vamos ver se funciona.

```{.plain}
$ ./linecount linecount.hs
The file has 7 lines!
$ ./linecount i_dont_exist.txt
Whoops, had some trouble!
```

No manipulador, não verificamos que tipo de `IOError` obtivemos.
Nós apenas dizemos "Ops, tive alguns problemas" para qualquer tipo de erro.
Assim como as definições de exceção em outras linguagens, como Java, as exceções em Haskell têm uma hierarquia, de modo que podemos verificar que tipo de erro ocorreu.
Uma coisa útil que podemos fazer é verificar se o erro que obtivemos é um erro de "arquivo não existente" (*does not exist error*).
Se for, dizemos ao usuário que o arquivo não existe, mas se não for, lançamos a exceção novamente e deixamos que ela faça o que quiser (como travar nosso programa).
Aqui está o manipulador atualizado:

```{.haskell:hs}
handler :: IOError -> IO ()
handler e
    | isDoesNotExistError e = putStrLn "The file doesn't exist!"
    | otherwise = ioError e
```

A função `isDoesNotExistError`{.label .function} é um predicado sobre `IOError`s, o que significa que pega um `IOError` e retorna `True` ou `False`.
Se for um erro de que o arquivo não existe, retornamos uma ação que imprime uma mensagem agradável.
Caso contrário, usamos a função `ioError`{.label .function}, que pega um `IOError` e produz uma ação de E/S que lança esse erro.
A ação terá o mesmo resultado que a ação que falhou, mas nós a lançamos novamente para que o usuário (ou o sistema operacional) saiba que algo de estranho aconteceu.
A função `ioError` tem o tipo `ioError :: IOError -> IO a`, então o tipo `a` pode ser o que quisermos.
Isso ocorre porque `ioError` não produzirá realmente um resultado, mas lançará uma exceção.

Existem vários predicados úteis que operam no `IOError`:

*   `isAlreadyExistsError`
*   `isDoesNotExistError`
*   `isAlreadyInUseError`
*   `isFullError`
*   `isEOFError`
*   `isIllegalOperation`
*   `isPermissionError`
*   `isUserError`

A maioria deles é autoexplicativa.
`isUserError` avalia como `True` quando usamos a função `userError`{.label .function} para fazer exceções, que é para fazer exceções a partir do nosso próprio código e passá-las com `ioError`.
Por exemplo, você pode fazer `ioError $ userError "remote computer unplugged!"`.

Também podemos usar funções que começam com `ioe` para obter alguns dados de um `IOError`.

*   `ioeGetFileName` :: IOError -> Maybe FilePath
*   `ioeGetHandle` :: IOError -> Maybe Handle
*   `ioeGetLocation` :: IOError -> String

Vamos usar `ioeGetFileName` para imprimir o caminho do arquivo que causou o erro.

```{.haskell:hs}
handler :: IOError -> IO ()
handler e
    | isDoesNotExistError e =
        case ioeGetFileName e of Just path -> putStrLn $ "Whoops! File does not exist at: " ++ path
                                 Nothing -> putStrLn "Whoops! File does not exist at unknown location!"
    | otherwise = ioError e
```

Tivemos que usar a expressão `case` aqui para verificar o `path` que estava dentro do `Maybe` retornado por `ioeGetFileName`.
Geralmente, o `IOError` contém o caminho do arquivo que causou o erro.

Exceções são legais, mas você não deve exagerar nelas.
Ainda assim, ao lidar com E/S e coisas que podem falhar a vontade, elas podem ser muito úteis.
Em código puro, tente usar tipos como `Either` e `Maybe` para representar resultados que podem ter falhado.
Isso porque, com o uso de tipos, o verificador de tipos (que é seu melhor amigo) pode ajudá-lo a encontrar erros no tempo da compilação e, mesmo que seu código seja compilado, você tem certeza de que lidou com tudo.

Parabéns, você agora sabe como lidar com E/S em Haskell!
Embora, à primeira vista, lidar com E/S possa parecer estranho, porque requer que encadeemos ações de E/S, no final não é tão difícil e nos dá a capacidade de separar as partes puras de nossos programas das impuras.
A seguir, falaremos mais sobre mônadas, que tornarão o E/S, o tratamento de erros e muitas outras coisas mais claras.
Você provavelmente também vai finalmente entender o que são aqueles blocos `do` e por que essa sintaxe funciona.
Além disso, aprender mônadas é como obter o privilégio de "usuário nível 10" em Haskell.
Então vá em frente e leia!




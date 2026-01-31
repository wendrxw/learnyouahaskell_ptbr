# Introdução

## Sobre este tutorial {#about-this-tutorial}

Bem-vindo ao **Learn You a Haskell for Great Good**!
Se você está lendo isso, é provável que queira aprender Haskell.
Bem, você veio ao lugar certo, mas vamos falar um pouco sobre este tutorial primeiro.

Decidi escrever isso porque queria solidificar meu próprio conhecimento de Haskell e porque pensei que poderia ajudar pessoas novas em Haskell a aprendê-lo da minha perspectiva.
Existem alguns tutoriais sobre Haskell flutuando na internet.
Quando eu estava começando em Haskell, não aprendi com apenas um recurso.
A maneira como aprendi foi lendo vários tutoriais e artigos diferentes, porque cada um explicava algo de uma maneira diferente do outro.
Ao examinar vários recursos, consegui juntar as peças e tudo se encaixou.
Portanto, esta é uma tentativa de adicionar outro recurso útil para aprender Haskell, para que você tenha uma chance maior de encontrar um que goste.

![bird](assets/images/introduction/bird.png){.left width=230 height=192}

Este tutorial é voltado para pessoas que têm experiência em linguagens de programação imperativas (C, C++, Java, Python ...), mas que não programaram em uma linguagem funcional antes (Haskell, ML, OCaml ...).
Embora eu aposte que, mesmo se você não tiver nenhuma experiência significativa em programação, uma pessoa inteligente como você será capaz de acompanhar e aprender Haskell.

O canal #haskell na rede Libera.Chat é um ótimo lugar para tirar dúvidas se você estiver se sentindo travado.
As pessoas lá são extremamente gentis, pacientes e compreensivas com os novatos.
Se o IRC não é sua praia, [https://discourse.haskell.org](https://discourse.haskell.org) é um fórum da comunidade popular com uma seção para aprendizes.

Eu falhei em aprender Haskell aproximadamente 2 vezes antes de finalmente compreendê-lo, porque tudo parecia muito estranho para mim e eu não entendia.
Mas então, uma vez que tudo "clicou" e depois de superar aquele obstáculo inicial, foi praticamente tudo tranquilo.
Acho que o que estou tentando dizer é: Haskell é ótimo e se você está interessado em programação, deveria realmente aprendê-lo, mesmo que pareça estranho no começo.
Aprender Haskell é muito parecido com aprender a programar pela primeira vez --- é divertido!
Isso obriga você a pensar de forma diferente, o que nos leva à próxima seção...

## Então, o que é Haskell? {#so-whats-haskell}

![fx](assets/images/introduction/fx.png){.right width=150 height=146}
Haskell é uma **linguagem de programação puramente funcional**.
Em linguagens imperativas, você realiza as coisas dando ao computador uma sequência de tarefas e, em seguida, ele as executa.
Ao executá-las, ele pode mudar de estado.
Por exemplo, você define a variável `a` como 5, faz algumas coisas e depois a define como outra coisa.
Você tem estruturas de fluxo de controle para fazer alguma ação várias vezes.
Na programação puramente funcional, você não diz ao computador o que fazer como tal, mas diz a ele o que as coisas *são*.
O fatorial de um número é o produto de todos os números de 1 até esse número, a soma de uma lista de números é o primeiro número mais a soma de todos os outros números, e assim por diante.
Você expressa isso na forma de funções.
Você também não pode definir uma variável como algo e depois defini-la como outra coisa mais tarde.
Se você diz que `a` é 5, não pode dizer que é outra coisa depois, porque acabou de dizer que era 5.
O que você é, algum tipo de mentiroso?
Portanto, em linguagens puramente funcionais, uma função não tem efeitos colaterais.
A única coisa que uma função pode fazer é calcular algo e retorná-lo como resultado.
A princípio, isso parece meio limitante, mas na verdade tem algumas consequências muito boas: se uma função for chamada duas vezes com os mesmos parâmetros, é garantido que retornará o mesmo resultado.
Isso é chamado de transparência referencial e não apenas permite que o compilador raciocine sobre o comportamento do programa, mas também permite que você deduza facilmente (e até prove) que uma função está correta e, em seguida, construa funções mais complexas colando funções simples umas nas outras.

![lazy](assets/images/introduction/lazy.png){.right width=240 height=209}
Haskell é **preguiçoso (lazy)**.
Isso significa que, a menos que seja especificamente instruído de outra forma, o Haskell não executará funções e calculará coisas até que seja realmente forçado a mostrar um resultado.
Isso combina bem com a transparência referencial e permite que você pense nos programas como uma série de **transformações em dados**.
Também permite coisas legais, como estruturas de dados infinitas.
Digamos que você tenha uma lista imutável de números `xs = [1,2,3,4,5,6,7,8]` e uma função `doubleMe` que multiplica cada elemento por 2 e depois retorna uma nova lista.
Se quiséssemos multiplicar nossa lista por 8 em uma linguagem imperativa e fizéssemos `doubleMe(doubleMe(doubleMe(xs)))`, provavelmente passaria pela lista uma vez e faria uma cópia e depois a retornaria.
Em seguida, passaria pela lista mais duas vezes e retornaria o resultado.
Em uma linguagem preguiçosa, chamar `doubleMe` em uma lista sem forçá-la a mostrar o resultado acaba com o programa meio que dizendo "Sim, sim, farei isso mais tarde!".
Mas assim que você quiser ver o resultado, o primeiro `doubleMe` diz ao segundo que quer o resultado, agora!
O segundo diz isso ao terceiro e o terceiro relutantemente devolve um 1 dobrado, que é um 2.
O segundo recebe isso e devolve 4 para o primeiro.
O primeiro vê isso e diz que o primeiro elemento é 8.
Portanto, ele faz apenas uma passagem pela lista e somente quando você realmente precisa.
Dessa forma, quando você quer algo de uma linguagem preguiçosa, pode apenas pegar alguns dados iniciais e transformá-los e corrigi-los com eficiência para que se assemelhem ao que você deseja no final.

![boat](assets/images/introduction/boat.png){.right width=160 height=153}
Haskell é **estaticamente tipado**.
Quando você compila seu programa, o compilador sabe qual pedaço de código é um número, qual é uma string e assim por diante.
Isso significa que muitos erros possíveis são detectados em tempo de compilação.
Se você tentar somar um número e uma string, o compilador reclamará com você.
Haskell usa um sistema de tipos muito bom que possui **inferência de tipos**.
Isso significa que você não precisa rotular explicitamente cada pedaço de código com um tipo, porque o sistema de tipos pode descobrir de forma inteligente muito sobre ele.
Se você disser `a = 5 + 4`, não precisa dizer a Haskell que `a` é um número, ele pode descobrir isso sozinho.
A inferência de tipos também permite que seu código seja mais geral.
Se uma função que você cria recebe dois parâmetros e os soma e você não declara explicitamente seu tipo, a função funcionará em quaisquer dois parâmetros que ajam como números.

Haskell é **elegante e conciso**.
Como usa muitos conceitos de alto nível, os programas Haskell geralmente são mais curtos que seus equivalentes imperativos.
E programas mais curtos são mais fáceis de manter do que os mais longos e têm menos bugs.

Haskell foi feito por um **pessoal muito inteligente** (com PhDs).
O trabalho em Haskell começou em 1987, quando um comitê de pesquisadores se reuniu para projetar uma linguagem incrível.
Em 2003, o Haskell Report foi publicado, o que define uma versão estável da linguagem.

## O que você precisa para começar {#what-you-need}

Um editor de texto e um compilador Haskell.
Você provavelmente já tem seu editor de texto favorito instalado, então não perderemos tempo com isso.
Para os fins deste tutorial, usaremos o GHC, o compilador Haskell mais amplamente utilizado.
A melhor maneira de começar é baixar o [GHCup](https://www.haskell.org/ghcup/), que é o instalador Haskell recomendado.

O GHC pode pegar um arquivo Haskell (eles geralmente têm uma extensão .hs) e compilá-lo, mas também possui um modo interativo que permite interagir interativamente com arquivos.
Interativamente.
Você pode chamar funções de arquivos que carrega e os resultados são exibidos imediatamente.
Para aprender, é muito mais fácil e rápido do que compilar toda vez que você faz uma alteração e depois executar o programa no prompt.
O modo interativo é invocado digitando `ghci` no seu prompt.
Se você definiu algumas funções em um arquivo chamado, digamos, `myfunctions.hs`, você carrega essas funções digitando `:l myfunctions` e, em seguida, pode brincar com elas, desde que `myfunctions.hs` esteja na mesma pasta de onde o `ghci` foi invocado.
Se você alterar o arquivo .hs, basta executar `:l myfunctions` novamente ou fazer `:r`, o que é equivalente porque recarrega o arquivo atual.
O fluxo de trabalho usual para mim ao brincar com as coisas é definir algumas funções em um arquivo .hs, carregá-lo e mexer com elas e depois alterar o arquivo .hs, carregá-lo novamente e assim por diante.
Isso também é o que faremos aqui.

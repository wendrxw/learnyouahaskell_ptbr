# Haskell Documentation Reader

Este é um leitor moderno de documentação Haskell escrito em Haskell usando Scotty e Pandoc.

## Pré-requisitos

Você precisa ter o **GHC** (Haskell Compiler) e o **Cabal** instalados. A maneira recomendada é através do [GHCup](https://www.haskell.org/ghcup/).

## Como Rodar

1.  Abra o terminal na pasta do projeto:
    ```bash
    cd /home/wendrew/Documentos/learnyouahaskellptbr/haskell-reader
    ```

2.  Atualize a lista de pacotes (opcional, mas recomendado):
    ```bash
    cabal update
    ```

3.  Compile e execute o projeto:
    ```bash
    cabal run
    ```

    *O Cabal irá baixar automaticamente as dependências (scotty, pandoc, etc.) na primeira vez.*

4.  Acesse no seu navegador:
    [http://localhost:3000](http://localhost:3000)

## Estrutura do Projeto

- `Main.hs`: Lógica do servidor e rotas.
- `src/Files.hs`: Manipulação de arquivos .md.
- `src/Markdown.hs`: Conversão de Markdown para HTML.
- `src/Templates.hs`: Design e interface (UI).
- `docs/`: Pasta contendo os arquivos .md traduzidos.

## Customização

Você pode adicionar novos arquivos `.md` na pasta `docs/` e eles aparecerão automaticamente na barra lateral do leitor.

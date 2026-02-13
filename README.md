# Aprenda Haskell para o Bem de Todos! (Tradução PT-BR)

Este projeto é uma tradução para o Português Brasileiro (PT-BR) do renomado tutorial **"Learn You a Haskell for Great Good!"**.

O repositório foi simplificado para priorizar os arquivos Markdown: você edita os capítulos diretamente na raiz e gera a versão estática para o GitHub Pages com um comando simples.

---

## 📖 Sobre o Projeto Original

O **Learn You a Haskell for Great Good! (LYAH)** é um dos guias mais populares e acessíveis para aprender Haskell.

- **Autor Original:** Miran Lipovača ([@learnyouahaskell](https://github.com/learnyouahaskell))
- **Site Original:** [learnyouahaskell.com](http://learnyouahaskell.com/)
- **Mantenedores da Versão Comunitária:** Baseado em [learnyouahaskell.github.io](https://github.com/learnyouahaskell/learnyouahaskell.github.io).

---

## 🛠️ Como Funciona (Workflow Simplificado)

Este repositório segue uma estrutura "flat":

1.  **Edição:** Os capítulos traduzidos estão na raiz (`introduction.md`, `starting-out.md`, etc.).
2.  **Build:** O script `build.hs` transpila esses arquivos para HTML usando o estilo clássico do LYAH.
3.  **Deploy:** Os arquivos gerados ficam na pasta `docs/`, que o GitHub Pages usa para renderizar o site.

### Pré-requisitos
Você precisa do **GHC** e do **Cabal** (instalados via **GHCup**).

### Gerar o Site (Build)
Para atualizar o site na pasta `docs/` após fazer edições nos Markdowns:
```bash
cabal run build-site
```

---

## 📂 Estrutura Simplificada

- `*.md`: Capítulos traduzidos (edite estes arquivos).
- `assets/`: Imagens e estilos originais.
- `sh/`: Scripts de Syntax Highlighting original.
- `build.hs`: Script consolidado de geração do site.
- `docs/`: (Gerado) Site final pronto para GitHub Pages.

---

## 🤝 Créditos

- Conteúdo original por **Miran Lipovača**.
- Tradução e adaptação técnica para PT-BR por **wendrxw**.

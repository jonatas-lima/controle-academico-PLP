## Executando
<p>1. Clone o repositório e entre na pasta do projeto </p>

```
git clone https://github.com/jonatas-lima/controle-academico-PLP.git
cd controle-academico-PLP
```

<p>2. Para compilar e executar o programa algumas bibliotecas precisam ser instaladas com antecedência. Seguem abaixo os comandos para instalação dessas bibliotecas.</p>

```
cabal install split
cabal install strict
cabal install ansi-terminal
```
<p>3. Após instalar as bibliotecas, entre na pasta 'funcional' e execute:</p>

```
cd funcional
runhaskell Main.hs
```
<!-- Controle Acadêmico or Img -->

* [PROJETO](#controle-acadêmico)  
* [FUNCIONALIDADES](#funcionalidades) 
* [FERRAMENTAS](#ferramentas) 
* [TIME](#time)

## Controle Acadêmico
<p> O sistema simula o funcionamento do controle academico, contendo as funcionalidades e restrições típicas de um CRUD. As três entidades chave do projeto são aluno, professor e admin, são eles três os possíveis usuários do sistema.
Professor e aluno se relacionam através das disciplinas e o admin é o responsável por ter o controle geral sobre as demais entidades do sistema.</p>

## Ferramentas
<p>Para compilar e rodar o programa algumas bibliotecas precisam ser instaladas com antecedência. Seguem abaixo os comandos para instalação dessas bibliotecas.</p>

```
cabal install split
cabal install strict
cabal install ansi-terminal
```

## Funcionalidades
* **Aluno**
&nbsp;<p>O aluno é capaz de: </p>
  * Visualizar as disciplinas matriculadas. 
  * Realizar matrícula em novas disciplinas.
  * Cancelar a matrícula em uma cadeira que ele está matriculado. 
  * Visualizar a média geral levando em consideração todas as disciplinas matriculadas.
* **Professor**
&nbsp;<p>O professor é capaz de: </p>
  * Visualizar as disciplinas lecionadas.
  * Registrar aulas e provas dessas disciplinas.
  * Consultar a situação de cada disciplina lecionada.
* **Admin**
&nbsp;<p>O admin é capaz de: </p>
  * Cadastrar professores, alunos e disciplinas no sistema. 
  * Associar professores a disciplinas. 
  * Listar alunos sem matrículas e professores sem disciplinas. 
  * Verificar as disciplinas com maior e menor média.
 
## Time
| [<img src="https://avatars.githubusercontent.com/u/62180037?v=4" width="120px;" /><br /><sub><b>Jonatas Lima</b></sub>](https://github.com/jonatas-lima)<br /> | [<img src="https://avatars.githubusercontent.com/u/56925275?v=4 " width="120px;"/><br /><sub><b>Kleber Sobrinho</b></sub>](https://github.com/kleberfsobrinho)<br /> | [<img src="https://avatars.githubusercontent.com/u/56238268?v=4" width="120px;"/><br /><sub><b>João Vitor Moura</b></sub>](https://github.com/joaovmoura)<br /> | [<img src="https://avatars.githubusercontent.com/u/50924659?v=4" width="120px;"/><br /><sub><b>Bernard Odon</b></sub>](https://github.com/bernardodon)<br> | [<img src="https://avatars.githubusercontent.com/u/55746103?v=4" width="120px;"/><br /><sub><b>Vinícius Trindade</b></sub>](https://github.com/viniciustrr)<br /> |
| :---: | :---: | :---: | :---: | :---: |


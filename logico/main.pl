
main:- 
    write("Bem-Vindo(a)!\nPara acessar o controle, faça login:\n\n"),
    login(),
    halt.

login:-
    write("Digite sua matrícula:"),
    read(Matricula),
    write("Digite sua senha:"),
    read(Senha).
    %autenticar(Matricula,Senha).


%essa funcao deve verificar com os arquivos se matricula e senha batem e caso sim abrir a função screen e caso nao ele volta para o login
%id é o id de identificação do UserId dele
autenticar(Matricula,Senha) :-
    verificadoComSucesso , screen(UserId) ; login().

screen("prof"):- 
    professorScreen(ID).

screen("admin"):- 
    adminScreen().

screen("aluno"):- 
    studentScreen(ID).

studentScreen(ID):-
    %ler arquivos e pegar nome do aluno e printar
    studentOptions(ID).

studentOptions(ID):-
    nl,nl,
    write("1) Visualizar disciplinas"),nl,
    write("2) Realizar matrícula"),nl,
    write("3) Cancelar matrícula"),nl,
    write("4) Visualizar média geral"),nl,
    write("(S)air do sistema"),nl,
    read(Inp),
    studentPanel(Inp,ID).

studentPanel(1,ID).
    %UI visualizar disciplinas

studentPanel(2,ID).
    %UI Realizar matrícula

studentPanel(3,ID).
    %UI Cancelar matrícula

studentPanel(4,ID).
    %UI visualizar media geral

studentPanel("S",ID).
    %sair do sistema

studentPanel("s",ID).
    %sair do sistema

studentPanel(_,ID):-
    write("Opção invalida, tente novamente"),
    read(Inp),
    studentPanel(Inp,ID).


professorScreen(ID):-
    %ler arquivos e pegar nome do professor e printar
    professorOptions(ID).

    
professorOptions(ID):-
    nl,nl,
    write("1) Visualizar disciplinas"),nl,
    write("2) Registrar aula"),nl,
    write("3) Cadastrar prova"),nl,
    write("4) Situação da classe"),nl,
    write("(S)air do sistema"),nl,
    read(Inp),
    professorPanel(Inp,ID).

    
professorPanel(1,ID).
    %UI visualizar disciplinas

professorPanel(2,ID).
    %UI Registrar aula

professorPanel(3,ID).
    %UI Cadastrar prova

professorPanel(4,ID).
    %UI Situação da classe

professorPanel("S",ID).
    %sair do sistema

professorPanel("s",ID).
    %sair do sistema

professorPanel(_,ID):-
    write("Opção invalida, tente novamente"),
    read(Inp),
    professorPanel(Inp,ID).


adminScreen():-
    write("Bem vindo, Adm"),
    adminOptions().

adminOptions():-
    nl,nl,
    write("1) Cadastrar professor"),nl,
    write("2) Cadastrar aluno"),nl,
    write("3) Cadastrar disciplina"),nl,
    write("4) Associar professor à disciplina"),nl,
    write("5) Listar alunos sem matrículas"),nl,
    write("6) Listar professores sem disciplinas"),nl,
    write("7) Disciplina com a maior média"),nl,
    write("8) Disciplina com a menor média"),nl,
    write("9) Consultar aluno com a maior média"),nl,
    write("(S)air do sistema"),nl,
    read(Inp),
    adminPanel(Inp).


adminPanel(1).
    %UI opção 1

adminPanel(2).
    %UI opção 2

adminPanel(3).
    %UI opção 3

adminPanel(4).
    %UI opção 4

adminPanel(5).
    %UI opção 5

adminPanel(6).
    %UI opção 6

adminPanel(7).
    %UI opção 7

adminPanel(8). 
    %UI opção 8

adminPanel(9).
    %UI opção 9

adminPanel('S'). 
    %sair do sistema

adminPanel('s'). 
    %sair do sistema

adminPanel(_):-
    write("Opção invalida, tente novamente "),
    read(Inp),
    adminPanel(Inp).

:- include('./controller.pl').

read_string(X) :- read_line_to_codes(user_input, I), atom_string(I, X).
read_number(N) :- read_string(X), atom_number(X, N).

main:- 
    write("Bem-Vindo(a)!\nPara acessar o controle, faça login:\n\n"),
    login,
    halt.

login:-
    write("Digite sua matrícula: "),
    read_string(Registration),
    authenticate(Registration, Role),
    writeln(Role),
    screen(Role, Registration) ;
    writeln("Usuario ou senha invalido! Tente novamente..."),
    nl,
    login.

screen(prof, ID):- 
    professorScreen(ID).

screen(admin):- 
    adminScreen.

screen(aluno, ID):- 
    studentScreen(ID).

screen:-
    write("Role invalido").

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
    read_string(Inp),
    studentPanel(Inp, ID).

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
    read_string(Inp),
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
    read_string(Inp),
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
    read_string(Inp),
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
    read_string(Inp),
    adminPanel(Inp).

adminPanel(1) :- 
    write("Digite a matrícula do professor: "),
    read_string(Registration),
    write("Digite o nome do professor: "),
    read_string(Name),
    create_professor(Registration, Name, Password).
    %UI opção 1

adminPanel(2) :- 
    write("Digite a matrícula do aluno: "),
    read_string(Registration),
    write("Digite o nome do aluno: "),
    read_string(Name),
    write("Digite a senha do aluno: "),
    read_string(Password),
    create_student(Registration, Name, Password).
    %UI opção 2

adminPanel(3) :- 
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

press_to_continue.
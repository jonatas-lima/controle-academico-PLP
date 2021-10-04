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
    write("Digite sua senha: "),
    read_string(Password),
    authenticate(Registration, Password, Role),
    writeln(Role),
    screen(Role, Registration) ;
    writeln("Usuario ou senha invalido! Tente novamente..."),
    nl,
    login.

screen(prof, ID):- 
    professorScreen(ID).

screen(admin, _):- 
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
    admin_options.

admin_options:-
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
    admin_panel(Inp).

admin_panel("1") :- 
    write("Digite a matrícula do professor: "),
    read_string(Registration),
    write("Digite o nome do professor: "),
    read_string(Name),
    write("Digite a senha do professor: "),
    read_string(Password),
    (find_user(Registration, R), R -> writeln("Professor ja existe!");
    save_professor(Registration, Name, Password), writeln("Professor cadastrado!")),
    admin_options.
    %UI opção 1

admin_panel("2") :- 
    write("Digite a matrícula do aluno: "),
    read_string(Registration),
    write("Digite o nome do aluno: "),
    read_string(Name),
    write("Digite a senha do aluno: "),
    read_string(Password),
    (find_user(Registration, R), R -> writeln("Aluno ja existe!");
    save_student(Registration, Name, Password), writeln("Aluno cadastrado!")),
    admin_options.

admin_panel("3").
    write("Digite o código da disciplina: "),
    read_string(Code),
    write("Digite o nome da disciplina: "),
    read_string(Name),
    write("Digite o número de aulas da disciplina: "),
    read_string(Classes),
    write("Digite o número de vagas da disciplina: "),
    read_string(MaxEnrollments),
    (find_user(Registration, _) -> writeln("Professor ja existe!");
    save_enrollment(Registration, Name, "123"), writeln("Professor cadastrado!")),
    admin_options.

admin_panel("4").
    %UI opção 4

admin_panel("5").
    %UI opção 5

admin_panel("6").
    %UI opção 6

admin_panel("7").
    %UI opção 7

admin_panel("8"). 
    %UI opção 8

admin_panel("9").
    %UI opção 9

admin_panel("S"). 
    %sair do sistema

admin_panel("s"). 
    %sair do sistema

admin_panel(_):-
    write("Opção invalida, tente novamente "),
    read(Inp),
    admin_panel(Inp).

press_to_continue.
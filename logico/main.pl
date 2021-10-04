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
    professor_screen(ID).

screen(admin, _):- 
    admin_screen.

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
    student_panel(Inp, ID).

student_panel(1,ID).
    %UI visualizar disciplinas

student_panel(2,ID).
    %UI Realizar matrícula

student_panel(3,ID).
    %UI Cancelar matrícula

student_panel(4,ID).
    %UI visualizar media geral

student_panel("S",ID).
    %sair do sistema

student_panel("s",ID).
    %sair do sistema

student_panel(_,ID):-
    write("Opção invalida, tente novamente"),
    read_string(Inp),
    student_panel(Inp,ID).

professor_screen(ID):-
    %ler arquivos e pegar nome do professor e printar
    professor_options(ID).
    
professor_options(ID):-
    nl,nl,
    write("1) Visualizar disciplinas"),nl,
    write("2) Registrar aula"),nl,
    write("3) Cadastrar prova"),nl,
    write("4) Situação da classe"),nl,
    write("(S)air do sistema"),nl,
    read_string(Inp),
    professor_panel(Inp, ID).

professor_panel(1, ID).
    %UI visualizar disciplinas

professor_panel(2, ID).
    %UI Registrar aula

professor_panel(3, ID).
    %UI Cadastrar prova

professor_panel(4, ID).
    %UI Situação da classe

professor_panel("S", ID) :- quit.

professor_panel("s", ID) :- quit.

professor_panel(_,ID):-
    write("Opção invalida, tente novamente"),
    read_string(Inp),
    professor_panel(Inp,ID).

admin_screen:-
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
    press_to_continue,
    admin_options.

admin_panel("4").
    %UI opção 4

admin_panel("5").
    %UI opção 5

admin_panel("6") :- 
    professors_without_subjects(Professors),
    nl,
    show_professors_without_subjects(Professors).

admin_panel("7").
    %UI opção 7

admin_panel("8"). 
    %UI opção 8

admin_panel("9").
    %UI opção 9

admin_panel("S") :- quit.

admin_panel("s") :- quit.

admin_panel(_):-
    write("Opção invalida, tente novamente "),
    read(Inp),
    admin_panel(Inp).

press_to_continue :- 
    writeln("Pressione alguma tecla para continuar..."),
    read_string(_),
    tty_clear.

show_professors_without_subjects([]) :- writeln("Todos os professores possuem pelo menos uma disciplina!").
show_professors_without_subjects(Professors) :-
    writeln("Professores sem disciplinas:"),
    show_professors_without_subjects_aux(Professors),
    press_to_continue,
    admin_options.

show_professors_without_subjects_aux([]).
show_professors_without_subjects_aux([P|T]) :- 
    nth0(0, P, Registration),
    nth0(1, P, Name),
    string_concat(Registration, "\t - \t", S1),
    string_concat(S1, Name, R),
    writeln(R),
    show_professors_without_subjects_aux(T).

quit :- 
    writeln("Até a próxima!"),
    halt.
:- include('./controller.pl').
:- include('./student.pl').
:- include('./professor.pl').

read_string(X) :- read_line_to_codes(user_input, I), atom_string(I, X).
read_number(N) :- read_string(X), atom_number(X, N).

main :- 
    tty_clear,
    write("Bem-Vindo(a)!\nPara acessar o controle, faça login:\n\n"),
    login,
    halt.

login :-
    write("Digite sua matrícula: "),
    read_string(Registration),
    write("Digite sua senha: "),
    read_string(Password),
    authenticate(Registration, Password, Role),
    tty_clear,
    screen(Role, Registration).

screen(prof, ID) :- 
    professor_screen(ID).

screen(admin, _) :- 
    admin_screen.

screen(aluno, ID):- 
    students_screen(ID).

screen:-
    write("Role invalido").

screen(false, _) :-
    writeln("Usuario ou senha invalido! Tente novamente..."),
    nl,
    login.

students_screen(ID) :-
    student_options(ID).

student_options(ID) :-
    nl,nl,
    get_student_name(ID, StudentName),
    write("Bem vindo, "),
    write(StudentName),
    writeln("!"),
    writeln(""),
    writeln("1) Visualizar disciplinas"),
    writeln("2) Realizar matrícula"),
    writeln("3) Cancelar matrícula"),
    writeln("4) Visualizar média geral"),
    writeln("(S)air do sistema"),
    read_string(Inp),
    student_panel(Inp, ID).

student_panel("1", ID) :- 
    student_subjects(ID, Subjects),
    show_student_subjects(ID, Subjects).

student_panel("2", ID):- 
    available_subjects_for_enrollment(ID, AvailableSubjects),
    (AvailableSubjects =@= "Aluno lotado de disciplinas.", press_to_continue, student_options(ID) -> writeln(AvailableSubjects), press_to_continue; 
    writeln("Codigo\t - \tDisciplina"),
    show_available_subjects(AvailableSubjects),
    writeln("Entre com o código da disciplina a ser matriculada: "),
    read_string(Code),
    find_student(ID, Student),
    delete_student(Student),
    nth0(2, Student, EnrolledSubjects),
    (number(EnrolledSubjects) -> term_string(EnrolledSubjects, Classes) ; Classes = EnrolledSubjects), 
    string_concat(Classes, ";", S1),
    string_concat(S1, Code, R),
    nth0(1, Student, Name),
    save_student(ID, Name, R)),
    press_to_continue,
    student_options(ID).

student_panel("3", ID):-
    student_subjects(ID, Subjects),
    show_available_subjects(Subjects),
    write("Entre com o código da disciplina a ser cancelada: "),
    read_string(SubjectCode),
    cancel_enrollment(ID, SubjectCode),
    press_to_continue,
    student_options(ID).

student_panel("4", ID):-
    get_student_average(ID, Average),
    write("\nCRA: "),
    format("~2f\n", [Average]),
    press_to_continue,
    student_options(ID).

student_panel("S", ID) :- quit.
student_panel("s", ID) :- quit.

student_panel(_, ID) :-
    write("Opção invalida, tente novamente"),
    press_to_continue,
    student_panel(Inp, ID).

professor_screen(ID) :-
    professor_options(ID).
    
professor_options(ID) :-
    nl,nl,
    get_professor_name(ID, ProfessorName),
    write("Bem vindo, "),
    writeln(ProfessorName),
    write("\n1) Visualizar disciplinas"),nl,
    write("2) Registrar aula"),nl,
    write("3) Cadastrar prova"),nl,
    write("4) Situação da classe"),nl,
    write("(S)air do sistema"),nl,
    read_string(Inp),
    professor_panel(Inp, ID).

professor_panel("1", ID) :-
    professor_subjects(ID, Subjects),
    show_professor_subjects_with_classes(Subjects),
    press_to_continue,
    professor_options(ID).

professor_panel("2", ID) :-
    professor_subjects(ID, Subjects),
    show_professor_subjects_with_classes(Subjects),
    (empty(Subjects) -> press_to_continue ; 
    write("Entre com o código da disciplina: "),
    read_string(CodeString),
    register_class(ID, CodeString), press_to_continue),
    professor_options(ID).

professor_panel("3", ID) :-
    professor_subjects(ID, Subjects),
    show_professor_subjects(ID, Subjects),
    (empty(Subjects) -> press_to_continue ; 
    write("Entre com o código da disciplina: "),
    read_string(CodeString),
    register_test(ID, CodeString), press_to_continue),
    professor_options(ID).
    %UI Cadastrar prova

professor_panel("4", ID) :-
    professor_subjects(ID, Subjects),
    show_professor_subjects(ID, Subjects),
    (empty(Subjects) -> press_to_continue ;
    write("Entre com o código da disciplina a ser consultada: "),
    read_string(SubjectCode),
    show_class_situation(SubjectCode), press_to_continue),
    professor_options(ID).

professor_panel("S", _) :- quit.

professor_panel("s", _) :- quit.

professor_panel(_, ID):-
    write("Opção invalida, tente novamente"),
    press_to_continue,
    professor_panel(Inp, ID).

admin_screen:-
    write("Bem vindo, Adm"),
    admin_options.

admin_options:-
    nl,nl,
    writeln("1) Cadastrar professor"),
    writeln("2) Cadastrar aluno"),
    writeln("3) Cadastrar disciplina"),
    writeln("4) Associar professor à disciplina"),
    writeln("5) Listar alunos sem matrículas"),
    writeln("6) Listar professores sem disciplinas"),
    writeln("7) Disciplina com a maior média"),
    writeln("8) Disciplina com a menor média"),
    writeln("9) Consultar aluno com a maior média"),
    writeln("(S)air do sistema"),
    read_string(Inp),
    admin_panel(Inp).

admin_panel("1") :- 
    write("Digite a matrícula do professor: "),
    read_string(Registration),
    write("Digite o nome do professor: "),
    read_string(Name),
    write("Digite a senha do professor: "),
    read_string(Password),
    (find_user(Registration, R), empty(R) -> save_professor(Registration, Name, Password), writeln("Professor cadastrado!");
    writeln("Professor ja existe!")),
    press_to_continue,
    admin_options.

admin_panel("2") :- 
    write("Digite a matrícula do aluno: "),
    read_string(Registration),
    write("Digite o nome do aluno: "),
    read_string(Name),
    write("Digite a senha do aluno: "),
    read_string(Password),
    (find_user(Registration, R), empty(R) -> save_new_student(Registration, Name, Password), writeln("Aluno cadastrado!");
    writeln("Aluno ja existe!")),
    press_to_continue,
    admin_options.

admin_panel("3") :-
    write("Digite o código da disciplina: "),
    read_string(Code),
    write("Digite o nome da disciplina: "),
    read_string(Name),
    write("Digite o número de aulas da disciplina: "),
    read_string(Classes),
    write("Digite o número de vagas da disciplina: "),
    read_string(MaxEnrollments),
    (find_subject(Code, R), empty(R) -> save_new_subject(Code, Name, Classes, MaxEnrollments), writeln("Disciplina cadastrada!");
    writeln("Disciplina ja existe!")),
    press_to_continue,
    admin_options.

admin_panel("4") :- 
    show_available_professors,
    write("Matrícula do professor: "),
    read_string(Registration),
    available_subjects_for_association(Registration, AvailableSubjects),
    show_available_subjects_for_association(Registration, AvailableSubjects),
    press_to_continue,
    admin_options.

admin_panel("5") :-
    students_without_enrollment(Students),
    nl,
    show_students_without_enrollments(Students).

admin_panel("6") :- 
    professors_without_subjects(Professors),
    nl,
    show_professors_without_subjects(Professors).

admin_panel("7") :-
    subject_with_highest_average(Subject, Average),
    nl,
    writeln("Disciplina com maior média:"),
    show_subject_with_average(Subject, Average).

admin_panel("8") :-
    subject_with_lowest_average(Subject, Average),
    nl,
    writeln("Disciplina com menor média"),
    show_subject_with_average(Subject, Average).

admin_panel("9"):-
    student_with_highest_average(Student, Average),
    show_student_with_highest_average(Student, Average).

admin_panel("S") :- quit.

admin_panel("s") :- quit.

admin_panel(_):-
    write("Opção invalida, tente novamente "),
    press_to_continue,
    admin_panel(Inp).

press_to_continue :- 
    writeln("Pressione alguma tecla para continuar..."),
    read_string(_),
    tty_clear.

show_professors_without_subjects([]) :- writeln("Todos os professores possuem pelo menos uma disciplina!"), fail.
show_professors_without_subjects(Professors) :-
    writeln("Professores sem disciplinas:"),
    show_entities(Professors),
    press_to_continue,
    admin_options.

show_students_without_enrollments([]) :- writeln("Todos os alunos estão matriculados em pelo menos uma disciplina!").
show_students_without_enrollments(Students) :-
    writeln("Alunos sem matrículas:"),
    show_entities(Students),
    press_to_continue,
    admin_options.

show_entities([]).
show_entities([E|T]) :- 
    nth0(0, E, Registration),
    nth0(1, E, Name),
    string_concat(Registration, "\t - \t", S1),
    string_concat(S1, Name, R),
    writeln(R),
    show_entities(T).

show_student_subjects(_, []) :- writeln("O aluno não está matriculado em nenhuma disciplina!").
show_student_subjects(ID, Subjects) :-
    writeln("Disciplinas matriculadas:"),
    writeln("Código \t -\tNome \t -\tMédia Geral"),
    show_subjects(Subjects),
    press_to_continue,
    student_options(ID).

show_professor_subjects(_, []) :- writeln("O professor não leciona nenhuma disciplina!").
show_professor_subjects(ID, Subjects) :-
    writeln("Disciplinas lecionadas:"),
    writeln("Código \t -\tNome"),
    show_professor_subjects_aux(Subjects).

show_professor_subjects_aux([]).
show_professor_subjects_aux([H|T]) :-
    nth0(0, H, Code),
    nth0(2, H, Name),
    string_concat(Code, "\t - \t", S1),
    string_concat(S1, Name, R),
    writeln(R),
    show_professor_subjects_aux(T).

show_professor_subjects_with_classes([]) :- writeln("O professor não leciona nenhuma disciplina!").
show_professor_subjects_with_classes(Subjects) :-
    writeln("Disciplinas lecionadas:"),
    writeln("Código \t -\tNome \t -\tAulas Restantes"),
    show_professor_subjects_with_classes_aux(Subjects).

show_professor_subjects_with_classes_aux([]).
show_professor_subjects_with_classes_aux([H|T]) :- 
    nth0(0, H, Code),
    nth0(2, H, Name),
    nth0(3, H, Classes),
    string_concat(Code, "\t - \t", S1),
    string_concat(S1, Name, S2),
    string_concat(S2, "\t - \t", S3),
    string_concat(S3, Classes, R),
    writeln(R),
    show_professor_subjects_with_classes_aux(T).

show_class_situation(SubjectCode) :-
    get_enrolled_students(SubjectCode, Result),
    writeln("\nCódigo \t -\tNome \t -\tMédia Geral"),
    show_class_situation_aux(SubjectCode, Result),
    nl.

show_class_situation_aux(_, []).
show_class_situation_aux(SubjectCode, [H|T]):-
    find_student(H, Student),
    nth0(0, Student, Registration),
    nth0(1, Student, Name),
    term_string(Registration, RegistrationString),
    get_student_average_subject(RegistrationString, SubjectCode, Average),
    string_concat(Registration, "\t - \t", S1),
    string_concat(S1, Name, S2),
    string_concat(S2, "\t - \t", S3),
    write(S3),
    format("~2f ", [Average]),
    student_situation(Average, Situation),
    write("["),
    write(Situation),
    writeln("]"),
    show_class_situation_aux(SubjectCode, T).

show_available_professors :-
    available_professors(Professors),
   (empty(Professors) -> writeln("Não há professores disponiveis!") ; show_entities(Professors)).

show_subject_with_average(Subject, Average) :-
    nth0(0, Subject, Code),
    nth0(2, Subject, Name),
    writeln("Código \t - \t Nome \t - \t Média Geral"),
    show_subject_with_grade(Code, Name, Average),
    press_to_continue,
    admin_options.

show_subject_with_grade(Code, Name, Average) :-
    string_concat(Code, "\t - \t", S1),
    string_concat(S1, Name, S2),
    string_concat(S2, "\t - \t", S3),
    write(S3),
    format("~2f\n", [Average]).

show_available_subjects_for_association(_, []) :- writeln("Não há disciplinas disponíveis para associação!").
show_available_subjects_for_association(ProfessorCode, Subjects) :-
    writeln("Disciplinas disponíveis para associação:"),
    nl,
    show_subjects(Subjects),
    write("Código da disciplina: "),
    read_string(SubjectCode),
    associate_professor(ProfessorCode, SubjectCode).

show_subjects([]).
show_subjects([S|T]) :-
    nth0(0, S, Code),
    nth0(2, S, Name),
    term_string(Code, CodeString),
    string_concat(Code, "\t - \t", S1),
    string_concat(S1, Name, S2),
    string_concat(S2, "\t - \t", S3),
    writeln(S3),
    show_subjects(T).

show_available_subjects([]).
show_available_subjects([S|T]) :-
    nth0(0, S, Code),
    nth0(2, S, Name),
    string_concat(Code, "\t - \t", S1),
    string_concat(S1, Name, S2),
    writeln(S2),
    show_available_subjects(T).

show_student_with_highest_average(Student, Average) :-
    get_student_registration(Student, Registration),
    nth0(1, Student, Name),
    nl,
    write("O aluno com a maior média é ["),
    write(Registration),
    write(" - "),
    write(Name),
    write("] com uma média de: "),
    format("~2f!\n", [Average]),
    nl,
    press_to_continue,
    admin_options.

quit :- 
    writeln("Até a próxima!"),
    halt.
create_professor(Registration, Name, Password) :- 
  format_new_professor(Registration, Name, R),
  create_entity('./data/professores.csv', R),
  create_user(Registration, Password, 'prof').

create_user(Username, Password, Role) :- 
  format_new_user(Username, Password, Role, R),
  create_entity('./data/usuarios.csv', R).

create_enrollment(SubjectCode) :- 
  string_concat(SubjectCode, ',', R),
  create_entity('./data/matriculas.csv', R).

create_student(Registration, Name, Password) :- 
  format_new_student(Registration, Name, R),
  create_entity('./data/alunos.csv', R),
  create_user(Registration, Password, 'aluno').

update_student(Registration, Name, Subjects) :-
  format_student(Registration, Name, Subjects, R),
  create_entity('./data/alunos.csv', R).

create_new_subject(Code, Name, Classes, MaxEnrollments):- 
  create_subject(Code, "", Name, Classes, MaxEnrollments).

create_subject(Code, ProfessorCode, Name, Classes, MaxEnrollments) :-
  format_new_subject(Code, ProfessorCode, Name, Classes, MaxEnrollments, R),
  create_enrollment(Code),
  create_entity('./data/disciplinas.csv', R).

create_entity(FilePath, Entity) :- 
  open(FilePath, append, File),
  writeln(File, Entity),
  close(File).

format_new_professor(Registration, Name, Subjects, R) :-
  string_concat(Registration, ',', S1),
  string_concat(S1, Name, S2),
  string_concat(S2, ',', S3),
  string_concat(S3, Subjects, R).

format_new_professor(Registration, Name, R) :-
  format_new_professor(Registration, Name, '', R).
  
format_new_user(Username, Password, Role, R) :-
  string_concat(Username, ',', S1),
  string_concat(S1, Password, S2),
  string_concat(S2, ',', S3),
  string_concat(S3, Role, R).

format_new_student(Registration, Name, R) :-
  format_new_professor(Registration, Name, R).

format_student(Registration, Name, Classes, R) :-
  format_new_student(Registration, Name, S1),
  string_concat(S1, Classes, R).

format_new_subject(Code, ProfessorCode, Name, Classes, MaxEnrollments, R) :-
  string_concat(Code, ',', S1),
  string_concat(S1, ProfessorCode, S2),
  string_concat(S2, ',', S3),
  string_concat(S3, Name, S4),
  string_concat(S4, ',', S5),
  string_concat(S5, Classes, S6),
  string_concat(S6, ',', S7),
  string_concat(S7, MaxEnrollments, R).

update_subjects([], _).
update_subjects([H|T], Control):-
  (Control == 0 -> open('./data/disciplinas.csv', write, File);
  open('./data/disciplinas.csv', append, File)),
  nth0(0, H, Code),
  nth0(1, H, ProfessorCode),
  nth0(2, H, Name),
  nth0(3, H, NumClasses),
  nth0(4, H, MaxEnrollments),
  format_new_subject(Code, ProfessorCode, Name, NumClasses, MaxEnrollments, R),
  writeln(File, R),
  close(File),
  update_subjects(T, 1).

update_professors([], _).
update_professors([H|T], Control) :-
  (Control =:= 0 -> open('./data/professores.csv', write, File);
  open('./data/professores.csv', append, File)),
  nth0(0, H, Registration),
  nth0(1, H, Name),
  nth0(2, H, Subjects),
  format_new_professor(Registration, Name, Subjects, R),
  writeln(File, R),
  close(File),
  update_professors(T, 1).

update_users([], _).
update_users([H|T], Control) :-
  (Control =:= 0 -> open('./data/usuarios.csv', write, File);
  open('./data/usuarios.csv', append, File)),
  nth0(0, H, Nickname),
  nth0(1, H, Password),
  nth0(2, H, Role),
  format_new_user(Nickname, Password, Role, R),
  writeln(File, R),
  close(File),
  update_users(T, 1).

update_students([],_).
update_students([H|T], Control):-
  (Control =:= 0 -> open('./data/alunos.csv', write, File);
  open('./data/alunos.csv', append, File)),
  nth0(0, H, Code),
  nth0(1, H, Name),
  nth0(2, H, Subjects),
  format_new_professor(Code, Name, Subjects, R),
  writeln(File, R),
  close(File),
  update_students(T, 1).
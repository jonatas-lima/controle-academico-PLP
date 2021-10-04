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

create_subject(Code, Name, Classes, MaxEnrollments) :- 
  format_new_subject(Code, Name, Classes, MaxEnrollments, R),
  create_entity('./data/disciplinas.csv', R).

create_entity(FilePath, Entity) :- 
  open(FilePath, append, File),
  writeln(File, Entity),
  close(File).

format_new_professor(Registration, Name, R) :-
  string_concat(Registration, ',', S1),
  string_concat(S1, Name, S2),
  string_concat(S2, ',', R).
  
format_new_user(Username, Password, Role, R) :-
  string_concat(Username, ',', S1),
  string_concat(S1, Password, S2),
  string_concat(S2, ',', S3),
  string_concat(S3, Role, R).

format_new_student(Registration, Name, R) :-
  format_new_professor(Registration, Name, R).

format_new_subject(Code, Name, Classes, MaxEnrollments, R) :-
  string_concat(Code, ',', S1),
  string_concat(S1, ',', S2),
  string_concat(S2, Name, S3),
  string_concat(S3, ',', S4),
  string_concat(S4, Classes, S5),
  string_concat(S5, ',', S6),
  string_concat(S6, MaxEnrollments, R).
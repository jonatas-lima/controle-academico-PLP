:- module(data_saver, [load_all_students/1, load_all_subjects/1, load_all_professors/1, load_all_users/1]).

load_all_students(Students) :-
  load_file('./data/alunos.csv', Students).

load_all_subjects(Subjects) :-
  load_file('./data/disciplinas.csv', Subjects).

load_all_professors(Professors) :-
  load_file('./data/professores.csv', Professors).

load_all_users(Users) :-
  load_file('./data/usuarios.csv', Users).

load_all_enrollments(Enrollments) :-
  load_file('./data/matriculas.csv', Enrollments).

load_file(FilePath, Entities) :-
  read_csv_row_list(FilePath, Data),
  Entities = Data.

read_csv_row_list(Path, Lists):-
  csv_read_file(Path, Rows, []),
  rows_to_lists(Rows, Lists).

rows_to_lists(Rows, Lists):- maplist(row_to_list, Rows, Lists).

row_to_list(Row, List):-
  Row =.. [row|List].

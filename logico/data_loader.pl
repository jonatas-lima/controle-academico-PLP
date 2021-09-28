:- module(data_saver, [load_all_students/1, load_all_subjects/1, load_all_professors/1, load_all_users/1]).

load_all_students(Students) :-
  load_file('./data/alunos.csv', Students).

load_all_subjects(Subjects) :-
  load_file('./data/disciplinas.csv', Subjects).

load_all_professors(Professors) :-
  load_file('./data/professores.csv', Professors).

load_all_users(Users) :-
  load_file('./data/usuarios.csv', Users).

read_file(Stream, []) :- 
  at_end_of_stream(Stream), !.

read_file(Stream, [H|T]) :-
  \+ at_end_of_stream(Stream), !,
  read(Stream, H),
  read_file(Stream, T).

load_file(FilePath, Entities) :-
  open(FilePath, read, Stream),
  read_file(Stream, EntitiesList),
  Entities = EntitiesList,
  close(Stream).
:- include('./data_loader.pl').
:- include('./data_saver.pl').
:- include('./util.pl').
:- include('./subject.pl').
:- include('./user.pl').

get_professor_registration(Professor, Registration):-
  nth0(0, Professor, Registration).

find_professor(Registration, Professor) :-
  load_all_professors(Professors),
  find(Professors, Registration, Professor).

get_professor_subjects(Registration, Subjects) :-
  find_professor(Registration, Professor),
  get_user_subjects(Professor, Subjects).

get_professor_name(Registration, Name) :-
  find_professor(Registration, Professor),
  nth0(1, Professor, Name).

is_professor_available(Professor) :-
  nth0(2, Professor, Subjects),
  term_string(Subjects, SubjectsString),
  split_string(SubjectsString, ";", "", SubjectsList),
  length(SubjectsList, L),
  L < 3.

delete_professor(Professor) :-
  load_all_professors(Professors),
  delete(Professors, Professor, NewProfessors),
  update_professors(NewProfessors, 0).
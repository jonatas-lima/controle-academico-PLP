:- include('./data_loader.pl').
:- include('./util.pl').
:- include('./subject.pl').

get_professor_registration(Professor, Registration):-
  nth0(0, Professor, Registration).

find_professor(Registration, Professor) :-
  load_all_professors(Professors),
  find(Professors, Registration, Professor).

get_professor_subjects(Registration, Subjects) :-
  find_professor(Registration, Professor),
  get_user_subjects(Professor, Subjects).

get_professor_list_subjects(Registration, Subjects) :-
  find_professor(Registration, Professor),
  get_user_subjects(Professor, UserSubjects),
  get_professor_list_subjects_aux(UserSubjects, Subjects).

get_professor_list_subjects_aux([], []).
get_professor_list_subjects_aux([H|T], [X|Y]) :-
  find_subject(H, Subject),
  X = Subject,
  get_professor_list_subjects_aux(T, Y).

get_professor_name(Registration, Name) :-
  find_professor(Registration, Professor),
  nth0(1, Professor, Name).

is_available(Professor) :-
  nth0(2, Professor, Subjects),
  term_string(Subjects, SubjectsString),
  split_string(SubjectsString, ";", "", SubjectsList),
  length(SubjectsList, L),
  L < 3.

:- include('./data_loader.pl').
:- include('./util.pl').

find_user(Nickname, User) :-
  load_all_users(Users),
  find(Users, Nickname, User).

get_user_subjects(Nickname, Subjects) :-
  find_user(Nickname, user),
  nth0(2, user, userSubjects),
  term_to_atom(userSubjects, SubjectsString),
  split_string(SubjectsString, ';', '', Subject).
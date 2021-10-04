:- include('./data_loader.pl').

find_user(Nickname, User) :-
  load_all_users(Users),
  find_user_aux(Nickname, Users, User).

find_user_aux(_, [], false).
find_user_aux(Nickname, [User|T], Result) :- 
  nth0(0, User, Username),
  atom_string(Username, UsernameString),
  UsernameString == Nickname -> Result = User;
  find_user_aux(Nickname, T, Result).

get_user_subjects(Nickname, Subjects) :-
  find_user(Nickname, user),
  nth0(2, user, userSubjects).
  term_to_atom(userSubjects, SubjectsString),
  split_string(SubjectsString, ';', '', Subject).
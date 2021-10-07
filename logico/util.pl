empty([]).
empty("").
empty('').
empty('""').
empty("''").
empty([""]).
empty(['']).

find([], _, []).
find([end_of_file], _, []).
find([H|T], Key, Result) :-
  nth0(0, H, Code),
  term_string(Code, CodeString),
  CodeString =@= Key -> Result = H ; find(T, Key, Result).

without_enrollment(User) :-
  nth0(2, User, Enrollments),
  empty(Enrollments).

get_user_subjects(Entity, Result) :-
  nth0(2, Entity, Subjects),
  (number(Subjects) -> atom_string(Subjects, S); S = Subjects),
  split_string(S, ";", "", List),
  exclude(empty, List, Result).

max(X, Y, X) :-
  X >= Y.
max(X, Y, Y) :-
  X < Y.

max_element([X], X).
max_element([H|T], X) :-
  max_element(T, I),
  max(H, I, X).

map_to_number([], []).
map_to_number([H|T], [X|Y]) :-
  atom_number(H, X),
  map_to_number(T, Y).

first(List, Element) :-
  nth0(0, List, Element).

second(List, Element) :-
  nth0(1, List, Element).

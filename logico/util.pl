empty([]).
empty("").
empty('').

find([], _, false).
find([end_of_file], _, false).
find([H|T], Key, Result) :-
  nth0(0, H, Code),
  term_string(Code, CodeString),
  CodeString =@= Key -> Result = H ; find(T, Key, Result).

without_enrollment(User) :-
  nth0(2, User, Enrollments),
  empty(Enrollments).
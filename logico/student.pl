:-include('data_loader.pl').
:-include('./util.pl').

have_max_enrollments(Registration) :-
  num_enrolled_subjects(Registration, Num),
  Num >= 3.

num_enrolled_subjects(Registration, Num) :-
  get_user_subjects(Registration, Subjects),
  length(Subjects, Num).

find_student(Registration, Result) :-
  load_all_students(Students),
  find(Students, Registration, Result).

get_student_subjects(Registration, Result) :-
  find_student(Registration, Student),
  get_user_subjects(Student, Result).
:-include('data_loader.pl').
:-include('./util.pl').
:-include('./subject.pl').
:- include('./data_saver.pl').

have_max_enrollments(Registration) :-
  num_enrolled_subjects(Registration, Num),
  Num >= 4.

num_enrolled_subjects(Registration, Num) :-
  get_student_subjects(Registration, Subjects),
  length(Subjects, Num).

find_student(Registration, Result) :-
  load_all_students(Students),
  find(Students, Registration, Result).

get_student_registration(Student, Registration) :-
  nth0(0, Student, Registration).

get_student_name(Registration, Result) :-
  find_student(Registration, Student),
  nth0(1, Student, Result).

get_student_subjects(Registration, Result) :-
  find_student(Registration, Student),
  get_user_subjects(Student, Result).

get_student_average(Registration, Result) :-
  get_student_subjects(Registration, Subjects),
  get_student_average_aux(Registration, Subjects, Averages),
  length(Averages, Length),
  sum_list(Averages, Sum),
  (Length =:= 0 -> Result = 0 ; Result is Sum/Length).

get_student_average_aux(_, [], []). 
get_student_average_aux(Registration, [H|T], [X|Y]) :-
  get_student_average_subject(Registration, H, SubjectAverage),
  X = SubjectAverage,
  get_student_average_aux(Registration, T, Y). 

delete_student(Student) :-
  load_all_students(Students),
  delete(Students, Student, Result),
  update_students(Result, 0).
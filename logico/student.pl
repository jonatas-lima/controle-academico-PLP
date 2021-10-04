:-include('data_loader.pl').

show_student().

show_students().

have_max_enrollments(Registration) :-
  num_enrolled_subjects(Registration, Num),
  Num >= 3.

num_enrolled_subjects(Registration, Num) :-
  find_student(Registration, Result).

find_student(Registration, Result) :-
  load_all_students(Students),
  find_student_aux(Registration, Students, Result).

find_student_aux(_, [end_of_file], false).
find_student_aux(Registration, [Student|T], Result) :-
  nth0(0, Student, StudentRegistration),
  nth0(1, Student, StudentName),
  nth0(2, Student, StudentSubjects),
  StudentRegistration =@= Registration -> Result = Student ;
  find_student_aux(Registration, T, Result).

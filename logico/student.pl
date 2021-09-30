:-include('data_loader.pl').

save_student(Registration, Name, Subjects) :- 
  open('./data/alunos.csv', append, Stream),
  format_student(Registration, Name, Subjects, Result),
  writeln(Stream, Result),
  close(Stream).

format_student(Registration, Name, Subjects, Result) :-
  string_concat(Registration, ';', S1),
  string_concat(S1, Name, S2),
  string_concat(S2, ';', S3),
  string_concat(S3, Subjects, S4),
  string_concat(S4, '.\n', Result).

show_student().

show_students().

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

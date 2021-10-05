:- include('./data_loader.pl').
:- include('./util.pl').

find_subject(Code, Subject) :-
  load_all_subjects(Subjects),
  find(Subjects, Code, Subject).

find_subject_enrollments(Code, SubjectEnrollments) :-
  load_all_enrollments(Enrollments),
  find_subject_enrollments_aux(Code, Enrollments, SubjectEnrollments).

find_subject_enrollments_aux(_, [end_of_file], _).
find_subject_enrollments_aux(Code, [SubjectEnrollment|T], Result) :-
  nth0(0, SubjectEnrollment, SubjectCode),
  nth0(1, SubjectEnrollment, SubjectEnrollments),
  term_string(SubjectCode, CodeString),
  CodeString =@= Code -> split_string(SubjectEnrollments, ";", "", Result);
  find_subject_enrollments_aux(Code, T, Result).

get_subject_average(Code, Result) :-
  get_subject_grades(Code, Grades),
  get_subject_grades_sum_list(Grades, S),
  sum_list(Sum, Total),
  writeln(Total),
  length(Sum, L),
  writeln(L),
  Result is Total / L.

get_total_tests(Code, Total) :-
  get_enrolled_students(Code, StudentCodes),
  find_subject_enrollments(Code, Enrollments),
  get_total_tests_students(StudentCodes, Enrollments, List),
  sum_list(List, Total).

get_total_tests_students([], _, _).
get_total_tests_students([R|T], Enrollments, [X|Y]) :-
  find_student_grades(R, Enrollments, Grades),
  length(Grades, X),
  get_total_tests_students(T, Enrollments, Y).

get_enrolled_students(SubjectCode, Result) :-
  find_subject_enrollments(SubjectCode, Enrollments),
  get_enrolled_students_aux(Enrollments, Result).

get_enrolled_students_aux([], []).
get_enrolled_students_aux([E|T], [X|Y]) :-
  split_string(E, "|", "", EData),
  first(EData, Registration),
  X = Registration,
  get_enrolled_students_aux(T, Y).

get_student_average_subject(Registration, SubjectCode, Result) :-
  find_subject_enrollments(SubjectCode, Enrollments),
  find_student_grades(Registration, Enrollments, Grades),
  length(Grades, Length),
  sum_list(Grades, Sum),
  (Length =:= 0 -> Result = 0 ; Result is Sum / Length). 

find_student_grades(_, [], false).
find_student_grades(Registration, [E|T], Result) :-
  split_string(E, "|", "", EData),
  nth0(0, EData, StudentRegistration),
  nth0(1, EData, Grades),
  StudentRegistration =@= Registration -> parse_grades(Grades, Result) ; find_student_grades(Registration, T, Result).

get_subject_grades_sum_list([], []).
get_subject_grades_sum_list([G|T], [X|Y]) :-
  map_to_number(G, Grades),
  sum_list(Grades, SumList),
  X = SumList,
  get_subject_grades_sum_list(T, Y).

get_subject_grades(Code, Result) :-
  find_subject_enrollments(Code, Enrollments),
  get_subject_grades_aux(Enrollments, Result).

get_subject_grades_aux([], []).
get_subject_grades_aux([Enrollment|T], [X|Y]) :-
  split_string(Enrollment, "|", "", EnrollmentData),
  nth0(1, EnrollmentData, GradesString),
  (empty(GradesString) -> X = [] ; parse_grades(GradesString, Grades), X = Grades),
  get_subject_grades_aux(T, Y).

parse_grades(Grades, []) :-
  empty(Grades).
parse_grades(Grades, Result) :-
  split_string(Grades, "-", "", G),
  map_to_number(G, Result).
:- include('./data_loader.pl').
:- include('./data_saver.pl').
:- include('./util.pl').

is_subject_available(Subject) :-
  nth0(1, Subject, ProfessorCode),
  empty(ProfessorCode).

find_enrollment(SubjectCode, Enrollment) :-
  load_all_enrollments(Enrollments),
  find(Enrollments, SubjectCode, Enrollment).

find_subject(Code, Subject) :-
  load_all_subjects(Subjects),
  find(Subjects, Code, Subject).

find_subject_enrollments(Code, SubjectEnrollments) :-
  load_all_enrollments(Enrollments),
  find_subject_enrollments_aux(Code, Enrollments, SubjectEnrollments).

find_subject_enrollments_aux(_, [end_of_file], _).
find_subject_enrollments_aux(Code, [Enrollment|T], Result) :-
  nth0(0, Enrollment, SubjectCode),
  nth0(1, Enrollment, SubjectEnrollments),
  term_string(SubjectCode, CodeString),
  (CodeString =@= Code -> parse_enrollments(SubjectEnrollments, Result));
  find_subject_enrollments_aux(Code, T, Result).

get_subject_average(Code, Result) :-
  get_subject_grades(Code, Grades),
  get_subject_grades_sum_list(Grades, Sum),
  sum_list(Sum, Total),
  get_total_tests(Code, Tests),
  (Tests =:= 0 -> Result is 0 ; Result is Total / Tests).

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
  sum_list(G, SumList),
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

parse_enrollments(Enrollments, []) :- empty(Enrollments).
parse_enrollments(Enrollments, Result) :-
  split_string(Enrollments, ";", "", Result).

delete_subject(Subject):-
  load_all_subjects(AllSubjects),
  delete(AllSubjects, Subject, Result),
  update_subjects(Result, 0).

delete_enrollment(Enrollment):-
  load_all_enrollments(AllEnrollments),
  delete(AllEnrollments, Enrollment, Result),
  update_enrollments(Result, 0).
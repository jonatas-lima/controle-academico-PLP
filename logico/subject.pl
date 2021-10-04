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
  get_subject_average_aux(Grades, Result).
  % writeln(Grades).

get_subject_average_aux([], _).
get_subject_average_aux([G|T], Result) :-
  map_to_number(G, Grades),
  sum_list(Grades, Sum),
  get_subject_average_aux(T, Sum),
  Result is G + Sum.

get_subject_grades(Code, Result) :-
  find_subject_enrollments(Code, Enrollments),
  get_subject_grades_aux(Enrollments, Result).

get_subject_grades_aux([], []).
get_subject_grades_aux([Enrollment|T], [X|Y]) :-
  split_string(Enrollment, "|", "", EnrollmentData),
  nth0(1, EnrollmentData, GradesString),
  (empty(GradesString) -> X = [] ; split_string(GradesString, "-", "", Grades), X = Grades),
  get_subject_grades_aux(T, Y).
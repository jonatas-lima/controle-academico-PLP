:- include('./user.pl').

authenticate(Username, Password, Role) :-
  find_user(Username, User),
  nth0(1, User, UserPassword),
  term_string(UserPassword, UserPassword),
  term_string(Password, Password),
  UserPassword =@= Password,
  nth0(2, User, Role).

% consultas
students_without_enrollment(Result) :-
  load_all_students(Students),
  students_without_enrollment_aux(Students, Result).

students_without_enrollment_aux([], []).
students_without_enrollment_aux([H|T], [X|Y]) :-
  nth0(0, H, Registration),
  nth0(2, H, Enrollments),
  (Enrollments =@= '' -> X = H, students_without_enrollment_aux(T, Y) ; students_without_enrollment_aux(T, Y)).

professors_without_subjects(Result) :-
  load_all_professors(Professors),
  professors_without_subjects_aux(Professors, Result).

professors_without_subjects_aux([], []).
professors_without_subjects_aux([H|T], [X|Y]) :-
  students_without_enrollment_aux([H|T], [X|Y]).

available_professors(AvailableProfessors).

student_with_highest_average(Student).

available_subjects_for_association(AvailableSubjects).

subject_with_highest_average(Subject).

subject_with_lowest_average(Subject).

available_subjects_for_enrollment(StudentCode, AvailableSubjects).

class_situation(ProfessorRegistration, SubjectCode).

student_situation(StudentRegistration, SubjectCode).

% associacoes / delecoes
enroll_student(StudentRegistration, SubjectCode).

associate_professor(ProfessorRegistration, SubjectCode).

cancel_enrollment(StudentCode, SubjectCode).

register_class(ProfessorRegistration, SubjectCode).

register_test(ProfessorRegistration, SubjectCode).

% criacoes
save_professor(Registration, Name).

save_student(Registration, Name).

save_subject(Code, Name, Credits, Classes).
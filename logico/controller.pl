:- include('./user.pl').
:- include('./data_saver.pl').

authenticate(Username, Password, Role) :-
  find_user(Username, User),
  nth0(1, User, Pass),
  atom_string(Pass, PassString),
  PassString =@= Password -> nth0(2, User, Role) ; false.

% consultas
students_without_enrollment(Result) :-
  load_all_students(Students),
  users_without_enrollment_aux(Students, Result).

users_without_enrollment_aux([], []).
users_without_enrollment_aux([H|T], [X|Y]) :-
  nth0(0, H, Registration),
  nth0(2, H, Enrollments),
  (Enrollments =@= '' -> X = H, users_without_enrollment_aux(T, Y) ; users_without_enrollment_aux(T, Y)).

professors_without_subjects(Result) :-
  load_all_professors(Professors),
  professors_without_subjects_aux(Professors, Result).

professors_without_subjects_aux([], []).
professors_without_subjects_aux([H|T], [X|Y]) :-
  users_without_enrollment_aux([H|T], [X|Y]).

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
save_professor(Registration, Name, Password) :- 
  create_professor(Registration, Name, Password).

save_student(Registration, Name, Password) :- 
  create_student(Registration, Name, Password).

save_subject(Code, Name, Credits, Classes) :- 
  create_subject(Code, Name, Credits, Classes).

empty("").
empty('').
empty([]).
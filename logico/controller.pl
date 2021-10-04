:- include('./user.pl').
:- include('./util.pl').
:- include('./subject.pl').
:- include('./student.pl').
:- include('./professor.pl').
:- include('./data_saver.pl').

authenticate(Username, Password, Role) :-
  find_user(Username, User),
  nth0(1, User, Pass),
  atom_string(Pass, PassString),
  PassString =@= Password -> nth0(2, User, Role) ; false.

% consultas
students_without_enrollment(Result) :-
  load_all_students(Students),
  include(without_enrollment, Students, Result).

professors_without_subjects(Result) :-
  load_all_professors(Professors),
  include(without_enrollment, Professors, Result).

available_professors(AvailableProfessors) :- 
  load_all_professors(Professors),
  available_professors_aux(Professors, AvailableProfessors).

available_professors_aux(Professors, Result) :- 
  professors_without_subjects(ProfWithoutSubjects),
  include(is_available, Professors, AvailableProfessors),
  union(ProfWithoutSubjects, AvailableProfessors, Result).

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

save_subject(Code, Name, Classes, MaxEnrollments) :- 
  create_subject(Code, Name, Classes, MaxEnrollments).

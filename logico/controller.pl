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

student_with_highest_average(Student):-
  load_all_students(Students),
  get_registrations_list(Students, RegistrationsList),
  get_averages_list(RegistrationsList, AveragesList),
  max_list(AveragesList, MaxAverage),
  student_with_max_average(RegistrationsList, MaxAverage, Registration),
  term_string(Registration, String),
  find_student(String, Student).

student_with_max_average([H|T], Average, Registration):-
  term_string(H, String),
  get_student_average(String, StudentAverage),
  (StudentAverage == Average -> Registration = H ; student_with_max_average(T, Average, Registration)).

get_registrations_list([], []).
get_registrations_list([H|T], [X|Y]):- 
  nth0(0, H, Registration),
  X = Registration,
  get_registrations_list(T, Y).

get_averages_list([], []).
get_averages_list([H|T], [X|Y]):-
  term_string(H, String),
  get_student_average(String, StudentAverage),
  X = StudentAverage,
  get_averages_list(T, Y).

student_subjects(Registration, Result) :-
  get_student_subjects(Registration, SubjectCodes),
  find_subjects(SubjectCodes, Result).

find_subjects([], []).
find_subjects([C|T], [X|Y]) :-
  find_subject(C, X),
  find_subjects(T, Y).

professor_subjects(Registration, Result) :-
  get_professor_subjects(Registration, SubjectCodes),
  find_subjects(SubjectCodes, Result).

available_subjects_for_association(AvailableSubjects).

% subject_with_highest_average(Subject):-
%   load_all_subjects(Subjects),
%   get_registrations_list(Subjects, RegistrationsList),
%   get_averages_list_subjects(RegistrationsList, AveragesList),
%   max_list(AveragesList, MaxAverage),
%   subject_with_max_average(RegistrationsList, MaxAverage, Registration),
%   term_string(Registration, String),
%   find_subject(String, Student).

% subject_with_max_average([H|T], Average, Registration):-
%   term_string(H, String),
%   get_subject_average(String, SubjectAverage),
%   (SubjectAverage == Average -> Registration = H ; subject_with_max_average(T, Average, Registration)).

% get_averages_list_subjects([], []).
% get_averages_list_subjects([H|T], [X|Y]):-
%   term_string(H, String),
%   get_subject_average(String, SubjectAverage),
%   X = SubjectAverage,
%   get_averages_list(T, Y).

subject_with_lowest_average(Subject).

available_subjects_for_enrollment(StudentCode, AvailableSubjects).

class_situation(ProfessorRegistration, SubjectCode).

student_situation(StudentRegistration, SubjectCode).

% associacoes / delecoes
enroll_student(StudentRegistration, SubjectCode).

associate_professor(ProfessorRegistration, SubjectCode).

cancel_enrollment(StudentCode, SubjectCode).

register_class(ProfessorRegistration, SubjectCode):-
  term_string(ProfessorRegistration, ProfRegistStr),
  term_string(SubjectCode, SubCodetStr),
  get_professor_subjects(ProfRegistStr, [H|T]),
  (H =@= SubCodetStr -> class_registration(SubCodetStr);
  register_class_aux(T, SubCodetStr)).

class_registration(SubjectCode):-
find_subject(SubjectCode, Subject),
nth0(1, Subject, Professor),
nth0(2, Subject, Name),
nth0(3, Subject, NumClasses),
nth0(4, Subject, MaxEnrollments),
NewNumClasses is NumClasses - 1,
save_subject(SubjectCode, Professor, Name, NewNumClasses, MaxEnrollments),
writeln("Registro de aula realizado").


register_class_aux([], _):-
writeln("Disciplina não encontrada.").
  
register_class_aux([H|T], SubjectCode):- 
  (H =@= SubjectCode -> class_registration(SubjectCode);
  register_class_aux(T, SubjectCode)).


register_test(ProfessorRegistration, SubjectCode).

% criacoes
save_professor(Registration, Name, Password) :- 
  create_professor(Registration, Name, Password).

save_student(Registration, Name, Password) :- 
  create_student(Registration, Name, Password).

save_subject(Code, ProfessorCode, Name, Classes, MaxEnrollments) :- 
  create_subject(Code, ProfessorCode, Name, Classes, MaxEnrollments).

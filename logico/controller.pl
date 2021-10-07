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
  include(is_professor_available, Professors, AvailableProfessors),
  union(ProfWithoutSubjects, AvailableProfessors, Result).

student_with_highest_average(Student, Average):-
  load_all_students(Students),
  get_registrations_list(Students, RegistrationsList),
  get_averages_list(RegistrationsList, AveragesList),
  max_list(AveragesList, Average),
  student_with_max_average(RegistrationsList, Average, Registration),
  find_student(Registration, Student).

student_with_max_average([H|T], Average, Registration):-
  get_student_average(H, StudentAverage),
  (StudentAverage =:= Average -> Registration = H ; student_with_max_average(T, Average, Registration)).

get_registrations_list([], []).
get_registrations_list([H|T], [X|Y]):- 
  nth0(0, H, Registration),
  term_string(Registration, X),
  get_registrations_list(T, Y).

get_averages_list([], []).
get_averages_list([H|T], [X|Y]):-
  get_student_average(H, StudentAverage),
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

available_subjects_for_association(ProfessorCode, AvailableSubjects) :-
  subjects_without_professors(SubjectsWithoutProfessor),
  professor_subjects(ProfessorCode, ProfessorSubjects),
  subtract(SubjectsWithoutProfessor, ProfessorSubjects, AvailableSubjects).

subjects_without_professors(Subjects) :-
  load_all_subjects(AllSubjects),
  include(is_subject_available, AllSubjects, Subjects).

subject_with_highest_average(Subject, Average):-
  load_all_subjects(Subjects),
  get_registrations_list(Subjects, RegistrationsList),
  get_averages_list_subjects(RegistrationsList, AveragesList),
  max_list(AveragesList, Average),
  subject_with_average(RegistrationsList, Average, SubjectCode),
  find_subject(SubjectCode, Subject).

subject_with_average([H|T], Average, Result):-
  get_subject_average(H, SubjectAverage),
  (SubjectAverage =:= Average -> Result = H ; subject_with_average(T, Average, Result)).

get_averages_list_subjects([], []).
get_averages_list_subjects([H|T], [X|Y]) :-
  get_subject_average(H, SubjectAverage),
  X = SubjectAverage,
  get_averages_list_subjects(T, Y).

subject_with_lowest_average(Subject, Average) :-
  load_all_subjects(Subjects),
  get_registrations_list(Subjects, RegistrationsList),
  get_averages_list_subjects(RegistrationsList, AveragesList),
  min_list(AveragesList, Average),
  subject_with_average(RegistrationsList, Average, SubjectCode),
  find_subject(SubjectCode, Subject).

available_subjects_for_enrollment(StudentCode, AvailableSubjects):-
  num_enrolled_subjects(StudentCode, Num),
  (Num >= 4 -> AvailableSubjects = "Aluno lotado de disciplinas.";
  get_available_subjects(StudentCode, AvailableSubjects)).

get_available_subjects(StudentCode, AvailableSubjects):-
  load_all_subjects(Subjects),
  get_student_subjects(StudentCode, StudentSubjects),
  remove_student_subjects(StudentSubjects, Subjects, Result),
  AvailableSubjects = Result.

remove_student_subjects([], Subjects, Result):- Result = Subjects.
remove_student_subjects([H|T], Subjects, Result):-
  find_subject(H, Subject),
  delete(Subjects, Subject, ResultAux),
  remove_student_subjects(T, ResultAux, Result).

class_situation(ProfessorRegistration, SubjectCode).

student_situation(Average, Result):-
  (Average >= 7 -> Result = "Aprovado" ; Average >= 4 -> Result = "Final" ; Result = "Reprovado").

% associacoes / delecoes
enroll_student(StudentRegistration, SubjectCode).

associate_professor(ProfessorRegistration, SubjectCode) :- 
  find_professor(ProfessorRegistration, Professor),
  find_subject(SubjectCode, Subject).

cancel_enrollment(StudentCode, SubjectCode).

register_class(ProfessorRegistration, SubjectCode):-
  get_professor_subjects(ProfessorRegistration, [H|T]),
  (H =@= SubjectCode -> class_registration(SubjectCode);
  register_class_aux(T, SubjectCode)).

register_class_aux([], _):-
  writeln("Disciplina não encontrada.").
  
register_class_aux([H|T], SubjectCode):- 
  (H =@= SubjectCode -> class_registration(SubjectCode);
  register_class_aux(T, SubjectCode)).

class_registration(SubjectCode):-
  find_subject(SubjectCode, Subject),
  nth0(1, Subject, Professor),
  nth0(2, Subject, Name),
  nth0(3, Subject, NumClasses),
  nth0(4, Subject, MaxEnrollments),
  NewNumClasses is NumClasses - 1,
  delete_subject(Subject),
  save_subject(SubjectCode, Professor, Name, NewNumClasses, MaxEnrollments),
  writeln("\nRegistro de aula realizado com sucesso!\n").

register_test(ProfessorRegistration, SubjectCode).

% criacoes
save_professor(Registration, Name, Password) :- 
  create_professor(Registration, Name, Password).

save_student(Registration, Name, Password) :- 
  create_student(Registration, Name, Password).

save_subject(Code, Name, Classes, MaxEnrollments) :- 
  save_subject(Code, "", Name, Classes, MaxEnrollments).

save_subject(Code, ProfessorCode, Name, Classes, MaxEnrollments) :-
  create_subject(Code, ProfessorCode, Name, Classes, MaxEnrollments).
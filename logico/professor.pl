:- include('./data_loader.pl').
:- include('./util.pl').

find_professor(Registration, Professor) :-
  load_all_professors(Professors),
  find_professor_aux(Registration, Professors, Professor).

find_professor_aux(_, [end_of_file], false).
find_professor_aux(Registration, [Professor|T], Result) :- 
  nth0(0, Professor, ProfessorRegistration),
  term_string(ProfessorRegistration, RegistrationString),
  RegistrationString =@= Registration -> Result = Professor;
  find_professor_aux(Registration, T, Result).

get_professor_subjects(Registration, Subjects) :-
  find_professor(Registration, Professor),
  nth0(2, Professor, ProfessorSubjects).
  term_string(ProfessorSubjects, SubjectsString),
  split_string(SubjectsString, ";", "", Subjects).

is_available(Professor) :-
  nth0(2, Professor, Subjects),
  term_string(Subjects, SubjectsString),
  split_string(SubjectsString, ";", "", SubjectsList),
  length(SubjectsList, L),
  writeln(L),
  L < 3.

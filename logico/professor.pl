:- include('./data_loader.pl').

find_professor(Code, Professor) :-
  load_all_professors(Professors),
  find_professor_aux(Code, Professors, Professor).

find_professor_aux(_, [end_of_file], _).
find_professor_aux(Code, [Professor|T], Result) :- 
  nth0(0, Professor, ProfessorCode),
  ProfessorCode =@= Code -> Result = Professor;
  find_professor_aux(Code, T, Result).

get_professor_subjects(Code, Subjects) :-
  find_professor(Code, Professor),
  nth0(2, Professor, ProfessorSubjects).
  term_to_atom(ProfessorSubjects, SubjectsString),
  split_string(SubjectsString, ';', '', Subject).
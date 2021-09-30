:- include('./data_loader.pl').

find_subject(Code, Subject) :-
  load_all_subjects(Subjects),
  find_subject_aux(Code, Subjects, Subject).

find_subject_aux(_, [end_of_file], false).
find_subject_aux(Code, [Subject|T], Result) :- 
  nth0(0, Subject, SubjectCode),
  SubjectCode =@= Code -> Result = Subject;
  find_subject_aux(Code, T, Result).

find_subject_enrollments(Code, SubjectEnrollments) :-
  load_all_enrollments(Enrollments),
  find_subject_enrollments_aux(Code, Enrollments, SubjectEnrollments).

find_subject_enrollments_aux(_, [end_of_file], false).
find_subject_enrollments_aux(Code, [SubjectEnrollment|T], Result) :-
  nth0(0, SubjectEnrollment, SubjectCode),
  nth0(1, SubjectEnrollment, SubjectEnrollments),
  SubjectCode =@= Code -> Result = SubjectEnrollments;
  find_subject_enrollments_aux(Code, T, Result).
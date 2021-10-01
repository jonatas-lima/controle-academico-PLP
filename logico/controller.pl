:- include('./user.pl').

authenticate(Username, Password, Role) :-
  find_user(Username, User),
  nth0(1, User, UserPassword),
  term_string(UserPassword, UserPassword),
  term_string(Password, Password),
  UserPassword == Password,
  nth0(2, User, Role).

% consultas
students_without_enrollment(StudentsWithoutEnrollment).

professors_without_subjects(ProfessorsWithoutSubjects).

available_professors(AvailableProfessors).

student_with_highest_average(Student).

available_subjects_for_association(AvailableSubjects).

subject_with_highest_average(Subject).

subject_with_lowest_average(Subject).

available_subjects_for_enrollment(StudentCode, AvailableSubjects).

class_situation(ProfessorRegistration, SubjectCode).

student_situation(StudentRegistration, SubjectCode).

% associacoes / criacoes / delecoes
enroll_student(StudentRegistration, SubjectCode).

associate_professor(ProfessorRegistration, SubjectCode).

cancel_enrollment(StudentCode, SubjectCode).

register_class(ProfessorRegistration, SubjectCode).

register_test(ProfessorRegistration, SubjectCode).

% criacoes
save_professor(Registration, Name).

save_student(Registration, Name).

save_subject(Code, Name, Credits, Classes).
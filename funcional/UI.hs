module UI where

import Student (Student)
import qualified Student
import Professor (Professor)
import qualified Professor
import Subject (Subject)
import qualified Subject
import qualified DataLoader
import qualified DataSaver
import qualified Controller
import Text.Printf

showStudentSubjectsScreen :: Int -> IO()
showStudentSubjectsScreen studentRegistration = do
  subjectsFile <- DataLoader.readArq "./data/disciplinas.csv"
  studentsFile <- DataLoader.readArq "./data/alunos.csv"
  let subjects = DataLoader.loadSubjects subjectsFile
  let students = DataLoader.loadStudents studentsFile
  let student = DataLoader.loadStudent studentRegistration students

  let enrolledSubjectCodes = Student.enrolledSubjects student
  let enrolledSubjects = DataLoader.loadSubjectsByCode enrolledSubjectCodes subjects

  putStrLn $ "\nCódigo\t - Disciplina\t - Média atual/Status\n" ++ Controller.showStudentSubjects student enrolledSubjects

enrollSubjectScreen :: Int -> IO()
enrollSubjectScreen studentRegistration = do
  subjectsFile <- DataLoader.readArq "./data/disciplinas.csv"
  studentsFile <- DataLoader.readArq "./data/alunos.csv"
  let subjects = DataLoader.loadSubjects subjectsFile
  let students = DataLoader.loadStudents studentsFile
  let student = DataLoader.loadStudent studentRegistration students

  if Student.numberEnrolledSubjects student < 4
    then Controller.enroll student subjects
    else putStrLn ("\nO aluno [" ++ printf "%.d" (Student.registration student) ++ "] já possui 4 disciplinas matriculadas!\n")

cancelEnrollmentScreen :: Int -> IO()
cancelEnrollmentScreen studentRegistration = do
  subjectsFile <- DataLoader.readArq "./data/disciplinas.csv"
  studentsFile <- DataLoader.readArq "./data/alunos.csv"
  let subjects = DataLoader.loadSubjects subjectsFile
  let students = DataLoader.loadStudents studentsFile
  let student = DataLoader.loadStudent studentRegistration students

  let enrolledSubjectCodes = Student.enrolledSubjects student
  let enrolledSubjects = DataLoader.loadSubjectsByCode enrolledSubjectCodes subjects

  if Student.numberEnrolledSubjects student > 0
    then Controller.cancelRegistration student enrolledSubjects enrolledSubjectCodes
    else putStrLn "\nO aluno não está matriculado em nenhuma disciplina!"

showProfessorSubjects :: Int -> IO()
showProfessorSubjects id = do
  professorFile <- DataLoader.readArq "./data/professores.csv"
  let professors = DataLoader.loadProfessors professorFile
  let professor = DataLoader.loadProfessor id professors

  subjectsFile <- DataLoader.readArq "./data/disciplinas.csv"
  let subjects = DataLoader.loadSubjects subjectsFile
  let codesProfessorSubjects = Professor.subjects professor
  let professorSubjects = DataLoader.loadSubjectsByCode codesProfessorSubjects subjects

  putStrLn ("\nCódigo\t - Disciplina\t - Número de aulas restantes\n" ++ Controller.getProfessorSubjects codesProfessorSubjects professorSubjects)

classRegistrationScreen :: Int -> IO()
classRegistrationScreen id = do
  professorFile <- DataLoader.readArq "./data/professores.csv"
  let professors = DataLoader.loadProfessors professorFile
  let professor = DataLoader.loadProfessor id professors

  subjectsFile <- DataLoader.readArq "./data/disciplinas.csv"
  let subjects = DataLoader.loadSubjects subjectsFile
  let codesProfessorSubjects = Professor.subjects professor
  let professorSubjects = DataLoader.loadSubjectsByCode codesProfessorSubjects subjects

  putStrLn "\nSubjects lecionadas:"
  putStrLn ("\nCódigo\t - Disciplina\t - Número de aulas restantes\n" ++ Controller.getProfessorSubjects codesProfessorSubjects professorSubjects)
  putStr "Entre com o código da disciplina: "
  
  code <- getLine
  
  if Professor.hasSubject professor $ read code
    then do
      let subject = DataLoader.loadSubject (read code) professorSubjects
      Controller.registerClass professor (Subject.code subject)
      putStrLn"\nAula registrada com sucesso!\n"
    else putStrLn "O profesor não leciona essa disciplina!\n"

registerTestScreen :: Int -> IO()
registerTestScreen id = do
  professorFile <- DataLoader.readArq "./data/professores.csv"
  let professors = DataLoader.loadProfessors professorFile
  let professor = DataLoader.loadProfessor id professors

  subjectsFile <- DataLoader.readArq "./data/disciplinas.csv"
  let subjects = DataLoader.loadSubjects subjectsFile
  let codesProfessorSubjects = Professor.subjects professor
  let professorSubjects = DataLoader.loadSubjectsByCode codesProfessorSubjects subjects
  
  Controller.registerTest professor

classSituationScreen :: Int -> IO()
classSituationScreen id = do
  professorFile <- DataLoader.readArq "./data/professores.csv"
  let professors = DataLoader.loadProfessors professorFile
  let professor = DataLoader.loadProfessor id professors

  subjectsFile <- DataLoader.readArq "./data/disciplinas.csv"
  let subjects = DataLoader.loadSubjects subjectsFile

  Controller.classSituation professor subjects

totalAverage :: Int -> IO()
totalAverage studentRegistration = do
  subjectsFile <- DataLoader.readArq "./data/disciplinas.csv"
  studentsFile <- DataLoader.readArq "./data/alunos.csv"
  let subjects = DataLoader.loadSubjects subjectsFile
  let students = DataLoader.loadStudents studentsFile
  let student = DataLoader.loadStudent studentRegistration students

  putStr "\nCRA: "
  printf "%.2f\n\n" (Student.totalAverage student subjects)

registrationScreen :: String -> IO ()
registrationScreen option = do
  putStr "\nDigite a matrícula: \n> "
  id <- getLine

  putStr "Digite seu nome: \n> "
  name <- getLine

  putStr "Digite sua senha: \n> "
  password <- getLine

  if option == "professor"
    then Controller.registerProfessor (read id) name password
    else Controller.registerStudent (read id) name password

createSubjectScreen :: IO ()
createSubjectScreen = do
  subjectsFile <- DataLoader.readArq "./data/disciplinas.csv"
  let subjects = DataLoader.loadSubjects subjectsFile
  let subjectCodes = map Subject.code subjects

  putStr "\nDigite o código da disciplina: \n> "
  subjectCode <- getLine

  putStr "Digite o nome da disciplina: \n> "
  subjectName <- getLine

  putStr "Digite o número de aulas: \n> "
  numberClasses <- getLine

  putStr "Digite a quantidade de vagas: \n> "
  studentLimit <- getLine

  if read subjectCode `elem` subjectCodes
    then putStrLn "Subject já cadastrada!"
    else do
      let newSubject = Subject.newSubject (read subjectCode) 0 subjectName (read numberClasses) (read studentLimit) []
      DataSaver.saveSubject newSubject
      putStrLn "Disciplina cadastrada com sucesso!"

associateTeacherScreen :: IO ()
associateTeacherScreen = do
  professorsFile <- DataLoader.readArq "./data/professores.csv"
  subjectsFile <- DataLoader.readArq "./data/disciplinas.csv"
  let professors = DataLoader.loadProfessors professorsFile
  let subjects = DataLoader.loadSubjects subjectsFile

  putStrLn "Professores disponíveis:"
  putStr $ Controller.listAvailableProfessors professors

  putStr "Matrícula do professor a ser associado > "
  id <- getLine
  
  let professor = DataLoader.loadProfessor (read id) professors
  if Professor.name professor /= "not found"
    then do
      Controller.associateProfessor professor subjects
    else putStrLn "Professor inexistente"

listStudentsWithoutEnrollment :: IO ()
listStudentsWithoutEnrollment = do
  showData "Estudantes sem matrículas:" "./data/alunos.csv" Controller.listStudentsWithoutRegistration DataLoader.loadStudents

listProfessorWithoutEnrollment :: IO ()
listProfessorWithoutEnrollment = do
  showData "Professores sem disciplinas:" "./data/professores.csv" Controller.listProfessorsWithoutRegistration DataLoader.loadProfessors

showsSubjectHigherAverage :: IO ()
showsSubjectHigherAverage = do
  showData "Disciplina com maior média:" "./data/disciplinas.csv" Controller.showsSubjectWithHigherAverage DataLoader.loadSubjects

showsSubjectLowestAverage :: IO ()
showsSubjectLowestAverage = do
  showData "Disciplina com menor média:" "./data/disciplinas.csv" Controller.showsSubjectWithLowestAverage DataLoader.loadSubjects

showStudentWithHighestAverage :: IO()
showStudentWithHighestAverage = do
  studentsFile <- DataLoader.readArq "./data/alunos.csv"
  subjectsFile <- DataLoader.readArq "./data/disciplinas.csv"
  let students = DataLoader.loadStudents studentsFile
  let subjects = DataLoader.loadSubjects subjectsFile

  putStrLn $ "Estudante com a maior média geral:\n" ++ Controller.showStudentWithHighestAverage students subjects

showData :: String -> String -> ([t] -> String) -> ([String] -> [t]) -> IO ()
showData message filePath display loadAll = do
  entityFile <- DataLoader.readArq filePath
  let entities = loadAll entityFile

  putStrLn $ "\n" ++ display entities

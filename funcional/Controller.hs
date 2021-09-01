module Controller where

import Student (Student)
import qualified Student
import DataLoader
import qualified DataLoader
import DataSaver
import qualified DataSaver
import Subject (Subject)
import qualified Subject
import Professor (Professor)
import qualified Professor
import Text.Printf
import Data.List (delete, sort)

registerProfessor :: Int -> String -> String -> IO ()
registerProfessor professorId professorName password = do
  arqProfessors <- readArq "./data/professores.csv"
  let registeredProfessors = loadProfessors arqProfessors
  let registeredEnrollments = Professor.registrations registeredProfessors

  if isThereRegristration professorId registeredEnrollments
    then putStrLn "Professor já cadastrado!"
    else do
      saveProfessor professor password
      putStrLn "Professor cadastrado!"
  where
    professor = Professor.newProfessor professorId professorName []

registerStudent :: Int -> String -> String -> IO ()
registerStudent studentId studentName password = do
  arqStudents <- readArq "./data/alunos.csv"
  let registeredStudents = loadStudents arqStudents
  let registeredEnrollments = Student.registrations registeredStudents

  if isThereRegristration studentId registeredEnrollments
    then putStrLn "Professor já cadastrado!"
    else do
      saveStudent aluno password
      putStrLn "Professor cadastrado!"
  where
    aluno = Student.newStudent studentId studentName []

isThereRegristration :: Int -> [Int] -> Bool
isThereRegristration id ids = id `elem` ids

listStudentsWithoutRegistration :: [Student] -> String
listStudentsWithoutRegistration [] = ""
listStudentsWithoutRegistration (a : as) =
  if null $ Student.enrolledSubjects a
    then listFormatStudent a ++ "\n" ++ listStudentsWithoutRegistration as
    else listStudentsWithoutRegistration as

listFormatStudent :: Student -> String
listFormatStudent student = show (Student.registration student) ++ "\t - \t" ++ Student.name student

listProfessorsWithoutRegistration :: [Professor] -> String
listProfessorsWithoutRegistration [] = ""
listProfessorsWithoutRegistration (p : ps) =
  if null $ Professor.subjects p
    then formatListProfessor p ++ "\n" ++ listProfessorsWithoutRegistration ps
    else listProfessorsWithoutRegistration ps

listAvailableProfessors :: [Professor] -> String
listAvailableProfessors [] = ""
listAvailableProfessors (p : ps) =
  if Professor.numberOfSubjects p < 3
    then formatListProfessor p ++ "\n" ++ listAvailableProfessors ps
    else listAvailableProfessors ps

-- formatStudents :: [Student] -> String
-- formatStudents [] = ""
-- formatStudents (s : sa) =
--   formatStudent s ++ "\n" ++ formatStudents sa

formatStudent :: Student -> [Subject] -> String
formatStudent student subjects = 
  show (Student.registration student) ++ "\t - \t" ++ Student.name student ++ "\t - \t" ++ printf "%.2f" (Student.totalAverage student subjects)

showStudentWithHighestAverage :: [Student] -> [Subject] -> String
showStudentWithHighestAverage students subjects =
  formatStudent (highestAverageStudent students subjects) subjects

highestAverageStudent :: [Student] -> [Subject] -> Student
highestAverageStudent students subjects = do
  let studentsAverage' = studentsAverage students subjects
  let grades = map snd studentsAverage'
  let studentRegistration = highestAverageStudentCode studentsAverage' grades
  DataLoader.loadStudent studentRegistration students

studentsAverage :: [Student] -> [Subject] -> [(Int, Double)]
studentsAverage students subjects = [(Student.registration s, Student.totalAverage s subjects) | s <- students]

highestAverageStudentCode :: [(Int, Double)] -> [Double] -> Int
highestAverageStudentCode [] _ = -1
highestAverageStudentCode (s : sa) grades =
  if snd s == highestGrade then fst s else highestAverageStudentCode sa grades
  where
    highestGrade = maximum grades

availableSubjectsForAssociation :: Professor -> [Subject] -> [Subject]
availableSubjectsForAssociation professor subjects = 
  DataLoader.loadSubjectsByCode codesAvailableSubjects subjects
  where
    subjectsWithoutProfessor = filter (\subj -> not (Subject.hasProfessor subj)) subjects
    codesSubjects = map Subject.code subjectsWithoutProfessor
    subjectsTaught = Professor.subjects professor
    codesAvailableSubjects = filter (`notElem` subjectsTaught) codesSubjects

associateProfessor :: Professor -> [Subject] -> IO ()
associateProfessor professor subjects = do
  if null availableSubjects
    then putStrLn "Não há disciplinas disponíveis para esse professor!" 
    else do 
      putStrLn "Subjects disponíveis"
      putStr $ Controller.formatListSubjects availableSubjects

      putStr "Código da disciplina a ser associada > "
      subjectCode <- getLine
      let subject = DataLoader.loadSubject (read subjectCode) subjects

      if Subject.name subject /= "not found"
        then associateProfessorSubject professor subject subjects
        else putStrLn "Subject inválida"
  where 
    availableSubjects = availableSubjectsForAssociation professor subjects

associateProfessorSubject :: Professor -> Subject -> [Subject] -> IO ()
associateProfessorSubject professor subject subjects =
  if notElem (Subject.code subject) codesSubjects || Professor.hasSubject professor (Subject.code subject) || Subject.hasProfessor subject
    then putStrLn "Erro ao associar professor à disciplina"
    else do
      DataSaver.updateProfessor (Professor.registration professor) updatedProfessor
      DataSaver.updateSubject (Subject.code subject) updatedSubject
      putStrLn "Subject associada!"
  where
    codesSubjects = map Subject.code subjects
    subjectsTaught = Professor.subjects professor
    updatedProfessor = Professor.newProfessor (Professor.registration professor) (Professor.name professor) (Subject.code subject : Professor.subjects professor)
    updatedSubject = Subject.newSubject (Subject.code subject) (Professor.registration professor) (Subject.name subject) (Subject.numberClasses subject) (Subject.studentLimit subject) (Subject.grades subject)

formatListSubjects :: [Subject] -> String
formatListSubjects [] = ""
formatListSubjects (d : ds) = formatListSubject d ++ "\n" ++ formatListSubjects ds

formatListSubject :: Subject -> String
formatListSubject subjects = show (Subject.code subjects) ++ "\t - \t" ++ Subject.name subjects

formatListSubjectAverage :: Subject -> String
formatListSubjectAverage subject =
  formatListSubject subject ++ "\t - \t" ++ show media
  where
    media = Subject.subjectAverage subject

formatListProfessor :: Professor -> String
formatListProfessor professor = show (Professor.registration professor) ++ "\t - \t" ++ Professor.name professor

enrolledSubjects :: Student -> [Subject] -> [Subject]
enrolledSubjects student subjects = [DataLoader.loadSubject c subjects | c <- Student.enrolledSubjects student]

showsSubjectWithHigherAverage :: [Subject] -> String
showsSubjectWithHigherAverage subjects =
  formatListSubjectAverage $ subjectWithHigherAverage subjects

subjectWithHigherAverage :: [Subject] -> Subject
subjectWithHigherAverage subjects = do
  let subjectId = subjectWithHighestAverageCode (subjectsAverage subjects)
  DataLoader.loadSubject subjectId subjects

showsSubjectWithLowestAverage :: [Subject] -> String
showsSubjectWithLowestAverage subjects =
  formatListSubjectAverage $ subjectWithLowestAverage subjects

subjectWithLowestAverage :: [Subject] -> Subject
subjectWithLowestAverage subjects = do
  let subjectId = subjectWithLowestAverageCode (subjectsAverage subjects)
  DataLoader.loadSubject subjectId subjects

subjectsAverage :: [Subject] -> [(Int, Double)]
subjectsAverage subjects = [(Subject.code d, Subject.subjectAverage d) | d <- subjects]

subjectWithHighestAverageCode :: [(Int, Double)] -> Int
subjectWithHighestAverageCode [] = -1
subjectWithHighestAverageCode (d : ds) =
  if snd d == highestGrade then fst d else subjectWithHighestAverageCode ds
  where
    grades = [snd m | m <- d : ds]
    highestGrade = maximum grades

subjectWithLowestAverageCode :: [(Int, Double)] -> Int
subjectWithLowestAverageCode [] = -1
subjectWithLowestAverageCode (d : ds) =
  if snd d == lowestGrade then fst d else subjectWithLowestAverageCode ds
  where
    grades = [snd m | m <- d : ds]
    lowestGrade = minimum grades

showStudentSubjects :: Student -> [Subject] -> String
showStudentSubjects _ [] = ""
showStudentSubjects student (s : sa) =
  Subject.showSubjectWithoutClasses s ++ "\t - " ++ printf "%.2f" (Subject.studentAverage (Student.registration student) s) ++ " " ++ (Student.situation student s) ++ "\n" ++ showStudentSubjects student sa

enroll :: Student -> [Subject] -> IO ()
enroll student subjects = do
  putStrLn ("\nCódigo\t - Subject\n" ++ showAvailableSubjectsToStudent (Student.enrolledSubjects student) subjects)

  putStr "Entre com o código da cadeira: "

  code <- getLine

  putStrLn ""

  let subjectCode = read code :: Int
  let subject = DataLoader.loadSubject subjectCode subjects
  -- verificar codigo da cadeira --
  if subjectCode `elem` map Subject.code subjects && notElem subjectCode (Student.enrolledSubjects student) && (Subject.hasProfessor subject)
    then do
      let newEnrolledSubjects = sort (subjectCode : Student.enrolledSubjects student)

      -- variaveis aluno
      let studentId = Student.registration student
      let studentName = Student.name student

      -- variaveis disciplina
      let subjectName = Subject.name subject
      let numberClassesSubject = Subject.numberClasses subject
      let studentLimit = Subject.studentLimit subject
      let newGrades = (studentId, []) : Subject.grades subject

      let newStudent = Student.newStudent studentId studentName newEnrolledSubjects
      let newSubject = Subject.newSubject subjectCode (Subject.professorRegistration subject) subjectName numberClassesSubject studentLimit newGrades

      -- putStrLn $ Subject.toString newSubject

      DataSaver.updateStudent studentId newStudent
      DataSaver.updateSubject subjectCode newSubject

      putStrLn "Matricula realizada com sucesso!\n" -- matricular ou cancelar matricula do aluno na cadeira
    else putStrLn "Código Inválido\n"

showAvailableSubjectsToStudent :: [Int] -> [Subject] -> String
showAvailableSubjectsToStudent _ [] = ""
showAvailableSubjectsToStudent enrolledSubjectCodes (s : sa) =
  if Subject.code s `notElem` enrolledSubjectCodes && not (Subject.isFinished s) && not (Subject.isFull s) && (Subject.hasProfessor s)
    then Subject.showSubjectWithoutClasses s ++ "\n" ++ showAvailableSubjectsToStudent enrolledSubjectCodes sa
    else showAvailableSubjectsToStudent enrolledSubjectCodes sa

cancelRegistration :: Student -> [Subject] -> [Int] -> IO ()
cancelRegistration student subjects studentCodes = do
  putStrLn ("\nCódigo\t - Subject\n" ++ showSubjectsWithoutClasses subjects)
  putStr "Entre com o código da cadeira: "

  code <- getLine

  putStrLn ""

  let subjectCode = read code :: Int

  -- verificar codigo da cadeira --
  if subjectCode `elem` studentCodes
    then do
      let newCodes = delete subjectCode studentCodes
      let studentId = Student.registration student
      let studentName = Student.name student
      let subject = DataLoader.loadSubject subjectCode subjects

      let newStudent = Student.newStudent studentId studentName newCodes
      let newSubject = Subject.newSubject subjectCode (Subject.professorRegistration subject) (Subject.name subject) (Subject.numberClasses subject) (Subject.studentLimit subject) (removeEnrollment studentId (Subject.grades subject))

      DataSaver.updateStudent studentId newStudent
      DataSaver.updateSubject subjectCode newSubject

      putStrLn "Matricula cancelada com sucesso!\n" -- matricular ou cancelar matricula do aluno na cadeira
    else putStrLn "Código Inválido\n"

removeEnrollment :: Int -> [(Int, [Double])] -> [(Int, [Double])]
removeEnrollment _ [] = []
removeEnrollment registration (g : gs) =
  if fst g == registration
    then removeEnrollment registration gs
    else g : removeEnrollment registration gs

showSubjectsWithoutClasses :: [Subject] -> String
showSubjectsWithoutClasses [] = ""
showSubjectsWithoutClasses (d : ds) =
  Subject.showSubjectWithoutClasses d ++ "\n" ++ showSubjectsWithoutClasses ds

showSubjects :: [Subject] -> String
showSubjects [] = ""
showSubjects (s : sa) =
  Subject.showSubject s ++ "\n" ++ showSubjects sa

getProfessorSubjects :: [Int] -> [Subject] -> String
getProfessorSubjects subjectCodes subjects = do
  let professorSubjects = DataLoader.loadSubjectsByCode subjectCodes subjects
  showSubjects professorSubjects

registerTest :: Professor -> IO ()
registerTest professor = do
  subjectsFile <- DataLoader.readArq "./data/disciplinas.csv"
  studentsFile <- DataLoader.readArq "./data/alunos.csv"
  let subjects = DataLoader.loadSubjects subjectsFile
  let students = DataLoader.loadStudents studentsFile

  putStrLn "\nSubjects lecionadas:\n"
  putStrLn $ getProfessorSubjects (Professor.subjects professor) subjects

  putStr "Entre com o código da disciplina: "
  subjectCode <- getLine

  if read subjectCode `elem` Professor.subjects professor
    then do
      let subject = DataLoader.loadSubject (read subjectCode) subjects
      if not $ Subject.isFinished subject
        then do
          let grades = Subject.grades subject

          if null grades
            then putStrLn "\nNão há matrículas nessa disciplina!\n"
            else do
              let enrolledStudentsCode = Subject.enrolledStudents subject
              let enrolledStudents = DataLoader.loadStudentsByRegistration enrolledStudentsCode students

              addStudentsGrades enrolledStudents subject
        else putStrLn "\nA disciplina encontra-se encerrada!\n"
    else putStrLn "\nO professor não leciona essa disciplina!\n"

addStudentsGrades :: [Student] -> Subject -> IO ()
addStudentsGrades [] _ = putStrLn "\nTodas as notas cadastradas!\n"
addStudentsGrades enrolledStudents subject = do
  putStrLn $ "\nStudents matriculados na disciplina [" ++ Subject.name subject ++ "]"
  putStr $ "\n" ++ showStudents enrolledStudents

  let studentRegistration = Student.registration $ head enrolledStudents

  if studentRegistration `elem` Student.registrations enrolledStudents
    then do
      putStr $ "Digite a nota do aluno [" ++ show studentRegistration ++ "]: "
      grade <- getLine

      if read grade <= 10.0 && read grade >= 0.0 
        then do
          addGrade studentRegistration (read grade) (Subject.code subject)
          addStudentsGrades (tail enrolledStudents) subject
        else do 
          putStrLn "\nErro: Insira um valor válido para a nota. ( 10 <= nota >= 0)"
          addStudentsGrades enrolledStudents subject
    else putStrLn "O aluno não está matriculado!"

addGrade :: Int -> Double -> Int -> IO ()
addGrade studentRegistration grade subjectCode = do
  subjectsFile <- DataLoader.readArq "./data/disciplinas.csv"
  let subjects = DataLoader.loadSubjects subjectsFile
  let subject = DataLoader.loadSubject subjectCode subjects

  let newGrades = replaceGrade studentRegistration grade (Subject.grades subject)
  let newSubject = Subject.newSubject (Subject.code subject) (Subject.professorRegistration subject) (Subject.name subject) (Subject.numberClasses subject) (Subject.studentLimit subject) newGrades
  DataSaver.updateSubject (Subject.code subject) newSubject
  putStrLn "Nota adicionada!"

replaceGrade :: Int -> Double -> [(Int, [Double])] -> [(Int, [Double])]
replaceGrade _ _ [] = []
replaceGrade studentRegistration newGrade (s : sa) =
  if studentRegistration == fst s
    then (fst s, newGrade : snd s) : replaceGrade studentRegistration newGrade sa
    else s : replaceGrade studentRegistration newGrade sa

showStudents :: [Student] -> String
showStudents [] = ""
showStudents (s : sa) =
  showStudent s ++ "\n" ++ showStudents sa

showStudent :: Student -> String
showStudent student = show (Student.registration student) ++ "\t - \t" ++ Student.name student

registerClass :: Professor -> Int -> IO ()
registerClass professor subjectCode =
  if Professor.hasSubject professor subjectCode
    then do
      subjectsFile <- DataLoader.readArq "./data/disciplinas.csv"
      let subjects = DataLoader.loadSubjects subjectsFile
      let subject = DataLoader.loadSubject subjectCode subjects

      if Subject.numberClasses subject > 0
        then do
          let newSubject = Subject.newSubject subjectCode (Subject.professorRegistration subject) (Subject.name subject) (Subject.numberClasses subject - 1) (Subject.studentLimit subject) (Subject.grades subject)
          DataSaver.updateSubject subjectCode newSubject
        else putStrLn "A disciplina encontra-se encerrada!"
    else putStrLn "Subject inválida"

classSituation :: Professor -> [Subject] -> IO()
classSituation professor subjects = do

  let codesProfessorSubjects = Professor.subjects professor
  let professorSubjects = DataLoader.loadSubjectsByCode codesProfessorSubjects subjects

  putStrLn "\nSubjects lecionadas:"
  putStr $ "\n" ++ Controller.getProfessorSubjects (Professor.subjects professor) subjects

  putStr "\nEntre com o código da disciplina a ser consultada: "
  code <- getLine
  if read code `elem` codesProfessorSubjects
    then do
      let subject = DataLoader.loadSubject (read code) professorSubjects

      classSituation' subject
    else putStrLn "\nO professor não leciona essa disciplina.\n"

classSituation' :: Subject -> IO ()
classSituation' subject = do
  studentsFile <- DataLoader.readArq "./data/alunos.csv"
  let students = DataLoader.loadStudents studentsFile
  let subjectStudents = DataLoader.loadStudentsByRegistration (Subject.enrolledStudents subject) students

  if null subjectStudents then putStrLn "\nA disciplina não possui alunos cadastrados.\n"
  else putStr $ "\nMatrícula\t - Student\t - Média\n" ++ studentsSituations subjectStudents subject ++ "\n"

studentsSituations :: [Student] -> Subject -> String
studentsSituations [] _ = ""
studentsSituations (s : sa) subject =
  studentSituation s subject ++ "\n" ++ studentsSituations sa subject

studentSituation :: Student -> Subject -> String
studentSituation student subject = do
  show (Student.registration student) ++ "\t\t - " ++ Student.name student ++ printf "\t - %.2f" (Student.subjectAverage student subject) ++ " " ++ Student.situation student subject

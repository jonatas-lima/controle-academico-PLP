module Controle where

import Aluno (Aluno)
import qualified Aluno
import DataLoader
import qualified DataLoader
import DataSaver
import qualified DataSaver
import Disciplina (Disciplina)
import qualified Disciplina
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
  let registeredEnrollments = Aluno.registrations registeredStudents

  if isThereRegristration studentId registeredEnrollments
    then putStrLn "Professor já cadastrado!"
    else do
      saveStudent aluno password
      putStrLn "Professor cadastrado!"
  where
    aluno = Aluno.newStudent studentId studentName []

isThereRegristration :: Int -> [Int] -> Bool
isThereRegristration id ids = id `elem` ids

listStudentsWithoutRegistration :: [Aluno] -> String
listStudentsWithoutRegistration [] = ""
listStudentsWithoutRegistration (a : as) =
  if null $ Aluno.enrolledSubjects a
    then listFormatStudent a ++ "\n" ++ listStudentsWithoutRegistration as
    else listStudentsWithoutRegistration as

listFormatStudent :: Aluno -> String
listFormatStudent student = show (Aluno.registration student) ++ "\t - \t" ++ Aluno.name student

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

availableSubjectsForAssociation :: Professor -> [Disciplina] -> [Disciplina]
availableSubjectsForAssociation professor subjects = 
  DataLoader.loadSubjectsByCode codesAvailableSubjects subjects
  where
    subjectsWithoutProfessor = filter (\subj -> not (Disciplina.hasProfessor subj)) subjects
    codesSubjects = map Disciplina.code subjectsWithoutProfessor
    subjectsTaught = Professor.subjects professor
    codesAvailableSubjects = filter (`notElem` subjectsTaught) codesSubjects

associateProfessor :: Professor -> [Disciplina] -> IO ()
associateProfessor professor subjects = do
  if null availableSubjects
    then putStrLn "Não há disciplinas disponíveis para esse professor!" 
    else do 
      putStrLn "Disciplinas disponíveis"
      putStr $ Controle.formatListSubjects availableSubjects

      putStr "Código da disciplina a ser associada > "
      subjectCode <- getLine
      let subject = DataLoader.loadSubject (read subjectCode) subjects

      if Disciplina.name subject /= "not found"
        then associateProfessorSubject professor subject subjects
        else putStrLn "Disciplina inválida"
  where 
    availableSubjects = availableSubjectsForAssociation professor subjects

associateProfessorSubject :: Professor -> Disciplina -> [Disciplina] -> IO ()
associateProfessorSubject professor subject subjects =
  if notElem (Disciplina.code subject) codesSubjects || Professor.hasSubject professor (Disciplina.code subject) || Disciplina.hasProfessor subject
    then putStrLn "Erro ao associar professor à disciplina"
    else do
      DataSaver.updateProfessor (Professor.registration professor) updatedProfessor
      DataSaver.updateSubject (Disciplina.code subject) updatedSubject
      putStrLn "Disciplina associada!"
  where
    codesSubjects = map Disciplina.code subjects
    subjectsTaught = Professor.subjects professor
    updatedProfessor = Professor.newProfessor (Professor.registration professor) (Professor.name professor) (Disciplina.code subject : Professor.subjects professor)
    updatedSubject = Disciplina.newSubject (Disciplina.code subject) (Professor.registration professor) (Disciplina.name subject) (Disciplina.numberClasses subject) (Disciplina.studentLimit subject) (Disciplina.grades subject)

formatListSubjects :: [Disciplina] -> String
formatListSubjects [] = ""
formatListSubjects (d : ds) = formatListSubject d ++ "\n" ++ formatListSubjects ds

formatListSubject :: Disciplina -> String
formatListSubject subjects = show (Disciplina.code subjects) ++ "\t - \t" ++ Disciplina.name subjects

formatListSubjectAverage :: Disciplina -> String
formatListSubjectAverage subject =
  formatListSubject subject ++ "\t - \t" ++ show media
  where
    media = Disciplina.subjectAverage subject

formatListProfessor :: Professor -> String
formatListProfessor professor = show (Professor.registration professor) ++ "\t - \t" ++ Professor.name professor

enrolledSubjects :: Aluno -> [Disciplina] -> [Disciplina]
enrolledSubjects student subjects = [DataLoader.loadSubject c subjects | c <- Aluno.enrolledSubjects student]

showsSubjectWithHigherAverage :: [Disciplina] -> String
showsSubjectWithHigherAverage subjects =
  formatListSubjectAverage $ subjectWithHigherAverage subjects

subjectWithHigherAverage :: [Disciplina] -> Disciplina
subjectWithHigherAverage subjects = do
  let subjectId = subjectWithHighestAverageCode (subjectsAverage subjects)
  DataLoader.loadSubject subjectId subjects

showsSubjectWithLowestAverage :: [Disciplina] -> String
showsSubjectWithLowestAverage subjects =
  formatListSubjectAverage $ subjectWithLowestAverage subjects

subjectWithLowestAverage :: [Disciplina] -> Disciplina
subjectWithLowestAverage subjects = do
  let subjectId = subjectWithLowestAverageCode (subjectsAverage subjects)
  DataLoader.loadSubject subjectId subjects

subjectsAverage :: [Disciplina] -> [(Int, Double)]
subjectsAverage subjects = [(Disciplina.code d, Disciplina.subjectAverage d) | d <- subjects]

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

showStudentSubjects :: Aluno -> [Disciplina] -> String
showStudentSubjects _ [] = ""
showStudentSubjects student (s : sa) =
  Disciplina.showSubjectWithoutClasses s ++ "\t - " ++ printf "%.2f" (Disciplina.studentAverage (Aluno.registration student) s) ++ " " ++ (Aluno.situation student s) ++ "\n" ++ showStudentSubjects student sa

enroll :: Aluno -> [Disciplina] -> IO ()
enroll student subjects = do
  putStrLn ("\nCódigo\t - Disciplina\n" ++ showAvailableSubjectsToStudent (Aluno.enrolledSubjects student) subjects)

  putStr "Entre com o código da cadeira: "

  code <- getLine

  putStrLn ""

  let subjectCode = read code :: Int
  let subject = DataLoader.loadSubject subjectCode subjects
  -- verificar codigo da cadeira --
  if subjectCode `elem` map Disciplina.code subjects && notElem subjectCode (Aluno.enrolledSubjects student)
    then do
      let newEnrolledSubjects = sort (subjectCode : Aluno.enrolledSubjects student)

      -- variaveis aluno
      let studentId = Aluno.registration student
      let studentName = Aluno.name student

      -- variaveis disciplina
      let subjectName = Disciplina.name subject
      let numberClassesSubject = Disciplina.numberClasses subject
      let studentLimit = Disciplina.studentLimit subject
      let newGrades = (studentId, []) : Disciplina.grades subject

      let newStudent = Aluno.newStudent studentId studentName newEnrolledSubjects
      let newSubject = Disciplina.newSubject subjectCode (Disciplina.professorRegistration subject) subjectName numberClassesSubject studentLimit newGrades

      -- putStrLn $ Disciplina.toString newDisciplina

      DataSaver.updateStudent studentId newStudent
      DataSaver.updateSubject subjectCode newSubject

      putStrLn "Matricula realizada com sucesso!\n" -- matricular ou cancelar matricula do aluno na cadeira
    else putStrLn "Código Inválido\n"

showAvailableSubjectsToStudent :: [Int] -> [Disciplina] -> String
showAvailableSubjectsToStudent _ [] = ""
showAvailableSubjectsToStudent enrolledSubjectCodes (s : sa) =
  if Disciplina.code s `notElem` enrolledSubjectCodes && not (Disciplina.isFinished s) && not (Disciplina.isFull s)
    then Disciplina.showSubjectWithoutClasses s ++ "\n" ++ showAvailableSubjectsToStudent enrolledSubjectCodes sa
    else showAvailableSubjectsToStudent enrolledSubjectCodes sa

cancelRegistration :: Aluno -> [Disciplina] -> [Int] -> IO ()
cancelRegistration student subjects studentCodes = do
  putStrLn ("\nCódigo\t - Disciplina\n" ++ showSubjectsWithoutClasses subjects)
  putStr "Entre com o código da cadeira: "

  code <- getLine

  putStrLn ""

  let subjectCode = read code :: Int

  -- verificar codigo da cadeira --
  if subjectCode `elem` studentCodes
    then do
      let newCodes = delete subjectCode studentCodes
      let studentId = Aluno.registration student
      let studentName = Aluno.name student
      let subject = DataLoader.loadSubject subjectCode subjects

      let newStudent = Aluno.newStudent studentId studentName newCodes
      let newSubject = Disciplina.newSubject subjectCode (Disciplina.professorRegistration subject) (Disciplina.name subject) (Disciplina.numberClasses subject) (Disciplina.studentLimit subject) (removeEnrollment studentId (Disciplina.grades subject))

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

showSubjectsWithoutClasses :: [Disciplina] -> String
showSubjectsWithoutClasses [] = ""
showSubjectsWithoutClasses (d : ds) =
  Disciplina.showSubjectWithoutClasses d ++ "\n" ++ showSubjectsWithoutClasses ds

showSubjects :: [Disciplina] -> String
showSubjects [] = ""
showSubjects (s : sa) =
  Disciplina.showSubject s ++ "\n" ++ showSubjects sa

getProfessorSubjects :: [Int] -> [Disciplina] -> String
getProfessorSubjects subjectCodes subjects = do
  let professorSubjects = DataLoader.loadSubjectsByCode subjectCodes subjects
  showSubjects professorSubjects

registerTest :: Professor -> IO ()
registerTest professor = do
  subjectsFile <- DataLoader.readArq "./data/disciplinas.csv"
  studentsFile <- DataLoader.readArq "./data/alunos.csv"
  let subjects = DataLoader.loadSubjects subjectsFile
  let students = DataLoader.loadStudents studentsFile

  putStrLn "\nDisciplinas lecionadas:\n"
  putStrLn $ getProfessorSubjects (Professor.subjects professor) subjects

  putStr "Entre com o código da disciplina: "
  subjectCode <- getLine

  if read subjectCode `elem` Professor.subjects professor
    then do
      let subject = DataLoader.loadSubject (read subjectCode) subjects
      if not $ Disciplina.isFinished subject
        then do
          let grades = Disciplina.grades subject

          if null grades
            then putStrLn "\nNão há matrículas nessa disciplina!\n"
            else do
              let enrolledStudentsCode = Disciplina.enrolledStudents subject
              let enrolledStudents = DataLoader.loadStudentsByRegistration enrolledStudentsCode students

              addStudentsGrades enrolledStudents subject
        else putStrLn "\nA disciplina encontra-se encerrada!\n"
    else putStrLn "\nO professor não leciona essa disciplina!\n"

addStudentsGrades :: [Aluno] -> Disciplina -> IO ()
addStudentsGrades [] _ = putStrLn "\nTodas as notas cadastradas!\n"
addStudentsGrades enrolledStudents subject = do
  putStrLn $ "\nAlunos matriculados na disciplina [" ++ Disciplina.name subject ++ "]"
  putStr $ "\n" ++ showStudents enrolledStudents

  let studentRegistration = Aluno.registration $ head enrolledStudents

  if studentRegistration `elem` Aluno.registrations enrolledStudents
    then do
      putStr $ "Digite a nota do aluno [" ++ show studentRegistration ++ "]: "
      grade <- getLine

      if read grade <= 10.0 && read grade >= 0.0 
        then do
          addGrade studentRegistration (read grade) (Disciplina.code subject)
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

  let newGrades = replaceGrade studentRegistration grade (Disciplina.grades subject)
  let newSubject = Disciplina.newSubject (Disciplina.code subject) (Disciplina.professorRegistration subject) (Disciplina.name subject) (Disciplina.numberClasses subject) (Disciplina.studentLimit subject) newGrades
  DataSaver.updateSubject (Disciplina.code subject) newSubject
  putStrLn "Nota adicionada!"

replaceGrade :: Int -> Double -> [(Int, [Double])] -> [(Int, [Double])]
replaceGrade _ _ [] = []
replaceGrade studentRegistration newGrade (s : sa) =
  if studentRegistration == fst s
    then (fst s, newGrade : snd s) : replaceGrade studentRegistration newGrade sa
    else s : replaceGrade studentRegistration newGrade sa

showStudents :: [Aluno] -> String
showStudents [] = ""
showStudents (s : sa) =
  showStudent s ++ "\n" ++ showStudents sa

showStudent :: Aluno -> String
showStudent student = show (Aluno.registration student) ++ "\t - \t" ++ Aluno.name student

registerClass :: Professor -> Int -> IO ()
registerClass professor subjectCode =
  if Professor.hasSubject professor subjectCode
    then do
      subjectsFile <- DataLoader.readArq "./data/disciplinas.csv"
      let subjects = DataLoader.loadSubjects subjectsFile
      let subject = DataLoader.loadSubject subjectCode subjects

      if Disciplina.numberClasses subject > 0
        then do
          let newSubject = Disciplina.newSubject subjectCode (Disciplina.professorRegistration subject) (Disciplina.name subject) (Disciplina.numberClasses subject - 1) (Disciplina.studentLimit subject) (Disciplina.grades subject)
          DataSaver.updateSubject subjectCode newSubject
        else putStrLn "A disciplina encontra-se encerrada!"
    else putStrLn "Disciplina inválida"

classSituation :: Professor -> [Disciplina] -> IO()
classSituation professor subjects = do

  let codesProfessorSubjects = Professor.subjects professor
  let professorSubjects = DataLoader.loadSubjectsByCode codesProfessorSubjects subjects

  putStrLn "\nDisciplinas lecionadas:"
  putStr $ "\n" ++ Controle.getProfessorSubjects (Professor.subjects professor) subjects

  putStr "\nEntre com o código da disciplina a ser consultada: "
  code <- getLine
  if read code `elem` codesProfessorSubjects
    then do
      let subject = DataLoader.loadSubject (read code) professorSubjects

      classSituation' subject
    else putStrLn "\nO professor não leciona essa disciplina.\n"

classSituation' :: Disciplina -> IO ()
classSituation' subject = do
  studentsFile <- DataLoader.readArq "./data/alunos.csv"
  let students = DataLoader.loadStudents studentsFile
  let subjectStudents = DataLoader.loadStudentsByRegistration (Disciplina.enrolledStudents subject) students

  if null subjectStudents then putStrLn "\nA disciplina não possui alunos cadastrados.\n"
  else putStr $ "\nMatrícula\t - Aluno\t - Média\n" ++ studentsSituations subjectStudents subject ++ "\n"

studentsSituations :: [Aluno] -> Disciplina -> String
studentsSituations [] _ = ""
studentsSituations (s : sa) subject =
  studentSituation s subject ++ "\n" ++ studentsSituations sa subject

studentSituation :: Aluno -> Disciplina -> String
studentSituation student subject = do
  show (Aluno.registration student) ++ "\t\t - " ++ Aluno.name student ++ printf "\t - %.2f" (Aluno.subjectAverage student subject) ++ " " ++ Aluno.situation student subject

module Main where

import Aluno (Aluno)
import qualified Aluno
import Control.Concurrent (threadDelay)
import qualified Controle
import Data.List (delete, sort)
import qualified DataLoader
import qualified DataSaver
import Disciplina (Disciplina)
import qualified Disciplina
import Professor (Professor)
import qualified Professor
import System.Console.ANSI (clearScreen)
import Text.Printf
import qualified Usuario

main :: IO ()
main = do
  putStrLn ("Bem-Vindo(a)!" ++ "\nPara acessar o controle, faça login:\n")
  loginScreen

loginScreen :: IO ()
loginScreen = do
  putStr "Digite sua matrícula: "
  userId <- getLine

  putStr "Digite sua senha: "
  password <- getLine

  usersFile <- DataLoader.readArq "./data/usuarios.csv"
  let availableUsers = DataLoader.loadUsers usersFile

  let authentication = Usuario.authenticates userId password availableUsers
  let authenticated = fst authentication
  let role = snd authentication

  if authenticated
    then do
      putStrLn "\nLogin realizado..."
      threadDelay (10 ^ 6)
      clearScreen
      screen userId role
    else do
      putStr "\nUsuario ou senha invalido! Deseja tentar novamente? (s/n) "
      opcao <- getLine
      if opcao == "s"
        then do
          clearScreen
          loginScreen
        else
          if opcao == "n"
            then do
              putStr "\nSaindo..."
              threadDelay (10 ^ 6)
            else do
              putStr "\nOpção inválida. Saindo do sistema por segurança."
              threadDelay (10 ^ 6)

screen :: String -> String -> IO ()
screen id role
  | role == "prof" = professorScreen id
  | role == "admin" = adminScreen
  | role == "aluno" = studentScreen id
  | otherwise = putStrLn "Role inválido."

header :: Int -> String -> String
header id name =
  "\n--------------------------\n"
    ++ "Usuário: "
    ++ show id
    ++ " - "
    ++ name

studentScreen :: String -> IO ()
studentScreen id' = do
  studentsFile <- DataLoader.readArq "./data/alunos.csv"
  let students = DataLoader.loadStudents studentsFile
  let student = DataLoader.loadStudent (read id') students

  subjectsFile <- DataLoader.readArq "./data/disciplinas.csv"
  let subjects = DataLoader.loadSubjects subjectsFile

  -- variaveis auxiliares
  let enrolledSubjectsCodes = Aluno.enrolledSubjects student
  let enrolledSubjects = DataLoader.loadSubjectsByCode enrolledSubjectsCodes subjects

  putStrLn "\n\n--- Controle Acadêmico ---"

  putStrLn (studentOptions student)

  putStr "Qual a opcao selecionada?\n> "
  option <- getLine
  putStrLn ""

  if option == "1"
    then putStrLn ("Código\t - Disciplina\t - Média\n" ++ showSubjects (read id') enrolledSubjects)
    else
      if option == "2"
        then
          if Aluno.numberEnrolledSubjects student < 4
            then enroll student subjects
            else putStrLn ("O aluno [" ++ printf "%.d" (Aluno.registration student) ++ "] já possui 4 disciplinas matriculadas!\n")
        else
          if option == "3"
            then
              if Aluno.numberEnrolledSubjects student > 0
                then cancelRegistration student enrolledSubjects enrolledSubjectsCodes
                else putStrLn "O aluno não está matriculado em nenhuma disciplina!"
            else
              if option == "4"
                then do
                  putStr "CRA: "
                  printf "%.2f" (Aluno.totalAverage student subjects)
                  putStrLn "\n"
                else
                  if option == "5"
                    then do
                      clearScreen
                      quit id' "aluno"
                    else
                      if option == "6"
                        then do
                          clearScreen
                          logout id' "aluno"
                        else putStrLn "Opção inválida"

  if option /= "5" && option /= "6"
    then do
      putStr "Pressione enter para continuar..."
      x <- getLine
      clearScreen
      studentScreen id'
    else putStrLn ""

studentOptions :: Aluno -> String
studentOptions student =
  header (Aluno.registration student) (Aluno.name student) ++ Aluno.availableOptions

-- showStudentSubjects' :: Aluno -> Int -> [Disciplina] -> String
-- showStudentSubjects' student codeSubject subjects = do
--   let subject = DataLoader.loadSubject codeSubject subjects
--   Disciplina.showSubjectWithoutClasses subject ++ "\t - " ++ printf "%.2f" (Aluno.subjectAverage student subject) ++ "\n"

-- showStudentSubjects :: Aluno -> [Int] -> [Disciplina] -> String
-- showStudentSubjects student _ [] = ""
-- showStudentSubjects student [] _ = ""
-- showStudentSubjects student (c : cs) (d : ds) =
--   if c == Disciplina.code d
--     then showStudentSubjects' student c (d : ds) ++ showStudentSubjects student cs ds
--     else showStudentSubjects student (c : cs) ds

showSubjects :: Int -> [Disciplina] -> String
showSubjects _ [] = ""
showSubjects studentRegistration (s : sa) =
  Disciplina.showSubjectWithoutClasses s ++ "\t - " ++ printf "%.2f" (Disciplina.studentAverage studentRegistration s) ++ "\n"

showAvailableSubjectsToStudent :: [Int] -> [Disciplina] -> String
showAvailableSubjectsToStudent _ [] = ""
showAvailableSubjectsToStudent enrolledSubjectCodes (s : sa) =
  if Disciplina.code s `notElem` enrolledSubjectCodes && not (Disciplina.isFinished s)
    then Disciplina.showSubjectWithoutClasses s ++ "\n" ++ showAvailableSubjectsToStudent enrolledSubjectCodes sa
    else showAvailableSubjectsToStudent enrolledSubjectCodes sa

enroll :: Aluno -> [Disciplina] -> IO ()
enroll student subjects = do
  putStrLn ("Código\t - Disciplina\n" ++ showAvailableSubjectsToStudent (Aluno.enrolledSubjects student) subjects)

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
      let newGrades = (studentId, []) : Disciplina.grades subject

      let newStudent = Aluno.newStudent studentId studentName newEnrolledSubjects
      let newSubject = Disciplina.newSubject subjectCode subjectName numberClassesSubject newGrades

      -- putStrLn $ Disciplina.toString newDisciplina

      DataSaver.updateStudent studentId newStudent
      DataSaver.updateSubject subjectCode newSubject

      putStrLn "Matricula realizada com sucesso!\n" -- matricular ou cancelar matricula do aluno na cadeira
    else putStrLn "Código Inválido\n"

cancelRegistration :: Aluno -> [Disciplina] -> [Int] -> IO ()
cancelRegistration student subjects studentCodes = do
  putStrLn ("Código\t - Disciplina\n" ++ showSubjectsWithoutClasses subjects)
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
      let newSubject = Disciplina.newSubject subjectCode (Disciplina.name subject) (Disciplina.numberClasses subject) (removeEnrollment studentId (Disciplina.grades subject))

      DataSaver.updateStudent studentId newStudent
      DataSaver.updateSubject subjectCode newSubject

      putStrLn "Matricula cancelada...\n" -- matricular ou cancelar matricula do aluno na cadeira
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

professorScreen :: String -> IO ()
professorScreen id' = do
  professorFile <- DataLoader.readArq "./data/professores.csv"
  let professors = DataLoader.loadProfessors professorFile
  let professor = DataLoader.loadProfessor (read id') professors

  arqSubjects <- DataLoader.readArq "./data/disciplinas.csv"
  let subjects = DataLoader.loadSubjects arqSubjects
  let codesProfessorSubjects = Professor.subjects professor
  let professorSubjects = DataLoader.loadSubjectsByCode codesProfessorSubjects subjects

  putStrLn (professorOptions professor)

  putStr "Qual a opcao selecionada? "
  option <- getLine

  if option == "1"
    then putStrLn ("\nCódigo\t - Disciplina\t - Numero de aulas restantes\n" ++ showProfessorSubjects codesProfessorSubjects professorSubjects)
    else
      if option == "2"
        then do
          putStrLn "Essas são as disciplinas que você leciona:"
          putStrLn ("\nCódigo\t - Disciplina\t - Numero de aulas restantes\n" ++ showProfessorSubjects codesProfessorSubjects professorSubjects)
          putStr "Código da disciplina para qual você deseja cadastrar aula: "
          code <- getLine
          if Professor.hasSubject professor $ read code
            then do
              let subject = DataLoader.loadSubject (read code) professorSubjects
              registerClass professor (Disciplina.code subject)
            else putStrLn "Disciplina inválida"
        else
          if option == "3"
            then registerTest professor
            else
              if option == "4"
                then do
                  clearScreen
                  quit id' "prof"
                else
                  if option == "5"
                    then do
                      clearScreen
                      logout id' "prof"
                    else putStrLn "Opção inválida"

  if option /= "4" && option /= "5"
    then do
      putStr "Pressione enter para continuar..."
      x <- getLine
      clearScreen
      professorScreen id'
    else putStrLn ""

professorOptions :: Professor -> String
professorOptions professor =
  header (Professor.registration professor) (Professor.name professor) ++ Professor.availableOptions

registerTest :: Professor -> IO ()
registerTest professor = do
  subjectsFile <- DataLoader.readArq "./data/disciplinas.csv"
  studentsFile <- DataLoader.readArq "./data/alunos.csv"
  let subjects = DataLoader.loadSubjects subjectsFile
  let students = DataLoader.loadStudents studentsFile

  putStrLn "Disciplinas lecionadas:"
  putStrLn $ showProfessorSubjects (Professor.subjects professor) subjects

  putStr "Qual disciplina (código) deseja registrar uma prova?\n > "
  subjectCode <- getLine

  if read subjectCode `elem` Professor.subjects professor
    then do
      let subject = DataLoader.loadSubject (read subjectCode) subjects
      if not $ Disciplina.isFinished subject
        then do
          let grades = Disciplina.grades subject

          if null grades
            then putStrLn "Não há matrículas nessa disciplina!"
            else do
              let enrolledStudentsCode = Disciplina.enrolledStudents subject
              let enrolledStudents = DataLoader.loadStudentsByRegistration enrolledStudentsCode students

              addStudentsGrades enrolledStudents subject
        else putStrLn "A disciplina encontra-se encerrada!"
    else putStrLn "O professor não leciona essa disciplina!"

addStudentsGrades :: [Aluno] -> Disciplina -> IO ()
addStudentsGrades [] _ = putStrLn "Todas as notas cadastradas!"
addStudentsGrades enrolledStudents subject = do
  putStrLn $ "Alunos matriculados na disciplina " ++ Disciplina.name subject
  putStr $ showStudents enrolledStudents

  let studentRegistration = Aluno.registration $ head enrolledStudents

  if studentRegistration `elem` Aluno.registrations enrolledStudents
    then do
      putStr $ "Digite a nota do aluno " ++ show studentRegistration ++ ": "
      grade <- getLine

      addGrade studentRegistration (read grade) (Disciplina.code subject)
      addStudentsGrades (tail enrolledStudents) subject
    else putStrLn "O aluno não está matriculado!"

addGrade :: Int -> Double -> Int -> IO ()
addGrade studentRegistration grade subjectCode = do
  subjectsFile <- DataLoader.readArq "./data/disciplinas.csv"
  let subjects = DataLoader.loadSubjects subjectsFile
  let subject = DataLoader.loadSubject subjectCode subjects

  let newGrades = replaceGrade studentRegistration grade (Disciplina.grades subject)
  let newSubject = Disciplina.newSubject (Disciplina.code subject) (Disciplina.name subject) (Disciplina.numberClasses subject) newGrades
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

showProfessorSubjects' :: Int -> [Disciplina] -> String
showProfessorSubjects' codigoDisciplina disciplinas = do
  let disciplina = DataLoader.loadSubject codigoDisciplina disciplinas
  Disciplina.showSubject disciplina ++ "\n"

showProfessorSubjects :: [Int] -> [Disciplina] -> String
showProfessorSubjects _ [] = ""
showProfessorSubjects [] _ = ""
showProfessorSubjects (c : cs) (d : ds) =
  if c == Disciplina.code d
    then showProfessorSubjects' c (d : ds) ++ showProfessorSubjects cs ds
    else showProfessorSubjects (c : cs) ds

registerClass :: Professor -> Int -> IO ()
registerClass professor subjectCode =
  if Professor.hasSubject professor subjectCode
    then do
      subjectsFile <- DataLoader.readArq "./data/disciplinas.csv"
      let subjects = DataLoader.loadSubjects subjectsFile
      let subject = DataLoader.loadSubject subjectCode subjects

      if Disciplina.numberClasses subject > 0
        then do
          let newSubject = Disciplina.newSubject subjectCode (Disciplina.name subject) (Disciplina.numberClasses subject - 1) (Disciplina.grades subject)
          DataSaver.updateSubject subjectCode newSubject
        else putStrLn "A disciplina encontra-se encerrada!"
    else putStrLn "Disciplina inválida"

adminScreen :: IO ()
adminScreen = do
  putStr (adminOptions ++ "> ")
  option <- getLine
  adminPanel option

adminOptions :: String
adminOptions =
  header 0 "admin"
    ++ "\n\n1) Cadastrar professor\n"
    ++ "2) Cadastrar aluno\n"
    ++ "3) Cadastrar disciplina\n"
    ++ "4) Associar professor à disciplina\n"
    ++ "5) Listar alunos sem matrículas\n"
    ++ "6) Listar professores sem disciplinas\n"
    ++ "7) Disciplina com a maior média\n"
    ++ "8) Disciplina com a menor média\n"
    ++ "(S)air do sistema\n"
    ++ "Fazer (l)ogoff\n"

adminPanel :: String -> IO ()
adminPanel option
  | option == "1" = registrationScreen "professor"
  | option == "2" = registrationScreen "aluno"
  | option == "3" = createSubjectScreen
  | option == "4" = associateTeacherScreen
  | option == "5" = listStudentsWithoutEnrollment
  | option == "6" = listProfessorWithoutEnrollment
  | option == "7" = showsSubjectHigherAverage
  | option == "8" = showsSubjectLowestAverage
  | option == "S" = quit "" ""
  | otherwise = putStrLn "opcao invalida"

createSubjectScreen :: IO ()
createSubjectScreen = do
  subjectsFile <- DataLoader.readArq "./data/disciplinas.csv"
  let subjects = DataLoader.loadSubjects subjectsFile
  let subjectCodes = map Disciplina.code subjects

  putStr "\nDigite o código da disciplina: \n> "
  subjectCode <- getLine

  putStr "Digite o nome da disciplina: \n> "
  subjectName <- getLine

  putStr "Digite o número de aulas: \n> "
  numberClasses <- getLine

  if read subjectCode `elem` subjectCodes
    then putStrLn "Disciplina já cadastrada!"
    else do
      let newSubject = Disciplina.newSubject (read subjectCode) subjectName (read numberClasses) []
      DataSaver.saveSubject newSubject
      putStrLn "Disciplina cadastrada com sucesso!"

  waitEnterAdmin

registrationScreen :: String -> IO ()
registrationScreen option = do
  putStr "\nDigite a matrícula: \n> "
  id <- getLine

  putStr "Digite seu nome: \n> "
  name <- getLine

  putStr "Digite sua senha: \n> "
  password <- getLine

  if option == "professor"
    then Controle.registerProfessor (read id) name password
    else Controle.registerStudent (read id) name password

  waitEnterAdmin

associateTeacherScreen :: IO ()
associateTeacherScreen = do
  clearScreen

  professorsFile <- DataLoader.readArq "./data/professores.csv"
  subjectsFile <- DataLoader.readArq "./data/disciplinas.csv"
  let professors = DataLoader.loadProfessors professorsFile
  let subjects = DataLoader.loadSubjects subjectsFile

  putStrLn "Professores disponíveis:"
  putStr $ Controle.listAvailableProfessors professors

  putStr "Matrícula do professor a ser associado > "
  id <- getLine
  clearScreen

  let professor = DataLoader.loadProfessor (read id) professors
  if Professor.name professor /= "not found"
    then do
      putStrLn "Disciplinas disponíveis"
      putStr $ Controle.listSubjectsAvailableForAssociation professor subjects

      putStr "Código da disciplina a ser associada > "
      subjectCode <- getLine
      clearScreen

      let subject = DataLoader.loadSubject (read subjectCode) subjects
      associateProfessor professor subject subjects
    else putStrLn "Professor inválido"
  waitEnterAdmin

associateProfessor :: Professor -> Disciplina -> [Disciplina] -> IO ()
associateProfessor professor subject subjects =
  if Disciplina.name subject /= "not found"
    then Controle.associateProfessorSubject professor subject subjects
    else putStrLn "Disciplina inválida"

listStudentsWithoutEnrollment :: IO ()
listStudentsWithoutEnrollment = do
  showData "Alunos sem matrículas:" "./data/alunos.csv" Controle.listStudentsWithoutRegistration DataLoader.loadStudents
  waitEnterAdmin

listProfessorWithoutEnrollment :: IO ()
listProfessorWithoutEnrollment = do
  showData "Professores sem disciplinas:" "./data/professores.csv" Controle.listProfessorsWithoutRegistration DataLoader.loadProfessors
  waitEnterAdmin

showsSubjectHigherAverage :: IO ()
showsSubjectHigherAverage = do
  showData "Disciplina com maior média:" "./data/disciplinas.csv" Controle.showsSubjectWithHigherAverage DataLoader.loadSubjects
  waitEnterAdmin

showsSubjectLowestAverage :: IO ()
showsSubjectLowestAverage = do
  showData "Disciplina com menor média:" "./data/disciplinas.csv" Controle.showsSubjectWithLowestAverage DataLoader.loadSubjects
  waitEnterAdmin

showData :: String -> String -> ([t] -> String) -> ([String] -> [t]) -> IO ()
showData message filePath display loadAll = do
  clearScreen
  putStrLn message
  entityFile <- DataLoader.readArq filePath
  let entities = loadAll entityFile

  putStrLn $ display entities

quit :: String -> String -> IO ()
quit id' role' = do
  putStr "Deseja sair do sistema? (s/n) "
  option <- getLine
  if option == "s"
    then do
      putStr "\nSaindo..."
      threadDelay (10 ^ 6)
    else
      if option == "n"
        then do
          clearScreen
          screen id' role'
        else do
          putStrLn "Opção inválida"
          putStrLn "Pressione enter para continuar..."
          x <- getLine
          clearScreen
          quit id' role'

logout :: String -> String -> IO ()
logout id' role' = do
  putStr "Deseja realizar o logout? (s/n) "
  option <- getLine
  if option == "s"
    then do
      putStrLn "logout realizado."
      clearScreen
      main
    else
      if option == "n"
        then do
          clearScreen
          screen id' role'
        else do
          putStrLn "Opção inválida"
          putStrLn "Pressione enter para continuar..."
          x <- getLine
          clearScreen
          logout id' role'

waitEnterAdmin :: IO ()
waitEnterAdmin = do
  putStr "Pressione enter para continuar..."
  x <- getLine
  clearScreen
  adminScreen
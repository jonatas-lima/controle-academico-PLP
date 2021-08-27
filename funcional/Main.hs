module Main where

import Aluno (Aluno)
import qualified Aluno
import Control.Concurrent (threadDelay)
import qualified Controle
import qualified DataLoader
import qualified DataSaver
import Disciplina (Disciplina)
import qualified Disciplina
import Professor (Professor)
import qualified Professor
import System.Console.ANSI (clearScreen)
import Text.Printf ( printf )
import Data.List (sort, delete)
import qualified Usuario

main :: IO ()
main = do
  putStrLn ("Bem-Vindo(a)!" ++ "\nPara acessar o controle, faça login:\n")
  loginScreen

loginScreen :: IO ()
loginScreen = do
  putStr "Digite sua matrícula: "
  idUser <- getLine

  putStr "Digite sua senha: "
  password <- getLine

  arqUsers <- DataLoader.readArq "./data/usuarios.csv"
  let availableUsers = DataLoader.loadUsers arqUsers

  let authentication = Usuario.authenticates idUser password availableUsers
  let authenticated = fst authentication
  let role = snd authentication

  if authenticated
    then do
      putStrLn "\nLogin realizado..."
      threadDelay (10 ^ 6)
      clearScreen
      screen idUser role
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
  arqStudents <- DataLoader.readArq "./data/alunos.csv"
  let students = DataLoader.loadStudents arqStudents
  let student = DataLoader.loadStudent (read id') students

  arqSubjects <- DataLoader.readArq "./data/disciplinas.csv"
  let subjects = DataLoader.loadSubjects arqSubjects

  -- variaveis auxiliares
  let codesRegistered = Aluno.enrolledSubjects student
  let subjectsRegistered = subjectsFilter subjects codesRegistered
  let codesNotRegistered = codesFilter (codesAllSubjects subjects) codesRegistered
  let discNaoMatriculadas = subjectsFilter subjects codesNotRegistered

  putStrLn "\n\n--- Controle Acadêmico ---"

  putStrLn (studentOptions student)

  putStr "Qual a opcao selecionada?\n> "
  option <- getLine
  putStrLn ""

  if option == "1"
    then do
      putStrLn ("Código\t - Disciplina\t - Média\n" ++ showStudentSubjects student codesRegistered subjects)
    else
      if option == "2"
        then checksEnrol student discNaoMatriculadas codesNotRegistered codesRegistered
        else
          if option == "3"
            then cancelRegistration student subjectsRegistered codesRegistered
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

codesAllSubjects :: [Disciplina] -> [Int]
codesAllSubjects subjects = [Disciplina.code subject | subject <- subjects]

codesFilter :: [Int] -> [Int] -> [Int]
codesFilter codesSubjects codesStudent = filter (\cod -> not $ cod `elem` codesStudent) codesSubjects

subjectsFilter :: [Disciplina] -> [Int] -> [Disciplina]
subjectsFilter subjects codes = filter (\cod -> Disciplina.code cod `elem` codes) subjects

studentOptions :: Aluno -> String
studentOptions student =
  header (Aluno.registration student) (Aluno.name student) ++ Aluno.availableOptions

showStudentSubjects' :: Aluno -> Int -> [Disciplina] -> String
showStudentSubjects' student codeSubject subjects = do
  let subject = DataLoader.loadSubject codeSubject subjects
  Disciplina.exibeDisciplina subject ++ "\t - " ++ printf "%.2f" (Aluno.subjectAverage student subject) ++ "\n"

showStudentSubjects :: Aluno -> [Int] -> [Disciplina] -> String
showStudentSubjects student _ [] = ""
showStudentSubjects student [] _ = ""
showStudentSubjects student (c : cs) (d : ds) =
  if c == Disciplina.code d
    then showStudentSubjects' student c (d : ds) ++ showStudentSubjects student cs ds
    else showStudentSubjects student (c : cs) ds

-- verificar se o aluno pode realizar matricula
checksEnrol :: Aluno -> [Disciplina] -> [Int] -> [Int] -> IO ()
checksEnrol student subjects codesSubjects codesStudents = do
  if Aluno.numberEnrolledSubjects student == 4
    then putStrLn ("O aluno [" ++ printf "%.d" (Aluno.registration student) ++ "] já possui 4 disciplinas matriculadas!\n")
    else enrol student subjects codesSubjects codesStudents

enrol :: Aluno -> [Disciplina] -> [Int] -> [Int] -> IO ()
enrol student subjects codesSubjects codesStudent = do
  putStrLn ("Código\t - Disciplina\n" ++ showSubjects subjects)

  putStr "Entre com o código da cadeira: "

  code <- getLine

  putStrLn ""

  let codeInt = read code :: Int
  let subject = DataLoader.loadSubject codeInt subjects
  -- verificar codigo da cadeira --
  if codeInt `elem` codesSubjects
    then do
      let newCods = sort (codeInt : codesStudent)

      -- variaveis aluno
      let studentId = Aluno.registration student
      let studentName = Aluno.name student

      -- variaveis disciplina
      let subjectName = Disciplina.name subject
      let numberClassesSubject = Disciplina.numberClasses subject
      let newGrades = (studentId, []) : Disciplina.grades subject

      let newStudent = Aluno.newStudent studentId studentName newCods
      let newSubject = Disciplina.newSubject codeInt subjectName numberClassesSubject newGrades

      -- putStrLn $ Disciplina.toString newDisciplina

      DataSaver.updateStudent studentId newStudent
      DataSaver.updateSubject codeInt newSubject

      putStrLn "Matricula realizada com sucesso!\n" -- matricular ou cancelar matricula do aluno na cadeira
    else putStrLn "Código Inválido\n"

cancelRegistration :: Aluno -> [Disciplina] -> [Int] -> IO ()
cancelRegistration student subjects codesStudent = do
  putStrLn ("Código\t - Disciplina\n" ++ showSubjects subjects)

  putStr "Entre com o código da cadeira: "

  code <- getLine

  putStrLn ""

  let codeInt = read code :: Int

  -- verificar codigo da cadeira --
  if codeInt `elem` codesStudent
    then do
      let newCodes = delete codeInt codesStudent
      let studentId = Aluno.registration student
      let studentName = Aluno.name student

      let newStudent = Aluno.newStudent studentId studentName newCodes
      
      DataSaver.updateStudent studentId newStudent

      putStrLn "Matricula cancelada...\n" -- matricular ou cancelar matricula do aluno na cadeira
    else putStrLn "Código Inválido\n"

showSubjects :: [Disciplina] -> String
showSubjects [] = ""
showSubjects (d : ds) =
  Disciplina.exibeDisciplina d ++ "\n" ++ showSubjects ds

professorScreen :: String -> IO ()
professorScreen id' = do
  arqProfessors <- DataLoader.readArq "./data/professores.csv"
  let professors = DataLoader.loadProfessors arqProfessors
  let professor = DataLoader.loadProfessor (read id') professors

  arqSubjects <- DataLoader.readArq "./data/disciplinas.csv"
  let disciplinas = DataLoader.loadSubjects arqSubjects
  let codesProfessorSubjects = Professor.subjects professor
  let professorSubjects = subjectsFilter disciplinas codesProfessorSubjects

  putStrLn (professorOptions professor)

  putStr "Qual a opcao selecionada? "
  option <- getLine

  if option == "1"
    then putStrLn ("\nCódigo\t - Disciplina\n" ++ showSubjects professorSubjects)
    else
      if option == "2"
        then do
          putStrLn "Essas são as disciplinas que você leciona:"
          putStrLn ("\nCódigo\t - Disciplina\n" ++ showSubjects professorSubjects)
          putStr "Código da disciplina para qual você deseja cadastrar aula: "
          code <- getLine
          registerClass professor $ read code
        else
          if option == "3"
            then putStrLn "Cadastra prova"
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

showProfessorSubjects' :: Int -> [Disciplina] -> String
showProfessorSubjects' codigoDisciplina disciplinas = do
  let disciplina = DataLoader.loadSubject codigoDisciplina disciplinas
  Disciplina.exibeDisciplina disciplina ++ "\n"

showProfessorSubjects :: [Int] -> [Disciplina] -> String
showProfessorSubjects _ [] = ""
showProfessorSubjects [] _ = ""
showProfessorSubjects (c : cs) (d : ds) =
  if c == Disciplina.code d
    then showProfessorSubjects' c (d : ds) ++ showProfessorSubjects cs ds
    else showProfessorSubjects (c : cs) ds

registerClass :: Professor -> Int -> IO ()
registerClass professor codeSubject =
  if Professor.hasSubject professor codeSubject
    then putStrLn "Registrado"
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
    ++ "3) Associar professor à disciplina\n"
    ++ "4) Listar alunos sem matrículas\n"
    ++ "5) Listar professores sem disciplinas\n"
    ++ "6) Disciplina com a maior média\n"
    ++ "7) Disciplina com a menor média\n"
    ++ "(S)air do sistema\n"
    ++ "Fazer (l)ogoff\n"

adminPanel :: String -> IO ()
adminPanel option
  | option == "1" = registrationScreen "professor"
  | option == "2" = registrationScreen "aluno"
  | option == "3" = associateProfessorScreen
  | option == "4" = listStudentsWithoutEnrollment
  | option == "5" = listProfessorWithoutEnrollment
  | option == "6" = showsSubjectHigherAverage
  | option == "7" = showsSubjectLowestAverage
  | option == "S" = quit "" ""
  | otherwise = putStrLn "opcao invalida"

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

associateProfessorScreen :: IO ()
associateProfessorScreen = do
  clearScreen

  arqProfessors <- DataLoader.readArq "./data/professores.csv"
  arqSubjects <- DataLoader.readArq "./data/disciplinas.csv"
  let professors = DataLoader.loadProfessors arqProfessors
  let subjects = DataLoader.loadSubjects arqSubjects

  putStrLn "Professores disponíveis:"
  putStr $ Controle.listAvailableProfessors professors

  putStr "Matrícula do professor a ser associado > "
  id <- getLine
  clearScreen

  let professor = DataLoader.loadProfessor (read id) professors
  putStrLn "Disciplinas disponíveis"
  putStr $ Controle.listSubjectsAvailableForAssociation professor subjects

  putStr "Código da disciplina a ser associada > "
  code <- getLine
  clearScreen

  let subject = DataLoader.loadSubject (read code) subjects

  Controle.associateProfessorSubject professor subject subjects

listStudentsWithoutEnrollment :: IO ()
listStudentsWithoutEnrollment = do
  showData "Alunos sem matrículas:" "./data/alunos.csv" Controle.listStudentsWithoutRegistration DataLoader.loadStudents

listProfessorWithoutEnrollment :: IO ()
listProfessorWithoutEnrollment = do
  showData "Professores sem disciplinas:" "./data/professores.csv" Controle.listProfessorsWithoutRegistration DataLoader.loadProfessors

showsSubjectHigherAverage :: IO ()
showsSubjectHigherAverage = do
  showData "Disciplina com maior média:" "./data/disciplinas.csv" Controle.showsSubjectWithHigherAverage DataLoader.loadSubjects

showsSubjectLowestAverage :: IO ()
showsSubjectLowestAverage = do
  showData "Disciplina com menor média:" "./data/disciplinas.csv" Controle.showsSubjectWithLowestAverage DataLoader.loadSubjects

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
module Main where

import Aluno (Aluno)
import qualified Aluno
import Control.Concurrent (threadDelay)
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
import qualified UI

main :: IO ()
main = do
  clearScreen
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
      screen (read userId) role
    else do
      putStr "\nUsuario ou senha invalido! Deseja tentar novamente? (s/n) "
      option <- getLine
      if option == "s" || option == "S"
        then do
          clearScreen
          loginScreen
        else
          if option == "n" || option == "N"
            then do
              putStr "\nSaindo do sistema..."
              threadDelay (10 ^ 6)
            else do
              putStr "\nOpção inválida. Saindo do sistema por segurança."
              threadDelay (10 ^ 6)

screen :: Int -> String -> IO ()
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

studentScreen :: Int -> IO ()
studentScreen id = do
  studentsFile <- DataLoader.readArq "./data/alunos.csv"
  let students = DataLoader.loadStudents studentsFile
  let student = DataLoader.loadStudent id students

  putStr (studentOptions id (Aluno.name student) ++ "> ")
  option <- getLine
  studentPanel id option

studentOptions :: Int -> String -> String
studentOptions id name =
  header id name ++ 
    "\n\n1) Visualizar disciplinas\n"
    ++ "2) Realizar matrícula\n"
    ++ "3) Cancelar matrícula\n"
    ++ "4) Visualizar média geral\n"
    ++ "(S)air do sistema\n"

studentPanel :: Int -> String -> IO ()
studentPanel id option
  | option == "1" = do 
    UI.showStudentSubjectsScreen id
    waitUserResponse id studentScreen
  | option == "2" = do
     UI.enrollSubjectScreen id
     waitUserResponse id studentScreen
  | option == "3" = do 
    UI.cancelEnrollmentScreen id
    waitUserResponse id studentScreen
  | option == "4" = do
    UI.totalAverage id
    waitUserResponse id studentScreen
  | option == "S" || option == "s" = do 
    quit
  | otherwise = do 
    putStrLn "\nOpção inválida! Tente novamente.\n"
    waitUserResponse id studentScreen

professorScreen :: Int -> IO ()
professorScreen id = do
  professorsFile <- DataLoader.readArq "./data/professores.csv"
  let professors = DataLoader.loadProfessors professorsFile
  let professor = DataLoader.loadProfessor id professors

  putStr (professorOptions id (Professor.name professor) ++ "> ")
  option <- getLine
  professorPanel id option

professorOptions :: Int -> String -> String
professorOptions id name =
  header id name
    ++ "\n\n1) Visualizar disciplinas\n"
    ++ "2) Registrar aula\n"
    ++ "3) Cadastrar prova\n"
    ++ "4) Situação da classe\n"
    ++ "(S)air do sistema\n"

professorPanel :: Int -> String -> IO ()
professorPanel id option
  | option == "1" = do
    UI.showProfessorSubjects id
    waitUserResponse id professorScreen
  | option == "2" = do
    UI.classRegistrationScreen id
    waitUserResponse id professorScreen
  | option == "3" = do 
    UI.registerTestScreen id
    waitUserResponse id professorScreen
  | option == "4" = do 
    UI.classSituationScreen id
    waitUserResponse id professorScreen
  | option == "S" || option == "s" = do
    quit
  | otherwise = do
    putStrLn "\nOpção inválida! Tente novamente.\n"
    waitUserResponse id professorScreen

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

adminPanel :: String -> IO ()
adminPanel option
  | option == "1" = do 
    UI.registrationScreen "professor"
    waitEnterAdmin
  | option == "2" = do
    UI.registrationScreen "aluno"
    waitEnterAdmin
  | option == "3" = do
    UI.createSubjectScreen
    waitEnterAdmin
  | option == "4" = do
    UI.associateTeacherScreen
    waitEnterAdmin
  | option == "5" = do
    UI.listStudentsWithoutEnrollment
    waitEnterAdmin
  | option == "6" = do 
    UI.listProfessorWithoutEnrollment
    waitEnterAdmin
  | option == "7" = do 
    UI.showsSubjectHigherAverage
    waitEnterAdmin
  | option == "8" = do 
    UI.showsSubjectLowestAverage
    waitEnterAdmin
  | option == "S" = quit
  | otherwise = do 
    putStrLn "opcao invalida"
    waitEnterAdmin

quit :: IO ()
quit = putStrLn "\nAté a próxima!"

waitUserResponse :: Int -> (Int -> IO()) -> IO()
waitUserResponse id screen = do
  putStr "Pressione enter para continuar..."
  x <- getLine
  clearScreen
  screen id

waitEnterAdmin :: IO ()
waitEnterAdmin = do
  putStr "Pressione enter para continuar..."
  x <- getLine
  clearScreen
  adminScreen
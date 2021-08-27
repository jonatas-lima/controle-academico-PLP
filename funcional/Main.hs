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

  usersFile <- DataLoader.leArquivo "./data/usuarios.csv"
  let availableUsers = DataLoader.carregaUsuarios usersFile

  let authentication = Usuario.autentica userId password availableUsers
  let authenticated = fst authentication
  let role = snd authentication

  if authenticated
    then do
      putStrLn "\nLogin realizado..."
      threadDelay (10 ^ 6)
      clearScreen
      mainScreen userId role
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

mainScreen :: String -> String -> IO ()
mainScreen id role
  | role == "prof" = teacherScreen id
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
  studentsFile <- DataLoader.leArquivo "./data/alunos.csv"
  let students = DataLoader.carregaAlunos studentsFile
  let student = DataLoader.carregaAluno (read id') students

  subjectsFile <- DataLoader.leArquivo "./data/disciplinas.csv"
  let subjects = DataLoader.carregaDisciplinas subjectsFile

  -- variaveis auxiliares
  let enrolledSubjectsCodes = Aluno.disciplinasMatriculadas student
  let enrolledSubjects = subjectsFilter subjects enrolledSubjectsCodes
  let notEnrolledSubjectsCodes = codesFilter (allSubjectsCode subjects) enrolledSubjectsCodes
  let notEnrolledSubjects = subjectsFilter subjects notEnrolledSubjectsCodes

  putStrLn "\n\n--- Controle Acadêmico ---"

  putStrLn (studentOptions student)

  putStr "Qual a opcao selecionada?\n> "
  option <- getLine
  putStrLn ""

  if option == "1"
    then putStrLn ("Código\t - Disciplina\t - Média\n" ++ showStudentSubjects student subjects)
    else
      if option == "2"
        then do
          if checkStudentEnrollment student then enroll student subjects else putStrLn "Impossivel matricular"
        else
          if option == "3"
            then cancelRegistration student enrolledSubjects enrolledSubjectsCodes
            else
              if option == "4"
                then do
                  putStr "CRA: "
                  printf "%.2f" (Aluno.mediaTotal student subjects)
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

-- studentPanel :: String -> String -> Aluno -> [Disciplina] -> [Disciplina] -> IO ()
-- studentPanel option id' student notEnrolledSubjects
--   | option == "1" = do
--     putStrLn ("Código\t - Disciplina\t - Média\n" ++ showStudentSubjects student enrolledSubjectsCodes subjects)
--   | option == "2" = do
--     checkStudentEnrollment student notEnrolledSubjects notEnrolledSubjectsCodes enrolledSubjectsCodes
--   | option == "3" = do
--     cancelRegistration student enrolledSubjects enrolledSubjectsCodes
--   | option == "4" = do
--     putStr "CRA: "
--     printf "%.2f" (Aluno.mediaTotal student subjects)
--     putStrLn "\n"
--   | option == "5" = do
--     clearScreen
--     quit id' "aluno"
--   | option == "6" = do
--     clearScreen
--     logout id' "aluno"
--   | otherwise = putStrLn "Opção inválida"

allSubjectsCode :: [Disciplina] -> [Int]
allSubjectsCode subjects = [Disciplina.codigo subject | subject <- subjects]

codesFilter :: [Int] -> [Int] -> [Int]
codesFilter subjectCodes studentCode = filter (`notElem` studentCode) subjectCodes

subjectsFilter :: [Disciplina] -> [Int] -> [Disciplina]
subjectsFilter subjects subjectCodes = filter (\cod -> Disciplina.codigo cod `elem` subjectCodes) subjects

studentOptions :: Aluno -> String
studentOptions student =
  header (Aluno.matricula student) (Aluno.nome student) ++ Aluno.opcoesDisponiveis

showSubjects' :: Aluno -> [Disciplina] -> String
showSubjects' _ [] = ""
showSubjects' student (s : sa) =
  Disciplina.exibeDisciplina s ++ "\t - " ++ printf "%.2f" (Aluno.mediaDisciplina student s) ++ "\n" ++ showSubjects' student sa

showStudentSubjects :: Aluno -> [Disciplina] -> String
showStudentSubjects student subjects =
  showSubjects' student enrolledSubjects
  where
    enrolledSubjects = DataLoader.carregaDisciplinasPorCodigo (Aluno.disciplinasMatriculadas student) subjects

-- verificar se o aluno pode realizar matricula
checkStudentEnrollment :: Aluno -> Bool
checkStudentEnrollment student = Aluno.numDisciplinasMatriculadas student < 4

enroll :: Aluno -> [Disciplina] -> IO ()
enroll student subjects = do
  putStrLn ("Código\t - Disciplina\n" ++ showSubjects subjects)

  putStr "Entre com o código da cadeira: "

  code <- getLine

  putStrLn ""

  let subjectCodes = map Disciplina.codigo subjects
  let subjectCode = read code :: Int
  let subject = DataLoader.carregaDisciplina subjectCode subjects
  -- verificar codigo da cadeira --
  if subjectCode `elem` subjectCodes
    then do
      let newCods = sort (subjectCode : studentCode)

      -- variaveis aluno
      let studentId = Aluno.matricula student
      let studentName = Aluno.nome student

      -- variaveis disciplina
      let subjectName = Disciplina.nome subject
      let numberClassesSubject = Disciplina.qtdDeAulas subject
      let newGrades = (studentId, []) : Disciplina.notas subject

      let newStudent = Aluno.newAluno studentId studentName newCods
      let newSubject = Disciplina.newDisciplina subjectCode subjectName numberClassesSubject newGrades

      -- putStrLn $ Disciplina.toString newDisciplina

      DataSaver.atualizaAluno studentId newStudent
      DataSaver.atualizaDisciplina subjectCode newSubject

      putStrLn "Matricula realizada com sucesso!\n" -- matricular ou cancelar matricula do aluno na cadeira
    else putStrLn "Código Inválido\n"

studentCode :: [Int]
studentCode = error "not implemented"

cancelRegistration :: Aluno -> [Disciplina] -> [Int] -> IO ()
cancelRegistration student subjects studentCode = do
  putStrLn ("Código\t - Disciplina\n" ++ showSubjects subjects)

  putStr "Entre com o código da cadeira: "

  code <- getLine

  putStrLn ""

  let subjectCode = read code :: Int

  -- verificar codigo da cadeira --
  if subjectCode `elem` studentCode
    then do
      let newCodes = delete subjectCode studentCode
      let studentId = Aluno.matricula student
      let studentName = Aluno.nome student

      let newStudent = Aluno.newAluno studentId studentName newCodes

      DataSaver.atualizaAluno studentId newStudent

      putStrLn "Matricula cancelada...\n" -- matricular ou cancelar matricula do aluno na cadeira
    else putStrLn "Código Inválido\n"

showSubjects :: [Disciplina] -> String
showSubjects [] = ""
showSubjects (d : ds) =
  Disciplina.exibeDisciplina d ++ "\n" ++ showSubjects ds

teacherScreen :: String -> IO ()
teacherScreen id' = do
  teachersFile <- DataLoader.leArquivo "./data/professores.csv"
  let teachers = DataLoader.carregaProfessores teachersFile
  let teacher = DataLoader.carregaProfessor (read id') teachers

  subjectsFile <- DataLoader.leArquivo "./data/disciplinas.csv"
  let disciplinas = DataLoader.carregaDisciplinas subjectsFile
  let teacherSubjectsCode = Professor.disciplinasLecionadas teacher
  let teacherSubjects = subjectsFilter disciplinas teacherSubjectsCode

  putStrLn (teacherOptions teacher)

  putStr "Qual a opcao selecionada? "
  option <- getLine

  teacherPanel option id' teacher teacherSubjects

  if option /= "4" && option /= "5"
    then waitResponse id' teacherScreen
    else putStrLn ""

teacherPanel :: String -> String -> Professor -> [Disciplina] -> IO ()
teacherPanel option id' teacher teacherSubjects
  | option == "1" =
    putStrLn ("\nCódigo\t - Disciplina\n" ++ showSubjects teacherSubjects)
  | option == "2" = do
    putStrLn "Essas são as disciplinas que você leciona:"
    putStrLn ("\nCódigo\t - Disciplina\n" ++ showSubjects teacherSubjects)
    putStr "Código da disciplina para qual você deseja cadastrar aula: "
    code <- getLine
    registerClass teacher $ read code
  | option == "3" =
    putStrLn "Cadastra prova"
  | option == "4" = do
    clearScreen
    quit id' "prof"
  | option == "5" = do
    clearScreen
    logout id' "prof"
  | otherwise =
    putStrLn "Opção inválida"

teacherOptions :: Professor -> String
teacherOptions professor =
  header (Professor.matricula professor) (Professor.nome professor) ++ Professor.opcoesDisponiveis

showTeacherSubjects' :: Int -> [Disciplina] -> String
showTeacherSubjects' codigoDisciplina disciplinas = do
  let disciplina = DataLoader.carregaDisciplina codigoDisciplina disciplinas
  Disciplina.exibeDisciplina disciplina ++ "\n"

showTeacherSubjects :: [Int] -> [Disciplina] -> String
showTeacherSubjects _ [] = ""
showTeacherSubjects [] _ = ""
showTeacherSubjects (c : cs) (d : ds) =
  if c == Disciplina.codigo d
    then showTeacherSubjects' c (d : ds) ++ showTeacherSubjects cs ds
    else showTeacherSubjects (c : cs) ds

registerClass :: Professor -> Int -> IO ()
registerClass professor codeSubject =
  if Professor.temDisciplina professor codeSubject
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
  | option == "3" = associateTeacherScreen
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
    then Controle.cadastraProfessor (read id) name password
    else Controle.cadastraAluno (read id) name password

associateTeacherScreen :: IO ()
associateTeacherScreen = do
  clearScreen

  teachersFile <- DataLoader.leArquivo "./data/professores.csv"
  subjectsFile <- DataLoader.leArquivo "./data/disciplinas.csv"
  let teachers = DataLoader.carregaProfessores teachersFile
  let subjects = DataLoader.carregaDisciplinas subjectsFile

  putStrLn "Professores disponíveis:"
  putStr $ Controle.listaProfessoresDisponiveis teachers

  putStr "Matrícula do professor a ser associado > "
  id <- getLine
  clearScreen

  let teacher = DataLoader.carregaProfessor (read id) teachers
  putStrLn "Disciplinas disponíveis"
  putStr $ Controle.listaDisciplinasDisponiveisParaAssociacao teacher subjects

  putStr "Código da disciplina a ser associada > "
  subjectCode <- getLine
  clearScreen

  let subject = DataLoader.carregaDisciplina (read subjectCode) subjects

  Controle.associaProfessorDisciplina teacher subject subjects

listStudentsWithoutEnrollment :: IO ()
listStudentsWithoutEnrollment =
  showData "Alunos sem matrículas:" "./data/alunos.csv" Controle.listaAlunosSemMatriculas DataLoader.carregaAlunos

listProfessorWithoutEnrollment :: IO ()
listProfessorWithoutEnrollment =
  showData "Professores sem disciplinas:" "./data/professores.csv" Controle.listaProfessoresSemMatriculas DataLoader.carregaProfessores

showsSubjectHigherAverage :: IO ()
showsSubjectHigherAverage =
  showData "Disciplina com maior média:" "./data/disciplinas.csv" Controle.exibeDisciplinaComMaiorMedia DataLoader.carregaDisciplinas

showsSubjectLowestAverage :: IO ()
showsSubjectLowestAverage =
  showData "Disciplina com menor média:" "./data/disciplinas.csv" Controle.exibeDisciplinaComMenorMedia DataLoader.carregaDisciplinas

showData :: String -> String -> ([t] -> String) -> ([String] -> [t]) -> IO ()
showData message filePath display loadAll = do
  clearScreen
  putStrLn message
  entityFile <- DataLoader.leArquivo filePath
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
          mainScreen id' role'
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
          mainScreen id' role'
        else do
          putStrLn "Opção inválida"
          putStrLn "Pressione enter para continuar..."
          x <- getLine
          clearScreen
          logout id' role'

waitResponse :: String -> (String -> IO ()) -> IO ()
waitResponse id' screen = do
  putStr "Pressione enter para continuar..."
  x <- getLine
  clearScreen
  screen id'
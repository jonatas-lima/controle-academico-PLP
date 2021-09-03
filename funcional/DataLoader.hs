module DataLoader where

import Student (Student (..), notFound)
import Data.List.Split (splitOn)
import Subject (Subject (..), notFound)
import Professor (Professor (..), notFound)
import qualified System.IO.Strict as Strict
import User (User (..))

-- / Lê um arquivo, sendo passado o 'path' no parâmetro
readArq :: String -> IO [String]
readArq path = do
  arq <- Strict.readFile path
  let list = lines arq
  return list

-- / Carrega todos os usuários, passando as linhas do arquivo como parâmetro
loadUsers :: [String] -> [User]
loadUsers lines = [parseUser line | line <- lines]

-- / Faz o parsing de uma linha do arquivo para um User
parseUser :: String -> User
parseUser line =
  User
    { User.name = head data',
      User.password = data' !! 1,
      User.role = data' !! 2
    }
  where
    data' = splitOn "," line

-- / Carrega todos os usuários, passando as linhas do arquivo como parâmetro
loadProfessors :: [String] -> [Professor]
loadProfessors lines = [parseProfessor line | line <- lines]

-- / Carrega um único professor, através da matrícula, passando uma lista de professores como parâmetro
loadProfessor :: Int -> [Professor] -> Professor
loadProfessor _ [] = Professor.notFound
loadProfessor id (p : ps) =
  if Professor.registration p == id
    then p
    else loadProfessor id ps

-- / Faz o parsing de uma linha do arquivo para um Professor
parseProfessor :: String -> Professor
parseProfessor line =
  Professor
    { Professor.registration = read (head data') :: Int,
      Professor.name = data' !! 1,
      Professor.subjects = read (data' !! 2) :: [Int]
    }
  where
    data' = splitOn ";" line

-- / Carrega todos os alunos, passando as linhas do arquivo como parâmetro
loadStudents :: [String] -> [Student]
loadStudents lines = [parseStudent line | line <- lines]

-- / Carrega um único aluno, através da matrícula, passando uma lista de alunos como parâmetro
loadStudent :: Int -> [Student] -> Student
loadStudent _ [] = Student.notFound
loadStudent id' (a : as)
  | Student.registration a == id' = a
  | otherwise = loadStudent id' as

-- / Faz o parsing de uma linha do arquivo para um Student
parseStudent :: String -> Student
parseStudent line =
  Student
    { Student.registration = read (head data') :: Int,
      Student.name = data' !! 1,
      Student.enrolledSubjects = read (data' !! 2) :: [Int]
    }
  where
    data' = splitOn ";" line

-- / Faz o parsing de uma linha do arquivo para uma Subject
parseSubject :: String -> Subject
parseSubject lines =
  Subject
    { Subject.code = read (head data') :: Int,
      Subject.professorRegistration = read (data' !! 1) :: Int,
      Subject.name = data' !! 2,
      Subject.numberClasses = read (data' !! 3) :: Int,
      Subject.studentLimit = read (data' !! 4) :: Int,
      Subject.grades = read (data' !! 5) :: [(Int, [Double])]
    }
  where
    data' = splitOn ";" lines

-- / Carrega todas as disciplinas, passando as linhas do arquivo como parâmetro
loadSubjects :: [String] -> [Subject]
loadSubjects lines = [parseSubject linha | linha <- lines]

-- / Carrega uma única disciplina, através do seu código, passando uma lista de disciplinas como parâmetro
loadSubject :: Int -> [Subject] -> Subject
loadSubject _ [] = Subject.notFound
loadSubject code' (d : ds) =
  if Subject.code d == code'
    then d
    else loadSubject code' ds

-- / Carrega disciplinas a partir do seu código
loadSubjectsByCode :: [Int] -> [Subject] -> [Subject]
loadSubjectsByCode subjectCodes = loadEntityByKey subjectCodes loadSubject Subject.code

-- / Carrega alunos a partir de sua matrícula
loadStudentsByRegistration :: [Int] -> [Student] -> [Student]
loadStudentsByRegistration studentRegistrations = loadEntityByKey studentRegistrations loadStudent Student.registration

-- / Carrega professores a partir de sua matrícula
loadProfessorsByRegistration :: [Int] -> [Professor] -> [Professor]
loadProfessorsByRegistration professorsRegistrations = loadEntityByKey professorsRegistrations loadProfessor Professor.registration

-- / Função genérica que carrega uma lista de entidades a partir de uma chave
loadEntityByKey :: [Int] -> (Int -> [t] -> t) -> (t -> Int) -> [t] -> [t]
loadEntityByKey [] _ _ _ = []
loadEntityByKey (k : ks) loadOne keyMap entities =
  if k `elem` keys
    then loadOne k entities : loadEntityByKey ks loadOne keyMap entities
    else loadEntityByKey ks loadOne keyMap entities
  where
    keys = map keyMap entities
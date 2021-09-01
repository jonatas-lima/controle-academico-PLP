module DataLoader where

import Student (Student (..), notFound)
import Data.List.Split (splitOn)
import Subject (Subject (..), notFound)
import Professor (Professor (..), notFound)
import qualified System.IO.Strict as Strict
import User (User (..))

readArq :: String -> IO [String]
readArq path = do
  arq <- Strict.readFile path
  let list = lines arq
  return list

loadUsers :: [String] -> [User]
loadUsers lines = [parseUser line | line <- lines]

parseUser :: String -> User
parseUser line =
  User
    { User.name = head data',
      User.password = data' !! 1,
      User.role = data' !! 2
    }
  where
    data' = splitOn "," line

loadProfessors :: [String] -> [Professor]
loadProfessors lines = [parseProfessor line | line <- lines]

loadProfessor :: Int -> [Professor] -> Professor
loadProfessor _ [] = Professor.notFound
loadProfessor id (p : ps) =
  if Professor.registration p == id
    then p
    else loadProfessor id ps

parseProfessor :: String -> Professor
parseProfessor line =
  Professor
    { Professor.registration = read (head data') :: Int,
      Professor.name = data' !! 1,
      Professor.subjects = read (data' !! 2) :: [Int]
    }
  where
    data' = splitOn ";" line

loadStudents :: [String] -> [Student]
loadStudents lines = [parseStudent line | line <- lines]

loadStudent :: Int -> [Student] -> Student
loadStudent _ [] = Student.notFound
loadStudent id' (a : as)
  | Student.registration a == id' = a
  | otherwise = loadStudent id' as

parseStudent :: String -> Student
parseStudent line =
  Student
    { Student.registration = read (head data') :: Int,
      Student.name = data' !! 1,
      Student.enrolledSubjects = read (data' !! 2) :: [Int]
    }
  where
    data' = splitOn ";" line

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

loadSubjects :: [String] -> [Subject]
loadSubjects lines = [parseSubject linha | linha <- lines]

loadSubject :: Int -> [Subject] -> Subject
loadSubject _ [] = Subject.notFound
loadSubject code' (d : ds) =
  if Subject.code d == code'
    then d
    else loadSubject code' ds

loadSubjectsByCode :: [Int] -> [Subject] -> [Subject]
loadSubjectsByCode subjectCodes = loadEntityByKey subjectCodes loadSubject Subject.code

loadStudentsByRegistration :: [Int] -> [Student] -> [Student]
loadStudentsByRegistration studentRegistrations = loadEntityByKey studentRegistrations loadStudent Student.registration

loadProfessorsByRegistration :: [Int] -> [Professor] -> [Professor]
loadProfessorsByRegistration professorsRegistrations = loadEntityByKey professorsRegistrations loadProfessor Professor.registration

loadEntityByKey :: [Int] -> (Int -> [t] -> t) -> (t -> Int) -> [t] -> [t]
loadEntityByKey [] _ _ _ = []
loadEntityByKey (k : ks) loadOne keyMap entities =
  if k `elem` keys
    then loadOne k entities : loadEntityByKey ks loadOne keyMap entities
    else loadEntityByKey ks loadOne keyMap entities
  where
    keys = map keyMap entities
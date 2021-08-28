module DataLoader where

import Aluno (Aluno (..), notFound)
import Data.List.Split (splitOn)
import Disciplina (Disciplina (..), notFound)
import Professor (Professor (..), notFound)
import qualified System.IO.Strict as Strict
import Usuario (Usuario (..))

readArq :: String -> IO [String]
readArq path = do
  arq <- Strict.readFile path
  let list = lines arq
  return list

loadUsers :: [String] -> [Usuario]
loadUsers lines = [parseUser line | line <- lines]

parseUser :: String -> Usuario
parseUser line =
  Usuario
    { Usuario.name = head data',
      Usuario.password = data' !! 1,
      Usuario.role = data' !! 2
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

loadStudents :: [String] -> [Aluno]
loadStudents lines = [parseStudent line | line <- lines]

loadStudent :: Int -> [Aluno] -> Aluno
loadStudent _ [] = Aluno.notFound
loadStudent id' (a : as)
  | Aluno.registration a == id' = a
  | otherwise = loadStudent id' as

parseStudent :: String -> Aluno
parseStudent line =
  Aluno
    { Aluno.registration = read (head data') :: Int,
      Aluno.name = data' !! 1,
      Aluno.enrolledSubjects = read (data' !! 2) :: [Int]
    }
  where
    data' = splitOn ";" line

parseSubject :: String -> Disciplina
parseSubject lines =
  Disciplina
    { Disciplina.code = read (head data') :: Int,
      Disciplina.professorRegistration = read (data' !! 1) :: Int,
      Disciplina.name = data' !! 2,
      Disciplina.numberClasses = read (data' !! 3) :: Int,
      Disciplina.studentLimit = read (data' !! 4) :: Int,
      Disciplina.grades = read (data' !! 5) :: [(Int, [Double])]
    }
  where
    data' = splitOn ";" lines

loadSubjects :: [String] -> [Disciplina]
loadSubjects lines = [parseSubject linha | linha <- lines]

loadSubject :: Int -> [Disciplina] -> Disciplina
loadSubject _ [] = Disciplina.notFound
loadSubject code' (d : ds) =
  if Disciplina.code d == code'
    then d
    else loadSubject code' ds

loadSubjectsByCode :: [Int] -> [Disciplina] -> [Disciplina]
loadSubjectsByCode subjectCodes = loadEntityByKey subjectCodes loadSubject Disciplina.code

loadStudentsByRegistration :: [Int] -> [Aluno] -> [Aluno]
loadStudentsByRegistration studentRegistrations = loadEntityByKey studentRegistrations loadStudent Aluno.registration

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
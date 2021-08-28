module Aluno where

import Disciplina (Disciplina)
import qualified Disciplina

data Aluno = Aluno
  { registration :: Int,
    name :: String,
    enrolledSubjects :: [Int]
  }

notFound :: Aluno
notFound = Aluno 0 "not found" []

newStudent :: Int -> String -> [Int] -> Aluno
newStudent = Aluno

numberEnrolledSubjects :: Aluno -> Int
numberEnrolledSubjects student = length (enrolledSubjects student)

registrations :: [Aluno] -> [Int]
registrations students = [registration student | student <- students]

approved :: Aluno -> Disciplina -> Bool
approved student subject = Disciplina.studentAverage (registration student) subject >= 7

final :: Aluno -> Disciplina -> Bool
final student subject = not (approved student subject) && Disciplina.studentAverage (registration student) subject >= 5

situation :: Aluno -> Disciplina -> String
situation student subject
  | approved student subject = "(APROVADO)"
  | final student subject = "(FINAL)"
  | reproved student subject = "(REPROVADO)"
  | otherwise = "(EM ANDAMENTO)"

reproved :: Aluno -> Disciplina -> Bool
reproved student subject = not (final student subject) && not (approved student subject)

subjectAverage :: Aluno -> Disciplina -> Double
subjectAverage student = Disciplina.studentAverage (registration student)

totalAverage :: Aluno -> [Disciplina] -> Double
totalAverage student subjects =
  sumAllAverage / fromIntegral (length averages)
  where
    averages = allAverages student subjects
    sumAllAverage = sum averages

allAverages :: Aluno -> [Disciplina] -> [Double]
allAverages student subjects =
  [Disciplina.studentAverage (registration student) d | d <- getEnrolledSubjects (enrolledSubjects student) subjects]

getEnrolledSubjects :: [Int] -> [Disciplina] -> [Disciplina]
getEnrolledSubjects [] _ = []
getEnrolledSubjects (c : cs) subjects =
  filter (\disc -> Disciplina.code disc == c) subjects ++ getEnrolledSubjects cs subjects

getNotEnrolledSubjects :: [Int] -> [Disciplina] -> [Disciplina]
getNotEnrolledSubjects [] _ = []
getNotEnrolledSubjects (c : cs) subjects =
  filter (\disc -> Disciplina.code disc /= c) subjects ++ getEnrolledSubjects cs subjects

availableOptions :: String
availableOptions =
  "\n\n1) Visualizar disciplinas\n"
    ++ "2) Realizar matrícula\n"
    ++ "3) Cancelar matrícula\n"
    ++ "4) Visualizar média geral\n"
    ++ "5) Sair do sistema\n"
    ++ "6) Logout\n"

toString :: Aluno -> String
toString aluno =
  show registration' ++ ";" ++ name' ++ ";" ++ show enrolledSubjects'
  where
    registration' = registration aluno
    name' = name aluno
    enrolledSubjects' = enrolledSubjects aluno
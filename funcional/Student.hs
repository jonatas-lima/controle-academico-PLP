module Student where

import Subject (Subject)
import qualified Subject

data Student = Student
  { registration :: Int,
    name :: String,
    enrolledSubjects :: [Int]
  }

notFound :: Student
notFound = Student 0 "not found" []

newStudent :: Int -> String -> [Int] -> Student
newStudent = Student

numberEnrolledSubjects :: Student -> Int
numberEnrolledSubjects student = length (enrolledSubjects student)

registrations :: [Student] -> [Int]
registrations students = [registration student | student <- students]

approved :: Student -> Subject -> Bool
approved student subject = Subject.studentAverage (registration student) subject >= 7

final :: Student -> Subject -> Bool
final student subject = not (approved student subject) && Subject.studentAverage (registration student) subject >= 5

situation :: Student -> Subject -> String
situation student subject
  | approved student subject = "(APROVADO)"
  | final student subject = "(FINAL)"
  | reproved student subject = "(REPROVADO)"
  | otherwise = "(EM ANDAMENTO)"

reproved :: Student -> Subject -> Bool
reproved student subject = not (final student subject) && not (approved student subject)

subjectAverage :: Student -> Subject -> Double
subjectAverage student = Subject.studentAverage (registration student)

totalAverage :: Student -> [Subject] -> Double
totalAverage student subjects =
  sumAllAverage / fromIntegral (length averages)
  where
    averages = allAverages student subjects
    sumAllAverage = sum averages

allAverages :: Student -> [Subject] -> [Double]
allAverages student subjects =
  [Subject.studentAverage (registration student) d | d <- getEnrolledSubjects (enrolledSubjects student) subjects]

getEnrolledSubjects :: [Int] -> [Subject] -> [Subject]
getEnrolledSubjects [] _ = []
getEnrolledSubjects (c : cs) subjects =
  filter (\disc -> Subject.code disc == c) subjects ++ getEnrolledSubjects cs subjects

getNotEnrolledSubjects :: [Int] -> [Subject] -> [Subject]
getNotEnrolledSubjects [] _ = []
getNotEnrolledSubjects (c : cs) subjects =
  filter (\disc -> Subject.code disc /= c) subjects ++ getEnrolledSubjects cs subjects

availableOptions :: String
availableOptions =
  "\n\n1) Visualizar disciplinas\n"
    ++ "2) Realizar matrícula\n"
    ++ "3) Cancelar matrícula\n"
    ++ "4) Visualizar média geral\n"
    ++ "5) Sair do sistema\n"
    ++ "6) Logout\n"

toString :: Student -> String
toString aluno =
  show registration' ++ ";" ++ name' ++ ";" ++ show enrolledSubjects'
  where
    registration' = registration aluno
    name' = name aluno
    enrolledSubjects' = enrolledSubjects aluno
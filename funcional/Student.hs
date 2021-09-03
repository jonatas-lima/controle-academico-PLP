module Student where

import Subject (Subject)
import qualified Subject

data Student = Student
  { registration :: Int,
    name :: String,
    enrolledSubjects :: [Int]
  }

-- / Representação de um aluno inexistente
notFound :: Student
notFound = Student 0 "not found" []

newStudent :: Int -> String -> [Int] -> Student
newStudent = Student

-- / Retorna o número de disciplina matriculadas
numberEnrolledSubjects :: Student -> Int
numberEnrolledSubjects student = length (enrolledSubjects student)

-- / Dada uma lista de estudantes, retorna suas respectivas matrículas
registrations :: [Student] -> [Int]
registrations students = [registration student | student <- students]

-- / Verifica se um estudante está aprovado em uma disciplina
approved :: Student -> Subject -> Bool
approved student subject = Subject.studentAverage (registration student) subject >= 7

-- / Verifica se um estudante está na final em uma disciplina
final :: Student -> Subject -> Bool
final student subject = not (approved student subject) && Subject.studentAverage (registration student) subject >= 5

-- / Verifica se um estudante está reprovado em uma disciplina
reproved :: Student -> Subject -> Bool
reproved student subject = not (final student subject) && not (approved student subject)

-- / Representação em string da situação de um aluno em uma disciplina
situation :: Student -> Subject -> String
situation student subject
  | approved student subject = "(APROVADO)"
  | final student subject = "(FINAL)"
  | reproved student subject = "(REPROVADO)"
  | otherwise = "(EM ANDAMENTO)"

-- / Retorna a média de um estudante em uma disciplina
subjectAverage :: Student -> Subject -> Double
subjectAverage student = Subject.studentAverage (registration student)

-- / Retorna a média geral (CRA) de um estudante
totalAverage :: Student -> [Subject] -> Double
totalAverage student subjects =
  sumAllAverage / fromIntegral (length averages)
  where
    averages = allAverages student subjects
    sumAllAverage = sum averages

-- / Retorna todas as médias do aluno
allAverages :: Student -> [Subject] -> [Double]
allAverages student subjects =
  [Subject.studentAverage (registration student) d | d <- getEnrolledSubjects (enrolledSubjects student) subjects]

-- / Retorna as disciplinas matrículadas pelo aluno
getEnrolledSubjects :: [Int] -> [Subject] -> [Subject]
getEnrolledSubjects [] _ = []
getEnrolledSubjects (c : cs) subjects =
  filter (\disc -> Subject.code disc == c) subjects ++ getEnrolledSubjects cs subjects

-- / Formato de salvamento no arquivo
toString :: Student -> String
toString aluno =
  show registration' ++ ";" ++ name' ++ ";" ++ show enrolledSubjects'
  where
    registration' = registration aluno
    name' = name aluno
    enrolledSubjects' = enrolledSubjects aluno
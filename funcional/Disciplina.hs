module Disciplina where

data Disciplina = Disciplina
  { code :: Int,
    name :: String,
    numberClasses :: Int,
    grades :: [(Int, [Double])]
  }

notFound :: Disciplina
notFound = Disciplina 0 "not found" 0 []

newSubject :: Int -> String -> Int -> [(Int, [Double])] -> Disciplina
newSubject = Disciplina

enrolledStudents :: Disciplina -> [Int]
enrolledStudents subject = [fst student | student <- grades subject]

-- / Calcula a média da turma
subjectAverage :: Disciplina -> Double
subjectAverage subject =
  sumAverages (grades subject) / fromIntegral numStudents
  where
    numStudents = length (grades subject)

-- / Soma das médias da turma
sumAverages :: [(Int, [Double])] -> Double
sumAverages [] = 0
sumAverages (n : ns) =
  average + sumAverages ns
  where
    studentId = fst n
    studentGrades = snd n
    numGrades = length studentGrades
    average = sum studentGrades / fromIntegral numGrades

-- / Calcula a média de um aluno a partir de sua matrícula
studentAverage :: Int -> Disciplina -> Double
studentAverage studentId subject =
  average
  where
    grades' = findStudentGrades studentId (grades subject)
    numGrades = length grades'
    average = if numGrades == 0 then 0 else sum grades' / fromIntegral numGrades

-- / Acha as notas de um aluno a partir de sua matrícula
findStudentGrades :: Int -> [(Int, [Double])] -> [Double]
findStudentGrades _ [] = []
findStudentGrades studentId (x : xs) =
  if studentId == id
    then grades
    else findStudentGrades studentId xs
  where
    id = fst x
    grades = snd x

isFinished :: Disciplina -> Bool
isFinished subject = numberClasses subject == 0

showSubject :: Disciplina -> String
showSubject d = show (code d) ++ "\t - " ++ showsSubjectName (name d) ++ "\t - " ++ show (numberClasses d)

showSubjectWithoutClasses :: Disciplina -> String
showSubjectWithoutClasses d = show (code d) ++ "\t - " ++ showsSubjectName (name d)

showsSubjectName :: String -> String
showsSubjectName name
  | length name < 6 = showsSubjectName (name ++ " ")
  | otherwise = name

toString :: Disciplina -> String
toString subject =
  show code' ++ ";" ++ name' ++ ";" ++ show numberClasses' ++ ";" ++ show grades'
  where
    code' = code subject
    name' = name subject
    numberClasses' = numberClasses subject
    grades' = grades subject
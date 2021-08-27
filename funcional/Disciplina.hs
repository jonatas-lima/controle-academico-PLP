module Disciplina where

data Disciplina = Disciplina
  { code :: Int,
    name :: String,
    numberClasses :: Int,
    grades :: [(Int, [Double])]
  }

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

exibeDisciplina :: Disciplina -> String
exibeDisciplina d = show (code d) ++ "\t - " ++ exibeNomeDisciplina (name d)

exibeNomeDisciplina :: String -> String
exibeNomeDisciplina nome
  | length nome < 6 = exibeNomeDisciplina (nome ++ " ")
  | otherwise = nome

toString :: Disciplina -> String
toString disciplina =
  show codigo' ++ ";" ++ nome' ++ ";" ++ show qtdDeAulas' ++ ";" ++ show notas'
  where
    codigo' = code disciplina
    nome' = name disciplina
    qtdDeAulas' = numberClasses disciplina
    notas' = grades disciplina
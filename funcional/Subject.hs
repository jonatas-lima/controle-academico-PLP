module Subject where

data Subject = Subject
  { code :: Int,
    professorRegistration :: Int,
    name :: String,
    numberClasses :: Int,
    studentLimit :: Int,
    grades :: [(Int, [Double])]
  }

-- / Função usada para representar uma disciplina inexistente
notFound :: Subject
notFound = Subject 0 0 "not found" 0 0 []

-- / Cria uma nova disciplina
newSubject :: Int -> Int -> String -> Int -> Int -> [(Int, [Double])] -> Subject
newSubject = Subject

-- / Verifica se as vagas para estudantes se esgotaram
isFull :: Subject -> Bool
isFull subject = length (grades subject) == studentLimit subject

-- / Verifica se um professor está alocado à disciplina
hasProfessor :: Subject -> Bool
hasProfessor subject = professorRegistration subject /= 0

-- / Matrícula dos estudantes matriculados na disciplina
enrolledStudents :: Subject -> [Int]
enrolledStudents subject = [fst student | student <- grades subject]

-- / Calcula a média da turma
subjectAverage :: Subject -> Double
subjectAverage subject =
  average
  where
    numStudents = length (grades subject)
    average = if numStudents == 0 then 0 else sumAverages (grades subject) / fromIntegral numStudents

-- / Soma das médias da turma
sumAverages :: [(Int, [Double])] -> Double
sumAverages [] = 0
sumAverages (n : ns) =
  average + sumAverages ns
  where
    studentId = fst n
    studentGrades = snd n
    numGrades = length studentGrades
    average = if numGrades == 0 then 0 else sum studentGrades / fromIntegral numGrades

-- / Calcula a média de um aluno a partir de sua matrícula
studentAverage :: Int -> Subject -> Double
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

-- / Verifica se a disciplina encontra-se encerrada
isFinished :: Subject -> Bool
isFinished subject = numberClasses subject == 0

-- / Representação em string da disciplina
showSubject :: Subject -> String
showSubject d = show (code d) ++ "\t - " ++ showsSubjectName (name d) ++ "\t - " ++ show (numberClasses d)

-- / Representação em string da disciplina sem o número de aulas restantes
showSubjectWithoutClasses :: Subject -> String
showSubjectWithoutClasses d = show (code d) ++ "\t - " ++ showsSubjectName (name d)

showsSubjectName :: String -> String
showsSubjectName name
  | length name < 6 = showsSubjectName (name ++ " ")
  | otherwise = name

-- / Formato no qual a disciplina será salva no arquivo csv
toString :: Subject -> String
toString subject =
  show code' ++ ";" ++ show professorRegistration' ++ ";" ++ name' ++ ";" ++ show numberClasses' ++ ";" ++ show studentLimit' ++ ";" ++show grades'
  where
    code' = code subject
    professorRegistration' = professorRegistration subject
    name' = name subject
    numberClasses' = numberClasses subject
    studentLimit' = studentLimit subject
    grades' = grades subject
module Professor where

data Professor = Professor
  { registration :: Int,
    name :: String,
    subjects :: [Int]
  }

-- / Representação de um professor inexistente
notFound :: Professor
notFound = Professor 0 "not found" []

newProfessor :: Int -> String -> [Int] -> Professor
newProfessor = Professor

-- / Dada uma lista de professores, retorna suas respectivas matrículas
registrations :: [Professor] -> [Int]
registrations professors = [registration prof | prof <- professors]

-- / Retorna o número de disciplinas lecionadas pelo professor
numberOfSubjects :: Professor -> Int
numberOfSubjects professor = length (subjects professor)

-- / Verifica se um professor possui uma determinada disciplina
hasSubject :: Professor -> Int -> Bool
hasSubject professor codeSubject = codeSubject `elem` subjects professor

-- / Formato de salvamento no arquivo
toString :: Professor -> String
toString professor =
  show id' ++ ";" ++ name' ++ ";" ++ show subjects'
  where
    id' = registration professor
    name' = name professor
    subjects' = subjects professor
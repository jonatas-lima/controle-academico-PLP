module Professor where

data Professor = Professor
  { registration :: Int,
    name :: String,
    subjects :: [Int]
  }

notFound :: Professor
notFound = Professor 0 "not found" []

newProfessor :: Int -> String -> [Int] -> Professor
newProfessor = Professor

registrations :: [Professor] -> [Int]
registrations professors = [registration prof | prof <- professors]

numberOfSubjects :: Professor -> Int
numberOfSubjects professor = length (subjects professor)

availableOptions :: String
availableOptions =
  "\n\n1) Visualizar disciplinas\n"
    ++ "2) Registrar aula\n"
    ++ "3) Cadastrar prova\n"
    ++ "4) Situação da classe\n"
    ++ "5) Sair do sistema\n"
    ++ "6) Logout\n"

hasSubject :: Professor -> Int -> Bool
hasSubject professor codeSubject = codeSubject `elem` subjects professor

toString :: Professor -> String
toString professor =
  show id' ++ ";" ++ name' ++ ";" ++ show subjects'
  where
    id' = registration professor
    name' = name professor
    subjects' = subjects professor
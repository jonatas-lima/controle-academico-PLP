module Professor where

data Professor = Professor
  { matricula :: Int,
    nome :: String,
    disciplinasLecionadas :: [Int]
  }

newProfessor :: Int -> String -> [Int] -> Professor
newProfessor matricula' nome' disciplinasLecionadas' =
  Professor
    { matricula = matricula',
      nome = nome',
      disciplinasLecionadas = disciplinasLecionadas'
    }

matriculas :: [Professor] -> [Int]
matriculas professores = [matricula prof | prof <- professores]

numDisciplinasLecionadas :: Professor -> Int
numDisciplinasLecionadas professor = length (disciplinasLecionadas professor)

toString :: Professor -> String
toString professor =
  show matricula' ++ "," ++ nome' ++ "," ++ show disciplinasLecionadas'
  where
    matricula' = matricula professor
    nome' = nome professor
    disciplinasLecionadas' = disciplinasLecionadas professor
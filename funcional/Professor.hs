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

opcoesDisponiveis :: String
opcoesDisponiveis =
  "\n\n1) Visualizar disciplinas\n"
    ++ "2) Registrar aula\n"
    ++ "3) Cadastrar prova\n"
    ++ "4) Sair do sistema\n"
    ++ "5) Fazer logoff\n"

temDisciplina :: Int -> [Int]-> Bool
temDisciplina n [] = False
temDisciplina n (x:xs) = if n == x then True else temDisciplina n xs

toString :: Professor -> String
toString professor =
  show matricula' ++ ";" ++ nome' ++ ";" ++ show disciplinasLecionadas'
  where
    matricula' = matricula professor
    nome' = nome professor
    disciplinasLecionadas' = disciplinasLecionadas professor
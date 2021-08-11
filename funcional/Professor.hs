module Professor where

import Disciplina (Disciplina)

data Professor = Professor
  { matricula :: Int,
    nome :: String,
    disciplinasLecionadas :: (Disciplina, Disciplina)
  }

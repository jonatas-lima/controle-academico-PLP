module Aluno where

import Disciplina (Disciplina)

data Aluno = Aluno
  { matricula :: Int,
    nome :: String,
    disciplinasMatriculadas :: (Int, Int, Int, Int)
  }

module Disciplina where

data Disciplina = Disciplina
  { codigo :: String,
    nome :: String,
    creditos :: Int,
    notas :: [(String, [Double])],
    descartaNotaMaisBaixa :: Bool
  }

-- / Calcula a média da turma
mediaDisciplina :: [String] -> Disciplina -> Double
mediaDisciplina matriculas disciplina =
  somaMedias matriculas tuplasMatriculaNotas / numAlunos
  where
    numAlunos = fromIntegral (length tuplasMatriculaNotas)
    tuplasMatriculaNotas = notas disciplina

-- / Soma das médias da turma
somaMedias :: [String] -> [(String, [Double])] -> Double
somaMedias [] _ = 0
somaMedias (a : as) tuplasMatriculaNotas =
  mediaAluno a tuplasMatriculaNotas + somaMedias as tuplasMatriculaNotas

-- / Calcula a média de um aluno
mediaAluno :: String -> [(String, [Double])] -> Double
mediaAluno _ [] = 0
mediaAluno matrAluno tuplasMatriculaNotas =
  sum notas / fromIntegral (length notas)
  where
    notas = findNotasAluno matrAluno tuplasMatriculaNotas

alunosMatriculados :: Disciplina -> [String]
alunosMatriculados disciplina = [x | x <- matriculas (notas disciplina)]

matriculas :: [(String, [Double])] -> [String]
matriculas = map fst

-- / Acha as notas de um aluno
findNotasAluno :: String -> [(String, [Double])] -> [Double]
findNotasAluno _ [] = []
findNotasAluno matrAluno (x : xs) =
  if matrAluno == matr
    then notas
    else findNotasAluno matrAluno xs
  where
    matr = fst x
    notas = snd x

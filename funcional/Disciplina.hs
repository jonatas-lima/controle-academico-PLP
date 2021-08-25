module Disciplina where

data Disciplina = Disciplina
  { codigo :: Int,
    nome :: String,
    qtdDeAulas :: Int,
    notas :: [(Int, [Double])],
    descartaNotaMaisBaixa :: Bool
  }

-- / Calcula a média da turma
mediaDisciplina :: [Int] -> Disciplina -> Double
mediaDisciplina matriculas disciplina =
  somaMedias matriculas tuplasMatriculaNotas / numAlunos
  where
    numAlunos = fromIntegral (length tuplasMatriculaNotas)
    tuplasMatriculaNotas = notas disciplina

-- / Soma das médias da turma
somaMedias :: [Int] -> [(Int, [Double])] -> Double
somaMedias [] _ = 0
somaMedias (a : as) tuplasMatriculaNotas =
  mediaAluno a tuplasMatriculaNotas + somaMedias as tuplasMatriculaNotas

-- / Calcula a média de um aluno a partir de sua matrícula
mediaAluno :: Int -> [(Int, [Double])] -> Double
mediaAluno _ [] = 0
mediaAluno matrAluno tuplasMatriculaNotas =
  sum notas / fromIntegral (length notas)
  where
    notas = findNotasAluno matrAluno tuplasMatriculaNotas

-- / Acha as notas de um aluno a partir de sua matrícula
findNotasAluno :: Int -> [(Int, [Double])] -> [Double]
findNotasAluno _ [] = []
findNotasAluno matrAluno (x : xs) =
  if matrAluno == matr
    then notas
    else findNotasAluno matrAluno xs
  where
    matr = fst x
    notas = snd x

exibeDisciplina :: Disciplina -> String
exibeDisciplina d = nome d++ " - " ++ show (codigo d)

toString :: Disciplina -> String
toString disciplina =
  show codigo' ++ ";" ++ nome' ++ ";" ++ show creditos' ++ ";" ++ show descartaNotaMaisBaixa' ++ ";" ++ show notas'
  where
    codigo' = codigo disciplina
    nome' = nome disciplina
    qtdDeAulas' = qtdDeAulas disciplina
    descartaNotaMaisBaixa' = descartaNotaMaisBaixa disciplina
    notas' = notas disciplina
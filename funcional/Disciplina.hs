module Disciplina where

data Disciplina = Disciplina
  { codigo :: Int,
    nome :: String,
    qtdDeAulas :: Int,
    notas :: [(Int, [Double])],
    descartaNotaMaisBaixa :: Bool
  }

alunosMatriculados :: Disciplina -> [Int]
alunosMatriculados disciplina = [fst aluno | aluno <- notas disciplina]

-- / Calcula a média da turma
mediaDisciplina :: Disciplina -> Double
mediaDisciplina disciplina =
  somaMedias (notas disciplina) / fromIntegral numAlunos
  where
    numAlunos = length (notas disciplina)

-- / Soma das médias da turma
somaMedias :: [(Int, [Double])] -> Double
somaMedias [] = 0
somaMedias (n : ns) =
  media + somaMedias ns
  where
    matrAluno = fst n
    notasAluno = snd n
    numNotas = length notasAluno
    media = sum notasAluno / fromIntegral numNotas

-- / Calcula a média de um aluno a partir de sua matrícula
mediaAluno :: Int -> Disciplina -> Double
mediaAluno matrAluno disciplina =
  sum notas' / fromIntegral numNotas
  where
    notas' = findNotasAluno matrAluno (notas disciplina)
    numNotas = length notas'

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
exibeDisciplina d = show (codigo d) ++ "\t - " ++ nome d

toString :: Disciplina -> String
toString disciplina =
  show codigo' ++ ";" ++ nome' ++ ";" ++ show qtdDeAulas' ++ ";" ++ show descartaNotaMaisBaixa' ++ ";" ++ show notas'
  where
    codigo' = codigo disciplina
    nome' = nome disciplina
    qtdDeAulas' = qtdDeAulas disciplina
    descartaNotaMaisBaixa' = descartaNotaMaisBaixa disciplina
    notas' = notas disciplina
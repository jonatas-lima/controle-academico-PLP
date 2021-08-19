module Aluno where

import Disciplina (Disciplina, findNotasAluno, mediaAluno, notas)

data Aluno = Aluno
  { matricula :: Int,
    nome :: String,
    disciplinasMatriculadas :: [Disciplina]
  }

aprovado :: Aluno -> Disciplina -> Bool
aprovado aluno disciplina = mediaAluno (matricula aluno) (notas disciplina) >= 7

final :: Aluno -> Disciplina -> Bool
final aluno disciplina = not (aprovado aluno disciplina) && mediaAluno (matricula aluno) (notas disciplina) >= 5

reprovado :: Aluno -> Disciplina -> Bool
reprovado aluno disciplina = not (final aluno disciplina) && not (aprovado aluno disciplina)

mediaDisciplina :: Aluno -> Disciplina -> Double
mediaDisciplina aluno disciplina = mediaAluno (matricula aluno) (notas disciplina)

mediaTotal :: Aluno -> Double
mediaTotal aluno =
  somaTodasMedias / fromIntegral (length medias)
  where
    medias = todasMedias aluno
    somaTodasMedias = sum medias

todasMedias :: Aluno -> [Double]
todasMedias aluno = [mediaAluno (matricula aluno) (notas d) | d <- disciplinasMatriculadas aluno]
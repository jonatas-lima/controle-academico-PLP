module Aluno where

import Disciplina (Disciplina, codigo, findNotasAluno, mediaAluno, notas)

data Aluno = Aluno
  { matricula :: Int,
    nome :: String,
    disciplinasMatriculadas :: [Int]
  }

newAluno :: Int -> String -> [Int] -> Aluno
newAluno matricula' nome' disciplinasMatriculadas' =
  Aluno
    { matricula = matricula',
      nome = nome',
      disciplinasMatriculadas = disciplinasMatriculadas'
    }

numDisciplinasMatriculadas :: Aluno -> Int
numDisciplinasMatriculadas aluno = length (disciplinasMatriculadas aluno)

matriculas :: [Aluno] -> [Int]
matriculas alunos = [matricula aluno | aluno <- alunos]

aprovado :: Aluno -> Disciplina -> Bool
aprovado aluno disciplina = mediaAluno (matricula aluno) (notas disciplina) >= 7

final :: Aluno -> Disciplina -> Bool
final aluno disciplina = not (aprovado aluno disciplina) && mediaAluno (matricula aluno) (notas disciplina) >= 5

reprovado :: Aluno -> Disciplina -> Bool
reprovado aluno disciplina = not (final aluno disciplina) && not (aprovado aluno disciplina)

mediaDisciplina :: Aluno -> Disciplina -> Double
mediaDisciplina aluno disciplina = mediaAluno (matricula aluno) (notas disciplina)

mediaTotal :: Aluno -> [Disciplina] -> Double
mediaTotal aluno disciplinas =
  somaTodasMedias / fromIntegral (length medias)
  where
    medias = todasMedias aluno disciplinas
    somaTodasMedias = sum medias

todasMedias :: Aluno -> [Disciplina] -> [Double]
todasMedias aluno = todasMediasAux (matricula aluno) (disciplinasMatriculadas aluno)

todasMediasAux :: Int -> [Int] -> [Disciplina] -> [Double]
todasMediasAux _ _ [] = []
todasMediasAux _ [] _ = []
todasMediasAux matricula (codDisciplina : cs) (disciplina : ds) =
  if codDisciplina == codigo disciplina
    then mediaAluno matricula (notas disciplina) : todasMediasAux matricula cs ds
    else todasMediasAux matricula cs ds

toString :: Aluno -> String
toString aluno =
  show matricula' ++ "," ++ nome' ++ "," ++ show disciplinasMatriculadas'
  where
    matricula' = matricula aluno
    nome' = nome aluno
    disciplinasMatriculadas' = disciplinasMatriculadas aluno
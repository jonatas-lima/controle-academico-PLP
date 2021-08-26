module Controle where

import Aluno (Aluno)
import qualified Aluno
import DataLoader (carregaAlunos, carregaDisciplina, carregaProfessores, leArquivo)
import DataSaver (salvaAluno, salvaProfessor)
import Disciplina (Disciplina)
import qualified Disciplina
import Professor (Professor)
import qualified Professor

cadastraProfessor :: Int -> String -> String -> IO ()
cadastraProfessor matrProf nomeProf senha = do
  arquivoProfessores <- leArquivo "./data/professores.csv"
  let professoresCadastrados = carregaProfessores arquivoProfessores
  let matriculasCadastradas = Professor.matriculas professoresCadastrados

  if existeMatricula matrProf matriculasCadastradas
    then putStrLn "Professor já cadastrado!"
    else do
      salvaProfessor professor senha
      putStrLn "Professor cadastrado!"
  where
    professor = Professor.newProfessor matrProf nomeProf []

cadastraAluno :: Int -> String -> String -> IO ()
cadastraAluno matrAluno nomeAluno senha = do
  arquivoAlunos <- leArquivo "./data/alunos.csv"
  let alunosCadastrados = carregaAlunos arquivoAlunos
  let matriculasCadastradas = Aluno.matriculas alunosCadastrados

  if existeMatricula matrAluno matriculasCadastradas
    then putStrLn "Professor já cadastrado!"
    else do
      salvaAluno aluno senha
      putStrLn "Professor cadastrado!"
  where
    aluno = Aluno.newAluno matrAluno nomeAluno []

existeMatricula :: Int -> [Int] -> Bool
existeMatricula matr matriculas = matr `elem` matriculas

associaProfessorDisciplina :: Professor -> Disciplina -> Bool
associaProfessorDisciplina prof disciplina = True

matriculaAluno :: Aluno -> Disciplina -> Bool
matriculaAluno aluno disciplina = True

alunosSemMatriculas :: [Aluno] -> [Int]
alunosSemMatriculas [] = []
alunosSemMatriculas (a : as) =
  if null (Aluno.disciplinasMatriculadas a)
    then Aluno.matricula a : alunosSemMatriculas as
    else alunosSemMatriculas as

professoresSemDisciplinas :: [Professor] -> [Int]
professoresSemDisciplinas [] = []
professoresSemDisciplinas (p : ps) =
  if null (Professor.disciplinasLecionadas p)
    then Professor.matricula p : professoresSemDisciplinas ps
    else professoresSemDisciplinas ps

disciplinasMatriculadas :: Aluno -> [Disciplina] -> [Disciplina]
disciplinasMatriculadas aluno disciplinas = [DataLoader.carregaDisciplina c disciplinas | c <- Aluno.disciplinasMatriculadas aluno]

disciplinaComMaiorMedia :: [Disciplina] -> Disciplina
disciplinaComMaiorMedia disciplinas = do
  let matrDisciplina = matriculaDisciplinaMaiorMedia (mediasDisciplinas disciplinas)
  DataLoader.carregaDisciplina matrDisciplina disciplinas

disciplinaComMenorMedia :: [Disciplina] -> Disciplina
disciplinaComMenorMedia disciplinas = do
  let matrDisciplina = matriculaDisciplinaMenorMedia (mediasDisciplinas disciplinas)
  DataLoader.carregaDisciplina matrDisciplina disciplinas

mediasDisciplinas :: [Disciplina] -> [(Int, Double)]
mediasDisciplinas disciplinas = [(Disciplina.codigo d, Disciplina.mediaDisciplina d) | d <- disciplinas]

matriculaDisciplinaMaiorMedia :: [(Int, Double)] -> Int
matriculaDisciplinaMaiorMedia [] = -1
matriculaDisciplinaMaiorMedia (d : ds) =
  if snd d == maiorNota then fst d else matriculaDisciplinaMaiorMedia ds
  where
    notas = [snd m | m <- d : ds]
    maiorNota = maximum notas

matriculaDisciplinaMenorMedia :: [(Int, Double)] -> Int
matriculaDisciplinaMenorMedia [] = -1
matriculaDisciplinaMenorMedia (d : ds) =
  if snd d == maiorNota then fst d else matriculaDisciplinaMaiorMedia ds
  where
    notas = [snd m | m <- d : ds]
    maiorNota = minimum notas
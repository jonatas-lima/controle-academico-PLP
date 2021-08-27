module Controle where

import Aluno (Aluno)
import qualified Aluno
import DataLoader
import qualified DataLoader
import DataSaver (atualizaProfessor, salvaAluno, salvaProfessor)
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

matriculaAluno :: Aluno -> Disciplina -> Bool
matriculaAluno aluno disciplina = True

listaAlunosSemMatriculas :: [Aluno] -> String
listaAlunosSemMatriculas [] = ""
listaAlunosSemMatriculas (a : as) =
  if null $ Aluno.disciplinasMatriculadas a
    then formataListagemAluno a ++ "\n" ++ listaAlunosSemMatriculas as
    else listaAlunosSemMatriculas as

formataListagemAluno :: Aluno -> String
formataListagemAluno aluno = show (Aluno.matricula aluno) ++ "\t - \t" ++ Aluno.nome aluno

listaProfessoresSemMatriculas :: [Professor] -> String
listaProfessoresSemMatriculas [] = ""
listaProfessoresSemMatriculas (p : ps) =
  if null $ Professor.disciplinasLecionadas p
    then formataListagemProfessor p ++ "\n" ++ listaProfessoresSemMatriculas ps
    else listaProfessoresSemMatriculas ps

listaProfessoresDisponiveis :: [Professor] -> String
listaProfessoresDisponiveis [] = ""
listaProfessoresDisponiveis (p : ps) =
  if Professor.numDisciplinasLecionadas p < 3
    then formataListagemProfessor p ++ "\n" ++ listaProfessoresDisponiveis ps
    else listaProfessoresDisponiveis ps

listaDisciplinasDisponiveisParaAssociacao :: Professor -> [Disciplina] -> String
listaDisciplinasDisponiveisParaAssociacao professor disciplinas =
  formataListagemDisciplinas $ DataLoader.carregaDisciplinasPorCodigo codDisciplinasDisponiveis disciplinas
  where
    codDisciplinas = map Disciplina.codigo disciplinas
    disciplinasLecionadas = Professor.disciplinasLecionadas professor
    codDisciplinasDisponiveis = filter (`notElem` disciplinasLecionadas) codDisciplinas

formataListagemDisciplinas :: [Disciplina] -> String
formataListagemDisciplinas [] = ""
formataListagemDisciplinas (d : ds) = formataListagemDisciplina d ++ "\n" ++ formataListagemDisciplinas ds

formataListagemDisciplina :: Disciplina -> String
formataListagemDisciplina disciplina = show (Disciplina.codigo disciplina) ++ "\t - \t" ++ Disciplina.nome disciplina

formataListagemDisciplinaMedia :: Disciplina -> String
formataListagemDisciplinaMedia disciplina =
  formataListagemDisciplina disciplina ++ "\t - \t" ++ show media
  where
    media = Disciplina.mediaDisciplina disciplina

formataListagemProfessor :: Professor -> String
formataListagemProfessor professor = show (Professor.matricula professor) ++ "\t - \t" ++ Professor.nome professor

associaProfessorDisciplina :: Professor -> Disciplina -> [Disciplina] -> IO ()
associaProfessorDisciplina professor disciplina disciplinas =
  if notElem (Disciplina.codigo disciplina) codDisciplinas || Professor.temDisciplina professor (Disciplina.codigo disciplina)
    then putStrLn "Erro ao associar professor à disciplina"
    else do
      DataSaver.atualizaProfessor (Professor.matricula professor) professorAtualizado
      putStrLn "Disciplina associada!"
  where
    codDisciplinas = map Disciplina.codigo disciplinas
    disciplinasLecionadas = Professor.disciplinasLecionadas professor
    professorAtualizado = Professor.newProfessor (Professor.matricula professor) (Professor.nome professor) (Disciplina.codigo disciplina : Professor.disciplinasLecionadas professor)

disciplinasMatriculadas :: Aluno -> [Disciplina] -> [Disciplina]
disciplinasMatriculadas aluno disciplinas = [DataLoader.carregaDisciplina c disciplinas | c <- Aluno.disciplinasMatriculadas aluno]

exibeDisciplinaComMaiorMedia :: [Disciplina] -> String
exibeDisciplinaComMaiorMedia disciplinas =
  formataListagemDisciplinaMedia $ disciplinaComMaiorMedia disciplinas

disciplinaComMaiorMedia :: [Disciplina] -> Disciplina
disciplinaComMaiorMedia disciplinas = do
  let matrDisciplina = matriculaDisciplinaMaiorMedia (mediasDisciplinas disciplinas)
  DataLoader.carregaDisciplina matrDisciplina disciplinas

exibeDisciplinaComMenorMedia :: [Disciplina] -> String
exibeDisciplinaComMenorMedia disciplinas =
  formataListagemDisciplinaMedia $ disciplinaComMenorMedia disciplinas

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
  if snd d == menorNota then fst d else matriculaDisciplinaMenorMedia ds
  where
    notas = [snd m | m <- d : ds]
    menorNota = minimum notas
module DataSaver where

import Aluno (Aluno (matricula), toString)
import Data.List.Split (splitOn)
import Data.Text (replace, unpack)
import qualified DataLoader
import Disciplina (Disciplina, toString)
import Professor (Professor (Professor, matricula), toString)
import qualified System.IO.Strict as Strict
import Usuario (Usuario (Usuario), toString)

append :: String -> String -> IO ()
append linha arquivo = appendFile arquivo (linha ++ "\n")

salvaUsuario :: Usuario -> IO ()
salvaUsuario usuario = append (Usuario.toString usuario) "./data/usuarios.csv"

salvaProfessor :: Professor -> String -> IO ()
salvaProfessor professor senha = do
  append (Professor.toString professor) "./data/professores.csv"
  append (show (Professor.matricula professor) ++ "," ++ senha ++ "," ++ "prof") "./data/usuarios.csv"

salvaAluno :: Aluno -> String -> IO ()
salvaAluno aluno senha = do
  append (Aluno.toString aluno) "./data/alunos.csv"
  append (show (Aluno.matricula aluno) ++ "," ++ senha ++ "," ++ "aluno") "./data/usuarios.csv"

salvaDisciplina :: Disciplina -> IO ()
salvaDisciplina disciplina = append (Disciplina.toString disciplina) "./data/disciplinas.csv"

atualizaProfessor :: Int -> Professor -> IO ()
atualizaProfessor matricula =
  atualizaEntidade matricula "./data/professores.csv" Professor.toString DataLoader.carregaProfessores DataLoader.carregaProfessor

atualizaAluno :: Int -> Aluno -> IO ()
atualizaAluno matricula =
  atualizaEntidade matricula "./data/alunos.csv" Aluno.toString DataLoader.carregaAlunos DataLoader.carregaAluno

atualizaDisciplina :: Int -> Disciplina -> IO ()
atualizaDisciplina codigo =
  atualizaEntidade codigo "./data/disciplinas.csv" Disciplina.toString DataLoader.carregaDisciplinas DataLoader.carregaDisciplina

atualizaEntidade :: Int -> String -> (t -> String) -> ([String] -> [t]) -> (Int -> [t] -> t) -> t -> IO ()
atualizaEntidade matricula filePath toString loadAll loadOne entidade = do
  arquivo <- DataLoader.leArquivo filePath

  let entidades = loadAll arquivo
  let entidadeAntiga = toString $ loadOne matricula entidades
  let arquivoAtualizado = updateEntityData (toString entidade) entidadeAntiga arquivo

  writeFile filePath $ parseLines arquivoAtualizado

parseLines :: [String] -> String
parseLines [] = ""
parseLines (l : ls) = l ++ "\n" ++ parseLines ls

updateEntityData :: String -> String -> [String] -> [String]
updateEntityData _ _ [] = []
updateEntityData updatedEntity outdatedEntity (l : ls) =
  if l == outdatedEntity
    then updatedEntity : updateEntityData updatedEntity outdatedEntity ls
    else l : updateEntityData updatedEntity outdatedEntity ls

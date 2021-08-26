module DataSaver where

import Aluno (Aluno (matricula), toString)
import Disciplina (Disciplina, toString)
import Professor (Professor (matricula), toString)
import Usuario (Usuario (Usuario), toString)

write :: String -> String -> IO ()
write linha arquivo = appendFile arquivo (linha ++ "\n")

salvaUsuario :: Usuario -> IO ()
salvaUsuario usuario = write (Usuario.toString usuario) "./data/usuarios.csv"

salvaProfessor :: Professor -> String -> IO ()
salvaProfessor professor senha = do
  write (Professor.toString professor) "./data/professores.csv"
  write (show (Professor.matricula professor) ++ "," ++ senha ++ "," ++ "prof") "./data/usuarios.csv"

salvaAluno :: Aluno -> String -> IO ()
salvaAluno aluno senha = do
  write (Aluno.toString aluno) "./data/alunos.csv"
  write (show (Aluno.matricula aluno) ++ "," ++ senha ++ "," ++ "aluno") "./data/usuarios.csv"

salvaDisciplina :: Disciplina -> IO ()
salvaDisciplina disciplina = write (Disciplina.toString disciplina) "./data/disciplinas.csv"
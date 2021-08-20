module DataSaver where

import Aluno (Aluno, toString)
import Disciplina (Disciplina, toString)
import Professor (Professor, toString)
import Usuario (Usuario (Usuario), toString)

write :: String -> String -> IO ()
write linha arquivo = appendFile arquivo (linha ++ "\n")

salvaUsuario :: Usuario -> IO ()
salvaUsuario usuario = write (Usuario.toString usuario) "./data/usuarios.csv"

salvaProfessor :: Professor -> IO ()
salvaProfessor professor = write (Professor.toString professor) "./data/professores.csv"

salvaAluno :: Aluno -> IO ()
salvaAluno aluno = write (Aluno.toString aluno) "./data/alunos.csv"

salvaDisciplina :: Disciplina -> IO ()
salvaDisciplina disciplina = write (Disciplina.toString disciplina) "./data/disciplinas.csv"
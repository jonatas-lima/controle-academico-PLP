module DataLoader where

import Aluno (Aluno (..))
import Data.List.Split (splitOn)
import Disciplina (Disciplina (..))
import Professor (Professor (..))
import qualified System.IO.Strict as Strict
import Usuario (Usuario (..))

leArquivo :: String -> IO [String]
leArquivo path = do
  arq <- Strict.readFile path
  let list = lines arq
  return list

carregaUsuarios :: [String] -> [Usuario]
carregaUsuarios linhas = [parseUsuario linha | linha <- linhas]

parseUsuario :: String -> Usuario
parseUsuario linha =
  Usuario
    { Usuario.nickname = head dados,
      Usuario.senha = dados !! 1,
      Usuario.role = dados !! 2
    }
  where
    dados = splitOn "," linha

carregaProfessores :: [String] -> [Professor]
carregaProfessores linhas = [parseProfessor linha | linha <- linhas]

parseProfessor :: String -> Professor
parseProfessor linha =
  Professor
    { Professor.matricula = read (head dados) :: Int,
      Professor.nome = dados !! 1,
      Professor.disciplinasLecionadas = parseDisciplinasLecionadas (dados !! 2)
    }
  where
    dados = splitOn "," linha

parseDisciplinasLecionadas :: String -> (Int, Int)
parseDisciplinasLecionadas = read

carregaAlunos :: [String] -> [Aluno]
carregaAlunos linhas = [parseAluno linha | linha <- linhas]

parseAluno :: String -> Aluno
parseAluno linha =
  Aluno
    { Aluno.matricula = read (head dados) :: Int,
      Aluno.nome = dados !! 1,
      Aluno.disciplinasMatriculadas = parseDisciplinasMatriculadas (dados !! 2)
    }
  where
    dados = splitOn "," linha

parseDisciplinasMatriculadas :: String -> (Int, Int, Int, Int)
parseDisciplinasMatriculadas = read

parseDisciplina :: String -> Disciplina
parseDisciplina linha =
  Disciplina
    { Disciplina.codigo = read (head dados) :: Int,
      Disciplina.nome = dados !! 1,
      Disciplina.descartaNotaMaisBaixa = read (dados !! 2) :: Bool,
      Disciplina.creditos = read (dados !! 3) :: Int,
      Disciplina.notas = parseAlunosMatriculados (dados !! 4)
    }
  where
    dados = splitOn "," linha

parseAlunosMatriculados :: String -> [(Int, [Double])]
parseAlunosMatriculados = read
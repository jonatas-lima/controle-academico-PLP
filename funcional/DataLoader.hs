{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module DataLoader where

import Aluno (Aluno (..))
import Data.List.Split (splitOn)
import Disciplina (Disciplina (..))
import Professor (Professor (..))
import qualified System.IO.Strict as Strict
import Usuario (Usuario (..))

-- / Retorna as linhas de um arquivo passado no parâmetro
leArquivo :: String -> IO [String]
leArquivo path = do
  arq <- Strict.readFile path
  let list = lines arq
  return list

-- / Carrega os usuários do sistema
carregaUsuarios :: [String] -> [Usuario]
carregaUsuarios linhas = [parseUsuario linha | linha <- linhas]

-- / Transforma uma linha do arquivo em um usuário
parseUsuario :: String -> Usuario
parseUsuario linha =
  Usuario
    { Usuario.nickname = head dados,
      Usuario.senha = dados !! 1,
      Usuario.role = dados !! 2
    }
  where
    dados = splitOn "," linha

-- / Carrega os professores do sistema
carregaProfessores :: [String] -> [Professor]
carregaProfessores linhas = [parseProfessor linha | linha <- linhas]

-- / Carrega um professor a partir de sua matrícula
carregaProfessor :: Int -> [Professor] -> Professor
carregaProfessor matricula' (p : ps) =
  if Professor.matricula p == matricula'
    then p
    else carregaProfessor matricula' ps

professorExiste :: Int -> [Professor] -> Bool
professorExiste _ [] = False
professorExiste matr (p : ps) = (Professor.matricula p == matr) || professorExiste matr ps

parseProfessor :: String -> Professor
parseProfessor linha =
  Professor
    { Professor.matricula = read (head dados) :: Int,
      Professor.nome = dados !! 1,
      Professor.disciplinasLecionadas = read (dados !! 2) :: [Int]
    }
  where
    dados = splitOn "," linha

carregaAlunos :: [String] -> [Aluno]
carregaAlunos linhas = [parseAluno linha | linha <- linhas]

carregaAluno :: Int -> [Aluno] -> Aluno
carregaAluno matricula' (a : as) =
  if Aluno.matricula a == matricula'
    then a
    else carregaAluno matricula' as

alunoExiste :: Int -> [Aluno] -> Bool
alunoExiste _ [] = False
alunoExiste matr (a : as) = (Aluno.matricula a == matr) || alunoExiste matr as

parseAluno :: String -> Aluno
parseAluno linha =
  Aluno
    { Aluno.matricula = read (head dados) :: Int,
      Aluno.nome = dados !! 1,
      Aluno.disciplinasMatriculadas = read (dados !! 2) :: [Int]
    }
  where
    dados = splitOn "," linha

parseDisciplina :: String -> Disciplina
parseDisciplina linha =
  Disciplina
    { Disciplina.codigo = read (head dados) :: Int,
      Disciplina.nome = dados !! 1,
      Disciplina.descartaNotaMaisBaixa = read (dados !! 2) :: Bool,
      Disciplina.numMaxAlunos = read (dados !! 3) :: Int,
      Disciplina.notas = parseAlunosMatriculados (dados !! 4)
    }
  where
    dados = splitOn "," linha

parseAlunosMatriculados :: String -> [(Int, [Double])]
parseAlunosMatriculados = read
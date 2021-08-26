-- {-# OPTIONS_GHC -Wno-incomplete-patterns #-}
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

carregaProfessor :: Int -> [Professor] -> Professor
carregaProfessor matricula' (p:ps) =
  if Professor.matricula p == matricula' then p
  else carregaProfessor matricula' ps

parseProfessor :: String -> Professor
parseProfessor linha =
  Professor
    { Professor.matricula = read (head dados) :: Int,
      Professor.nome = dados !! 1,
      Professor.disciplinasLecionadas = read (dados !! 2) :: [Int]
    }
  where
    dados = splitOn ";" linha

carregaAlunos :: [String] -> [Aluno]
carregaAlunos linhas = [parseAluno linha | linha <- linhas]

carregaAluno :: Int -> [Aluno] -> Aluno
carregaAluno matricula' (a:as)
  | Aluno.matricula a == matricula' = a
  | otherwise = carregaAluno matricula' as

parseAluno :: String -> Aluno
parseAluno linha =
  Aluno
    { Aluno.matricula = read (head dados) :: Int,
      Aluno.nome = dados !! 1,
      Aluno.disciplinasMatriculadas = read (dados !! 2) :: [Int]
    }
  where
    dados = splitOn ";" linha

parseDisciplina :: String -> Disciplina
parseDisciplina linha =
  Disciplina
    { Disciplina.codigo = read (head dados) :: Int,
      Disciplina.nome = dados !! 1,
      Disciplina.descartaNotaMaisBaixa = read (dados !! 2) :: Bool,
      Disciplina.qtdDeAulas = read (dados !! 3) :: Int,
      Disciplina.notas = parseAlunosMatriculados (dados !! 4)
    }
  where
    dados = splitOn ";" linha

parseAlunosMatriculados :: String -> [(Int, [Double])]
parseAlunosMatriculados = read

carregaDisciplinas :: [String] -> [Disciplina]
carregaDisciplinas linhas = [parseDisciplina linha | linha <- linhas]

carregaDisciplina :: Int -> [Disciplina] -> Disciplina
carregaDisciplina codigo' (d:ds)=
  if Disciplina.codigo d == codigo' then d
  else carregaDisciplina codigo' ds
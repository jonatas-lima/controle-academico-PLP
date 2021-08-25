module Main where

import Aluno (Aluno, disciplinasMatriculadas, matricula, matriculas, mediaTotal, newAluno, nome, opcoesDisponiveis, toString, toStringDisciplinas)
import Control.Monad.Trans.State.Strict (put)
import DataLoader (carregaAluno, carregaAlunos, carregaDisciplina, carregaDisciplinas, carregaProfessor, carregaProfessores, carregaUsuarios, leArquivo)
import DataSaver (salvaAluno, salvaProfessor)
import Disciplina (Disciplina)
import qualified Disciplina
import Professor (Professor (disciplinasLecionadas), matricula, matriculas, newProfessor, nome, opcoesDisponiveis, toString)
import Usuario (Usuario, autentica)

main :: IO ()
main =
  opcoes

opcoes :: IO ()
opcoes = do
  putStrLn
    ( "1) Fazer login\n"
        ++ "2) Novo usuário? Fazer cadastro\n"
    )

  opcao <- getLine

  putStrLn ""

  if opcao == "1"
    then fazerLogin
    else
      if opcao == "2"
        then fazerCadastro
        else putStrLn "Opção inválida"

fazerLogin :: IO ()
fazerLogin = do
  putStr "Digite sua matrícula: "
  matriculaUsuario <- getLine

  putStr "Digite sua senha: "
  senha <- getLine

  arquivoUsuarios <- leArquivo "./data/usuarios.csv"
  let usuariosDisponiveis = carregaUsuarios arquivoUsuarios

  let autenticacao = Usuario.autentica matriculaUsuario senha usuariosDisponiveis
  let autenticado = fst autenticacao
  let role = snd autenticacao

  if autenticado
    then tela matriculaUsuario role
    else
      putStrLn
        "Usuario ou senha invalido"

fazerCadastro :: IO ()
fazerCadastro = do
  putStr "(P)rofessor ou (A)luno > "
  opcao <- getLine

  putStr "\nDigite a matrícula: "
  matricula <- getLine

  putStr "Digite o nickname: "
  nick <- getLine

  putStr "Digite seu nome: "
  nome <- getLine

  putStr "Digite sua senha: "
  senha <- getLine

  if opcao == "P"
    then cadastraProfessor matricula nick nome senha
    else
      if opcao == "A"
        then cadastraAluno matricula nick nome senha
        else putStrLn "Opção inválida!"

cadastraProfessor :: String -> String -> String -> String -> IO ()
cadastraProfessor matricula nickname nome senha = do
  arquivoProfessores <- leArquivo "./data/professores.csv"
  let professoresCadastrados = carregaProfessores arquivoProfessores
  let matriculasCadastradas = Professor.matriculas professoresCadastrados

  if read matricula `elem` matriculasCadastradas
    then putStrLn "Professor já cadastrado!"
    else salvaProfessor professor
  where
    professor = newProfessor (read matricula) nome []

cadastraAluno :: String -> String -> String -> String -> IO ()
cadastraAluno matricula nickname nome senha = do
  arquivoAlunos <- leArquivo "./data/alunos.csv"
  let alunosCadastrados = carregaAlunos arquivoAlunos
  let matriculasCadastradas = Aluno.matriculas alunosCadastrados

  if read matricula `elem` matriculasCadastradas
    then putStrLn "Aluno já cadastrado!"
    else salvaAluno aluno
  where
    aluno = newAluno (read matricula) nome []

tela :: String -> String -> IO ()
tela matricula role
  | role == "prof" = telaProf matricula
  | role == "admin" = telaAdmin
  | role == "aluno" = telaAluno matricula
  | otherwise = putStrLn "Role invalido"

telaAdmin :: IO ()
telaAdmin = putStrLn "tela admin"

telaAluno :: String -> IO ()
telaAluno matricula' = do
  arquivoAlunos <- leArquivo "./data/alunos.csv"

  putStrLn (arquivoAlunos !! 1)

  let alunos = DataLoader.carregaAlunos arquivoAlunos
  let aluno = DataLoader.carregaAluno (read matricula') alunos

  putStrLn (opcoesAluno aluno)

header :: Int -> String -> String
header matricula nome =
  "\n--------------------------\n"
    ++ "Usuário: "
    ++ show matricula
    ++ " - "
    ++ nome

opcoesAluno :: Aluno -> String
opcoesAluno aluno =
  header (Aluno.matricula aluno) (Aluno.nome aluno)
    ++ Aluno.opcoesDisponiveis

opcoesProf :: Professor -> String
opcoesProf professor =
  header (Professor.matricula professor) (Professor.nome professor)
    ++ Professor.opcoesDisponiveis

telaProf :: String -> IO ()
telaProf matricula' = do
  arquivoProfessores <- leArquivo "./data/professores.csv"
  let professores = DataLoader.carregaProfessores arquivoProfessores
  let professor = DataLoader.carregaProfessor (read matricula') professores

  arquivoDisciplinas <- leArquivo "./data/disciplinas.csv"
  let disciplinas = DataLoader.carregaDisciplinas arquivoDisciplinas

  putStrLn (opcoesProf professor)

  putStr "Qual a opcao selecionada? "
  opcao <- getLine

  if opcao == "1"
    then putStrLn (exibeDisciplinas (Professor.disciplinasLecionadas professor) disciplinas)
    else putStrLn "Opcao inválida"

getDisciplina :: Int -> [Disciplina] -> String
getDisciplina codigoDisciplina disciplinas = do
  let disciplina = DataLoader.carregaDisciplina codigoDisciplina disciplinas
  Disciplina.exibeDisciplina disciplina ++ "\n"

exibeDisciplinas :: [Int] -> [Disciplina] -> String
exibeDisciplinas _ [] = ""
exibeDisciplinas [] _ = ""
exibeDisciplinas (c : cs) (d : ds) =
  if c == Disciplina.codigo d
    then getDisciplina c (d : ds) ++ exibeDisciplinas cs ds
    else exibeDisciplinas (c : cs) ds
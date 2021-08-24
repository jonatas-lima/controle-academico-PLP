module Main where

import Aluno (matriculas, newAluno)
import DataLoader (carregaAlunos, carregaProfessores, carregaUsuarios, leArquivo)
import DataSaver (salvaAluno, salvaProfessor)
import Professor (Professor, matriculas, newProfessor)
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

  if opcao == "1"
    then fazerLogin
    else
      if opcao == "2"
        then fazerCadastro
        else putStrLn "Opção inválida"

fazerLogin :: IO ()
fazerLogin = do
  putStr "Digite seu nome de usuário: "
  nomeDeUsuario <- getLine

  putStr "Digite sua senha: "
  senha <- getLine

  arquivoUsuarios <- leArquivo "./data/usuarios.csv"
  let usuariosDisponiveis = carregaUsuarios arquivoUsuarios

  let autenticacao = Usuario.autentica nomeDeUsuario senha usuariosDisponiveis
  let autenticado = fst autenticacao
  let role = snd autenticacao

  if autenticado
    then tela role
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
    else cadastraAluno matricula nick nome senha

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

tela :: String -> IO ()
tela role
  | role == "prof" = telaProf
  | role == "admin" = telaAdmin
  | role == "aluno" = telaAluno
  | otherwise = putStrLn "Role invalido"

telaProf :: IO ()
telaProf =
  putStrLn "Tela de Professor"

telaAdmin :: IO ()
telaAdmin =
  putStrLn "Tela de admin"

telaAluno :: IO ()
telaAluno =
  putStrLn "Tela de aluno"
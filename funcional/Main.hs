module Main where

import DataLoader (carregaUsuarios, leArquivo)
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
  putStrLn "Digite seu nome de usuário: "

  nomeDeUsuario <- getLine

  putStrLn
    "Digite sua senha: "

  senha <- getLine

  arquivoUsuarios <- DataLoader.leArquivo "./data/usuarios.csv"
  let usuariosDisponiveis = DataLoader.carregaUsuarios arquivoUsuarios

  let autenticacao = Usuario.autentica nomeDeUsuario senha usuariosDisponiveis

  putStrLn (if fst autenticacao then "sim" else "nao")
  putStrLn ("role: " ++ snd autenticacao)

fazerCadastro :: IO ()
fazerCadastro =
  putStrLn ("nada")


if autenticado then
    tela role
  else
    putStrLn "Usuario ou senha invalido"

  putStrLn (if fst autenticacao then "sim" else "nao")
  putStrLn ("role: " ++ snd autenticacao)

fazerCadastro :: IO ()
fazerCadastro =
  putStrLn ("nada")

tela :: String -> IO()
tela = do
    role =
        | role == "prof"  telaProf
        | role == "admin" = telaAdmin
        | role == "aluno" = telaAluno
        | otherwise = putStrLn "Role invalido"


telaProf :: IO()
telaProf = do
    putStrLn("Tela de Professor")

telaAdmin :: IO()
telaAdmin = do
    putStrLn("Tela de admin")

telaAluno :: IO()
telaAluno = do
    putStrLn("Tela de aluno")
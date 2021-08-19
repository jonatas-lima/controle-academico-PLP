module Main where

-- import DataLoader (DataLoader, carregaUsuarios, leArquivo)
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

  putStrLn "Digite sua senha: "
  senha <- getLine

  arquivoUsuarios <- DataLoader.leArquivo "./data/usuarios.csv"
  let usuariosDisponiveis = DataLoader.carregaUsuarios arquivoUsuarios

  let autenticacao = Usuario.autentica nomeDeUsuario senha usuariosDisponiveis
  let autenticado = fst autenticacao
  let role = snd autenticacao

  if autenticado
    then tela role
    else
      putStrLn
        "Usuario ou senha invalido"

fazerCadastro :: IO ()
fazerCadastro = putStrLn "nada"

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
module Main where

import Aluno (Aluno, disciplinasMatriculadas, matricula, matriculas, mediaTotal, newAluno, nome, opcoesDisponiveis, toString, mediaDisciplina, todasMedias, todasMediasAux)
import DataLoader (carregaAluno, carregaAlunos, carregaDisciplina, carregaDisciplinas, carregaProfessor, carregaProfessores, carregaUsuarios, leArquivo)
import DataSaver (salvaAluno, salvaProfessor)
import Professor (Professor, disciplinasLecionadas, matricula, matriculas, newProfessor, nome, opcoesDisponiveis, temDisciplina)
import Disciplina (Disciplina, codigo, exibeDisciplina, mediaAluno, notas)
import Usuario (Usuario, autentica)
import System.Console.ANSI
import Control.Concurrent

main :: IO ()
main = do
  putStrLn ("Bem-Vindo(a)!" ++ "\nPara acessar o controle, faça login:\n")
  telaLogin

telaLogin :: IO ()
telaLogin = do
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
    then do 
      putStrLn "Login realizado..."
      threadDelay (2*10^6)
      clearScreen
      tela matriculaUsuario role
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

tela :: String -> String -> IO ()
tela matricula role
  | role == "prof" = telaProf matricula
  | role == "admin" = telaAdmin
  | role == "aluno" = telaAluno matricula
  | otherwise = putStrLn "Role invalido"

telaAluno :: String -> IO ()
telaAluno matricula' = do
  arquivoAlunos <- leArquivo "./data/alunos.csv"
  let alunos = DataLoader.carregaAlunos arquivoAlunos
  let aluno = DataLoader.carregaAluno (read matricula') alunos

  arquivoDisciplinas <- leArquivo "./data/disciplinas.csv"
  let disciplinas = DataLoader.carregaDisciplinas arquivoDisciplinas

  putStrLn "\n\n--- Controle Acadêmico ---"

  putStrLn (opcoesAluno aluno)

  putStr "Qual a opcao selecionada? "
  opcao <- getLine

  putStrLn ""

  if opcao == "1"
    then do
      clearScreen
      putStrLn "-- Visualizando Disciplinas --\n"
      putStrLn ("Código\t - Disciplina\t - Média\n" ++ exibeDisciplinasAluno aluno (Aluno.disciplinasMatriculadas aluno) disciplinas)
    else
      if opcao == "2"
        then do
          clearScreen
          putStrLn "-- Realizar Matrícula --\n"
          realizarMatricula
        else
          if opcao == "3"
            then do
              clearScreen
              putStrLn "-- Visualizar Média Geral --\nCRA: "
              -- visualizarMediaGeral aluno disciplinas
            else
              if opcao == "4"
                then putStrLn "Saindo do sistema!"
                else putStrLn "Opção inválida"

  if opcao /= "4"
    then telaAluno matricula'
    else putStrLn ""

opcoesAluno :: Aluno -> String
opcoesAluno aluno =
  header (Aluno.matricula aluno) (Aluno.nome aluno) ++ Aluno.opcoesDisponiveis

getDisciplinaAluno :: Aluno -> Int -> [Disciplina] -> String
getDisciplinaAluno aluno codigoDisciplina disciplinas = do
  let disciplina = DataLoader.carregaDisciplina codigoDisciplina disciplinas
  Disciplina.exibeDisciplina disciplina ++ "\t\t - " ++ show (Aluno.mediaDisciplina aluno disciplina) ++ "\n"

exibeDisciplinasAluno :: Aluno -> [Int] -> [Disciplina] -> String
exibeDisciplinasAluno aluno _ [] = ""
exibeDisciplinasAluno aluno [] _ = ""
exibeDisciplinasAluno aluno (c : cs) (d : ds) =
  if c == Disciplina.codigo d
    then getDisciplinaAluno aluno c (d : ds) ++ exibeDisciplinasAluno aluno cs ds
    else exibeDisciplinasAluno aluno (c : cs) ds

realizarMatricula :: IO ()
realizarMatricula =
  putStrLn "Realizar matrícula..."

-- visualizarMediaGeral :: Aluno -> [Disciplina] -> IO ()
-- visualizarMediaGeral aluno (d:ds) =
  -- putStr (show Disciplina.mediaAluno (Aluno.matricula aluno) (Disciplina.notas d))

header :: Int -> String -> String
header matricula nome =
  "\n--------------------------\n"
    ++ "Usuário: "
    ++ show matricula
    ++ " - "
    ++ nome

telaProf :: String -> IO ()
telaProf matricula' = do
  arquivoProfessores <- leArquivo "./data/professores.csv"
  let professores = DataLoader.carregaProfessores arquivoProfessores
  let professor = DataLoader.carregaProfessor (read matricula') professores

  arquivoDisciplinas <- leArquivo "./data/disciplinas.csv"
  let disciplinas = DataLoader.carregaDisciplinas arquivoDisciplinas
  let disciplinasDoProf = Professor.disciplinasLecionadas professor

  putStrLn (opcoesProfessor professor)

  putStr "Qual a opcao selecionada? "
  opcao <- getLine

  if opcao == "1"
    then putStrLn ("Código\t - Disciplina\n" ++ exibeDisciplinasProfessor (disciplinasDoProf) disciplinas)
    else if opcao == "2"
       then do
         putStr ("Código da disciplina: ")
         codigo <- getLine
         registraAula disciplinasDoProf codigo
         else if opcao == "3"
           then putStrLn "Cadastra prova"
           else if opcao == "4" then do
              clearScreen
              saiDoSistema matricula' "prof"
              else if opcao == "5" then do
                clearScreen
                logoff matricula' "prof"
                else putStrLn "Opção inválida"
  if opcao /= "4" && opcao /= "5"
  then do
    putStrLn "Pressione enter para continuar..."
    x <- getLine
    clearScreen
    telaProf matricula'
  else putStrLn ""

opcoesProfessor :: Professor -> String
opcoesProfessor professor =
  header (Professor.matricula professor) (Professor.nome professor) ++ Professor.opcoesDisponiveis

getDisciplina :: Int -> [Disciplina] -> String
getDisciplina codigoDisciplina disciplinas = do
  let disciplina = DataLoader.carregaDisciplina codigoDisciplina disciplinas
  Disciplina.exibeDisciplina disciplina ++ "\n"

exibeDisciplinasProfessor :: [Int] -> [Disciplina] -> String
exibeDisciplinasProfessor _ [] = ""
exibeDisciplinasProfessor [] _ = ""
exibeDisciplinasProfessor (c : cs) (d : ds) =
  if c == Disciplina.codigo d
    then getDisciplina c (d : ds) ++ exibeDisciplinasProfessor cs ds
    else exibeDisciplinasProfessor (c : cs) ds

registraAula :: [Int] -> String -> IO ()
registraAula x codigo = do
  if (Professor.temDisciplina (read codigo :: Int)  x)
    then putStrLn "Registrado"
    else putStrLn "Disciplina inválida"
  

telaAdmin :: IO ()
telaAdmin =
  putStrLn "Tela de admin"

saiDoSistema :: String -> String -> IO ()
saiDoSistema matricula' role' = do
  putStr "Deseja sair do sistema? (s/n) "
  opcao <- getLine
  if opcao == "s" then do
    putStrLn "Saindo..."
    threadDelay (10^6)
    else if opcao == "n" then do
      clearScreen
      tela matricula' role'
      else do
        putStrLn "Opção inválida"
        putStrLn "Pressione enter para continuar..."
        x <- getLine
        clearScreen
        saiDoSistema matricula' role'


logoff :: String -> String -> IO ()
logoff matricula' role' =  do
  putStr "Deseja realizar o logoff? (s/n)"
  opcao <- getLine
  if opcao == "s" then do
     putStrLn "Logoff realizado."
     clearScreen
     main
  else if opcao == "n" then do
    clearScreen
    tela matricula' role'
    else do 
      putStrLn "Opção inválida"
      putStrLn "Pressione enter para continuar..."
      x <- getLine
      clearScreen
      logoff matricula' role'
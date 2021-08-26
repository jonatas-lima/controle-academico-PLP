module Main where

import Aluno (Aluno)
import qualified Aluno
import Control.Concurrent (threadDelay)
import qualified Controle
import qualified DataLoader
import qualified DataSaver
import Disciplina (Disciplina)
import qualified Disciplina
import Professor (Professor)
import qualified Professor
import System.Console.ANSI (clearScreen)
import qualified Usuario

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

  arquivoUsuarios <- DataLoader.leArquivo "./data/usuarios.csv"
  let usuariosDisponiveis = DataLoader.carregaUsuarios arquivoUsuarios

  let autenticacao = Usuario.autentica matriculaUsuario senha usuariosDisponiveis
  let autenticado = fst autenticacao
  let role = snd autenticacao

  if autenticado
    then do
      putStrLn "Login realizado..."
      threadDelay (2 * 10 ^ 6)
      clearScreen
      tela matriculaUsuario role
    else
      putStrLn
        "Usuario ou senha invalido"

tela :: String -> String -> IO ()
tela matricula role
  | role == "prof" = telaProf matricula
  | role == "admin" = telaAdmin
  | role == "aluno" = telaAluno matricula
  | otherwise = putStrLn "Role invalido"

telaAluno :: String -> IO ()
telaAluno matricula' = do
  arquivoAlunos <- DataLoader.leArquivo "./data/alunos.csv"
  let alunos = DataLoader.carregaAlunos arquivoAlunos
  let aluno = DataLoader.carregaAluno (read matricula') alunos

  arquivoDisciplinas <- DataLoader.leArquivo "./data/disciplinas.csv"
  let disciplinas = DataLoader.carregaDisciplinas arquivoDisciplinas

  putStrLn "\n\n--- Controle Acadêmico ---"

  putStrLn (opcoesAluno aluno)

  putStr "Qual a opcao selecionada?\n> "
  opcao <- getLine
  putStrLn ""

  painelAluno opcao aluno disciplinas

painelAluno :: String -> Aluno -> [Disciplina] -> IO ()
painelAluno opcao aluno disciplinas
  | opcao == "1" = do
    clearScreen
    putStrLn "-- Visualizando Disciplinas --\n"
    putStrLn ("Código\t - Disciplina\t - Média\n" ++ exibeDisciplinasAluno aluno (Aluno.disciplinasMatriculadas aluno) disciplinas)
  | opcao == "2" = do
    clearScreen
    putStrLn "-- Realizar Matrícula --\n"
    realizarMatricula
  | opcao == "3" = do
    clearScreen
    putStrLn "-- Visualizar Média Geral --\nCRA: "
  | opcao == "4" = do
    putStrLn "Saindo do sistema!"
  | otherwise = do
    putStrLn "Opção inválida"

-- telaAluno matricula'

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
  arquivoProfessores <- DataLoader.leArquivo "./data/professores.csv"
  let professores = DataLoader.carregaProfessores arquivoProfessores
  let professor = DataLoader.carregaProfessor (read matricula') professores

  arquivoDisciplinas <- DataLoader.leArquivo "./data/disciplinas.csv"
  let disciplinas = DataLoader.carregaDisciplinas arquivoDisciplinas
  let disciplinasDoProf = Professor.disciplinasLecionadas professor

  putStrLn (opcoesProfessor professor)

  putStr "Qual a opcao selecionada? "
  opcao <- getLine

  if opcao == "1"
    then putStrLn ("Código\t - Disciplina\n" ++ exibeDisciplinasProfessor disciplinasDoProf disciplinas)
    else
      if opcao == "2"
        then do
          putStr "Código da disciplina: "
          codigo <- getLine
          registraAula disciplinasDoProf codigo
        else
          if opcao == "3"
            then putStrLn "Cadastra prova"
            else
              if opcao == "4"
                then do
                  clearScreen
                  saiDoSistema matricula' "prof"
                else
                  if opcao == "5"
                    then do
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
registraAula x codigo =
  if Professor.temDisciplina (read codigo) x
    then putStrLn "Registrado"
    else putStrLn "Disciplina inválida"

telaAdmin :: IO ()
telaAdmin = do
  putStr (opcoesAdmin ++ "> ")
  opcao <- getLine
  painelAdm opcao

painelAdm :: String -> IO ()
painelAdm opcao
  | opcao == "1" = telaCadastro "professor"
  | opcao == "2" = telaCadastro "aluno"
  | opcao == "3" = telaAssociacaoProfessor
  | opcao == "4" = listaAlunosSemMatriculas
  | opcao == "5" = listaProfessoresSemMatriculas
  | opcao == "6" = exibeDisciplinaMaiorMedia
  | opcao == "7" = exibeDisciplinaMenorMedia
  | opcao == "S" = saiDoSistema "" ""
  | otherwise = putStrLn "opcao invalida"

telaAssociacaoProfessor :: IO ()
telaAssociacaoProfessor = putStrLn "associa professor"

listaAlunosSemMatriculas :: IO ()
listaAlunosSemMatriculas = putStrLn "lista alunos sem matriculas"

listaProfessoresSemMatriculas :: IO ()
listaProfessoresSemMatriculas = putStrLn "lista professores sem matriculas"

exibeDisciplinaMaiorMedia :: IO ()
exibeDisciplinaMaiorMedia = putStrLn "exibe disciplina com maior media geral"

exibeDisciplinaMenorMedia :: IO ()
exibeDisciplinaMenorMedia = putStrLn "exibe disciplina com menor media geral"

opcoesAdmin :: String
opcoesAdmin =
  header 0 "admin"
    ++ "\n\n1) Cadastrar professor\n"
    ++ "2) Cadastrar aluno\n"
    ++ "3) Associar professor à disciplina\n"
    ++ "4) Listar alunos sem matrículas\n"
    ++ "5) Listar professores sem disciplinas\n"
    ++ "6) Disciplina com a maior média\n"
    ++ "7) Disciplina com a menor média\n"
    ++ "(S)air do sistema\n"
    ++ "Fazer (l)ogoff\n"

telaCadastro :: String -> IO ()
telaCadastro opcao = do
  putStr "\nDigite a matrícula: \n> "
  matricula <- getLine

  putStr "Digite seu nome: \n> "
  nome <- getLine

  putStr "Digite sua senha: \n> "
  senha <- getLine

  if opcao == "professor"
    then Controle.cadastraProfessor (read matricula) nome senha
    else Controle.cadastraAluno (read matricula) nome senha

saiDoSistema :: String -> String -> IO ()
saiDoSistema matricula' role' = do
  putStr "Deseja sair do sistema? (s/n) "
  opcao <- getLine
  if opcao == "s"
    then do
      putStrLn "Saindo..."
      threadDelay (10 ^ 6)
    else
      if opcao == "n"
        then do
          clearScreen
          tela matricula' role'
        else do
          putStrLn "Opção inválida"
          putStrLn "Pressione enter para continuar..."
          x <- getLine
          clearScreen
          saiDoSistema matricula' role'

logoff :: String -> String -> IO ()
logoff matricula' role' = do
  putStr "Deseja realizar o logoff? (s/n)"
  opcao <- getLine
  if opcao == "s"
    then do
      putStrLn "Logoff realizado."
      clearScreen
      main
    else
      if opcao == "n"
        then do
          clearScreen
          tela matricula' role'
        else do
          putStrLn "Opção inválida"
          putStrLn "Pressione enter para continuar..."
          x <- getLine
          clearScreen
          logoff matricula' role'
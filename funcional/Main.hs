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
import Text.Printf
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
      putStrLn "\nLogin realizado..."
      threadDelay (10 ^ 6)
      clearScreen
      tela matriculaUsuario role
    else do
      putStr "\nUsuario ou senha invalido! Deseja tentar novamente? (s/n) "
      opcao <- getLine
      if opcao == "s"
        then do
          clearScreen
          telaLogin
        else
          if opcao == "n"
            then do
              putStr "\nSaindo..."
              threadDelay (10 ^ 6)
            else do
              putStr "\nOpção inválida. Saindo do sistema por segurança."
              threadDelay (10 ^ 6)

tela :: String -> String -> IO ()
tela matricula role
  | role == "prof" = telaProf matricula
  | role == "admin" = telaAdmin
  | role == "aluno" = telaAluno matricula
  | otherwise = putStrLn "Role invalido"

codTodasDisciplinas :: [Disciplina] -> [Int]
codTodasDisciplinas disciplinas = [Disciplina.codigo disciplina | disciplina <- disciplinas]

codFilter :: [Int] -> [Int] -> [Int]
codFilter codD codA = filter (\cod -> not $ cod `elem` codA) codD

disciplinasFilter :: [Disciplina] -> [Int] -> [Disciplina]
disciplinasFilter disciplinas cods = filter (\cod -> Disciplina.codigo cod `elem` cods) disciplinas

telaAluno :: String -> IO ()
telaAluno matricula' = do
  arquivoAlunos <- DataLoader.leArquivo "./data/alunos.csv"
  let alunos = DataLoader.carregaAlunos arquivoAlunos
  let aluno = DataLoader.carregaAluno (read matricula') alunos

  arquivoDisciplinas <- DataLoader.leArquivo "./data/disciplinas.csv"
  let disciplinas = DataLoader.carregaDisciplinas arquivoDisciplinas

  -- variaveis auxiliares
  let codMatriculadas = Aluno.disciplinasMatriculadas aluno
  let discMatriculadas = disciplinasFilter disciplinas codMatriculadas
  let codNaoMatriculadas = codFilter (codTodasDisciplinas disciplinas) codMatriculadas
  let discNaoMatriculadas = disciplinasFilter disciplinas codNaoMatriculadas

  putStrLn "\n\n--- Controle Acadêmico ---"

  putStrLn (opcoesAluno aluno)

  putStr "Qual a opcao selecionada?\n> "
  opcao <- getLine
  putStrLn ""

  if opcao == "1"
    then do
      putStrLn ("Código\t - Disciplina\t - Média\n" ++ exibeDisciplinasAluno aluno codMatriculadas disciplinas)
    else
      if opcao == "2"
        then verificaRealizarMatricula aluno discNaoMatriculadas codNaoMatriculadas
        else
          if opcao == "3"
            then matricula discMatriculadas codMatriculadas "Matricula cancelada...\n"
            else
              if opcao == "4"
                then do
                  putStr "CRA: "
                  printf "%.2f" (Aluno.mediaTotal aluno disciplinas)
                  putStrLn "\n"
                else
                  if opcao == "5"
                    then do
                      clearScreen
                      saiDoSistema matricula' "aluno"
                    else
                      if opcao == "6"
                        then do
                          clearScreen
                          logoff matricula' "aluno"
                        else putStrLn "Opção inválida"

  if opcao /= "5" && opcao /= "6"
    then do
      putStr "Pressione enter para continuar..."
      x <- getLine
      clearScreen
      telaAluno matricula'
    else putStrLn ""

opcoesAluno :: Aluno -> String
opcoesAluno aluno =
  header (Aluno.matricula aluno) (Aluno.nome aluno) ++ Aluno.opcoesDisponiveis

getDisciplinaAluno :: Aluno -> Int -> [Disciplina] -> String
getDisciplinaAluno aluno codigoDisciplina disciplinas = do
  let disciplina = DataLoader.carregaDisciplina codigoDisciplina disciplinas
  Disciplina.exibeDisciplina disciplina ++ "\t - " ++ printf "%.2f" (Aluno.mediaDisciplina aluno disciplina) ++ "\n"

exibeDisciplinasAluno :: Aluno -> [Int] -> [Disciplina] -> String
exibeDisciplinasAluno aluno _ [] = ""
exibeDisciplinasAluno aluno [] _ = ""
exibeDisciplinasAluno aluno (c : cs) (d : ds) =
  if c == Disciplina.codigo d
    then getDisciplinaAluno aluno c (d : ds) ++ exibeDisciplinasAluno aluno cs ds
    else exibeDisciplinasAluno aluno (c : cs) ds

-- verificar se o aluno pode realizar matricula
verificaRealizarMatricula :: Aluno -> [Disciplina] -> [Int] -> IO ()
verificaRealizarMatricula aluno disciplinas codD = do
  if Aluno.numDisciplinasMatriculadas aluno == 4
    then putStrLn ("O aluno [" ++ printf "%.d" (Aluno.matricula aluno) ++ "] já possui 4 disciplinas matriculadas!\n")
    else matricula disciplinas codD "Matricula realizada...\n"

matricula :: [Disciplina] -> [Int] -> String -> IO ()
matricula disciplinas codD stringMatricula = do
  putStrLn ("Código\t - Disciplina\n" ++ exibeDisciplinas disciplinas)

  putStr "Entre com o código da cadeira: "

  codigo <- getLine

  putStrLn ""

  -- verificar codigo da cadeira --
  if read codigo `elem` codD
    then putStrLn stringMatricula -- matricular ou cancelar matricula do aluno na cadeira
    else putStrLn "Código Inválido\n"

exibeDisciplinas :: [Disciplina] -> String
exibeDisciplinas [] = ""
exibeDisciplinas (d : ds) =
  Disciplina.exibeDisciplina d ++ "\n" ++ exibeDisciplinas ds

-- mediaGeralAluno :: Aluno -> [Disciplina] -> IO ()
-- mediaGeralAluno aluno disciplinas = do
--   print (Aluno.todasMedias aluno disciplinas)

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
  let codDisciplinasDoProf = Professor.disciplinasLecionadas professor
  let disciplinasDoProf = disciplinasFilter disciplinas codDisciplinasDoProf

  putStrLn (opcoesProfessor professor)

  putStr "Qual a opcao selecionada? "
  opcao <- getLine

  if opcao == "1"
    then putStrLn ("\nCódigo\t - Disciplina\n" ++ exibeDisciplinas disciplinasDoProf)
    else
      if opcao == "2"
        then do
          putStrLn "Essas são as disciplinas que você leciona:"
          putStrLn ("\nCódigo\t - Disciplina\n" ++ exibeDisciplinas disciplinasDoProf)
          putStr "Código da disciplina para qual você deseja cadastrar aula: "
          codigo <- getLine
          registraAula professor $ read codigo
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
      putStr "Pressione enter para continuar..."
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

registraAula :: Professor -> Int -> IO ()
registraAula professor codigoDisciplina =
  if Professor.temDisciplina professor codigoDisciplina
    then putStrLn "Registrado"
    else putStrLn "Disciplina inválida"

telaAdmin :: IO ()
telaAdmin = do
  putStr (opcoesAdmin ++ "> ")
  opcao <- getLine
  painelAdm opcao

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
telaAssociacaoProfessor = do
  clearScreen

  arquivoProfessores <- DataLoader.leArquivo "./data/professores.csv"
  arquivoDisciplinas <- DataLoader.leArquivo "./data/disciplinas.csv"
  let professores = DataLoader.carregaProfessores arquivoProfessores
  let disciplinas = DataLoader.carregaDisciplinas arquivoDisciplinas

  putStrLn "Professores disponíveis:"
  putStr $ Controle.listaProfessoresDisponiveis professores

  putStr "Matrícula do professor a ser associado > "
  matricula <- getLine
  clearScreen

  let professor = DataLoader.carregaProfessor (read matricula) professores
  putStrLn "Disciplinas disponíveis"
  putStr $ Controle.listaDisciplinasDisponiveisParaAssociacao professor disciplinas

  putStr "Código da disciplina a ser associada > "
  codigo <- getLine
  clearScreen

  let disciplina = DataLoader.carregaDisciplina (read codigo) disciplinas

  Controle.associaProfessorDisciplina professor disciplina disciplinas

listaAlunosSemMatriculas :: IO ()
listaAlunosSemMatriculas = do
  showData "Alunos sem matrículas:" "./data/alunos.csv" Controle.listaAlunosSemMatriculas DataLoader.carregaAlunos

listaProfessoresSemMatriculas :: IO ()
listaProfessoresSemMatriculas = do
  showData "Professores sem disciplinas:" "./data/professores.csv" Controle.listaProfessoresSemMatriculas DataLoader.carregaProfessores

exibeDisciplinaMaiorMedia :: IO ()
exibeDisciplinaMaiorMedia = do
  showData "Disciplina com maior média:" "./data/disciplinas.csv" Controle.exibeDisciplinaComMaiorMedia DataLoader.carregaDisciplinas

exibeDisciplinaMenorMedia :: IO ()
exibeDisciplinaMenorMedia = do
  showData "Disciplina com menor média:" "./data/disciplinas.csv" Controle.exibeDisciplinaComMenorMedia DataLoader.carregaDisciplinas

showData :: String -> String -> ([t] -> String) -> ([String] -> [t]) -> IO ()
showData message filePath display loadAll = do
  clearScreen
  putStrLn message
  entityFile <- DataLoader.leArquivo filePath
  let entities = loadAll entityFile

  putStrLn $ display entities

saiDoSistema :: String -> String -> IO ()
saiDoSistema matricula' role' = do
  putStr "Deseja sair do sistema? (s/n) "
  opcao <- getLine
  if opcao == "s"
    then do
      putStr "\nSaindo..."
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
  putStr "Deseja realizar o logoff? (s/n) "
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
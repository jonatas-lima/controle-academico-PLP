module DataSaver where

import Aluno (Aluno (registration), toString)
import Data.List.Split (splitOn)
import Data.Text (replace, unpack)
import qualified DataLoader
import Disciplina (Disciplina, toString)
import Professor (Professor (Professor, registration), toString)
import qualified System.IO.Strict as Strict
import Usuario (Usuario (Usuario), toString)

append :: String -> String -> IO ()
append line arq = appendFile arq (line ++ "\n")

saveUser :: Usuario -> IO ()
saveUser user = append (Usuario.toString user) "./data/usuarios.csv"

saveProfessor :: Professor -> String -> IO ()
saveProfessor professor password = do
  append (Professor.toString professor) "./data/professores.csv"
  append (show (Professor.registration professor) ++ "," ++ password ++ "," ++ "prof") "./data/usuarios.csv"

saveStudent :: Aluno -> String -> IO ()
saveStudent student password = do
  append (Aluno.toString student) "./data/alunos.csv"
  append (show (Aluno.registration student) ++ "," ++ password ++ "," ++ "aluno") "./data/usuarios.csv"

saveSubject :: Disciplina -> IO ()
saveSubject subject = append (Disciplina.toString subject) "./data/disciplinas.csv"

updateProfessor :: Int -> Professor -> IO ()
updateProfessor registration =
  updateEntity registration "./data/professores.csv" Professor.toString DataLoader.loadProfessors DataLoader.loadProfessor

updateStudent :: Int -> Aluno -> IO ()
updateStudent registration =
  updateEntity registration "./data/alunos.csv" Aluno.toString DataLoader.loadStudents DataLoader.loadStudent

updateSubject :: Int -> Disciplina -> IO ()
updateSubject codigo =
  updateEntity codigo "./data/disciplinas.csv" Disciplina.toString DataLoader.loadSubjects DataLoader.loadSubject

updateEntity :: Int -> String -> (t -> String) -> ([String] -> [t]) -> (Int -> [t] -> t) -> t -> IO ()
updateEntity registration filePath toString loadAll loadOne entity = do
  arq <- DataLoader.readArq filePath

  let entitys = loadAll arq
  let oldEntity = toString $ loadOne registration entitys
  let updatedArq = updateEntityData (toString entity) oldEntity arq

  writeFile filePath $ parseLines updatedArq

parseLines :: [String] -> String
parseLines [] = ""
parseLines (l : ls) = l ++ "\n" ++ parseLines ls

updateEntityData :: String -> String -> [String] -> [String]
updateEntityData _ _ [] = []
updateEntityData updatedEntity outdatedEntity (l : ls) =
  if l == outdatedEntity
    then updatedEntity : updateEntityData updatedEntity outdatedEntity ls
    else l : updateEntityData updatedEntity outdatedEntity ls

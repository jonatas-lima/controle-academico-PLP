module DataSaver where

import Student (Student (registration), toString)
import Data.List.Split (splitOn)
import Data.Text (replace, unpack)
import qualified DataLoader
import Subject (Subject, toString)
import Professor (Professor (Professor, registration), toString)
import qualified System.IO.Strict as Strict
import User (User (User), toString)

append :: String -> String -> IO ()
append line arq = appendFile arq (line ++ "\n")

saveUser :: User -> IO ()
saveUser user = append (User.toString user) "./data/usuarios.csv"

saveProfessor :: Professor -> String -> IO ()
saveProfessor professor password = do
  append (Professor.toString professor) "./data/professores.csv"
  append (show (Professor.registration professor) ++ "," ++ password ++ "," ++ "prof") "./data/usuarios.csv"

saveStudent :: Student -> String -> IO ()
saveStudent student password = do
  append (Student.toString student) "./data/alunos.csv"
  append (show (Student.registration student) ++ "," ++ password ++ "," ++ "aluno") "./data/usuarios.csv"

saveSubject :: Subject -> IO ()
saveSubject subject = append (Subject.toString subject) "./data/disciplinas.csv"

updateProfessor :: Int -> Professor -> IO ()
updateProfessor registration =
  updateEntity registration "./data/professores.csv" Professor.toString DataLoader.loadProfessors DataLoader.loadProfessor

updateStudent :: Int -> Student -> IO ()
updateStudent registration =
  updateEntity registration "./data/alunos.csv" Student.toString DataLoader.loadStudents DataLoader.loadStudent

updateSubject :: Int -> Subject -> IO ()
updateSubject codigo =
  updateEntity codigo "./data/disciplinas.csv" Subject.toString DataLoader.loadSubjects DataLoader.loadSubject

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

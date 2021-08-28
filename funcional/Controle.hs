module Controle where

import Aluno (Aluno)
import qualified Aluno
import DataLoader
import qualified DataLoader
import DataSaver (saveProfessor, saveStudent, updateProfessor)
import Disciplina (Disciplina)
import qualified Disciplina
import Professor (Professor)
import qualified Professor

registerProfessor :: Int -> String -> String -> IO ()
registerProfessor professorId professorName password = do
  arqProfessors <- readArq "./data/professores.csv"
  let registeredProfessors = loadProfessors arqProfessors
  let registeredEnrollments = Professor.registrations registeredProfessors

  if isThereRegristration professorId registeredEnrollments
    then putStrLn "Professor já cadastrado!"
    else do
      saveProfessor professor password
      putStrLn "Professor cadastrado!"
  where
    professor = Professor.newProfessor professorId professorName []

registerStudent :: Int -> String -> String -> IO ()
registerStudent studentId studentName password = do
  arqStudents <- readArq "./data/alunos.csv"
  let registeredStudents = loadStudents arqStudents
  let registeredEnrollments = Aluno.registrations registeredStudents

  if isThereRegristration studentId registeredEnrollments
    then putStrLn "Professor já cadastrado!"
    else do
      saveStudent aluno password
      putStrLn "Professor cadastrado!"
  where
    aluno = Aluno.newStudent studentId studentName []

isThereRegristration :: Int -> [Int] -> Bool
isThereRegristration id ids = id `elem` ids

studentRegistration :: Aluno -> Disciplina -> Bool
studentRegistration student subjects = True

listStudentsWithoutRegistration :: [Aluno] -> String
listStudentsWithoutRegistration [] = ""
listStudentsWithoutRegistration (a : as) =
  if null $ Aluno.enrolledSubjects a
    then listFormatStudent a ++ "\n" ++ listStudentsWithoutRegistration as
    else listStudentsWithoutRegistration as

listFormatStudent :: Aluno -> String
listFormatStudent student = show (Aluno.registration student) ++ "\t - \t" ++ Aluno.name student

listProfessorsWithoutRegistration :: [Professor] -> String
listProfessorsWithoutRegistration [] = ""
listProfessorsWithoutRegistration (p : ps) =
  if null $ Professor.subjects p
    then formatListProfessor p ++ "\n" ++ listProfessorsWithoutRegistration ps
    else listProfessorsWithoutRegistration ps

listAvailableProfessors :: [Professor] -> String
listAvailableProfessors [] = ""
listAvailableProfessors (p : ps) =
  if Professor.numberOfSubjects p < 3
    then formatListProfessor p ++ "\n" ++ listAvailableProfessors ps
    else listAvailableProfessors ps

listSubjectsAvailableForAssociation :: Professor -> [Disciplina] -> String
listSubjectsAvailableForAssociation professor subjects =
  formatListSubjects $ DataLoader.loadSubjectsByCode codesAvailableSubjects subjects
  where
    codesSubjects = map Disciplina.code subjects
    subjectsTaught = Professor.subjects professor
    codesAvailableSubjects = filter (`notElem` subjectsTaught) codesSubjects

formatListSubjects :: [Disciplina] -> String
formatListSubjects [] = ""
formatListSubjects (d : ds) = formatListSubject d ++ "\n" ++ formatListSubjects ds

formatListSubject :: Disciplina -> String
formatListSubject subjects = show (Disciplina.code subjects) ++ "\t - \t" ++ Disciplina.name subjects

formatListSubjectAverage :: Disciplina -> String
formatListSubjectAverage subject =
  formatListSubject subject ++ "\t - \t" ++ show media
  where
    media = Disciplina.subjectAverage subject

formatListProfessor :: Professor -> String
formatListProfessor professor = show (Professor.registration professor) ++ "\t - \t" ++ Professor.name professor

associateProfessorSubject :: Professor -> Disciplina -> [Disciplina] -> IO ()
associateProfessorSubject professor subject subjects =
  if notElem (Disciplina.code subject) codesSubjects || Professor.hasSubject professor (Disciplina.code subject)
    then putStrLn "Erro ao associar professor à disciplina"
    else do
      DataSaver.updateProfessor (Professor.registration professor) updatedProfessor
      putStrLn "Disciplina associada!"
  where
    codesSubjects = map Disciplina.code subjects
    subjectsTaught = Professor.subjects professor
    updatedProfessor = Professor.newProfessor (Professor.registration professor) (Professor.name professor) (Disciplina.code subject : Professor.subjects professor)

enrolledSubjects :: Aluno -> [Disciplina] -> [Disciplina]
enrolledSubjects student subjects = [DataLoader.loadSubject c subjects | c <- Aluno.enrolledSubjects student]

showsSubjectWithHigherAverage :: [Disciplina] -> String
showsSubjectWithHigherAverage subjects =
  formatListSubjectAverage $ subjectWithHigherAverage subjects

subjectWithHigherAverage :: [Disciplina] -> Disciplina
subjectWithHigherAverage subjects = do
  let subjectId = subjectWithHighestAverageCode (subjectsAverage subjects)
  DataLoader.loadSubject subjectId subjects

showsSubjectWithLowestAverage :: [Disciplina] -> String
showsSubjectWithLowestAverage subjects =
  formatListSubjectAverage $ subjectWithLowestAverage subjects

subjectWithLowestAverage :: [Disciplina] -> Disciplina
subjectWithLowestAverage subjects = do
  let subjectId = subjectWithLowestAverageCode (subjectsAverage subjects)
  DataLoader.loadSubject subjectId subjects

subjectsAverage :: [Disciplina] -> [(Int, Double)]
subjectsAverage subjects = [(Disciplina.code d, Disciplina.subjectAverage d) | d <- subjects]

subjectWithHighestAverageCode :: [(Int, Double)] -> Int
subjectWithHighestAverageCode [] = -1
subjectWithHighestAverageCode (d : ds) =
  if snd d == highestGrade then fst d else subjectWithHighestAverageCode ds
  where
    grades = [snd m | m <- d : ds]
    highestGrade = maximum grades

subjectWithLowestAverageCode :: [(Int, Double)] -> Int
subjectWithLowestAverageCode [] = -1
subjectWithLowestAverageCode (d : ds) =
  if snd d == lowestGrade then fst d else subjectWithLowestAverageCode ds
  where
    grades = [snd m | m <- d : ds]
    lowestGrade = minimum grades
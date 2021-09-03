module User where

data User = User
  { name :: String,
    password :: String,
    role :: String
  }

newUser :: String -> String -> String -> User
newUser = User

-- / Verifica se um usuário e senha está cadastrado no sistema e retorna (Se está autenticado, seu 'role')
authenticates :: String -> String -> [User] -> (Bool, String)
authenticates user password availableUsers =
  (usernameAndPassword `elem` usersAndPasswords, role)
  where
    usersAndPasswords = usernamesAndPasswords availableUsers
    usernameAndPassword = (user, password)
    role = findRole user password availableUsers

-- / Dada uma lista de usuários, retorna uma tupla, na qual o primeiro elemento é o nome de usuário e o segundo elemento é a senha
usernamesAndPasswords :: [User] -> [(String, String)]
usernamesAndPasswords = map (\u -> (name u, password u))

-- / Dado o nome de usuário, a senha e uma lista de usuários, retorna o cargo desse usuário
findRole :: String -> String -> [User] -> String
findRole _ _ [] = ""
findRole name' password' (u : us) = if name' == name u && password' == password u then role u else findRole name' password' us

-- / Formato de salvamento em arquivos
toString :: User -> String
toString user =
  name' ++ "," ++ password' ++ "," ++ role'
  where
    name' = name user
    password' = password user
    role' = role user
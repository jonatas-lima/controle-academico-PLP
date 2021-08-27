module Usuario where

data Usuario = Usuario
  { name :: String,
    password :: String,
    role :: String
  }

newUser :: String -> String -> String -> Usuario
newUser = Usuario

-- / Verifica se um usuário e senha está cadastrado no sistema e retorna (Se está autenticado, seu 'role')
authenticates :: String -> String -> [Usuario] -> (Bool, String)
authenticates user password availableUsers =
  (userPassword `elem` usersAndPasswords, role)
  where
    usersAndPasswords = usersPasswords availableUsers
    userPassword = (user, password)
    role = findRole user password availableUsers

usersPasswords :: [Usuario] -> [(String, String)]
usersPasswords = map (\u -> (name u, password u))

findRole :: String -> String -> [Usuario] -> String
findRole _ _ [] = ""
findRole name' password' (u : us) = if name' == name u && password' == password u then role u else findRole name' password' us

toString :: Usuario -> String
toString user =
  name' ++ "," ++ password' ++ "," ++ role'
  where
    name' = name user
    password' = password user
    role' = role user
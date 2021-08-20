module Usuario where

data Usuario = Usuario
  { nickname :: String,
    senha :: String,
    role :: String
  }

newUsuario :: String -> String -> String -> Usuario
newUsuario nickname' senha' role' =
  Usuario
    { nickname = nickname',
      senha = senha',
      role = role'
    }

-- / Verifica se um usuário e senha está cadastrado no sistema e retorna (Se está autenticado, seu 'role')
autentica :: String -> String -> [Usuario] -> (Bool, String)
autentica usuario senha usuariosDisponiveis =
  (parUsuarioSenha `elem` usuariosESenhas, role)
  where
    usuariosESenhas = usuariosSenhas usuariosDisponiveis
    parUsuarioSenha = (usuario, senha)
    role = findRole usuario senha usuariosDisponiveis

usuariosSenhas :: [Usuario] -> [(String, String)]
usuariosSenhas = map (\u -> (nickname u, senha u))

findRole :: String -> String -> [Usuario] -> String
findRole _ _ [] = ""
findRole nick password (u : us) = if nick == nickname u && password == senha u then role u else findRole nick password us

toString :: Usuario -> String
toString usuario =
  nickname' ++ "," ++ senha' ++ "," ++ role'
  where
    nickname' = nickname usuario
    senha' = senha usuario
    role' = role usuario
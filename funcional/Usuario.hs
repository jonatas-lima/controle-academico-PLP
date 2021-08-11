module Usuario where

data Usuario = Usuario
  { nickname :: String,
    senha :: String,
    role :: String
  }

autentica :: String -> String -> [Usuario] -> (Bool, String)
autentica usuario pwd usuariosDisponiveis =
  ((usuario, pwd) `elem` usuariosSenhas usuariosDisponiveis, findRole usuario pwd usuariosDisponiveis)

usuariosSenhas :: [Usuario] -> [(String, String)]
usuariosSenhas = map (\u -> (nickname u, senha u))

findRole :: String -> String -> [Usuario] -> String
findRole _ _ [] = ""
findRole nick pwd (u : us) = if nick == nickname u && pwd == senha u then role u else findRole nick pwd us

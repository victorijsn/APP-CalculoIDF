# Configurando git.hub

library(usethis)

# Criar PAT (chave de autenticação do GIT, caso tenha expirado)
# usethis::create_github_token()

use_git_config(user.name = "Victor Toscano", user.email = "victor.ijsn@gmail.com")

gitcreds::gitcreds_set()

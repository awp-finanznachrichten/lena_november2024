git2r::config(user.name = "awp-finanznachrichten",user.email = "sw@awp.ch")
token <- read.csv(WD_GITHUB_TOKEN,header=FALSE)[1,1]
git2r::cred_token(token)
gitadd()
gitcommit()
gitpush()

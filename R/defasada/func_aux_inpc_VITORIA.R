coletando_inpc <- function(){
  library(magrittr)
  library(data.table)
  
  # coletando base de dados
  # dados <- read.csv("https://sidra.ibge.gov.br/geratabela?format=us.csv&name=tabela1736.csv&terr=N&rank=-&query=t/1736/n1/all/v/44/p/all/d/v44%202/l/t%2Bv,,p", 
  #                   skip = 5, 
  #                   header = F,
  #                   encoding = "UTF-8",
  #                   col.names = c("meses", "inpc"))
  dados <- read.csv("./bases/tabela1736.csv",
                    skip = 5, 
                    header = F,
                    encoding = "UTF-8",
                    col.names = c("meses", "inpc"))
  
  # tratando a base de dados
  dados <- as.data.table(dados)
  linhas <- NROW(dados)-11
  dados <- dados[1:linhas,]
  matriz <- stringr::str_split(dados$meses, 
                               pattern = " ", 
                               simplify = T)
  
  dados[, ano := matriz[,2]]
  dados[, mes := matriz[,1]]
  dados[, mes_num := fcase(
    mes == "janeiro", 1L,
    mes == "fevereiro", 2L,
    mes == "março", 3L,
    mes == "abril", 4L,
    mes == "maio", 5L,
    mes == "junho", 06L,
    mes == "julho", 07L,
    mes == "agosto", 08L,
    mes == "setembro", 09L,
    mes == "outubro", 10L,
    mes == "novembro", 11L,
    mes == "dezembro", 12L,
    default = NA_integer_
  )]
  
  dados[, date := as.Date(paste(ano, mes_num, '01', sep = '-'))]
  dados[, value:=as.numeric(inpc)]
  dados <- dados[, .(date,value)]
  rm(linhas,matriz)
  
  # enviando base de dados tratada
  return(dados)
}
coletando_inpc()

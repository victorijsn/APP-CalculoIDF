calcula_Deflator <- function(ano_inicial, data_Referencia, inpc){
  
  library(magrittr) 
  # Tratando o inpc
  # primeiro vamos chamar os valores no período estabelecido entre o ano inicial e a data de referência
  # depois prosseguiremos aos cálculos das deflações
  
  # TRATANDO --------------------------------------------------------
  data_inicial <- paste(ano_inicial,"-01-01",sep="")
  inpc <- as.data.table(inpc)
  inpc <- inpc[date <= data_Referencia]
  tabela <- as.data.table(inpc)
  tabela[, ano := year(date)]
  tabela[, mes := month(date)]
  tabela[, date := format(as.Date(tabela$date), "%Y-%m")]
  setnames(tabela, c("date","value"), c("data", "inpc"))
  
  # Verificando se data de referência (dataref_dados) é menor ou igual a última data da inflação.
  data_INPC <- max(tabela$data) # ultima data disponível do INPC.
  data_Referencia <- format(as.Date(data_Referencia), "%Y-%m") # ano e mês de referência.
  
  # verificando se a data de referencia é maior que a data do inpc mais recente
  
  if(data_INPC < data_Referencia) {
    data_Referencia = data_INPC
  } 
  
  ## CÁLCULOS ------------------------------
  
  # criando função
  func_calc <- function(tabela) {
    tamanho <- dim(tabela)[1]
    vetor <- as.matrix(c(1, replicate( tamanho - 1, NA)))
    
    for (i in 2:tamanho){
      vetor[i,1] <- vetor[i-1,1]*(1 + tabela$inpc[i]/100)
    }
    return(as.data.table(vetor)$V1)
  }
  
  # aplicando a função na tabela
  tabela[, calculo1 := func_calc(tabela)]
  
  # reordenando 
  ordem <- c("data", "ano", "mes", "inpc", "calculo1")
  tabela <- tabela[ , ..ordem]
  
  # Cálculo 2 inpc / indice base da data de referencia
  indBase <- tabela[data == data_Referencia]$calculo1
  tabela[, calculo2 := calculo1/indBase]
  
  # Cálculo 3 : Deflator
  tabela[, deflator := 1/calculo2]
  
  # Saídas 
  saida <- tabela[,.(data, deflator)]
  setnames(saida, c("data", "deflator"), c("ano_mes", "deflatores"))
  
  return(saida)
}

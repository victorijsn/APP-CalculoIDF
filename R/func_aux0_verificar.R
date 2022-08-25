verificar <- function(dado, tipo) {
  
  # renomeando
  df <- dado
  tamanho_df <- length(df)
  
  # achando quais e quantos erros há no arquivo upado para realizar o cálculo do df posteriormente
  erro_quantos <- 0
  erro_quais <- c()
  acerto_quais <- c()
  posicao_coluna_errada <- c()
  coluna_certa <- c()
  coluna_errada <- c()
  nomes_achados <- c()
  
  ## 1) VERIFICANDO OS ERROS ##################
  
  # tipo do dado
  if (tipo != "csv" & tipo != "zip") {
    erro_quantos <- erro_quantos  + 1
    erro_quais <- c(erro_quais, 
                    '- Tipo de arquivo não adequado')}
  
  # se há menos colunas
  if (tamanho_df < 53) {
    erro_quantos <- erro_quantos  + 1
    erro_quais <- c(erro_quais, 
                    '- Quantidade de colunas insuficientes: abaixo de 53 colunas')} 
  
  if (tamanho_df > 53) {
    erro_quantos <- erro_quantos  + 1
    erro_quais <- c(erro_quais, 
                    '- Quantidade de colunas excedente: acima de 53 colunas')} 
  
  # verificando o nome das colunas
  if (tamanho_df == 53) {
    
    for (i in 1:53) {
      if (colnames(df)[i] != dicionario$nome_prodest[i]) {
        coluna_certa <- c(coluna_certa, dicionario$nome_prodest[i])
        coluna_errada <- c(coluna_errada, colnames(df)[i])
        posicao_coluna_errada <- c(posicao_coluna_errada, i)}}
    
    if (!is.null(coluna_errada)) {
      erro_quantos <- erro_quantos + 1
      erro_quais <- c(erro_quais,
                      '- Colunas com nomes não adequados.')}
  }
  
  # tabela nomes errados
  if (!is.null(coluna_errada)) {
    nomes_achados <- cbind(posicao_coluna_errada, coluna_errada, coluna_certa)
    colnames(nomes_achados) <- c('Posição da Coluna Errada','Coluna Errada', 'Coluna Certa')
  } else { nomes_achados <- c() }
  
  # enviando saídas ---------------------------------------------------
  lista <- list(
    "erro_quantos" = erro_quantos,
    "erro_quais" = erro_quais,
    "posicao_coluna_errada" = posicao_coluna_errada,
    "coluna_errada" = coluna_errada,
    "coluna_certa" = coluna_certa,
    "nomes_achados" = nomes_achados)
  
  return(lista)
}
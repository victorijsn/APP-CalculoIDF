ler_arrumar <- function(endereco_arquivo, endereco_dicionario){
  library(magrittr) 
  library(data.table)
  library(stringr)
  
  # leitura e nomes
  dicionario <- fread(endereco_dicionario)
  nomes_col <- dicionario$nome_funcao
  dataset <- read_delim(endereco_arquivo, 
                        ";", escape_double = FALSE, col_types = cols(QtdComodosDormitorioFam = col_double(), 
                                                                     QtdPessoasDomicFam = col_double(), 
                                                                     QtdPessoaInter017AnosFam = col_double(), 
                                                                     QtdPessoaInter1864AnosFam = col_double(), 
                                                                     QtdPessoaInter65AnosFam = col_double(), 
                                                                     CodParentescoRfPessoa = col_double(), 
                                                                     CodCursoFrequentaMemb = col_double(), 
                                                                     CodAnoSerieFrequentaMemb = col_double(), 
                                                                     CodCursoFrequentouPessoaMemb = col_double(), 
                                                                     CodAnoSerieFrequentouMemb = col_double(), 
                                                                     CodPrincipalTrabMemb = col_double()), 
                        locale = locale(decimal_mark = ",", grouping_mark = "."), 
                        trim_ws = TRUE)
  names(dataset) <- nomes_col
  rm(nomes_col, dicionario,endereco_arquivo, endereco_dicionario)
  dataset <- as.data.table(dataset)
  
  # Arrumar datas
  dataset[, ':='(dat_atual_fam = lubridate::dmy(dat_atual_fam),
                 dat_cadastramento_fam = lubridate::dmy(dat_cadastramento_fam),
                 dta_nasc_pessoa = lubridate::dmy(dta_nasc_pessoa))]
  dataset[,':='(dat_atual_fam = format(dat_atual_fam, "%d%m%Y"),
                dat_cadastramento_fam = format(dat_cadastramento_fam, "%d%m%Y"),
                dta_nasc_pessoa = format(dta_nasc_pessoa, "%d%m%Y")
  )]
  
  
  # Arrumar numeros e valores
  dataset[, ':='(
    vlr_renda_media_fam=  as.numeric(gsub(",00","",vlr_renda_media_fam)),
    val_desp_energia_fam=  as.numeric(gsub(",00","",val_desp_energia_fam)),
    val_desp_agua_esgoto_fam=  as.numeric(gsub(",00","",val_desp_agua_esgoto_fam)),
    val_desp_gas_fam=  as.numeric(gsub(",00","",val_desp_gas_fam)),
    val_desp_alimentacao_fam=  as.numeric(gsub(",00","",val_desp_alimentacao_fam)),
    val_desp_transpor_fam=  as.numeric(gsub(",00","",val_desp_transpor_fam)),
    val_desp_aluguel_fam=  as.numeric(gsub(",00","",val_desp_aluguel_fam)),
    val_desp_medicamentos_fam=  as.numeric(gsub(",00","",val_desp_medicamentos_fam)),
    val_remuner_emprego_memb=  as.numeric(val_remuner_emprego_memb),
    val_renda_bruta_12_meses_memb=  as.numeric(val_renda_bruta_12_meses_memb),
    val_renda_doacao_memb=  as.numeric(val_renda_doacao_memb),
    val_renda_aposent_memb=  as.numeric(val_renda_aposent_memb),
    val_renda_seguro_desemp_memb=  as.numeric(val_renda_seguro_desemp_memb),
    val_renda_pensao_alimen_memb=  as.numeric(val_renda_pensao_alimen_memb),
    val_outras_rendas_memb=  as.numeric(val_outras_rendas_memb)
  )]
  
  return(dataset)
}

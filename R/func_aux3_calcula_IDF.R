# Índice de Desenvolvimento Familiar - IDF


IDF <- function(dados = banco_de_dados, lin_pextr = 155, lin_pobreza = 450, sal_min = 1212, ano_inicial= "2016", dataref_dados = "2019-10-18", inpc){
  library(dplyr)
  library(tidyr)
  ## Sanity test - ENTRADA
  #testa se valor para linha de extrema pobreza é válido.
  if(lin_pextr < 0){stop("lin_pextr deve ser maior ou igual a zero")}
  #testa se salario mínimo é válido.
  if(sal_min < 0){stop("sal_min deve ser maior ou igual a zero")}
  #testa se valor para linha da pobreza é válido.
  if(lin_pobreza < 0){stop("lin_pobreza deve ser maior ou igual a zero")}
  #testa se valor para linha de extrema pobreza é maior do que linha da extrema pobreza.
  if(lin_pextr >= lin_pobreza){stop("lin_pextr deve ser menor do que lin_pobreza")}
  #testa classe do ano_inicial
  if( is.character(ano_inicial) != TRUE ){ stop("ano_inicial deve ser objeto do tipo 'character'. ")}
  #testa se tamanho do ano original está correto
  if( nchar(ano_inicial) != 4 ){ stop("ano_inicial deve conter 4 algarismos.")}
  #testa se ano inicial é maior do que o primeiro ano do INPC
  if( ano_inicial < 1980){stop("ano_inicial deve ser maior ou igual a 1980")}
  #testa classo da data de referência
  if( is.character(dataref_dados) != TRUE ){ stop("dataref_dados deve ser objeto do tipo 'character'. ")}
  #testa formato da data de referência
  if( suppressWarnings({ !is.na(lubridate::parse_date_time(dataref_dados,orders="ymd"))==FALSE}))
  {stop("O formato de dataref_dados deve ser: yyyy-mm-dd")}
  
  
  
  ##Inicio do código
  ano_inicial <- as.numeric(ano_inicial) # transformando entrada character em numeric.
  
  dados <- as.data.table(dados)
  
  # trocando d.cod_familiar_fam (nome no arquivo original) por cod_familiar_fam
  bd <- setnames(dados, "d.cod_familiar_fam", "cod_familiar_fam")
  
  #bd <- dados
  ### Manipulando DATAS
  bd[,nasc:= dta_nasc_pessoa]
  
  #bd$dcadas <- bd$dat_atual_fam
  bd[, data_atual := dataref_dados]
  
  # bd[, dataref_dados:= dataref_dados] 
  
  
  bd[, dentrada:= dat_cadastramento_fam]
  
  ################################ PRIMEIRO FILTRO ##############################################
  ### Selecionando universo: pessoas com cadastro atualizado a partir do ano_inicial
  b <- bd[year(data_atual) >= ano_inicial] 
  
  # Criando variável idade
  b[,diasp:= lubridate::time_length(lubridate::interval(b$nasc,b$data_atual),"days") ]
  
  b[,idade:= floor(lubridate::time_length(lubridate::interval(b$nasc,b$data_atual),"years"))]
  
  #b$idade <- floor(b$diasp/365)
  b[,idade:= fifelse(idade < 0, NA_real_,
                     fifelse(idade >= 120 , NA_real_, idade))]
  
  b[,pes_cad:= fifelse((cod_est_cadastral_memb==3 | cod_est_cadastral_memb==6),1,0)]
  
  ################################ SEGUNDO FILTRO ##############################################
  b <- b[pes_cad==1,]
  
  b[, index:= 1:.N]
  
  # Início do cálculo das variáveis
  # AUX_V1 - CRIANCAS DE 0 A 6 ANOS
  b[, aux_v1:= fifelse((idade >= 0 & idade <= 6),1, fifelse(is.na(idade), NA_real_, 0))]
  
  # AUX_V2 - CRIANCAS DE 0 A 14 ANOS
  b[,aux_v2:= fifelse((idade >= 0 & idade <= 14),1, fifelse(is.na(idade), NA_real_, 0))]
  
  # AUX_V3 - CRIANCAS DE 0 A 17 ANOS
  b[,aux_v3:= fifelse((idade >= 0 & idade <= 17),1, fifelse(is.na(idade), NA_real_, 0))]
  
  
  # AUX_V4:PORTADORES DE DEFICIENCIA E IDOSOS
  b[,aux_v4:= fifelse(cod_deficiencia_memb == 1,1, fifelse(is.na(cod_deficiencia_memb), NA_real_,0))]
  
  # AUX_V5: IDOSOS DE 65 ANOS OU MAIS
  b[,aux_v5:= fifelse((idade >= 65),1, fifelse(is.na(idade), NA_real_, 0))]
  
  #OBS: variável V6 será criada mais a frente.
  
  # AUX_V7: PRESENCA DO CONJUGE
  b[,aux_v7:= fifelse(cod_parentesco_rf_pessoa == 2,1, fifelse(is.na(cod_deficiencia_memb), NA_real_,0))]
  
  # AUX_V8A: TOTAL DE MEMBROS EM IDADE ATIVA (15 A 64 ANOS)
  b[,aux_v8a:= fifelse((idade >= 15 & idade <= 64),1, fifelse(is.na(idade), NA_real_, 0))]
  
  # CRIANDO VARIAVEL PARENTESCO_RF_2
  b[,parentesco_rf_2:= fifelse(cod_parentesco_rf_pessoa >= 1 & cod_parentesco_rf_pessoa <= 10,1,
                               fifelse(is.na(cod_parentesco_rf_pessoa), NA_real_, 11)) ]
  
  
  # AUX_V8B - Total de membros
  b[,aux_v8b:=fifelse((parentesco_rf_2 >= 1 & parentesco_rf_2 <= 11),1,
                      fifelse(is.na(cod_parentesco_rf_pessoa), NA_real_, 0))]
  
  # AUX_V9: PESSOAS COM 15 ANOS OU MAIS
  b[,aux_v9:= fifelse(idade >= 15,1, fifelse(is.na(idade), NA_real_, 0))]
  
  
  # AUX_V10: PESSOAS COM 18 ANOS OU MAIS
  b[,aux_v10:= fifelse(idade >= 18,1, fifelse(is.na(idade), NA_real_, 0))]
  
  # AUX_V11: PESSOAS COM 21 ANOS OU MAIS
  b[,aux_v11:= fifelse(idade >= 21,1, fifelse(is.na(idade), NA_real_, 0))]
  
  # AUX_V12: PESSOAS COM ATE 9 ANOS QUE NAO SEJA FILHO OU ENTEADO DO RF
  b[,aux_v12:= fifelse(idade >= 0 & idade <= 9 & 
                         ((cod_parentesco_rf_pessoa >= 1 & cod_parentesco_rf_pessoa <= 2) |
                            (cod_parentesco_rf_pessoa >= 5 & cod_parentesco_rf_pessoa <= 11)), 1,
                       fifelse(is.na(idade) | is.na(cod_parentesco_rf_pessoa), NA_real_,0) )]
  
  
  # AUX_V13: PESSOAS COM ATE 9 ANOS (INCLUSIVE) QUE SEJA OUTRO PARENTE OU NAO PARENTE DO RF
  b[,aux_v13:=fifelse(idade >= 0 & idade <= 9 &
                        cod_parentesco_rf_pessoa >= 10 & cod_parentesco_rf_pessoa <= 11,1,
                      fifelse(is.na(idade) | is.na(cod_parentesco_rf_pessoa),NA_real_,0))]
  
  # AUX_V14: RESPONSAVEL PELA FAMILIA NASCEU NESSE MUNICIPIO
  b[,aux_v14:= fifelse(cod_parentesco_rf_pessoa==1 & cod_local_nascimento_pessoa==1,1,
                       fifelse(is.na(cod_parentesco_rf_pessoa) | is.na(cod_local_nascimento_pessoa),NA_real_,0))]
  # AUX_V15: CRIANCA OU ADOLESCENTE COM ATE 14 (INCLUSIVE) ANOS QUE NASCEU EM OUTRO MUNICIPIO
  b[,aux_v15:= fifelse(idade >= 0  & idade <= 14 &
                         b$cod_local_nascimento_pessoa >= 2 & b$cod_local_nascimento_pessoa<=3,1,
                       fifelse(is.na(idade) | is.na(cod_local_nascimento_pessoa),NA_real_,0))]
  ## CRIANDO A VARIAVEL ANALFABETO - LINHA 167
  b[,analf:= fifelse(cod_sabe_ler_escrever_memb==2,1,
                     fifelse(cod_sabe_ler_escrever_memb==1,0,NA_real_))]
  
  # AUX_D9: ADOLESCENTE COM 15 A 17 ANOS ANALFABETO
  b[,aux_d9:= fifelse((idade >=15 & idade<=17 & analf == 1),1,
                      fifelse(is.na(idade) | is.na(analf),NA_real_,0))]
  ################################ CRIANDO A VARIAVEL EDUCA ###########################################
  
  # Creche, pre-escola, C.A.: educa=0
  b[,educa:= fifelse(ind_frequenta_escola_memb >= 1 & ind_frequenta_escola_memb <= 2  &
                       cod_curso_frequenta_memb  >= 1 & cod_curso_frequenta_memb  <= 3,0,NA_real_)]
  
  # Fundamental: educa: serie-1 (fundamental antigo)
  b[, educa:= fifelse(ind_frequenta_escola_memb >=1 & ind_frequenta_escola_memb<=2    &
                        cod_curso_frequenta_memb ==4  & cod_ano_serie_frequenta_memb>=1 &
                        cod_ano_serie_frequenta_memb<=8, as.double(cod_ano_serie_frequenta_memb) - 1, educa)]
  
  # Fundamental nao seriado: educa=0
  b[, educa := fifelse(ind_frequenta_escola_memb >=1 & ind_frequenta_escola_memb <=2 &
                         cod_curso_frequenta_memb == 4 & cod_ano_serie_frequenta_memb == 10, 0, educa)]
  #Fundamental novo 1o ano: educa=0
  b[, educa:= fifelse(ind_frequenta_escola_memb >= 1 & ind_frequenta_escola_memb <= 2 &
                        cod_curso_frequenta_memb == 5 & cod_ano_serie_frequenta_memb == 1, 0, educa)]
  
  # Fundamental novo 2o ao 9o ano: educa=serie-2
  b[, educa:= fifelse(ind_frequenta_escola_memb >= 1 & ind_frequenta_escola_memb <=2     &
                        cod_curso_frequenta_memb  == 5 & cod_ano_serie_frequenta_memb >= 2 &
                        cod_ano_serie_frequenta_memb <= 9, cod_ano_serie_frequenta_memb-2, educa)]
  #Fundamental especial: educa=serie-1
  b[, educa:= fifelse(ind_frequenta_escola_memb >= 1 & ind_frequenta_escola_memb <= 2 &
                        cod_curso_frequenta_memb  == 6 & cod_ano_serie_frequenta_memb>=1 &
                        cod_ano_serie_frequenta_memb <=8, cod_ano_serie_frequenta_memb-1,educa)]
  #Fundamental especial nao seriado: educa=0
  b[, educa:= fifelse(ind_frequenta_escola_memb >= 1 & ind_frequenta_escola_memb<=2 &
                        cod_curso_frequenta_memb == 6  & cod_ano_serie_frequenta_memb==10,0,educa)]
  #Ensino medio 1a serie: educa=8
  b[, educa:= fifelse(ind_frequenta_escola_memb >= 1 & ind_frequenta_escola_memb<=2 &
                        cod_curso_frequenta_memb  >= 7 & cod_curso_frequenta_memb <=8 &
                        cod_ano_serie_frequenta_memb==1,8,educa)]
  
  #Ensino medio 2a serie: educa=9
  b[, educa:= fifelse(ind_frequenta_escola_memb >= 1 & ind_frequenta_escola_memb<=2 &
                        cod_curso_frequenta_memb  >= 7 & cod_curso_frequenta_memb <=8 &
                        cod_ano_serie_frequenta_memb==2,9,educa)]
  
  # Ensino medio 3a / 4a serie: educa=10
  b[, educa:= fifelse(ind_frequenta_escola_memb >= 1 & ind_frequenta_escola_memb <= 2 &
                        cod_curso_frequenta_memb  >= 7 & cod_curso_frequenta_memb <= 8  &
                        (cod_ano_serie_frequenta_memb==3 | cod_ano_serie_frequenta_memb==4),
                      10,educa)]
  #Ensino medio nao seriado: educa=8
  b[, educa:= fifelse(ind_frequenta_escola_memb >= 1 & ind_frequenta_escola_memb<=2 &
                        cod_curso_frequenta_memb  >= 7 & cod_curso_frequenta_memb <=8 &
                        cod_ano_serie_frequenta_memb == 10,8,educa)]
  #EJA primeiro ciclo: educa=0
  b[, educa:= fifelse(ind_frequenta_escola_memb>=1 & ind_frequenta_escola_memb<=2 &
                        cod_curso_frequenta_memb==9,0,educa)]
  
  #EJA segundo ciclo: educa=4
  b[, educa:= fifelse(ind_frequenta_escola_memb >= 1 & ind_frequenta_escola_memb <= 2 &
                        cod_curso_frequenta_memb==10,4,educa)]
  #EJA medio: educa=8
  b[, educa:= fifelse(ind_frequenta_escola_memb >= 1 & ind_frequenta_escola_memb<=2 &
                        cod_curso_frequenta_memb==11,8,educa)]
  #Alfabetizacao de adultos: educa=0
  b[, educa:= fifelse(ind_frequenta_escola_memb >= 1 & ind_frequenta_escola_memb <= 2 &
                        cod_curso_frequenta_memb==12,0, educa)]
  #Superior, aperfeicoamento, mestrado, doutorado: educa=13
  b[, educa:= fifelse(ind_frequenta_escola_memb >= 1 & ind_frequenta_escola_memb<=2 &
                        cod_curso_frequenta_memb==13,13,educa)]
  #Pre-vestibular: educa=11
  b[, educa:= fifelse(ind_frequenta_escola_memb >= 1 & ind_frequenta_escola_memb<=2 &
                        cod_curso_frequenta_memb==14,11,educa)]
  #NAO FREQUENTRA MAS JA FREQUENTOU ESCOLA
  
  #Creche, pre, C.A.: educa=0
  b[, educa:= fifelse(ind_frequenta_escola_memb ==3 & cod_curso_frequentou_pessoa_memb>=1 &
                        cod_curso_frequentou_pessoa_memb<=3,0,educa)]
  
  #Fundamental primeiro ciclo: concluiu (educa=4) nao concluiu (educa=serie-1)
  b[, educa:= fifelse(ind_frequenta_escola_memb==3 & cod_curso_frequentou_pessoa_memb==4 &
                        cod_concluiu_frequentou_memb==1,4,educa)]
  
  b[, educa:= fifelse(ind_frequenta_escola_memb == 3 & cod_curso_frequentou_pessoa_memb==4 &
                        cod_ano_serie_frequentou_memb >= 1 & cod_ano_serie_frequentou_memb<=4 &
                        cod_concluiu_frequentou_memb == 2, cod_ano_serie_frequentou_memb-1,educa)]
  #fundamental primeiro ciclo nao seriado; educa=0.
  b[, educa:= fifelse(ind_frequenta_escola_memb==3 & cod_curso_frequentou_pessoa_memb==4 &
                        cod_ano_serie_frequentou_memb==10 & cod_concluiu_frequentou_memb==2,0,educa)]
  
  #Fundamental segundo ciclo
  b[, educa:= fifelse(ind_frequenta_escola_memb == 3 & cod_curso_frequentou_pessoa_memb == 5 &
                        cod_concluiu_frequentou_memb == 1, 8, educa)]
  
  b[, educa:= fifelse(ind_frequenta_escola_memb==3 & cod_curso_frequentou_pessoa_memb == 5 &
                        cod_ano_serie_frequentou_memb >= 5 & cod_ano_serie_frequentou_memb <= 8 &
                        cod_concluiu_frequentou_memb == 2 , cod_ano_serie_frequentou_memb-1,educa)]
  
  #Fundamental segundo ciclo nao seriado linha 247
  b[, educa:= fifelse(ind_frequenta_escola_memb == 3 & cod_curso_frequentou_pessoa_memb == 5 &
                        cod_ano_serie_frequentou_memb == 10 & cod_concluiu_frequentou_memb == 1,8,educa)]
  
  b[, educa:= fifelse(ind_frequenta_escola_memb == 3     & cod_curso_frequentou_pessoa_memb == 5 &
                        cod_ano_serie_frequentou_memb == 10 & cod_concluiu_frequentou_memb == 2,4,educa)]
  
  #Fundamental novo: 1o ano educa=0
  b[, educa:=  fifelse(ind_frequenta_escola_memb==3 & cod_curso_frequentou_pessoa_memb==6 &
                         cod_ano_serie_frequentou_memb==1,0,educa)]
  
  #Fundamental novo: concluiu educa=8
  b[, educa:=  fifelse(ind_frequenta_escola_memb==3 & cod_curso_frequentou_pessoa_memb==6 &
                         cod_concluiu_frequentou_memb==1,8,educa)]
  
  b[, educa:= fifelse(ind_frequenta_escola_memb==3       & cod_curso_frequentou_pessoa_memb==6 &
                        cod_ano_serie_frequentou_memb>=2 & cod_ano_serie_frequentou_memb<=9    &
                        cod_concluiu_frequentou_memb==2,cod_ano_serie_frequentou_memb-2,educa)]
  
  #fundamental nao seriado: educa = 0
  b[, educa:=  fifelse(ind_frequenta_escola_memb==3 & cod_curso_frequentou_pessoa_memb==6 &
                         cod_ano_serie_frequentou_memb==10 & cod_concluiu_frequentou_memb==2,0,educa)]
  
  #fundamental especial: concluiu: educa=8
  b[, educa:=  fifelse(ind_frequenta_escola_memb==3 & cod_curso_frequentou_pessoa_memb==7 &
                         cod_concluiu_frequentou_memb==1,8,educa)]
  
  #fundamental especial: nao concluiu
  b[, educa:= fifelse(ind_frequenta_escola_memb==3     & cod_curso_frequentou_pessoa_memb==7 &
                        cod_ano_serie_frequentou_memb>=1 & cod_ano_serie_frequentou_memb<=8 &
                        cod_concluiu_frequentou_memb==2, cod_ano_serie_frequentou_memb-1,educa)]
  
  #fundamental especial nao seriado
  b[, educa:= fifelse(ind_frequenta_escola_memb==3      & cod_curso_frequentou_pessoa_memb==7 &
                        cod_ano_serie_frequentou_memb==10 & cod_concluiu_frequentou_memb==1,8,educa)]
  
  b[, educa:= fifelse(ind_frequenta_escola_memb==3      & cod_curso_frequentou_pessoa_memb==7 &
                        cod_ano_serie_frequentou_memb==10 & cod_concluiu_frequentou_memb==2,0,educa)]
  
  ### MEDIO
  b[, educa:= fifelse(ind_frequenta_escola_memb==3        & cod_curso_frequentou_pessoa_memb>=8 &
                        cod_curso_frequentou_pessoa_memb<=9 & cod_concluiu_frequentou_memb==1,11,educa)]
  
  b[, educa:= fifelse(ind_frequenta_escola_memb==3         & cod_curso_frequentou_pessoa_memb>=8 &
                        cod_curso_frequentou_pessoa_memb<=9  & cod_ano_serie_frequentou_memb==1 &
                        cod_concluiu_frequentou_memb==2,8,educa)]
  
  b[, educa:= fifelse(ind_frequenta_escola_memb==3        & cod_curso_frequentou_pessoa_memb>=8 &
                        cod_curso_frequentou_pessoa_memb<=9 & cod_ano_serie_frequentou_memb==2 &
                        cod_concluiu_frequentou_memb==2,9,educa)]
  
  b[, educa:= fifelse(ind_frequenta_escola_memb==3        & cod_curso_frequentou_pessoa_memb>=8 &
                        cod_curso_frequentou_pessoa_memb<=9 & cod_ano_serie_frequentou_memb==3 &
                        cod_concluiu_frequentou_memb==2,10,educa)]
  
  b[, educa:= fifelse(ind_frequenta_escola_memb==3        & cod_curso_frequentou_pessoa_memb>=8 &
                        cod_curso_frequentou_pessoa_memb<=9 & cod_ano_serie_frequentou_memb==4    &
                        cod_concluiu_frequentou_memb==2,10,educa)]
  
  
  b[, educa:= fifelse(ind_frequenta_escola_memb==3        & cod_curso_frequentou_pessoa_memb>=8 &
                        cod_curso_frequentou_pessoa_memb<=9 & cod_ano_serie_frequentou_memb==10   &
                        cod_concluiu_frequentou_memb==2,8,educa)]
  
  #EJA primeiro ciclo fundamental
  b[, educa:= fifelse(ind_frequenta_escola_memb==3 & cod_curso_frequentou_pessoa_memb==10 &
                        cod_concluiu_frequentou_memb==1,4,educa)]
  
  b[, educa:= fifelse(ind_frequenta_escola_memb==3 & cod_curso_frequentou_pessoa_memb==10 &
                        cod_concluiu_frequentou_memb==2,0,educa)]
  
  #EJA segundo ciclo fundamental
  b[, educa:= fifelse(ind_frequenta_escola_memb==3 & cod_curso_frequentou_pessoa_memb==11 &
                        cod_concluiu_frequentou_memb==1,8,educa)]
  
  b[, educa:= fifelse(ind_frequenta_escola_memb==3 & cod_curso_frequentou_pessoa_memb==11 &
                        cod_concluiu_frequentou_memb==2,4,educa)]
  
  #EJA medio
  b[, educa:= fifelse(ind_frequenta_escola_memb==3 & cod_curso_frequentou_pessoa_memb==12 &
                        cod_concluiu_frequentou_memb==1,11,educa)]
  
  b[, educa:= fifelse(ind_frequenta_escola_memb==3 & cod_curso_frequentou_pessoa_memb==12 &
                        cod_concluiu_frequentou_memb==2,8,educa)]
  
  #Superior, aperfeicoamento, mestrado, doutorado
  b[, educa:= fifelse(ind_frequenta_escola_memb==3 & cod_curso_frequentou_pessoa_memb==13,13,educa)]
  
  #Alfabetizacao de adultos
  b[, educa:= fifelse(ind_frequenta_escola_memb==3 & cod_curso_frequentou_pessoa_memb==14,0,educa)]
  
  #Nao frequentou nenhum curso
  b[, educa:= fifelse(ind_frequenta_escola_memb==3 & cod_curso_frequentou_pessoa_memb==15,0,educa)]
  
  #Nunca frequentou escola
  b[, educa:=  fifelse(ind_frequenta_escola_memb==4,0,educa)]
  
  ######### FIM VARIAVEL EDUCACAO
  
  # AUX_C1: ADULTO COM MAIS DE 17 ANOS ANALFABETO
  b[,aux_c1:= fifelse(idade > 17  & analf==1 , 1, fifelse(is.na(idade) | is.na(analf),NA_real_,0))]
  
  # AUX_C2: ADULTO COM MAIS DE 17 ANOS ANALFABETO FUNCIONAL (MENOS DE 4 ANOS DE ESTUDO)
  b[,aux_c2:= fifelse(idade>17 &  educa>=0 & educa<=3, 1,  fifelse(is.na(idade) | is.na(educa),NA_real_,0))]
  
  # AUX_C3: PRESENCA DE PELO MENOS UMA PESSOA COM 15 ANOS OU MAIS ALFABETIZADA
  b[, aux_c3:=  fifelse(idade>=15 & analf==0 ,1, fifelse(is.na(idade) | is.na(analf),NA_real_,0))]
  
  # FREQESC_JAFREQ: FREQUENTA A ESCOLA OU JA FREQUENTOU
  b[, freqesc_jafreq:= fifelse(ind_frequenta_escola_memb>=1 & ind_frequenta_escola_memb<=3,1,
                               fifelse(is.na(ind_frequenta_escola_memb),NA_real_,0))]
  
  # AUX_C4: ADULTO COM 15 ANOS  OU MAIS QUE FREQUENTA OU JA FREQUENTOU A ESCOLA
  b[,aux_c4:=  fifelse(idade >= 15 &  freqesc_jafreq == 1,1, fifelse(is.na(idade) | is.na(freqesc_jafreq) ,NA_real_,0))]
  
  # AUX_C5: ADULTO COM 15 ANOS OU MAIS ALFABETIZADA QUE FREQUENTA OU JA FREQUENTOU A ESCOLA
  b[,aux_c5:= fifelse(idade>=15 &  analf==0 & freqesc_jafreq==1,1,
                      fifelse(is.na(idade) | is.na(analf) | is.na(freqesc_jafreq),NA_real_,0))]
  
  # AUX_C6: ADULTO COM MAIS DE 17 ANOS COM FUNDAMENTAL COMPLETO (MAIS DE 7 ANOS DE ESTUDO)
  b[,aux_c6:= fifelse(idade>17 & educa>=8,1, fifelse(is.na(idade) | is.na(educa),NA_real_,0))]
  
  # AUX_C7: ADULTO COM MAIS DE 17 ANOS COM MEDIO COMPLETO (MAIS DE 10 ANOS DE ESTUDO)
  b[,aux_c7:= fifelse(idade>17 &  educa>=11,1, fifelse(is.na(idade) | is.na(educa),NA_real_,0))]
  
  # AUX_C8: ADULTO COM MAS DE 17 ANOS COM ALGUMA EDUCACAO SUPERIOR (MAIS DE 11 ANOS DE ESTUDO)
  b[,aux_c8:= fifelse(idade>17 & educa>=12,1, fifelse(is.na(idade) | is.na(educa),NA_real_,0))]
  
  # AUX_T1: MEMBRO EM IDADE ATIVA
  b[,aux_t1:= fifelse(idade>=15,1, fifelse(is.na(idade),NA_real_,0))]
  
  # PESS_OCUP
  b[,pess_ocup:= fifelse((cod_trabalhou_memb==1) | (cod_trabalhou_memb==2 & cod_afastado_trab_memb==1),1,
                         fifelse((cod_trabalhou_memb==2 & cod_afastado_trab_memb==2) |
                                   (cod_trabalhou_memb==2 & (cod_afastado_trab_memb!=1 & cod_afastado_trab_memb!=2)),0,NA_real_))]
  
  
  # AUX_T2: MEMBRO EM IDADE ATIVA OCUPADO
  b[,aux_t2:= fifelse(idade>=15 & idade<=64 & pess_ocup==1,1, fifelse(is.na(idade)| is.na(pess_ocup),NA_real_,0))]
  
  # AUX_T3: OCUPADO NO SETOR FORMAL
  b[,aux_t3:= fifelse((pess_ocup==1 & cod_principal_trab_memb==4) |
                        (pess_ocup==1 & cod_principal_trab_memb==6) |
                        (pess_ocup==1 & cod_principal_trab_memb>=8 & cod_principal_trab_memb<=11),1,
                      fifelse(pess_ocup==1,0,NA_real_))]
  
  # AUX_T4: OCUPADO EM SETOR NAO AGRO
  b[,aux_t4:= fifelse(pess_ocup==1 & cod_agricultura_trab_memb==2,1,
                      fifelse(pess_ocup==1 & is.na(cod_agricultura_trab_memb)==FALSE ,0,NA_real_))]
  
  # TRATAMENTO DA RENDA
  b[, val_remuner_emprego_memb:= fifelse(is.na(val_remuner_emprego_memb), 0, as.numeric(val_remuner_emprego_memb))]
  b[,val_renda_bruta_12_meses_memb:= fifelse(is.na(val_renda_bruta_12_meses_memb), 0, as.numeric(val_renda_bruta_12_meses_memb))]
  
  # RENDA DO TRABALHO
  b[,rendatrab:= val_remuner_emprego_memb]
  
  # ALGUMA RENDA 
  b[, renda_alg := rowSums(as.data.frame(.(val_renda_bruta_12_meses_memb, val_renda_doacao_memb ,val_renda_aposent_memb,
                                           val_renda_seguro_desemp_memb , val_renda_pensao_alimen_memb,val_outras_rendas_memb)), na.rm = TRUE)]
  # RENDA_TRANSF 
  b[, renda_transf := rowSums(as.data.frame(.(val_renda_doacao_memb, val_renda_aposent_memb, val_renda_seguro_desemp_memb,
                                              val_renda_pensao_alimen_memb)), na.rm = TRUE)]
  
  # RENDA
  b[,renda:= renda_alg]
  
  
  #ALGUMA RENDA EXCETO TRANSFERENCIAS
  b[,renda_ntrans:= renda_alg - renda_transf]
  
  b[, renda_ntrans:= fifelse(renda_ntrans < 0,0, renda_ntrans)]
  
  
  ###################### Criando Base Domicilios a partir
  D <- bd[, .SD[c(1)], by= cod_familiar_fam] 
  
  
  ######## CALCULANDO DEFLATORES
  
  tabela_deflatores <- calcula_Deflator(ano_inicial = ano_inicial, 
                                        data_Referencia = dataref_dados,
                                        inpc = inpc)
  
  ####### RELACIONANDO CADA DEFLATOR COM A DATA MAIS RECENTE DE CADASTRAMENTO
  D[, ano_mes := format(as.Date(dat_atual_fam), "%Y-%m")]
  b[, ano_mes := format(as.Date(dat_atual_fam), "%Y-%m")]
  
  # l<- left_join(x = k, y = tabela_deflatores, by = "data")
  b <- merge.data.table(x = b, y = tabela_deflatores, by = "ano_mes")
  
  #####
  
  #RENDA DEFLACIONADA
  b[,renda_defla:= renda*deflatores]
  
  
  #SALARIO MINIMO 2016
  sm <- sal_min
  
  #AUX_T5: OCUPADO COM RENDIMENTO SUPERIOR A 1 SM
  b[,aux_t5:= fifelse((pess_ocup==1 & renda>sm), 1,
                      if_else(is.na(pess_ocup) | is.na(renda), NA_real_,0))]
  
  # AUX_T6: OCUPADO COM RENDIMENTO SUPERIOR A 2 SM
  b[, aux_t6:= fifelse(pess_ocup==1 & renda > 2*sm,1,
                       if_else(is.na(pess_ocup) | is.na(renda),NA_real_,0))]
  
  # CRIANDO A VARIAVEL DEFAS
  b[,defas:= if_else( 0 >= idade - 7 - educa, 0, if_else(idade-7-educa>0, idade-7-educa,NA_real_))]
  
  
  # AUX_D1: CRIANCAS MENORES DE 12 ANOS TRABALHANDO
  b[, aux_d1:= fifelse((idade>=0 & idade<=11 & (pess_ocup==1 | ind_trabalho_infantil_pessoa==1)),1,
                       if_else(is.na(idade) | is.na(pess_ocup) | is.na(ind_trabalho_infantil_pessoa), NA_real_,0))]
  
  # AUX_D2: CRIANCAS MENORES DE 14 ANOS TRABALHANDO
  b[,aux_d2:= fifelse(idade>=0 & idade<=13 & (pess_ocup==1 |
                                                ind_trabalho_infantil_pessoa==1),1,
                      if_else(is.na(idade) | is.na(pess_ocup) | is.na(ind_trabalho_infantil_pessoa),NA_real_,0))]
  
  
  # AUX_D3: CRIANCAS MENORES DE 16 ANOS TRABALHANDO
  b[,aux_d3:= fifelse(idade>=0 & idade<=15 & (pess_ocup==1 | ind_trabalho_infantil_pessoa==1),1,
                      if_else(is.na(idade) | is.na(pess_ocup) | is.na(ind_trabalho_infantil_pessoa), NA_real_,0))]
  
  # AUX_D4: CRIANCAS DE 4 A 6 ANOS QUE NAO FREQUENTAM A ESCOLA
  b[,aux_d4:= fifelse(idade>=4 & idade<=6 & freqesc_jafreq==0,1,
                      if_else(is.na(idade) | is.na(freqesc_jafreq),NA_real_,0))]
  
  
  # AUX_D5: CRIANCAS DE 7 A 14 ANOS QUE NAO FREQUENTAM A ESCOLA
  b[,aux_d5:= fifelse(idade >= 7 & idade<=14 & freqesc_jafreq==0, 1,
                      if_else(is.na(idade) | is.na(freqesc_jafreq),NA_real_,0))]
  
  # AUX_D6: CRIANCAS DE 7 A 17 ANOS QUE NAO FREQUENTAM A ESCOLA
  b[,aux_d6:= fifelse(idade>=7 & idade<=17 & freqesc_jafreq==0, 1,
                      if_else(is.na(idade) | is.na(freqesc_jafreq),NA_real_,0))]
  
  # AUX_D7: CRIANCAS DE 0 A 14 ANOS COM MAIS DE 2 ANOS DE DEFASAGEM
  b[,aux_d7:= fifelse(0 >= idade & idade <= 14 & defas > 2, 1,
                      if_else(is.na(idade) | is.na(defas), NA_real_,0))]
  
  # AUX_D8: ADOLESCENTE COM 10 A 14 ANOS ANALFABETO
  b[,aux_d8:= fifelse(idade >= 10 & idade<=14 & analf==1,1,
                      if_else(is.na(idade) | is.na(analf),NA_real_,0))]
  
  b[,aux_v8a_:= aux_v8a]
  
  b[,aux_v8b_ := aux_v8b]
  
  b[,aux_t1_:= aux_t1]
  
  b[,aux_t2_:= aux_t2]
  
  b[, renda_transf_ := renda_transf]
  
  
  
  ## COLLAPSE 1
  ## COLLAPSE - via data.table
  b <- as.data.table(b)
  
  nomes_collapse1 <- c("idade",                           "pes_cad",                          "index",
                       "aux_v1",                          "aux_v2",                           "aux_v3",
                       "aux_v4",                          "aux_v5",                           "aux_v7",
                       "aux_v8a",                         "parentesco_rf_2",                  "aux_v8b",
                       "aux_v9",                          "aux_v10",                          "aux_v11",
                       "aux_v12",                         "aux_v13",                          "aux_v14",
                       "aux_v15",                         "analf",                            "aux_d9",
                       "educa",                           "aux_c1",                           "aux_c2",
                       "aux_c3",                          "freqesc_jafreq",                   "aux_c4",
                       "aux_c5",                          "aux_c6",                           "aux_c7",
                       "aux_c8",                          "aux_t1",                           "pess_ocup",
                       "aux_t2",                          "aux_t3",                           "aux_t4",
                       "rendatrab",                       "renda_alg",                        "renda_transf",
                       "renda",                           "renda_ntrans",                     
                       "deflatores",
                       "renda_defla",                     "aux_t5",                           "aux_t6",
                       "defas",                           "aux_d1",                           "aux_d2",
                       "aux_d3",                          "aux_d4",                           "aux_d5",
                       "aux_d6",                          "aux_d7",                           "aux_d8")
  
  collapse1 <- b[, by = c("cod_familiar_fam","cd_ibge"), lapply(.SD, mean, na.rm=TRUE),.SDcols =nomes_collapse1]
  
  nomes_collapse1a <- c("aux_v8a_", "aux_v8b_", "aux_t1_", "aux_t2_", "renda_transf_")
  collapse1a <- b[,by = c("cod_familiar_fam","cd_ibge"), lapply(.SD, sum, na.rm = TRUE),.SDcols = nomes_collapse1a]
  
  B <- merge(collapse1, collapse1a, by=c("cd_ibge", "cod_familiar_fam"))
  
  ###################### BASE DOMICILIOS ##########################################################
  #DESPESA TOTAL -
  
  # l<- left_join(x = k, y = tabela_deflatores, by = "data")
  D <- merge.data.table(x = D, y = tabela_deflatores, by = "ano_mes")
  
  D[, desptot := rowSums(as.data.table(.( val_desp_energia_fam,val_desp_agua_esgoto_fam,val_desp_gas_fam,
                                          val_desp_alimentacao_fam,val_desp_transpor_fam,val_desp_aluguel_fam,
                                          val_desp_medicamentos_fam)), na.rm = TRUE) * deflatores]
  
  
  # DESPALIM
  D[, despalim := val_desp_alimentacao_fam]
  
  # RENDA_MEDIA
  D[, renda_media:= vlr_renda_media_fam*deflatores]
  
  # TOT_PESS
  D[, tot_pess := qtd_pessoas_domic_fam]
  
  # GRUPO: DEFICIT HABITACIONAL 
  D[, dormitorios := fcase(
    qtd_comodos_dormitorio_fam == 1L , 1L,
    qtd_comodos_dormitorio_fam == 2L , 2L,
    qtd_comodos_dormitorio_fam == 3L , 3L,
    qtd_comodos_dormitorio_fam == 4L , 4L,
    qtd_comodos_dormitorio_fam == 5L , 5L,
    qtd_comodos_dormitorio_fam == 6L , 6L,
    qtd_comodos_dormitorio_fam == 7L , 7L,
    qtd_comodos_dormitorio_fam == 8L , 8L,
    qtd_comodos_dormitorio_fam == 50L , 50L,
    qtd_comodos_dormitorio_fam == 60L , 60L,
    default = NA_integer_
  )]
  
  
  # DENSIDADE
  D[, densidade := fifelse(tot_pess<0 | qtd_comodos_dormitorio_fam<=0, NA_real_, tot_pess / qtd_comodos_dormitorio_fam)]
  
  
  D[, fam_cad := fifelse(cod_est_cadastral_fam==3,1,0)]
  
  ## BASE DOMICILIOS
  D <- D[fam_cad == 1]
  
  # AUX_V6 - Pessoa na familia internada ou abrigada em hospital. casa de saude. asilo. orfanato ou estabelecimento similr;
  ##Minha interpretação#
  D[, aux_v6 := fifelse((qtd_pessoa_inter_0_17_anos_fam > 0  & is.na(qtd_pessoa_inter_0_17_anos_fam)==FALSE)  |
                          (qtd_pessoa_inter_18_64_anos_fam > 0  & is.na(qtd_pessoa_inter_18_64_anos_fam)==FALSE)  |
                          (qtd_pessoa_inter_65_anos_fam > 0     & is.na(qtd_pessoa_inter_65_anos_fam)==FALSE ), 1,
                        fifelse(is.na(qtd_pessoa_inter_0_17_anos_fam) | is.na(qtd_pessoa_inter_18_64_anos_fam) |
                                  is.na(qtd_pessoa_inter_65_anos_fam),NA_real_,0))]
  
  
  # AUX_V16 - Familia que nao e indigena nem quilombola
  D[, aux_v16 := fifelse(cod_familia_indigena_fam==2 & ind_familia_quilombola_fam == 2, 1,
                         if_else(cod_familia_indigena_fam==1 | ind_familia_quilombola_fam==1,0,NA_real_))]
  
  # H1: DOMICILIO PARTICULAR OU COLETIVO
  D[, h1 := fcase(cod_especie_domic_fam == 1 | 
                    cod_especie_domic_fam == 2 |
                    cod_especie_domic_fam == 3, 1L,
                  is.na(cod_especie_domic_fam), 0L)]
  
  # H2: DOMICILIO PARTICULAR PERMANENTE OU PROVISORIO
  D[, h2 := fcase(cod_especie_domic_fam == 1 |
                    cod_especie_domic_fam == 2, 1L,
                  is.na(cod_especie_domic_fam), 0L, default = 0L)]
  
  # H3: DOMICILIO PARTICULAR PERMANENTE
  D[, h3 := fcase(cod_especie_domic_fam == 1, 1L, 
                  is.na(cod_especie_domic_fam), 0L, default = 0L)]
  
  # H4:DENSIDADE DE ATE 2 MORADORES POR DORMITORIO
  D[, h4 :=  fcase(h3==1 & densidade <= 2, 1L,
                   h3==1 & is.na(densidade), 1L,
                   h3==0 | densidade > 2, 0L)]
  
  
  #H5: MATERIAL DE CONSTRU??O PERMANENTE EM DOMICILIO PARTICULAR PERMANENTE
  D[, h5 := fcase(h3==1 & cod_material_domic_fam %in% c(1:4), 1L,
                  h3==1 & is.na(cod_material_domic_fam), 0L,
                  h3 != 1 | cod_material_domic_fam %in% c(5:8), 0L)]
  
  
  # H6: ACESSO ADEQUADO A AGUA DE REDE GERAL DE DISTRIBUICAO EM DOMICILIO PARTICULAR PERMANENTE
  D[, h6 := fcase(h3==1 & cod_abaste_agua_domic_fam==1, 1L,
                  h3 != 1 |  cod_abaste_agua_domic_fam != 1 | 
                    is.na(cod_abaste_agua_domic_fam), 0L)]
  
  # H7: ACESSO ADEQUADO A AGUA EM DOMICILIO PARTICULAR PERMANENTE
  D[, h7 :=  fcase((h3==1 & cod_abaste_agua_domic_fam==1), 1L,
                   (h3==1 & cod_abaste_agua_domic_fam==2), 1L,
                   (h3==1 & cod_abaste_agua_domic_fam==3), 1L,
                   h3 != 1 | is.na(cod_abaste_agua_domic_fam) | 
                     cod_abaste_agua_domic_fam == 4, 0L)]  
  
  
  # H8: DOMICILIO PARTICULAR PERMANENTE POSSUI BANHEIRO OU SANITARIO
  D[, h8 := fcase(h3==1 & cod_banheiro_domic_fam==1, 1L,
                  default = 0L)]
  
  # H9: ESGOTAMENTO SANITARIO ADEQUADO EM DOMICILIO PARTICULAR PERMANENTE
  D[, h9 := fcase(h3==1 & cod_banheiro_domic_fam==1 & cod_escoa_sanitario_domic_fam %in% c(1:2), 1L,
                  default = 0L)]
  
  # H10: LIXO COLETADO DE FORMA DIRETA EM DOMICILIO PARTICULAR PERMANENTE
  D[, h10 :=  fcase(h3==1 & cod_destino_lixo_domic_fam==1, 1L,
                    default = 0L)]
  
  # H11: LIXO COLETADO DE FORMA DIRETA OU INDIRETA EM DOMICILIO PzRTICULAR PERMANENTE
  D[, h11 := fcase(h3==1 & cod_destino_lixo_domic_fam %in% c(1:2), 1L,
                   default = 0L)]
  
  # H12: ACESSO A ELETRICIDADE COM MOTOR DE USO EM DOMICILIO PARTICULAR PERMANENTE
  D[, h12 := fcase(h3==1 & cod_iluminacao_domic_fam %in% c(1:2), 1L,
                   default = 0L)]
  
  # H13: ACESSO A ELETRICIDADE EM DOMICILIO PARTICULAR PERMANENTE
  D[, h13 := fcase(h3==1 & cod_iluminacao_domic_fam %in% c(1:3), 1L, 
                   default = 0L)]
  
  # H14: Domicilio particular permanente localizado em trecho de logradouro com cacamba/pavimentacao total
  D[, h14 := fcase(h3==1 & cod_calcamento_domic_fam==1, 1L,
                   default = 0L)]
  
  # H15: Domicilio particular permanente localizado em trecho de logradouro com cacamba/pavimentacao total ou parcial
  D[, h15 := fcase(h3==1 & cod_calcamento_domic_fam %in% c(1,2), 1L, default = 0L)]
  
  
  ################### BASE PESSOA E DOMICILIO
  J <- merge(D, B, by = c("cd_ibge", "cod_familiar_fam"))
  rm(D,B) 
  
  J <- as.data.table(J)
  
  # DESPTOTPC
  J[, desptotpc := fifelse(tot_pess==0, NA_real_, desptot/tot_pess)]
  
  # DESPALIMPC
  J[, despalimpc := fifelse(tot_pess==0, NA_real_, despalim/tot_pess)]
  
  # COMPONENTE: VULNERABILIDADE DA FAMILIA
  # GRUPO: CRIANCASS. ADOLESCENTES E JOVENS
  
  # V1: Ausencia de criancas
  J[, v1 := fifelse(aux_v1 > 0 & is.na(aux_v1)==FALSE,0,1)]
  
  # V2: Ausencia de crianca ou adolescente
  J[, v2 := fifelse(aux_v2 > 0 & is.na(aux_v2)==FALSE, 0 ,1)]
  
  # V3: Ausencia de crianca ou adolescente ou jovem
  J[, v3 := fifelse(aux_v3 > 0 & is.na(aux_v3)==FALSE, 0, 1)]
  
  #Media V1 V2 V3
  J[, sub_v1 := rowMeans(as.data.table(.(v1,v2,v3)), na.rm = TRUE)]
  
  # GRUPO: PORTADORES DE DEFICIENCIA
  # V4: Ausencia de deficientes e idosos
  J[, v4 := fifelse(aux_v4 > 0 & is.na(aux_v4)==FALSE, 0, 1)]
  
  # V5: Ausencia de idoso
  J[, v5 := fifelse(aux_v5>0 & is.na(aux_v5)==FALSE, 0, 1)]
  
  # V6: Ausencia de pessoas na familia internada ou abrigada em hospital. casa de saude. asilo. orfanato ou estabelecimento similar
  J[, v6 := fifelse(aux_v6>0 & is.na(aux_v6)==FALSE, 0, 1)]
  
  # Media V4 V5 V6
  J[, sub_v2 := rowMeans(as.data.table(.(v4, v5, v6)), na.rm = TRUE)]
  
  # GRUPO: DEPENDENCIA ECONOMICA
  # V7: Presenca de conjuge
  J[, v7 := fifelse(aux_v7>0 & is.na(aux_v7)==FALSE, 1, 0)]
  
  
  # v8: Mais da metade dos membros encontra-se em idade ativa
  J[, v8 := fifelse( (aux_v8a > (tot_pess/2)) & is.na(aux_v8a)==FALSE & is.na(tot_pess)==FALSE , 1, 0)]
  
  # Media V7 V8
  J[, sub_v3 := rowMeans(as.data.table(.(v7, v8)), na.rm = TRUE)]
  
  # GRUPO: PRESENCA DE JOVENS OU ADULTOS
  # V9: Presenca de pelo menos uma pessoa com 15 anos ou mais
  J[, v9 := fifelse(aux_v9>0 & is.na(aux_v9)==FALSE ,1 ,0)]
  
  # V10: Presenca de pelo menos uma pessoa com 18 anos ou mais
  J[, v10 :=  fifelse(aux_v10>0 & is.na(aux_v10)==FALSE, 1, 0)]
  
  # V11: Presenca de pelo menos uma pessoa com 21 anos ou mais
  J[, v11 := fifelse(aux_v11>0 & is.na(aux_v11)==FALSE, 1, 0)]
  
  
  #  Media V9 V10 V11
  J[, sub_v4 := rowMeans(as.data.table(.(v9, v10, v11)), na.rm = TRUE)]
  
  # GRUPO: CONVIVENCIA FAMILIAR
  # V12: Ausencia de criancas com ate nove anos que nao sao filhos ou enteado do responsavel pela unidade familiar
  J[, v12 := fifelse(aux_v12>0 & is.na(aux_v12)==FALSE,0,1)]
  
  # V13: Ausencia de criancas com ate nove anos que seja outro parente ou nao parente
  J[, v13 := fifelse(aux_v13>0 & is.na(aux_v13)==FALSE,0,1)]
  
  
  # Media V12 V13
  J[, sub_v5 := rowMeans(as.data.table(.(v12, v13)), na.rm = TRUE)]
  
  
  
  #GRUPO: MIGRACAO
  # V14: Responsavel pela familia nasceu neste muncipio
  J[, v14 := fifelse(aux_v14>0 & is.na(aux_v14)==FALSE, 1, 0)]
  
  # V15: Ausencia de criancas ou adolescentes com ate 14 anos que nasceu em outro municipio
  J[, v15 := fifelse(aux_v15>0 & is.na(aux_v15)==FALSE,0,1)]
  
  # Media V14 V15
  J[, sub_v6 := rowMeans(as.data.table(.(v14, v15)), na.rm = TRUE)]
  
  
  # GRUPO: COMUNIDADES TRADICIONAIS
  # V16: Familia que nao e indigena nem quilombola
  J[, v16 := fifelse(aux_v16>0 & is.na(aux_v16)==FALSE,1,0)]
  
  # Media V16
  J[, sub_v7 := rowMeans(as.data.table(.(v16)), na.rm = TRUE)]
  
  # MEDIA DE VULBERABILIDADE DA FAMILIA
  J[, comp1 := rowMeans(as.data.table(.(sub_v1, sub_v2, sub_v3, sub_v4, sub_v5, sub_v6, sub_v7)), na.rm = TRUE)]
  
  
  
  ##
  # COMPONENTE: ACESSO AO CONHECIMENTO
  # GRUPO:ANALFABETISMO
  # C1: Ausencia de adultos analfabetos
  J[, c1 := fifelse(aux_c1>0 & is.na(aux_c1)==FALSE, 0, 1)]
  
  # C2: Ausencia de adultos analfabetos funcionais
  J[, c2 := fifelse(aux_c2>0 & is.na(aux_c2)==FALSE, 0, 1)]
  
  # C3: Presenca de  pelo menos uma pessoa com 15 anos ou mais alfabetizada
  J[, c3 := fifelse(aux_c3>0 & is.na(aux_c3)==FALSE, 1, 0)]
  
  # C4: Presenca de pelo menos uma pessoa com 15 anos ou mais que frequenta ou que tenha frequentado a escola
  J[, c4 := fifelse(aux_c4>0 & is.na(aux_c4)==FALSE, 1, 0)]
  
  # C5: Presenca de pelo menos uma pessoa com 15 anos ou mais alfabetizada. que frequenta ou tenha frequentado a escola
  J[, c5 := fifelse(aux_c5>0 & is.na(aux_c5)==FALSE, 1, 0)]
  
  # Media C1 C2 C3 C4 C5
  J[, sub_c1 := rowMeans(as.data.table(.(c1, c2, c3, c4, c5)), na.rm = TRUE)]
  
  
  # GRUPO: ESCOLARIDADE
  # C6: PRESENCA DE PELO MENOS UM ADULTO COM FUNDAMENTAL COMPLETO
  J[, c6 := fifelse(aux_c6>0 & is.na(aux_c6)==FALSE,1,0)]
  
  # C7: PRESENCA DE PELO MENOS UM ADULTO COM SECUNDARIO COMPLETO
  J[, c7 := fifelse(aux_c7>0 & is.na(aux_c7)==FALSE,1,0)]
  
  # C8: PRESENCA DE PELO MENOS UM ADULTO COM ALGUMA EDUCACAO SUPERIOR
  J[, c8 := fifelse(aux_c8>0 & is.na(aux_c8)==FALSE,1,0)]
  
  # Media C6 C7 C8
  J[, sub_c2 := rowMeans(as.data.table(.(c6, c7, c8)), na.rm = TRUE)]
  
  # Media do componente 2
  J[, comp2 := rowMeans(as.data.table(.(sub_c1,sub_c2)), na.rm = TRUE)]
  
  
  # COMPONENTE: ACESSO AO TRABALHO
  # GRUPO: DISPONIBILIDADE DE TRABALHO
  # T1: PRESENCA DE PELO MENOS UM MEMBRO EM IDADE ATIVA
  J[, t1 := fifelse(aux_t1>0  & is.na(aux_t1)==FALSE,1,0)]
  
  # T2: MAIS DA METADE DOS MEMBROS EM IDADE ATIVA ENCONTRAM-SE OCUPADOS NA SEMANA ANTERIOR A PESQUISA
  J[, t2:= fifelse(aux_t2 > aux_t1/2  & is.na(aux_t2)==FALSE, 1, 0)]
  
  # Media T1 T2
  J[, sub_t1 := rowMeans(as.data.table(.(t1, t2)), na.rm = TRUE)]
  
  # GRUPO: QUALIDADE DO POSTO DE TRABALHO
  # T3: PRESENCA DE PELO MENOS UM OCUPADO NO SETOR FORMAL
  J[, t3 := fifelse(aux_t3 > 0  & is.na(aux_t3)==FALSE,1,0)]
  
  # T4: PRESENCA DE PELO MENOS UM OCUPADO EM ATIVIDADE NAO AGRICOLA
  J[, t4 := fifelse(aux_t4 > 0  & is.na(aux_t4)==FALSE,1,0)]
  
  #Media T3 T4
  J[, sub_t2 := rowMeans(as.data.table(.(t3, t4)), na.rm = TRUE)]
  
  
  # GRUPO: REMUNERACAO
  # T5: PRESENCA DE PELO MENOS UM OCUPADO COM RENDIMENTO SUPERIOR A 1 SALARIO MINIMO
  J[, t5 := fifelse(aux_t5 > 0 & is.na(aux_t5)==FALSE, 1, 0)]
  
  # T6: PRESENCA DE PELO MENOS UM OCUPADO COM RENDIMENTO SUPERIOR A 2 SALARIO MINIMO
  J[, t6 := fifelse(aux_t6 > 0 & is.na(aux_t6)==FALSE, 1, 0)]
  
  # Media T5 T6
  J[, sub_t3 := rowMeans(as.data.table(.(t5, t6)), na.rm = TRUE)]
  
  # Media componente 3
  J[, comp3 := rowMeans(as.data.table(.(sub_t1, sub_t2, sub_t3)), na.rm = TRUE)]
  
  
  # COMPONENTE: DISPONIBILIDADE DE RECURSOS
  # GRUPO: Existencia de renda e despesas;
  # R1: Familia tem alguma despesa mensal
  J[, r1 := fifelse(desptot > 0 & is.na(desptot)==FALSE, 1, 0 )]
  
  
  #R2: Familia possue alguma renda. excluindo-se as transferencias
  J[, r2 := fifelse( (renda_ntrans > 0 | is.na(renda_ntrans)==TRUE ) & is.na(desptot)==FALSE , 1, 0)]
  
  #R3: Familia possue alguma renda
  J[, r3 := fifelse(renda_alg > 0 & is.na(renda_alg)==FALSE, 1, 0)]
  
  # Medias R1 R2 R3
  J[, sub_r1 := rowMeans(as.data.table(.( r1, r2, r3)), na.rm = TRUE)]
  
  # GRUPO: Extrema pobreza
  # R4: Despesa familiar per capita superior a linha de extrema pobreza
  J[, r4 := fifelse(desptotpc > lin_pextr & is.na(desptotpc)==FALSE , 1, 0)]
  
  # R5: Renda familiar per capita superior a linha da extrema pobreza;
  J[, r5 := fifelse(renda_media > lin_pextr & is.na(renda_media)==FALSE ,1 ,0)]
  
  # R6: Despesa com alimentos. higiene e limpeza superior a linha de extrema pobreza
  J[, r6 := fifelse(despalimpc > lin_pextr & is.na(despalimpc)==FALSE, 1, 0)]
  
  # Medias R4 R5 R6
  J[, sub_r2 := rowMeans(as.data.table(.(r4, r5, r6)), na.rm = TRUE)]
  
  # GRUPO: POBREZA
  # R7: Despesa familiar per capita superior a linha de pobreza
  J[, r7 := fifelse(desptotpc > lin_pobreza & is.na(desptotpc)==FALSE, 1,0)]
  
  # R8: Renda familiar per capita superior a linha de pobreza
  J[, r8 := fifelse(renda_media > lin_pobreza & is.na(renda_media)==FALSE, 1, 0)]
  
  # Medias R7 R8
  J[, sub_r3 := rowMeans(as.data.table(.(r7, r8)), na.rm = TRUE)]
  
  # GRUPO: CAPACIDADE DE GERACAO DE RENDA
  # R9: Maior parte da renda familiar nao advem de transferencia
  J[, r9 := fifelse(renda_transf<(renda/2) & is.na(renda_transf)==FALSE, 1, 0)]
  
  # Medias
  J[, sub_r4 := r9]
  
  # MEDIA DO COMPONENTE 4
  J[, comp4 := rowMeans(as.data.table(.(sub_r1, sub_r2, sub_r3, sub_r4)), na.rm = TRUE)]
  
  
  #COMPONENTE: DESENVOLVIMENTO INFANTIL
  # GRUPO: TRABALHO PRECOCE
  # D1: AUSENCIA DE CRIANCAS COM MENOS DE 12 ANOS TRABALHANDO
  J[, d1 := fifelse(aux_d1>0 & is.na(aux_d1)==FALSE, 0, 1)]
  
  # D2: AUSENCIA DE CRIANCAS COM MENOS DE 14 ANOS TRABALHANDO
  J[, d2 := fifelse(aux_d2>0 & is.na(aux_d2)==FALSE, 0, 1)]
  
  # D3: AUSENCIA DE CRIANCAS COM MENOS DE 16 ANOS TRABALHANDO
  #gen d3=1
  J[, d3 := fifelse(aux_d3>0 & is.na(aux_d3)==FALSE, 0 , 1)]
  
  # Medias D1 D2 D3
  J[, sub_d1 := rowMeans(as.data.table(.(d1, d2, d3)), na.rm = TRUE)]
  
  # GRUPO: ACESSO A ESCOLA
  # D4: Ausencia de crianca de 4-6 anos fora da escola
  J[, d4 := fifelse(aux_d4>0 & is.na(aux_d4)==FALSE, 0, 1)]
  
  # D5: Ausencia de crianca de 7-14 anos fora da escola
  J[, d5 := fifelse(aux_d5 > 0 & is.na(aux_d5)==FALSE, 0, 1)]
  
  # D6: Ausencia de pelo menos uma crianca de 7-17 anos fora da escola
  #gen d6=1
  J[, d6 := fifelse(aux_d6>0 & is.na(aux_d6)==FALSE, 0, 1)]
  
  # Medias D4 D5 D6
  J[, sub_d2 := rowMeans(as.data.table(.(d4, d5, d6)), na.rm = TRUE)]
  
  # GRUPO: PROGRESSO ESCOLAR
  # D7: Ausencia de crianca com ate 14 anos com mais de 2 anos de atraso
  J[, d7 := fifelse(aux_d7>0 & is.na(aux_d7)==FALSE, 0,1)]
  
  # D8: Ausencia de pelo menos um adolescente de 10 a 14 anos analfabeto
  J[, d8 := fifelse(aux_d8 > 0 & is.na(aux_d8)==FALSE , 0, 1)]
  
  
  # D9: Ausencia de pelo menos um jovem de 15 a 17 anos analfabeto
  J[, d9 := fifelse(aux_d9 > 0 & is.na(aux_d9)==FALSE, 0 ,1)]
  
  # Medias D7 D8 D9
  J[, sub_d3 := rowMeans(as.data.table(.(d7, d8, d9)), na.rm = TRUE)]
  
  # MEDIA DO COMPONENTE 5
  J[, comp5 := rowMeans(as.data.table(.(sub_d1, sub_d2, sub_d3)), na.rm = TRUE)]
  
  
  #  GRUPO: PROPRIEDADE DO DOMICILIO
  # H1: DOMICILIO PARTICULAR OU COLETIVO
  # H2: DOMICILIO PARTICULAR PERMANENTE OU PROVISORIO
  # H3: DOMICILIO PARTICULAR PERMANENTE
  # Medias H1. H2 e H3
  J[, sub_h1 := rowMeans(as.data.table(.(h1, h2, h3)), na.rm = TRUE)]
  
  
  #  H4:DENSIDADE DE ATE 2 MORADORES POR DORMITORIO
  #  Media H4
  J[, sub_h2 := h4]
  #  GRUPO:ABRIGALIDADE
  #  H5: MATERIAL DE CONSTRUCAO PERMANENTE
  #  Medias H5
  J[, sub_h3 := h5]
  #  GRUPO: ACESSO ADEQUADO A AGUA
  #  H6: ACESSO ADEQUADO A AGUA DE REDE GERAL DE DISTRIBUICAO
  #  H7: ACESSO ADEQUADO A AGUA
  #  Medias H6 H7
  J[, sub_h4 := rowMeans(as.data.table(.(h6, h7)), na.rm = TRUE)]
  #  GRUPO: ACESSO ADEQUADO A ESGOTAMENTO SANITARIO
  #  H8: DOMICILIO POSSUI BANHEIRO OU SANITARIO
  #  H9: ESGOTAMENTO SANITARIO ADEQUADO
  #  Medias H8 H9
  J[, sub_h5 := rowMeans(as.data.table(.(h8, h9)), na.rm = TRUE)]
  #  GRUPO: ACESSO A COLETA DE LIXO
  #  H10: LIXO COLETADO DE FORMA DIRETA
  #  H11: LIXO COLETADO DE FORMA DIRETA E INDIRETA
  #  Medias
  J[, sub_h6 := rowMeans(as.data.table(.(h10, h11)), na.rm = TRUE)]
  #  GRUPO: ACESSO A ELETRICIDADE
  #  H12: ACESSO A ELETRICIDADE COM MOTOR DE USO
  #  H13: ACESSO A ELETRICIDADE
  #  Medias H12 H13
  J[, sub_h7 := rowMeans(as.data.table(.(h12, h13)), na.rm = TRUE)]
  #  GRUPO: PAVIMENTACAO
  #  H14: Domicilio localizado em trecho de logradouro com cacamba/pavimentacao total
  #  H15: Domicilio localizado em trecho de logradouro com cacamba/pavimentacao total ou parcial
  #  Medias H14 H15
  J[, sub_h8 := rowMeans(as.data.table(.(h14, h15)), na.rm = TRUE)]
  #  Media Condicoes habitacionais
  J[, comp6 := rowMeans(as.data.table(.(sub_h1, sub_h2, sub_h3, sub_h4, sub_h5, sub_h6, sub_h7, sub_h8)), na.rm = TRUE)]
  # IDF
  J[, idf := rowMeans(as.data.table(.(comp1, comp2, comp3, comp4, comp5, comp6)), na.rm = TRUE)]
  #Preparando Saídas
  out <- J[,.(cd_ibge, cod_familiar_fam, num_nis_pessoa_atual, idf, v1, v2, v3, sub_v1, v4,
              v5, v6, sub_v2,	v7,	v8,	sub_v3,	v9,	v10,	v11,	sub_v4, v12, v13,	sub_v5,
              v14, v15,	sub_v6,	v16,	sub_v7,	comp1,	c1,	c2,	c3,	c4,	c5,	sub_c1, c6,	 c7,
              c8,	sub_c2,	comp2,	t1,	t2,	sub_t1,	t3,	t4,	sub_t2,	t5,	t6,	sub_t3,	comp3,
              r1,	r2,	r3,	sub_r1,	r4,	r5,	r6,	sub_r2,	r7,	r8,	sub_r3,	r9,	sub_r4,	comp4,
              d1, d2,	d3,	sub_d1,	d4,	d5,	d6,	sub_d2,	d7,	d8,	d9,	sub_d3,	comp5, sub_h1,
              sub_h2,	sub_h3,	sub_h4,	sub_h5,	sub_h6,	sub_h7,	sub_h8,	comp6, h1, h2, h3,
              h4, h5, h6,	h7,	h8,	h9,	h10, h11,	h12, h13, h14, h15)]
  return(out)
  
}# fim da função





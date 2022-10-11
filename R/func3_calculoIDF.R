#### Funcionalidade 3 : Definição de parâmetros e cálculo do IDF #####
# Responsável: Victor Toscano

calculoIDF_ui <- function(id) {
  ns <- NS(id)
  tagList(
    br(),
    sidebarPanel(width = 12,
                 strong('Instruções'),
                 p('Preencha os parâmetros definidos abaixo:'),
                 p('- Linha de Extrema pobreza: valor (em reais por mês) da linha de extrema pobreza definida para a politica.'),
                 p('- Linha de pobreza: valor (em reais por mês) da linha de pobreza definida para a politica.'),
                 p('- Salário mínimo vigente: é o salário mínimo definido por lei na data de referência.'),
                 p('- Data dos dados: é a data de referência do Cadastro Único. Será considerado como o último dia de atualização dos cadastros.')
    ),
    sidebarPanel(width = 12,
                 fluidRow(
                   column(width = 3,
                          numericInput(inputId = ns("lin_pextr"),
                                       label = "Linha de extrema pobreza:",
                                       value = 155L)
                   ),
                   column(width = 3,
                          numericInput(inputId = ns("lin_pobreza"),
                                       label = "Linha de pobreza:",
                                       value = 450L)
                   ),
                   column(width = 3,
                          numericInput(inputId = ns("sal_min"),
                                       label = "Salário mínimo vigente:",
                                       value = 1100L)
                   ),
                   column(width = 3,
                          dateInput(inputId = ns("dataref_dados"),
                                    label = "Data dos dados:",
                                    format = "dd/mm/yyyy",
                                    language = "pt-BR"),
                          textOutput(ns('data_ref')))
                 ),
                 fluidRow(
                   column(width = 3,
                          actionButton(inputId = ns("calcularIDF"),
                                       label = "Calcular"),
                          textOutput(ns('teste_tempo')))
                 )
    ),
    br(),
    mainPanel(
      column(width = 12,
             textOutput(ns("resultado")),
             br())
    ),
    fluidRow(
      column(width = 12,
             tableOutput(ns("tabela")))
    ),
    br(),
    mainPanel(width = 12,
              useShinyjs(),
              htmlOutput(ns("visualizacao_erro"))),
    br(),
    
  )
}

calculoIDF_server <- function(id, dados, inpc) {
  moduleServer(id, function(input, output, session) {
    
    # verificando o último mês deflatado
    ultima_mes_deflatado <- reactive(
      tail(inpc$date, 1)
    )
    
    output$data_ref <- renderText({
      sprintf('última data em que o INPC foi divulgado: %s', paste0(lubridate::month(ultima_mes_deflatado(), label = T),'/', lubridate::year(ultima_mes_deflatado()) ))
    })
    
    # se a data está adqueada ou não
    
    resultado_data <- reactive({
      (lubridate::ymd(ultima_mes_deflatado()) + months(1)) < lubridate::ymd(input$dataref_dados)
    })
    
    
    # chamando a função calcula IDF
    source("R/func_aux2_atualiza_deflator.R", encoding = "UTF-8")
    source("R/func_aux3_calcula_IDF.R", encoding = "UTF-8")
    
    calculo <- eventReactive(input$calcularIDF, {
      IDFcalc <- IDF(
        dados = dados()$base,
        lin_pextr = input$lin_pextr,
        lin_pobreza = input$lin_pobreza,
        sal_min = input$sal_min, 
        dataref_dados = as.character(lubridate::ymd(input$dataref_dados)),
        ano_inicial = as.character(lubridate::year(input$dataref_dados)-4),
        inpc = inpc
      )
      return(IDFcalc)
    })
    
    IDF <- observeEvent(input$calcularIDF,{
      
      showModal(modalDialog("Calculando...", footer=NULL))
      
      
      if (resultado_data() == TRUE) {
        showModal(modalDialog(
          title = "Erro: data inadequada!",
          "Verifique se a data dos dados ultrapassa o último mês em que o INPC foi divulgado.",
          easyClose = TRUE,
          footer = NULL
        ))
      } else {
        req(calculo())
        dado_calculado <- calculo()
        removeModal()
        
        
        if (is.null(dado_calculado)) {
          output$resultado <-  renderText("Ocorreu algum problema para calcular o IDF.")
        } else {
          output$resultado <-  renderText("O cálculo terminou. Verifique as primeiras linhas da tabela antes de continuar.")
          output$tabela <- renderTable(head(dado_calculado[1:4],5))
          colunas_dado_calculado <- dado_calculado %>% 
            select(cod_familiar_fam) %>% unique() %>% nrow()
          
          if (colunas_dado_calculado != dados()$registro_familiar) {
            output$visualizacao_erro <- renderUI(
              tagList(
                hr(),
                # título layout visualização
                strong('Discrepância na base calculada'),
                
                # botões layout visualização
                fluidRow(
                  br(),
                  column(4, 
                         htmlOutput(session$ns('resultado_erro'))),
                ) ) )
            
            shinyjs::show("visualizacao")
            
            
            # output resultado registrado
            output$resultado_erro <- renderUI(
              HTML(paste0(
                "<font color=\"#9b2226\"><b>",
                'Total de Famílias com IDF calculado: ', 
                colunas_dado_calculado,
                "</b></font>")
              )
            )
            
          } else {
            output$visualizacao_erro <- renderUI(
              tagList(
                hr(),
                # título layout visualização
                strong('Informações da base calculada'),
                
                # botões layout visualização
                fluidRow(
                  br(),
                  column(4, 
                         htmlOutput(session$ns('resultado_erro'))),
                ) ) )
            
            shinyjs::show("visualizacao")
            
            
            # output resultado registrado
            output$resultado_erro <- renderUI(
              HTML(paste0(
                "<font color=\"#9932CC\"><b>",
                'Total de Famílias com IDF calculado: ', 
                colunas_dado_calculado,
                "</b></font>")
              )
            )
          }
          
          
        } 
        
      }
    })
    
    # SALVANDO O ARQUIVO PRONTO PARA O USO EM OUTRO MÓDULO
    arquivo_pronto <- reactive({
      if (is.null(dados())) {
        valor <- NULL
        return(valor)
      } else {
        dado_calculado <- calculo()
        return(dado_calculado)
      }
    })
    
    
    return(arquivo_pronto)
  })
}

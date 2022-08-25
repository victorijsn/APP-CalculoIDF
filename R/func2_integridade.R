#### Funcionalidade 2 : Integridade do arquivo #####
# Responsável: Vitória Sesana

source("R/func_aux0_verificar.R", encoding = 'utf-8')
source("R/func_aux1_ler_arrumar.R", encoding = 'utf-8')

# Dicionario dos nomes das colunas 
dicionario <- read.csv(file = 'bases/dicionario.csv', sep = ';')
dicionario <- dicionario %>%
  as_tibble()

# Módulo Ui 
integridade_ui <- function(id) {
  ns <- NS(id)
  
  # ui 
  tagList(
    # Layout Integridade ####
    br(),

    # Instruções 
    sidebarPanel(width = 12,
                 strong('Condições'),
                 p('O arquivo deve ter algumas condições para a realização do cálculo do idf:'),
                 p('- Possuir 53 colunas'),
                 p('- Nome das colunas'),
                 p('- Tipo das colunas')),
    
    
    # Botão & Resultado
    mainPanel(width = 12,
              # Botão Integridade dos arquivos
              actionButton(ns('botao_integridade'), 'Verificar Arquivo'),
              
              # Resultado da verificação do arquivo
              htmlOutput(ns("resultado"))),
    
    
    # Layout Visualização #### 
    mainPanel(width = 12,
              useShinyjs(),
              htmlOutput(ns("visualizacao")))
  )
}


# Módulo Server
integridade_server <- function(id, arquivo_entrada) {
  moduleServer(id, function(input, output, session) { 
    
    #1) BOTÃO INTEGRIDADE ####
    observeEvent(input$botao_integridade, {
      
      # mensagem de carregamento da verificação
      showModal(modalDialog('Carregando...', footer = NULL))
      
      # # mensagem de erro pro tipo de arquivo
      # 
      # if (arquivo_entrada()$tipo != "csv" & arquivo_entrada()$tipo != "zip") {
      #   showModal(modalDialog(
      #     title = "Erro: tipo de arquivo não válido! ",
      #     "Impossibilidade de verificação da integridade do arquivo, pois o arquivo não está em csv ou zipado",
      #     easyClose = TRUE,
      #     footer = NULL
      #   ))
      # 
      # } 
      
      # mensagem de erro quando o arquivo não estiver upado
      if (is.null(arquivo_entrada())) {
        showModal(modalDialog(
          title = "Erro: arquivo não encontrado! ",
          "Impossibilidade de verificação da integridade do arquivo devido ao não envio do mesmo.",
          easyClose = TRUE,
          footer = NULL
        ))
      } else {
        
        # chamando o arquivo upado
        resultado_verificacao <- verificar(dado = arquivo_entrada()$dado, tipo =arquivo_entrada()$tipo)
        removeModal()
        
        ## 2.1) RESULTADO DA VERIFICAÇÃO ##################
        
        # COMPATIVEL ####
        if (resultado_verificacao$erro_quantos == 0) {
          
          # alerta de mensagem
          showModal(modalDialog(
            title = "Tudo certo com a verificação.",
            "Você poderá visualizar o arquivo agora!",
            easyClose = TRUE,
            footer = NULL
          ))
          
          ## LAYOUT RESULTADO ####
          output$resultado <- renderUI(
            tagList(
              br(),
              hr(),
              fluidRow(
                column(10,
                       strong('Situação do arquivo')),
                column(2,
                       htmlOutput(session$ns('situacao'))
                )
              ),
              HTML('Verificação concluida com sucesso, o arquivo está nas condições desejáveis'),
              hr(),
            )
          )
          
          # cor palavra compativel 
          output$situacao <- renderUI(
            HTML(paste(
              "<font color=\"#0B932A\" 
            size = \"4\"
            ><b>",
              'Compatível',
              "</b></font>"
            ))
          )
          
          ## LAYOUT VISUALIZAÇÃO ####
          output$visualizacao <- renderUI(
            tagList(
              
              # título layout visualização
              h3('Visualização do Arquivo'),
              br(),
              
              # botões layout visualização
              fluidRow(
                column(4, 
                       actionButton(session$ns('botao_tabela'), 'Mostrar tabela')),
                column(4,
                       actionButton(session$ns('botao_registro'), 'Registro Total')),
                column(4, 
                       htmlOutput(session$ns('resultado_registrado')))
              ),
              br(),
              
    
              # tabela de visualização do arquivo upado
              htmlOutput(session$ns('tabela_final')),
            )
          )
          
          # mostrar o layout visualização caso esteja tudo certo
          shinyjs::show("visualizacao")

          #### REGISTRO TOTAL ####
          # calculando o total registrado
          registro_calculado <- eventReactive(input$botao_registro,{
            registro <- nrow(x = arquivo_entrada()$dado)
            return(registro)
          })
          
          # output resultado registrado
          output$resultado_registrado <- renderUI(
            HTML(paste0(
              "<font color=\"#0B4993\"><b>",
              'Total Registrados: ', 
              registro_calculado(),
              "</b></font>")
            )
          )
          
          
          #### TABELA DO ARQUIVO ####
          observeEvent(input$botao_tabela, {
            output$tabela_final <- renderUI(
              tagList(
                # espaço das colunas
                br(),
                
                fluidRow(
                  column(width = 4, 
                         tagList(
                           strong('Selecione as colunas'),
                           conditionalPanel(
                             session$ns('input.dataset'),
                             box(
                               style='width:270px; height:510px; overflow-y: scroll;',
                               checkboxGroupInput(session$ns("mostre_colunas"), "",
                                                  choices = NULL,
                                                  selected = NULL))))),
                  
                  column(width = 8,
                         box(
                           style='width:620px; overflow-x: scroll;',
                           DT::dataTableOutput(session$ns('tabela'))))
                ),
                
                br(),
                br()
              )
            )
            
            # selecionando colunas 
            selecao_colunas <- eventReactive(input$botao_tabela, {
              x <- colnames(x=arquivo_entrada()$dado)
              y <- c('NumNisPessoaAtual', 
                     'DtaNascPessoa', 
                     'CodParentescoRfPessoa', 
                     'CodFamiliarFam',
                     'VlrRendaMediaFam',
                     'QtdPessoasDomicFam')
              
              lista <- list('x' = x, 'y' = y)
              return(lista)
              })
            
            
            observe({
              updateCheckboxGroupInput(session, "mostre_colunas",
                                       choices = selecao_colunas()$x,
                                       selected = selecao_colunas()$y)
            })
            
            # Colunas selecionadas de acordo com o checkbox
            valores_selecionados <- reactive({
              colunas_selecionadas <- select(arquivo_entrada()$dado, input$mostre_colunas)
              return(colunas_selecionadas)
            })
            
            # Saída da tabela 
            output$tabela <- DT::renderDataTable(
              DT::datatable(data = valores_selecionados())
            )
          }) 
          
        }
        
        
        ## INCOMPATIVEL ####
        if (resultado_verificacao$erro_quantos != 0 | is.null(arquivo_entrada())) {
          ## LAYOUT RESULTADO ####
          output$resultado <- renderUI(
            tagList(
              br(),
              hr(),
              fluidRow(
                column(10,
                       strong('Situação do arquivo')),
                column(2,
                       htmlOutput(session$ns('situacao'))
                )),
              hr(),
              strong('Motivo(s): '),
              br(),
              
              # mostrar o motivo
              HTML(paste(resultado_verificacao$erro_quais, collapse = '<br/>')),
              
              # mostrar a tabela caso os nomes das colunas estiverem erradas
              fluidRow(
                column(width = 2),
                column(width = 6,
                       br(),
                       tableOutput(session$ns('tabela_nomes_errados'))
                ))
            )
          )
          
          # tabela nomes errados
          if (!is.null(resultado_verificacao$coluna_errada)) {
            output$tabela_nomes_errados <- renderTable(
              resultado_verificacao$nomes_achados
            ) 
          } else {
            output$tabela_nomes_errados <- renderTable(
              NULL
            ) 
          }
          
          # cor palavra incompativel 
          output$situacao <- renderUI(
            tagList(HTML(paste(
              "<font color=\"#930B20\" 
            size = \"4\"
            ><b>",
              
              'Incompatível',
              "</b></font>"
            )))
          )
          
          # escondendo o layout visualização
          shinyjs::hide("visualizacao")

          removeModal()
        }
      }
      
    })
    
    #2) SALVADO OS VALORES PARA USO EM OUTRO MÓDULO ####
    arquivo_verificado <- eventReactive(input$botao_integridade, {
      valor <- NULL
      verificacao <- verificar(dado = arquivo_entrada()$dado, tipo = arquivo_entrada()$tipo)$erro_quantos
      if (is.null(arquivo_entrada()) | verificacao != 0) {
        return(valor)
      } else {
        caminho <- arquivo_entrada()$caminho
        dado <- ler_arrumar(endereco_arquivo = caminho, endereco_dicionario = "bases/dicionario.csv")
        dado <- as.data.frame(dado)
        return(dado)
      }
    })
    
    return(arquivo_verificado)
    
  })
}
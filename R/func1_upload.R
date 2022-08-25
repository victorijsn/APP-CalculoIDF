#### Funcionalidade 1 : Upload do arquivo #####
# Responsável: Vitória Sesana

# Módulo Ui 
upload_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    # Layout Upload ####
    br(),
    
    # Instruções
    sidebarPanel(width = 12,
                 strong('Instruções'),
                 p('O arquivo deve ter alguns critérios:'),
                 p('- Tamanho max 100MB'),
                 p('- Tipo de arquivo: csv ou zip'),
                 p('- Separado por ponto e vírgula')
    ),
    
    mainPanel(width = 12,
              # Botão upar arquivo
              fileInput(ns("arquivo_upado"),
                        "Escolha o arquivo",
                        multiple = FALSE,
                        accept = c(".csv"),
                        buttonLabel = 'Upload',
                        placeholder = 'exemplo.csv'),
              
              # Situação do upload do arquivo
              textOutput(ns("situacao_arquivo_upado")),
              br(),
    ),
    
  )
}


# Módulo Server
upload_server <- function(id) {
  moduleServer(id, function(input, output, session, prefix = "") {
    
    # 1 Situação do botão arquivo upado ####
    situacao_upload <- reactive({
      validate(
        need(input$arquivo_upado, 'Arquivo não selecionado!')
      )
      validate(
        need(tipo_arquivo() == "csv" | tipo_arquivo() == "zip", "Arquivo não upado, favor selecionar arquivo em csv!")
      )
      validate(
        need(!isTruthy(input$arquivo_upado), 'Arquivo selecionado com sucesso!')
      )
    })
    
    output$situacao_arquivo_upado <- renderText({
      situacao_upload()
    })
    
    # 2 Tipo do arquivo upado ####
    tipo_arquivo <- reactive({
      tools::file_ext(input$arquivo_upado$datapath)
    })
    
    # 2 CHAMANDO O ARQUIVO UPADO ####
    arquivo <- reactive({
      if(tipo_arquivo() == "csv") {
        x <- read.csv(input$arquivo_upado$datapath, sep =';') 
        return(x)
      }
      
      if (tipo_arquivo() == "zip") {
        caminho_zip <- unzip(zipfile = input$arquivo_upado$datapath, exdir = './Data')
        x <- read.csv(caminho_zip, sep =';')
        return(x)
      }
    })
    
    # 3 CHAMANDO O CAMINHO DO ARQUIVO UPADO ####
    caminho <- reactive({
      input$arquivo_upado$datapath
    })
    
    
    # 4 SALVADO OS VALORES PARA USO EM OUTRO MÓDULO ####
    arquivo_entrada <- reactive({
      if (!isTruthy(input$arquivo_upado)) {
        valor <- NULL
        return(valor)
      } else {
        lista <- list("dado" = arquivo(), "caminho" = caminho(), "tipo" = tipo_arquivo())
        return(lista)
      }
    })
    
    return(arquivo_entrada)
  })
}

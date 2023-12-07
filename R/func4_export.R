#### Funcionalidade 5 : Exportar arquivo para download #####
# Responsável: Victor Toscano

download_ui <- function(id) {
  ns <- NS(id)
  tagList(
    br(),
    fluidRow(
      column(width = 4,
             downloadButton(ns("download1"), label = "Dowload sistema")),
      column(width = 4,
             downloadButton(ns("download2"), label = "Dowload base completa"))
    )
  )
}

download_server <- function(id, arquivo) {
  moduleServer(id, function(input, output, session) {
    
    dado_download1 <- reactive({
      if (!is.null(arquivo())) {
        colunas <- arquivo() %>% 
          mutate(ID = 1:n()) %>%  
          select(ID, cod_familiar_fam, idf)
        return(colunas)
      }
    })
    
    dado_download2 <- reactive({
      if (!is.null(arquivo())) {
        completa <- arquivo()
        return(completa)
      }
    })
    
    
    # Download da base pro sistema ----
    
    output$download1 <- downloadHandler(
      
      filename = function() {
        paste("idf_sistema_",lubridate::today(),".csv", sep = "")
      },
      
      content = function(file) {
        write_excel_csv2(dado_download1(), file, row.names = FALSE)
      }
    )
    
    output$download2 <- downloadHandler(
      
      filename = function() {
        paste("idf_basecompleta_",lubridate::today(),".csv", sep = "")
      },
      
      content = function(file) {
        write_excel_csv2(dado_download2(), file, row.names = FALSE)
      }
    )
  })
}

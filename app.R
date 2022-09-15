rm(list=ls())
# Bibliotecas -------------------------------------------------------------

library(shiny)
library(magrittr) 
library(data.table)
library(lubridate)
library(tidyverse)
library(data.table)
library(DT)
library(shinyjs)
library(shinydashboard)
library(readr)

# Módulos -----------------------------------------------------------------
source("R/func1_upload.R", encoding = "UTF-8")
source("R/func2_integridade.R", encoding = "UTF-8")
source("R/func3_calculoIDF.R", encoding = "UTF-8")
source("R/func4_export.R", encoding = "UTF-8")
# source("R/func_aux_inpc.R", encoding = "UTF-8")

# Em caso de pânico troque
source("R/defasada/func_aux_inpc_VITORIA.R", encoding = "UTF-8")


# Calculando INPC ---------------------------------------------------------
# inpc <- coletando_inpc_api()
inpc <- coletando_inpc()

# UI ----------------------------------------------------------------------
ui <- fluidPage(
  titlePanel("Cálculo do IDF para o programa Bolsa Capixaba"),
  h3("Passo 1: Upload do arquivo"),
  fluidRow(
    column(
      width = 12,
      upload_ui("func1_upload")
    )
  ),
  h3("Passo 2: Verificação do arquivo"),
  fluidRow(
    column(
      width = 12,
      integridade_ui("func2_integridade")
    )
  ),
  h3("Passo 3: Cálculo do IDF"),
  fluidRow(
    column(
      width = 12,
      calculoIDF_ui("func3_calculoIDF")
    )
  ),
  h3("Passo 4: Exportar"),
  fluidRow(
    column(
      width = 12,
      download_ui("func4_export")
    )
  )
)

# server ------------------------------------------------------------------
server <- function(input, output, session) {
  options(shiny.maxRequestSize=100*1024^2)
  
  dados_upload <- upload_server("func1_upload")
  
  dados <- integridade_server("func2_integridade", dados_upload)
  
  arquivo <- calculoIDF_server("func3_calculoIDF", dados, inpc)
  
  download_server("func4_export", arquivo)
  
}

shinyApp(ui, server)
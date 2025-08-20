#---------------------------
#- 1. IMPORTAR BIBLIOTECAS - 
#---------------------------

# Função para importar os pacotes ou instalar (caso não esteja instalado)
importa_pacotes <- function(pacotes) {
  for (pct in pacotes) {
    if (!require(pct, character.only = TRUE)) {
      install.packages(pct, dependencies = TRUE)
      library(pct, character.only = TRUE)
    }
  }
}

# Lista de pacotes a serem importados
pacotes <- c(
  "here",         # Evita problemas de caminhos dos arquivos
  "openxlsx",     # Permite trabalhar com Excel diretamente do R
  "openxlsx2",    # Permite trabalhar com Excel diretamente do R
  "tidyverse",    # Permite manipular dataframes 
  "data.table",   # Permite manipular arquivos .csv
  "reshape2",     # Permite converter long para wide e vice versa
  "glue",         # Permite combinar strings
  "googledrive",  # Permite conectar com o Google Drive
  "writexl"       # Permite exportar um dataframe para excel
)

# Importa os pacotes listados
importa_pacotes(pacotes)



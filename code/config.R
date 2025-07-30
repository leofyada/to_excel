#---------------------------
#- 1. IMPORTAR BIBLIOTECAS - 
#---------------------------

# Importa funções
source(here("code", "functions.R"))

# Lista de pacotes a serem importados
pacotes <- c(
  "here",      # Evita problemas de caminhos dos arquivos
  "openxlsx",  # Permite trabalhar com Excel diretamente do R
  "tidyverse", # Permite manipular dataframes 
  "data.table" # Permite manipular arquivos .csv
)

# Importa os pacotes listados
importa_pacotes(pacotes)



#---------------------------------------
#- 1. FUNÇÃO PARA IMPORTAR BIBLIOTECAS - 
#---------------------------------------

# Função para importar os pacotes ou instalar (caso não esteja instalado)
importa_pacotes <- function(pacotes) {
  for (pct in pacotes) {
    if (!require(pct, character.only = TRUE)) {
      install.packages(pct, dependencies = TRUE)
      library(pct, character.only = TRUE)
    }
  }
}

#------------------------------------------------
#- 2. FUNÇÃO PARA EXCLUIR AS COLUNAS DAS FONTES -
#------------------------------------------------

# Função para excluir de um dataframe qualquer coluna com "source"
exclui_source <- function(dataframe) {
  # Exclui todas as colunas que contém "source" no nome
  dataframe <- dataframe %>% 
    select(-contains("source"))
  # Retorna a base limpa
  return(dataframe)
}



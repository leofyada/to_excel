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

#----------------------------------------------------------------------
#- 3. FUNÇÃO PARA RETORNAR A DATA DE HOJE (PARA INCLUIR NOS ARQUIVOS) -
#----------------------------------------------------------------------

# Função para retornar a data de hoje
data_hoje <- function() {
  data_hoje <- Sys.Date()
  data_hoje_formatada <- format(data_hoje, format = "%Y%m%d")
  return(data_hoje_formatada)
}




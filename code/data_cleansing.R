#--------------------------------------------
#- PLANILHA 1: DADOS DOS PAÍSES (SEM FONTE) -
#--------------------------------------------

# Importa funções
source(here("code", "functions.R"))
# Remove colunas com "source"
df_parametros_limpa <- exclui_source(df_parametros)


#--------------------------------------------
#- PLANILHA 1: DADOS DOS PAÍSES (SEM FONTE) -
#--------------------------------------------

# Importa funções
source(here("code", "functions.R"))
source(here("code", "data_import.R"))

# Remove colunas com "source"
df_parametros_limpa <- exclui_source(df_parametros)
# Exporta base limpa
data.table::fwrite(x=df_parametros_limpa, file = here("data", "base_parametros_limpa.csv"))

#-----------------------------------------------------------
#- 1. CRIA A PLANILHA COM OS DADOS DOS PAÍSES E PARÂMETROS - 
#-----------------------------------------------------------

# Importa funções
source(here("code", "functions.R"))
# Importa a base limpa
df_parametros_limpa <- data.table::fread(here("data", "base_parametros_limpa.csv"))
# Lista de países
lista_paises <- df_parametros_limpa$country

# Teste
#lista_paises <- c("Argentina", "Bahamas")

# Gera arquivos em excel
for(pais in lista_paises) {
  funcao_excel(pais, here("output", "v0", glue("{data_hoje()}_{pais}_calculadora.xlsx")), df_parametros_limpa)
}






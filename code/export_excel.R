#-----------------------------------------------------------
#- 1. CRIA A PLANILHA COM OS DADOS DOS PAÍSES E PARÂMETROS - 
#-----------------------------------------------------------

# Importa funções
source(here("code", "functions.R"))

# A) Cria um arquivo Excel e adiciona uma planilha 

# Cria um workbook (arquivo Excel) 
wb <- createWorkbook()
# Adiciona uma planilha ao arquivo criado
addWorksheet(wb, "Planilha1")

# B) Inclui os dados iniciais na planilha 1

# Inclui a base de dados na planilha 1
writeData(wb, sheet = "Planilha1", x = df_parametros_limpa, startCol = 1, startRow = 1)
# Salva o arquivo 
saveWorkbook(wb, file = here("data", paste0(data_hoje(), "_calculadora.xlsx")), overwrite = TRUE)

#------------------------------------------------------------------
#- 2. CRIA PLANILHA COM OS RESULTADOS DOS CÁLCULOS (COM FÓRMULAS) - 
#------------------------------------------------------------------




#-----------------------------------------------------------
#- 1. CRIA A PLANILHA COM OS DADOS DOS PAÍSES E PARÂMETROS - 
#-----------------------------------------------------------

# Importa funções
source(here("code", "functions.R"))

# Importa a base limpa
df_parametros_limpa <- data.table::fread(here("data", "silver", "base_parametros_limpa.csv"))
df_parametros_limpa_aj <- data.table::fread(here("data", "silver", "base_parametros_limpa_aj.csv"))
# Reshape
df_parametros_limpa_reshaped <- reshape2::melt(data = df_parametros_limpa, id.vars = c("country", "subregion"))
df_parametros_limpa_aj_reshaped <- reshape2::melt(data = df_parametros_limpa_aj, id.vars = c("country", "subregion"))

# Estilos
num_style_init <- createStyle(numFmt = "0", border = "TopBottomLeftRight", halign = "CENTER", valign = "CENTER")
num_style <- createStyle(numFmt = "$#,##0.00", border = "TopBottomLeftRight", halign = "CENTER", valign = "CENTER")
general_style <- createStyle(border = "TopBottomLeftRight")

sce0 <- createStyle(border = "TopBottomLeftRight", fgFill = "#ADD8E6")
sce1 <- createStyle(border = "TopBottomLeftRight", fgFill = "#FF7F7F")
sce2 <- createStyle(border = "TopBottomLeftRight", fgFill = "#FFFFC5")
sce3 <- createStyle(border = "TopBottomLeftRight", fgFill = "#90EE90")

# Lista de países
lista_paises <- df_parametros_limpa$country
# Teste
#lista_paises <- c("Argentina", "Bahamas")

# Gera arquivos em excel
for(pais_selecionado in lista_paises) {
  
  # Filtra apenas o país selecionado
  df_base_planilha <- df_parametros_limpa_reshaped %>% filter(country==pais_selecionado)
  df_base_planilha_aj <- df_parametros_limpa_aj_reshaped %>% filter(country==pais_selecionado)
  # Inclui uma coluna as referências das células
  df_base_planilha$cell_refs <- 1:nrow(df_base_planilha)
  df_base_planilha$cell_refs <- paste0("Inputs!D", df_base_planilha$cell_refs+1)
  
  # Cria workbook e planilha "Description"
  wb_description <- funcao_description()
  # Cria planilha "Inputs"
  wb_inputs <- funcao_inputs(pais = pais_selecionado, df_base = df_base_planilha_aj, wb = wb_description)
  # Cria planilhas com fórmulas
  wb_results <- funcao_formulas(wb = wb_inputs, df = df_base_planilha)
  
  # Salva o arquivo 
  saveWorkbook(wb = wb_results, file = here("output", "v3", glue("{data_hoje()}_{pais_selecionado}.xlsx")), overwrite = T)
  
}

# Remove tudo
rm(list = ls())
gc()



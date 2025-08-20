#--------------------------------------------
#- 1. IMPORTAR BASE DE DADOS COM PARÂMETROS -
#--------------------------------------------

# Importa arquivo com os parâmetros
df_parametros <- data.table::fread(here("data", "base_parametros.csv"))

# Ajusta variável source Unesco
df_parametros$source_number_of_students <- gsub("DATA UIS", "Unesco", df_parametros$source_number_of_students)
df_parametros$source_number_of_students_primary <- gsub("DATA UIS", "Unesco", df_parametros$source_number_of_students_primary)
df_parametros$source_number_of_students_secondary <- gsub("DATA UIS", "Unesco", df_parametros$source_number_of_students_secondary)
df_parametros$source_number_of_teachers <- gsub("DATA UIS", "Unesco", df_parametros$source_number_of_teachers)

lista_rural <- c("Bahamas","Barbados","Bolivia (Plurinational State of)","Dominican Republic","El Salvador","Guatemala","Guyana","Haiti","Jamaica","Mexico","Nicaragua","Trinidad and Tobago","Venezuela (Bolivarian Republic of)")
df_parametros$rural_schools <- ifelse(
  df_parametros$country %in% lista_rural, "Estimated based on Latam average", ""
)


#df_parametros <- reshape2::melt(data = df_parametros, id.vars = c("country", "subregion"))
#writexl::write_xlsx(x=df_parametros, path=glue("/Users/leonardoyada/Downloads/{Sys.Date()}df_parametros.xlsx"))
#df_product_2_final <- reshape2::melt(data = df_product_2_final, id.vars = c("country", "subregion"))
#writexl::write_xlsx(x=df_product_2_final, path=glue("/Users/leonardoyada/Downloads/{Sys.Date()}df_produto_2_final.xlsx"))

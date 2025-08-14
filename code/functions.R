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

#------------------------------------------------------------
#- 4. FUNÇÃO PARA CRIAR O WORKBOOK E A PLANILHA DESCRIPTION - 
#------------------------------------------------------------

# Função para criar o workbook e a planilha de descrição do arquivo
funcao_description <- function() {
  
  # Cria um workbook (arquivo Excel) 
  wb <- createWorkbook()
  # Adiciona uma planilha denominada "Description" (instruções iniciais)
  addWorksheet(wb, "Description")
  
  # Define os estilos utilizados na planilha
  estilo_titulo <- createStyle(fontSize = 18, textDecoration = "bold", halign = "left")
  estilo_secao <- createStyle(fontSize = 14, textDecoration = "bold", halign = "left")
  estilo_texto <- createStyle(fontSize = 12, halign = "left")
  
  # Título (conteúdo e estilo)
  writeData(wb, "Description", "📘 Calculator Overview", startRow = 1, startCol = 1)
  addStyle(wb, "Description", style = estilo_titulo, rows = 1, cols = 1)
  
  # Seção 1: Objetivo
  addStyle(wb, "Description", style = estilo_secao, rows = 3, cols = 1)
  writeData(wb, "Description", "Purpose of the Calculator", startRow = 3, startCol = 1)
  writeData(wb, "Description", "This calculator estimates the cost of buying technological products and services in different scenarios.", startRow = 4, startCol = 1)
  addStyle(wb, "Description", style = estilo_texto, rows = 4, cols = 1)
  
  # Seção 2: Planilhas
  writeData(wb, "Description", "Sheet Overview", startRow = 6, startCol = 1)
  addStyle(wb, "Description", style = estilo_secao, rows = 6, cols = 1)
  
  sheets <- c(
    "1. Inputs: where you input your raw data.",
    "2. Results: automatically calculates outputs.",
    "3. TotalDimension: automatically calculates total values per dimension."
  )
  
  writeData(wb, "Description", sheets, startRow = 7, startCol = 1)
  addStyle(wb, "Description", style = estilo_texto, rows = 7:9, cols = 1, gridExpand = TRUE)
  
  # Largura da coluna 1
  setColWidths(wb, "Description", cols = 1, widths = 90)
  
  # Retorna workbook
  return(wb)

}
# Função para adicionar a planilha de inputs 
funcao_inputs <- function(pais, wb, df_base_planilha_aj) {
  
  # Adiciona uma planilha ao arquivo criado
  addWorksheet(wb, "Inputs")
  # Inclui a base de dados na planilha 1
  writeData(wb, sheet = "Inputs", x = df_base_planilha_aj, startCol = 1, startRow = 1)
  # Estabelece o tamanho da coluna 3, que possui os nomes das variáveis
  setColWidths(wb, sheet = "Inputs", cols = 3, widths = 50) 
  
  # Estabelece os estilos das colunas da planilha
  
  # Inclui bordas  
  addStyle(wb, sheet="Inputs", general_style, rows = 1:64, cols = 1)
  addStyle(wb, sheet="Inputs", general_style, rows = 1:64, cols = 2)
  addStyle(wb, sheet="Inputs", general_style, rows = 1:64, cols = 3)
  # Ajusta as variáveis numéricas
  addStyle(wb, sheet="Inputs", num_style_init, rows = 1:64, cols = 4)
  
  # Retorna workbook
  return(wb)
  
}
# Função para adicionar as planilhas com fórmulas
funcao_formulas <- function(wb, df) {
  
  # Mapeia as referências das células
  map_df <- df %>%
    distinct(variable, .keep_all = TRUE) %>%
    select(variable, cell_refs) %>%
    deframe()  
  map_df <- map_df[order(nchar(names(map_df)), decreasing = TRUE)]
  
  #---- TotalperItem -----
  
  # Adiciona uma planilha ao arquivo criado
  addWorksheet(wb, "TotalperItem")
  
  # Nome do país
  writeFormula(wb, sheet="TotalperItem", x=paste0("=Inputs!A", 1), startCol=1, startRow=1)
  writeFormula(wb, sheet="TotalperItem", x=paste0("=Inputs!A", 2), startCol=2, startRow=1)
  # Nome da subregião
  writeFormula(wb, sheet="TotalperItem", x=paste0("=Inputs!B", 1), startCol=1, startRow=2)
  writeFormula(wb, sheet="TotalperItem", x=paste0("=Inputs!B", 2), startCol=2, startRow=2)
  
  #---- TotalperItem: Cenário geral ----
  
  # Dimensão 1 - Fiber expansion
  writeData(wb, sheet = "TotalperItem", x="dim_1_price_fiber_exp", startCol=1, startRow=3)
  
  dim_1_price_fiber_exp <- "itu_pop_between_25_50*number_of_schools*parameter_fiber_exp*reference_fiber_exp_price"
  dim_1_price_fiber_exp <- str_replace_all(dim_1_price_fiber_exp, map_df)
  writeFormula(wb, sheet="TotalperItem", x=paste0("=", dim_1_price_fiber_exp), startCol=2, startRow=3)
  
  # Dimensão 1 - Satellite service
  writeData(wb, sheet = "TotalperItem", x="dim_1_price_satellite_service", startCol=1, startRow=4)
  
  dim_1_price_satellite_service <- "itu_pop_above_50*number_of_schools*reference_satellite_link_price*type_opex"
  dim_1_price_satellite_service <- str_replace_all(dim_1_price_satellite_service, map_df)
  writeFormula(wb, sheet="TotalperItem", x=paste0("=", dim_1_price_satellite_service), startCol=2, startRow=4)
  
  # Dimensão 1 - Fiber link
  writeData(wb, sheet = "TotalperItem", x="dim_1_price_fiber_link", startCol=1, startRow=5)
  
  dim_1_price_fiber_link <- "itu_pop_below_50*number_of_schools*(avg_student_per_school)*(reference_fiber_link_price/200)*type_opex"
  dim_1_price_fiber_link <- str_replace_all(dim_1_price_fiber_link, map_df)
  writeFormula(wb, sheet="TotalperItem", x=paste0("=", dim_1_price_fiber_link), startCol=2, startRow=5)
  
  # Dimensão 1 - Access point
  writeData(wb, sheet = "TotalperItem", x="dim_1_price_access_point", startCol=1, startRow=6)
  
  dim_1_price_access_point <- "(((number_of_students_primary/25)+primary_schools_cima*3)+((number_of_students_secondary/35)+secondary_schools_cima*3))*parameter_access_point*reference_access_point_price"
  dim_1_price_access_point <- str_replace_all(dim_1_price_access_point, map_df)
  writeFormula(wb, sheet="TotalperItem", x=paste0("=", dim_1_price_access_point), startCol=2, startRow=6)
  
  # Dimensão 1 - Firewall
  writeData(wb, sheet = "TotalperItem", x="dim_1_price_firewall", startCol=1, startRow=7)
  
  dim_1_price_firewall <- "number_of_schools*parameter_firewall*reference_firewall_price"
  dim_1_price_firewall <- str_replace_all(dim_1_price_firewall, map_df)
  writeFormula(wb, sheet="TotalperItem", x=paste0("=", dim_1_price_firewall), startCol=2, startRow=7)
  
  # Dimensão 1 - Nobreak
  writeData(wb, sheet = "TotalperItem", x="dim_1_price_nobreak", startCol=1, startRow=8)
  
  dim_1_price_nobreak <- "number_of_schools*parameter_nobreak*reference_nobreak_price"
  dim_1_price_nobreak <- str_replace_all(dim_1_price_nobreak, map_df)
  writeFormula(wb, sheet="TotalperItem", x=paste0("=", dim_1_price_nobreak), startCol=2, startRow=8)
  
  # Dimensão 1 - Switch
  writeData(wb, sheet = "TotalperItem", x="dim_1_price_switch", startCol=1, startRow=9)
  
  dim_1_price_switch <- "number_of_schools*parameter_switch*reference_switch_price"
  dim_1_price_switch <- str_replace_all(dim_1_price_switch, map_df)
  writeFormula(wb, sheet="TotalperItem", x=paste0("=", dim_1_price_switch), startCol=2, startRow=9)
  
  # Dimensão 1 - Rack 6U/8U
  writeData(wb, sheet = "TotalperItem", x="dim_1_price_rack", startCol=1, startRow=10)
  
  dim_1_price_rack <- "number_of_schools*parameter_rack*reference_rack_price"
  dim_1_price_rack <- str_replace_all(dim_1_price_rack, map_df)
  writeFormula(wb, sheet="TotalperItem", x=paste0("=", dim_1_price_rack), startCol=2, startRow=10)
  
  # Dimensão 2 - Device per students (Min Scenario)
  writeData(wb, sheet = "TotalperItem", x="dim_2_price_device_student_min", startCol=1, startRow=11)
  
  dim_2_price_device_student_min <- "(number_of_students_primary*parameter_min_scenario_primary*reference_tablet_price)+(number_of_students_secondary*parameter_min_scenario_secondary*reference_cloudbook_price)"
  dim_2_price_device_student_min <- str_replace_all(dim_2_price_device_student_min, map_df)
  writeFormula(wb, sheet="TotalperItem", x=paste0("=", dim_2_price_device_student_min), startCol=2, startRow=11)
  
  # Dimensão 2 - Device per students (Mild Scenario)
  writeData(wb, sheet = "TotalperItem", x="dim_2_price_device_student_mild", startCol=1, startRow=12)
  
  dim_2_price_device_student_mild <- "(number_of_students_primary*parameter_mild_scenario_primary*reference_tablet_price)+(number_of_students_secondary*parameter_mild_scenario_secondary*reference_cloudbook_price)"
  dim_2_price_device_student_mild <- str_replace_all(dim_2_price_device_student_mild, map_df)
  writeFormula(wb, sheet="TotalperItem", x=paste0("=", dim_2_price_device_student_mild), startCol=2, startRow=12)
  
  # Dimensão 2 - Device per students (Comprehensive Scenario)
  writeData(wb, sheet = "TotalperItem", x="dim_2_price_device_student_comprehensive", startCol=1, startRow=13)
  
  dim_2_price_device_student_comprehensive <- "(number_of_students_primary*parameter_comprehensive_primary*reference_tablet_price)+(number_of_students_secondary*parameter_comprehensive_secondary*reference_cloudbook_price)"
  dim_2_price_device_student_comprehensive <- str_replace_all(dim_2_price_device_student_comprehensive, map_df)
  writeFormula(wb, sheet="TotalperItem", x=paste0("=", dim_2_price_device_student_comprehensive), startCol=2, startRow=13)
  
  # Dimensão 2 - Devices per teacher 
  writeData(wb, sheet = "TotalperItem", x="dim_2_price_device_teacher", startCol=1, startRow=14)
  
  dim_2_price_device_teacher <- "number_of_teachers*parameter_device_per_teacher*reference_laptop_price"
  dim_2_price_device_teacher <- str_replace_all(dim_2_price_device_teacher, map_df)
  writeFormula(wb, sheet="TotalperItem", x=paste0("=", dim_2_price_device_teacher), startCol=2, startRow=14)

  # Dimensão 2 - Devices per school 
  writeData(wb, sheet = "TotalperItem", x="dim_2_price_device_school", startCol=1, startRow=15)
  
  dim_2_price_device_school <- "number_of_schools*parameter_device_per_school*reference_desktop_price"
  dim_2_price_device_school <- str_replace_all(dim_2_price_device_school, map_df)
  writeFormula(wb, sheet="TotalperItem", x=paste0("=", dim_2_price_device_school), startCol=2, startRow=15)
  
  # Dimensão 2 - Charging cart (Min Scenario)
  writeData(wb, sheet = "TotalperItem", x="dim_2_price_charging_cart_min", startCol=1, startRow=16)
  
  dim_2_price_charging_cart_min <- "((number_of_students_primary*parameter_min_scenario_primary)+(number_of_students_secondary*parameter_min_scenario_secondary))/30*reference_charging_cart_price"
  dim_2_price_charging_cart_min <- str_replace_all(dim_2_price_charging_cart_min, map_df)
  writeFormula(wb, sheet="TotalperItem", x=paste0("=", dim_2_price_charging_cart_min), startCol=2, startRow=16)
  
  # Dimensão 2 - Charging cart (Mild Scenario)
  writeData(wb, sheet = "TotalperItem", x="dim_2_price_charging_cart_mild", startCol=1, startRow=17)
  
  dim_2_price_charging_cart_mild <- "((number_of_students_primary*parameter_mild_scenario_primary)+(number_of_students_secondary*parameter_mild_scenario_secondary))/30*reference_charging_cart_price"
  dim_2_price_charging_cart_mild <- str_replace_all(dim_2_price_charging_cart_mild, map_df)
  writeFormula(wb, sheet="TotalperItem", x=paste0("=", dim_2_price_charging_cart_mild), startCol=2, startRow=17)
  
  # Dimensão 2 - Charging cart (Comprehensive Scenario)
  writeData(wb, sheet = "TotalperItem", x="dim_2_price_charging_cart_comprehensive", startCol=1, startRow=18)
  
  dim_2_price_charging_cart_comprehensive <- "((number_of_students_primary*parameter_comprehensive_primary)+(number_of_students_secondary*parameter_comprehensive_secondary))/30*reference_charging_cart_price"
  dim_2_price_charging_cart_comprehensive <- str_replace_all(dim_2_price_charging_cart_comprehensive, map_df)
  writeFormula(wb, sheet="TotalperItem", x=paste0("=", dim_2_price_charging_cart_comprehensive), startCol=2, startRow=18)
  
  # Dimensão 2 - Multimedia projector
  writeData(wb, sheet = "TotalperItem", x="dim_2_price_multimedia_projector", startCol=1, startRow=19)
  
  dim_2_price_multimedia_projector <- "number_of_schools*parameter_multimedia_projector*reference_multimedia_projector_price"
  dim_2_price_multimedia_projector <- str_replace_all(dim_2_price_multimedia_projector, map_df)
  writeFormula(wb, sheet="TotalperItem", x=paste0("=", dim_2_price_multimedia_projector), startCol=2, startRow=19)
  
  # Dimensão 2 - Headphones (Min Scenario)
  writeData(wb, sheet = "TotalperItem", x="dim_2_price_headphones_min_scenario", startCol=1, startRow=20)
  
  dim_2_price_headphones_min_scenario <- "((number_of_students_primary*parameter_min_scenario_primary)+(number_of_students_secondary*parameter_min_scenario_secondary))*reference_headphone_price"
  dim_2_price_headphones_min_scenario <- str_replace_all(dim_2_price_headphones_min_scenario, map_df)
  writeFormula(wb, sheet="TotalperItem", x=paste0("=", dim_2_price_headphones_min_scenario), startCol=2, startRow=20)
  
  # Dimensão 2 - Headphones (Mild Scenario)
  writeData(wb, sheet = "TotalperItem", x="dim_2_price_headphones_mild_scenario", startCol=1, startRow=21)
  
  dim_2_price_headphones_mild_scenario <- "((number_of_students_primary*parameter_mild_scenario_primary)+(number_of_students_secondary*parameter_mild_scenario_secondary))*reference_headphone_price"
  dim_2_price_headphones_mild_scenario <- str_replace_all(dim_2_price_headphones_mild_scenario, map_df)
  writeFormula(wb, sheet="TotalperItem", x=paste0("=", dim_2_price_headphones_mild_scenario), startCol=2, startRow=21)
  
  # Dimensão 2 - Headphones (Comprehensive Scenario)
  writeData(wb, sheet = "TotalperItem", x="dim_2_price_headphones_comprehensive", startCol=1, startRow=22)
  
  dim_2_price_headphones_comprehensive <- "((number_of_students_primary*parameter_comprehensive_primary)+(number_of_students_secondary*parameter_comprehensive_secondary))*reference_headphone_price"
  dim_2_price_headphones_comprehensive <- str_replace_all(dim_2_price_headphones_comprehensive, map_df)
  writeFormula(wb, sheet="TotalperItem", x=paste0("=", dim_2_price_headphones_comprehensive), startCol=2, startRow=22)
  
  # Dimensão 3 - Diagnostic Tool
  writeData(wb, sheet = "TotalperItem", x="dim_3_price_diagnostic_tool", startCol=1, startRow=23)
  
  dim_3_price_diagnostic_tool <- "reference_price_diagnostic_tool"
  dim_3_price_diagnostic_tool <- str_replace_all(dim_3_price_diagnostic_tool, map_df)
  writeFormula(wb, sheet="TotalperItem", x=paste0("=", dim_3_price_diagnostic_tool), startCol=2, startRow=23)
  
  # Dimensão 3 - LMS Platform
  writeData(wb, sheet = "TotalperItem", x="dim_3_price_lms_platform", startCol=1, startRow=24)
  
  dim_3_price_lms_platform <- "reference_price_lms"
  dim_3_price_lms_platform <- str_replace_all(dim_3_price_lms_platform, map_df)
  writeFormula(wb, sheet="TotalperItem", x=paste0("=", dim_3_price_lms_platform), startCol=2, startRow=24)
  
  # Dimensão 3 - Specialist Trainers (Scenario online-60)
  writeData(wb, sheet = "TotalperItem", x="dim_3_price_specialist_trainers_scenario_online60", startCol=1, startRow=25)
  
  dim_3_price_specialist_trainers_scenario_online60 <- "reference_price_trainers60*(number_of_graduated_teacher/parameter_teacher_class)"
  dim_3_price_specialist_trainers_scenario_online60 <- str_replace_all(dim_3_price_specialist_trainers_scenario_online60, map_df)
  writeFormula(wb, sheet="TotalperItem", x=paste0("=", dim_3_price_specialist_trainers_scenario_online60), startCol=2, startRow=25)
  
  # Dimensão 3 - Course for Trainers (Scenario online-60)
  writeData(wb, sheet = "TotalperItem", x="dim_3_price_course_for_trainers_scenario_online60", startCol=1, startRow=26)
  
  dim_3_price_course_for_trainers_scenario_online60 <- "reference_price_trainers60*1.5*(number_of_graduated_teacher/parameter_teacher_class)/parameter_teacher_class"
  dim_3_price_course_for_trainers_scenario_online60 <- str_replace_all(dim_3_price_course_for_trainers_scenario_online60, map_df)
  writeFormula(wb, sheet="TotalperItem", x=paste0("=", dim_3_price_course_for_trainers_scenario_online60), startCol=2, startRow=26)
  
  # Dimensão 3 - Training design (Scenario online-60)
  writeData(wb, sheet = "TotalperItem", x="dim_3_price_training_design_scenario_online60", startCol=1, startRow=27)
  
  dim_3_price_training_design_scenario_online60 <- "reference_price_training_design"
  dim_3_price_training_design_scenario_online60 <- str_replace_all(dim_3_price_training_design_scenario_online60, map_df)
  writeFormula(wb, sheet="TotalperItem", x=paste0("=", dim_3_price_training_design_scenario_online60), startCol=2, startRow=27)
  
  # Dimensão 3 - Specialist Trainers (Scenario online-40)
  writeData(wb, sheet = "TotalperItem", x="dim_3_price_specialist_trainers_scenario_online40", startCol=1, startRow=28)
  
  dim_3_price_specialist_trainers_scenario_online40 <- "(reference_price_trainers60*(number_of_graduated_teacher/parameter_teacher_class))*reference_multiplier40h"
  dim_3_price_specialist_trainers_scenario_online40 <- str_replace_all(dim_3_price_specialist_trainers_scenario_online40, map_df)
  writeFormula(wb, sheet="TotalperItem", x=paste0("=", dim_3_price_specialist_trainers_scenario_online40), startCol=2, startRow=28)
  
  # Dimensão 3 - Course for Trainers (Scenario online-40)
  writeData(wb, sheet = "TotalperItem", x="dim_3_price_course_for_trainers_scenario_online40", startCol=1, startRow=29)
  
  dim_3_price_course_for_trainers_scenario_online40 <- "(reference_price_trainers60*1.5*(number_of_graduated_teacher/parameter_teacher_class)/parameter_teacher_class)*reference_multiplier40h"
  dim_3_price_course_for_trainers_scenario_online40 <- str_replace_all(dim_3_price_course_for_trainers_scenario_online40, map_df)
  writeFormula(wb, sheet="TotalperItem", x=paste0("=", dim_3_price_course_for_trainers_scenario_online40), startCol=2, startRow=29)
  
  # Dimensão 3 - Training design (Scenario online-40)
  writeData(wb, sheet = "TotalperItem", x="dim_3_price_training_design_scenario_online40", startCol=1, startRow=30)
  
  dim_3_price_training_design_scenario_online40 <- "reference_price_training_design*reference_multiplier40h"
  dim_3_price_training_design_scenario_online40 <- str_replace_all(dim_3_price_training_design_scenario_online40, map_df)
  writeFormula(wb, sheet="TotalperItem", x=paste0("=", dim_3_price_training_design_scenario_online40), startCol=2, startRow=30)
  
  # Dimensão 3 - Specialist Trainers (Scenario in-person)
  writeData(wb, sheet = "TotalperItem", x="dim_3_price_specialist_trainers_scenario_inperson40", startCol=1, startRow=31)
  
  dim_3_price_specialist_trainers_scenario_inperson40 <- "(reference_price_trainers60*(number_of_graduated_teacher/parameter_teacher_class))*reference_multiplier40h*reference_multiplier_inperson"
  dim_3_price_specialist_trainers_scenario_inperson40 <- str_replace_all(dim_3_price_specialist_trainers_scenario_inperson40, map_df)
  writeFormula(wb, sheet="TotalperItem", x=paste0("=", dim_3_price_specialist_trainers_scenario_inperson40), startCol=2, startRow=31)
  
  # Dimensão 3 - Course for Trainers (Scenario in-person)
  writeData(wb, sheet = "TotalperItem", x="dim_3_price_course_for_trainers_scenario_inperson40", startCol=1, startRow=32)
  
  dim_3_price_course_for_trainers_scenario_inperson40 <- "(reference_price_trainers60*1.5*(number_of_graduated_teacher/parameter_teacher_class)/parameter_teacher_class)*reference_multiplier40h*reference_multiplier_inperson"
  dim_3_price_course_for_trainers_scenario_inperson40 <- str_replace_all(dim_3_price_course_for_trainers_scenario_inperson40, map_df)
  writeFormula(wb, sheet="TotalperItem", x=paste0("=", dim_3_price_course_for_trainers_scenario_inperson40), startCol=2, startRow=32)
  
  # Dimensão 3 - Training design (Scenario in-person)
  writeData(wb, sheet = "TotalperItem", x="dim_3_price_training_design_scenario_inperson40", startCol=1, startRow=33)
  
  dim_3_price_training_design_scenario_inperson40 <- "(reference_price_training_design*reference_multiplier40h)*reference_multiplier40h*reference_multiplier_inperson"
  dim_3_price_training_design_scenario_inperson40 <- str_replace_all(dim_3_price_training_design_scenario_inperson40, map_df)
  writeFormula(wb, sheet="TotalperItem", x=paste0("=", dim_3_price_training_design_scenario_inperson40), startCol=2, startRow=33)
  
  # Dimensão 4 - Teaching and learning platform
  writeData(wb, sheet = "TotalperItem", x="dim_4_price_teaching_learning_platform", startCol=1, startRow=34)
  
  dim_4_price_teaching_learning_platform <- "(number_of_students_secondary+number_of_students_primary)*reference_price_teaching_learning*parameter_teaching_learning*(type_opex/12)"
  dim_4_price_teaching_learning_platform <- str_replace_all(dim_4_price_teaching_learning_platform, map_df)
  writeFormula(wb, sheet="TotalperItem", x=paste0("=", dim_4_price_teaching_learning_platform), startCol=2, startRow=34)
  
  # Dimensão 4 - Management platform
  writeData(wb, sheet = "TotalperItem", x="dim_4_price_management_platform", startCol=1, startRow=35)
  
  dim_4_price_management_platform <- "number_of_schools*reference_price_management_platform*parameter_management*(type_opex/12)"
  dim_4_price_management_platform <- str_replace_all(dim_4_price_management_platform, map_df)
  writeFormula(wb, sheet="TotalperItem", x=paste0("=", dim_4_price_management_platform), startCol=2, startRow=35)
  
  # Dimensão 5 - Central team
  writeData(wb, sheet = "TotalperItem", x="dim_5_price_central_team", startCol=1, startRow=36)
  
  dim_5_price_central_team <- "reference_consultant_price*((parameter_central_team_min+parameter_central_team_max)/2)*type_opex"
  dim_5_price_central_team <- str_replace_all(dim_5_price_central_team, map_df)
  writeFormula(wb, sheet="TotalperItem", x=paste0("=", dim_5_price_central_team), startCol=2, startRow=36)
  
  # Dimensão 5 - Regional team
  writeData(wb, sheet = "TotalperItem", x="dim_5_price_regional_team", startCol=1, startRow=37)
  
  dim_5_price_regional_team <- "parameter_regional_team*number_of_schools*reference_teacher_price*type_opex"
  dim_5_price_regional_team <- str_replace_all(dim_5_price_regional_team, map_df)
  writeFormula(wb, sheet="TotalperItem", x=paste0("=", dim_5_price_regional_team), startCol=2, startRow=37)
  
  # Dimensão 5 - Local team
  writeData(wb, sheet = "TotalperItem", x="dim_5_price_local_team", startCol=1, startRow=38)
  
  dim_5_price_local_team <- "(parameter_local_team_urban*(number_of_schools-number_of_rural_schools)+(parameter_local_team_rural*number_of_rural_schools))*reference_teacher_price*type_opex"
  dim_5_price_local_team <- str_replace_all(dim_5_price_local_team, map_df)
  writeFormula(wb, sheet="TotalperItem", x=paste0("=", dim_5_price_local_team), startCol=2, startRow=38)
  
  #---- TotalperItem: Cenário 1 ----
  
  # Dimensão 1 - Fiber expansion (Cenário 1)
  writeData(wb, sheet = "TotalperItem", x="sce1_sec_dim_1_price_fiber_exp", startCol=1, startRow=39)
  
  sce1_sec_dim_1_price_fiber_exp <- "itu_pop_between_25_50*secondary_schools_cima*parameter_fiber_exp*reference_fiber_exp_price"
  sce1_sec_dim_1_price_fiber_exp <- str_replace_all(sce1_sec_dim_1_price_fiber_exp, map_df)
  writeFormula(wb, sheet="TotalperItem", x=paste0("=", sce1_sec_dim_1_price_fiber_exp), startCol=2, startRow=39)
  
  # Dimensão 1 - Satellite service (Cenário 1)
  writeData(wb, sheet = "TotalperItem", x="sce1_sec_dim_1_price_satellite_service", startCol=1, startRow=40)
  
  sce1_sec_dim_1_price_satellite_service <- "itu_pop_above_50*secondary_schools_cima*reference_satellite_link_price*type_opex"
  sce1_sec_dim_1_price_satellite_service <- str_replace_all(sce1_sec_dim_1_price_satellite_service, map_df)
  writeFormula(wb, sheet="TotalperItem", x=paste0("=", sce1_sec_dim_1_price_satellite_service), startCol=2, startRow=40)
  
  # Dimensão 1 - Fiber link (Cenário 1)
  writeData(wb, sheet = "TotalperItem", x="sce1_sec_dim_1_price_fiber_link", startCol=1, startRow=41)
  
  sce1_sec_dim_1_price_fiber_link <- "itu_pop_below_50*secondary_schools_cima*avg_student_per_school*(reference_fiber_link_price/200)*type_opex"
  sce1_sec_dim_1_price_fiber_link <- str_replace_all(sce1_sec_dim_1_price_fiber_link, map_df)
  writeFormula(wb, sheet="TotalperItem", x=paste0("=", sce1_sec_dim_1_price_fiber_link), startCol=2, startRow=41)
  
  # Dimensão 1 - Access point (Cenário 1)
  writeData(wb, sheet = "TotalperItem", x="sce1_sec_dim_1_price_access_point", startCol=1, startRow=42)
  
  sce1_sec_dim_1_price_access_point <- "((number_of_students_secondary/35)+secondary_schools_cima*3)*parameter_access_point*reference_access_point_price"
  sce1_sec_dim_1_price_access_point <- str_replace_all(sce1_sec_dim_1_price_access_point, map_df)
  writeFormula(wb, sheet="TotalperItem", x=paste0("=", sce1_sec_dim_1_price_access_point), startCol=2, startRow=42)
  
  # Dimensão 1 - Firewall (Cenário 1)
  writeData(wb, sheet = "TotalperItem", x="sce1_sec_dim_1_price_firewall", startCol=1, startRow=43)
  
  sce1_sec_dim_1_price_firewall <- "secondary_schools_cima*parameter_firewall*reference_firewall_price"
  sce1_sec_dim_1_price_firewall <- str_replace_all(sce1_sec_dim_1_price_firewall, map_df)
  writeFormula(wb, sheet="TotalperItem", x=paste0("=", sce1_sec_dim_1_price_firewall), startCol=2, startRow=43)
  
  # Dimensão 1 - Nobreak (Cenário 1)
  writeData(wb, sheet = "TotalperItem", x="sce1_sec_dim_1_price_nobreak", startCol=1, startRow=44)
  
  sce1_sec_dim_1_price_nobreak <- "secondary_schools_cima*parameter_nobreak*reference_nobreak_price"
  sce1_sec_dim_1_price_nobreak <- str_replace_all(sce1_sec_dim_1_price_nobreak, map_df)
  writeFormula(wb, sheet="TotalperItem", x=paste0("=", sce1_sec_dim_1_price_nobreak), startCol=2, startRow=44)
  
  # Dimensão 1 - Switch (Cenário 1)
  writeData(wb, sheet = "TotalperItem", x="sce1_sec_dim_1_price_switch", startCol=1, startRow=45)
  
  sce1_sec_dim_1_price_switch <- "secondary_schools_cima*parameter_switch*reference_switch_price"
  sce1_sec_dim_1_price_switch <- str_replace_all(sce1_sec_dim_1_price_switch, map_df)
  writeFormula(wb, sheet="TotalperItem", x=paste0("=", sce1_sec_dim_1_price_switch), startCol=2, startRow=45)
  
  # Dimensão 1 - Rack 6U/8U (Cenário 1)
  writeData(wb, sheet = "TotalperItem", x="sce1_sec_dim_1_price_rack", startCol=1, startRow=46)
  
  sce1_sec_dim_1_price_rack <- "secondary_schools_cima*parameter_rack*reference_rack_price"
  sce1_sec_dim_1_price_rack <- str_replace_all(sce1_sec_dim_1_price_rack, map_df)
  writeFormula(wb, sheet="TotalperItem", x=paste0("=", sce1_sec_dim_1_price_rack), startCol=2, startRow=46)
  
  # Dimensão 2 - Device per students (Min Scenario) (Cenário 1)
  writeData(wb, sheet = "TotalperItem", x="sce1_sec_dim_2_price_device_student_min", startCol=1, startRow=47)
  
  sce1_sec_dim_2_price_device_student_min <- "number_of_students_secondary*parameter_min_scenario_secondary*reference_cloudbook_price"
  sce1_sec_dim_2_price_device_student_min <- str_replace_all(sce1_sec_dim_2_price_device_student_min, map_df)
  writeFormula(wb, sheet="TotalperItem", x=paste0("=", sce1_sec_dim_2_price_device_student_min), startCol=2, startRow=47)
  
  # Dimensão 2 - Device per students (Mild Scenario) (Cenário 1)
  writeData(wb, sheet = "TotalperItem", x="sce1_sec_dim_2_price_device_student_mild", startCol=1, startRow=48)
  
  sce1_sec_dim_2_price_device_student_mild <- "number_of_students_secondary*parameter_mild_scenario_secondary*reference_cloudbook_price"
  sce1_sec_dim_2_price_device_student_mild <- str_replace_all(sce1_sec_dim_2_price_device_student_mild, map_df)
  writeFormula(wb, sheet="TotalperItem", x=paste0("=", sce1_sec_dim_2_price_device_student_mild), startCol=2, startRow=48)
  
  # Dimensão 2 - Device per students (Comprehensive Scenario) (Cenário 1)
  writeData(wb, sheet = "TotalperItem", x="sce1_sec_dim_2_price_device_student_comprehensive", startCol=1, startRow=49)
  
  sce1_sec_dim_2_price_device_student_comprehensive <- "number_of_students_secondary*parameter_comprehensive_secondary*reference_cloudbook_price"
  sce1_sec_dim_2_price_device_student_comprehensive <- str_replace_all(sce1_sec_dim_2_price_device_student_comprehensive, map_df)
  writeFormula(wb, sheet="TotalperItem", x=paste0("=", sce1_sec_dim_2_price_device_student_comprehensive), startCol=2, startRow=49)
  
  # Dimensão 2 - Devices per teacher (Cenário 1)
  writeData(wb, sheet = "TotalperItem", x="sce1_sec_dim_2_price_device_teacher", startCol=1, startRow=50)
  
  sce1_sec_dim_2_price_device_teacher <- "(number_of_teachers*(number_of_students_secondary/(number_of_students)))*parameter_device_per_teacher*reference_laptop_price"
  sce1_sec_dim_2_price_device_teacher <- str_replace_all(sce1_sec_dim_2_price_device_teacher, map_df)
  writeFormula(wb, sheet="TotalperItem", x=paste0("=", sce1_sec_dim_2_price_device_teacher), startCol=2, startRow=50)
  
  # Dimensão 2 - Devices per school (Cenário 1)
  writeData(wb, sheet = "TotalperItem", x="sce1_sec_dim_2_price_device_school", startCol=1, startRow=51)
  
  sce1_sec_dim_2_price_device_school <- "secondary_schools_cima*parameter_device_per_school*reference_desktop_price"
  sce1_sec_dim_2_price_device_school <- str_replace_all(sce1_sec_dim_2_price_device_school, map_df)
  writeFormula(wb, sheet="TotalperItem", x=paste0("=", sce1_sec_dim_2_price_device_school), startCol=2, startRow=51)
  
  # Dimensão 2 - Charging cart (Min Scenario) (Cenário 1)
  writeData(wb, sheet = "TotalperItem", x="sce1_sec_dim_2_price_charging_cart_min", startCol=1, startRow=52)
  
  sce1_sec_dim_2_price_charging_cart_min <- "(number_of_students_secondary*parameter_min_scenario_secondary)/30*reference_charging_cart_price"
  sce1_sec_dim_2_price_charging_cart_min <- str_replace_all(sce1_sec_dim_2_price_charging_cart_min, map_df)
  writeFormula(wb, sheet="TotalperItem", x=paste0("=", sce1_sec_dim_2_price_charging_cart_min), startCol=2, startRow=52)
  
  # Dimensão 2 - Charging cart (Mild Scenario) (Cenário 1)
  writeData(wb, sheet = "TotalperItem", x="sce1_sec_dim_2_price_charging_cart_mild", startCol=1, startRow=53)
  
  sce1_sec_dim_2_price_charging_cart_mild <- "(number_of_students_secondary*parameter_mild_scenario_secondary)/30*reference_charging_cart_price"
  sce1_sec_dim_2_price_charging_cart_mild <- str_replace_all(sce1_sec_dim_2_price_charging_cart_mild, map_df)
  writeFormula(wb, sheet="TotalperItem", x=paste0("=", sce1_sec_dim_2_price_charging_cart_mild), startCol=2, startRow=53)

  # Dimensão 2 - Charging cart (Comprehensive Scenario) (Cenário 1)
  writeData(wb, sheet = "TotalperItem", x="sce1_sec_dim_2_price_charging_cart_comprehensive", startCol=1, startRow=54)
  
  sce1_sec_dim_2_price_charging_cart_comprehensive <- "(number_of_students_secondary*parameter_comprehensive_secondary)/30*reference_charging_cart_price"
  sce1_sec_dim_2_price_charging_cart_comprehensive <- str_replace_all(sce1_sec_dim_2_price_charging_cart_comprehensive, map_df)
  writeFormula(wb, sheet="TotalperItem", x=paste0("=", sce1_sec_dim_2_price_charging_cart_comprehensive), startCol=2, startRow=54)

  # Dimensão 2 - Multimedia projector (Cenário 1)
  writeData(wb, sheet = "TotalperItem", x="sce1_sec_dim_2_price_multimedia_projector", startCol=1, startRow=55)
  
  sce1_sec_dim_2_price_multimedia_projector <- "secondary_schools_cima*parameter_multimedia_projector*reference_multimedia_projector_price"
  sce1_sec_dim_2_price_multimedia_projector <- str_replace_all(sce1_sec_dim_2_price_multimedia_projector, map_df)
  writeFormula(wb, sheet="TotalperItem", x=paste0("=", sce1_sec_dim_2_price_multimedia_projector), startCol=2, startRow=55)

  # Dimensão 2 - Headphones (Min Scenario) (Cenário 1)
  writeData(wb, sheet = "TotalperItem", x="sce1_sec_dim_2_price_headphones_min_scenario", startCol=1, startRow=56)
  
  sce1_sec_dim_2_price_headphones_min_scenario <- "(number_of_students_secondary*parameter_min_scenario_secondary)*reference_headphone_price"
  sce1_sec_dim_2_price_headphones_min_scenario <- str_replace_all(sce1_sec_dim_2_price_headphones_min_scenario, map_df)
  writeFormula(wb, sheet="TotalperItem", x=paste0("=", sce1_sec_dim_2_price_headphones_min_scenario), startCol=2, startRow=56)

  # Dimensão 2 - Headphones (Mild Scenario) (Cenário 1)
  writeData(wb, sheet = "TotalperItem", x="sce1_sec_dim_2_price_headphones_mild_scenario", startCol=1, startRow=57)
  
  sce1_sec_dim_2_price_headphones_mild_scenario <- "(number_of_students_secondary*parameter_mild_scenario_secondary)*reference_headphone_price"
  sce1_sec_dim_2_price_headphones_mild_scenario <- str_replace_all(sce1_sec_dim_2_price_headphones_mild_scenario, map_df)
  writeFormula(wb, sheet="TotalperItem", x=paste0("=", sce1_sec_dim_2_price_headphones_mild_scenario), startCol=2, startRow=57)
  
  # Dimensão 2 - Headphones (Comprehensive Scenario) (Cenário 1)
  writeData(wb, sheet = "TotalperItem", x="sce1_sec_dim_2_price_headphones_comprehensive", startCol=1, startRow=58)
  
  sce1_sec_dim_2_price_headphones_comprehensive <- "(number_of_students_secondary*parameter_comprehensive_secondary)*reference_headphone_price"
  sce1_sec_dim_2_price_headphones_comprehensive <- str_replace_all(sce1_sec_dim_2_price_headphones_comprehensive, map_df)
  writeFormula(wb, sheet="TotalperItem", x=paste0("=", sce1_sec_dim_2_price_headphones_comprehensive), startCol=2, startRow=58)
  
  # Dimensão 3 - Diagnostic Tool (Cenário 1)
  writeData(wb, sheet = "TotalperItem", x="sce1_sec_dim_3_price_diagnostic_tool", startCol=1, startRow=59)
  
  sce1_sec_dim_3_price_diagnostic_tool <- "reference_price_diagnostic_tool"
  sce1_sec_dim_3_price_diagnostic_tool <- str_replace_all(sce1_sec_dim_3_price_diagnostic_tool, map_df)
  writeFormula(wb, sheet="TotalperItem", x=paste0("=", sce1_sec_dim_3_price_diagnostic_tool), startCol=2, startRow=59)

  # Dimensão 3 - LMS Platform (Cenário 1)
  writeData(wb, sheet = "TotalperItem", x="sce1_sec_dim_3_price_lms_platform", startCol=1, startRow=60)
  
  sce1_sec_dim_3_price_lms_platform <- "reference_price_lms"
  sce1_sec_dim_3_price_lms_platform <- str_replace_all(sce1_sec_dim_3_price_lms_platform, map_df)
  writeFormula(wb, sheet="TotalperItem", x=paste0("=", sce1_sec_dim_3_price_lms_platform), startCol=2, startRow=60)
  
  # Dimensão 3 - Specialist Trainers (Scenario online-60) (Cenário 1)
  writeData(wb, sheet = "TotalperItem", x="sce1_sec_dim_3_price_specialist_trainers_scenario_online60", startCol=1, startRow=61)
  
  sce1_sec_dim_3_price_specialist_trainers_scenario_online60 <- "reference_price_trainers60*((number_of_graduated_teacher*(number_of_students_secondary/(number_of_students)))/parameter_teacher_class)"
  sce1_sec_dim_3_price_specialist_trainers_scenario_online60 <- str_replace_all(sce1_sec_dim_3_price_specialist_trainers_scenario_online60, map_df)
  writeFormula(wb, sheet="TotalperItem", x=paste0("=", sce1_sec_dim_3_price_specialist_trainers_scenario_online60), startCol=2, startRow=61)
  
  # Dimensão 3 - Course for Trainers (Scenario online-60) (Cenário 1)
  writeData(wb, sheet = "TotalperItem", x="sce1_sec_dim_3_price_course_for_trainers_scenario_online60", startCol=1, startRow=62)
  
  sce1_sec_dim_3_price_course_for_trainers_scenario_online60 <- "reference_teacher_price*1.5*((number_of_graduated_teacher*(number_of_students_secondary/(number_of_students)))/parameter_teacher_class)/parameter_teacher_class"
  sce1_sec_dim_3_price_course_for_trainers_scenario_online60 <- str_replace_all(sce1_sec_dim_3_price_course_for_trainers_scenario_online60, map_df)
  writeFormula(wb, sheet="TotalperItem", x=paste0("=", sce1_sec_dim_3_price_course_for_trainers_scenario_online60), startCol=2, startRow=62)
  
  # Dimensão 3 - Training design (Scenario online-60) (Cenário 1)
  writeData(wb, sheet = "TotalperItem", x="sce1_sec_dim_3_price_training_design_scenario_online60", startCol=1, startRow=63)
  
  sce1_sec_dim_3_price_training_design_scenario_online60 <- "reference_price_training_design"
  sce1_sec_dim_3_price_training_design_scenario_online60 <- str_replace_all(sce1_sec_dim_3_price_training_design_scenario_online60, map_df)
  writeFormula(wb, sheet="TotalperItem", x=paste0("=", sce1_sec_dim_3_price_training_design_scenario_online60), startCol=2, startRow=63)
  
  # Dimensão 3 - Specialist Trainers (Scenario online-40) (Cenário 1)
  writeData(wb, sheet = "TotalperItem", x="sce1_sec_dim_3_price_specialist_trainers_scenario_online40", startCol=1, startRow=64)
  
  sce1_sec_dim_3_price_specialist_trainers_scenario_online40 <- "(reference_price_trainers60*((number_of_graduated_teacher*(number_of_students_secondary/(number_of_students)))/parameter_teacher_class))*reference_multiplier40h"
  sce1_sec_dim_3_price_specialist_trainers_scenario_online40 <- str_replace_all(sce1_sec_dim_3_price_specialist_trainers_scenario_online40, map_df)
  writeFormula(wb, sheet="TotalperItem", x=paste0("=", sce1_sec_dim_3_price_specialist_trainers_scenario_online40), startCol=2, startRow=64)
  
  # Dimensão 3 - Course for Trainers (Scenario online-40) (Cenário 1)
  writeData(wb, sheet = "TotalperItem", x="sce1_sec_dim_3_price_course_for_trainers_scenario_online40", startCol=1, startRow=65)
  
  sce1_sec_dim_3_price_course_for_trainers_scenario_online40 <- "(reference_teacher_price*1.5*((number_of_graduated_teacher*(number_of_students_secondary/(number_of_students)))/parameter_teacher_class)/parameter_teacher_class)*reference_multiplier40h"
  sce1_sec_dim_3_price_course_for_trainers_scenario_online40 <- str_replace_all(sce1_sec_dim_3_price_course_for_trainers_scenario_online40, map_df)
  writeFormula(wb, sheet="TotalperItem", x=paste0("=", sce1_sec_dim_3_price_course_for_trainers_scenario_online40), startCol=2, startRow=65)
  
  # Dimensão 3 - Training design (Scenario online-40) (Cenário 1)
  writeData(wb, sheet = "TotalperItem", x="sce1_sec_dim_3_price_training_design_scenario_online40", startCol=1, startRow=66)
  
  sce1_sec_dim_3_price_training_design_scenario_online40 <- "reference_price_training_design*reference_multiplier40h"
  sce1_sec_dim_3_price_training_design_scenario_online40 <- str_replace_all(sce1_sec_dim_3_price_training_design_scenario_online40, map_df)
  writeFormula(wb, sheet="TotalperItem", x=paste0("=", sce1_sec_dim_3_price_training_design_scenario_online40), startCol=2, startRow=66)
  
  # Dimensão 3 - Specialist Trainers (Scenario in-person) (Cenário 1)
  writeData(wb, sheet = "TotalperItem", x="sce1_sec_dim_3_price_specialist_trainers_scenario_inperson40", startCol=1, startRow=67)
  
  sce1_sec_dim_3_price_specialist_trainers_scenario_inperson40 <- "(reference_price_trainers60*((number_of_graduated_teacher*(number_of_students_secondary/(number_of_students)))/parameter_teacher_class))*reference_multiplier40h*reference_multiplier_inperson"
  sce1_sec_dim_3_price_specialist_trainers_scenario_inperson40 <- str_replace_all(sce1_sec_dim_3_price_specialist_trainers_scenario_inperson40, map_df)
  writeFormula(wb, sheet="TotalperItem", x=paste0("=", sce1_sec_dim_3_price_specialist_trainers_scenario_inperson40), startCol=2, startRow=67)

  # Dimensão 3 - Course for Trainers (Scenario in-person) (Cenário 1)
  writeData(wb, sheet = "TotalperItem", x="sce1_sec_dim_3_price_course_for_trainers_scenario_inperson40", startCol=1, startRow=68)
  
  sce1_sec_dim_3_price_course_for_trainers_scenario_inperson40 <- "(reference_teacher_price*1.5*((number_of_graduated_teacher*(number_of_students_secondary/(number_of_students)))/parameter_teacher_class)/parameter_teacher_class)*reference_multiplier40h*reference_multiplier_inperson"
  sce1_sec_dim_3_price_course_for_trainers_scenario_inperson40 <- str_replace_all(sce1_sec_dim_3_price_course_for_trainers_scenario_inperson40, map_df)
  writeFormula(wb, sheet="TotalperItem", x=paste0("=", sce1_sec_dim_3_price_course_for_trainers_scenario_inperson40), startCol=2, startRow=68)
  
  # Dimensão 3 - Training design (Scenario in-person) (Cenário 1)
  writeData(wb, sheet = "TotalperItem", x="sce1_sec_dim_3_price_training_design_scenario_inperson40", startCol=1, startRow=69)
  
  sce1_sec_dim_3_price_training_design_scenario_inperson40 <- "reference_price_training_design*reference_multiplier40h*reference_multiplier_inperson"
  sce1_sec_dim_3_price_training_design_scenario_inperson40 <- str_replace_all(sce1_sec_dim_3_price_training_design_scenario_inperson40, map_df)
  writeFormula(wb, sheet="TotalperItem", x=paste0("=", sce1_sec_dim_3_price_training_design_scenario_inperson40), startCol=2, startRow=69)
  
  # Dimensão 4 - Teaching and learning platform (Cenário 1)
  writeData(wb, sheet = "TotalperItem", x="sce1_sec_dim_4_price_teaching_learning_platform", startCol=1, startRow=70)
  
  sce1_sec_dim_4_price_teaching_learning_platform <- "number_of_students_secondary*reference_price_teaching_learning*parameter_teaching_learning*(type_opex/12)"
  sce1_sec_dim_4_price_teaching_learning_platform <- str_replace_all(sce1_sec_dim_4_price_teaching_learning_platform, map_df)
  writeFormula(wb, sheet="TotalperItem", x=paste0("=", sce1_sec_dim_4_price_teaching_learning_platform), startCol=2, startRow=70)
  
  # Dimensão 4 - Management platform (Cenário 1)
  writeData(wb, sheet = "TotalperItem", x="sce1_sec_dim_4_price_management_platform", startCol=1, startRow=71)
  
  sce1_sec_dim_4_price_management_platform <- "secondary_schools_cima*reference_price_management_platform*parameter_management*(type_opex/12)"
  sce1_sec_dim_4_price_management_platform <- str_replace_all(sce1_sec_dim_4_price_management_platform, map_df)
  writeFormula(wb, sheet="TotalperItem", x=paste0("=", sce1_sec_dim_4_price_management_platform), startCol=2, startRow=71)
  
  # Dimensão 5 - Central team (Cenário 1)
  writeData(wb, sheet = "TotalperItem", x="sce1_sec_dim_5_price_central_team", startCol=1, startRow=72)
  
  sce1_sec_dim_5_price_central_team <- "reference_consultant_price*((parameter_central_team_min+parameter_central_team_max)/2)*type_opex"
  sce1_sec_dim_5_price_central_team <- str_replace_all(sce1_sec_dim_5_price_central_team, map_df)
  writeFormula(wb, sheet="TotalperItem", x=paste0("=", sce1_sec_dim_5_price_central_team), startCol=2, startRow=72)
  
  # Dimensão 5 - Regional team (Cenário 1)
  writeData(wb, sheet = "TotalperItem", x="sce1_sec_dim_5_price_regional_team", startCol=1, startRow=73)
  
  sce1_sec_dim_5_price_regional_team <- "parameter_regional_team*secondary_schools_cima*reference_teacher_price*type_opex"
  sce1_sec_dim_5_price_regional_team <- str_replace_all(sce1_sec_dim_5_price_regional_team, map_df)
  writeFormula(wb, sheet="TotalperItem", x=paste0("=", sce1_sec_dim_5_price_regional_team), startCol=2, startRow=73)
  
  # Dimensão 5 - Local team (Cenário 1)
  writeData(wb, sheet = "TotalperItem", x="sce1_sec_dim_5_price_local_team", startCol=1, startRow=74)
  
  sce1_sec_dim_5_price_local_team <- "(parameter_local_team_urban*(secondary_schools_cima/number_of_schools)*(number_of_schools-number_of_rural_schools)+(secondary_schools_cima/number_of_schools)*(parameter_local_team_rural*number_of_rural_schools))*reference_teacher_price*type_opex"
  sce1_sec_dim_5_price_local_team <- str_replace_all(sce1_sec_dim_5_price_local_team, map_df)
  writeFormula(wb, sheet="TotalperItem", x=paste0("=", sce1_sec_dim_5_price_local_team), startCol=2, startRow=74)
  
  #---- TotalperItem: Cenário 2 ----
  
  # Dimensão 1 - Fiber expansion (Cenário 2)
  writeData(wb, sheet = "TotalperItem", x="sce2_rur_dim_1_price_fiber_exp", startCol=1, startRow=75)
  
  sce2_rur_dim_1_price_fiber_exp <- "MIN(number_of_rural_schools-MIN(number_of_rural_schools,(itu_pop_above_50*number_of_schools)),itu_pop_between_25_50*number_of_schools)*parameter_fiber_exp*reference_fiber_exp_price"
  sce2_rur_dim_1_price_fiber_exp <- str_replace_all(sce2_rur_dim_1_price_fiber_exp, map_df)
  writeFormula(wb, sheet="TotalperItem", x=paste0("=", sce2_rur_dim_1_price_fiber_exp), startCol=2, startRow=75)
  
  # Dimensão 1 - Satellite service (Cenário 2)
  writeData(wb, sheet = "TotalperItem", x="sce2_rur_dim_1_price_satellite_service", startCol=1, startRow=76)
  
  sce2_rur_dim_1_price_satellite_service <- "MIN(number_of_rural_schools,itu_pop_above_50*number_of_schools)*reference_satellite_link_price*type_opex"
  sce2_rur_dim_1_price_satellite_service <- str_replace_all(sce2_rur_dim_1_price_satellite_service, map_df)
  writeFormula(wb, sheet="TotalperItem", x=paste0("=", sce2_rur_dim_1_price_satellite_service), startCol=2, startRow=76)
  
  # Dimensão 1 - Fiber link (Cenário 2)
  writeData(wb, sheet = "TotalperItem", x="sce2_rur_dim_1_price_fiber_link", startCol=1, startRow=77)
  
  sce2_rur_dim_1_price_fiber_link <- "(number_of_rural_schools-MIN(number_of_rural_schools,itu_pop_above_50))*(avg_student_per_school)*(reference_fiber_link_price/200)*type_opex"
  sce2_rur_dim_1_price_fiber_link <- str_replace_all(sce2_rur_dim_1_price_fiber_link, map_df)
  writeFormula(wb, sheet="TotalperItem", x=paste0("=", sce2_rur_dim_1_price_fiber_link), startCol=2, startRow=77)
  
  # Dimensão 1 - Access point (Cenário 2)
  writeData(wb, sheet = "TotalperItem", x="sce2_rur_dim_1_price_access_point", startCol=1, startRow=78)
  
  sce2_rur_dim_1_price_access_point <- "((number_of_students_primary/25)+primary_schools_cima*3)+((number_of_students_secondary/35)+secondary_schools_cima*3)*(number_of_rural_schools/number_of_schools)*parameter_access_point*reference_access_point_price"
  sce2_rur_dim_1_price_access_point <- str_replace_all(sce2_rur_dim_1_price_access_point, map_df)
  writeFormula(wb, sheet="TotalperItem", x=paste0("=", sce2_rur_dim_1_price_access_point), startCol=2, startRow=78)
  
  # Dimensão 1 - Firewall (Cenário 2)
  writeData(wb, sheet = "TotalperItem", x="sce2_rur_dim_1_price_firewall", startCol=1, startRow=79)
  
  sce2_rur_dim_1_price_firewall <- "number_of_rural_schools*parameter_firewall*reference_firewall_price"
  sce2_rur_dim_1_price_firewall <- str_replace_all(sce2_rur_dim_1_price_firewall, map_df)
  writeFormula(wb, sheet="TotalperItem", x=paste0("=", sce2_rur_dim_1_price_firewall), startCol=2, startRow=79)
  
  # Dimensão 1 - Nobreak (Cenário 2)
  writeData(wb, sheet = "TotalperItem", x="sce2_rur_dim_1_price_nobreak", startCol=1, startRow=80)
  
  sce2_rur_dim_1_price_nobreak <- "number_of_rural_schools*parameter_nobreak*reference_nobreak_price"
  sce2_rur_dim_1_price_nobreak <- str_replace_all(sce2_rur_dim_1_price_nobreak, map_df)
  writeFormula(wb, sheet="TotalperItem", x=paste0("=", sce2_rur_dim_1_price_nobreak), startCol=2, startRow=80)
  
  # Dimensão 1 - Switch (Cenário 2)
  writeData(wb, sheet = "TotalperItem", x="sce2_rur_dim_1_price_switch", startCol=1, startRow=81)
  
  sce2_rur_dim_1_price_switch <- "number_of_rural_schools*parameter_switch*reference_switch_price"
  sce2_rur_dim_1_price_switch <- str_replace_all(sce2_rur_dim_1_price_switch, map_df)
  writeFormula(wb, sheet="TotalperItem", x=paste0("=", sce2_rur_dim_1_price_switch), startCol=2, startRow=81)
  
  # Dimensão 1 - Rack 6U/8U (Cenário 2)
  writeData(wb, sheet = "TotalperItem", x="sce2_rur_dim_1_price_rack", startCol=1, startRow=82)
  
  sce2_rur_dim_1_price_rack <- "number_of_rural_schools*parameter_rack*reference_rack_price"
  sce2_rur_dim_1_price_rack <- str_replace_all(sce2_rur_dim_1_price_rack, map_df)
  writeFormula(wb, sheet="TotalperItem", x=paste0("=", sce2_rur_dim_1_price_rack), startCol=2, startRow=82)
  
  # Dimensão 2 - Device per students (Min Scenario) (Cenário 2)
  writeData(wb, sheet = "TotalperItem", x="sce2_rur_dim_2_price_device_student_min", startCol=1, startRow=83)
  
  sce2_rur_dim_2_price_device_student_min <- "((number_of_students_primary*(number_of_rural_schools/number_of_schools))*parameter_min_scenario_primary*reference_tablet_price)+((number_of_students_secondary*(number_of_rural_schools/number_of_schools))*parameter_min_scenario_secondary*reference_cloudbook_price)"
  sce2_rur_dim_2_price_device_student_min <- str_replace_all(sce2_rur_dim_2_price_device_student_min, map_df)
  writeFormula(wb, sheet="TotalperItem", x=paste0("=", sce2_rur_dim_2_price_device_student_min), startCol=2, startRow=83)
  
  # Dimensão 2 - Device per students (Mild Scenario) (Cenário 2)
  writeData(wb, sheet = "TotalperItem", x="sce2_rur_dim_2_price_device_student_mild", startCol=1, startRow=84)
  
  sce2_rur_dim_2_price_device_student_mild <- "((number_of_students_primary*(number_of_rural_schools/number_of_schools))*parameter_mild_scenario_primary*reference_tablet_price)+((number_of_students_secondary*(number_of_rural_schools/number_of_schools))*parameter_mild_scenario_secondary*reference_cloudbook_price)"
  sce2_rur_dim_2_price_device_student_mild <- str_replace_all(sce2_rur_dim_2_price_device_student_mild, map_df)
  writeFormula(wb, sheet="TotalperItem", x=paste0("=", sce2_rur_dim_2_price_device_student_mild), startCol=2, startRow=84)
  
  # Dimensão 2 - Device per students (Comprehensive Scenario) (Cenário 2)
  writeData(wb, sheet = "TotalperItem", x="sce2_rur_dim_2_price_device_student_comprehensive", startCol=1, startRow=85)
  
  sce2_rur_dim_2_price_device_student_comprehensive <- "((number_of_students_primary*(number_of_rural_schools/number_of_schools))*parameter_comprehensive_primary*reference_tablet_price)+((number_of_students_secondary*(number_of_rural_schools/number_of_schools))*parameter_comprehensive_secondary*reference_cloudbook_price)"
  sce2_rur_dim_2_price_device_student_comprehensive <- str_replace_all(sce2_rur_dim_2_price_device_student_comprehensive, map_df)
  writeFormula(wb, sheet="TotalperItem", x=paste0("=", sce2_rur_dim_2_price_device_student_comprehensive), startCol=2, startRow=85)
  
  # Dimensão 2 - Devices per teacher(Cenário 2)
  writeData(wb, sheet = "TotalperItem", x="sce2_rur_dim_2_price_device_teacher", startCol=1, startRow=86)
  
  sce2_rur_dim_2_price_device_teacher <- "(number_of_teachers*(number_of_rural_schools/number_of_schools))*parameter_device_per_teacher*reference_laptop_price"
  sce2_rur_dim_2_price_device_teacher <- str_replace_all(sce2_rur_dim_2_price_device_teacher, map_df)
  writeFormula(wb, sheet="TotalperItem", x=paste0("=", sce2_rur_dim_2_price_device_teacher), startCol=2, startRow=86)
  
  # Dimensão 2 - Devices per school (Cenário 2) 
  writeData(wb, sheet = "TotalperItem", x="sce2_rur_dim_2_price_device_school", startCol=1, startRow=87)
  
  sce2_rur_dim_2_price_device_school <- "number_of_rural_schools*parameter_device_per_school*reference_desktop_price"
  sce2_rur_dim_2_price_device_school <- str_replace_all(sce2_rur_dim_2_price_device_school, map_df)
  writeFormula(wb, sheet="TotalperItem", x=paste0("=", sce2_rur_dim_2_price_device_school), startCol=2, startRow=87)
  
  # Dimensão 2 - Charging cart (Min Scenario) (Cenário 2) 
  writeData(wb, sheet = "TotalperItem", x="sce2_rur_dim_2_price_charging_cart_min", startCol=1, startRow=88)
  
  sce2_rur_dim_2_price_charging_cart_min <- "(((number_of_students_primary*(number_of_rural_schools/number_of_schools))*parameter_min_scenario_primary)+((number_of_students_secondary*(number_of_rural_schools/number_of_schools))*parameter_min_scenario_secondary))/30*reference_charging_cart_price"
  sce2_rur_dim_2_price_charging_cart_min <- str_replace_all(sce2_rur_dim_2_price_charging_cart_min, map_df)
  writeFormula(wb, sheet="TotalperItem", x=paste0("=", sce2_rur_dim_2_price_charging_cart_min), startCol=2, startRow=88)
  
  # Dimensão 2 - Charging cart (Mild Scenario) (Cenário 2) 
  writeData(wb, sheet = "TotalperItem", x="sce2_rur_dim_2_price_charging_cart_mild", startCol=1, startRow=89)
  
  sce2_rur_dim_2_price_charging_cart_mild <- "(((number_of_students_primary*(number_of_rural_schools/number_of_schools))*parameter_mild_scenario_primary)+((number_of_students_secondary*(number_of_rural_schools/number_of_schools))*parameter_mild_scenario_secondary))/30*reference_charging_cart_price"
  sce2_rur_dim_2_price_charging_cart_mild <- str_replace_all(sce2_rur_dim_2_price_charging_cart_mild, map_df)
  writeFormula(wb, sheet="TotalperItem", x=paste0("=", sce2_rur_dim_2_price_charging_cart_mild), startCol=2, startRow=89)
  
  # Dimensão 2 - Charging cart (Comprehensive Scenario) (Cenário 2) 
  writeData(wb, sheet = "TotalperItem", x="sce2_rur_dim_2_price_charging_cart_comprehensive", startCol=1, startRow=90)
  
  sce2_rur_dim_2_price_charging_cart_comprehensive <- "(((number_of_students_primary*(number_of_rural_schools/number_of_schools))*parameter_comprehensive_primary)+((number_of_students_secondary*(number_of_rural_schools/number_of_schools))*parameter_comprehensive_secondary))/30*reference_charging_cart_price"
  sce2_rur_dim_2_price_charging_cart_comprehensive <- str_replace_all(sce2_rur_dim_2_price_charging_cart_comprehensive, map_df)
  writeFormula(wb, sheet="TotalperItem", x=paste0("=", sce2_rur_dim_2_price_charging_cart_comprehensive), startCol=2, startRow=90)
  
  # Dimensão 2 - Multimedia projector (Cenário 2) 
  writeData(wb, sheet = "TotalperItem", x="sce2_rur_dim_2_price_multimedia_projector", startCol=1, startRow=91)
  
  sce2_rur_dim_2_price_multimedia_projector <- "number_of_rural_schools*parameter_multimedia_projector*reference_multimedia_projector_price"
  sce2_rur_dim_2_price_multimedia_projector <- str_replace_all(sce2_rur_dim_2_price_multimedia_projector, map_df)
  writeFormula(wb, sheet="TotalperItem", x=paste0("=", sce2_rur_dim_2_price_multimedia_projector), startCol=2, startRow=91)
  
  # Dimensão 2 - Headphones (Min Scenario) (Cenário 2) 
  writeData(wb, sheet = "TotalperItem", x="sce2_rur_dim_2_price_headphones_min_scenario", startCol=1, startRow=92)
  
  sce2_rur_dim_2_price_headphones_min_scenario <- "(((number_of_students_primary*(number_of_rural_schools/number_of_schools))*parameter_min_scenario_primary)+((number_of_students_secondary*(number_of_rural_schools/number_of_schools))*parameter_min_scenario_secondary))*reference_headphone_price"
  sce2_rur_dim_2_price_headphones_min_scenario <- str_replace_all(sce2_rur_dim_2_price_headphones_min_scenario, map_df)
  writeFormula(wb, sheet="TotalperItem", x=paste0("=", sce2_rur_dim_2_price_headphones_min_scenario), startCol=2, startRow=92)
  
  # Dimensão 2 - Headphones (Mild Scenario) (Cenário 2) 
  writeData(wb, sheet = "TotalperItem", x="sce2_rur_dim_2_price_headphones_mild_scenario", startCol=1, startRow=93)
  
  sce2_rur_dim_2_price_headphones_mild_scenario <- "(((number_of_students_primary*(number_of_rural_schools/number_of_schools))*parameter_mild_scenario_primary)+((number_of_students_secondary*(number_of_rural_schools/number_of_schools))*parameter_mild_scenario_secondary))*reference_headphone_price"
  sce2_rur_dim_2_price_headphones_mild_scenario <- str_replace_all(sce2_rur_dim_2_price_headphones_mild_scenario, map_df)
  writeFormula(wb, sheet="TotalperItem", x=paste0("=", sce2_rur_dim_2_price_headphones_mild_scenario), startCol=2, startRow=93)
  
  # Dimensão 2 - Headphones (Comprehensive Scenario) (Cenário 2) 
  writeData(wb, sheet = "TotalperItem", x="sce2_rur_dim_2_price_headphones_comprehensive", startCol=1, startRow=94)
  
  sce2_rur_dim_2_price_headphones_comprehensive <- "(((number_of_students_primary*(number_of_rural_schools/number_of_schools))*parameter_comprehensive_primary)+((number_of_students_secondary*(number_of_rural_schools/number_of_schools))*parameter_comprehensive_secondary))*reference_headphone_price"
  sce2_rur_dim_2_price_headphones_comprehensive <- str_replace_all(sce2_rur_dim_2_price_headphones_comprehensive, map_df)
  writeFormula(wb, sheet="TotalperItem", x=paste0("=", sce2_rur_dim_2_price_headphones_comprehensive), startCol=2, startRow=94)
  
  # Dimensão 3 - Diagnostic Tool (Cenário 2) 
  writeData(wb, sheet = "TotalperItem", x="sce2_rur_dim_3_price_diagnostic_tool", startCol=1, startRow=95)
  
  sce2_rur_dim_3_price_diagnostic_tool <- "reference_price_diagnostic_tool"
  sce2_rur_dim_3_price_diagnostic_tool <- str_replace_all(sce2_rur_dim_3_price_diagnostic_tool, map_df)
  writeFormula(wb, sheet="TotalperItem", x=paste0("=", sce2_rur_dim_3_price_diagnostic_tool), startCol=2, startRow=95)
  
  # Dimensão 3 - LMS Platform (Cenário 2) 
  writeData(wb, sheet = "TotalperItem", x="sce2_rur_dim_3_price_lms_platform", startCol=1, startRow=96)
  
  sce2_rur_dim_3_price_lms_platform <- "reference_price_lms"
  sce2_rur_dim_3_price_lms_platform <- str_replace_all(sce2_rur_dim_3_price_lms_platform, map_df)
  writeFormula(wb, sheet="TotalperItem", x=paste0("=", sce2_rur_dim_3_price_lms_platform), startCol=2, startRow=96)
  
  # Dimensão 3 - Specialist Trainers (Scenario online-60) (Cenário 2) 
  writeData(wb, sheet = "TotalperItem", x="sce2_rur_dim_3_price_specialist_trainers_scenario_online60", startCol=1, startRow=97)
  
  sce2_rur_dim_3_price_specialist_trainers_scenario_online60 <- "reference_price_trainers60*((number_of_graduated_teacher*(number_of_rural_schools/number_of_schools))/parameter_teacher_class)"
  sce2_rur_dim_3_price_specialist_trainers_scenario_online60 <- str_replace_all(sce2_rur_dim_3_price_specialist_trainers_scenario_online60, map_df)
  writeFormula(wb, sheet="TotalperItem", x=paste0("=", sce2_rur_dim_3_price_specialist_trainers_scenario_online60), startCol=2, startRow=97)
  
  # Dimensão 3 - Course for Trainers (Scenario online-60) (Cenário 2) 
  writeData(wb, sheet = "TotalperItem", x="sce2_rur_dim_3_price_course_for_trainers_scenario_online60", startCol=1, startRow=98)
  
  sce2_rur_dim_3_price_course_for_trainers_scenario_online60 <- "reference_price_trainers60*1.5*((number_of_graduated_teacher*(number_of_rural_schools/number_of_schools))/parameter_teacher_class)/parameter_teacher_class"
  sce2_rur_dim_3_price_course_for_trainers_scenario_online60 <- str_replace_all(sce2_rur_dim_3_price_course_for_trainers_scenario_online60, map_df)
  writeFormula(wb, sheet="TotalperItem", x=paste0("=", sce2_rur_dim_3_price_course_for_trainers_scenario_online60), startCol=2, startRow=98)
  
  # Dimensão 3 - Training design (Scenario online-60) (Cenário 2) 
  writeData(wb, sheet = "TotalperItem", x="sce2_rur_dim_3_price_training_design_scenario_online60", startCol=1, startRow=99)
  
  sce2_rur_dim_3_price_training_design_scenario_online60 <- "reference_price_training_design"
  sce2_rur_dim_3_price_training_design_scenario_online60 <- str_replace_all(sce2_rur_dim_3_price_training_design_scenario_online60, map_df)
  writeFormula(wb, sheet="TotalperItem", x=paste0("=", sce2_rur_dim_3_price_training_design_scenario_online60), startCol=2, startRow=99)
  
  # Dimensão 3 - Specialist Trainers (Scenario online-40) (Cenário 2) 
  writeData(wb, sheet = "TotalperItem", x="sce2_rur_dim_3_price_specialist_trainers_scenario_online40", startCol=1, startRow=100)
  
  sce2_rur_dim_3_price_specialist_trainers_scenario_online40 <- "(reference_price_trainers60*((number_of_graduated_teacher*(number_of_rural_schools/number_of_schools))/parameter_teacher_class))*reference_multiplier40h"
  sce2_rur_dim_3_price_specialist_trainers_scenario_online40 <- str_replace_all(sce2_rur_dim_3_price_specialist_trainers_scenario_online40, map_df)
  writeFormula(wb, sheet="TotalperItem", x=paste0("=", sce2_rur_dim_3_price_specialist_trainers_scenario_online40), startCol=2, startRow=100)
  
  # Dimensão 3 - Course for Trainers (Scenario online-40) (Cenário 2) 
  writeData(wb, sheet = "TotalperItem", x="sce2_rur_dim_3_price_course_for_trainers_scenario_online40", startCol=1, startRow=101)
  
  sce2_rur_dim_3_price_course_for_trainers_scenario_online40 <- "(reference_price_trainers60*1.5*((number_of_graduated_teacher*(number_of_rural_schools/number_of_schools))/parameter_teacher_class)/parameter_teacher_class)*reference_multiplier40h"
  sce2_rur_dim_3_price_course_for_trainers_scenario_online40 <- str_replace_all(sce2_rur_dim_3_price_course_for_trainers_scenario_online40, map_df)
  writeFormula(wb, sheet="TotalperItem", x=paste0("=", sce2_rur_dim_3_price_course_for_trainers_scenario_online40), startCol=2, startRow=101)
  
  # Dimensão 3 - Training design (Scenario online-40) (Cenário 2) 
  writeData(wb, sheet = "TotalperItem", x="sce2_rur_dim_3_price_training_design_scenario_online40", startCol=1, startRow=102)
  
  sce2_rur_dim_3_price_training_design_scenario_online40 <- "reference_price_training_design*reference_multiplier40h"
  sce2_rur_dim_3_price_training_design_scenario_online40 <- str_replace_all(sce2_rur_dim_3_price_training_design_scenario_online40, map_df)
  writeFormula(wb, sheet="TotalperItem", x=paste0("=", sce2_rur_dim_3_price_training_design_scenario_online40), startCol=2, startRow=102)
  
  # Dimensão 3 - Specialist Trainers (Scenario in-person) (Cenário 2) 
  writeData(wb, sheet = "TotalperItem", x="sce2_rur_dim_3_price_specialist_trainers_scenario_inperson40", startCol=1, startRow=103)
  
  sce2_rur_dim_3_price_specialist_trainers_scenario_inperson40 <- "(reference_price_trainers60*((number_of_graduated_teacher*(number_of_rural_schools/number_of_schools))/parameter_teacher_class))*reference_multiplier40h*reference_multiplier_inperson"
  sce2_rur_dim_3_price_specialist_trainers_scenario_inperson40 <- str_replace_all(sce2_rur_dim_3_price_specialist_trainers_scenario_inperson40, map_df)
  writeFormula(wb, sheet="TotalperItem", x=paste0("=", sce2_rur_dim_3_price_specialist_trainers_scenario_inperson40), startCol=2, startRow=103)
  
  # Dimensão 3 - Course for Trainers (Scenario in-person) (Cenário 2) 
  writeData(wb, sheet = "TotalperItem", x="sce2_rur_dim_3_price_course_for_trainers_scenario_inperson40", startCol=1, startRow=104)
  
  sce2_rur_dim_3_price_course_for_trainers_scenario_inperson40 <- "(reference_price_trainers60*1.5*((number_of_graduated_teacher*(number_of_rural_schools/number_of_schools))/parameter_teacher_class)/parameter_teacher_class)*reference_multiplier40h*reference_multiplier_inperson"
  sce2_rur_dim_3_price_course_for_trainers_scenario_inperson40 <- str_replace_all(sce2_rur_dim_3_price_course_for_trainers_scenario_inperson40, map_df)
  writeFormula(wb, sheet="TotalperItem", x=paste0("=", sce2_rur_dim_3_price_course_for_trainers_scenario_inperson40), startCol=2, startRow=104)
  
  # Dimensão 3 - Training design (Scenario in-person) (Cenário 2) 
  writeData(wb, sheet = "TotalperItem", x="sce2_rur_dim_3_price_training_design_scenario_inperson40", startCol=1, startRow=105)
  
  sce2_rur_dim_3_price_training_design_scenario_inperson40 <- "reference_price_training_design*reference_multiplier40h*reference_multiplier_inperson"
  sce2_rur_dim_3_price_training_design_scenario_inperson40 <- str_replace_all(sce2_rur_dim_3_price_training_design_scenario_inperson40, map_df)
  writeFormula(wb, sheet="TotalperItem", x=paste0("=", sce2_rur_dim_3_price_training_design_scenario_inperson40), startCol=2, startRow=105)
  
  # Dimensão 4 - Teaching and learning platform (Cenário 2) 
  writeData(wb, sheet = "TotalperItem", x="sce2_rur_dim_4_price_teaching_learning_platform", startCol=1, startRow=106)
  
  sce2_rur_dim_4_price_teaching_learning_platform <- "((number_of_students_secondary+number_of_students_primary)*(number_of_rural_schools/number_of_schools))*reference_price_teaching_learning*parameter_teaching_learning*(type_opex/12)"
  sce2_rur_dim_4_price_teaching_learning_platform <- str_replace_all(sce2_rur_dim_4_price_teaching_learning_platform, map_df)
  writeFormula(wb, sheet="TotalperItem", x=paste0("=", sce2_rur_dim_4_price_teaching_learning_platform), startCol=2, startRow=106)
  
  # Dimensão 4 - Management platform (Cenário 2) 
  writeData(wb, sheet = "TotalperItem", x="sce2_rur_dim_4_price_management_platform", startCol=1, startRow=107)
  
  sce2_rur_dim_4_price_management_platform <- "number_of_rural_schools*reference_price_management_platform*parameter_management*(type_opex/12)"
  sce2_rur_dim_4_price_management_platform <- str_replace_all(sce2_rur_dim_4_price_management_platform, map_df)
  writeFormula(wb, sheet="TotalperItem", x=paste0("=", sce2_rur_dim_4_price_management_platform), startCol=2, startRow=107)
  
  # Dimensão 5 - Central team (Cenário 2) 
  writeData(wb, sheet = "TotalperItem", x="sce2_rur_dim_5_price_central_team", startCol=1, startRow=108)
  
  sce2_rur_dim_5_price_central_team <- "reference_consultant_price*((parameter_central_team_min+parameter_central_team_max)/2)*type_opex"
  sce2_rur_dim_5_price_central_team <- str_replace_all(sce2_rur_dim_5_price_central_team, map_df)
  writeFormula(wb, sheet="TotalperItem", x=paste0("=", sce2_rur_dim_5_price_central_team), startCol=2, startRow=108)
  
  # Dimensão 5 - Regional team (Cenário 2) 
  writeData(wb, sheet = "TotalperItem", x="sce2_rur_dim_5_price_regional_team", startCol=1, startRow=109)
  
  sce2_rur_dim_5_price_regional_team <- "parameter_regional_team*number_of_rural_schools*reference_teacher_price*type_opex"
  sce2_rur_dim_5_price_regional_team <- str_replace_all(sce2_rur_dim_5_price_regional_team, map_df)
  writeFormula(wb, sheet="TotalperItem", x=paste0("=", sce2_rur_dim_5_price_regional_team), startCol=2, startRow=109)
  
  # Dimensão 5 - Local team (Cenário 2) 
  writeData(wb, sheet = "TotalperItem", x="sce2_rur_dim_5_price_local_team", startCol=1, startRow=110)
  
  sce2_rur_dim_5_price_local_team <- "parameter_local_team_rural*number_of_rural_schools*reference_teacher_price*type_opex"
  sce2_rur_dim_5_price_local_team <- str_replace_all(sce2_rur_dim_5_price_local_team, map_df)
  writeFormula(wb, sheet="TotalperItem", x=paste0("=", sce2_rur_dim_5_price_local_team), startCol=2, startRow=110)
  
  #---- TotalperItem: Cenário 3 ----
  
  # Dimensão 2: Apenas equipamentos para professores e escolas (Cenário 3)
  writeData(wb, sheet = "TotalperItem", x="sce3_devt_dim_2_price_device_teacher", startCol=1, startRow=111)
  
  sce3_devt_dim_2_price_device_teacher <- "number_of_teachers*parameter_device_per_teacher*reference_laptop_price"
  sce3_devt_dim_2_price_device_teacher <- str_replace_all(sce3_devt_dim_2_price_device_teacher, map_df)
  writeFormula(wb, sheet="TotalperItem", x=paste0("=", sce3_devt_dim_2_price_device_teacher), startCol=2, startRow=111)
  
  # Dimensão 2: Apenas equipamentos para professores e escolas (Cenário 3)
  writeData(wb, sheet = "TotalperItem", x="sce3_devt_dim_2_price_device_school", startCol=1, startRow=112)
  
  sce3_devt_dim_2_price_device_school <- "number_of_schools*parameter_device_per_school*reference_desktop_price"
  sce3_devt_dim_2_price_device_school <- str_replace_all(sce3_devt_dim_2_price_device_school, map_df)
  writeFormula(wb, sheet="TotalperItem", x=paste0("=", sce3_devt_dim_2_price_device_school), startCol=2, startRow=112)
  
  # Dimensão 2: Apenas equipamentos para professores e escolas (Cenário 3)
  writeData(wb, sheet = "TotalperItem", x="sce3_devt_dim_2_price_multimedia_projector", startCol=1, startRow=113)
  
  sce3_devt_dim_2_price_multimedia_projector <- "number_of_schools*parameter_multimedia_projector*reference_multimedia_projector_price"
  sce3_devt_dim_2_price_multimedia_projector <- str_replace_all(sce3_devt_dim_2_price_multimedia_projector, map_df)
  writeFormula(wb, sheet="TotalperItem", x=paste0("=", sce3_devt_dim_2_price_multimedia_projector), startCol=2, startRow=113)
  
  
  #---- TotalperItem: Estilo da planilha ----
  
  # Estabelece o tamanho da coluna 3, que possui os nomes das variáveis
  setColWidths(wb, sheet = "TotalperItem", cols = 1, widths = 52) 
  setColWidths(wb, sheet = "TotalperItem", cols = 2, widths = 16)
  
  # Estabelece os estilos das colunas da planilha
  
  # Inclui bordas  
  addStyle(wb, sheet="TotalperItem", general_style, rows = 1:113, cols = 1)
  addStyle(wb, sheet="TotalperItem", general_style, rows = 1:2, cols = 2)
  # Ajusta as variáveis numéricas
  addStyle(wb, sheet="TotalperItem", num_style, rows = 3:113, cols = 2)
  
  # Inclui cores
  addStyle(wb, sheet="TotalperItem", style = sce0, rows = 3:38, cols = 1)
  addStyle(wb, sheet="TotalperItem", style = sce1, rows = 39:74, cols = 1)
  addStyle(wb, sheet="TotalperItem", style = sce2, rows = 75:110, cols = 1)
  addStyle(wb, sheet="TotalperItem", style = sce3, rows = 111:113, cols = 1)
  
  #---- TotalperDimension -----
  
  # Adiciona uma planilha ao arquivo criado
  addWorksheet(wb, "TotalperDimension")
  
  # Nome do país
  writeFormula(wb, sheet="TotalperDimension", x=paste0("=Inputs!A", 1), startCol=1, startRow=1)
  writeFormula(wb, sheet="TotalperDimension", x=paste0("=Inputs!A", 2), startCol=2, startRow=1)
  # Nome da subregião
  writeFormula(wb, sheet="TotalperDimension", x=paste0("=Inputs!B", 1), startCol=1, startRow=2)
  writeFormula(wb, sheet="TotalperDimension", x=paste0("=Inputs!B", 2), startCol=2, startRow=2)
  
  #---- TotalperDimension: Cenário geral -----
  
  # Total da dimensão 1
  writeData(wb, sheet = "TotalperDimension", x="dim_1_total", startCol=1, startRow=3)
  dim_1_total <- paste0("(", dim_1_price_fiber_exp, ")", "+", "(", dim_1_price_satellite_service, ")", "+", "(", dim_1_price_fiber_link, ")", "+", "(", dim_1_price_access_point, ")", "+", "(", dim_1_price_firewall, ")", "+", "(", dim_1_price_nobreak, ")", "+", "(", dim_1_price_switch, ")", "+", "(", dim_1_price_rack, ")")
  writeFormula(wb, sheet="TotalperDimension", x=paste0("=", dim_1_total), startCol=2, startRow=3)

  # Total da dimensão 2 (min)
  writeData(wb, sheet = "TotalperDimension", x="dim_2_total_min", startCol=1, startRow=4)
  dim_2_total_min <- paste0("(", dim_2_price_device_student_min, ")", "+", "(", dim_2_price_device_teacher, ")", "+", "(", dim_2_price_device_school, ")", "+", "(", dim_2_price_charging_cart_min, ")", "+", "(", dim_2_price_multimedia_projector, ")", "+", "(", dim_2_price_headphones_min_scenario, ")")
  writeFormula(wb, sheet="TotalperDimension", x=paste0("=", dim_2_total_min), startCol=2, startRow=4)
  
  # Total da dimensão 2 (mild)
  writeData(wb, sheet = "TotalperDimension", x="dim_2_total_mild", startCol=1, startRow=5)
  dim_2_total_mild <- paste0("(", dim_2_price_device_student_mild, ")", "+", "(", dim_2_price_device_teacher, ")", "+", "(", dim_2_price_device_school, ")", "+", "(", dim_2_price_charging_cart_mild, ")", "+", "(", dim_2_price_multimedia_projector, ")", "+", "(", dim_2_price_headphones_mild_scenario, ")")
  writeFormula(wb, sheet="TotalperDimension", x=paste0("=", dim_2_total_mild), startCol=2, startRow=5)
  
  # Total da dimensão 2 (comprehensive)
  writeData(wb, sheet = "TotalperDimension", x="dim_2_total_comprehensive", startCol=1, startRow=6)
  dim_2_total_comprehensive <- paste0("(", dim_2_price_device_student_comprehensive, ")", "+", "(", dim_2_price_device_teacher, ")", "+", "(", dim_2_price_device_school, ")", "+", "(", dim_2_price_charging_cart_comprehensive, ")", "+", "(", dim_2_price_multimedia_projector, ")", "+", "(", dim_2_price_headphones_comprehensive, ")")
  writeFormula(wb, sheet="TotalperDimension", x=paste0("=", dim_2_total_comprehensive), startCol=2, startRow=6)
  
  # Total da dimensão 3
  writeData(wb, sheet = "TotalperDimension", x="dim_3_total_online_60", startCol=1, startRow=7)
  dim_3_total_online_60 <- paste0("(", dim_3_price_diagnostic_tool, ")", "+", "(", dim_3_price_lms_platform, ")", "+", "(", dim_3_price_specialist_trainers_scenario_online60, ")", "+", "(", dim_3_price_course_for_trainers_scenario_online60, ")", "+", "(", dim_3_price_training_design_scenario_online60, ")")
  writeFormula(wb, sheet="TotalperDimension", x=paste0("=", dim_3_total_online_60), startCol=2, startRow=7)
  
  # Total da dimensão 3
  writeData(wb, sheet = "TotalperDimension", x="dim_3_total_online_40", startCol=1, startRow=8)
  dim_3_total_online_40 <- paste0("(", dim_3_price_diagnostic_tool, ")", "+", "(", dim_3_price_lms_platform, ")", "+", "(", dim_3_price_specialist_trainers_scenario_online40, ")", "+", "(", dim_3_price_course_for_trainers_scenario_online40, ")", "+", "(", dim_3_price_training_design_scenario_online40, ")")
  writeFormula(wb, sheet="TotalperDimension", x=paste0("=", dim_3_total_online_40), startCol=2, startRow=8)
  
  # Total da dimensão 3
  writeData(wb, sheet = "TotalperDimension", x="dim_3_total_inperson_40", startCol=1, startRow=9)
  dim_3_total_inperson_40 <- paste0("(", dim_3_price_diagnostic_tool, ")", "+", "(", dim_3_price_lms_platform, ")", "+", "(", dim_3_price_specialist_trainers_scenario_inperson40, ")", "+", "(", dim_3_price_course_for_trainers_scenario_inperson40, ")", "+", "(", dim_3_price_training_design_scenario_inperson40, ")")
  writeFormula(wb, sheet="TotalperDimension", x=paste0("=", dim_3_total_inperson_40), startCol=2, startRow=9)
  
  # Total da dimensão 4
  writeData(wb, sheet = "TotalperDimension", x="dim_4_total", startCol=1, startRow=10)
  dim_4_total <- paste0("(", dim_4_price_teaching_learning_platform, ")", "+", "(", dim_4_price_management_platform, ")")
  writeFormula(wb, sheet="TotalperDimension", x=paste0("=", dim_4_total), startCol=2, startRow=10)
  
  # Total da dimensão 5
  writeData(wb, sheet = "TotalperDimension", x="dim_5_total", startCol=1, startRow=11)
  dim_5_total <- paste0("(", dim_5_price_central_team, ")", "+", "(", dim_5_price_regional_team, ")", "+", "(", dim_5_price_local_team, ")")
  writeFormula(wb, sheet="TotalperDimension", x=paste0("=", dim_5_total), startCol=2, startRow=11)
  
  #---- TotalperDimension: Cenário 1 -----
  
  # Total da dimensão 1
  writeData(wb, sheet = "TotalperDimension", x="sce1_sec_dim_1_total", startCol=1, startRow=12)
  sce1_sec_dim_1_total <- paste0("(", sce1_sec_dim_1_price_fiber_exp, ")", "+", "(", sce1_sec_dim_1_price_satellite_service, ")", "+", "(", sce1_sec_dim_1_price_fiber_link, ")", "+", "(", sce1_sec_dim_1_price_access_point, ")", "+", "(", sce1_sec_dim_1_price_firewall, ")", "+", "(", sce1_sec_dim_1_price_nobreak, ")", "+", "(", sce1_sec_dim_1_price_switch, ")", "+", "(", sce1_sec_dim_1_price_rack, ")")
  writeFormula(wb, sheet="TotalperDimension", x=paste0("=", sce1_sec_dim_1_total), startCol=2, startRow=12)
  
  # Total da dimensão 2 (min)
  writeData(wb, sheet = "TotalperDimension", x="sce1_sec_dim_2_total_min", startCol=1, startRow=13)
  sce1_sec_dim_2_total_min <- paste0("(", sce1_sec_dim_2_price_device_student_min, ")", "+", "(", sce1_sec_dim_2_price_device_teacher, ")", "+", "(", sce1_sec_dim_2_price_device_school, ")", "+", "(", sce1_sec_dim_2_price_charging_cart_min, ")", "+", "(", sce1_sec_dim_2_price_multimedia_projector, ")", "+", "(", sce1_sec_dim_2_price_headphones_min_scenario, ")")
  writeFormula(wb, sheet="TotalperDimension", x=paste0("=", sce1_sec_dim_2_total_min), startCol=2, startRow=13)
  
  # Total da dimensão 2 (mild)
  writeData(wb, sheet = "TotalperDimension", x="sce1_sec_dim_2_total_mild", startCol=1, startRow=14)
  sce1_sec_dim_2_total_mild <- paste0("(", sce1_sec_dim_2_price_device_student_mild, ")", "+", "(", sce1_sec_dim_2_price_device_teacher, ")", "+", "(", sce1_sec_dim_2_price_device_school, ")", "+", "(", sce1_sec_dim_2_price_charging_cart_mild, ")", "+", "(", sce1_sec_dim_2_price_multimedia_projector, ")", "+", "(", sce1_sec_dim_2_price_headphones_mild_scenario, ")")
  writeFormula(wb, sheet="TotalperDimension", x=paste0("=", sce1_sec_dim_2_total_mild), startCol=2, startRow=14)
  
  # Total da dimensão 2 (comprehensive)
  writeData(wb, sheet = "TotalperDimension", x="sce1_sec_dim_2_total_comprehensive", startCol=1, startRow=15)
  sce1_sec_dim_2_total_comprehensive <- paste0("(", sce1_sec_dim_2_price_device_student_comprehensive, ")", "+", "(", sce1_sec_dim_2_price_device_teacher, ")", "+", "(", sce1_sec_dim_2_price_device_school, ")", "+", "(", sce1_sec_dim_2_price_charging_cart_comprehensive, ")", "+", "(", sce1_sec_dim_2_price_multimedia_projector, ")", "+", "(", sce1_sec_dim_2_price_headphones_comprehensive, ")")
  writeFormula(wb, sheet="TotalperDimension", x=paste0("=", sce1_sec_dim_2_total_comprehensive), startCol=2, startRow=15)
  
  # Total da dimensão 3
  writeData(wb, sheet = "TotalperDimension", x="sce1_sec_dim_3_total_online_60", startCol=1, startRow=16)
  sce1_sec_dim_3_total_online_60 <- paste0("(", sce1_sec_dim_3_price_diagnostic_tool, ")", "+", "(", sce1_sec_dim_3_price_lms_platform, ")", "+", "(", sce1_sec_dim_3_price_specialist_trainers_scenario_online60, ")", "+", "(", sce1_sec_dim_3_price_course_for_trainers_scenario_online60, ")", "+", "(", sce1_sec_dim_3_price_training_design_scenario_online60, ")")
  writeFormula(wb, sheet="TotalperDimension", x=paste0("=", sce1_sec_dim_3_total_online_60), startCol=2, startRow=16)
  
  # Total da dimensão 3
  writeData(wb, sheet = "TotalperDimension", x="sce1_sec_dim_3_total_online_40", startCol=1, startRow=17)
  sce1_sec_dim_3_total_online_40 <- paste0("(", sce1_sec_dim_3_price_diagnostic_tool, ")", "+", "(", sce1_sec_dim_3_price_lms_platform, ")", "+", "(", sce1_sec_dim_3_price_specialist_trainers_scenario_online40, ")", "+", "(", sce1_sec_dim_3_price_course_for_trainers_scenario_online40, ")", "+", "(", sce1_sec_dim_3_price_training_design_scenario_online40, ")")
  writeFormula(wb, sheet="TotalperDimension", x=paste0("=", sce1_sec_dim_3_total_online_40), startCol=2, startRow=17)
  
  # Total da dimensão 3
  writeData(wb, sheet = "TotalperDimension", x="sce1_sec_dim_3_total_inperson_40", startCol=1, startRow=18)
  sce1_sec_dim_3_total_inperson_40 <- paste0("(", sce1_sec_dim_3_price_diagnostic_tool, ")", "+", "(", sce1_sec_dim_3_price_lms_platform, ")", "+", "(", sce1_sec_dim_3_price_specialist_trainers_scenario_inperson40, ")", "+", "(", sce1_sec_dim_3_price_course_for_trainers_scenario_inperson40, ")", "+", "(", sce1_sec_dim_3_price_training_design_scenario_inperson40, ")")
  writeFormula(wb, sheet="TotalperDimension", x=paste0("=", sce1_sec_dim_3_total_inperson_40), startCol=2, startRow=18)
  
  # Total da dimensão 4
  writeData(wb, sheet = "TotalperDimension", x="sce1_sec_dim_4_total", startCol=1, startRow=19)
  sce1_sec_dim_4_total <- paste0("(", sce1_sec_dim_4_price_teaching_learning_platform, ")", "+", "(", sce1_sec_dim_4_price_management_platform, ")")
  writeFormula(wb, sheet="TotalperDimension", x=paste0("=", sce1_sec_dim_4_total), startCol=2, startRow=19)
  
  # Total da dimensão 5
  writeData(wb, sheet = "TotalperDimension", x="sce1_sec_dim_5_total", startCol=1, startRow=20)
  sce1_sec_dim_5_total <- paste0("(", sce1_sec_dim_5_price_central_team, ")", "+", "(", sce1_sec_dim_5_price_regional_team, ")", "+", "(", sce1_sec_dim_5_price_local_team, ")")
  writeFormula(wb, sheet="TotalperDimension", x=paste0("=", sce1_sec_dim_5_total), startCol=2, startRow=20)
  
  #---- TotalperDimension: Cenário 2 -----
  
  # Total da dimensão 1
  writeData(wb, sheet = "TotalperDimension", x="sce2_rur_dim_1_total", startCol=1, startRow=21)
  sce2_rur_dim_1_total <- paste0("(", sce2_rur_dim_1_price_fiber_exp, ")", "+", "(", sce2_rur_dim_1_price_satellite_service, ")", "+", "(", sce2_rur_dim_1_price_fiber_link, ")", "+", "(", sce2_rur_dim_1_price_access_point, ")", "+", "(", sce2_rur_dim_1_price_firewall, ")", "+", "(", sce2_rur_dim_1_price_nobreak, ")", "+", "(", sce2_rur_dim_1_price_switch, ")", "+", "(", sce2_rur_dim_1_price_rack, ")")
  writeFormula(wb, sheet="TotalperDimension", x=paste0("=", sce2_rur_dim_1_total), startCol=2, startRow=21)
  
  # Total da dimensão 2 (min)
  writeData(wb, sheet = "TotalperDimension", x="sce2_rur_dim_2_total_min", startCol=1, startRow=22)
  sce2_rur_dim_2_total_min <- paste0("(", sce2_rur_dim_2_price_device_student_min, ")", "+", "(", sce2_rur_dim_2_price_device_teacher, ")", "+", "(", sce2_rur_dim_2_price_device_school, ")", "+", "(", sce2_rur_dim_2_price_charging_cart_min, ")", "+", "(", sce2_rur_dim_2_price_multimedia_projector, ")", "+", "(", sce2_rur_dim_2_price_headphones_min_scenario, ")")
  writeFormula(wb, sheet="TotalperDimension", x=paste0("=", sce2_rur_dim_2_total_min), startCol=2, startRow=22)
  
  # Total da dimensão 2 (mild)
  writeData(wb, sheet = "TotalperDimension", x="sce2_rur_dim_2_total_mild", startCol=1, startRow=23)
  sce2_rur_dim_2_total_mild <- paste0("(", sce2_rur_dim_2_price_device_student_mild, ")", "+", "(", sce2_rur_dim_2_price_device_teacher, ")", "+", "(", sce2_rur_dim_2_price_device_school, ")", "+", "(", sce2_rur_dim_2_price_charging_cart_mild, ")", "+", "(", sce2_rur_dim_2_price_multimedia_projector, ")", "+", "(", sce2_rur_dim_2_price_headphones_mild_scenario, ")")
  writeFormula(wb, sheet="TotalperDimension", x=paste0("=", sce2_rur_dim_2_total_mild), startCol=2, startRow=23)
  
  # Total da dimensão 2 (comprehensive)
  writeData(wb, sheet = "TotalperDimension", x="sce2_rur_dim_2_total_comprehensive", startCol=1, startRow=24)
  sce2_rur_dim_2_total_comprehensive <- paste0("(", sce2_rur_dim_2_price_device_student_comprehensive, ")", "+", "(", sce2_rur_dim_2_price_device_teacher, ")", "+", "(", sce2_rur_dim_2_price_device_school, ")", "+", "(", sce2_rur_dim_2_price_charging_cart_comprehensive, ")", "+", "(", sce2_rur_dim_2_price_multimedia_projector, ")", "+", "(", sce2_rur_dim_2_price_headphones_comprehensive, ")")
  writeFormula(wb, sheet="TotalperDimension", x=paste0("=", sce2_rur_dim_2_total_comprehensive), startCol=2, startRow=24)
  
  # Total da dimensão 3
  writeData(wb, sheet = "TotalperDimension", x="sce2_rur_dim_3_total_online_60", startCol=1, startRow=25)
  sce2_rur_dim_3_total_online_60 <- paste0("(", sce2_rur_dim_3_price_diagnostic_tool, ")", "+", "(", sce2_rur_dim_3_price_lms_platform, ")", "+", "(", sce2_rur_dim_3_price_specialist_trainers_scenario_online60, ")", "+", "(", sce2_rur_dim_3_price_course_for_trainers_scenario_online60, ")", "+", "(", sce2_rur_dim_3_price_training_design_scenario_online60, ")")
  writeFormula(wb, sheet="TotalperDimension", x=paste0("=", sce2_rur_dim_3_total_online_60), startCol=2, startRow=25)
  
  # Total da dimensão 3
  writeData(wb, sheet = "TotalperDimension", x="sce2_rur_dim_3_total_online_40", startCol=1, startRow=26)
  sce2_rur_dim_3_total_online_40 <- paste0("(", sce2_rur_dim_3_price_diagnostic_tool, ")", "+", "(", sce2_rur_dim_3_price_lms_platform, ")", "+", "(", sce2_rur_dim_3_price_specialist_trainers_scenario_online40, ")", "+", "(", sce2_rur_dim_3_price_course_for_trainers_scenario_online40, ")", "+", "(", sce2_rur_dim_3_price_training_design_scenario_online40, ")")
  writeFormula(wb, sheet="TotalperDimension", x=paste0("=", sce2_rur_dim_3_total_online_40), startCol=2, startRow=26)
  
  # Total da dimensão 3
  writeData(wb, sheet = "TotalperDimension", x="sce2_rur_dim_3_total_inperson_40", startCol=1, startRow=27)
  sce2_rur_dim_3_total_inperson_40 <- paste0("(", sce2_rur_dim_3_price_diagnostic_tool, ")", "+", "(", sce2_rur_dim_3_price_lms_platform, ")", "+", "(", sce2_rur_dim_3_price_specialist_trainers_scenario_inperson40, ")", "+", "(", sce2_rur_dim_3_price_course_for_trainers_scenario_inperson40, ")", "+", "(", sce2_rur_dim_3_price_training_design_scenario_inperson40, ")")
  writeFormula(wb, sheet="TotalperDimension", x=paste0("=", sce2_rur_dim_3_total_inperson_40), startCol=2, startRow=27)
  
  # Total da dimensão 4
  writeData(wb, sheet = "TotalperDimension", x="sce2_rur_dim_4_total", startCol=1, startRow=28)
  sce2_rur_dim_4_total <- paste0("(", sce2_rur_dim_4_price_teaching_learning_platform, ")", "+", "(", sce2_rur_dim_4_price_management_platform, ")")
  writeFormula(wb, sheet="TotalperDimension", x=paste0("=", sce2_rur_dim_4_total), startCol=2, startRow=28)
  
  # Total da dimensão 5
  writeData(wb, sheet = "TotalperDimension", x="sce2_rur_dim_5_total", startCol=1, startRow=29)
  sce2_rur_dim_5_total <- paste0("(", sce2_rur_dim_5_price_central_team, ")", "+", "(", sce2_rur_dim_5_price_regional_team, ")", "+", "(", sce2_rur_dim_5_price_local_team, ")")
  writeFormula(wb, sheet="TotalperDimension", x=paste0("=", sce2_rur_dim_5_total), startCol=2, startRow=29)
  
  #---- TotalperDimension: Cenário 3 -----
  
  # Total da dimensão 2 (comprehensive)
  writeData(wb, sheet = "TotalperDimension", x="sce3_devt_dim_2_total", startCol=1, startRow=30)
  sce3_devt_dim_2_total <- paste0("(", sce3_devt_dim_2_price_device_teacher, "+", sce3_devt_dim_2_price_device_school, "+", sce3_devt_dim_2_price_multimedia_projector, ")", "/1000000")
  writeFormula(wb, sheet="TotalperDimension", x=paste0("=", sce3_devt_dim_2_total), startCol=2, startRow=30)
  
  #---- TotalperDimension: Estilo da planilha ----
  
  # Estabelece o tamanho da coluna 3, que possui os nomes das variáveis
  setColWidths(wb, sheet = "TotalperDimension", cols = 1, widths = 30) 
  setColWidths(wb, sheet = "TotalperDimension", cols = 2, widths = 16)
  
  # Estabelece os estilos das colunas da planilha
  
  # Inclui bordas  
  addStyle(wb, sheet="TotalperDimension", general_style, rows = 1:30, cols = 1)
  addStyle(wb, sheet="TotalperDimension", general_style, rows = 1:2, cols = 2)
  # Ajusta as variáveis numéricas
  addStyle(wb, sheet="TotalperDimension", num_style, rows = 3:30, cols = 2)
  
  # Inclui cores
  addStyle(wb, sheet="TotalperDimension", style = sce0, rows = 3:11, cols = 1)
  addStyle(wb, sheet="TotalperDimension", style = sce1, rows = 12:20, cols = 1)
  addStyle(wb, sheet="TotalperDimension", style = sce2, rows = 21:29, cols = 1)
  addStyle(wb, sheet="TotalperDimension", style = sce3, rows = 30, cols = 1)
  
  #---- Scenarios -----
  
  ## Adiciona uma planilha ao arquivo criado
  #addWorksheet(wb, "Scenarios")
  #
  ## Nome do país
  #writeFormula(wb, sheet="Scenarios", x=paste0("=Inputs!A", 1), startCol=1, startRow=1)
  #writeFormula(wb, sheet="Scenarios", x=paste0("=Inputs!A", 2), startCol=2, startRow=1)
  ## Nome da subregião
  #writeFormula(wb, sheet="Scenarios", x=paste0("=Inputs!B", 1), startCol=1, startRow=2)
  #writeFormula(wb, sheet="Scenarios", x=paste0("=Inputs!B", 2), startCol=2, startRow=2)
  
  #---- Scenarios: Cenário geral -----
  
  #writeData(wb, sheet = "Scenarios", x="total_geral_min_dev_online_60", startCol=1, startRow=3)
  #total_geral_min_dev_online_60 <- paste0(dim_1_total, "+", dim_2_total_min, "+", dim_3_total_online_60, "+", dim_4_total, "+", dim_5_total)
  #writeFormula(wb, sheet="Scenarios", x=paste0("=", total_geral_min_dev_online_60), startCol=2, startRow=3)
  #
  #writeData(wb, sheet = "Scenarios", x="total_geral_min_dev_online_40", startCol=1, startRow=4)
  #total_geral_min_dev_online_40 <- paste0(dim_1_total, "+", dim_2_total_min, "+", dim_3_total_online_40, "+", dim_4_total, "+", dim_5_total)
  #writeFormula(wb, sheet="Scenarios", x=paste0("=", total_geral_min_dev_online_40), startCol=2, startRow=4)
  #
  #writeData(wb, sheet = "Scenarios", x="total_geral_min_dev_inperson_40", startCol=1, startRow=5)
  #total_geral_min_dev_inperson_40 <- paste0(dim_1_total, "+", dim_2_total_min, "+", dim_3_total_inperson_40, "+", dim_4_total, "+", dim_5_total)
  #writeFormula(wb, sheet="Scenarios", x=paste0("=", total_geral_min_dev_inperson_40), startCol=2, startRow=5)
  #
  #writeData(wb, sheet = "Scenarios", x="total_geral_mild_dev_online_60", startCol=1, startRow=6)
  #total_geral_mild_dev_online_60 <- paste0(dim_1_total, "+", dim_2_total_mild, "+", dim_3_total_online_60, "+", dim_4_total, "+", dim_5_total)
  #writeFormula(wb, sheet="Scenarios", x=paste0("=", total_geral_mild_dev_online_60), startCol=2, startRow=6)
  #
  #writeData(wb, sheet = "Scenarios", x="total_geral_mild_dev_online_40", startCol=1, startRow=7)
  #total_geral_mild_dev_online_40 <- paste0(dim_1_total, "+", dim_2_total_mild, "+", dim_3_total_online_40, "+", dim_4_total, "+", dim_5_total)
  #writeFormula(wb, sheet="Scenarios", x=paste0("=", total_geral_mild_dev_online_40), startCol=2, startRow=7)
  #
  #writeData(wb, sheet = "Scenarios", x="total_geral_mild_dev_inperson_40", startCol=1, startRow=8)
  #total_geral_mild_dev_inperson_40 <- paste0(dim_1_total, "+", dim_2_total_mild, "+", dim_3_total_inperson_40, "+", dim_4_total, "+", dim_5_total)
  #writeFormula(wb, sheet="Scenarios", x=paste0("=", total_geral_mild_dev_inperson_40), startCol=2, startRow=8)
  #
  #writeData(wb, sheet = "Scenarios", x="total_geral_comp_dev_online_60", startCol=1, startRow=9)
  #total_geral_comp_dev_online_60 <- paste0(dim_1_total, "+", dim_2_total_comprehensive, "+", dim_3_total_online_60, "+", dim_4_total, "+", dim_5_total)
  #writeFormula(wb, sheet="Scenarios", x=paste0("=", total_geral_comp_dev_online_60), startCol=2, startRow=9)
  #
  #writeData(wb, sheet = "Scenarios", x="total_geral_comp_dev_online_40", startCol=1, startRow=10)
  #total_geral_comp_dev_online_40 <- paste0(dim_1_total, "+", dim_2_total_comprehensive, "+", dim_3_total_online_40, "+", dim_4_total, "+", dim_5_total)
  #writeFormula(wb, sheet="Scenarios", x=paste0("=", total_geral_comp_dev_online_40), startCol=2, startRow=10)
  #
  #writeData(wb, sheet = "Scenarios", x="total_geral_comp_dev_inperson_40", startCol=1, startRow=11)
  #total_geral_comp_dev_inperson_40 <- paste0(dim_1_total, "+", dim_2_total_comprehensive, "+", dim_3_total_inperson_40, "+", dim_4_total, "+", dim_5_total)
  #writeFormula(wb, sheet="Scenarios", x=paste0("=", total_geral_comp_dev_inperson_40), startCol=2, startRow=11)
  #
  #writeData(wb, sheet = "Scenarios", x="total_geral_min_dev_online_60_menos_25", startCol=1, startRow=12)
  #total_geral_min_dev_online_60_menos_25 <- paste0("(", total_geral_min_dev_online_60, ")", "*", "0.75")
  #writeFormula(wb, sheet="Scenarios", x=paste0("=", total_geral_min_dev_online_60_menos_25), startCol=2, startRow=12)
  #writeData(wb, sheet = "Scenarios", x="total_geral_min_dev_online_60_mais_25", startCol=1, startRow=13)
  #total_geral_min_dev_online_60_mais_25 <- paste0("(", total_geral_min_dev_online_60, ")", "*", "1.25")
  #writeFormula(wb, sheet="Scenarios", x=paste0("=", total_geral_min_dev_online_60_mais_25), startCol=2, startRow=13)
  #
  #writeData(wb, sheet = "Scenarios", x="total_geral_min_dev_online_40_menos_25", startCol=1, startRow=14)
  #total_geral_min_dev_online_40_menos_25 <- paste0("(", total_geral_min_dev_online_40, ")", "*", "0.75")
  #writeFormula(wb, sheet="Scenarios", x=paste0("=", total_geral_min_dev_online_40_menos_25), startCol=2, startRow=14)
  #writeData(wb, sheet = "Scenarios", x="total_geral_min_dev_online_40_mais_25", startCol=1, startRow=15)
  #total_geral_min_dev_online_40_mais_25 <- paste0("(", total_geral_min_dev_online_40, ")", "*", "1.25")
  #writeFormula(wb, sheet="Scenarios", x=paste0("=", total_geral_min_dev_online_40_mais_25), startCol=2, startRow=15)
  #
  #writeData(wb, sheet = "Scenarios", x="total_geral_min_dev_inperson_40_menos_25", startCol=1, startRow=16)
  #total_geral_min_dev_inperson_40_menos_25 <- paste0("(", total_geral_min_dev_inperson_40, ")", "*", "0.75")
  #writeFormula(wb, sheet="Scenarios", x=paste0("=", total_geral_min_dev_inperson_40_menos_25), startCol=2, startRow=16)
  #writeData(wb, sheet = "Scenarios", x="total_geral_min_dev_inperson_40_mais_25", startCol=1, startRow=17)
  #total_geral_min_dev_inperson_40_mais_25 <- paste0("(", total_geral_min_dev_inperson_40, ")", "*", "1.25")
  #writeFormula(wb, sheet="Scenarios", x=paste0("=", total_geral_min_dev_inperson_40_mais_25), startCol=2, startRow=17)
  #
  #writeData(wb, sheet = "Scenarios", x="total_geral_mild_dev_online_60_menos_25", startCol=1, startRow=18)
  #total_geral_mild_dev_online_60_menos_25 <- paste0("(", total_geral_mild_dev_online_60, ")", "*", "0.75")
  #writeFormula(wb, sheet="Scenarios", x=paste0("=", total_geral_mild_dev_online_60_menos_25), startCol=2, startRow=18)
  #writeData(wb, sheet = "Scenarios", x="total_geral_mild_dev_online_60_mais_25", startCol=1, startRow=19)
  #total_geral_mild_dev_online_60_mais_25 <- paste0("(", total_geral_mild_dev_online_60, ")", "*", "1.25")
  #writeFormula(wb, sheet="Scenarios", x=paste0("=", total_geral_mild_dev_online_60_mais_25), startCol=2, startRow=19)
  #
  #writeData(wb, sheet = "Scenarios", x="total_geral_mild_dev_online_40_menos_25", startCol=1, startRow=20)
  #total_geral_mild_dev_online_40_menos_25 <- paste0("(", total_geral_mild_dev_online_40, ")", "*", "0.75")
  #writeFormula(wb, sheet="Scenarios", x=paste0("=", total_geral_mild_dev_online_40_menos_25), startCol=2, startRow=20)
  #writeData(wb, sheet = "Scenarios", x="total_geral_mild_dev_online_40_mais_25", startCol=1, startRow=21)
  #total_geral_mild_dev_online_40_mais_25 <- paste0("(", total_geral_mild_dev_online_40, ")", "*", "1.25")
  #writeFormula(wb, sheet="Scenarios", x=paste0("=", total_geral_mild_dev_online_40_mais_25), startCol=2, startRow=21)
  #
  #writeData(wb, sheet = "Scenarios", x="total_geral_mild_dev_inperson_40_menos_25", startCol=1, startRow=22)
  #total_geral_mild_dev_inperson_40_menos_25 <- paste0("(", total_geral_mild_dev_inperson_40, ")", "*", "0.75")
  #writeFormula(wb, sheet="Scenarios", x=paste0("=", total_geral_mild_dev_inperson_40_menos_25), startCol=2, startRow=22)
  #writeData(wb, sheet = "Scenarios", x="total_geral_mild_dev_inperson_40_mais_25", startCol=1, startRow=23)
  #total_geral_mild_dev_inperson_40_mais_25 <- paste0("(", total_geral_mild_dev_inperson_40, ")", "*", "1.25")
  #writeFormula(wb, sheet="Scenarios", x=paste0("=", total_geral_mild_dev_inperson_40_mais_25), startCol=2, startRow=23)
  #
  #writeData(wb, sheet = "Scenarios", x="total_geral_comp_dev_online_60_menos_25", startCol=1, startRow=24)
  #total_geral_comp_dev_online_60_menos_25 <- paste0("(", total_geral_comp_dev_online_60, ")", "*", "0.75")
  #writeFormula(wb, sheet="Scenarios", x=paste0("=", total_geral_comp_dev_online_60_menos_25), startCol=2, startRow=24)
  #writeData(wb, sheet = "Scenarios", x="total_geral_comp_dev_online_60_mais_25", startCol=1, startRow=25)
  #total_geral_comp_dev_online_60_mais_25 <- paste0("(", total_geral_comp_dev_online_60, ")", "*", "1.25")
  #writeFormula(wb, sheet="Scenarios", x=paste0("=", total_geral_comp_dev_online_60_mais_25), startCol=2, startRow=25)
  #
  #writeData(wb, sheet = "Scenarios", x="total_geral_comp_dev_online_40_menos_25", startCol=1, startRow=26)
  #total_geral_comp_dev_online_40_menos_25 <- paste0("(", total_geral_comp_dev_online_40, ")", "*", "0.75")
  #writeFormula(wb, sheet="Scenarios", x=paste0("=", total_geral_comp_dev_online_40_menos_25), startCol=2, startRow=26)
  #writeData(wb, sheet = "Scenarios", x="total_geral_comp_dev_online_40_mais_25", startCol=1, startRow=27)
  #total_geral_comp_dev_online_40_mais_25 <- paste0("(", total_geral_comp_dev_online_40, ")", "*", "1.25")
  #writeFormula(wb, sheet="Scenarios", x=paste0("=", total_geral_comp_dev_online_40_mais_25), startCol=2, startRow=27)
  #
  #writeData(wb, sheet = "Scenarios", x="total_geral_comp_dev_inperson_40_menos_25", startCol=1, startRow=28)
  #total_geral_comp_dev_inperson_40_menos_25 <- paste0("(", total_geral_comp_dev_inperson_40, ")", "*", "0.75")
  #writeFormula(wb, sheet="Scenarios", x=paste0("=", total_geral_comp_dev_inperson_40_menos_25), startCol=2, startRow=28)
  #writeData(wb, sheet = "Scenarios", x="total_geral_comp_dev_inperson_40_mais_25", startCol=1, startRow=29)
  #total_geral_comp_dev_inperson_40_mais_25 <- paste0("(", total_geral_comp_dev_inperson_40, ")", "*", "1.25")
  #writeFormula(wb, sheet="Scenarios", x=paste0("=", total_geral_comp_dev_inperson_40_mais_25), startCol=2, startRow=29)
  
  #---- Scenarios: Cenário 1 -----
  
  #writeData(wb, sheet = "Scenarios", x="sce1_sec_total_geral_min_dev_online_60", startCol=1, startRow=30)
  #sce1_sec_total_geral_min_dev_online_60 <- paste0(sce1_sec_dim_1_total, "+", sce1_sec_dim_2_total_min, "+", sce1_sec_dim_3_total_online_60, "+", sce1_sec_dim_4_total, "+", sce1_sec_dim_5_total)
  #writeFormula(wb, sheet="Scenarios", x=paste0("=", sce1_sec_total_geral_min_dev_online_60), startCol=2, startRow=30)
  #
  #writeData(wb, sheet = "Scenarios", x="sce1_sec_total_geral_min_dev_online_40", startCol=1, startRow=31)
  #sce1_sec_total_geral_min_dev_online_40 <- paste0(sce1_sec_dim_1_total, "+", sce1_sec_dim_2_total_min, "+", sce1_sec_dim_3_total_online_40, "+", sce1_sec_dim_4_total, "+", sce1_sec_dim_5_total)
  #writeFormula(wb, sheet="Scenarios", x=paste0("=", sce1_sec_total_geral_min_dev_online_40), startCol=2, startRow=31)
  #
  #writeData(wb, sheet = "Scenarios", x="sce1_sec_total_geral_min_dev_inperson_40", startCol=1, startRow=32)
  #sce1_sec_total_geral_min_dev_inperson_40 <- paste0(sce1_sec_dim_1_total, "+", sce1_sec_dim_2_total_min, "+", sce1_sec_dim_3_total_inperson_40, "+", sce1_sec_dim_4_total, "+", sce1_sec_dim_5_total)
  #writeFormula(wb, sheet="Scenarios", x=paste0("=", sce1_sec_total_geral_min_dev_inperson_40), startCol=2, startRow=32)
  #
  #writeData(wb, sheet = "Scenarios", x="sce1_sec_total_geral_mild_dev_online_60", startCol=1, startRow=33)
  #sce1_sec_total_geral_mild_dev_online_60 <- paste0(sce1_sec_dim_1_total, "+", sce1_sec_dim_2_total_mild, "+", sce1_sec_dim_3_total_online_60, "+", sce1_sec_dim_4_total, "+", sce1_sec_dim_5_total)
  #writeFormula(wb, sheet="Scenarios", x=paste0("=", sce1_sec_total_geral_mild_dev_online_60), startCol=2, startRow=33)
  #
  #writeData(wb, sheet = "Scenarios", x="sce1_sec_total_geral_mild_dev_online_40", startCol=1, startRow=34)
  #sce1_sec_total_geral_mild_dev_online_40 <- paste0(sce1_sec_dim_1_total, "+", sce1_sec_dim_2_total_mild, "+", sce1_sec_dim_3_total_online_40, "+", sce1_sec_dim_4_total, "+", sce1_sec_dim_5_total)
  #writeFormula(wb, sheet="Scenarios", x=paste0("=", sce1_sec_total_geral_mild_dev_online_40), startCol=2, startRow=34)
  #
  #writeData(wb, sheet = "Scenarios", x="sce1_sec_total_geral_mild_dev_inperson_40", startCol=1, startRow=35)
  #sce1_sec_total_geral_mild_dev_inperson_40 <- paste0(sce1_sec_dim_1_total, "+", sce1_sec_dim_2_total_mild, "+", sce1_sec_dim_3_total_inperson_40, "+", sce1_sec_dim_4_total, "+", sce1_sec_dim_5_total)
  #writeFormula(wb, sheet="Scenarios", x=paste0("=", sce1_sec_total_geral_mild_dev_inperson_40), startCol=2, startRow=35)
  #
  #writeData(wb, sheet = "Scenarios", x="sce1_sec_total_geral_comp_dev_online_60", startCol=1, startRow=36)
  #sce1_sec_total_geral_comp_dev_online_60 <- paste0(sce1_sec_dim_1_total, "+", sce1_sec_dim_2_total_comprehensive, "+", sce1_sec_dim_3_total_online_60, "+", sce1_sec_dim_4_total, "+", sce1_sec_dim_5_total)
  #writeFormula(wb, sheet="Scenarios", x=paste0("=", sce1_sec_total_geral_comp_dev_online_60), startCol=2, startRow=36)
  #
  #writeData(wb, sheet = "Scenarios", x="sce1_sec_total_geral_comp_dev_online_40", startCol=1, startRow=37)
  #sce1_sec_total_geral_comp_dev_online_40 <- paste0(sce1_sec_dim_1_total, "+", sce1_sec_dim_2_total_comprehensive, "+", sce1_sec_dim_3_total_online_40, "+", sce1_sec_dim_4_total, "+", sce1_sec_dim_5_total)
  #writeFormula(wb, sheet="Scenarios", x=paste0("=", sce1_sec_total_geral_comp_dev_online_40), startCol=2, startRow=37)
  #
  #writeData(wb, sheet = "Scenarios", x="sce1_sec_total_geral_comp_dev_inperson_40", startCol=1, startRow=38)
  #sce1_sec_total_geral_comp_dev_inperson_40 <- paste0(sce1_sec_dim_1_total, "+", sce1_sec_dim_2_total_comprehensive, "+", sce1_sec_dim_3_total_inperson_40, "+", sce1_sec_dim_4_total, "+", sce1_sec_dim_5_total)
  #writeFormula(wb, sheet="Scenarios", x=paste0("=", sce1_sec_total_geral_comp_dev_inperson_40), startCol=2, startRow=38)
  #
  #writeData(wb, sheet = "Scenarios", x="sce1_sec_total_por_aluno_min_dev_online_60", startCol=1, startRow=39)
  #sce1_sec_total_por_aluno_min_dev_online_60 <- paste0("(", sce1_sec_total_geral_min_dev_online_60, ")", "/number_of_students_secondary")
  #sce1_sec_total_por_aluno_min_dev_online_60 <- str_replace_all(sce1_sec_total_por_aluno_min_dev_online_60, map_df)
  #writeFormula(wb, sheet="Scenarios", x=paste0("=", sce1_sec_total_por_aluno_min_dev_online_60), startCol=2, startRow=39)
  #
  #writeData(wb, sheet = "Scenarios", x="sce1_sec_total_por_aluno_min_dev_online_40", startCol=1, startRow=40)
  #sce1_sec_total_por_aluno_min_dev_online_40 <- paste0("(", sce1_sec_total_geral_min_dev_online_40, ")", "/number_of_students_secondary")
  #sce1_sec_total_por_aluno_min_dev_online_40 <- str_replace_all(sce1_sec_total_por_aluno_min_dev_online_40, map_df)
  #writeFormula(wb, sheet="Scenarios", x=paste0("=", sce1_sec_total_por_aluno_min_dev_online_40), startCol=2, startRow=40)
  #
  #writeData(wb, sheet = "Scenarios", x="sce1_sec_total_por_aluno_min_dev_inperson_40", startCol=1, startRow=41)
  #sce1_sec_total_por_aluno_min_dev_inperson_40 <- paste0("(", sce1_sec_total_geral_min_dev_inperson_40, ")", "/number_of_students_secondary")
  #sce1_sec_total_por_aluno_min_dev_inperson_40 <- str_replace_all(sce1_sec_total_por_aluno_min_dev_inperson_40, map_df)
  #writeFormula(wb, sheet="Scenarios", x=paste0("=", sce1_sec_total_por_aluno_min_dev_inperson_40), startCol=2, startRow=41)
  #
  #writeData(wb, sheet = "Scenarios", x="sce1_sec_total_por_aluno_mild_dev_online_60", startCol=1, startRow=42)
  #sce1_sec_total_por_aluno_mild_dev_online_60 <- paste0("(", sce1_sec_total_geral_mild_dev_online_60, ")", "/number_of_students_secondary")
  #sce1_sec_total_por_aluno_mild_dev_online_60 <- str_replace_all(sce1_sec_total_por_aluno_mild_dev_online_60, map_df)
  #writeFormula(wb, sheet="Scenarios", x=paste0("=", sce1_sec_total_por_aluno_mild_dev_online_60), startCol=2, startRow=42)
  #
  #writeData(wb, sheet = "Scenarios", x="sce1_sec_total_por_aluno_mild_dev_online_40", startCol=1, startRow=43)
  #sce1_sec_total_por_aluno_mild_dev_online_40 <- paste0("(", sce1_sec_total_geral_mild_dev_online_40, ")", "/number_of_students_secondary")
  #sce1_sec_total_por_aluno_mild_dev_online_40 <- str_replace_all(sce1_sec_total_por_aluno_mild_dev_online_40, map_df)
  #writeFormula(wb, sheet="Scenarios", x=paste0("=", sce1_sec_total_por_aluno_mild_dev_online_40), startCol=2, startRow=43)
  #
  #writeData(wb, sheet = "Scenarios", x="sce1_sec_total_por_aluno_mild_dev_inperson_40", startCol=1, startRow=44)
  #sce1_sec_total_por_aluno_mild_dev_inperson_40 <- paste0("(", sce1_sec_total_geral_mild_dev_inperson_40, ")", "/number_of_students_secondary")
  #sce1_sec_total_por_aluno_mild_dev_inperson_40 <- str_replace_all(sce1_sec_total_por_aluno_mild_dev_inperson_40, map_df)
  #writeFormula(wb, sheet="Scenarios", x=paste0("=", sce1_sec_total_por_aluno_mild_dev_inperson_40), startCol=2, startRow=44)
  #
  #writeData(wb, sheet = "Scenarios", x="sce1_sec_total_por_aluno_comp_dev_online_60", startCol=1, startRow=45)
  #sce1_sec_total_por_aluno_comp_dev_online_60 <- paste0("(", sce1_sec_total_geral_comp_dev_online_60, ")", "/number_of_students_secondary")
  #sce1_sec_total_por_aluno_comp_dev_online_60 <- str_replace_all(sce1_sec_total_por_aluno_comp_dev_online_60, map_df)
  #writeFormula(wb, sheet="Scenarios", x=paste0("=", sce1_sec_total_por_aluno_comp_dev_online_60), startCol=2, startRow=45)
  #
  #writeData(wb, sheet = "Scenarios", x="sce1_sec_total_por_aluno_comp_dev_online_40", startCol=1, startRow=46)
  #sce1_sec_total_por_aluno_comp_dev_online_40 <- paste0("(", sce1_sec_total_geral_comp_dev_online_40, ")", "/number_of_students_secondary")
  #sce1_sec_total_por_aluno_comp_dev_online_40 <- str_replace_all(sce1_sec_total_por_aluno_comp_dev_online_40, map_df)
  #writeFormula(wb, sheet="Scenarios", x=paste0("=", sce1_sec_total_por_aluno_comp_dev_online_40), startCol=2, startRow=46)
  #
  #writeData(wb, sheet = "Scenarios", x="sce1_sec_total_por_aluno_comp_dev_inperson_40", startCol=1, startRow=47)
  #sce1_sec_total_por_aluno_comp_dev_inperson_40 <- paste0("(", sce1_sec_total_geral_comp_dev_inperson_40, ")", "/number_of_students_secondary")
  #sce1_sec_total_por_aluno_comp_dev_inperson_40 <- str_replace_all(sce1_sec_total_por_aluno_comp_dev_inperson_40, map_df)
  #writeFormula(wb, sheet="Scenarios", x=paste0("=", sce1_sec_total_por_aluno_comp_dev_inperson_40), startCol=2, startRow=47)
  #
  #writeData(wb, sheet = "Scenarios", x="sce1_sec_total_por_escola_min_dev_online_60", startCol=1, startRow=48)
  #sce1_sec_total_por_escola_min_dev_online_60 <- paste0("(", sce1_sec_total_geral_min_dev_online_60, ")", "/secondary_schools_cima")
  #sce1_sec_total_por_escola_min_dev_online_60 <- str_replace_all(sce1_sec_total_por_escola_min_dev_online_60, map_df)
  #writeFormula(wb, sheet="Scenarios", x=paste0("=", sce1_sec_total_por_escola_min_dev_online_60), startCol=2, startRow=48)
  #
  #writeData(wb, sheet = "Scenarios", x="sce1_sec_total_por_escola_min_dev_online_40", startCol=1, startRow=49)
  #sce1_sec_total_por_escola_min_dev_online_40 <- paste0("(", sce1_sec_total_geral_min_dev_online_40, ")", "/secondary_schools_cima")
  #sce1_sec_total_por_escola_min_dev_online_40 <- str_replace_all(sce1_sec_total_por_escola_min_dev_online_40, map_df)
  #writeFormula(wb, sheet="Scenarios", x=paste0("=", sce1_sec_total_por_escola_min_dev_online_40), startCol=2, startRow=49)
  #
  #writeData(wb, sheet = "Scenarios", x="sce1_sec_total_por_escola_min_dev_inperson_40", startCol=1, startRow=50)
  #sce1_sec_total_por_escola_min_dev_inperson_40 <- paste0("(", sce1_sec_total_geral_min_dev_inperson_40, ")", "/secondary_schools_cima")
  #sce1_sec_total_por_escola_min_dev_inperson_40 <- str_replace_all(sce1_sec_total_por_escola_min_dev_inperson_40, map_df)
  #writeFormula(wb, sheet="Scenarios", x=paste0("=", sce1_sec_total_por_escola_min_dev_inperson_40), startCol=2, startRow=50)
  #
  #writeData(wb, sheet = "Scenarios", x="sce1_sec_total_por_escola_mild_dev_online_60", startCol=1, startRow=51)
  #sce1_sec_total_por_escola_mild_dev_online_60 <- paste0("(", sce1_sec_total_geral_mild_dev_online_60, ")", "/secondary_schools_cima")
  #sce1_sec_total_por_escola_mild_dev_online_60 <- str_replace_all(sce1_sec_total_por_escola_mild_dev_online_60, map_df)
  #writeFormula(wb, sheet="Scenarios", x=paste0("=", sce1_sec_total_por_escola_mild_dev_online_60), startCol=2, startRow=51)
  #
  #writeData(wb, sheet = "Scenarios", x="sce1_sec_total_por_escola_mild_dev_online_40", startCol=1, startRow=52)
  #sce1_sec_total_por_escola_mild_dev_online_40 <- paste0("(", sce1_sec_total_geral_mild_dev_online_40, ")", "/secondary_schools_cima")
  #sce1_sec_total_por_escola_mild_dev_online_40 <- str_replace_all(sce1_sec_total_por_escola_mild_dev_online_40, map_df)
  #writeFormula(wb, sheet="Scenarios", x=paste0("=", sce1_sec_total_por_escola_mild_dev_online_40), startCol=2, startRow=52)
  #
  #writeData(wb, sheet = "Scenarios", x="sce1_sec_total_por_escola_mild_dev_inperson_40", startCol=1, startRow=53)
  #sce1_sec_total_por_escola_mild_dev_inperson_40 <- paste0("(", sce1_sec_total_geral_mild_dev_inperson_40, ")", "/secondary_schools_cima")
  #sce1_sec_total_por_escola_mild_dev_inperson_40 <- str_replace_all(sce1_sec_total_por_escola_mild_dev_inperson_40, map_df)
  #writeFormula(wb, sheet="Scenarios", x=paste0("=", sce1_sec_total_por_escola_mild_dev_inperson_40), startCol=2, startRow=53)
  #
  #writeData(wb, sheet = "Scenarios", x="sce1_sec_total_por_escola_comp_dev_online_60", startCol=1, startRow=54)
  #sce1_sec_total_por_escola_comp_dev_online_60 <- paste0("(", sce1_sec_total_geral_comp_dev_online_60, ")", "/secondary_schools_cima")
  #sce1_sec_total_por_escola_comp_dev_online_60 <- str_replace_all(sce1_sec_total_por_escola_comp_dev_online_60, map_df)
  #writeFormula(wb, sheet="Scenarios", x=paste0("=", sce1_sec_total_por_escola_comp_dev_online_60), startCol=2, startRow=54)
  #
  #writeData(wb, sheet = "Scenarios", x="sce1_sec_total_por_escola_comp_dev_online_40", startCol=1, startRow=55)
  #sce1_sec_total_por_escola_comp_dev_online_40 <- paste0("(", sce1_sec_total_geral_comp_dev_online_40, ")", "/secondary_schools_cima")
  #sce1_sec_total_por_escola_comp_dev_online_40 <- str_replace_all(sce1_sec_total_por_escola_comp_dev_online_40, map_df)
  #writeFormula(wb, sheet="Scenarios", x=paste0("=", sce1_sec_total_por_escola_comp_dev_online_40), startCol=2, startRow=55)
  #
  #writeData(wb, sheet = "Scenarios", x="sce1_sec_total_por_escola_comp_dev_inperson_40", startCol=1, startRow=56)
  #sce1_sec_total_por_escola_comp_dev_inperson_40 <- paste0("(", sce1_sec_total_geral_comp_dev_inperson_40, ")", "/secondary_schools_cima")
  #sce1_sec_total_por_escola_comp_dev_inperson_40 <- str_replace_all(sce1_sec_total_por_escola_comp_dev_inperson_40, map_df)
  #writeFormula(wb, sheet="Scenarios", x=paste0("=", sce1_sec_total_por_escola_comp_dev_inperson_40), startCol=2, startRow=56)
  #
  #writeData(wb, sheet = "Scenarios", x="sce1_sec_total_geral_min_dev_online_60_menos_25", startCol=1, startRow=57)
  #sce1_sec_total_geral_min_dev_online_60_menos_25 <- paste0("(", sce1_sec_total_geral_min_dev_online_60, ")", "*", "0.75")
  #writeFormula(wb, sheet="Scenarios", x=paste0("=", sce1_sec_total_geral_min_dev_online_60_menos_25), startCol=2, startRow=57)
  #writeData(wb, sheet = "Scenarios", x="sce1_sec_total_geral_min_dev_online_60_mais_25", startCol=1, startRow=58)
  #sce1_sec_total_geral_min_dev_online_60_mais_25 <- paste0("(", sce1_sec_total_geral_min_dev_online_60, ")", "*", "1.25")
  #writeFormula(wb, sheet="Scenarios", x=paste0("=", sce1_sec_total_geral_min_dev_online_60_mais_25), startCol=2, startRow=58)
  #
  #writeData(wb, sheet = "Scenarios", x="sce1_sec_total_geral_min_dev_online_40_menos_25", startCol=1, startRow=59)
  #sce1_sec_total_geral_min_dev_online_40_menos_25 <- paste0("(", sce1_sec_total_geral_min_dev_online_40, ")", "*", "0.75")
  #writeFormula(wb, sheet="Scenarios", x=paste0("=", sce1_sec_total_geral_min_dev_online_40_menos_25), startCol=2, startRow=59)
  #writeData(wb, sheet = "Scenarios", x="sce1_sec_total_geral_min_dev_online_40_mais_25", startCol=1, startRow=60)
  #sce1_sec_total_geral_min_dev_online_40_mais_25 <- paste0("(", sce1_sec_total_geral_min_dev_online_40, ")", "*", "1.25")
  #writeFormula(wb, sheet="Scenarios", x=paste0("=", sce1_sec_total_geral_min_dev_online_40_mais_25), startCol=2, startRow=60)
  #
  #writeData(wb, sheet = "Scenarios", x="sce1_sec_total_geral_min_dev_inperson_40_menos_25", startCol=1, startRow=61)
  #sce1_sec_total_geral_min_dev_inperson_40_menos_25 <- paste0("(", sce1_sec_total_geral_min_dev_inperson_40, ")", "*", "0.75")
  #writeFormula(wb, sheet="Scenarios", x=paste0("=", sce1_sec_total_geral_min_dev_inperson_40_menos_25), startCol=2, startRow=61)
  #writeData(wb, sheet = "Scenarios", x="sce1_sec_total_geral_min_dev_inperson_40_mais_25", startCol=1, startRow=62)
  #sce1_sec_total_geral_min_dev_inperson_40_mais_25 <- paste0("(", sce1_sec_total_geral_min_dev_inperson_40, ")", "*", "1.25")
  #writeFormula(wb, sheet="Scenarios", x=paste0("=", sce1_sec_total_geral_min_dev_inperson_40_mais_25), startCol=2, startRow=62)
  #
  #writeData(wb, sheet = "Scenarios", x="sce1_sec_total_geral_mild_dev_online_60_menos_25", startCol=1, startRow=63)
  #sce1_sec_total_geral_mild_dev_online_60_menos_25 <- paste0("(", sce1_sec_total_geral_mild_dev_online_60, ")", "*", "0.75")
  #writeFormula(wb, sheet="Scenarios", x=paste0("=", sce1_sec_total_geral_mild_dev_online_60_menos_25), startCol=2, startRow=63)
  #writeData(wb, sheet = "Scenarios", x="sce1_sec_total_geral_mild_dev_online_60_mais_25", startCol=1, startRow=64)
  #sce1_sec_total_geral_mild_dev_online_60_mais_25 <- paste0("(", sce1_sec_total_geral_mild_dev_online_60, ")", "*", "1.25")
  #writeFormula(wb, sheet="Scenarios", x=paste0("=", sce1_sec_total_geral_mild_dev_online_60_mais_25), startCol=2, startRow=64)
  #
  #writeData(wb, sheet = "Scenarios", x="sce1_sec_total_geral_mild_dev_online_40_menos_25", startCol=1, startRow=65)
  #sce1_sec_total_geral_mild_dev_online_40_menos_25 <- paste0("(", sce1_sec_total_geral_mild_dev_online_40, ")", "*", "0.75")
  #writeFormula(wb, sheet="Scenarios", x=paste0("=", sce1_sec_total_geral_mild_dev_online_40_menos_25), startCol=2, startRow=65)
  #writeData(wb, sheet = "Scenarios", x="sce1_sec_total_geral_mild_dev_online_40_mais_25", startCol=1, startRow=66)
  #sce1_sec_total_geral_mild_dev_online_40_mais_25 <- paste0("(", sce1_sec_total_geral_mild_dev_online_40, ")", "*", "1.25")
  #writeFormula(wb, sheet="Scenarios", x=paste0("=", sce1_sec_total_geral_mild_dev_online_40_mais_25), startCol=2, startRow=66)
  #
  #writeData(wb, sheet = "Scenarios", x="sce1_sec_total_geral_mild_dev_inperson_40_menos_25", startCol=1, startRow=67)
  #sce1_sec_total_geral_mild_dev_inperson_40_menos_25 <- paste0("(", sce1_sec_total_geral_mild_dev_inperson_40, ")", "*", "0.75")
  #writeFormula(wb, sheet="Scenarios", x=paste0("=", sce1_sec_total_geral_mild_dev_inperson_40_menos_25), startCol=2, startRow=67)
  #writeData(wb, sheet = "Scenarios", x="sce1_sec_total_geral_mild_dev_inperson_40_mais_25", startCol=1, startRow=68)
  #sce1_sec_total_geral_mild_dev_inperson_40_mais_25 <- paste0("(", sce1_sec_total_geral_mild_dev_inperson_40, ")", "*", "1.25")
  #writeFormula(wb, sheet="Scenarios", x=paste0("=", sce1_sec_total_geral_mild_dev_inperson_40_mais_25), startCol=2, startRow=68)
  #
  #writeData(wb, sheet = "Scenarios", x="sce1_sec_total_geral_comp_dev_online_60_menos_25", startCol=1, startRow=69)
  #sce1_sec_total_geral_comp_dev_online_60_menos_25 <- paste0("(", sce1_sec_total_geral_comp_dev_online_60, ")", "*", "0.75")
  #writeFormula(wb, sheet="Scenarios", x=paste0("=", sce1_sec_total_geral_comp_dev_online_60_menos_25), startCol=2, startRow=69)
  #writeData(wb, sheet = "Scenarios", x="sce1_sec_total_geral_comp_dev_online_60_mais_25", startCol=1, startRow=70)
  #sce1_sec_total_geral_comp_dev_online_60_mais_25 <- paste0("(", sce1_sec_total_geral_comp_dev_online_60, ")", "*", "1.25")
  #writeFormula(wb, sheet="Scenarios", x=paste0("=", sce1_sec_total_geral_comp_dev_online_60_mais_25), startCol=2, startRow=70)
  #
  #writeData(wb, sheet = "Scenarios", x="sce1_sec_total_geral_comp_dev_online_40_menos_25", startCol=1, startRow=71)
  #sce1_sec_total_geral_comp_dev_online_40_menos_25 <- paste0("(", sce1_sec_total_geral_comp_dev_online_40, ")", "*", "0.75")
  #writeFormula(wb, sheet="Scenarios", x=paste0("=", sce1_sec_total_geral_comp_dev_online_40_menos_25), startCol=2, startRow=71)
  #writeData(wb, sheet = "Scenarios", x="sce1_sec_total_geral_comp_dev_online_40_mais_25", startCol=1, startRow=72)
  #sce1_sec_total_geral_comp_dev_online_40_mais_25 <- paste0("(", sce1_sec_total_geral_comp_dev_online_40, ")", "*", "1.25")
  #writeFormula(wb, sheet="Scenarios", x=paste0("=", sce1_sec_total_geral_comp_dev_online_40_mais_25), startCol=2, startRow=72)
  #
  #writeData(wb, sheet = "Scenarios", x="sce1_sec_total_geral_comp_dev_inperson_40_menos_25", startCol=1, startRow=73)
  #sce1_sec_total_geral_comp_dev_inperson_40_menos_25 <- paste0("(", sce1_sec_total_geral_comp_dev_inperson_40, ")", "*", "0.75")
  #writeFormula(wb, sheet="Scenarios", x=paste0("=", sce1_sec_total_geral_comp_dev_inperson_40_menos_25), startCol=2, startRow=73)
  #writeData(wb, sheet = "Scenarios", x="sce1_sec_total_geral_comp_dev_inperson_40_mais_25", startCol=1, startRow=74)
  #sce1_sec_total_geral_comp_dev_inperson_40_mais_25 <- paste0("(", sce1_sec_total_geral_comp_dev_inperson_40, ")", "*", "1.25")
  #writeFormula(wb, sheet="Scenarios", x=paste0("=", sce1_sec_total_geral_comp_dev_inperson_40_mais_25), startCol=2, startRow=74)
  
  
  #---- Scenarios: Cenário 2 -----
  
  #writeData(wb, sheet = "Scenarios", x="sce2_rur_total_geral_min_dev_online_60", startCol=1, startRow=75)
  #sce2_rur_total_geral_min_dev_online_60 <- paste0(sce2_rur_dim_1_total, "+", sce2_rur_dim_2_total_min, "+", sce2_rur_dim_3_total_online_60, "+", sce2_rur_dim_4_total, "+", sce2_rur_dim_5_total)
  #writeFormula(wb, sheet="Scenarios", x=paste0("=", sce2_rur_total_geral_min_dev_online_60), startCol=2, startRow=75)
  #
  #writeData(wb, sheet = "Scenarios", x="sce2_rur_total_geral_min_dev_online_40", startCol=1, startRow=76)
  #sce2_rur_total_geral_min_dev_online_40 <- paste0(sce2_rur_dim_1_total, "+", sce2_rur_dim_2_total_min, "+", sce2_rur_dim_3_total_online_40, "+", sce2_rur_dim_4_total, "+", sce2_rur_dim_5_total)
  #writeFormula(wb, sheet="Scenarios", x=paste0("=", sce2_rur_total_geral_min_dev_online_40), startCol=2, startRow=76)
  #
  #writeData(wb, sheet = "Scenarios", x="sce2_rur_total_geral_min_dev_inperson_40", startCol=1, startRow=77)
  #sce2_rur_total_geral_min_dev_inperson_40 <- paste0(sce2_rur_dim_1_total, "+", sce2_rur_dim_2_total_min, "+", sce2_rur_dim_3_total_inperson_40, "+", sce2_rur_dim_4_total, "+", sce2_rur_dim_5_total)
  #writeFormula(wb, sheet="Scenarios", x=paste0("=", sce2_rur_total_geral_min_dev_inperson_40), startCol=2, startRow=77)
  #
  #writeData(wb, sheet = "Scenarios", x="sce2_rur_total_geral_mild_dev_online_60", startCol=1, startRow=78)
  #sce2_rur_total_geral_mild_dev_online_60 <- paste0(sce2_rur_dim_1_total, "+", sce2_rur_dim_2_total_mild, "+", sce2_rur_dim_3_total_online_60, "+", sce2_rur_dim_4_total, "+", sce2_rur_dim_5_total)
  #writeFormula(wb, sheet="Scenarios", x=paste0("=", sce2_rur_total_geral_mild_dev_online_60), startCol=2, startRow=78)
  #
  #writeData(wb, sheet = "Scenarios", x="sce2_rur_total_geral_mild_dev_online_40", startCol=1, startRow=79)
  #sce2_rur_total_geral_mild_dev_online_40 <- paste0(sce2_rur_dim_1_total, "+", sce2_rur_dim_2_total_mild, "+", sce2_rur_dim_3_total_online_40, "+", sce2_rur_dim_4_total, "+", sce2_rur_dim_5_total)
  #writeFormula(wb, sheet="Scenarios", x=paste0("=", sce2_rur_total_geral_mild_dev_online_40), startCol=2, startRow=79)
  #
  #writeData(wb, sheet = "Scenarios", x="sce2_rur_total_geral_mild_dev_inperson_40", startCol=1, startRow=80)
  #sce2_rur_total_geral_mild_dev_inperson_40 <- paste0(sce2_rur_dim_1_total, "+", sce2_rur_dim_2_total_mild, "+", sce2_rur_dim_3_total_inperson_40, "+", sce2_rur_dim_4_total, "+", sce2_rur_dim_5_total)
  #writeFormula(wb, sheet="Scenarios", x=paste0("=", sce2_rur_total_geral_mild_dev_inperson_40), startCol=2, startRow=80)
  #
  #writeData(wb, sheet = "Scenarios", x="sce2_rur_total_geral_comp_dev_online_60", startCol=1, startRow=81)
  #sce2_rur_total_geral_comp_dev_online_60 <- paste0(sce2_rur_dim_1_total, "+", sce2_rur_dim_2_total_comprehensive, "+", sce2_rur_dim_3_total_online_60, "+", sce2_rur_dim_4_total, "+", sce2_rur_dim_5_total)
  #writeFormula(wb, sheet="Scenarios", x=paste0("=", sce2_rur_total_geral_comp_dev_online_60), startCol=2, startRow=81)
  #
  #writeData(wb, sheet = "Scenarios", x="sce2_rur_total_geral_comp_dev_online_40", startCol=1, startRow=82)
  #sce2_rur_total_geral_comp_dev_online_40 <- paste0(sce2_rur_dim_1_total, "+", sce2_rur_dim_2_total_comprehensive, "+", sce2_rur_dim_3_total_online_40, "+", sce2_rur_dim_4_total, "+", sce2_rur_dim_5_total)
  #writeFormula(wb, sheet="Scenarios", x=paste0("=", sce2_rur_total_geral_comp_dev_online_40), startCol=2, startRow=82)
  #
  #writeData(wb, sheet = "Scenarios", x="sce2_rur_total_geral_comp_dev_inperson_40", startCol=1, startRow=83)
  #sce2_rur_total_geral_comp_dev_inperson_40 <- paste0(sce2_rur_dim_1_total, "+", sce2_rur_dim_2_total_comprehensive, "+", sce2_rur_dim_3_total_inperson_40, "+", sce2_rur_dim_4_total, "+", sce2_rur_dim_5_total)
  #writeFormula(wb, sheet="Scenarios", x=paste0("=", sce2_rur_total_geral_comp_dev_inperson_40), startCol=2, startRow=83)
  #  
  #writeData(wb, sheet = "Scenarios", x="sce2_rur_total_por_aluno_min_dev_online_60", startCol=1, startRow=84)
  #sce2_rur_total_por_aluno_min_dev_online_60 <- paste0("(", sce2_rur_total_geral_min_dev_online_60, ")", "/number_of_students")
  #sce2_rur_total_por_aluno_min_dev_online_60 <- str_replace_all(sce2_rur_total_por_aluno_min_dev_online_60, map_df)
  #writeFormula(wb, sheet="Scenarios", x=paste0("=", sce2_rur_total_por_aluno_min_dev_online_60), startCol=2, startRow=84)
  #
  #writeData(wb, sheet = "Scenarios", x="sce2_rur_total_por_aluno_min_dev_online_40", startCol=1, startRow=85)
  #sce2_rur_total_por_aluno_min_dev_online_40 <- paste0("(", sce2_rur_total_geral_min_dev_online_40, ")", "/number_of_students")
  #sce2_rur_total_por_aluno_min_dev_online_40 <- str_replace_all(sce2_rur_total_por_aluno_min_dev_online_40, map_df)
  #writeFormula(wb, sheet="Scenarios", x=paste0("=", sce2_rur_total_por_aluno_min_dev_online_40), startCol=2, startRow=85)
  #
  #writeData(wb, sheet = "Scenarios", x="sce2_rur_total_por_aluno_min_dev_inperson_40", startCol=1, startRow=86)
  #sce2_rur_total_por_aluno_min_dev_inperson_40 <- paste0("(", sce2_rur_total_geral_min_dev_inperson_40, ")", "/number_of_students")
  #sce2_rur_total_por_aluno_min_dev_inperson_40 <- str_replace_all(sce2_rur_total_por_aluno_min_dev_inperson_40, map_df)
  #writeFormula(wb, sheet="Scenarios", x=paste0("=", sce2_rur_total_por_aluno_min_dev_inperson_40), startCol=2, startRow=86)
  #
  #writeData(wb, sheet = "Scenarios", x="sce2_rur_total_por_aluno_mild_dev_online_60", startCol=1, startRow=87)
  #sce2_rur_total_por_aluno_mild_dev_online_60 <- paste0("(", sce2_rur_total_geral_mild_dev_online_60, ")", "/number_of_students")
  #sce2_rur_total_por_aluno_mild_dev_online_60 <- str_replace_all(sce2_rur_total_por_aluno_mild_dev_online_60, map_df)
  #writeFormula(wb, sheet="Scenarios", x=paste0("=", sce2_rur_total_por_aluno_mild_dev_online_60), startCol=2, startRow=87)
  #
  #writeData(wb, sheet = "Scenarios", x="sce2_rur_total_por_aluno_mild_dev_online_40", startCol=1, startRow=88)
  #sce2_rur_total_por_aluno_mild_dev_online_40 <- paste0("(", sce2_rur_total_geral_mild_dev_online_40, ")", "/number_of_students")
  #sce2_rur_total_por_aluno_mild_dev_online_40 <- str_replace_all(sce2_rur_total_por_aluno_mild_dev_online_40, map_df)
  #writeFormula(wb, sheet="Scenarios", x=paste0("=", sce2_rur_total_por_aluno_mild_dev_online_40), startCol=2, startRow=88)
  #
  #writeData(wb, sheet = "Scenarios", x="sce2_rur_total_por_aluno_mild_dev_inperson_40", startCol=1, startRow=89)
  #sce2_rur_total_por_aluno_mild_dev_inperson_40 <- paste0("(", sce2_rur_total_geral_mild_dev_inperson_40, ")", "/number_of_students")
  #sce2_rur_total_por_aluno_mild_dev_inperson_40 <- str_replace_all(sce2_rur_total_por_aluno_mild_dev_inperson_40, map_df)
  #writeFormula(wb, sheet="Scenarios", x=paste0("=", sce2_rur_total_por_aluno_mild_dev_inperson_40), startCol=2, startRow=89)
  #
  #writeData(wb, sheet = "Scenarios", x="sce2_rur_total_por_aluno_comp_dev_online_60", startCol=1, startRow=90)
  #sce2_rur_total_por_aluno_comp_dev_online_60 <- paste0("(", sce2_rur_total_geral_comp_dev_online_60, ")", "/number_of_students")
  #sce2_rur_total_por_aluno_comp_dev_online_60 <- str_replace_all(sce2_rur_total_por_aluno_comp_dev_online_60, map_df)
  #writeFormula(wb, sheet="Scenarios", x=paste0("=", sce2_rur_total_por_aluno_comp_dev_online_60), startCol=2, startRow=90)
  #
  #writeData(wb, sheet = "Scenarios", x="sce2_rur_total_por_aluno_comp_dev_online_40", startCol=1, startRow=91)
  #sce2_rur_total_por_aluno_comp_dev_online_40 <- paste0("(", sce2_rur_total_geral_comp_dev_online_40, ")", "/number_of_students")
  #sce2_rur_total_por_aluno_comp_dev_online_40 <- str_replace_all(sce2_rur_total_por_aluno_comp_dev_online_40, map_df)
  #writeFormula(wb, sheet="Scenarios", x=paste0("=", sce2_rur_total_por_aluno_comp_dev_online_40), startCol=2, startRow=91)
  #
  #writeData(wb, sheet = "Scenarios", x="sce2_rur_total_por_aluno_comp_dev_inperson_40", startCol=1, startRow=92)
  #sce2_rur_total_por_aluno_comp_dev_inperson_40 <- paste0("(", sce2_rur_total_geral_comp_dev_inperson_40, ")", "/number_of_students")
  #sce2_rur_total_por_aluno_comp_dev_inperson_40 <- str_replace_all(sce2_rur_total_por_aluno_comp_dev_inperson_40, map_df)
  #writeFormula(wb, sheet="Scenarios", x=paste0("=", sce2_rur_total_por_aluno_comp_dev_inperson_40), startCol=2, startRow=92)
  #
  #writeData(wb, sheet = "Scenarios", x="sce2_rur_total_por_escola_min_dev_online_60", startCol=1, startRow=93)
  #sce2_rur_total_por_escola_min_dev_online_60 <- paste0("(", sce2_rur_total_geral_min_dev_online_60, ")", "/number_of_rural_schools")
  #sce2_rur_total_por_escola_min_dev_online_60 <- str_replace_all(sce2_rur_total_por_escola_min_dev_online_60, map_df)
  #writeFormula(wb, sheet="Scenarios", x=paste0("=", sce2_rur_total_por_escola_min_dev_online_60), startCol=2, startRow=93)
  #
  #writeData(wb, sheet = "Scenarios", x="sce2_rur_total_por_escola_min_dev_online_40", startCol=1, startRow=94)
  #sce2_rur_total_por_escola_min_dev_online_40 <- paste0("(", sce2_rur_total_geral_min_dev_online_40, ")", "/number_of_rural_schools")
  #sce2_rur_total_por_escola_min_dev_online_40 <- str_replace_all(sce2_rur_total_por_escola_min_dev_online_40, map_df)
  #writeFormula(wb, sheet="Scenarios", x=paste0("=", sce2_rur_total_por_escola_min_dev_online_40), startCol=2, startRow=94)
  #
  #writeData(wb, sheet = "Scenarios", x="sce2_rur_total_por_escola_min_dev_inperson_40", startCol=1, startRow=95)
  #sce2_rur_total_por_escola_min_dev_inperson_40 <- paste0("(", sce2_rur_total_geral_min_dev_inperson_40, ")", "/number_of_rural_schools")
  #sce2_rur_total_por_escola_min_dev_inperson_40 <- str_replace_all(sce2_rur_total_por_escola_min_dev_inperson_40, map_df)
  #writeFormula(wb, sheet="Scenarios", x=paste0("=", sce2_rur_total_por_escola_min_dev_inperson_40), startCol=2, startRow=95)
  #
  #writeData(wb, sheet = "Scenarios", x="sce2_rur_total_por_escola_mild_dev_online_60", startCol=1, startRow=96)
  #sce2_rur_total_por_escola_mild_dev_online_60 <- paste0("(", sce2_rur_total_geral_mild_dev_online_60, ")", "/number_of_rural_schools")
  #sce2_rur_total_por_escola_mild_dev_online_60 <- str_replace_all(sce2_rur_total_por_escola_mild_dev_online_60, map_df)
  #writeFormula(wb, sheet="Scenarios", x=paste0("=", sce2_rur_total_por_escola_mild_dev_online_60), startCol=2, startRow=96)
  #
  #writeData(wb, sheet = "Scenarios", x="sce2_rur_total_por_escola_mild_dev_online_40", startCol=1, startRow=97)
  #sce2_rur_total_por_escola_mild_dev_online_40 <- paste0("(", sce2_rur_total_geral_mild_dev_online_40, ")", "/number_of_rural_schools")
  #sce2_rur_total_por_escola_mild_dev_online_40 <- str_replace_all(sce2_rur_total_por_escola_mild_dev_online_40, map_df)
  #writeFormula(wb, sheet="Scenarios", x=paste0("=", sce2_rur_total_por_escola_mild_dev_online_40), startCol=2, startRow=97)
  #
  #writeData(wb, sheet = "Scenarios", x="sce2_rur_total_por_escola_mild_dev_inperson_40", startCol=1, startRow=98)
  #sce2_rur_total_por_escola_mild_dev_inperson_40 <- paste0("(", sce2_rur_total_geral_mild_dev_inperson_40, ")", "/number_of_rural_schools")
  #sce2_rur_total_por_escola_mild_dev_inperson_40 <- str_replace_all(sce2_rur_total_por_escola_mild_dev_inperson_40, map_df)
  #writeFormula(wb, sheet="Scenarios", x=paste0("=", sce2_rur_total_por_escola_mild_dev_inperson_40), startCol=2, startRow=98)
  #
  #writeData(wb, sheet = "Scenarios", x="sce2_rur_total_por_escola_comp_dev_online_60", startCol=1, startRow=99)
  #sce2_rur_total_por_escola_comp_dev_online_60 <- paste0("(", sce2_rur_total_geral_comp_dev_online_60, ")", "/number_of_rural_schools")
  #sce2_rur_total_por_escola_comp_dev_online_60 <- str_replace_all(sce2_rur_total_por_escola_comp_dev_online_60, map_df)
  #writeFormula(wb, sheet="Scenarios", x=paste0("=", sce2_rur_total_por_escola_comp_dev_online_60), startCol=2, startRow=99)
  #
  #writeData(wb, sheet = "Scenarios", x="sce2_rur_total_por_escola_comp_dev_online_40", startCol=1, startRow=100)
  #sce2_rur_total_por_escola_comp_dev_online_40 <- paste0("(", sce2_rur_total_geral_comp_dev_online_40, ")", "/number_of_rural_schools")
  #sce2_rur_total_por_escola_comp_dev_online_40 <- str_replace_all(sce2_rur_total_por_escola_comp_dev_online_40, map_df)
  #writeFormula(wb, sheet="Scenarios", x=paste0("=", sce2_rur_total_por_escola_comp_dev_online_40), startCol=2, startRow=100)
  #
  #writeData(wb, sheet = "Scenarios", x="sce2_rur_total_por_escola_comp_dev_inperson_40", startCol=1, startRow=101)
  #sce2_rur_total_por_escola_comp_dev_inperson_40 <- paste0("(", sce2_rur_total_geral_comp_dev_inperson_40, ")", "/number_of_rural_schools")
  #sce2_rur_total_por_escola_comp_dev_inperson_40 <- str_replace_all(sce2_rur_total_por_escola_comp_dev_inperson_40, map_df)
  #writeFormula(wb, sheet="Scenarios", x=paste0("=", sce2_rur_total_por_escola_comp_dev_inperson_40), startCol=2, startRow=101)
  #
  #
  #writeData(wb, sheet = "Scenarios", x="sce2_rur_total_geral_min_dev_online_60_menos_25", startCol=1, startRow=102)
  #sce2_rur_total_geral_min_dev_online_60_menos_25 <- paste0("(", sce2_rur_total_geral_min_dev_online_60, ")", "*", "0.75")
  #writeFormula(wb, sheet="Scenarios", x=paste0("=", sce2_rur_total_geral_min_dev_online_60_menos_25), startCol=2, startRow=102)
  #writeData(wb, sheet = "Scenarios", x="sce2_rur_total_geral_min_dev_online_60_mais_25", startCol=1, startRow=103)
  #sce2_rur_total_geral_min_dev_online_60_mais_25 <- paste0("(", sce2_rur_total_geral_min_dev_online_60, ")", "*", "1.25")
  #writeFormula(wb, sheet="Scenarios", x=paste0("=", sce2_rur_total_geral_min_dev_online_60_mais_25), startCol=2, startRow=103)
  #
  #writeData(wb, sheet = "Scenarios", x="sce2_rur_total_geral_min_dev_online_40_menos_25", startCol=1, startRow=104)
  #sce2_rur_total_geral_min_dev_online_40_menos_25 <- paste0("(", sce2_rur_total_geral_min_dev_online_40, ")", "*", "0.75")
  #writeFormula(wb, sheet="Scenarios", x=paste0("=", sce2_rur_total_geral_min_dev_online_40_menos_25), startCol=2, startRow=104)
  #writeData(wb, sheet = "Scenarios", x="sce2_rur_total_geral_min_dev_online_40_mais_25", startCol=1, startRow=105)
  #sce2_rur_total_geral_min_dev_online_40_mais_25 <- paste0("(", sce2_rur_total_geral_min_dev_online_40, ")", "*", "1.25")
  #writeFormula(wb, sheet="Scenarios", x=paste0("=", sce2_rur_total_geral_min_dev_online_40_mais_25), startCol=2, startRow=105)
  #
  #writeData(wb, sheet = "Scenarios", x="sce2_rur_total_geral_min_dev_inperson_40_menos_25", startCol=1, startRow=106)
  #sce2_rur_total_geral_min_dev_inperson_40_menos_25 <- paste0("(", sce2_rur_total_geral_min_dev_inperson_40, ")", "*", "0.75")
  #writeFormula(wb, sheet="Scenarios", x=paste0("=", sce2_rur_total_geral_min_dev_inperson_40_menos_25), startCol=2, startRow=106)
  #writeData(wb, sheet = "Scenarios", x="sce2_rur_total_geral_min_dev_inperson_40_mais_25", startCol=1, startRow=107)
  #sce2_rur_total_geral_min_dev_inperson_40_mais_25 <- paste0("(", sce2_rur_total_geral_min_dev_inperson_40, ")", "*", "1.25")
  #writeFormula(wb, sheet="Scenarios", x=paste0("=", sce2_rur_total_geral_min_dev_inperson_40_mais_25), startCol=2, startRow=107)
  #
  #writeData(wb, sheet = "Scenarios", x="sce2_rur_total_geral_mild_dev_online_60_menos_25", startCol=1, startRow=108)
  #sce2_rur_total_geral_mild_dev_online_60_menos_25 <- paste0("(", sce2_rur_total_geral_mild_dev_online_60, ")", "*", "0.75")
  #writeFormula(wb, sheet="Scenarios", x=paste0("=", sce2_rur_total_geral_mild_dev_online_60_menos_25), startCol=2, startRow=108)
  #writeData(wb, sheet = "Scenarios", x="sce2_rur_total_geral_mild_dev_online_60_mais_25", startCol=1, startRow=109)
  #sce2_rur_total_geral_mild_dev_online_60_mais_25 <- paste0("(", sce2_rur_total_geral_mild_dev_online_60, ")", "*", "1.25")
  #writeFormula(wb, sheet="Scenarios", x=paste0("=", sce2_rur_total_geral_mild_dev_online_60_mais_25), startCol=2, startRow=109)
  #
  #writeData(wb, sheet = "Scenarios", x="sce2_rur_total_geral_mild_dev_online_40_menos_25", startCol=1, startRow=110)
  #sce2_rur_total_geral_mild_dev_online_40_menos_25 <- paste0("(", sce2_rur_total_geral_mild_dev_online_40, ")", "*", "0.75")
  #writeFormula(wb, sheet="Scenarios", x=paste0("=", sce2_rur_total_geral_mild_dev_online_40_menos_25), startCol=2, startRow=110)
  #writeData(wb, sheet = "Scenarios", x="sce2_rur_total_geral_mild_dev_online_40_mais_25", startCol=1, startRow=111)
  #sce2_rur_total_geral_mild_dev_online_40_mais_25 <- paste0("(", sce2_rur_total_geral_mild_dev_online_40, ")", "*", "1.25")
  #writeFormula(wb, sheet="Scenarios", x=paste0("=", sce2_rur_total_geral_mild_dev_online_40_mais_25), startCol=2, startRow=111)
  #
  #writeData(wb, sheet = "Scenarios", x="sce2_rur_total_geral_mild_dev_inperson_40_menos_25", startCol=1, startRow=112)
  #sce2_rur_total_geral_mild_dev_inperson_40_menos_25 <- paste0("(", sce2_rur_total_geral_mild_dev_inperson_40, ")", "*", "0.75")
  #writeFormula(wb, sheet="Scenarios", x=paste0("=", sce2_rur_total_geral_mild_dev_inperson_40_menos_25), startCol=2, startRow=112)
  #writeData(wb, sheet = "Scenarios", x="sce2_rur_total_geral_mild_dev_inperson_40_mais_25", startCol=1, startRow=113)
  #sce2_rur_total_geral_mild_dev_inperson_40_mais_25 <- paste0("(", sce2_rur_total_geral_mild_dev_inperson_40, ")", "*", "1.25")
  #writeFormula(wb, sheet="Scenarios", x=paste0("=", sce2_rur_total_geral_mild_dev_inperson_40_mais_25), startCol=2, startRow=113)
  #
  #writeData(wb, sheet = "Scenarios", x="sce2_rur_total_geral_comp_dev_online_60_menos_25", startCol=1, startRow=114)
  #sce2_rur_total_geral_comp_dev_online_60_menos_25 <- paste0("(", sce2_rur_total_geral_comp_dev_online_60, ")", "*", "0.75")
  #writeFormula(wb, sheet="Scenarios", x=paste0("=", sce2_rur_total_geral_comp_dev_online_60_menos_25), startCol=2, startRow=114)
  #writeData(wb, sheet = "Scenarios", x="sce2_rur_total_geral_comp_dev_online_60_mais_25", startCol=1, startRow=115)
  #sce2_rur_total_geral_comp_dev_online_60_mais_25 <- paste0("(", sce2_rur_total_geral_comp_dev_online_60, ")", "*", "1.25")
  #writeFormula(wb, sheet="Scenarios", x=paste0("=", sce2_rur_total_geral_comp_dev_online_60_mais_25), startCol=2, startRow=115)
  #
  #writeData(wb, sheet = "Scenarios", x="sce2_rur_total_geral_comp_dev_online_40_menos_25", startCol=1, startRow=116)
  #sce2_rur_total_geral_comp_dev_online_40_menos_25 <- paste0("(", sce2_rur_total_geral_comp_dev_online_40, ")", "*", "0.75")
  #writeFormula(wb, sheet="Scenarios", x=paste0("=", sce2_rur_total_geral_comp_dev_online_40_menos_25), startCol=2, startRow=116)
  #writeData(wb, sheet = "Scenarios", x="sce2_rur_total_geral_comp_dev_online_40_mais_25", startCol=1, startRow=117)
  #sce2_rur_total_geral_comp_dev_online_40_mais_25 <- paste0("(", sce2_rur_total_geral_comp_dev_online_40, ")", "*", "1.25")
  #writeFormula(wb, sheet="Scenarios", x=paste0("=", sce2_rur_total_geral_comp_dev_online_40_mais_25), startCol=2, startRow=117)
  #
  #writeData(wb, sheet = "Scenarios", x="sce2_rur_total_geral_comp_dev_inperson_40_menos_25", startCol=1, startRow=118)
  #sce2_rur_total_geral_comp_dev_inperson_40_menos_25 <- paste0("(", sce2_rur_total_geral_comp_dev_inperson_40, ")", "*", "0.75")
  #writeFormula(wb, sheet="Scenarios", x=paste0("=", sce2_rur_total_geral_comp_dev_inperson_40_menos_25), startCol=2, startRow=118)
  #writeData(wb, sheet = "Scenarios", x="sce2_rur_total_geral_comp_dev_inperson_40_mais_25", startCol=1, startRow=119)
  #sce2_rur_total_geral_comp_dev_inperson_40_mais_25 <- paste0("(", sce2_rur_total_geral_comp_dev_inperson_40, ")", "*", "1.25")
  #writeFormula(wb, sheet="Scenarios", x=paste0("=", sce2_rur_total_geral_comp_dev_inperson_40_mais_25), startCol=2, startRow=119)
  
  #---- Scenarios: Cenário 3 -----
  
  #writeData(wb, sheet = "Scenarios", x="sce3_devt_total_geral_online_60", startCol=1, startRow=120)
  #sce3_devt_total_geral_online_60 <- paste0(dim_1_total, "+", sce3_devt_dim_2_total, "+", dim_3_total_online_60, "+", dim_4_total, "+", dim_5_total)
  #writeFormula(wb, sheet="Scenarios", x=paste0("=", sce3_devt_total_geral_online_60), startCol=2, startRow=120)
  #
  #writeData(wb, sheet = "Scenarios", x="sce3_devt_total_geral_online_40", startCol=1, startRow=121)
  #sce3_devt_total_geral_online_40 <- paste0(dim_1_total, "+", sce3_devt_dim_2_total, "+", dim_3_total_online_40, "+", dim_4_total, "+", dim_5_total)
  #writeFormula(wb, sheet="Scenarios", x=paste0("=", sce3_devt_total_geral_online_40), startCol=2, startRow=121)
  #
  #writeData(wb, sheet = "Scenarios", x="sce3_devt_total_geral_inperson_40", startCol=1, startRow=122)
  #sce3_devt_total_geral_inperson_40 <- paste0(dim_1_total, "+", sce3_devt_dim_2_total, "+", dim_3_total_inperson_40, "+", dim_4_total, "+", dim_5_total)
  #writeFormula(wb, sheet="Scenarios", x=paste0("=", sce3_devt_total_geral_inperson_40), startCol=2, startRow=122)
  #
  #writeData(wb, sheet = "Scenarios", x="sce3_devt_total_geral_online_60_menos_25", startCol=1, startRow=123)
  #sce3_devt_total_geral_online_60_menos_25 <- paste0("(", sce3_devt_total_geral_online_60, ")", "*", "0.75")
  #writeFormula(wb, sheet="Scenarios", x=paste0("=", sce3_devt_total_geral_online_60_menos_25), startCol=2, startRow=123)
  #writeData(wb, sheet = "Scenarios", x="sce3_devt_total_geral_online_60_mais_25", startCol=1, startRow=124)
  #sce3_devt_total_geral_online_60_mais_25 <- paste0("(", sce3_devt_total_geral_online_60, ")", "*", "1.25")
  #writeFormula(wb, sheet="Scenarios", x=paste0("=", sce3_devt_total_geral_online_60_mais_25), startCol=2, startRow=124)
  #
  #writeData(wb, sheet = "Scenarios", x="sce3_devt_total_geral_online_40_menos_25", startCol=1, startRow=125)
  #sce3_devt_total_geral_online_40_menos_25 <- paste0("(", sce3_devt_total_geral_online_40, ")", "*", "0.75")
  #writeFormula(wb, sheet="Scenarios", x=paste0("=", sce3_devt_total_geral_online_40_menos_25), startCol=2, startRow=125)
  #writeData(wb, sheet = "Scenarios", x="sce3_devt_total_geral_online_40_mais_25", startCol=1, startRow=126)
  #sce3_devt_total_geral_online_40_mais_25 <- paste0("(", sce3_devt_total_geral_online_40, ")", "*", "1.25")
  #writeFormula(wb, sheet="Scenarios", x=paste0("=", sce3_devt_total_geral_online_40_mais_25), startCol=2, startRow=126)
  #
  #writeData(wb, sheet = "Scenarios", x="sce3_devt_total_geral_inperson_40_menos_25", startCol=1, startRow=127)
  #sce3_devt_total_geral_inperson_40_menos_25 <- paste0("(", sce3_devt_total_geral_inperson_40, ")", "*", "0.75")
  #writeFormula(wb, sheet="Scenarios", x=paste0("=", sce3_devt_total_geral_inperson_40_menos_25), startCol=2, startRow=127)
  #writeData(wb, sheet = "Scenarios", x="sce3_devt_total_geral_inperson_40_mais_25", startCol=1, startRow=128)
  #sce3_devt_total_geral_inperson_40_mais_25 <- paste0("(", sce3_devt_total_geral_inperson_40, ")", "*", "1.25")
  #writeFormula(wb, sheet="Scenarios", x=paste0("=", sce3_devt_total_geral_inperson_40_mais_25), startCol=2, startRow=128)
  
  #---- Scenarios: Estilo da planilha ----
  
  ## Estabelece o tamanho da coluna 3, que possui os nomes das variáveis
  #setColWidths(wb, sheet = "Scenarios", cols = 1, widths = 45) 
  #
  ## Estabelece os estilos das colunas da planilha
  #
  ## Inclui bordas  
  #addStyle(wb, sheet="Scenarios", general_style, rows = 1:128, cols = 1)
  #addStyle(wb, sheet="Scenarios", general_style, rows = 1:2, cols = 2)
  ## Ajusta as variáveis numéricas
  #addStyle(wb, sheet="Scenarios", num_style, rows = 3:128, cols = 2)
  #
  ## Inclui cores
  #addStyle(wb, sheet="Scenarios", style = sce0, rows = 3:29, cols = 1)
  #addStyle(wb, sheet="Scenarios", style = sce1, rows = 30:74, cols = 1)
  #addStyle(wb, sheet="Scenarios", style = sce2, rows = 75:119, cols = 1)
  #addStyle(wb, sheet="Scenarios", style = sce3, rows = 120:128, cols = 1)
  
  #---- Retorna workbook final ----
  
  # Retorna o workbook
  return(wb)
  
}





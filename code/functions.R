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

#--------------------------------------
#- 4. FUNÇÃO PARA ELABORAR A PLANILHA - 
#--------------------------------------

# Função para criar as planilhas
funcao_excel <- function(pais, caminho_output, df_base) {
  
  df_base <- df_base %>% filter(country==pais)
  df_base_reshaped <- reshape2::melt(data=df_base, id.vars = c("country", "subregion"))
  country <- tolower(as.character(df_base %>% select(country)))
  
  # A) Cria um arquivo Excel e adiciona uma planilha 
  
  # Cria um workbook (arquivo Excel) 
  wb <- createWorkbook()
  # Adiciona uma planilha ao arquivo criado
  addWorksheet(wb, "Planilha1")
  
  # B) Inclui os dados iniciais na planilha 1
  
  # Inclui a base de dados na planilha 1
  writeData(wb, sheet = "Planilha1", x = df_base_reshaped, startCol = 1, startRow = 1)
  
  #------------------------------------------------------------------
  #- 2. CRIA PLANILHA COM OS RESULTADOS DOS CÁLCULOS (COM FÓRMULAS) - 
  #------------------------------------------------------------------
  
  # Adiciona uma planilha ao arquivo criado
  addWorksheet(wb, "Planilha2")
  
  # Fórmula 1: Nome do país
  writeFormula(wb, sheet="Planilha2", x=paste0("=Planilha1!A", 1:2), startCol=1, startRow=1)
  # Fórmula 2: Nome da subregião
  writeFormula(wb, sheet="Planilha2", x=paste0("=Planilha1!B", 1:2), startCol=2, startRow=1)
  
  #---------- CÁLCULO GERAL (CENÁRIO 0) ----------
  # Fórmulas para calcular as dimensões
  
  # Fórmula 3: Dimensão 1 - Fiber expansion
  writeData(wb, sheet = "Planilha2", x="dim_1_price_fiber_exp", startCol=3, startRow=1)
  writeFormula(wb, sheet="Planilha2", x="=Planilha1!D16*Planilha1!D2*Planilha1!D46*Planilha1!D20", startCol=3, startRow=2)
  # Fórmula 4: Dimensão 1 - Satellite service
  writeData(wb, sheet = "Planilha2", x="dim_1_price_satellite_service", startCol=4, startRow=1)
  writeFormula(wb, sheet="Planilha2", x="=Planilha1!D15*Planilha1!D2*Planilha1!D18*Planilha1!D45", startCol=4, startRow=2)
  # Fórmula 5: Dimensão 1 - Fiber link
  writeData(wb, sheet = "Planilha2", x="dim_1_price_fiber_link", startCol=5, startRow=1)
  writeFormula(wb, sheet="Planilha2", x="=Planilha1!D14*Planilha1!D2*Planilha1!D12*(Planilha1!D19/200)*Planilha1!D45", startCol=5, startRow=2)
  # Fórmula 6: Dimensão 1 - Access point
  writeData(wb, sheet = "Planilha2", x="dim_1_price_access_point", startCol=6, startRow=1)
  writeFormula(wb, sheet="Planilha2", x="=(((Planilha1!D5/ 25)+Planilha1!D8*3)+((Planilha1!D6/35)+Planilha1!D9*3))*Planilha1!D47*Planilha1!D21", startCol=6, startRow=2)
  # Fórmula 7: Dimensão 1 - Firewall
  writeData(wb, sheet = "Planilha2", x="dim_1_price_firewall", startCol=7, startRow=1)
  writeFormula(wb, sheet="Planilha2", x="=Planilha1!D2*Planilha1!D48*Planilha1!D22", startCol=7, startRow=2)
  # Fórmula 8: Dimensão 1 - Nobreak
  writeData(wb, sheet = "Planilha2", x="dim_1_price_nobreak", startCol=8, startRow=1)
  writeFormula(wb, sheet="Planilha2", x="=Planilha1!D2*Planilha1!D49*Planilha1!D23", startCol=8, startRow=2)
  # Fórmula 9: Dimensão 1 - Switch
  writeData(wb, sheet = "Planilha2", x="dim_1_price_switch", startCol=9, startRow=1)
  writeFormula(wb, sheet="Planilha2", x="=Planilha1!D2*Planilha1!D50*Planilha1!D24", startCol=9, startRow=2)
  # Fórmula 10: Dimensão 1 - Rack 6U/8U
  writeData(wb, sheet = "Planilha2", x="dim_1_price_rack", startCol=10, startRow=1)
  writeFormula(wb, sheet="Planilha2", x="=Planilha1!D2*Planilha1!D51*Planilha1!D25", startCol=10, startRow=2)
  
  # Fórmula 11: Dimensão 2 - Device per students (Min Scenario)
  writeData(wb, sheet = "Planilha2", x="dim_2_price_device_student_min", startCol=11, startRow=1)
  writeFormula(wb, sheet="Planilha2", x="=(Planilha1!D5*Planilha1!D52*Planilha1!D30)+(Planilha1!D6*Planilha1!D53*Planilha1!D32)", startCol=11, startRow=2)
  # Fórmula 12: Dimensão 2 - Device per students (Mild Scenario)
  writeData(wb, sheet = "Planilha2", x="dim_2_price_device_student_mild", startCol=12, startRow=1)
  writeFormula(wb, sheet="Planilha2", x="=(Planilha1!D5*Planilha1!D54*Planilha1!D30)+(Planilha1!D6*Planilha1!D55*Planilha1!D32)", startCol=12, startRow=2)
  # Fórmula 13: Dimensão 2 - Device per students (Comprehensive Scenario)
  writeData(wb, sheet = "Planilha2", x="dim_2_price_device_student_comprehensive", startCol=13, startRow=1)
  writeFormula(wb, sheet="Planilha2", x="=(Planilha1!D5*Planilha1!D56*Planilha1!D30)+(Planilha1!D6*Planilha1!D57*Planilha1!D32)", startCol=13, startRow=2)
  # Fórmula 14: Dimensão 2 - Devices per teacher 
  writeData(wb, sheet = "Planilha2", x="dim_2_price_device_teacher", startCol=14, startRow=1)
  writeFormula(wb, sheet="Planilha2", x="=Planilha1!D7*Planilha1!D58*Planilha1!D31", startCol=14, startRow=2)
  # Fórmula 15: Dimensão 2 - Devices per school 
  writeData(wb, sheet = "Planilha2", x="dim_2_price_device_school", startCol=15, startRow=1)
  writeFormula(wb, sheet="Planilha2", x="=Planilha1!D2*Planilha1!D59*Planilha1!D29", startCol=15, startRow=2)
  # Fórmula 16: Dimensão 2 - Charging cart (Min Scenario)
  writeData(wb, sheet = "Planilha2", x="dim_2_price_charging_cart_min", startCol=16, startRow=1)
  writeFormula(wb, sheet="Planilha2", x="=((Planilha1!D5*Planilha1!D52)+(Planilha1!D6*Planilha1!D53))/30*Planilha1!D28", startCol=16, startRow=2)
  # Fórmula 17: Dimensão 2 - Charging cart (Mild Scenario)
  writeData(wb, sheet = "Planilha2", x="dim_2_price_charging_cart_mild", startCol=17, startRow=1)
  writeFormula(wb, sheet="Planilha2", x="=((Planilha1!D5*Planilha1!D54)+(Planilha1!D6*Planilha1!D55))/30*Planilha1!D28", startCol=17, startRow=2)
  # Fórmula 18: Dimensão 2 - Charging cart (Comprehensive Scenario)
  writeData(wb, sheet = "Planilha2", x="dim_2_price_charging_cart_comprehensive", startCol=18, startRow=1)
  writeFormula(wb, sheet="Planilha2", x="=((Planilha1!D5*Planilha1!D56)+(Planilha1!D6*Planilha1!D57))/30*Planilha1!D28", startCol=18, startRow=2)
  # Fórmula 19: Dimensão 2 - Multimedia projector
  writeData(wb, sheet = "Planilha2", x="dim_2_price_multimedia_projector", startCol=19, startRow=1)
  writeFormula(wb, sheet="Planilha2", x="=Planilha1!D2*Planilha1!D60*Planilha1!D26", startCol=19, startRow=2)
  # Fórmula 20: Dimensão 2 - Headphones (Min Scenario)
  writeData(wb, sheet = "Planilha2", x="dim_2_price_headphones_min_scenario", startCol=20, startRow=1)
  writeFormula(wb, sheet="Planilha2", x="=((Planilha1!D5*Planilha1!D52)+(Planilha1!D6*Planilha1!D53))*Planilha1!D27", startCol=20, startRow=2)
  # Fórmula 21: Dimensão 2 - Headphones (Mild Scenario)
  writeData(wb, sheet = "Planilha2", x="dim_2_price_headphones_mild_scenario", startCol=21, startRow=1)
  writeFormula(wb, sheet="Planilha2", x="=((Planilha1!D5*Planilha1!D54)+(Planilha1!D6*Planilha1!D55))*Planilha1!D27", startCol=21, startRow=2)
  # Fórmula 22: Dimensão 2 - Headphones (Comprehensive Scenario)
  writeData(wb, sheet = "Planilha2", x="dim_2_price_headphones_comprehensive", startCol=22, startRow=1)
  writeFormula(wb, sheet="Planilha2", x="=((Planilha1!D5*Planilha1!D56)+(Planilha1!D6*Planilha1!D57))*Planilha1!D27", startCol=22, startRow=2)
  
  # Fórmula 23: Dimensão 3 - Diagnostic Tool
  writeData(wb, sheet = "Planilha2", x="dim_3_price_diagnostic_tool", startCol=23, startRow=1)
  writeFormula(wb, sheet="Planilha2", x="=Planilha1!D38", startCol=23, startRow=2)
  # Fórmula 24: Dimensão 3 - LMS Platform
  writeData(wb, sheet = "Planilha2", x="dim_3_price_lms_platform", startCol=24, startRow=1)
  writeFormula(wb, sheet="Planilha2", x="=Planilha1!D39", startCol=24, startRow=2)
  
  # Fórmula 25: Dimensão 3 - Content Production (Scenario online-60)
  writeData(wb, sheet = "Planilha2", x="dim_3_price_content_production_scenario_online60", startCol=25, startRow=1)
  writeFormula(wb, sheet="Planilha2", x="=Planilha1!D40", startCol=25, startRow=2)
  # Fórmula 26: Dimensão 3 - Specialist Trainers (Scenario online-60)
  writeData(wb, sheet = "Planilha2", x="dim_3_price_specialist_trainers_scenario_online60", startCol=26, startRow=1)
  writeFormula(wb, sheet="Planilha2", x="=Planilha1!D42*(Planilha1!D37/Planilha1!D61)", startCol=26, startRow=2)
  # Fórmula 27: Dimensão 3 - Course for Trainers (Scenario online-60)
  writeData(wb, sheet = "Planilha2", x="dim_3_price_course_for_trainers_scenario_online60", startCol=27, startRow=1)
  writeFormula(wb, sheet="Planilha2", x="=Planilha1!D42*1.5*(Planilha1!D37/Planilha1!D61)/Planilha1!D61", startCol=27, startRow=2)
  # Fórmula 28: Dimensão 3 - Training design (Scenario online-60)
  writeData(wb, sheet = "Planilha2", x="dim_3_price_training_design_scenario_online60", startCol=28, startRow=1)
  writeFormula(wb, sheet="Planilha2", x="=Planilha1!D41", startCol=28, startRow=2)
  
  # Fórmula 29: Dimensão 3 - Content Production (Scenario online-40)
  writeData(wb, sheet = "Planilha2", x="dim_3_price_content_production_scenario_online40", startCol=29, startRow=1)
  writeFormula(wb, sheet="Planilha2", x="=Planilha1!D40*Planilha1!D44", startCol=29, startRow=2)
  # Fórmula 30: Dimensão 3 - Specialist Trainers (Scenario online-40)
  writeData(wb, sheet = "Planilha2", x="dim_3_price_specialist_trainers_scenario_online40", startCol=30, startRow=1)
  writeFormula(wb, sheet="Planilha2", x="=(Planilha1!D42*(Planilha1!D37/Planilha1!D61))*Planilha1!D44", startCol=30, startRow=2)
  # Fórmula 31: Dimensão 3 - Course for Trainers (Scenario online-40)
  writeData(wb, sheet = "Planilha2", x="dim_3_price_course_for_trainers_scenario_online40", startCol=31, startRow=1)
  writeFormula(wb, sheet="Planilha2", x="=(Planilha1!D42*1.5*(Planilha1!D37/Planilha1!D61)/Planilha1!D61)*Planilha1!D44", startCol=31, startRow=2)
  # Fórmula 32: Dimensão 3 - Training design (Scenario online-40)
  writeData(wb, sheet = "Planilha2", x="dim_3_price_training_design_scenario_online40", startCol=32, startRow=1)
  writeFormula(wb, sheet="Planilha2", x="=Planilha1!D41*Planilha1!D44", startCol=32, startRow=2)
  
  # Fórmula 33: Dimensão 3 - Content Production (Scenario online-40)
  writeData(wb, sheet = "Planilha2", x="dim_3_price_content_production_scenario_inperson40", startCol=33, startRow=1)
  writeFormula(wb, sheet="Planilha2", x="=Planilha1!D40*Planilha1!D44", startCol=33, startRow=2)
  # Fórmula 34: Dimensão 3 - Specialist Trainers (Scenario online-40)
  writeData(wb, sheet = "Planilha2", x="dim_3_price_specialist_trainers_scenario_inperson40", startCol=34, startRow=1)
  writeFormula(wb, sheet="Planilha2", x="=(Planilha1!D42*(Planilha1!D37/Planilha1!D61))*Planilha1!D43*Planilha1!D44", startCol=34, startRow=2)
  # Fórmula 35: Dimensão 3 - Course for Trainers (Scenario online-40)
  writeData(wb, sheet = "Planilha2", x="dim_3_price_course_for_trainers_scenario_inperson40", startCol=35, startRow=1)
  writeFormula(wb, sheet="Planilha2", x="=(Planilha1!D42*1.5*(Planilha1!D37/Planilha1!D61)/Planilha1!D61)*Planilha1!D43*Planilha1!D44", startCol=35, startRow=2)
  # Fórmula 36: Dimensão 3 - Training design (Scenario online-40)
  writeData(wb, sheet = "Planilha2", x="dim_3_price_training_design_scenario_inperson40", startCol=36, startRow=1)
  writeFormula(wb, sheet="Planilha2", x="=Planilha1!D41*Planilha1!D44", startCol=36, startRow=2)
  
  # Fórmula 37: Dimensão 4 - Teaching and learning platform
  writeData(wb, sheet = "Planilha2", x="dim_4_price_teaching_learning_platform", startCol=37, startRow=1)
  writeFormula(wb, sheet="Planilha2", x="=(Planilha1!D6+Planilha1!D5)*Planilha1!D33*Planilha1!D62*(Planilha1!D45/12)", startCol=37, startRow=2)
  # Fórmula 38: Dimensão 4 - Management platform
  writeData(wb, sheet = "Planilha2", x="dim_4_price_management_platform", startCol=38, startRow=1)
  writeFormula(wb, sheet="Planilha2", x="=Planilha1!D2*Planilha1!D34*Planilha1!D63*(Planilha1!D45/12)", startCol=38, startRow=2)
  
  # Fórmula 39: Dimensão 5 - Central team
  writeData(wb, sheet = "Planilha2", x="dim_5_price_central_team", startCol=39, startRow=1)
  writeFormula(wb, sheet="Planilha2", x="=Planilha1!D35*((Planilha1!D64+Planilha1!D65)/2)*Planilha1!D45", startCol=39, startRow=2)
  # Fórmula 40: Dimensão 5 - Regional team
  writeData(wb, sheet = "Planilha2", x="dim_5_price_regional_team", startCol=40, startRow=1)
  writeFormula(wb, sheet="Planilha2", x="=Planilha1!D66*Planilha1!D2*Planilha1!D36*Planilha1!D45", startCol=40, startRow=2)
  # Fórmula 41: Dimensão 5 - Local team
  writeData(wb, sheet = "Planilha2", x="dim_5_price_local_team", startCol=41, startRow=1)
  writeFormula(wb, sheet="Planilha2", x="=(Planilha1!D67*(Planilha1!D2-Planilha1!D3)+(Planilha1!D68*Planilha1!D3))*Planilha1!D36*Planilha1!D45", startCol=41, startRow=2)
  
  # Adiciona uma planilha ao arquivo criado
  addWorksheet(wb, "Planilha3")
  
  # Fórmula 42: Nome do país
  writeFormula(wb, sheet="Planilha3", x=paste0("=Planilha1!A", 1:2), startCol=1, startRow=1)
  # Fórmula 43: Nome da subregião
  writeFormula(wb, sheet="Planilha3", x=paste0("=Planilha1!B", 1:2), startCol=2, startRow=1)
  
  # Fórmula 44: Total da dimensão 1
  writeData(wb, sheet = "Planilha3", x="dim_1_total", startCol=3, startRow=1)
  writeFormula(wb, sheet="Planilha3", x="=SUM(Planilha2!C2:J2)", startCol=3, startRow=2)
  
  # Fórmula 45: Total da dimensão 2 (min)
  writeData(wb, sheet = "Planilha3", x="dim_2_total_min", startCol=4, startRow=1)
  writeFormula(wb, sheet="Planilha3", x="=Planilha2!K2+Planilha2!N2+Planilha2!O2+Planilha2!P2+Planilha2!S2+Planilha2!T2", startCol=4, startRow=2)
  # Fórmula 46: Total da dimensão 2 (mild)
  writeData(wb, sheet = "Planilha3", x="dim_2_total_mild", startCol=5, startRow=1)
  writeFormula(wb, sheet="Planilha3", x="=Planilha2!L2+Planilha2!N2+Planilha2!O2+Planilha2!Q2+Planilha2!S2+Planilha2!U2", startCol=5, startRow=2)
  # Fórmula 47: Total da dimensão 2 (comprehensive)
  writeData(wb, sheet = "Planilha3", x="dim_2_total_comprehensive", startCol=6, startRow=1)
  writeFormula(wb, sheet="Planilha3", x="=Planilha2!M2+Planilha2!N2+Planilha2!O2+Planilha2!R2+Planilha2!S2+Planilha2!V2", startCol=6, startRow=2)
  
  # Fórmula 48: Total da dimensão 3
  writeData(wb, sheet = "Planilha3", x="dim_3_total_online_60", startCol=7, startRow=1)
  writeFormula(wb, sheet="Planilha3", x="=Planilha2!W2+Planilha2!X2+Planilha2!Y2+Planilha2!Z2+Planilha2!AA2+Planilha2!AB2", startCol=7, startRow=2)
  # Fórmula 49: Total da dimensão 3
  writeData(wb, sheet = "Planilha3", x="dim_3_total_online_40", startCol=8, startRow=1)
  writeFormula(wb, sheet="Planilha3", x="=Planilha2!W2+Planilha2!X2+Planilha2!AC2+Planilha2!AD2+Planilha2!AE2+Planilha2!AF2", startCol=8, startRow=2)
  # Fórmula 50: Total da dimensão 3
  writeData(wb, sheet = "Planilha3", x="dim_3_total_inperson_40", startCol=9, startRow=1)
  writeFormula(wb, sheet="Planilha3", x="=Planilha2!W2+Planilha2!X2+Planilha2!AG2+Planilha2!AH2+Planilha2!AI2+Planilha2!AJ2", startCol=9, startRow=2)
  
  # Fórmula 51: Total da dimensão 4
  writeData(wb, sheet = "Planilha3", x="dim_4_total", startCol=10, startRow=1)
  writeFormula(wb, sheet="Planilha3", x="=SUM(Planilha2!AK2:AL2)", startCol=10, startRow=2)
  # Fórmula 52: Total da dimensão 5
  writeData(wb, sheet = "Planilha3", x="dim_5_total", startCol=11, startRow=1)
  writeFormula(wb, sheet="Planilha3", x="=SUM(Planilha2!AM2:AO2)", startCol=11, startRow=2)
  
  # Adiciona uma planilha ao arquivo criado
  addWorksheet(wb, "Planilha4")
  
  # Fórmula 53: Nome do país
  writeFormula(wb, sheet="Planilha4", x=paste0("=Planilha1!A", 1:2), startCol=1, startRow=1)
  # Fórmula 54: Nome da subregião
  writeFormula(wb, sheet="Planilha4", x=paste0("=Planilha1!B", 1:2), startCol=2, startRow=1)
  
  # Fórmula 55: Total Geral para Cenário Mínimo de Dispositivos e Treinamento Online 60h
  writeData(wb, sheet = "Planilha4", x="total_geral_min_dev_online_60", startCol=3, startRow=1)
  writeFormula(wb, sheet="Planilha4", x="=Planilha3!C2+Planilha3!D2+Planilha3!G2+Planilha3!J2+Planilha3!K2", startCol=3, startRow=2)
  # Fórmula 56: Total Geral para Cenário Mínimo de Dispositivos e Treinamento Online 40h
  writeData(wb, sheet = "Planilha4", x="total_geral_min_dev_online_40", startCol=4, startRow=1)
  writeFormula(wb, sheet="Planilha4", x="=Planilha3!C2+Planilha3!D2+Planilha3!H2+Planilha3!J2+Planilha3!K2", startCol=4, startRow=2)
  # Fórmula 57: Total Geral para Cenário Mínimo de Dispositivos e Treinamento Presencial 40h
  writeData(wb, sheet = "Planilha4", x="total_geral_min_dev_inperson_40", startCol=5, startRow=1)
  writeFormula(wb, sheet="Planilha4", x="=Planilha3!C2+Planilha3!D2+Planilha3!I2+Planilha3!J2+Planilha3!K2", startCol=5, startRow=2)
  # Fórmula 58: Total Geral para Cenário Moderado de Dispositivos e Treinamento Online 60h
  writeData(wb, sheet = "Planilha4", x="total_geral_mild_dev_online_60", startCol=6, startRow=1)
  writeFormula(wb, sheet="Planilha4", x="=Planilha3!C2+Planilha3!E2+Planilha3!G2+Planilha3!J2+Planilha3!K2", startCol=6, startRow=2)
  # Fórmula 59: Total Geral para Cenário Moderado de Dispositivos e Treinamento Online 40h
  writeData(wb, sheet = "Planilha4", x="total_geral_mild_dev_online_40", startCol=7, startRow=1)
  writeFormula(wb, sheet="Planilha4", x="=Planilha3!C2+Planilha3!E2+Planilha3!H2+Planilha3!J2+Planilha3!K2", startCol=7, startRow=2)
  # Fórmula 60: Total Geral para Cenário Moderado de Dispositivos e Treinamento Presencial 40h
  writeData(wb, sheet = "Planilha4", x="total_geral_mild_dev_inperson_40", startCol=8, startRow=1)
  writeFormula(wb, sheet="Planilha4", x="=Planilha3!C2+Planilha3!E2+Planilha3!I2+Planilha3!J2+Planilha3!K2", startCol=8, startRow=2)
  # Fórmula 61: Total Geral para Cenário Abrangente de Dispositivos e Treinamento Online 60h
  writeData(wb, sheet = "Planilha4", x="total_geral_comp_dev_online_60", startCol=9, startRow=1)
  writeFormula(wb, sheet="Planilha4", x="=Planilha3!C2+Planilha3!F2+Planilha3!G2+Planilha3!J2+Planilha3!K2", startCol=9, startRow=2)
  # Fórmula 62: Total Geral para Cenário Abrangente de Dispositivos e Treinamento Online 40h
  writeData(wb, sheet = "Planilha4", x="total_geral_comp_dev_online_40", startCol=10, startRow=1)
  writeFormula(wb, sheet="Planilha4", x="=Planilha3!C2+Planilha3!F2+Planilha3!H2+Planilha3!J2+Planilha3!K2", startCol=10, startRow=2)
  # Fórmula 63: Total Geral para Cenário Abrangente de Dispositivos e Treinamento Presencial 40h
  writeData(wb, sheet = "Planilha4", x="total_geral_comp_dev_inperson_40", startCol=11, startRow=1)
  writeFormula(wb, sheet="Planilha4", x="=Planilha3!C2+Planilha3!F2+Planilha3!I2+Planilha3!J2+Planilha3!K2", startCol=11, startRow=2)
  
  # Fórmula 64: Range para Total Geral (Mínimo de Dispositivos e Treinamento Online 60h)
  writeData(wb, sheet = "Planilha4", x="total_geral_min_dev_online_60_menos_25", startCol=12, startRow=1)
  writeFormula(wb, sheet="Planilha4", x="=(Planilha3!C2+Planilha3!D2+Planilha3!G2+Planilha3!J2+Planilha3!K2)*0.75", startCol=12, startRow=2)
  writeData(wb, sheet = "Planilha4", x="total_geral_min_dev_online_60_mais_25", startCol=13, startRow=1)
  writeFormula(wb, sheet="Planilha4", x="=(Planilha3!C2+Planilha3!D2+Planilha3!G2+Planilha3!J2+Planilha3!K2)*1.25", startCol=13, startRow=2)
  
  # Fórmula 65: Range para Total Geral (Mínimo de Dispositivos e Treinamento Online 40h)
  writeData(wb, sheet = "Planilha4", x="total_geral_min_dev_online_40_menos_25", startCol=14, startRow=1)
  writeFormula(wb, sheet="Planilha4", x="=(Planilha3!C2+Planilha3!D2+Planilha3!H2+Planilha3!J2+Planilha3!K2)*0.75", startCol=14, startRow=2)
  writeData(wb, sheet = "Planilha4", x="total_geral_min_dev_online_40_mais_25", startCol=15, startRow=1)
  writeFormula(wb, sheet="Planilha4", x="=(Planilha3!C2+Planilha3!D2+Planilha3!H2+Planilha3!J2+Planilha3!K2)*1.25", startCol=15, startRow=2)
  
  # Fórmula 66: Range para Total Geral (Mínimo de Dispositivos e Treinamento Presencial 40h)
  writeData(wb, sheet = "Planilha4", x="total_geral_min_dev_inperson_40_menos_25", startCol=16, startRow=1)
  writeFormula(wb, sheet="Planilha4", x="=(Planilha3!C2+Planilha3!D2+Planilha3!I2+Planilha3!J2+Planilha3!K2)*0.75", startCol=16, startRow=2)
  writeData(wb, sheet = "Planilha4", x="total_geral_min_dev_inperson_40_mais_25", startCol=17, startRow=1)
  writeFormula(wb, sheet="Planilha4", x="=(Planilha3!C2+Planilha3!D2+Planilha3!I2+Planilha3!J2+Planilha3!K2)*1.25", startCol=17, startRow=2)
  
  # Fórmula 67: Range para Total Geral (Moderado de Dispositivos e Treinamento Online 60h)
  writeData(wb, sheet = "Planilha4", x="total_geral_mild_dev_online_60_menos_25", startCol=18, startRow=1)
  writeFormula(wb, sheet="Planilha4", x="=(Planilha3!C2+Planilha3!E2+Planilha3!G2+Planilha3!J2+Planilha3!K2)*0.75", startCol=18, startRow=2)
  writeData(wb, sheet = "Planilha4", x="total_geral_mild_dev_online_60_mais_25", startCol=19, startRow=1)
  writeFormula(wb, sheet="Planilha4", x="=(Planilha3!C2+Planilha3!E2+Planilha3!G2+Planilha3!J2+Planilha3!K2)*1.25", startCol=19, startRow=2)
  
  # Fórmula 68: Range para Total Geral (Moderado de Dispositivos e Treinamento Online 40h)
  writeData(wb, sheet = "Planilha4", x="total_geral_mild_dev_online_40_menos_25", startCol=20, startRow=1)
  writeFormula(wb, sheet="Planilha4", x="=(Planilha3!C2+Planilha3!E2+Planilha3!H2+Planilha3!J2+Planilha3!K2)*0.75", startCol=20, startRow=2)
  writeData(wb, sheet = "Planilha4", x="total_geral_mild_dev_online_40_mais_25", startCol=21, startRow=1)
  writeFormula(wb, sheet="Planilha4", x="=(Planilha3!C2+Planilha3!E2+Planilha3!H2+Planilha3!J2+Planilha3!K2)*1.25", startCol=21, startRow=2)
  
  # Fórmula 69: Range para Total Geral (Moderado de Dispositivos e Treinamento Presencial 40h)
  writeData(wb, sheet = "Planilha4", x="total_geral_mild_dev_inperson_40_menos_25", startCol=22, startRow=1)
  writeFormula(wb, sheet="Planilha4", x="=(Planilha3!C2+Planilha3!E2+Planilha3!I2+Planilha3!J2+Planilha3!K2)*0.75", startCol=22, startRow=2)
  writeData(wb, sheet = "Planilha4", x="total_geral_mild_dev_inperson_40_mais_25", startCol=23, startRow=1)
  writeFormula(wb, sheet="Planilha4", x="=(Planilha3!C2+Planilha3!E2+Planilha3!I2+Planilha3!J2+Planilha3!K2)*1.25", startCol=23, startRow=2)
  
  # Fórmula 70: Range para Total Geral (Abrangente de Dispositivos e Treinamento Online 60h)
  writeData(wb, sheet = "Planilha4", x="total_geral_comp_dev_online_60_menos_25", startCol=24, startRow=1)
  writeFormula(wb, sheet="Planilha4", x="=(Planilha3!C2+Planilha3!F2+Planilha3!G2+Planilha3!J2+Planilha3!K2)*0.75", startCol=24, startRow=2)
  writeData(wb, sheet = "Planilha4", x="total_geral_comp_dev_online_60_mais_25", startCol=25, startRow=1)
  writeFormula(wb, sheet="Planilha4", x="=(Planilha3!C2+Planilha3!F2+Planilha3!G2+Planilha3!J2+Planilha3!K2)*1.25", startCol=25, startRow=2)
  
  # Fórmula 71: Range para Total Geral (Abrangente de Dispositivos e Treinamento Online 40h)
  writeData(wb, sheet = "Planilha4", x="total_geral_comp_dev_online_40_menos_25", startCol=26, startRow=1)
  writeFormula(wb, sheet="Planilha4", x="=(Planilha3!C2+Planilha3!F2+Planilha3!H2+Planilha3!J2+Planilha3!K2)*0.75", startCol=26, startRow=2)
  writeData(wb, sheet = "Planilha4", x="total_geral_comp_dev_online_40_mais_25", startCol=27, startRow=1)
  writeFormula(wb, sheet="Planilha4", x="=(Planilha3!C2+Planilha3!F2+Planilha3!H2+Planilha3!J2+Planilha3!K2)*1.25", startCol=27, startRow=2)
  
  # Fórmula 72: Range para Total Geral (Abrangente de Dispositivos e Treinamento Presencial 40h)
  writeData(wb, sheet = "Planilha4", x="total_geral_comp_dev_inperson_40_menos_25", startCol=28, startRow=1)
  writeFormula(wb, sheet="Planilha4", x="=(Planilha3!C2+Planilha3!F2+Planilha3!I2+Planilha3!J2+Planilha3!K2)*0.75", startCol=28, startRow=2)
  writeData(wb, sheet = "Planilha4", x="total_geral_comp_dev_inperson_40_mais_25", startCol=29, startRow=1)
  writeFormula(wb, sheet="Planilha4", x="=(Planilha3!C2+Planilha3!F2+Planilha3!I2+Planilha3!J2+Planilha3!K2)*1.25", startCol=29, startRow=2)
  
  #---------- CENÁRIO 1 ----------
  
  # Fórmula 73: Dimensão 1 - Fiber expansion
  writeData(wb, sheet = "Planilha2", x="sce1_sec_dim_1_price_fiber_exp", startCol=42, startRow=1)
  writeFormula(wb, sheet="Planilha2", x="=Planilha1!D16*Planilha1!D9*Planilha1!D46*Planilha1!D20", startCol=42, startRow=2)
  # Fórmula 74: Dimensão 1 - Satellite service
  writeData(wb, sheet = "Planilha2", x="sce1_sec_dim_1_price_satellite_service", startCol=43, startRow=1)
  writeFormula(wb, sheet="Planilha2", x="=Planilha1!D15*Planilha1!D9*Planilha1!D18*Planilha1!D45", startCol=43, startRow=2)
  # Fórmula 75: Dimensão 1 - Fiber link
  writeData(wb, sheet = "Planilha2", x="sce1_sec_dim_1_price_fiber_link", startCol=44, startRow=1)
  writeFormula(wb, sheet="Planilha2", x="=Planilha1!D14*Planilha1!D9*Planilha1!D12*(Planilha1!D19/200)*Planilha1!D45", startCol=44, startRow=2)
  
  # Fórmula 76: Dimensão 1 - Access point
  writeData(wb, sheet = "Planilha2", x="sce1_sec_dim_1_price_access_point", startCol=45, startRow=1)
  writeFormula(wb, sheet="Planilha2", x="=((Planilha1!D6/35)+Planilha1!D9*3)*Planilha1!D47*Planilha1!D21", startCol=45, startRow=2)
  
  
  # Fórmula 77: Dimensão 1 - Firewall
  writeData(wb, sheet = "Planilha2", x="sce1_sec_dim_1_price_firewall", startCol=46, startRow=1)
  writeFormula(wb, sheet="Planilha2", x="=Planilha1!D9*Planilha1!D48*Planilha1!D22", startCol=46, startRow=2)
  # Fórmula 78: Dimensão 1 - Nobreak
  writeData(wb, sheet = "Planilha2", x="sce1_sec_dim_1_price_nobreak", startCol=47, startRow=1)
  writeFormula(wb, sheet="Planilha2", x="=Planilha1!D9*Planilha1!D49*Planilha1!D23", startCol=47, startRow=2)
  # Fórmula 79: Dimensão 1 - Switch
  writeData(wb, sheet = "Planilha2", x="sce1_sec_dim_1_price_switch", startCol=48, startRow=1)
  writeFormula(wb, sheet="Planilha2", x="=Planilha1!D9*Planilha1!D50*Planilha1!D24", startCol=48, startRow=2)
  # Fórmula 80: Dimensão 1 - Rack 6U/8U
  writeData(wb, sheet = "Planilha2", x="sce1_sec_dim_1_price_rack", startCol=49, startRow=1)
  writeFormula(wb, sheet="Planilha2", x="=Planilha1!D9*Planilha1!D51*Planilha1!D25", startCol=49, startRow=2)
  
  # Fórmula 81: Dimensão 2 - Device per students (Min Scenario)
  writeData(wb, sheet = "Planilha2", x="sce1_sec_dim_2_price_device_student_min", startCol=50, startRow=1)
  writeFormula(wb, sheet="Planilha2", x="=Planilha1!D6*Planilha1!D53*Planilha1!D32", startCol=50, startRow=2)
  # Fórmula 82: Dimensão 2 - Device per students (Mild Scenario)
  writeData(wb, sheet = "Planilha2", x="sce1_sec_dim_2_price_device_student_mild", startCol=51, startRow=1)
  writeFormula(wb, sheet="Planilha2", x="=Planilha1!D6*Planilha1!D55*Planilha1!D32", startCol=51, startRow=2)
  # Fórmula 83: Dimensão 2 - Device per students (Comprehensive Scenario)
  writeData(wb, sheet = "Planilha2", x="sce1_sec_dim_2_price_device_student_comprehensive", startCol=52, startRow=1)
  writeFormula(wb, sheet="Planilha2", x="=Planilha1!D6*Planilha1!D57*Planilha1!D32", startCol=52, startRow=2)
  # Fórmula 84: Dimensão 2 - Devices per teacher 
  writeData(wb, sheet = "Planilha2", x="sce1_sec_dim_2_price_device_teacher", startCol=53, startRow=1)
  writeFormula(wb, sheet="Planilha2", x="=(Planilha1!D7*(Planilha1!D6/(Planilha1!D4)))*Planilha1!D58*Planilha1!D31", startCol=53, startRow=2)
  # Fórmula 85: Dimensão 2 - Devices per school 
  writeData(wb, sheet = "Planilha2", x="sce1_sec_dim_2_price_device_school", startCol=54, startRow=1)
  writeFormula(wb, sheet="Planilha2", x="=Planilha1!D9*Planilha1!D59*Planilha1!D29", startCol=54, startRow=2)
  # Fórmula 86: Dimensão 2 - Charging cart (Min Scenario)
  writeData(wb, sheet = "Planilha2", x="sce1_sec_dim_2_price_charging_cart_min", startCol=55, startRow=1)
  writeFormula(wb, sheet="Planilha2", x="=(Planilha1!D6*Planilha1!D53)/30*Planilha1!D28", startCol=55, startRow=2)
  # Fórmula 87: Dimensão 2 - Charging cart (Mild Scenario)
  writeData(wb, sheet = "Planilha2", x="sce1_sec_dim_2_price_charging_cart_mild", startCol=56, startRow=1)
  writeFormula(wb, sheet="Planilha2", x="=(Planilha1!D6*Planilha1!D55)/30*Planilha1!D28", startCol=56, startRow=2)
  # Fórmula 88: Dimensão 2 - Charging cart (Comprehensive Scenario)
  writeData(wb, sheet = "Planilha2", x="sce1_sec_dim_2_price_charging_cart_comprehensive", startCol=57, startRow=1)
  writeFormula(wb, sheet="Planilha2", x="=(Planilha1!D6*Planilha1!D57)/30*Planilha1!D28", startCol=57, startRow=2)
  # Fórmula 89: Dimensão 2 - Multimedia projector
  writeData(wb, sheet = "Planilha2", x="sce1_sec_dim_2_price_multimedia_projector", startCol=58, startRow=1)
  writeFormula(wb, sheet="Planilha2", x="=Planilha1!D9*Planilha1!D60*Planilha1!D26", startCol=58, startRow=2)
  # Fórmula 90: Dimensão 2 - Headphones (Min Scenario)
  writeData(wb, sheet = "Planilha2", x="sce1_sec_dim_2_price_headphones_min_scenario", startCol=59, startRow=1)
  writeFormula(wb, sheet="Planilha2", x="=(Planilha1!D6*Planilha1!D53)*Planilha1!D27", startCol=59, startRow=2)
  # Fórmula 91: Dimensão 2 - Headphones (Mild Scenario)
  writeData(wb, sheet = "Planilha2", x="sce1_sec_dim_2_price_headphones_mild_scenario", startCol=60, startRow=1)
  writeFormula(wb, sheet="Planilha2", x="=(Planilha1!D6*Planilha1!D55)*Planilha1!D27", startCol=60, startRow=2)
  # Fórmula 92: Dimensão 2 - Headphones (Comprehensive Scenario)
  writeData(wb, sheet = "Planilha2", x="sce1_sec_dim_2_price_headphones_comprehensive", startCol=61, startRow=1)
  writeFormula(wb, sheet="Planilha2", x="=(Planilha1!D6*Planilha1!D57)*Planilha1!D27", startCol=61, startRow=2)
  
  # Fórmula 93: Dimensão 3 - Diagnostic Tool
  writeData(wb, sheet = "Planilha2", x="sce1_sec_dim_3_price_diagnostic_tool", startCol=62, startRow=1)
  writeFormula(wb, sheet="Planilha2", x="=Planilha1!D38", startCol=62, startRow=2)
  # Fórmula 94: Dimensão 3 - LMS Platform
  writeData(wb, sheet = "Planilha2", x="sce1_sec_dim_3_price_lms_platform", startCol=63, startRow=1)
  writeFormula(wb, sheet="Planilha2", x="=Planilha1!D39", startCol=63, startRow=2)
  
  # Fórmula 95: Dimensão 3 - Content Production (Scenario online-60)
  writeData(wb, sheet = "Planilha2", x="sce1_sec_dim_3_price_content_production_scenario_online60", startCol=64, startRow=1)
  writeFormula(wb, sheet="Planilha2", x="=Planilha1!D40", startCol=64, startRow=2)
  # Fórmula 96: Dimensão 3 - Specialist Trainers (Scenario online-60)
  writeData(wb, sheet = "Planilha2", x="sce1_sec_dim_3_price_specialist_trainers_scenario_online60", startCol=65, startRow=1)
  writeFormula(wb, sheet="Planilha2", x="=Planilha1!D42*((Planilha1!D37*(Planilha1!D6/(Planilha1!D4)))/Planilha1!D61)", startCol=65, startRow=2)
  # Fórmula 97: Dimensão 3 - Course for Trainers (Scenario online-60)
  writeData(wb, sheet = "Planilha2", x="sce1_sec_dim_3_price_course_for_trainers_scenario_online60", startCol=66, startRow=1)
  writeFormula(wb, sheet="Planilha2", x="=Planilha1!D36 *1.5*((Planilha1!D37*(Planilha1!D6/(Planilha1!D4)))/Planilha1!D61)/Planilha1!D61", startCol=66, startRow=2)
  # Fórmula 98: Dimensão 3 - Training design (Scenario online-60)
  writeData(wb, sheet = "Planilha2", x="sce1_sec_dim_3_price_training_design_scenario_online60", startCol=67, startRow=1)
  writeFormula(wb, sheet="Planilha2", x="=Planilha1!D41", startCol=67, startRow=2)
  
  # Fórmula 99: Dimensão 3 - Content Production (Scenario online-40)
  writeData(wb, sheet = "Planilha2", x="sce1_sec_dim_3_price_content_production_scenario_online40", startCol=68, startRow=1)
  writeFormula(wb, sheet="Planilha2", x="=Planilha1!D44*(Planilha1!D40)", startCol=68, startRow=2)
  # Fórmula 100: Dimensão 3 - Specialist Trainers (Scenario online-40)
  writeData(wb, sheet = "Planilha2", x="sce1_sec_dim_3_price_specialist_trainers_scenario_online40", startCol=69, startRow=1)
  writeFormula(wb, sheet="Planilha2", x="=Planilha1!D44*(Planilha1!D42*((Planilha1!D37*(Planilha1!D6/(Planilha1!D4)))/Planilha1!D61))", startCol=69, startRow=2)
  # Fórmula 101: Dimensão 3 - Course for Trainers (Scenario online-40)
  writeData(wb, sheet = "Planilha2", x="sce1_sec_dim_3_price_course_for_trainers_scenario_online40", startCol=70, startRow=1)
  writeFormula(wb, sheet="Planilha2", x="=Planilha1!D44*(Planilha1!D36 *1.5*((Planilha1!D37*(Planilha1!D6/(Planilha1!D4)))/Planilha1!D61)/Planilha1!D61)", startCol=70, startRow=2)
  # Fórmula 102: Dimensão 3 - Training design (Scenario online-40)
  writeData(wb, sheet = "Planilha2", x="sce1_sec_dim_3_price_training_design_scenario_online40", startCol=71, startRow=1)
  writeFormula(wb, sheet="Planilha2", x="=Planilha1!D44*(Planilha1!D41)", startCol=71, startRow=2)
  
  # Fórmula 103: Dimensão 3 - Content Production (Scenario online-40)
  writeData(wb, sheet = "Planilha2", x="sce1_sec_dim_3_price_content_production_scenario_inperson40", startCol=72, startRow=1)
  writeFormula(wb, sheet="Planilha2", x="=Planilha1!D44*(Planilha1!D40)", startCol=72, startRow=2)
  # Fórmula 104: Dimensão 3 - Specialist Trainers (Scenario online-40)
  writeData(wb, sheet = "Planilha2", x="sce1_sec_dim_3_price_specialist_trainers_scenario_inperson40", startCol=73, startRow=1)
  writeFormula(wb, sheet="Planilha2", x="=Planilha1!D43*Planilha1!D44*(Planilha1!D42*((Planilha1!D37*(Planilha1!D6/(Planilha1!D4)))/Planilha1!D61))", startCol=73, startRow=2)
  # Fórmula 105: Dimensão 3 - Course for Trainers (Scenario online-40)
  writeData(wb, sheet = "Planilha2", x="sce1_sec_dim_3_price_course_for_trainers_scenario_inperson40", startCol=74, startRow=1)
  writeFormula(wb, sheet="Planilha2", x="=Planilha1!D43*Planilha1!D44*(Planilha1!D36 *1.5*((Planilha1!D37*(Planilha1!D6/(Planilha1!D4)))/Planilha1!D61)/Planilha1!D61)", startCol=74, startRow=2)
  # Fórmula 106: Dimensão 3 - Training design (Scenario online-40)
  writeData(wb, sheet = "Planilha2", x="sce1_sec_dim_3_price_training_design_scenario_inperson40", startCol=75, startRow=1)
  writeFormula(wb, sheet="Planilha2", x="=Planilha1!D44*(Planilha1!D41)", startCol=75, startRow=2)
  
  # Fórmula 107: Dimensão 4 - Teaching and learning platform
  writeData(wb, sheet = "Planilha2", x="sce1_sec_dim_4_price_teaching_learning_platform", startCol=76, startRow=1)
  writeFormula(wb, sheet="Planilha2", x="=Planilha1!D6*Planilha1!D33*Planilha1!D62*(Planilha1!D45/12)", startCol=76, startRow=2)
  # Fórmula 108: Dimensão 4 - Management platform
  writeData(wb, sheet = "Planilha2", x="sce1_sec_dim_4_price_management_platform", startCol=77, startRow=1)
  writeFormula(wb, sheet="Planilha2", x="=Planilha1!D9*Planilha1!D34*Planilha1!D63*(Planilha1!D45/12)", startCol=77, startRow=2)
  
  # Fórmula 109: Dimensão 5 - Central team
  writeData(wb, sheet = "Planilha2", x="sce1_sec_dim_5_price_central_team", startCol=78, startRow=1)
  writeFormula(wb, sheet="Planilha2", x="=Planilha1!D35*((Planilha1!D64+Planilha1!D65)/2)*Planilha1!D45", startCol=78, startRow=2)
  # Fórmula 110: Dimensão 5 - Regional team
  writeData(wb, sheet = "Planilha2", x="sce1_sec_dim_5_price_regional_team", startCol=79, startRow=1)
  writeFormula(wb, sheet="Planilha2", x="=Planilha1!D66*Planilha1!D9*Planilha1!D36*Planilha1!D45", startCol=79, startRow=2)
  # Fórmula 111: Dimensão 5 - Local team
  writeData(wb, sheet = "Planilha2", x="sce1_sec_dim_5_price_local_team", startCol=80, startRow=1)
  writeFormula(wb, sheet="Planilha2", x="=(Planilha1!D67*(Planilha1!D9/Planilha1!D2)*(Planilha1!D2-Planilha1!D3)+(Planilha1!D9/Planilha1!D2)*(Planilha1!D68*Planilha1!D3))*Planilha1!D36*Planilha1!D45", startCol=80, startRow=2)
  
  # Fórmula 112: Total da dimensão 1
  writeData(wb, sheet = "Planilha3", x="sce1_sec_dim_1_total", startCol=12, startRow=1)
  writeFormula(wb, sheet="Planilha3", x="=SUM(Planilha2!AP2:AW2)", startCol=12, startRow=2)
  
  # Fórmula 113: Total da dimensão 2 (min)
  writeData(wb, sheet = "Planilha3", x="sce1_sec_dim_2_total_min", startCol=13, startRow=1)
  writeFormula(wb, sheet="Planilha3", x="=Planilha2!AX2+Planilha2!BA2+Planilha2!BB2+Planilha2!BC2+Planilha2!BF2+Planilha2!BG2", startCol=13, startRow=2)
  # Fórmula 114: Total da dimensão 2 (mild)
  writeData(wb, sheet = "Planilha3", x="sce1_sec_dim_2_total_mild", startCol=14, startRow=1)
  writeFormula(wb, sheet="Planilha3", x="=Planilha2!AY2+Planilha2!BA2+Planilha2!BB2+Planilha2!BD2+Planilha2!BF2+Planilha2!BH2", startCol=14, startRow=2)
  # Fórmula 115: Total da dimensão 2 (comprehensive)
  writeData(wb, sheet = "Planilha3", x="sce1_sec_dim_2_total_comprehensive", startCol=15, startRow=1)
  writeFormula(wb, sheet="Planilha3", x="=Planilha2!AZ2+Planilha2!BA2+Planilha2!BB2+Planilha2!BE2+Planilha2!BF2+Planilha2!BI2", startCol=15, startRow=2)
  
  # Fórmula 116: Total da dimensão 3
  writeData(wb, sheet = "Planilha3", x="sce1_sec_dim_3_total_online_60", startCol=16, startRow=1)
  writeFormula(wb, sheet="Planilha3", x="=Planilha2!BJ2+Planilha2!BK2+Planilha2!BL2+Planilha2!BM2+Planilha2!BN2+Planilha2!BO2", startCol=16, startRow=2)
  # Fórmula 117: Total da dimensão 3
  writeData(wb, sheet = "Planilha3", x="sce1_sec_dim_3_total_online_40", startCol=17, startRow=1)
  writeFormula(wb, sheet="Planilha3", x="=Planilha2!BJ2+Planilha2!BK2+Planilha2!BP2+Planilha2!BQ2+Planilha2!BR2+Planilha2!BS2", startCol=17, startRow=2)
  # Fórmula 118: Total da dimensão 3
  writeData(wb, sheet = "Planilha3", x="sce1_sec_dim_3_total_inperson_40", startCol=18, startRow=1)
  writeFormula(wb, sheet="Planilha3", x="=Planilha2!BJ2+Planilha2!BK2+Planilha2!BT2+Planilha2!BU2+Planilha2!BV2+Planilha2!BW2", startCol=18, startRow=2)
  
  # Fórmula 119: Total da dimensão 4
  writeData(wb, sheet = "Planilha3", x="sce1_sec_dim_4_total", startCol=19, startRow=1)
  writeFormula(wb, sheet="Planilha3", x="=SUM(Planilha2!BX2:BY2)", startCol=19, startRow=2)
  # Fórmula 120: Total da dimensão 5
  writeData(wb, sheet = "Planilha3", x="sce1_sec_dim_5_total", startCol=20, startRow=1)
  writeFormula(wb, sheet="Planilha3", x="=SUM(Planilha2!BZ2:CB2)", startCol=20, startRow=2)
  
  # Fórmula 121: Total Geral para Cenário Mínimo de Dispositivos e Treinamento Online 60h
  writeData(wb, sheet = "Planilha4", x="sce1_sec_total_geral_min_dev_online_60", startCol=30, startRow=1)
  writeFormula(wb, sheet="Planilha4", x="=Planilha3!L2+Planilha3!M2+Planilha3!P2+Planilha3!S2+Planilha3!T2", startCol=30, startRow=2)
  # Fórmula 122: Total Geral para Cenário Mínimo de Dispositivos e Treinamento Online 40h
  writeData(wb, sheet = "Planilha4", x="sce1_sec_total_geral_min_dev_online_40", startCol=31, startRow=1)
  writeFormula(wb, sheet="Planilha4", x="=Planilha3!L2+Planilha3!M2+Planilha3!Q2+Planilha3!S2+Planilha3!T2", startCol=31, startRow=2)
  # Fórmula 123: Total Geral para Cenário Mínimo de Dispositivos e Treinamento Presencial 40h
  writeData(wb, sheet = "Planilha4", x="sce1_sec_total_geral_min_dev_inperson_40", startCol=32, startRow=1)
  writeFormula(wb, sheet="Planilha4", x="=Planilha3!L2+Planilha3!M2+Planilha3!R2+Planilha3!S2+Planilha3!T2", startCol=32, startRow=2)
  # Fórmula 124: Total Geral para Cenário Moderado de Dispositivos e Treinamento Online 60h
  writeData(wb, sheet = "Planilha4", x="sce1_sec_total_geral_mild_dev_online_60", startCol=33, startRow=1)
  writeFormula(wb, sheet="Planilha4", x="=Planilha3!L2+Planilha3!N2+Planilha3!P2+Planilha3!S2+Planilha3!T2", startCol=33, startRow=2)
  # Fórmula 125: Total Geral para Cenário Moderado de Dispositivos e Treinamento Online 40h
  writeData(wb, sheet = "Planilha4", x="sce1_sec_total_geral_mild_dev_online_40", startCol=34, startRow=1)
  writeFormula(wb, sheet="Planilha4", x="=Planilha3!L2+Planilha3!N2+Planilha3!Q2+Planilha3!S2+Planilha3!T2", startCol=34, startRow=2)
  # Fórmula 126: Total Geral para Cenário Moderado de Dispositivos e Treinamento Presencial 40h
  writeData(wb, sheet = "Planilha4", x="sce1_sec_total_geral_mild_dev_inperson_40", startCol=35, startRow=1)
  writeFormula(wb, sheet="Planilha4", x="=Planilha3!L2+Planilha3!N2+Planilha3!R2+Planilha3!S2+Planilha3!T2", startCol=35, startRow=2)
  # Fórmula 127: Total Geral para Cenário Abrangente de Dispositivos e Treinamento Online 60h
  writeData(wb, sheet = "Planilha4", x="sce1_sec_total_geral_comp_dev_online_60", startCol=36, startRow=1)
  writeFormula(wb, sheet="Planilha4", x="=Planilha3!L2+Planilha3!O2+Planilha3!P2+Planilha3!S2+Planilha3!T2", startCol=36, startRow=2)
  # Fórmula 128: Total Geral para Cenário Abrangente de Dispositivos e Treinamento Online 40h
  writeData(wb, sheet = "Planilha4", x="sce1_sec_total_geral_comp_dev_online_40", startCol=37, startRow=1)
  writeFormula(wb, sheet="Planilha4", x="=Planilha3!L2+Planilha3!O2+Planilha3!Q2+Planilha3!S2+Planilha3!T2", startCol=37, startRow=2)
  # Fórmula 129: Total Geral para Cenário Abrangente de Dispositivos e Treinamento Presencial 40h
  writeData(wb, sheet = "Planilha4", x="sce1_sec_total_geral_comp_dev_inperson_40", startCol=38, startRow=1)
  writeFormula(wb, sheet="Planilha4", x="=Planilha3!L2+Planilha3!O2+Planilha3!R2+Planilha3!S2+Planilha3!T2", startCol=38, startRow=2)
  
  # Fórmula 130: Total Geral para Cenário Mínimo de Dispositivos e Treinamento Online 60h
  writeData(wb, sheet = "Planilha4", x="sce1_sec_total_por_aluno_min_dev_online_60", startCol=39, startRow=1)
  writeFormula(wb, sheet="Planilha4", x="=(Planilha3!L2+Planilha3!M2+Planilha3!P2+Planilha3!S2+Planilha3!T2)/Planilha1!D6", startCol=39, startRow=2)
  # Fórmula 131: Total Geral para Cenário Mínimo de Dispositivos e Treinamento Online 40h
  writeData(wb, sheet = "Planilha4", x="sce1_sec_total_por_aluno_min_dev_online_40", startCol=40, startRow=1)
  writeFormula(wb, sheet="Planilha4", x="=(Planilha3!L2+Planilha3!M2+Planilha3!Q2+Planilha3!S2+Planilha3!T2)/Planilha1!D6", startCol=40, startRow=2)
  # Fórmula 132: Total Geral para Cenário Mínimo de Dispositivos e Treinamento Presencial 40h
  writeData(wb, sheet = "Planilha4", x="sce1_sec_total_por_aluno_min_dev_inperson_40", startCol=41, startRow=1)
  writeFormula(wb, sheet="Planilha4", x="=(Planilha3!L2+Planilha3!M2+Planilha3!R2+Planilha3!S2+Planilha3!T2)/Planilha1!D6", startCol=41, startRow=2)
  
  # Fórmula 133: Total Geral para Cenário Moderado de Dispositivos e Treinamento Online 60h
  writeData(wb, sheet = "Planilha4", x="sce1_sec_total_por_aluno_mild_dev_online_60", startCol=42, startRow=1)
  writeFormula(wb, sheet="Planilha4", x="=(Planilha3!L2+Planilha3!N2+Planilha3!P2+Planilha3!S2+Planilha3!T2)/Planilha1!D6", startCol=42, startRow=2)
  # Fórmula 134: Total Geral para Cenário Moderado de Dispositivos e Treinamento Online 40h
  writeData(wb, sheet = "Planilha4", x="sce1_sec_total_por_aluno_mild_dev_online_40", startCol=43, startRow=1)
  writeFormula(wb, sheet="Planilha4", x="=(Planilha3!L2+Planilha3!N2+Planilha3!Q2+Planilha3!S2+Planilha3!T2)/Planilha1!D6", startCol=43, startRow=2)
  # Fórmula 135: Total Geral para Cenário Moderado de Dispositivos e Treinamento Presencial 40h
  writeData(wb, sheet = "Planilha4", x="sce1_sec_total_por_aluno_mild_dev_inperson_40", startCol=44, startRow=1)
  writeFormula(wb, sheet="Planilha4", x="=(Planilha3!L2+Planilha3!N2+Planilha3!R2+Planilha3!S2+Planilha3!T2)/Planilha1!D6", startCol=44, startRow=2)
  
  # Fórmula 136: Total Geral para Cenário Abrangente de Dispositivos e Treinamento Online 60h
  writeData(wb, sheet = "Planilha4", x="sce1_sec_total_por_aluno_comp_dev_online_60", startCol=45, startRow=1)
  writeFormula(wb, sheet="Planilha4", x="=(Planilha3!L2+Planilha3!O2+Planilha3!P2+Planilha3!S2+Planilha3!T2)/Planilha1!D6", startCol=45, startRow=2)
  # Fórmula 137: Total Geral para Cenário Abrangente de Dispositivos e Treinamento Online 40h
  writeData(wb, sheet = "Planilha4", x="sce1_sec_total_por_aluno_comp_dev_online_40", startCol=46, startRow=1)
  writeFormula(wb, sheet="Planilha4", x="=(Planilha3!L2+Planilha3!O2+Planilha3!Q2+Planilha3!S2+Planilha3!T2)/Planilha1!D6", startCol=46, startRow=2)
  # Fórmula 138: Total Geral para Cenário Abrangente de Dispositivos e Treinamento Presencial 40h
  writeData(wb, sheet = "Planilha4", x="sce1_sec_total_por_aluno_comp_dev_inperson_40", startCol=47, startRow=1)
  writeFormula(wb, sheet="Planilha4", x="=(Planilha3!L2+Planilha3!O2+Planilha3!R2+Planilha3!S2+Planilha3!T2)/Planilha1!D6", startCol=47, startRow=2)
  
  # Fórmula 139: Total Geral para Cenário Mínimo de Dispositivos e Treinamento Online 60h
  writeData(wb, sheet = "Planilha4", x="sce1_sec_total_por_escola_min_dev_online_60", startCol=48, startRow=1)
  writeFormula(wb, sheet="Planilha4", x="=(Planilha3!L2+Planilha3!M2+Planilha3!P2+Planilha3!S2+Planilha3!T2)/Planilha1!D9", startCol=48, startRow=2)
  # Fórmula 140: Total Geral para Cenário Mínimo de Dispositivos e Treinamento Online 40h
  writeData(wb, sheet = "Planilha4", x="sce1_sec_total_por_escola_min_dev_online_40", startCol=49, startRow=1)
  writeFormula(wb, sheet="Planilha4", x="=(Planilha3!L2+Planilha3!M2+Planilha3!Q2+Planilha3!S2+Planilha3!T2)/Planilha1!D9", startCol=49, startRow=2)
  # Fórmula 141: Total Geral para Cenário Mínimo de Dispositivos e Treinamento Presencial 40h
  writeData(wb, sheet = "Planilha4", x="sce1_sec_total_por_escola_min_dev_inperson_40", startCol=50, startRow=1)
  writeFormula(wb, sheet="Planilha4", x="=(Planilha3!L2+Planilha3!M2+Planilha3!R2+Planilha3!S2+Planilha3!T2)/Planilha1!D9", startCol=50, startRow=2)
  
  # Fórmula 142: Total Geral para Cenário Moderado de Dispositivos e Treinamento Online 60h
  writeData(wb, sheet = "Planilha4", x="sce1_sec_total_por_escola_mild_dev_online_60", startCol=51, startRow=1)
  writeFormula(wb, sheet="Planilha4", x="=(Planilha3!L2+Planilha3!N2+Planilha3!P2+Planilha3!S2+Planilha3!T2)/Planilha1!D9", startCol=51, startRow=2)
  # Fórmula 143: Total Geral para Cenário Moderado de Dispositivos e Treinamento Online 40h
  writeData(wb, sheet = "Planilha4", x="sce1_sec_total_por_escola_mild_dev_online_40", startCol=52, startRow=1)
  writeFormula(wb, sheet="Planilha4", x="=(Planilha3!L2+Planilha3!N2+Planilha3!Q2+Planilha3!S2+Planilha3!T2)/Planilha1!D9", startCol=52, startRow=2)
  # Fórmula 144: Total Geral para Cenário Moderado de Dispositivos e Treinamento Presencial 40h
  writeData(wb, sheet = "Planilha4", x="sce1_sec_total_por_escola_mild_dev_inperson_40", startCol=53, startRow=1)
  writeFormula(wb, sheet="Planilha4", x="=(Planilha3!L2+Planilha3!N2+Planilha3!R2+Planilha3!S2+Planilha3!T2)/Planilha1!D9", startCol=53, startRow=2)
  
  # Fórmula 145: Total Geral para Cenário Abrangente de Dispositivos e Treinamento Online 60h
  writeData(wb, sheet = "Planilha4", x="sce1_sec_total_por_escola_comp_dev_online_60", startCol=54, startRow=1)
  writeFormula(wb, sheet="Planilha4", x="=(Planilha3!L2+Planilha3!O2+Planilha3!P2+Planilha3!S2+Planilha3!T2)/Planilha1!D9", startCol=54, startRow=2)
  # Fórmula 146: Total Geral para Cenário Abrangente de Dispositivos e Treinamento Online 40h
  writeData(wb, sheet = "Planilha4", x="sce1_sec_total_por_escola_comp_dev_online_40", startCol=55, startRow=1)
  writeFormula(wb, sheet="Planilha4", x="=(Planilha3!L2+Planilha3!O2+Planilha3!Q2+Planilha3!S2+Planilha3!T2)/Planilha1!D9", startCol=55, startRow=2)
  # Fórmula 147: Total Geral para Cenário Abrangente de Dispositivos e Treinamento Presencial 40h
  writeData(wb, sheet = "Planilha4", x="sce1_sec_total_por_escola_comp_dev_inperson_40", startCol=56, startRow=1)
  writeFormula(wb, sheet="Planilha4", x="=(Planilha3!L2+Planilha3!O2+Planilha3!R2+Planilha3!S2+Planilha3!T2)/Planilha1!D9", startCol=56, startRow=2)
  
  
  # Fórmula 148: Range para Total Geral (Mínimo de Dispositivos e Treinamento Online 60h)
  writeData(wb, sheet = "Planilha4", x="sce1_sec_total_geral_min_dev_online_60_menos_25", startCol=57, startRow=1)
  writeFormula(wb, sheet="Planilha4", x="=(Planilha3!L2+Planilha3!M2+Planilha3!P2+Planilha3!S2+Planilha3!T2)*0.75", startCol=57, startRow=2)
  writeData(wb, sheet = "Planilha4", x="sce1_sec_total_geral_min_dev_online_60_mais_25", startCol=58, startRow=1)
  writeFormula(wb, sheet="Planilha4", x="=(Planilha3!L2+Planilha3!M2+Planilha3!P2+Planilha3!S2+Planilha3!T2)*1.25", startCol=58, startRow=2)
  
  # Fórmula 149: Range para Total Geral (Mínimo de Dispositivos e Treinamento Online 40h)
  writeData(wb, sheet = "Planilha4", x="sce1_sec_total_geral_min_dev_online_40_menos_25", startCol=59, startRow=1)
  writeFormula(wb, sheet="Planilha4", x="=(Planilha3!L2+Planilha3!M2+Planilha3!Q2+Planilha3!S2+Planilha3!T2)*0.75", startCol=59, startRow=2)
  writeData(wb, sheet = "Planilha4", x="sce1_sec_total_geral_min_dev_online_40_mais_25", startCol=60, startRow=1)
  writeFormula(wb, sheet="Planilha4", x="=(Planilha3!L2+Planilha3!M2+Planilha3!Q2+Planilha3!S2+Planilha3!T2)*1.25", startCol=60, startRow=2)
  
  # Fórmula 150: Range para Total Geral (Mínimo de Dispositivos e Treinamento Presencial 40h)
  writeData(wb, sheet = "Planilha4", x="sce1_sec_total_geral_min_dev_inperson_40_menos_25", startCol=61, startRow=1)
  writeFormula(wb, sheet="Planilha4", x="=(Planilha3!L2+Planilha3!M2+Planilha3!R2+Planilha3!S2+Planilha3!T2)*0.75", startCol=61, startRow=2)
  writeData(wb, sheet = "Planilha4", x="sce1_sec_total_geral_min_dev_inperson_40_mais_25", startCol=62, startRow=1)
  writeFormula(wb, sheet="Planilha4", x="=(Planilha3!L2+Planilha3!M2+Planilha3!R2+Planilha3!S2+Planilha3!T2)*1.25", startCol=62, startRow=2)
  
  # Fórmula 151: Range para Total Geral (Moderado de Dispositivos e Treinamento Online 60h)
  writeData(wb, sheet = "Planilha4", x="sce1_sec_total_geral_mild_dev_online_60_menos_25", startCol=63, startRow=1)
  writeFormula(wb, sheet="Planilha4", x="=(Planilha3!L2+Planilha3!N2+Planilha3!P2+Planilha3!S2+Planilha3!T2)*0.75", startCol=63, startRow=2)
  writeData(wb, sheet = "Planilha4", x="sce1_sec_total_geral_mild_dev_online_60_mais_25", startCol=64, startRow=1)
  writeFormula(wb, sheet="Planilha4", x="=(Planilha3!L2+Planilha3!N2+Planilha3!P2+Planilha3!S2+Planilha3!T2)*1.25", startCol=64, startRow=2)
  
  # Fórmula 152: Range para Total Geral (Moderado de Dispositivos e Treinamento Online 40h)
  writeData(wb, sheet = "Planilha4", x="sce1_sec_total_geral_mild_dev_online_40_menos_25", startCol=65, startRow=1)
  writeFormula(wb, sheet="Planilha4", x="=(Planilha3!L2+Planilha3!N2+Planilha3!Q2+Planilha3!S2+Planilha3!T2)*0.75", startCol=65, startRow=2)
  writeData(wb, sheet = "Planilha4", x="sce1_sec_total_geral_mild_dev_online_40_mais_25", startCol=66, startRow=1)
  writeFormula(wb, sheet="Planilha4", x="=(Planilha3!L2+Planilha3!N2+Planilha3!Q2+Planilha3!S2+Planilha3!T2)*1.25", startCol=66, startRow=2)
  
  # Fórmula 153: Range para Total Geral (Moderado de Dispositivos e Treinamento Presencial 40h)
  writeData(wb, sheet = "Planilha4", x="sce1_sec_total_geral_mild_dev_inperson_40_menos_25", startCol=67, startRow=1)
  writeFormula(wb, sheet="Planilha4", x="=(Planilha3!L2+Planilha3!N2+Planilha3!R2+Planilha3!S2+Planilha3!T2)*0.75", startCol=67, startRow=2)
  writeData(wb, sheet = "Planilha4", x="sce1_sec_total_geral_mild_dev_inperson_40_mais_25", startCol=68, startRow=1)
  writeFormula(wb, sheet="Planilha4", x="=(Planilha3!L2+Planilha3!N2+Planilha3!R2+Planilha3!S2+Planilha3!T2)*1.25", startCol=68, startRow=2)
  
  # Fórmula 154: Range para Total Geral (Abrangente de Dispositivos e Treinamento Online 60h)
  writeData(wb, sheet = "Planilha4", x="sce1_sec_total_geral_comp_dev_online_60_menos_25", startCol=69, startRow=1)
  writeFormula(wb, sheet="Planilha4", x="=(Planilha3!L2+Planilha3!O2+Planilha3!P2+Planilha3!S2+Planilha3!T2)*0.75", startCol=69, startRow=2)
  writeData(wb, sheet = "Planilha4", x="sce1_sec_total_geral_comp_dev_online_60_mais_25", startCol=70, startRow=1)
  writeFormula(wb, sheet="Planilha4", x="=(Planilha3!L2+Planilha3!O2+Planilha3!P2+Planilha3!S2+Planilha3!T2)*1.25", startCol=70, startRow=2)
  
  # Fórmula 155: Range para Total Geral (Abrangente de Dispositivos e Treinamento Online 40h)
  writeData(wb, sheet = "Planilha4", x="sce1_sec_total_geral_comp_dev_online_40_menos_25", startCol=71, startRow=1)
  writeFormula(wb, sheet="Planilha4", x="=(Planilha3!L2+Planilha3!O2+Planilha3!Q2+Planilha3!S2+Planilha3!T2)*0.75", startCol=71, startRow=2)
  writeData(wb, sheet = "Planilha4", x="sce1_sec_total_geral_comp_dev_online_40_mais_25", startCol=72, startRow=1)
  writeFormula(wb, sheet="Planilha4", x="=(Planilha3!L2+Planilha3!O2+Planilha3!Q2+Planilha3!S2+Planilha3!T2)*1.25", startCol=72, startRow=2)
  
  # Fórmula 156: Range para Total Geral (Abrangente de Dispositivos e Treinamento Presencial 40h)
  writeData(wb, sheet = "Planilha4", x="sce1_sec_total_geral_comp_dev_inperson_40_menos_25", startCol=73, startRow=1)
  writeFormula(wb, sheet="Planilha4", x="=(Planilha3!L2+Planilha3!O2+Planilha3!R2+Planilha3!S2+Planilha3!T2)*0.75", startCol=73, startRow=2)
  writeData(wb, sheet = "Planilha4", x="sce1_sec_total_geral_comp_dev_inperson_40_mais_25", startCol=74, startRow=1)
  writeFormula(wb, sheet="Planilha4", x="=(Planilha3!L2+Planilha3!O2+Planilha3!R2+Planilha3!S2+Planilha3!T2)*1.25", startCol=74, startRow=2)
  
  #---------- CENÁRIO 2 ----------
  
  # Fórmula 157: Dimensão 1 - Fiber expansion
  writeData(wb, sheet = "Planilha2", x="sce2_rur_dim_1_price_fiber_exp", startCol=81, startRow=1)
  writeFormula(wb, sheet="Planilha2", x="=MIN(Planilha1!D3-MIN(Planilha1!D3,(Planilha1!D15*Planilha1!D2)),Planilha1!D16*Planilha1!D2)*Planilha1!D20*Planilha1!D46", startCol=81, startRow=2)
  # Fórmula 158: Dimensão 1 - Satellite service
  writeData(wb, sheet = "Planilha2", x="sce2_rur_dim_1_price_satellite_service", startCol=82, startRow=1)
  writeFormula(wb, sheet="Planilha2", x="=MIN(Planilha1!D3,Planilha1!D15*Planilha1!D2)*Planilha1!D18*Planilha1!D45", startCol=82, startRow=2)
  # Fórmula 159: Dimensão 1 - Fiber link
  writeData(wb, sheet = "Planilha2", x="sce2_rur_dim_1_price_fiber_link", startCol=83, startRow=1)
  writeFormula(wb, sheet="Planilha2", x="=(Planilha1!D3-MIN(Planilha1!D3,Planilha1!D15))*(Planilha1!D12)*(Planilha1!D19/200)*Planilha1!D45", startCol=83, startRow=2)
  # Fórmula 160: Dimensão 1 - Access point
  writeData(wb, sheet = "Planilha2", x="sce2_rur_dim_1_price_access_point", startCol=84, startRow=1)
  writeFormula(wb, sheet="Planilha2", x="=((Planilha1!D5/25)+Planilha1!D8*3)+((Planilha1!D6/35)+Planilha1!D9*3)*(Planilha1!D3/Planilha1!D2)*Planilha1!D47*Planilha1!D21", startCol=84, startRow=2)
  # Fórmula 161: Dimensão 1 - Firewall
  writeData(wb, sheet = "Planilha2", x="sce2_rur_dim_1_price_firewall", startCol=85, startRow=1)
  writeFormula(wb, sheet="Planilha2", x="=Planilha1!D3*Planilha1!D48*Planilha1!D22", startCol=85, startRow=2)
  # Fórmula 162: Dimensão 1 - Nobreak
  writeData(wb, sheet = "Planilha2", x="sce2_rur_dim_1_price_nobreak", startCol=86, startRow=1)
  writeFormula(wb, sheet="Planilha2", x="=Planilha1!D3*Planilha1!D49*Planilha1!D23", startCol=86, startRow=2)
  # Fórmula 163: Dimensão 1 - Switch
  writeData(wb, sheet = "Planilha2", x="sce2_rur_dim_1_price_switch", startCol=87, startRow=1)
  writeFormula(wb, sheet="Planilha2", x="=Planilha1!D3*Planilha1!D50*Planilha1!D24", startCol=87, startRow=2)
  # Fórmula 164: Dimensão 1 - Rack 6U/8U
  writeData(wb, sheet = "Planilha2", x="sce2_rur_dim_1_price_rack", startCol=88, startRow=1)
  writeFormula(wb, sheet="Planilha2", x="=Planilha1!D3*Planilha1!D51*Planilha1!D25", startCol=88, startRow=2)
  
  # Fórmula 165: Dimensão 2 - Device per students (Min Scenario)
  writeData(wb, sheet = "Planilha2", x="sce2_rur_dim_2_price_device_student_min", startCol=89, startRow=1)
  writeFormula(wb, sheet="Planilha2", x="=((Planilha1!D5*(Planilha1!D3/Planilha1!D2))*Planilha1!D52*Planilha1!D30)+((Planilha1!D6*(Planilha1!D3/Planilha1!D2))*Planilha1!D53*Planilha1!D32)", startCol=89, startRow=2)
  # Fórmula 166: Dimensão 2 - Device per students (Mild Scenario)
  writeData(wb, sheet = "Planilha2", x="sce2_rur_dim_2_price_device_student_mild", startCol=90, startRow=1)
  writeFormula(wb, sheet="Planilha2", x="=((Planilha1!D5*(Planilha1!D3/Planilha1!D2))*Planilha1!D54*Planilha1!D30)+((Planilha1!D6*(Planilha1!D3/Planilha1!D2))*Planilha1!D55*Planilha1!D32)", startCol=90, startRow=2)
  # Fórmula 167: Dimensão 2 - Device per students (Comprehensive Scenario)
  writeData(wb, sheet = "Planilha2", x="sce2_rur_dim_2_price_device_student_comprehensive", startCol=91, startRow=1)
  writeFormula(wb, sheet="Planilha2", x="=((Planilha1!D5*(Planilha1!D3/Planilha1!D2))*Planilha1!D56*Planilha1!D30)+((Planilha1!D6*(Planilha1!D3/Planilha1!D2))*Planilha1!D57*Planilha1!D32)", startCol=91, startRow=2)
  # Fórmula 168: Dimensão 2 - Devices per teacher 
  writeData(wb, sheet = "Planilha2", x="sce2_rur_dim_2_price_device_teacher", startCol=92, startRow=1)
  writeFormula(wb, sheet="Planilha2", x="=(Planilha1!D7*(Planilha1!D3/Planilha1!D2))*Planilha1!D58*Planilha1!D31", startCol=92, startRow=2)
  # Fórmula 169: Dimensão 2 - Devices per school 
  writeData(wb, sheet = "Planilha2", x="sce2_rur_dim_2_price_device_school", startCol=93, startRow=1)
  writeFormula(wb, sheet="Planilha2", x="=Planilha1!D3*Planilha1!D59*Planilha1!D29", startCol=93, startRow=2)
  # Fórmula 160: Dimensão 2 - Charging cart (Min Scenario)
  writeData(wb, sheet = "Planilha2", x="sce2_rur_dim_2_price_charging_cart_min", startCol=94, startRow=1)
  writeFormula(wb, sheet="Planilha2", x="=(((Planilha1!D5*(Planilha1!D3/Planilha1!D2))*Planilha1!D52)+((Planilha1!D6*(Planilha1!D3/Planilha1!D2))*Planilha1!D53))/30*Planilha1!D28", startCol=94, startRow=2)
  # Fórmula 161: Dimensão 2 - Charging cart (Mild Scenario)
  writeData(wb, sheet = "Planilha2", x="sce2_rur_dim_2_price_charging_cart_mild", startCol=95, startRow=1)
  writeFormula(wb, sheet="Planilha2", x="=(((Planilha1!D5*(Planilha1!D3/Planilha1!D2))*Planilha1!D54)+((Planilha1!D6*(Planilha1!D3/Planilha1!D2))*Planilha1!D55))/30*Planilha1!D28", startCol=95, startRow=2)
  # Fórmula 162: Dimensão 2 - Charging cart (Comprehensive Scenario)
  writeData(wb, sheet = "Planilha2", x="sce2_rur_dim_2_price_charging_cart_comprehensive", startCol=96, startRow=1)
  writeFormula(wb, sheet="Planilha2", x="=(((Planilha1!D5*(Planilha1!D3/Planilha1!D2))*Planilha1!D56)+((Planilha1!D6*(Planilha1!D3/Planilha1!D2))*Planilha1!D57))/30*Planilha1!D28", startCol=96, startRow=2)
  
  # Fórmula 163: Dimensão 2 - Multimedia projector
  writeData(wb, sheet = "Planilha2", x="sce2_rur_dim_2_price_multimedia_projector", startCol=97, startRow=1)
  writeFormula(wb, sheet="Planilha2", x="=Planilha1!D3*Planilha1!D60*Planilha1!D26", startCol=97, startRow=2)
  # Fórmula 164: Dimensão 2 - Headphones (Min Scenario)
  writeData(wb, sheet = "Planilha2", x="sce2_rur_dim_2_price_headphones_min_scenario", startCol=98, startRow=1)
  writeFormula(wb, sheet="Planilha2", x="=(((Planilha1!D5*(Planilha1!D3/Planilha1!D2))*Planilha1!D52)+((Planilha1!D6*(Planilha1!D3/Planilha1!D2))*Planilha1!D53))*Planilha1!D27", startCol=98, startRow=2)
  # Fórmula 165: Dimensão 2 - Headphones (Mild Scenario)
  writeData(wb, sheet = "Planilha2", x="sce2_rur_dim_2_price_headphones_mild_scenario", startCol=99, startRow=1)
  writeFormula(wb, sheet="Planilha2", x="=(((Planilha1!D5*(Planilha1!D3/Planilha1!D2))*Planilha1!D54)+((Planilha1!D6*(Planilha1!D3/Planilha1!D2))*Planilha1!D55))*Planilha1!D27", startCol=99, startRow=2)
  # Fórmula 166: Dimensão 2 - Headphones (Comprehensive Scenario)
  writeData(wb, sheet = "Planilha2", x="sce2_rur_dim_2_price_headphones_comprehensive", startCol=100, startRow=1)
  writeFormula(wb, sheet="Planilha2", x="=(((Planilha1!D5*(Planilha1!D3/Planilha1!D2))*Planilha1!D56)+((Planilha1!D6*(Planilha1!D3/Planilha1!D2))*Planilha1!D57))*Planilha1!D27", startCol=100, startRow=2)
  
  # Fórmula 167: Dimensão 3 - Diagnostic Tool
  writeData(wb, sheet = "Planilha2", x="sce2_rur_dim_3_price_diagnostic_tool", startCol=101, startRow=1)
  writeFormula(wb, sheet="Planilha2", x="=Planilha1!D38", startCol=101, startRow=2)
  # Fórmula 168: Dimensão 3 - LMS Platform
  writeData(wb, sheet = "Planilha2", x="sce2_rur_dim_3_price_lms_platform", startCol=102, startRow=1)
  writeFormula(wb, sheet="Planilha2", x="=Planilha1!D39", startCol=102, startRow=2)
  
  # Fórmula 169: Dimensão 3 - Content Production (Scenario online-60)
  writeData(wb, sheet = "Planilha2", x="sce2_rur_dim_3_price_content_production_scenario_online60", startCol=103, startRow=1)
  writeFormula(wb, sheet="Planilha2", x="=Planilha1!D40", startCol=103, startRow=2)
  # Fórmula 170: Dimensão 3 - Specialist Trainers (Scenario online-60)
  writeData(wb, sheet = "Planilha2", x="sce2_rur_dim_3_price_specialist_trainers_scenario_online60", startCol=104, startRow=1)
  writeFormula(wb, sheet="Planilha2", x="=Planilha1!D42*((Planilha1!D37*(Planilha1!D3/Planilha1!D2))/Planilha1!D61)", startCol=104, startRow=2)
  # Fórmula 171: Dimensão 3 - Course for Trainers (Scenario online-60)
  writeData(wb, sheet = "Planilha2", x="sce2_rur_dim_3_price_course_for_trainers_scenario_online60", startCol=105, startRow=1)
  writeFormula(wb, sheet="Planilha2", x="=Planilha1!D42*1.5*((Planilha1!D37*(Planilha1!D3/Planilha1!D2))/Planilha1!D61)/Planilha1!D61", startCol=105, startRow=2)
  # Fórmula 172: Dimensão 3 - Training design (Scenario online-60)
  writeData(wb, sheet = "Planilha2", x="sce2_rur_dim_3_price_training_design_scenario_online60", startCol=106, startRow=1)
  writeFormula(wb, sheet="Planilha2", x="=Planilha1!D41", startCol=106, startRow=2)
  
  # Fórmula 173: Dimensão 3 - Content Production (Scenario online-40)
  writeData(wb, sheet = "Planilha2", x="sce2_rur_dim_3_price_content_production_scenario_online40", startCol=107, startRow=1)
  writeFormula(wb, sheet="Planilha2", x="=Planilha1!D44*Planilha1!D40", startCol=107, startRow=2)
  # Fórmula 174: Dimensão 3 - Specialist Trainers (Scenario online-40)
  writeData(wb, sheet = "Planilha2", x="sce2_rur_dim_3_price_specialist_trainers_scenario_online40", startCol=108, startRow=1)
  writeFormula(wb, sheet="Planilha2", x="=Planilha1!D44*(Planilha1!D42*((Planilha1!D37*(Planilha1!D3/Planilha1!D2))/Planilha1!D61))", startCol=108, startRow=2)
  # Fórmula 175: Dimensão 3 - Course for Trainers (Scenario online-40)
  writeData(wb, sheet = "Planilha2", x="sce2_rur_dim_3_price_course_for_trainers_scenario_online40", startCol=109, startRow=1)
  writeFormula(wb, sheet="Planilha2", x="=Planilha1!D44*(Planilha1!D42*1.5*((Planilha1!D37*(Planilha1!D3/Planilha1!D2))/Planilha1!D61)/Planilha1!D61)", startCol=109, startRow=2)
  # Fórmula 176: Dimensão 3 - Training design (Scenario online-40)
  writeData(wb, sheet = "Planilha2", x="sce2_rur_dim_3_price_training_design_scenario_online40", startCol=110, startRow=1)
  writeFormula(wb, sheet="Planilha2", x="=Planilha1!D44*Planilha1!D41", startCol=110, startRow=2)
  
  # Fórmula 177: Dimensão 3 - Content Production (Scenario online-40)
  writeData(wb, sheet = "Planilha2", x="sce2_rur_dim_3_price_content_production_scenario_inperson40", startCol=111, startRow=1)
  writeFormula(wb, sheet="Planilha2", x="=Planilha1!D44*Planilha1!D40", startCol=111, startRow=2)
  # Fórmula 178: Dimensão 3 - Specialist Trainers (Scenario online-40)
  writeData(wb, sheet = "Planilha2", x="sce2_rur_dim_3_price_specialist_trainers_scenario_inperson40", startCol=112, startRow=1)
  writeFormula(wb, sheet="Planilha2", x="=Planilha1!D43*Planilha1!D44*(Planilha1!D42*((Planilha1!D37*(Planilha1!D3/Planilha1!D2))/Planilha1!D61))", startCol=112, startRow=2)
  # Fórmula 179: Dimensão 3 - Course for Trainers (Scenario online-40)
  writeData(wb, sheet = "Planilha2", x="sce2_rur_dim_3_price_course_for_trainers_scenario_inperson40", startCol=113, startRow=1)
  writeFormula(wb, sheet="Planilha2", x="=Planilha1!D43*Planilha1!D44*(Planilha1!D42*1.5*((Planilha1!D37*(Planilha1!D3/Planilha1!D2))/Planilha1!D61)/Planilha1!D61)", startCol=113, startRow=2)
  # Fórmula 180: Dimensão 3 - Training design (Scenario online-40)
  writeData(wb, sheet = "Planilha2", x="sce2_rur_dim_3_price_training_design_scenario_inperson40", startCol=114, startRow=1)
  writeFormula(wb, sheet="Planilha2", x="=Planilha1!D44*Planilha1!D41", startCol=114, startRow=2)
  
  # Fórmula 181: Dimensão 4 - Teaching and learning platform
  writeData(wb, sheet = "Planilha2", x="sce2_rur_dim_4_price_teaching_learning_platform", startCol=115, startRow=1)
  writeFormula(wb, sheet="Planilha2", x="=((Planilha1!D6+Planilha1!D5)*(Planilha1!D3/Planilha1!D2))*Planilha1!D33*Planilha1!D62*(Planilha1!D45/12)", startCol=115, startRow=2)
  # Fórmula 182: Dimensão 4 - Management platform
  writeData(wb, sheet = "Planilha2", x="sce2_rur_dim_4_price_management_platform", startCol=116, startRow=1)
  writeFormula(wb, sheet="Planilha2", x="=Planilha1!D3*Planilha1!D34*Planilha1!D63*(Planilha1!D45/12)", startCol=116, startRow=2)
  
  # Fórmula 183: Dimensão 5 - Central team
  writeData(wb, sheet = "Planilha2", x="sce2_rur_dim_5_price_central_team", startCol=117, startRow=1)
  writeFormula(wb, sheet="Planilha2", x="=Planilha1!D35*((Planilha1!D64+Planilha1!D65)/2)*Planilha1!D45", startCol=117, startRow=2)
  # Fórmula 184: Dimensão 5 - Regional team
  writeData(wb, sheet = "Planilha2", x="sce2_rur_dim_5_price_regional_team", startCol=118, startRow=1)
  writeFormula(wb, sheet="Planilha2", x="=Planilha1!D66*Planilha1!D3*Planilha1!D36*Planilha1!D45", startCol=118, startRow=2)
  # Fórmula 185: Dimensão 5 - Local team
  writeData(wb, sheet = "Planilha2", x="sce2_rur_dim_5_price_local_team", startCol=119, startRow=1)
  writeFormula(wb, sheet="Planilha2", x="=Planilha1!D68*Planilha1!D3*Planilha1!D36*Planilha1!D45", startCol=119, startRow=2)
  
  # Fórmula 186: Total da dimensão 1
  writeData(wb, sheet = "Planilha3", x="sce2_rur_dim_1_total", startCol=21, startRow=1)
  writeFormula(wb, sheet="Planilha3", x="=SUM(Planilha2!CC2:CJ2)", startCol=21, startRow=2)
  
  # Fórmula 187: Total da dimensão 2 (min)
  writeData(wb, sheet = "Planilha3", x="sce2_rur_dim_2_total_min", startCol=22, startRow=1)
  writeFormula(wb, sheet="Planilha3", x="=Planilha2!CK2+Planilha2!CN2+Planilha2!CO2+Planilha2!CP2+Planilha2!CS2+Planilha2!CT2", startCol=22, startRow=2)
  # Fórmula 188: Total da dimensão 2 (mild)
  writeData(wb, sheet = "Planilha3", x="sce2_rur_dim_2_total_mild", startCol=23, startRow=1)
  writeFormula(wb, sheet="Planilha3", x="=Planilha2!CL2+Planilha2!CN2+Planilha2!CO2+Planilha2!CQ2+Planilha2!CS2+Planilha2!CU2", startCol=23, startRow=2)
  # Fórmula 189: Total da dimensão 2 (comprehensive)
  writeData(wb, sheet = "Planilha3", x="sce2_rur_dim_2_total_comprehensive", startCol=24, startRow=1)
  writeFormula(wb, sheet="Planilha3", x="=Planilha2!CM2+Planilha2!CN2+Planilha2!CO2+Planilha2!CR2+Planilha2!CS2+Planilha2!CV2", startCol=24, startRow=2)
  
  # Fórmula 190: Total da dimensão 3
  writeData(wb, sheet = "Planilha3", x="sce2_rur_dim_3_total_online_60", startCol=25, startRow=1)
  writeFormula(wb, sheet="Planilha3", x="=Planilha2!CW2+Planilha2!CX2+Planilha2!CY2+Planilha2!CZ2+Planilha2!DA2+Planilha2!DB2", startCol=25, startRow=2)
  # Fórmula 191: Total da dimensão 3
  writeData(wb, sheet = "Planilha3", x="sce2_rur_dim_3_total_online_40", startCol=26, startRow=1)
  writeFormula(wb, sheet="Planilha3", x="=Planilha2!CW2+Planilha2!CX2+Planilha2!DC2+Planilha2!DD2+Planilha2!DE2+Planilha2!DF2", startCol=26, startRow=2)
  # Fórmula 192: Total da dimensão 3
  writeData(wb, sheet = "Planilha3", x="sce2_rur_dim_3_total_inperson_40", startCol=27, startRow=1)
  writeFormula(wb, sheet="Planilha3", x="=Planilha2!CW2+Planilha2!CX2+Planilha2!DG2+Planilha2!DH2+Planilha2!DI2+Planilha2!DJ2", startCol=27, startRow=2)
  
  # Fórmula 193: Total da dimensão 4
  writeData(wb, sheet = "Planilha3", x="sce2_rur_dim_4_total", startCol=28, startRow=1)
  writeFormula(wb, sheet="Planilha3", x="=Planilha2!DK2+Planilha2!DL2", startCol=28, startRow=2)
  # Fórmula 194: Total da dimensão 5
  writeData(wb, sheet = "Planilha3", x="sce2_rur_dim_5_total", startCol=29, startRow=1)
  writeFormula(wb, sheet="Planilha3", x="=SUM(Planilha2!DM2:DO2)", startCol=29, startRow=2)
  
  # Fórmula 195: Total Geral para Cenário Mínimo de Dispositivos e Treinamento Online 60h
  writeData(wb, sheet = "Planilha4", x="sce2_rur_total_geral_min_dev_online_60", startCol=75, startRow=1)
  writeFormula(wb, sheet="Planilha4", x="=Planilha3!U2+Planilha3!V2+Planilha3!Y2+Planilha3!AB2+Planilha3!AC2", startCol=75, startRow=2)
  # Fórmula 196: Total Geral para Cenário Mínimo de Dispositivos e Treinamento Online 40h
  writeData(wb, sheet = "Planilha4", x="sce2_rur_total_geral_min_dev_online_40", startCol=76, startRow=1)
  writeFormula(wb, sheet="Planilha4", x="=Planilha3!U2+Planilha3!V2+Planilha3!Z2+Planilha3!AB2+Planilha3!AC2", startCol=76, startRow=2)
  # Fórmula 197: Total Geral para Cenário Mínimo de Dispositivos e Treinamento Presencial 40h
  writeData(wb, sheet = "Planilha4", x="sce2_rur_total_geral_min_dev_inperson_40", startCol=77, startRow=1)
  writeFormula(wb, sheet="Planilha4", x="=Planilha3!U2+Planilha3!V2+Planilha3!AA2+Planilha3!AB2+Planilha3!AC2", startCol=77, startRow=2)
  # Fórmula 198: Total Geral para Cenário Moderado de Dispositivos e Treinamento Online 60h
  writeData(wb, sheet = "Planilha4", x="sce2_rur_total_geral_mild_dev_online_60", startCol=78, startRow=1)
  writeFormula(wb, sheet="Planilha4", x="=Planilha3!U2+Planilha3!W2+Planilha3!Y2+Planilha3!AB2+Planilha3!AC2", startCol=78, startRow=2)
  # Fórmula 199: Total Geral para Cenário Moderado de Dispositivos e Treinamento Online 40h
  writeData(wb, sheet = "Planilha4", x="sce2_rur_total_geral_mild_dev_online_40", startCol=79, startRow=1)
  writeFormula(wb, sheet="Planilha4", x="=Planilha3!U2+Planilha3!W2+Planilha3!Z2+Planilha3!AB2+Planilha3!AC2", startCol=79, startRow=2)
  # Fórmula 200: Total Geral para Cenário Moderado de Dispositivos e Treinamento Presencial 40h
  writeData(wb, sheet = "Planilha4", x="sce2_rur_total_geral_mild_dev_inperson_40", startCol=80, startRow=1)
  writeFormula(wb, sheet="Planilha4", x="=Planilha3!U2+Planilha3!W2+Planilha3!AA2+Planilha3!AB2+Planilha3!AC2", startCol=80, startRow=2)
  # Fórmula 201: Total Geral para Cenário Abrangente de Dispositivos e Treinamento Online 60h
  writeData(wb, sheet = "Planilha4", x="sce2_rur_total_geral_comp_dev_online_60", startCol=81, startRow=1)
  writeFormula(wb, sheet="Planilha4", x="=Planilha3!U2+Planilha3!X2+Planilha3!Y2+Planilha3!AB2+Planilha3!AC2", startCol=81, startRow=2)
  # Fórmula 202: Total Geral para Cenário Abrangente de Dispositivos e Treinamento Online 40h
  writeData(wb, sheet = "Planilha4", x="sce2_rur_total_geral_comp_dev_online_40", startCol=82, startRow=1)
  writeFormula(wb, sheet="Planilha4", x="=Planilha3!U2+Planilha3!X2+Planilha3!Z2+Planilha3!AB2+Planilha3!AC2", startCol=82, startRow=2)
  # Fórmula 203: Total Geral para Cenário Abrangente de Dispositivos e Treinamento Presencial 40h
  writeData(wb, sheet = "Planilha4", x="sce2_rur_total_geral_comp_dev_inperson_40", startCol=83, startRow=1)
  writeFormula(wb, sheet="Planilha4", x="=Planilha3!U2+Planilha3!X2+Planilha3!AA2+Planilha3!AB2+Planilha3!AC2", startCol=83, startRow=2)
  
  
  # Fórmula 204: Total Geral para Cenário Mínimo de Dispositivos e Treinamento Online 60h
  writeData(wb, sheet = "Planilha4", x="sce2_rur_total_por_aluno_min_dev_online_60", startCol=84, startRow=1)
  writeFormula(wb, sheet="Planilha4", x="=(Planilha3!U2+Planilha3!V2+Planilha3!Y2+Planilha3!AB2+Planilha3!AC2)/Planilha1!D4", startCol=84, startRow=2)
  # Fórmula 205: Total Geral para Cenário Mínimo de Dispositivos e Treinamento Online 40h
  writeData(wb, sheet = "Planilha4", x="sce2_rur_total_por_aluno_min_dev_online_40", startCol=85, startRow=1)
  writeFormula(wb, sheet="Planilha4", x="=(Planilha3!U2+Planilha3!V2+Planilha3!Z2+Planilha3!AB2+Planilha3!AC2)/Planilha1!D4", startCol=85, startRow=2)
  # Fórmula 206: Total Geral para Cenário Mínimo de Dispositivos e Treinamento Presencial 40h
  writeData(wb, sheet = "Planilha4", x="sce2_rur_total_por_aluno_min_dev_inperson_40", startCol=86, startRow=1)
  writeFormula(wb, sheet="Planilha4", x="=(Planilha3!U2+Planilha3!V2+Planilha3!AA2+Planilha3!AB2+Planilha3!AC2)/Planilha1!D4", startCol=86, startRow=2)
  # Fórmula 207: Total Geral para Cenário Moderado de Dispositivos e Treinamento Online 60h
  writeData(wb, sheet = "Planilha4", x="sce2_rur_total_por_aluno_mild_dev_online_60", startCol=87, startRow=1)
  writeFormula(wb, sheet="Planilha4", x="=(Planilha3!U2+Planilha3!W2+Planilha3!Y2+Planilha3!AB2+Planilha3!AC2)/Planilha1!D4", startCol=87, startRow=2)
  # Fórmula 208: Total Geral para Cenário Moderado de Dispositivos e Treinamento Online 40h
  writeData(wb, sheet = "Planilha4", x="sce2_rur_total_por_aluno_mild_dev_online_40", startCol=88, startRow=1)
  writeFormula(wb, sheet="Planilha4", x="=(Planilha3!U2+Planilha3!W2+Planilha3!Z2+Planilha3!AB2+Planilha3!AC2)/Planilha1!D4", startCol=88, startRow=2)
  # Fórmula 209: Total Geral para Cenário Moderado de Dispositivos e Treinamento Presencial 40h
  writeData(wb, sheet = "Planilha4", x="sce2_rur_total_por_aluno_mild_dev_inperson_40", startCol=89, startRow=1)
  writeFormula(wb, sheet="Planilha4", x="=(Planilha3!U2+Planilha3!W2+Planilha3!AA2+Planilha3!AB2+Planilha3!AC2)/Planilha1!D4", startCol=89, startRow=2)
  # Fórmula 136: Total Geral para Cenário Abrangente de Dispositivos e Treinamento Online 60h
  writeData(wb, sheet = "Planilha4", x="sce2_rur_total_por_aluno_comp_dev_online_60", startCol=90, startRow=1)
  writeFormula(wb, sheet="Planilha4", x="=(Planilha3!U2+Planilha3!X2+Planilha3!Y2+Planilha3!AB2+Planilha3!AC2)/Planilha1!D4", startCol=90, startRow=2)
  # Fórmula 137: Total Geral para Cenário Abrangente de Dispositivos e Treinamento Online 40h
  writeData(wb, sheet = "Planilha4", x="sce2_rur_total_por_aluno_comp_dev_online_40", startCol=91, startRow=1)
  writeFormula(wb, sheet="Planilha4", x="=(Planilha3!U2+Planilha3!X2+Planilha3!Z2+Planilha3!AB2+Planilha3!AC2)/Planilha1!D4", startCol=91, startRow=2)
  # Fórmula 138: Total Geral para Cenário Abrangente de Dispositivos e Treinamento Presencial 40h
  writeData(wb, sheet = "Planilha4", x="sce2_rur_total_por_aluno_comp_dev_inperson_40", startCol=92, startRow=1)
  writeFormula(wb, sheet="Planilha4", x="=(Planilha3!U2+Planilha3!X2+Planilha3!AA2+Planilha3!AB2+Planilha3!AC2)/Planilha1!D4", startCol=92, startRow=2)
  
  # Fórmula 139: Total Geral para Cenário Mínimo de Dispositivos e Treinamento Online 60h
  writeData(wb, sheet = "Planilha4", x="sce2_rur_total_por_escola_min_dev_online_60", startCol=93, startRow=1)
  writeFormula(wb, sheet="Planilha4", x="=(Planilha3!U2+Planilha3!V2+Planilha3!Y2+Planilha3!AB2+Planilha3!AC2)/Planilha1!D3", startCol=93, startRow=2)
  # Fórmula 140: Total Geral para Cenário Mínimo de Dispositivos e Treinamento Online 40h
  writeData(wb, sheet = "Planilha4", x="sce2_rur_total_por_escola_min_dev_online_40", startCol=94, startRow=1)
  writeFormula(wb, sheet="Planilha4", x="=(Planilha3!U2+Planilha3!V2+Planilha3!Z2+Planilha3!AB2+Planilha3!AC2)/Planilha1!D3", startCol=94, startRow=2)
  # Fórmula 141: Total Geral para Cenário Mínimo de Dispositivos e Treinamento Presencial 40h
  writeData(wb, sheet = "Planilha4", x="sce2_rur_total_por_escola_min_dev_inperson_40", startCol=95, startRow=1)
  writeFormula(wb, sheet="Planilha4", x="=(Planilha3!U2+Planilha3!V2+Planilha3!AA2+Planilha3!AB2+Planilha3!AC2)/Planilha1!D3", startCol=95, startRow=2)
  
  # Fórmula 142: Total Geral para Cenário Moderado de Dispositivos e Treinamento Online 60h
  writeData(wb, sheet = "Planilha4", x="sce2_rur_total_por_escola_mild_dev_online_60", startCol=96, startRow=1)
  writeFormula(wb, sheet="Planilha4", x="=(Planilha3!U2+Planilha3!W2+Planilha3!Y2+Planilha3!AB2+Planilha3!AC2)/Planilha1!D3", startCol=96, startRow=2)
  # Fórmula 143: Total Geral para Cenário Moderado de Dispositivos e Treinamento Online 40h
  writeData(wb, sheet = "Planilha4", x="sce2_rur_total_por_escola_mild_dev_online_40", startCol=97, startRow=1)
  writeFormula(wb, sheet="Planilha4", x="=(Planilha3!U2+Planilha3!W2+Planilha3!Z2+Planilha3!AB2+Planilha3!AC2)/Planilha1!D3", startCol=97, startRow=2)
  # Fórmula 144: Total Geral para Cenário Moderado de Dispositivos e Treinamento Presencial 40h
  writeData(wb, sheet = "Planilha4", x="sce2_rur_total_por_escola_mild_dev_inperson_40", startCol=98, startRow=1)
  writeFormula(wb, sheet="Planilha4", x="=(Planilha3!U2+Planilha3!W2+Planilha3!AA2+Planilha3!AB2+Planilha3!AC2)/Planilha1!D3", startCol=98, startRow=2)
  
  # Fórmula 145: Total Geral para Cenário Abrangente de Dispositivos e Treinamento Online 60h
  writeData(wb, sheet = "Planilha4", x="sce2_rur_total_por_escola_comp_dev_online_60", startCol=99, startRow=1)
  writeFormula(wb, sheet="Planilha4", x="=(Planilha3!U2+Planilha3!X2+Planilha3!Y2+Planilha3!AB2+Planilha3!AC2)/Planilha1!D3", startCol=99, startRow=2)
  # Fórmula 146: Total Geral para Cenário Abrangente de Dispositivos e Treinamento Online 40h
  writeData(wb, sheet = "Planilha4", x="sce2_rur_total_por_escola_comp_dev_online_40", startCol=100, startRow=1)
  writeFormula(wb, sheet="Planilha4", x="=(Planilha3!U2+Planilha3!X2+Planilha3!Z2+Planilha3!AB2+Planilha3!AC2)/Planilha1!D3", startCol=100, startRow=2)
  # Fórmula 147: Total Geral para Cenário Abrangente de Dispositivos e Treinamento Presencial 40h
  writeData(wb, sheet = "Planilha4", x="sce2_rur_total_por_escola_comp_dev_inperson_40", startCol=101, startRow=1)
  writeFormula(wb, sheet="Planilha4", x="=(Planilha3!U2+Planilha3!X2+Planilha3!AA2+Planilha3!AB2+Planilha3!AC2)/Planilha1!D3", startCol=101, startRow=2)
  
  # Fórmula 148: Range para Total Geral (Mínimo de Dispositivos e Treinamento Online 60h)
  writeData(wb, sheet = "Planilha4", x="sce2_rur_total_geral_min_dev_online_60_menos_25", startCol=102, startRow=1)
  writeFormula(wb, sheet="Planilha4", x="=(Planilha3!U2+Planilha3!V2+Planilha3!Y2+Planilha3!AB2+Planilha3!AC2)*0.75", startCol=102, startRow=2)
  writeData(wb, sheet = "Planilha4", x="sce2_rur_total_geral_min_dev_online_60_mais_25", startCol=103, startRow=1)
  writeFormula(wb, sheet="Planilha4", x="=(Planilha3!U2+Planilha3!V2+Planilha3!Y2+Planilha3!AB2+Planilha3!AC2)*1.25", startCol=103, startRow=2)
  
  # Fórmula 149: Range para Total Geral (Mínimo de Dispositivos e Treinamento Online 40h)
  writeData(wb, sheet = "Planilha4", x="sce2_rur_total_geral_min_dev_online_40_menos_25", startCol=104, startRow=1)
  writeFormula(wb, sheet="Planilha4", x="=(Planilha3!U2+Planilha3!V2+Planilha3!Z2+Planilha3!AB2+Planilha3!AC2)*0.75", startCol=104, startRow=2)
  writeData(wb, sheet = "Planilha4", x="sce2_rur_total_geral_min_dev_online_40_mais_25", startCol=105, startRow=1)
  writeFormula(wb, sheet="Planilha4", x="=(Planilha3!U2+Planilha3!V2+Planilha3!Z2+Planilha3!AB2+Planilha3!AC2)*1.25", startCol=105, startRow=2)
  
  # Fórmula 150: Range para Total Geral (Mínimo de Dispositivos e Treinamento Presencial 40h)
  writeData(wb, sheet = "Planilha4", x="sce2_rur_total_geral_min_dev_inperson_40_menos_25", startCol=106, startRow=1)
  writeFormula(wb, sheet="Planilha4", x="=(Planilha3!U2+Planilha3!V2+Planilha3!AA2+Planilha3!AB2+Planilha3!AC2)*0.75", startCol=106, startRow=2)
  writeData(wb, sheet = "Planilha4", x="sce2_rur_total_geral_min_dev_inperson_40_mais_25", startCol=107, startRow=1)
  writeFormula(wb, sheet="Planilha4", x="=(Planilha3!U2+Planilha3!V2+Planilha3!AA2+Planilha3!AB2+Planilha3!AC2)*1.25", startCol=107, startRow=2)
  
  # Fórmula 151: Range para Total Geral (Moderado de Dispositivos e Treinamento Online 60h)
  writeData(wb, sheet = "Planilha4", x="sce2_rur_total_geral_mild_dev_online_60_menos_25", startCol=108, startRow=1)
  writeFormula(wb, sheet="Planilha4", x="=(Planilha3!U2+Planilha3!W2+Planilha3!Y2+Planilha3!AB2+Planilha3!AC2)*0.75", startCol=108, startRow=2)
  writeData(wb, sheet = "Planilha4", x="sce2_rur_total_geral_mild_dev_online_60_mais_25", startCol=109, startRow=1)
  writeFormula(wb, sheet="Planilha4", x="=(Planilha3!U2+Planilha3!W2+Planilha3!Y2+Planilha3!AB2+Planilha3!AC2)*1.25", startCol=109, startRow=2)
  
  # Fórmula 152: Range para Total Geral (Moderado de Dispositivos e Treinamento Online 40h)
  writeData(wb, sheet = "Planilha4", x="sce2_rur_total_geral_mild_dev_online_40_menos_25", startCol=110, startRow=1)
  writeFormula(wb, sheet="Planilha4", x="=(Planilha3!U2+Planilha3!W2+Planilha3!Z2+Planilha3!AB2+Planilha3!AC2)*0.75", startCol=110, startRow=2)
  writeData(wb, sheet = "Planilha4", x="sce2_rur_total_geral_mild_dev_online_40_mais_25", startCol=111, startRow=1)
  writeFormula(wb, sheet="Planilha4", x="=(Planilha3!U2+Planilha3!W2+Planilha3!Z2+Planilha3!AB2+Planilha3!AC2)*1.25", startCol=111, startRow=2)
  
  # Fórmula 153: Range para Total Geral (Moderado de Dispositivos e Treinamento Presencial 40h)
  writeData(wb, sheet = "Planilha4", x="sce2_rur_total_geral_mild_dev_inperson_40_menos_25", startCol=112, startRow=1)
  writeFormula(wb, sheet="Planilha4", x="=(Planilha3!U2+Planilha3!W2+Planilha3!AA2+Planilha3!AB2+Planilha3!AC2)*0.75", startCol=112, startRow=2)
  writeData(wb, sheet = "Planilha4", x="sce2_rur_total_geral_mild_dev_inperson_40_mais_25", startCol=113, startRow=1)
  writeFormula(wb, sheet="Planilha4", x="=(Planilha3!U2+Planilha3!W2+Planilha3!AA2+Planilha3!AB2+Planilha3!AC2)*1.25", startCol=113, startRow=2)
  
  # Fórmula 154: Range para Total Geral (Abrangente de Dispositivos e Treinamento Online 60h)
  writeData(wb, sheet = "Planilha4", x="sce2_rur_total_geral_comp_dev_online_60_menos_25", startCol=114, startRow=1)
  writeFormula(wb, sheet="Planilha4", x="=(Planilha3!U2+Planilha3!X2+Planilha3!Y2+Planilha3!AB2+Planilha3!AC2)*0.75", startCol=114, startRow=2)
  writeData(wb, sheet = "Planilha4", x="sce2_rur_total_geral_comp_dev_online_60_mais_25", startCol=115, startRow=1)
  writeFormula(wb, sheet="Planilha4", x="=(Planilha3!U2+Planilha3!X2+Planilha3!Y2+Planilha3!AB2+Planilha3!AC2)*1.25", startCol=115, startRow=2)
  
  # Fórmula 155: Range para Total Geral (Abrangente de Dispositivos e Treinamento Online 40h)
  writeData(wb, sheet = "Planilha4", x="sce2_rur_total_geral_comp_dev_online_40_menos_25", startCol=116, startRow=1)
  writeFormula(wb, sheet="Planilha4", x="=(Planilha3!U2+Planilha3!X2+Planilha3!Z2+Planilha3!AB2+Planilha3!AC2)*0.75", startCol=116, startRow=2)
  writeData(wb, sheet = "Planilha4", x="sce2_rur_total_geral_comp_dev_online_40_mais_25", startCol=117, startRow=1)
  writeFormula(wb, sheet="Planilha4", x="=(Planilha3!U2+Planilha3!X2+Planilha3!Z2+Planilha3!AB2+Planilha3!AC2)*1.25", startCol=117, startRow=2)
  
  # Fórmula 156: Range para Total Geral (Abrangente de Dispositivos e Treinamento Presencial 40h)
  writeData(wb, sheet = "Planilha4", x="sce2_rur_total_geral_comp_dev_inperson_40_menos_25", startCol=118, startRow=1)
  writeFormula(wb, sheet="Planilha4", x="=(Planilha3!U2+Planilha3!X2+Planilha3!AA2+Planilha3!AB2+Planilha3!AC2)*0.75", startCol=118, startRow=2)
  writeData(wb, sheet = "Planilha4", x="sce2_rur_total_geral_comp_dev_inperson_40_mais_25", startCol=119, startRow=1)
  writeFormula(wb, sheet="Planilha4", x="=(Planilha3!U2+Planilha3!X2+Planilha3!AA2+Planilha3!AB2+Planilha3!AC2)*1.25", startCol=119, startRow=2)
  
  
  
  
  #---------- CENÁRIO 3 ----------
  
  # Fórmula 185: Dimensão 2: Apenas equipamentos para professores e escolas
  writeData(wb, sheet = "Planilha2", x="sce3_devt_dim_2_price_device_teacher", startCol=120, startRow=1)
  writeFormula(wb, sheet="Planilha2", x="=Planilha1!D7*Planilha1!D58*Planilha1!D31", startCol=120, startRow=2)
  # Fórmula 185: Dimensão 2: Apenas equipamentos para professores e escolas
  writeData(wb, sheet = "Planilha2", x="sce3_devt_dim_2_price_device_school", startCol=121, startRow=1)
  writeFormula(wb, sheet="Planilha2", x="=Planilha1!D2*Planilha1!D59*Planilha1!D29", startCol=121, startRow=2)
  # Fórmula 185: Dimensão 2: Apenas equipamentos para professores e escolas
  writeData(wb, sheet = "Planilha2", x="sce3_devt_dim_2_price_multimedia_projector", startCol=122, startRow=1)
  writeFormula(wb, sheet="Planilha2", x="=Planilha1!D2*Planilha1!D60*Planilha1!D26", startCol=122, startRow=2)
  
  # Fórmula 187: Total da dimensão 2 (min)
  writeData(wb, sheet = "Planilha3", x="sce3_devt_dim_2_total", startCol=30, startRow=1)
  writeFormula(wb, sheet="Planilha3", x="=SUM(Planilha2!DP2:DR2)/1000000", startCol=30, startRow=2)
  
  # Fórmula 156: Totais Gerais combinando com demais dimensões existentes
  writeData(wb, sheet = "Planilha4", x="sce3_devt_total_geral_online_60", startCol=120, startRow=1)
  writeFormula(wb, sheet="Planilha4", x="=Planilha3!C2+Planilha3!AD2+Planilha3!G2+Planilha3!J2+Planilha3!K2", startCol=120, startRow=2)
  writeData(wb, sheet = "Planilha4", x="sce3_devt_total_geral_online_40", startCol=121, startRow=1)
  writeFormula(wb, sheet="Planilha4", x="=Planilha3!C2+Planilha3!AD2+Planilha3!H2+Planilha3!J2+Planilha3!K2", startCol=121, startRow=2)
  writeData(wb, sheet = "Planilha4", x="sce3_devt_total_geral_inperson_40", startCol=122, startRow=1)
  writeFormula(wb, sheet="Planilha4", x="=Planilha3!C2+Planilha3!AD2+Planilha3!I2+Planilha3!J2+Planilha3!K2", startCol=122, startRow=2)
  
  # --- Faixas de variação ±25% ---
  
  writeData(wb, sheet = "Planilha4", x="sce3_devt_total_geral_online_60_menos_25", startCol=123, startRow=1)
  writeFormula(wb, sheet="Planilha4", x="=(Planilha3!C2+Planilha3!AD2+Planilha3!G2+Planilha3!J2+Planilha3!K2)*0.75", startCol=123, startRow=2)
  writeData(wb, sheet = "Planilha4", x="sce3_devt_total_geral_online_60_mais_25", startCol=124, startRow=1)
  writeFormula(wb, sheet="Planilha4", x="=(Planilha3!C2+Planilha3!AD2+Planilha3!G2+Planilha3!J2+Planilha3!K2)*1.25", startCol=124, startRow=2)
  
  writeData(wb, sheet = "Planilha4", x="sce3_devt_total_geral_online_40_menos_25", startCol=125, startRow=1)
  writeFormula(wb, sheet="Planilha4", x="=(Planilha3!C2+Planilha3!AD2+Planilha3!H2+Planilha3!J2+Planilha3!K2)*0.75", startCol=125, startRow=2)
  writeData(wb, sheet = "Planilha4", x="sce3_devt_total_geral_online_40_mais_25", startCol=126, startRow=1)
  writeFormula(wb, sheet="Planilha4", x="=(Planilha3!C2+Planilha3!AD2+Planilha3!H2+Planilha3!J2+Planilha3!K2)*1.25", startCol=126, startRow=2)
  
  writeData(wb, sheet = "Planilha4", x="sce3_devt_total_geral_inperson_40_menos_25", startCol=127, startRow=1)
  writeFormula(wb, sheet="Planilha4", x="=(Planilha3!C2+Planilha3!AD2+Planilha3!I2+Planilha3!J2+Planilha3!K2)*0.75", startCol=127, startRow=2)
  writeData(wb, sheet = "Planilha4", x="sce3_devt_total_geral_inperson_40_mais_25", startCol=128, startRow=1)
  writeFormula(wb, sheet="Planilha4", x="=(Planilha3!C2+Planilha3!AD2+Planilha3!I2+Planilha3!J2+Planilha3!K2)*1.25", startCol=128, startRow=2)
  
  #---------------------------
  #- 3. FORMATAÇÃO DA TABELA - 
  #---------------------------
  
  # Create a title style
  cenario_0_estilo <- createStyle(border = "Bottom", fgFill = "#ADD8E6")
  cenario_1_estilo <- createStyle(border = "Bottom", fgFill = "#FF7F7F")
  cenario_2_estilo <- createStyle(border = "Bottom", fgFill = "#FFFFC5")
  cenario_3_estilo <- createStyle(border = "Bottom", fgFill = "#90EE90")
  num_style <- createStyle(numFmt = "0")
  
  addStyle(wb, sheet="Planilha2", cenario_0_estilo, rows = 1, cols = 3:41)
  addStyle(wb, sheet="Planilha2", cenario_1_estilo, rows = 1, cols = 42:80)
  addStyle(wb, sheet="Planilha2", cenario_2_estilo, rows = 1, cols = 81:119)
  addStyle(wb, sheet="Planilha2", cenario_3_estilo, rows = 1, cols = 120:122)
  
  addStyle(wb, sheet="Planilha3", cenario_0_estilo, rows = 1, cols = 3:11)
  addStyle(wb, sheet="Planilha3", cenario_1_estilo, rows = 1, cols = 12:20)
  addStyle(wb, sheet="Planilha3", cenario_2_estilo, rows = 1, cols = 21:29)
  addStyle(wb, sheet="Planilha3", cenario_3_estilo, rows = 1, cols = 30)
  
  addStyle(wb, sheet="Planilha4", cenario_0_estilo, rows = 1, cols = 3:29)
  addStyle(wb, sheet="Planilha4", cenario_1_estilo, rows = 1, cols = 30:74)
  addStyle(wb, sheet="Planilha4", cenario_2_estilo, rows = 1, cols = 75:119)
  addStyle(wb, sheet="Planilha4", cenario_3_estilo, rows = 1, cols = 120:128)
  
  addStyle(wb, sheet="Planilha1", num_style, rows = 2:68, cols = 4)
  addStyle(wb, sheet="Planilha2", num_style, rows = 2, cols = 3:122)
  addStyle(wb, sheet="Planilha3", num_style, rows = 2, cols = 3:30)
  addStyle(wb, sheet="Planilha4", num_style, rows = 2, cols = 3:128)
  
  #----------------------------
  #- 4. SALVA O ARQUIVO FINAL - 
  #----------------------------
  
  # Salva o arquivo 
  saveWorkbook(wb, file = caminho_output, overwrite = T)
  
}




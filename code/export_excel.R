#-----------------------------------------------------------
#- 1. CRIA A PLANILHA COM OS DADOS DOS PAÍSES E PARÂMETROS - 
#-----------------------------------------------------------

# Importa funções
source(here("code", "functions.R"))
# Importa a base limpa
df_parametros_limpa <- data.table::fread(here("data", "base_parametros_limpa.csv"))
# Remove variáveis 
df_parametros_limpa <- df_parametros_limpa %>% select(-c(teacher_monthly_salary, reference_price_content_production60))
# Renomeia as variáveis da planilha "input"
colnames(df_parametros_limpa)[colnames(df_parametros_limpa) == "number_of_schools"] <- "Number of Schools"
colnames(df_parametros_limpa)[colnames(df_parametros_limpa) == "number_of_rural_schools"] <- "Number of Rural Schools"
colnames(df_parametros_limpa)[colnames(df_parametros_limpa) == "number_of_students"] <- "Number of Students"
colnames(df_parametros_limpa)[colnames(df_parametros_limpa) == "number_of_students_primary"] <- "Number of Primary Students"
colnames(df_parametros_limpa)[colnames(df_parametros_limpa) == "number_of_students_secondary"] <- "Number of Secondary Students"
colnames(df_parametros_limpa)[colnames(df_parametros_limpa) == "number_of_teachers"] <- "Number of Teachers"
colnames(df_parametros_limpa)[colnames(df_parametros_limpa) == "primary_schools_cima"] <- "Number of Primary Schools"
colnames(df_parametros_limpa)[colnames(df_parametros_limpa) == "secondary_schools_cima"] <- "Number of Secondary Schools"
colnames(df_parametros_limpa)[colnames(df_parametros_limpa) == "student_primary_databank_worldbank"] <- "Primary Students (World Bank)"
colnames(df_parametros_limpa)[colnames(df_parametros_limpa) == "student_secondary_databank_worldbank"] <- "Secondary Students (World Bank)"
colnames(df_parametros_limpa)[colnames(df_parametros_limpa) == "avg_student_per_school"] <- "Average Students per School"
colnames(df_parametros_limpa)[colnames(df_parametros_limpa) == "number_of_classroom"] <- "Number of Classrooms"
colnames(df_parametros_limpa)[colnames(df_parametros_limpa) == "itu_pop_below_50"] <- "% of schools that need fiber link"
colnames(df_parametros_limpa)[colnames(df_parametros_limpa) == "itu_pop_above_50"] <- "% of schools that need satellite connection"
colnames(df_parametros_limpa)[colnames(df_parametros_limpa) == "itu_pop_between_25_50"] <- "% of schools that need fiber expansion"
colnames(df_parametros_limpa)[colnames(df_parametros_limpa) == "reference_satellite_link_price"] <- "Price Satellite"
colnames(df_parametros_limpa)[colnames(df_parametros_limpa) == "reference_fiber_link_price"] <- "Price Fiber Link 200mbps - Monthly"
colnames(df_parametros_limpa)[colnames(df_parametros_limpa) == "reference_fiber_exp_price"] <- "Price Fiber Expansion (Per Km)"
colnames(df_parametros_limpa)[colnames(df_parametros_limpa) == "reference_access_point_price"] <- "Price Access Point"
colnames(df_parametros_limpa)[colnames(df_parametros_limpa) == "reference_firewall_price"] <- "Price Firewall"
colnames(df_parametros_limpa)[colnames(df_parametros_limpa) == "reference_nobreak_price"] <- "Price No-Break"
colnames(df_parametros_limpa)[colnames(df_parametros_limpa) == "reference_switch_price"] <- "Price Switch"
colnames(df_parametros_limpa)[colnames(df_parametros_limpa) == "reference_rack_price"] <- "Price Rack"
colnames(df_parametros_limpa)[colnames(df_parametros_limpa) == "reference_multimedia_projector_price"] <- "Price Multimedia Projector"
colnames(df_parametros_limpa)[colnames(df_parametros_limpa) == "reference_headphone_price"] <- "Price Headphones"
colnames(df_parametros_limpa)[colnames(df_parametros_limpa) == "reference_charging_cart_price"] <- "Price Charging Cart"
colnames(df_parametros_limpa)[colnames(df_parametros_limpa) == "reference_desktop_price"] <- "Price Desktop Computer"
colnames(df_parametros_limpa)[colnames(df_parametros_limpa) == "reference_tablet_price"] <- "Price Tablet"
colnames(df_parametros_limpa)[colnames(df_parametros_limpa) == "reference_laptop_price"] <- "Price Laptop"
colnames(df_parametros_limpa)[colnames(df_parametros_limpa) == "reference_cloudbook_price"] <- "Price Cloudbook"
colnames(df_parametros_limpa)[colnames(df_parametros_limpa) == "reference_price_teaching_learning"] <- "Price Teaching & Learning Platform"
colnames(df_parametros_limpa)[colnames(df_parametros_limpa) == "reference_price_management_platform"] <- "Price Management Platform"
colnames(df_parametros_limpa)[colnames(df_parametros_limpa) == "reference_consultant_price"] <- "Salary Consultant"
colnames(df_parametros_limpa)[colnames(df_parametros_limpa) == "reference_teacher_price"] <- "Salary Teacher"
colnames(df_parametros_limpa)[colnames(df_parametros_limpa) == "number_of_graduated_teacher"] <- "Number of teachers to be trained"
colnames(df_parametros_limpa)[colnames(df_parametros_limpa) == "reference_price_diagnostic_tool"] <- "Price Diagnostic Tool (implementing tool)"
colnames(df_parametros_limpa)[colnames(df_parametros_limpa) == "reference_price_lms"] <- "Price LMS platform for the training"
colnames(df_parametros_limpa)[colnames(df_parametros_limpa) == "reference_price_training_design"] <- "Price Training Design"
colnames(df_parametros_limpa)[colnames(df_parametros_limpa) == "reference_price_trainers60"] <- "Cost of hiring Trainer (60h)"
colnames(df_parametros_limpa)[colnames(df_parametros_limpa) == "reference_multiplier_inperson"] <- "Multiplier In-Person Training"
colnames(df_parametros_limpa)[colnames(df_parametros_limpa) == "reference_multiplier40h"] <- "Multiplier (40h Training)"
colnames(df_parametros_limpa)[colnames(df_parametros_limpa) == "type_opex"] <- "Months to adjust"
colnames(df_parametros_limpa)[colnames(df_parametros_limpa) == "parameter_fiber_exp"] <- "Parameter Fiber Expansion (avg of km per school)"
colnames(df_parametros_limpa)[colnames(df_parametros_limpa) == "parameter_access_point"] <- "Parameter Access Point (# of AP per classrooms)"
colnames(df_parametros_limpa)[colnames(df_parametros_limpa) == "parameter_firewall"] <- "Parameter Firewall (# per school)"
colnames(df_parametros_limpa)[colnames(df_parametros_limpa) == "parameter_nobreak"] <- "Parameter No-Break  (# per school)"
colnames(df_parametros_limpa)[colnames(df_parametros_limpa) == "parameter_switch"] <- "Parameter Switch  (# per school)"
colnames(df_parametros_limpa)[colnames(df_parametros_limpa) == "parameter_rack"] <- "Parameter Rack  (# per school)"
colnames(df_parametros_limpa)[colnames(df_parametros_limpa) == "parameter_min_scenario_primary"] <- "Parameter Minimum Scenario (Primary) (# per student)"
colnames(df_parametros_limpa)[colnames(df_parametros_limpa) == "parameter_min_scenario_secondary"] <- "Parameter Minimum Scenario (Secondary) (# per student)"
colnames(df_parametros_limpa)[colnames(df_parametros_limpa) == "parameter_mild_scenario_primary"] <- "Parameter Mild Scenario (Primary) (# per student)"
colnames(df_parametros_limpa)[colnames(df_parametros_limpa) == "parameter_mild_scenario_secondary"] <- "Parameter Mild Scenario (Secondary) (# per student)"
colnames(df_parametros_limpa)[colnames(df_parametros_limpa) == "parameter_comprehensive_primary"] <- "Parameter Comprehensive Scenario (Primary) (# per student)"
colnames(df_parametros_limpa)[colnames(df_parametros_limpa) == "parameter_comprehensive_secondary"] <- "Parameter Comprehensive Scenario (Secondary) (# per student)"
colnames(df_parametros_limpa)[colnames(df_parametros_limpa) == "parameter_device_per_teacher"] <- "Parameter Devices per Teacher"
colnames(df_parametros_limpa)[colnames(df_parametros_limpa) == "parameter_device_per_school"] <- "Parameter Devices per School"
colnames(df_parametros_limpa)[colnames(df_parametros_limpa) == "parameter_multimedia_projector"] <- "Parameter Multimedia Projector (# per school)"
colnames(df_parametros_limpa)[colnames(df_parametros_limpa) == "parameter_teacher_class"] <- "Parameter Teachers per Class (size of teacher cohort)"
colnames(df_parametros_limpa)[colnames(df_parametros_limpa) == "parameter_teaching_learning"] <- "Parameter Teaching & Learning (per student)"
colnames(df_parametros_limpa)[colnames(df_parametros_limpa) == "parameter_management"] <- "Parameter Management (per school)"
colnames(df_parametros_limpa)[colnames(df_parametros_limpa) == "parameter_central_team_min"] <- "Parameter Central Team (Min)"
colnames(df_parametros_limpa)[colnames(df_parametros_limpa) == "parameter_central_team_max"] <- "Parameter Central Team (Max)"
colnames(df_parametros_limpa)[colnames(df_parametros_limpa) == "parameter_regional_team"] <- "Parameter Regional Team (# individuals per school)"
colnames(df_parametros_limpa)[colnames(df_parametros_limpa) == "parameter_local_team_urban"] <- "Parameter Local Team (Urban) (# individuals per school)"
colnames(df_parametros_limpa)[colnames(df_parametros_limpa) == "parameter_local_team_rural"] <- "Parameter Local Team (Rural) (# individuals per school)"

# Lista de países
lista_paises <- df_parametros_limpa$country

# Teste
lista_paises <- c("Argentina", "Bahamas")

# Gera arquivos em excel
for(pais in lista_paises) {
  funcao_excel(pais, here("output", "v1", glue("{data_hoje()}_{pais}_calculadora.xlsx")), df_parametros_limpa)
}






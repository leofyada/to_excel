#--------------------------------------------
#- PLANILHA 1: DADOS DOS PAÍSES (SEM FONTE) -
#--------------------------------------------

# Importa funções
source(here("code", "functions.R"))
source(here("code", "data_import.R"))

# Remove colunas com "source"
df_parametros_limpa <- exclui_source(df_parametros)

# Remove variáveis 
df_parametros_limpa <- df_parametros_limpa %>% 
  select(
    -c(
      teacher_monthly_salary, 
      reference_price_content_production60,
      student_primary_databank_worldbank,
      student_secondary_databank_worldbank
    )
  )

# Cria uma planilha com nomes ajustados
df_parametros_limpa_aj <- df_parametros_limpa
# Renomeia as variáveis da planilha "input"
colnames(df_parametros_limpa_aj)[colnames(df_parametros_limpa_aj) == "number_of_schools"] <- "Number of Schools"
colnames(df_parametros_limpa_aj)[colnames(df_parametros_limpa_aj) == "number_of_rural_schools"] <- "Number of Rural Schools"
colnames(df_parametros_limpa_aj)[colnames(df_parametros_limpa_aj) == "number_of_students"] <- "Number of Students"
colnames(df_parametros_limpa_aj)[colnames(df_parametros_limpa_aj) == "number_of_students_primary"] <- "Number of Primary Students"
colnames(df_parametros_limpa_aj)[colnames(df_parametros_limpa_aj) == "number_of_students_secondary"] <- "Number of Secondary Students"
colnames(df_parametros_limpa_aj)[colnames(df_parametros_limpa_aj) == "number_of_teachers"] <- "Number of Teachers"
colnames(df_parametros_limpa_aj)[colnames(df_parametros_limpa_aj) == "primary_schools_cima"] <- "Number of Primary Schools"
colnames(df_parametros_limpa_aj)[colnames(df_parametros_limpa_aj) == "secondary_schools_cima"] <- "Number of Secondary Schools"
colnames(df_parametros_limpa_aj)[colnames(df_parametros_limpa_aj) == "avg_student_per_school"] <- "Average Students per School"
colnames(df_parametros_limpa_aj)[colnames(df_parametros_limpa_aj) == "number_of_classroom"] <- "Number of Classrooms"
colnames(df_parametros_limpa_aj)[colnames(df_parametros_limpa_aj) == "itu_pop_below_50"] <- "% of schools that need fiber link"
colnames(df_parametros_limpa_aj)[colnames(df_parametros_limpa_aj) == "itu_pop_above_50"] <- "% of schools that need satellite connection"
colnames(df_parametros_limpa_aj)[colnames(df_parametros_limpa_aj) == "itu_pop_between_25_50"] <- "% of schools that need fiber expansion"
colnames(df_parametros_limpa_aj)[colnames(df_parametros_limpa_aj) == "reference_satellite_link_price"] <- "Price Satellite"
colnames(df_parametros_limpa_aj)[colnames(df_parametros_limpa_aj) == "reference_fiber_link_price"] <- "Price Fiber Link 200mbps - Monthly"
colnames(df_parametros_limpa_aj)[colnames(df_parametros_limpa_aj) == "reference_fiber_exp_price"] <- "Price Fiber Expansion (Per Km)"
colnames(df_parametros_limpa_aj)[colnames(df_parametros_limpa_aj) == "reference_access_point_price"] <- "Price Access Point"
colnames(df_parametros_limpa_aj)[colnames(df_parametros_limpa_aj) == "reference_firewall_price"] <- "Price Firewall"
colnames(df_parametros_limpa_aj)[colnames(df_parametros_limpa_aj) == "reference_nobreak_price"] <- "Price No-Break"
colnames(df_parametros_limpa_aj)[colnames(df_parametros_limpa_aj) == "reference_switch_price"] <- "Price Switch"
colnames(df_parametros_limpa_aj)[colnames(df_parametros_limpa_aj) == "reference_rack_price"] <- "Price Rack"
colnames(df_parametros_limpa_aj)[colnames(df_parametros_limpa_aj) == "reference_multimedia_projector_price"] <- "Price Multimedia Projector"
colnames(df_parametros_limpa_aj)[colnames(df_parametros_limpa_aj) == "reference_headphone_price"] <- "Price Headphones"
colnames(df_parametros_limpa_aj)[colnames(df_parametros_limpa_aj) == "reference_charging_cart_price"] <- "Price Charging Cart"
colnames(df_parametros_limpa_aj)[colnames(df_parametros_limpa_aj) == "reference_desktop_price"] <- "Price Desktop Computer"
colnames(df_parametros_limpa_aj)[colnames(df_parametros_limpa_aj) == "reference_tablet_price"] <- "Price Tablet"
colnames(df_parametros_limpa_aj)[colnames(df_parametros_limpa_aj) == "reference_laptop_price"] <- "Price Laptop"
colnames(df_parametros_limpa_aj)[colnames(df_parametros_limpa_aj) == "reference_cloudbook_price"] <- "Price Cloudbook"
colnames(df_parametros_limpa_aj)[colnames(df_parametros_limpa_aj) == "reference_price_teaching_learning"] <- "Price Teaching & Learning Platform"
colnames(df_parametros_limpa_aj)[colnames(df_parametros_limpa_aj) == "reference_price_management_platform"] <- "Price Management Platform"
colnames(df_parametros_limpa_aj)[colnames(df_parametros_limpa_aj) == "reference_consultant_price"] <- "Salary Consultant"
colnames(df_parametros_limpa_aj)[colnames(df_parametros_limpa_aj) == "reference_teacher_price"] <- "Salary Teacher"
colnames(df_parametros_limpa_aj)[colnames(df_parametros_limpa_aj) == "number_of_graduated_teacher"] <- "Number of teachers to be trained"
colnames(df_parametros_limpa_aj)[colnames(df_parametros_limpa_aj) == "reference_price_diagnostic_tool"] <- "Price Diagnostic Tool (implementing tool)"
colnames(df_parametros_limpa_aj)[colnames(df_parametros_limpa_aj) == "reference_price_lms"] <- "Price LMS platform for the training"
colnames(df_parametros_limpa_aj)[colnames(df_parametros_limpa_aj) == "reference_price_training_design"] <- "Price Training Design"
colnames(df_parametros_limpa_aj)[colnames(df_parametros_limpa_aj) == "reference_price_trainers60"] <- "Cost of hiring Trainer (60h)"
colnames(df_parametros_limpa_aj)[colnames(df_parametros_limpa_aj) == "reference_multiplier_inperson"] <- "Multiplier In-Person Training"
colnames(df_parametros_limpa_aj)[colnames(df_parametros_limpa_aj) == "reference_multiplier40h"] <- "Multiplier (40h Training)"
colnames(df_parametros_limpa_aj)[colnames(df_parametros_limpa_aj) == "type_opex"] <- "Months to adjust"
colnames(df_parametros_limpa_aj)[colnames(df_parametros_limpa_aj) == "parameter_fiber_exp"] <- "Parameter Fiber Expansion (avg of km per school)"
colnames(df_parametros_limpa_aj)[colnames(df_parametros_limpa_aj) == "parameter_access_point"] <- "Parameter Access Point (# of AP per classrooms)"
colnames(df_parametros_limpa_aj)[colnames(df_parametros_limpa_aj) == "parameter_firewall"] <- "Parameter Firewall (# per school)"
colnames(df_parametros_limpa_aj)[colnames(df_parametros_limpa_aj) == "parameter_nobreak"] <- "Parameter No-Break  (# per school)"
colnames(df_parametros_limpa_aj)[colnames(df_parametros_limpa_aj) == "parameter_switch"] <- "Parameter Switch  (# per school)"
colnames(df_parametros_limpa_aj)[colnames(df_parametros_limpa_aj) == "parameter_rack"] <- "Parameter Rack  (# per school)"
colnames(df_parametros_limpa_aj)[colnames(df_parametros_limpa_aj) == "parameter_min_scenario_primary"] <- "Parameter Minimum Scenario (Primary) (# per student)"
colnames(df_parametros_limpa_aj)[colnames(df_parametros_limpa_aj) == "parameter_min_scenario_secondary"] <- "Parameter Minimum Scenario (Secondary) (# per student)"
colnames(df_parametros_limpa_aj)[colnames(df_parametros_limpa_aj) == "parameter_mild_scenario_primary"] <- "Parameter Mild Scenario (Primary) (# per student)"
colnames(df_parametros_limpa_aj)[colnames(df_parametros_limpa_aj) == "parameter_mild_scenario_secondary"] <- "Parameter Mild Scenario (Secondary) (# per student)"
colnames(df_parametros_limpa_aj)[colnames(df_parametros_limpa_aj) == "parameter_comprehensive_primary"] <- "Parameter Comprehensive Scenario (Primary) (# per student)"
colnames(df_parametros_limpa_aj)[colnames(df_parametros_limpa_aj) == "parameter_comprehensive_secondary"] <- "Parameter Comprehensive Scenario (Secondary) (# per student)"
colnames(df_parametros_limpa_aj)[colnames(df_parametros_limpa_aj) == "parameter_device_per_teacher"] <- "Parameter Devices per Teacher"
colnames(df_parametros_limpa_aj)[colnames(df_parametros_limpa_aj) == "parameter_device_per_school"] <- "Parameter Devices per School"
colnames(df_parametros_limpa_aj)[colnames(df_parametros_limpa_aj) == "parameter_multimedia_projector"] <- "Parameter Multimedia Projector (# per school)"
colnames(df_parametros_limpa_aj)[colnames(df_parametros_limpa_aj) == "parameter_teacher_class"] <- "Parameter Teachers per Class (size of teacher cohort)"
colnames(df_parametros_limpa_aj)[colnames(df_parametros_limpa_aj) == "parameter_teaching_learning"] <- "Parameter Teaching & Learning (per student)"
colnames(df_parametros_limpa_aj)[colnames(df_parametros_limpa_aj) == "parameter_management"] <- "Parameter Management (per school)"
colnames(df_parametros_limpa_aj)[colnames(df_parametros_limpa_aj) == "parameter_central_team_min"] <- "Parameter Central Team (Min)"
colnames(df_parametros_limpa_aj)[colnames(df_parametros_limpa_aj) == "parameter_central_team_max"] <- "Parameter Central Team (Max)"
colnames(df_parametros_limpa_aj)[colnames(df_parametros_limpa_aj) == "parameter_regional_team"] <- "Parameter Regional Team (# individuals per school)"
colnames(df_parametros_limpa_aj)[colnames(df_parametros_limpa_aj) == "parameter_local_team_urban"] <- "Parameter Local Team (Urban) (# individuals per school)"
colnames(df_parametros_limpa_aj)[colnames(df_parametros_limpa_aj) == "parameter_local_team_rural"] <- "Parameter Local Team (Rural) (# individuals per school)"

# Exporta base limpa
data.table::fwrite(x=df_parametros_limpa, file = here("data", "base_parametros_limpa.csv"))
data.table::fwrite(x=df_parametros_limpa_aj, file = here("data", "base_parametros_limpa_aj.csv"))

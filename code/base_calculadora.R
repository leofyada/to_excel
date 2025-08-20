#--------------------------------------------
#- Importa a base de dados para os cálculos - 
#--------------------------------------------

# Base que serve de subsídio para os cálculos
base_parametros_limpa <- data.table::fread(input = here("data", "silver", "base_parametros_limpa.csv"))

#------------------
#- Total por item - 
#------------------

# Calcula o valor total por item
base_parametros_calculadora <- base_parametros_limpa %>% 
  mutate(
    
    #---- Total por item - Cenário 0 ----
    
    dim_1_price_fiber_exp = itu_pop_between_25_50 * number_of_schools * parameter_fiber_exp * reference_fiber_exp_price,
    dim_1_price_satellite_service = itu_pop_above_50 * number_of_schools * reference_satellite_link_price * type_opex,
    dim_1_price_fiber_link = itu_pop_below_50 * number_of_schools * (avg_student_per_school) * (reference_fiber_link_price/200) * type_opex,
    dim_1_price_access_point = (((number_of_students_primary/ 25) + primary_schools_cima * 3)  + ((number_of_students_secondary/ 35) + secondary_schools_cima * 3)) * parameter_access_point * reference_access_point_price,
    dim_1_price_firewall = number_of_schools * parameter_firewall * reference_firewall_price,
    dim_1_price_nobreak = number_of_schools * parameter_nobreak * reference_nobreak_price,
    dim_1_price_switch = number_of_schools * parameter_switch * reference_switch_price,
    dim_1_price_rack = number_of_schools * parameter_rack * reference_rack_price,
    
    dim_2_price_device_student_min = (number_of_students_primary * parameter_min_scenario_primary * reference_tablet_price) + (number_of_students_secondary * parameter_min_scenario_secondary * reference_cloudbook_price),
    dim_2_price_device_student_mild = (number_of_students_primary * parameter_mild_scenario_primary * reference_tablet_price) + (number_of_students_secondary * parameter_mild_scenario_secondary * reference_cloudbook_price),
    dim_2_price_device_student_comprehensive = (number_of_students_primary * parameter_comprehensive_primary * reference_tablet_price) + (number_of_students_secondary * parameter_comprehensive_secondary * reference_cloudbook_price),
    dim_2_price_device_teacher = number_of_teachers * parameter_device_per_teacher * reference_laptop_price,
    dim_2_price_device_school = number_of_schools * parameter_device_per_school * reference_desktop_price,
    dim_2_price_charging_cart_min = ((number_of_students_primary * parameter_min_scenario_primary) + (number_of_students_secondary * parameter_min_scenario_secondary)) / 30 * reference_charging_cart_price,
    dim_2_price_charging_cart_mild = ((number_of_students_primary * parameter_mild_scenario_primary) + (number_of_students_secondary * parameter_mild_scenario_secondary)) / 30 * reference_charging_cart_price,
    dim_2_price_charging_cart_comprehensive = ((number_of_students_primary * parameter_comprehensive_primary) + (number_of_students_secondary * parameter_comprehensive_secondary)) / 30 * reference_charging_cart_price,
    dim_2_price_multimedia_projector = number_of_schools * parameter_multimedia_projector * reference_multimedia_projector_price,
    dim_2_price_headphones_min_scenario = ((number_of_students_primary * parameter_min_scenario_primary) + (number_of_students_secondary * parameter_min_scenario_secondary)) * reference_headphone_price,
    dim_2_price_headphones_mild_scenario = ((number_of_students_primary * parameter_mild_scenario_primary) + (number_of_students_secondary * parameter_mild_scenario_secondary)) * reference_headphone_price,
    dim_2_price_headphones_comprehensive = ((number_of_students_primary * parameter_comprehensive_primary) + (number_of_students_secondary * parameter_comprehensive_secondary)) * reference_headphone_price,
    
    dim_3_price_diagnostic_tool = reference_price_diagnostic_tool,
    dim_3_price_lms_platform = reference_price_lms,
    
    dim_3_price_specialist_trainers_scenario_online60 = reference_price_trainers60 * (number_of_graduated_teacher / parameter_teacher_class),
    dim_3_price_course_for_trainers_scenario_online60 = reference_price_trainers60 * 1.5 * (number_of_graduated_teacher / parameter_teacher_class) / parameter_teacher_class,
    dim_3_price_training_design_scenario_online60 = reference_price_training_design,
    
    dim_3_price_specialist_trainers_scenario_online40 = dim_3_price_specialist_trainers_scenario_online60 * reference_multiplier40h,
    dim_3_price_course_for_trainers_scenario_online40 = dim_3_price_course_for_trainers_scenario_online60 * reference_multiplier40h,
    dim_3_price_training_design_scenario_online40 = dim_3_price_training_design_scenario_online60 * reference_multiplier40h,
    
    dim_3_price_specialist_trainers_scenario_inperson40 = dim_3_price_specialist_trainers_scenario_online60 * reference_multiplier_inperson * reference_multiplier40h, 
    dim_3_price_course_for_trainers_scenario_inperson40 = dim_3_price_course_for_trainers_scenario_online60 * reference_multiplier_inperson * reference_multiplier40h, 
    dim_3_price_training_design_scenario_inperson40 = dim_3_price_training_design_scenario_online60 * reference_multiplier_inperson * reference_multiplier40h,
    
    dim_4_price_teaching_learning_platform = (number_of_students_secondary + number_of_students_primary) * reference_price_teaching_learning * parameter_teaching_learning * (type_opex / 12),
    dim_4_price_management_platform = number_of_schools * reference_price_management_platform * parameter_management * (type_opex / 12),
    
    dim_5_price_central_team = reference_consultant_price * ((parameter_central_team_min + parameter_central_team_max) / 2) * type_opex,
    dim_5_price_regional_team = parameter_regional_team * number_of_schools * reference_teacher_price * type_opex,
    dim_5_price_local_team = (parameter_local_team_urban *(number_of_schools- number_of_rural_schools) + (parameter_local_team_rural* number_of_rural_schools)) * reference_teacher_price * type_opex,
    
    #---- Total por item - Cenário 1 (Secondary schools) ----
    
    sce1_sec_dim_1_price_fiber_exp = itu_pop_between_25_50 * secondary_schools_cima * parameter_fiber_exp * reference_fiber_exp_price,
    sce1_sec_dim_1_price_satellite_service = itu_pop_above_50 * secondary_schools_cima * reference_satellite_link_price * type_opex,
    sce1_sec_dim_1_price_fiber_link = itu_pop_below_50 * secondary_schools_cima * avg_student_per_school * (reference_fiber_link_price/200) * type_opex,
    sce1_sec_dim_1_price_access_point = ((number_of_students_secondary/ 35) + secondary_schools_cima * 3) * parameter_access_point * reference_access_point_price, 
    sce1_sec_dim_1_price_firewall = secondary_schools_cima * parameter_firewall * reference_firewall_price,
    sce1_sec_dim_1_price_nobreak = secondary_schools_cima * parameter_nobreak * reference_nobreak_price,
    sce1_sec_dim_1_price_switch = secondary_schools_cima * parameter_switch * reference_switch_price,
    sce1_sec_dim_1_price_rack = secondary_schools_cima * parameter_rack * reference_rack_price,
    
    sce1_sec_dim_2_price_device_student_min = number_of_students_secondary * parameter_min_scenario_secondary * reference_cloudbook_price,
    sce1_sec_dim_2_price_device_student_mild = number_of_students_secondary * parameter_mild_scenario_secondary * reference_cloudbook_price,
    sce1_sec_dim_2_price_device_student_comprehensive = number_of_students_secondary * parameter_comprehensive_secondary * reference_cloudbook_price,
    sce1_sec_dim_2_price_device_teacher = (number_of_teachers * (number_of_students_secondary / (number_of_students))) * parameter_device_per_teacher * reference_laptop_price,
    sce1_sec_dim_2_price_device_school = secondary_schools_cima * parameter_device_per_school * reference_desktop_price,
    sce1_sec_dim_2_price_charging_cart_min = (number_of_students_secondary * parameter_min_scenario_secondary) / 30 * reference_charging_cart_price,
    sce1_sec_dim_2_price_charging_cart_mild = (number_of_students_secondary * parameter_mild_scenario_secondary) / 30 * reference_charging_cart_price,
    sce1_sec_dim_2_price_charging_cart_comprehensive = (number_of_students_secondary * parameter_comprehensive_secondary) / 30 * reference_charging_cart_price,
    sce1_sec_dim_2_price_multimedia_projector = secondary_schools_cima * parameter_multimedia_projector * reference_multimedia_projector_price,
    sce1_sec_dim_2_price_headphones_min_scenario = (number_of_students_secondary * parameter_min_scenario_secondary) * reference_headphone_price,
    sce1_sec_dim_2_price_headphones_mild_scenario = (number_of_students_secondary * parameter_mild_scenario_secondary) * reference_headphone_price,
    sce1_sec_dim_2_price_headphones_comprehensive = (number_of_students_secondary * parameter_comprehensive_secondary) * reference_headphone_price,
    
    sce1_sec_dim_3_price_diagnostic_tool = reference_price_diagnostic_tool, 
    sce1_sec_dim_3_price_lms_platform = reference_price_lms, 

    sce1_sec_dim_3_price_specialist_trainers_scenario_online60 = reference_price_trainers60 * ((number_of_graduated_teacher * (number_of_students_secondary / (number_of_students))) / parameter_teacher_class),
    sce1_sec_dim_3_price_course_for_trainers_scenario_online60 = reference_teacher_price * 1.5* ((number_of_graduated_teacher * (number_of_students_secondary / (number_of_students))) / parameter_teacher_class) / parameter_teacher_class,
    sce1_sec_dim_3_price_training_design_scenario_online60 = reference_price_training_design,
    
    sce1_sec_dim_3_price_specialist_trainers_scenario_online40 = sce1_sec_dim_3_price_specialist_trainers_scenario_online60 * reference_multiplier40h,
    sce1_sec_dim_3_price_course_for_trainers_scenario_online40 = sce1_sec_dim_3_price_course_for_trainers_scenario_online60 * reference_multiplier40h,
    sce1_sec_dim_3_price_training_design_scenario_online40 = sce1_sec_dim_3_price_training_design_scenario_online60 * reference_multiplier40h,
    
    sce1_sec_dim_3_price_specialist_trainers_scenario_inperson40 = sce1_sec_dim_3_price_specialist_trainers_scenario_online60 * reference_multiplier_inperson * reference_multiplier40h,
    sce1_sec_dim_3_price_course_for_trainers_scenario_inperson40 = sce1_sec_dim_3_price_course_for_trainers_scenario_online60 * reference_multiplier_inperson * reference_multiplier40h,
    sce1_sec_dim_3_price_training_design_scenario_inperson40 = sce1_sec_dim_3_price_training_design_scenario_online60 * reference_multiplier_inperson * reference_multiplier40h,
    
    sce1_sec_dim_4_price_teaching_learning_platform = number_of_students_secondary * reference_price_teaching_learning * parameter_teaching_learning * (type_opex / 12),
    sce1_sec_dim_4_price_management_platform = secondary_schools_cima * reference_price_management_platform * parameter_management * (type_opex / 12),
    
    sce1_sec_dim_5_price_central_team = reference_consultant_price * ((parameter_central_team_min + parameter_central_team_max) / 2) * type_opex, 
    sce1_sec_dim_5_price_regional_team = parameter_regional_team * secondary_schools_cima * reference_teacher_price * type_opex,
    sce1_sec_dim_5_price_local_team = (parameter_local_team_urban *(secondary_schools_cima/number_of_schools)*(number_of_schools- number_of_rural_schools) + (secondary_schools_cima/number_of_schools)*(parameter_local_team_rural* number_of_rural_schools)) * reference_teacher_price *type_opex,
    
    #---- Total por item - Cenário 2 ----    
    
    sce2_rur_dim_1_price_satellite_service = pmin(number_of_rural_schools, itu_pop_above_50 * number_of_schools) * reference_satellite_link_price * type_opex,
    sce2_rur_dim_1_price_fiber_exp = pmin(number_of_rural_schools- pmin(number_of_rural_schools, (itu_pop_above_50 * number_of_schools)), itu_pop_between_25_50 * number_of_schools ) * parameter_fiber_exp * reference_fiber_exp_price,
    sce2_rur_dim_1_price_fiber_link = (number_of_rural_schools - pmin(number_of_rural_schools, itu_pop_above_50)) * (avg_student_per_school)*(reference_fiber_link_price / 200) * type_opex,
    sce2_rur_dim_1_price_access_point = ((number_of_students_primary/ 25) + primary_schools_cima * 3)  + ((number_of_students_secondary/ 35) + secondary_schools_cima * 3) * (number_of_rural_schools / number_of_schools) * parameter_access_point * reference_access_point_price,
    sce2_rur_dim_1_price_firewall = number_of_rural_schools * parameter_firewall * reference_firewall_price,
    sce2_rur_dim_1_price_nobreak = number_of_rural_schools * parameter_nobreak * reference_nobreak_price,
    sce2_rur_dim_1_price_switch = number_of_rural_schools * parameter_switch * reference_switch_price,
    sce2_rur_dim_1_price_rack = number_of_rural_schools * parameter_rack * reference_rack_price,
    sce2_rur_dim_2_price_device_student_min = ((number_of_students_primary * (number_of_rural_schools / number_of_schools)) * parameter_min_scenario_primary * reference_tablet_price) + ((number_of_students_secondary * (number_of_rural_schools / number_of_schools)) * parameter_min_scenario_secondary * reference_cloudbook_price),
    
    sce2_rur_dim_2_price_device_student_mild = ((number_of_students_primary * (number_of_rural_schools / number_of_schools)) * parameter_mild_scenario_primary * reference_tablet_price) + ((number_of_students_secondary * (number_of_rural_schools / number_of_schools)) * parameter_mild_scenario_secondary * reference_cloudbook_price),
    sce2_rur_dim_2_price_device_student_comprehensive = ((number_of_students_primary * (number_of_rural_schools / number_of_schools)) * parameter_comprehensive_primary * reference_tablet_price) + ((number_of_students_secondary * (number_of_rural_schools / number_of_schools)) * parameter_comprehensive_secondary * reference_cloudbook_price),
    sce2_rur_dim_2_price_device_teacher = (number_of_teachers * (number_of_rural_schools / number_of_schools)) * parameter_device_per_teacher * reference_laptop_price,
    sce2_rur_dim_2_price_device_school = number_of_rural_schools * parameter_device_per_school * reference_desktop_price,
    sce2_rur_dim_2_price_charging_cart_min = (((number_of_students_primary * (number_of_rural_schools / number_of_schools)) * parameter_min_scenario_primary) + ((number_of_students_secondary * (number_of_rural_schools / number_of_schools)) * parameter_min_scenario_secondary)) / 30 * reference_charging_cart_price,
    sce2_rur_dim_2_price_charging_cart_mild = (((number_of_students_primary * (number_of_rural_schools / number_of_schools)) * parameter_mild_scenario_primary) + ((number_of_students_secondary * (number_of_rural_schools / number_of_schools)) * parameter_mild_scenario_secondary)) / 30 * reference_charging_cart_price,
    sce2_rur_dim_2_price_charging_cart_comprehensive = (((number_of_students_primary * (number_of_rural_schools / number_of_schools)) * parameter_comprehensive_primary) + ((number_of_students_secondary * (number_of_rural_schools / number_of_schools)) * parameter_comprehensive_secondary)) / 30 * reference_charging_cart_price,
    sce2_rur_dim_2_price_multimedia_projector = number_of_rural_schools * parameter_multimedia_projector * reference_multimedia_projector_price,
    sce2_rur_dim_2_price_headphones_min_scenario = (((number_of_students_primary * (number_of_rural_schools / number_of_schools)) * parameter_min_scenario_primary) + ((number_of_students_secondary * (number_of_rural_schools / number_of_schools)) * parameter_min_scenario_secondary)) * reference_headphone_price,
    sce2_rur_dim_2_price_headphones_mild_scenario = (((number_of_students_primary * (number_of_rural_schools / number_of_schools)) * parameter_mild_scenario_primary) + ((number_of_students_secondary * (number_of_rural_schools / number_of_schools)) * parameter_mild_scenario_secondary)) * reference_headphone_price,
    sce2_rur_dim_2_price_headphones_comprehensive = (((number_of_students_primary * (number_of_rural_schools / number_of_schools)) * parameter_comprehensive_primary) + ((number_of_students_secondary * (number_of_rural_schools / number_of_schools)) * parameter_comprehensive_secondary)) * reference_headphone_price,
    
    sce2_rur_dim_3_price_diagnostic_tool = reference_price_diagnostic_tool,
    sce2_rur_dim_3_price_lms_platform = reference_price_lms,
    
    sce2_rur_dim_3_price_specialist_trainers_scenario_online60 = reference_price_trainers60 * ((number_of_graduated_teacher * (number_of_rural_schools / number_of_schools)) / parameter_teacher_class),
    sce2_rur_dim_3_price_course_for_trainers_scenario_online60 = reference_price_trainers60 * 1.5 * ((number_of_graduated_teacher * (number_of_rural_schools / number_of_schools)) / parameter_teacher_class) / parameter_teacher_class,
    sce2_rur_dim_3_price_training_design_scenario_online60 = reference_price_training_design,
    
    sce2_rur_dim_3_price_specialist_trainers_scenario_online40 = sce2_rur_dim_3_price_specialist_trainers_scenario_online60 * reference_multiplier40h,
    sce2_rur_dim_3_price_course_for_trainers_scenario_online40 = sce2_rur_dim_3_price_course_for_trainers_scenario_online60 * reference_multiplier40h,
    sce2_rur_dim_3_price_training_design_scenario_online40 = sce2_rur_dim_3_price_training_design_scenario_online60 * reference_multiplier40h,
    
    sce2_rur_dim_3_price_specialist_trainers_scenario_inperson40 = sce2_rur_dim_3_price_specialist_trainers_scenario_online60 * reference_multiplier_inperson * reference_multiplier40h,
    sce2_rur_dim_3_price_course_for_trainers_scenario_inperson40 = sce2_rur_dim_3_price_course_for_trainers_scenario_online60 * reference_multiplier_inperson * reference_multiplier40h,
    sce2_rur_dim_3_price_training_design_scenario_inperson40 = sce2_rur_dim_3_price_training_design_scenario_online60 * reference_multiplier_inperson * reference_multiplier40h,
    
    sce2_rur_dim_4_price_teaching_learning_platform = ((number_of_students_secondary + number_of_students_primary) * (number_of_rural_schools / number_of_schools)) * reference_price_teaching_learning * parameter_teaching_learning * (type_opex / 12),
    sce2_rur_dim_4_price_management_platform = number_of_rural_schools * reference_price_management_platform * parameter_management * (type_opex / 12),
    
    sce2_rur_dim_5_price_central_team = reference_consultant_price * ((parameter_central_team_min + parameter_central_team_max) / 2) * type_opex,
    sce2_rur_dim_5_price_regional_team = parameter_regional_team * number_of_rural_schools * reference_teacher_price * type_opex,
    sce2_rur_dim_5_price_local_team = parameter_local_team_rural * number_of_rural_schools * reference_teacher_price * type_opex,
    
    #---- Total por item - Cenário 3 ----
    
    sce3_devt_dim_2_price_device_teacher = dim_2_price_device_teacher,
    sce3_devt_dim_2_price_device_school = dim_2_price_device_school,
    sce3_devt_dim_2_price_multimedia_projector = dim_2_price_multimedia_projector
    
  )
  




#----------------------
#- Total por dimensão - 
#----------------------

# Calcula o valor total por dimensão
base_parametros_calculadora <- base_parametros_calculadora %>% 
  mutate(
    
    #---- Total por dimensão - Cenário 0 ----
    
    dim_1_total = (dim_1_price_fiber_exp + dim_1_price_satellite_service + dim_1_price_fiber_link + dim_1_price_access_point + dim_1_price_firewall + dim_1_price_nobreak + dim_1_price_switch + dim_1_price_rack),
    
    dim_2_total_min = (dim_2_price_device_student_min + dim_2_price_device_teacher + dim_2_price_device_school + dim_2_price_charging_cart_min + dim_2_price_multimedia_projector +dim_2_price_headphones_min_scenario),
    dim_2_total_mild = (dim_2_price_device_student_mild + dim_2_price_device_teacher + dim_2_price_device_school + dim_2_price_charging_cart_mild + dim_2_price_multimedia_projector + dim_2_price_headphones_mild_scenario),
    dim_2_total_comprehensive = (dim_2_price_device_student_comprehensive + dim_2_price_device_teacher + dim_2_price_device_school + dim_2_price_charging_cart_comprehensive + dim_2_price_multimedia_projector + dim_2_price_headphones_comprehensive),
    
    dim_3_total_online_60 = (dim_3_price_diagnostic_tool + dim_3_price_lms_platform + dim_3_price_specialist_trainers_scenario_online60 + dim_3_price_course_for_trainers_scenario_online60 + dim_3_price_training_design_scenario_online60),
    dim_3_total_online_40 = (dim_3_price_diagnostic_tool + dim_3_price_lms_platform + dim_3_price_specialist_trainers_scenario_online40 + dim_3_price_course_for_trainers_scenario_online40 + dim_3_price_training_design_scenario_online40),
    dim_3_total_inperson_40 = (dim_3_price_diagnostic_tool + dim_3_price_lms_platform + dim_3_price_specialist_trainers_scenario_inperson40 + dim_3_price_course_for_trainers_scenario_inperson40 + dim_3_price_training_design_scenario_inperson40),
    
    dim_4_total = (dim_4_price_teaching_learning_platform + dim_4_price_management_platform),
    dim_5_total = (dim_5_price_central_team + dim_5_price_regional_team + dim_5_price_local_team),
    
    #---- Total por item - Cenário 1 (Secondary schools) ----
    
    sce1_sec_dim_1_total = (sce1_sec_dim_1_price_fiber_exp + sce1_sec_dim_1_price_satellite_service + sce1_sec_dim_1_price_fiber_link + sce1_sec_dim_1_price_access_point + sce1_sec_dim_1_price_firewall + sce1_sec_dim_1_price_nobreak + sce1_sec_dim_1_price_switch + sce1_sec_dim_1_price_rack),
    
    sce1_sec_dim_2_total_min = (sce1_sec_dim_2_price_device_student_min + sce1_sec_dim_2_price_device_teacher + sce1_sec_dim_2_price_device_school + sce1_sec_dim_2_price_charging_cart_min + sce1_sec_dim_2_price_multimedia_projector + sce1_sec_dim_2_price_headphones_min_scenario),
    sce1_sec_dim_2_total_mild = (sce1_sec_dim_2_price_device_student_mild + sce1_sec_dim_2_price_device_teacher + sce1_sec_dim_2_price_device_school + sce1_sec_dim_2_price_charging_cart_mild + sce1_sec_dim_2_price_multimedia_projector + sce1_sec_dim_2_price_headphones_mild_scenario),
    sce1_sec_dim_2_total_comprehensive = (sce1_sec_dim_2_price_device_student_comprehensive + sce1_sec_dim_2_price_device_teacher +sce1_sec_dim_2_price_device_school +sce1_sec_dim_2_price_charging_cart_comprehensive + sce1_sec_dim_2_price_multimedia_projector + sce1_sec_dim_2_price_headphones_comprehensive),
    
    sce1_sec_dim_3_total_online_60 = (sce1_sec_dim_3_price_diagnostic_tool + sce1_sec_dim_3_price_lms_platform + sce1_sec_dim_3_price_specialist_trainers_scenario_online60 + sce1_sec_dim_3_price_course_for_trainers_scenario_online60 + sce1_sec_dim_3_price_training_design_scenario_online60),
    sce1_sec_dim_3_total_online_40 = (sce1_sec_dim_3_price_diagnostic_tool + sce1_sec_dim_3_price_lms_platform + sce1_sec_dim_3_price_specialist_trainers_scenario_online40 + sce1_sec_dim_3_price_course_for_trainers_scenario_online40 + sce1_sec_dim_3_price_training_design_scenario_online40),
    sce1_sec_dim_3_total_inperson_40 = (sce1_sec_dim_3_price_diagnostic_tool + sce1_sec_dim_3_price_lms_platform + sce1_sec_dim_3_price_specialist_trainers_scenario_inperson40 + sce1_sec_dim_3_price_course_for_trainers_scenario_inperson40 + sce1_sec_dim_3_price_training_design_scenario_inperson40),
    
    sce1_sec_dim_4_total = (sce1_sec_dim_4_price_teaching_learning_platform + sce1_sec_dim_4_price_management_platform),
    sce1_sec_dim_5_total = (sce1_sec_dim_5_price_central_team + sce1_sec_dim_5_price_regional_team + sce1_sec_dim_5_price_local_team),
    
    #---- Total por item - Cenário 2 ----    
    
    sce2_rur_dim_1_total = (sce2_rur_dim_1_price_fiber_exp + sce2_rur_dim_1_price_satellite_service + sce2_rur_dim_1_price_fiber_link + sce2_rur_dim_1_price_access_point + sce2_rur_dim_1_price_firewall + sce2_rur_dim_1_price_nobreak + sce2_rur_dim_1_price_switch + sce2_rur_dim_1_price_rack),
    
    sce2_rur_dim_2_total_min = (sce2_rur_dim_2_price_device_student_min + sce2_rur_dim_2_price_device_teacher + sce2_rur_dim_2_price_device_school + sce2_rur_dim_2_price_charging_cart_min + sce2_rur_dim_2_price_multimedia_projector + sce2_rur_dim_2_price_headphones_min_scenario),
    sce2_rur_dim_2_total_mild = (sce2_rur_dim_2_price_device_student_mild + sce2_rur_dim_2_price_device_teacher + sce2_rur_dim_2_price_device_school + sce2_rur_dim_2_price_charging_cart_mild + sce2_rur_dim_2_price_multimedia_projector + sce2_rur_dim_2_price_headphones_mild_scenario),
    sce2_rur_dim_2_total_comprehensive = (sce2_rur_dim_2_price_device_student_comprehensive + sce2_rur_dim_2_price_device_teacher + sce2_rur_dim_2_price_device_school + sce2_rur_dim_2_price_charging_cart_comprehensive + sce2_rur_dim_2_price_multimedia_projector + sce2_rur_dim_2_price_headphones_comprehensive),
    
    sce2_rur_dim_3_total_online_60 = (sce2_rur_dim_3_price_diagnostic_tool + sce2_rur_dim_3_price_lms_platform + sce2_rur_dim_3_price_specialist_trainers_scenario_online60 + sce2_rur_dim_3_price_course_for_trainers_scenario_online60 + sce2_rur_dim_3_price_training_design_scenario_online60),
    sce2_rur_dim_3_total_online_40 = (sce2_rur_dim_3_price_diagnostic_tool + sce2_rur_dim_3_price_lms_platform + sce2_rur_dim_3_price_specialist_trainers_scenario_online40 + sce2_rur_dim_3_price_course_for_trainers_scenario_online40 + sce2_rur_dim_3_price_training_design_scenario_online40),
    sce2_rur_dim_3_total_inperson_40 = (sce2_rur_dim_3_price_diagnostic_tool + sce2_rur_dim_3_price_lms_platform + sce2_rur_dim_3_price_specialist_trainers_scenario_inperson40 + sce2_rur_dim_3_price_course_for_trainers_scenario_inperson40 + sce2_rur_dim_3_price_training_design_scenario_inperson40),
    
    sce2_rur_dim_4_total = (sce2_rur_dim_4_price_teaching_learning_platform + sce2_rur_dim_4_price_management_platform),
    sce2_rur_dim_5_total = (sce2_rur_dim_5_price_central_team + sce2_rur_dim_5_price_regional_team + sce2_rur_dim_5_price_local_team),
    
    #---- Total por item - Cenário 3 ----
    sce3_devt_dim_2_total = (sce3_devt_dim_2_price_device_teacher + sce3_devt_dim_2_price_device_school + sce3_devt_dim_2_price_multimedia_projector) / 1e6
     
  )







#-----------------
#- Exporta dados - 
#-----------------

#data.table::fwrite(x = base_parametros_calculadora, file = here("data", "gold", "base_parametros_calculadora.csv"))
#writexl::write_xlsx(x = base_parametros_calculadora, path = here("data", "gold", "base_parametros_calculadora.xlsx"))

#----------------------------
#- Base de dados com fontes - 
#----------------------------

base_fontes <- df_product_2_final
lista_rural <- c("Bahamas","Barbados","Bolivia (Plurinational State of)","Dominican Republic","El Salvador","Guatemala","Guyana","Haiti","Jamaica","Mexico","Nicaragua","Trinidad and Tobago","Venezuela (Bolivarian Republic of)")
base_fontes$rural_schools <- ifelse(
  base_fontes$country %in% lista_rural, "Estimated based on Latam average", ""
)

writexl::write_xlsx(x = base_fontes, path = here("data", "gold", "base_fontes.xlsx"))




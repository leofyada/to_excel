#--------------------------
#- Importa bases de dados - 
#--------------------------

# Importa bibliotecas por meio do script "config.R"
source("code/config.R")
# Autentica à conta do Google Drive onde estão os arquivos
#googledrive::drive_auth()

# 1. COUNTRY GROUPS

# Comment: this is a way of downloading files from Google Drive. It's important
# to upload the files to Google Drive before running this code. 

# Downloading file 
#googledrive::drive_download(file = "Country_Groups.xlsx", path = here("data", "raw_data", "Country_Groups.xlsx"), overwrite = T)
# Importing to R environment 
df_country_groups <- readxl::read_xlsx(path = here("data", "raw_data", "Country_Groups.xlsx"))

# 2. DATA-UIS-UNESCO-Converted to excel

# a-) Downloading and importing data [Enrolment in pre-primary education]
caminho_preprimary_enrolment <- here("data", "raw_data", "preprimary_enrolment.xlsx")
#googledrive::drive_download(file = "Enrolment in pre-primary education, both sexes (number) - Matrículas educação infantil.xlsx", path = caminho_preprimary_enrolment, overwrite = T)
df_enrolment_preprimary_education <- readxl::read_xlsx(path = caminho_preprimary_enrolment, skip = 3)
df_enrolment_preprimary_education <- df_enrolment_preprimary_education %>% mutate(type = "enrolment_preprimary_education")
rm(caminho_preprimary_enrolment)

# b-) Downloading and importing data [Enrolment in primary education]
caminho_primary_enrolment <- here("data", "raw_data", "primary_enrolment.xlsx")
#googledrive::drive_download(file = "Enrolment in primary education, both sexes (number) - Matrículas educação fundamental.xlsx", path=caminho_primary_enrolment, overwrite = T)
df_enrolment_primary_education <- readxl::read_xlsx(path = caminho_primary_enrolment, skip = 3)
df_enrolment_primary_education <- df_enrolment_primary_education %>% mutate(type = "enrolment_primary_education")
rm(caminho_primary_enrolment)

# c-) Downloading and importing data [Enrolment in secondary education]
caminho_secondary_enrolment <- here("data", "raw_data", "secondary_enrolment.xlsx")
#googledrive::drive_download(file = "Enrolment in secondary education, both sexes (number) - Matrículas ensino médio.xlsx", path=caminho_secondary_enrolment, overwrite = T)
df_enrolment_secondary_education <- readxl::read_xlsx(path=caminho_secondary_enrolment, skip = 3)
df_enrolment_secondary_education <- df_enrolment_secondary_education %>% mutate(type = "enrolment_secondary_education")
rm(caminho_secondary_enrolment)

# d-) Downloading and importing data [Teachers in pre-primary education]
caminho_preprimary_teacher <- here("data", "raw_data", "preprimary_teacher.xlsx")
#googledrive::drive_download(file = "Teachers in pre-primary education, both sexes (number)-Professores-educação-infantil-por-país.xlsx", path=caminho_preprimary_teacher, overwrite = T)
df_teacher_preprimary_education <- readxl::read_xlsx(path = caminho_preprimary_teacher, skip = 3)
df_teacher_preprimary_education <- df_teacher_preprimary_education %>% mutate(type = "teacher_preprimary_education")
rm(caminho_preprimary_teacher)

# e-) Downloading and importing data [Teachers in primary education]
caminho_primary_teacher <- here("data", "raw_data", "primary_teacher.xlsx")
#googledrive::drive_download(file = "Teachers in primary education, both sexes (number) - Professores no ensino fundamental.xlsx", path = caminho_primary_teacher, overwrite = T)
df_teacher_primary_education <- readxl::read_xlsx(path = caminho_primary_teacher, skip = 3)
df_teacher_primary_education <- df_teacher_primary_education %>% mutate(type = "teacher_primary_education")
rm(caminho_primary_teacher)

# f-) Downloading and importing data [Teachers in secondary education]
caminho_secondary_teacher <- here("data", "raw_data", "secondary_teacher.xlsx")
#googledrive::drive_download(file = "Teachers in secondary education, both sexes (number) - Professores no ensino médio.xlsx", path=caminho_secondary_teacher, overwrite = T)
df_teacher_secondary_education <- readxl::read_xlsx(path = caminho_secondary_teacher, skip = 3)
df_teacher_secondary_education <- df_teacher_secondary_education %>% mutate(type = "teacher_secondary_education")
rm(caminho_secondary_teacher)

# Union of dataframes 
df_unesco <- rbind(df_enrolment_preprimary_education, df_enrolment_primary_education, df_enrolment_secondary_education, df_teacher_preprimary_education, df_teacher_primary_education, df_teacher_secondary_education)
# Removing unused dataframes
rm(df_enrolment_preprimary_education, df_enrolment_primary_education, df_enrolment_secondary_education, df_teacher_preprimary_education, df_teacher_primary_education, df_teacher_secondary_education)

# Removing empty cells 
df_unesco_cleaned <- df_unesco %>% select(-`...2`) %>% drop_na()
# Wide to long
df_unesco_cleaned <- reshape2::melt(data = df_unesco_cleaned, id.vars = c("Time", "type"), variable.name = "ano")
# Convert value to numeric
df_unesco_cleaned$value <- as.numeric(df_unesco_cleaned$value)
# Change column names
colnames(df_unesco_cleaned) <- c("country", "data", "year", "value")

# 3. IDB GEOSPATIAL DATA

# a-) Downloading and importing data [ARG_total.csv]
caminho_arg_total <- here("data", "raw_data", "ARG_total.csv")
#googledrive::drive_download(file = "ARG_total.csv", path = caminho_arg_total, overwrite = T)
dt_arg_total <- read.csv(caminho_arg_total)
rm(caminho_arg_total)

# b-) Downloading and importing data [BLZ_total.csv]
caminho_blz_total <- here("data", "raw_data", "BLZ_total.csv")
#googledrive::drive_download(file = "BLZ_total.csv", path = caminho_blz_total, overwrite = T)
dt_blz_total <- data.table::fread(caminho_blz_total, encoding = "Latin-1")
rm(caminho_blz_total)

# c-) Downloading and importing data [BOL_total.csv]
caminho_bol_total <- here("data", "raw_data", "BOL_total.csv")
#googledrive::drive_download(file = "BOL_total.csv", path = caminho_bol_total, overwrite = T)
dt_bol_total <- data.table::fread(caminho_bol_total, encoding = "Latin-1")
rm(caminho_bol_total)

# d-) Downloading and importing data [CHL_total.csv]
caminho_chl_total <- here("data", "raw_data", "CHL_total.csv")
#googledrive::drive_download(file = "CHL_total.csv", path = caminho_chl_total, overwrite = T)
dt_chl_total <- data.table::fread(caminho_chl_total, encoding = "Latin-1")
rm(caminho_chl_total)

# e-) Downloading and importing data [COL_total.csv]
caminho_col_total <- here("data", "raw_data", "COL_total.csv")
#googledrive::drive_download(file = "COL_total.csv", path = caminho_col_total, overwrite = T)
dt_col_total <- data.table::fread(caminho_col_total, encoding = "Latin-1")
colnames(dt_col_total) <- c("V1", "adm0_pcode", "latitud", "longitud", "id_edificio", "area", "internet", "electricidad", "id_centro", "nombre_centro", "nivel_inicial", "nivel_primaria", "nivel_secundaria", "nivel_media", "turno_manana", "turno_tarde", "turno_otros", "turno_junica", "docentes_total", "matricula_total", "matricula_manana", "matricula_tarde", "matricula_otros", "matricula_junica", "tasa_promocion", "tasa_abandono", "tasa_repeticion", "nse_centro", "geometry", "geometry_2")
rm(caminho_col_total)

# f-) Downloading and importing data [DOM_total.csv]
caminho_dom_total <- here("data", "raw_data", "DOM_total.csv")
#googledrive::drive_download(file = "DOM_total.csv", path = caminho_dom_total, overwrite = T)
dt_dom_total <- data.table::fread(caminho_dom_total, encoding = "Latin-1")
rm(caminho_dom_total)

# g-) Downloading and importing data [ECU_total.csv]
caminho_ecu_total <- here("data", "raw_data", "ECU_total.csv")
#googledrive::drive_download(file = "ECU_total.csv", path=caminho_ecu_total, overwrite = T)
dt_ecu_total <- data.table::fread(caminho_ecu_total, encoding = "Latin-1")
rm(caminho_ecu_total)

# h-) Downloading and importing data [GTM_total.csv]
caminho_gtm_total <- here("data", "raw_data", "GTM_total.csv")
#googledrive::drive_download(file = "GTM_total.csv", path = caminho_gtm_total, overwrite = T)
dt_gtm_total <- data.table::fread(caminho_gtm_total, encoding = "Latin-1")
colnames(dt_gtm_total) <- c("V1", "adm0_pcode", "latitud", "longitud", "id_edificio", "id_centro", "nombre_centro", "nivel_inicial", "nivel_primaria", "nivel_secundaria", "nivel_media", "turno_manana", "turno_tarde", "turno_otros", "turno_junica", "geometry", "geometry_2")
rm(caminho_gtm_total)

# i-) Downloading and importing data [GUY_total.csv]
caminho_guy_total <- here("data", "raw_data", "GUY_total.csv")
#googledrive::drive_download(file = "GUY_total.csv", path = caminho_guy_total, overwrite = T)
dt_guy_total <- data.table::fread(caminho_guy_total, encoding = "Latin-1")
rm(caminho_guy_total)

# j-) Downloading and importing data [HND_total.csv]
caminho_hnd_total <- here("data", "raw_data", "HND_total.csv")
#googledrive::drive_download(file = "HND_total.csv", path = caminho_hnd_total, overwrite = T)
dt_hnd_total <- data.table::fread(caminho_hnd_total, encoding = "Latin-1")
rm(caminho_hnd_total)

# k-) Downloading and importing data [MEX_total.csv]
caminho_mex_total <- here("data", "raw_data", "MEX_total.csv")
#googledrive::drive_download(file = "MEX_total.csv", path = caminho_mex_total, overwrite = T)
dt_mex_total <- read.csv(caminho_mex_total)
rm(caminho_mex_total)

# l-) Downloading and importing data [PAN_total.csv]
caminho_pan_total <- here("data", "raw_data", "PAN_total.csv")
#googledrive::drive_download(file = "PAN_total.csv", path = caminho_pan_total, overwrite = T)
dt_pan_total <- data.table::fread(caminho_pan_total, encoding = "Latin-1")
rm(caminho_pan_total)

# m-) Downloading and importing data [PER_total.csv]
caminho_per_total <- here("data", "raw_data", "PER_total.csv")
#googledrive::drive_download(file = "PER_total.csv", path = caminho_per_total, overwrite = T)
dt_per_total <- data.table::fread(caminho_per_total, encoding = "Latin-1")
rm(caminho_per_total)

# n-) Downloading and importing data [PRY_total.csv]
caminho_pry_total <- here("data", "raw_data", "PRY_total.csv")
#googledrive::drive_download(file = "PRY_total.csv", path = caminho_pry_total, overwrite = T)
dt_pry_total <- data.table::fread(caminho_pry_total, encoding = "Latin-1")
colnames(dt_pry_total) <- c("V1", "adm0_pcode", "latitud", "longitud", "id_edificio", "area", "id_centro", "nombre_centro", "nivel_inicial", "nivel_primaria", "nivel_secundaria", "nivel_media", "matricula_total", "geometry", "geometry_2"  )
rm(caminho_pry_total)

# o-) Downloading and importing data [SLV_total.csv]
caminho_slv_total <- here("data", "raw_data", "SLV_total.csv")
#googledrive::drive_download(file = "SLV_total.csv", path = caminho_slv_total, overwrite = T)
dt_slv_total <- data.table::fread(caminho_slv_total, encoding = "Latin-1")
rm(caminho_slv_total)

# p-) Downloading and importing data [SUR_total.csv]
caminho_sur_total <- here("data", "raw_data", "SUR_total.csv")
#googledrive::drive_download(file = "SUR_total.csv", path = caminho_sur_total, overwrite = T)
dt_sur_total <- data.table::fread(caminho_sur_total, encoding = "Latin-1")
rm(caminho_sur_total)

# q-) Downloading and importing data [URY_total.csv]
caminho_ury_total <- here("data", "raw_data", "URY_total.csv")
#googledrive::drive_download(file = "URY_total.csv", path = caminho_ury_total, overwrite = T)
dt_ury_total <- data.table::fread(caminho_ury_total, encoding = "Latin-1")
colnames(dt_ury_total) <- c("V1", "adm0_pcode", "latitud", "longitud", "id_edificio", "area", "id_centro", "nombre_centro", "nivel_inicial", "nivel_primaria", "nivel_secundaria", "nivel_media", "turno_manana", "turno_tarde", "turno_otros", "turno_junica", "geometry", "geometry_2")
rm(caminho_ury_total)

# 4. CIMA - Based on the previous version of the calculator
#googledrive::drive_download(file = "Escenarios costeo todos los países - marzo 24.xlsx", path = here("data", "raw_data", "cima.xlsx"), overwrite = T)
df_cima_latam <- readxl::read_xlsx( here("data", "raw_data", "cima.xlsx"), sheet = "Todo Latam", skip = 3)
df_cima_latam <- df_cima_latam %>% filter(!(is.na(País)) & País != "TOTALES") %>% select(País, `Total de escuelas`, `Número de estudantes por etapa (primaria)`, `Número de estudantes por etapa (sec)`, `Número de docentes primaria`, `Número de docentes secundaria`)
colnames(df_cima_latam) <- c("no_pais", "number_of_schools", "number_of_students_primary", "number_of_students_secondary", "number_of_teachers_primary", "number_of_teachers_secondary")

df_cima_latam$number_of_schools <- as.integer(df_cima_latam$number_of_schools)
df_cima_latam$number_of_students_primary <- as.integer(df_cima_latam$number_of_students_primary)
df_cima_latam$number_of_students_secondary <- as.integer(df_cima_latam$number_of_students_secondary)
df_cima_latam$number_of_teachers_primary <- as.integer(df_cima_latam$number_of_teachers_primary)
df_cima_latam$number_of_teachers_secondary <- as.integer(df_cima_latam$number_of_teachers_secondary)

#-------------------------------------
#- Constrói a base de dados por país - 
#-------------------------------------

# Downloading Costa Rica data of 2024
#googledrive::drive_download(file = "NominaCentrosEducativos2024 - Costa Rica.xlsx", path = here("data", "raw_data", "costa_rica_2024.xlsx"), overwrite = T)
#googledrive::drive_download(file = "microdados_ed_basica_2024.csv", path = here("data", "raw_data", "microdados_ed_basica_2024.csv"), overwrite = T)
dt_censo_2024 <- data.table::fread(here("data", "raw_data", "microdados_ed_basica_2024.csv"), encoding = "Latin-1")
dt_censo_2024_filtrada <- dt_censo_2024[TP_SITUACAO_FUNCIONAMENTO==1 & TP_DEPENDENCIA %in% c(1, 2, 3)]

#### ARGENTINA - OK (WITH SOURCE AND OUTDATED DUMMY VARIABLE) ####

# SOURCE: https://data.educacion.gob.ar/nivel/secundario-comun [p. 7] - accessed in may 7 2025
# NUMBER OF TEACHERS IN SECONDARY EDUCATION - 170.311 (Año: 2021 | Sector: Estatal)

# Number of Schools
arg_schools <- nrow(dt_arg_total)
# Students of Argentina
arg_students <- df_unesco_cleaned %>% filter(country=="Argentina" & year==2021 & data %in% c("enrolment_preprimary_education", "enrolment_primary_education", "enrolment_secondary_education"))
# Teachers of Argentina
arg_teachers <- df_unesco_cleaned %>% filter(country=="Argentina" & year==2021 & data %in% c("teacher_preprimary_education", "teacher_primary_education", "teacher_secondary_education"))

# Data of Argentina
df_argentina <- data.frame(
  country = "Argentina",
  country_acronym = as.character(df_country_groups %>% filter(country_name=="Argentina") %>% select(country_acronym)),
  
  number_of_schools = arg_schools,
  source_number_of_schools = "GEOSPATIAL DATA REPOSITORY - IDB",
  outdated_number_of_schools = 0,
  
  number_of_students = as.integer(sum(arg_students$value, na.rm=T)),
  source_number_of_students = "DATA UIS - 2021",
  outdated_number_of_students = 1,
  
  number_of_students_preprimary = as.integer(arg_students %>% filter(data=="enrolment_preprimary_education") %>% select(value)),
  source_number_of_students_preprimary = "DATA UIS - 2021",
  outdated_number_of_students_preprimary = 1,
  
  number_of_students_primary = as.integer(arg_students %>% filter(data=="enrolment_primary_education") %>% select(value)),
  source_number_of_students_primary = "DATA UIS - 2021",
  outdated_number_of_students_primary = 1,
  
  number_of_students_secondary = as.integer(arg_students %>% filter(data=="enrolment_secondary_education") %>% select(value)),
  source_number_of_students_secondary = "DATA UIS - 2021",
  outdated_number_of_students_secondary = 1,
  
  number_of_teachers = as.integer(sum(arg_teachers$value, na.rm=T) + 170311),
  source_number_of_teachers = "DATA UIS - 2021 | GOB ARGENTINA - 2021",
  outdated_number_of_teachers = 1,
  
  number_of_teachers_preprimary = as.integer(arg_teachers %>% filter(data=="teacher_preprimary_education") %>% select(value)),
  source_number_of_teachers_preprimary = "DATA UIS - 2021",
  outdated_number_of_teachers_preprimary = 1,
  
  number_of_teachers_primary = as.integer(arg_teachers %>% filter(data=="teacher_primary_education") %>% select(value)),
  source_number_of_teachers_primary = "DATA UIS - 2021",
  outdated_number_of_teachers_primary = 1,
  
  number_of_teachers_secondary = as.integer(170311),
  source_number_of_teachers_secondary = "GOB ARGENTINA - 2021",
  outdated_number_of_teachers_secondary = 1,
  
  number_of_urban_schools = nrow(dt_arg_total %>% filter(area==1)),
  source_number_of_urban_schools = "GEOSPATIAL DATA REPOSITORY - IDB",
  outdated_number_of_urban_schools = 0,
  
  number_of_rural_schools = nrow(dt_arg_total %>% filter(area==0)),
  source_number_of_rural_schools = "GEOSPATIAL DATA REPOSITORY - IDB",
  outdated_number_of_rural_schools = 0
  
)

# Removing unnecessary data
rm(arg_schools, arg_students, arg_teachers, dt_arg_total)

#### BAHAMAS - OK (WITH SOURCE AND OUTDATED DUMMY VARIABLE) #####

# SOURCE: https://www.bahamas.gov.bs/wps/portal/public/About%20Us/Bahamian%20Education%20System/!ut/p/b1/vZXJjqtIEEW_pT6AIiETMEvAYMBMZjDDBpnBTMbGNmb6-uduldRqdb-qzRMZq5Ru6oSOIpR4hAd4dD0NVXHqq9v1dPnrHtHxgQcHR2MhBxDFAU6QFO1AeIQnAdzHAzdE7BYpo6gksVA8a9oRC_3e2SqmFvKOm7qEEOoEOHv-EQ922nbwGW8uUw7Czvfsequot5B-DVR6PniNqOiXFyvOwr0fS35KrrzglSU1bhDlTpin0ZSZX8TZF1k7Sa9mrYMpsOYkTXKMvGaYkO0ygPS000wn7urkMjZBgXHdDZtsRE6M35GzX9v0abxnu64tXJF0CkJSeYJrGsmXikTTnwjYyXBO45Hfl0VNviWEbwngN4cD3zqC6Ov9N4EfHUd_RyDY6RyHNvqOAjRQPEvjLEYmTQS-At-1-C3EZfDwHWB-3ySFu3gAUOzUc6cszWLXYDJ0N4WG2E5ABITbMK7hioTT24Ze6xA0YO63_N5IRNCDUjf6s5UdbY_nhEcWGdQPQBWtDaTXBsKVgTxYG0iuDVx7SnlmZaD959dCxaMqaT_HtP0EnwxiEKQZGhIksaFphB_V8DVtC6UURTsunRdpZZHj01Y2nRNzqrbP3HFGUaq32Dl1HnlitGjO2WSvUrxehLu-Z_UNIssDz04V4l4nR95zS_hyXCHjWqnVbHaKn7f2zNFBxF6tTQ6s9OmqwLTZi8fOz2PYPO-PBL6jlS0EJP3SUgKV5mNOzCE-RkTOwa1EULJccB-4Id_a_AeFzP-swTg5i2Ho3qQRImEY4vJWKCkOG70VciPhemNfv3UuHDSW94_RW18Kt_UZu1s_AYm1gXBloIHWBq6t1PjzSv-9dxCQGwKyAFIkICgSP9YeNp0Kfdpa5NFfRjJ9Zo0DhkR1pO1emhdehVY5kmQkz7RpXZnqZgsXsL-ysSsC-BLkBy04RhOeyOmR2L48yPu0TmBra-bUndUbo1RCWg25r6fNtbDNDCpHWoGZzDLWY4wzcl-Gs9MzQTPcjiq7YXN7M1GGabo6UYNLkGBW5Qmq26ONFzHHDhbZmVCZoWPCrcaq7H24C1xgiNjLo2DvlFYuxfJyiBfheBMnHwlVIkVebUunOYyW4nixOO_ka7cz3Jfxogeg9WF8iNDSmj7etcNeo215Cf5T-W5o_ynu4-MXEbFROg!!/dl4/d5/L2dBISEvZ0FBIS9nQSEh/
# [Table 1] - accessed in may 7 2025
# NUMBER OF SCHOOLS - 170 

# Number of Schools
bhs_schools <- 170
# Students of Bahamas
bhs_students <- df_unesco_cleaned %>% filter(country=="Bahamas" & year==2023 & data %in% c("enrolment_preprimary_education", "enrolment_primary_education", "enrolment_secondary_education"))
# Teachers of Bahamas
bhs_teachers <- df_unesco_cleaned %>% filter(country=="Bahamas" & year==2023 & data %in% c("teacher_preprimary_education", "teacher_primary_education", "teacher_secondary_education"))

# Data of Bahamas
df_bahamas <- data.frame(
  country = "Bahamas",
  country_acronym = as.character(df_country_groups %>% filter(country_name=="Bahamas") %>% select(country_acronym)),
  
  number_of_schools = as.integer(bhs_schools),
  source_number_of_schools = "BAHAMAS GOV - 2015",
  outdated_number_of_schools = 1,
  
  number_of_students = as.integer(sum(bhs_students$value, na.rm=T)),
  source_number_of_students = "DATA UIS - 2023",
  outdated_number_of_students = 0,
  
  number_of_students_preprimary = as.integer(bhs_students %>% filter(data=="enrolment_preprimary_education") %>% select(value)),
  source_number_of_students_preprimary = "DATA UIS - 2023",
  outdated_number_of_students_preprimary = 0,
  
  number_of_students_primary = as.integer(bhs_students %>% filter(data=="enrolment_primary_education") %>% select(value)),
  source_number_of_students_primary = "DATA UIS - 2023",
  outdated_number_of_students_primary = 0,
  
  number_of_students_secondary = as.integer(bhs_students %>% filter(data=="enrolment_secondary_education") %>% select(value)),
  source_number_of_students_secondary = "DATA UIS - 2023",
  outdated_number_of_students_secondary = 0,
  
  number_of_teachers = as.integer(sum(bhs_teachers$value, na.rm=T)),
  source_number_of_teachers = "DATA UIS - 2023",
  outdated_number_of_teachers = 0,
  
  number_of_teachers_preprimary = as.integer(bhs_teachers %>% filter(data=="teacher_preprimary_education") %>% select(value)),
  source_number_of_teachers_preprimary = "DATA UIS - 2023",
  outdated_number_of_teachers_preprimary = 0,
  
  number_of_teachers_primary = as.integer(bhs_teachers %>% filter(data=="teacher_primary_education") %>% select(value)),
  source_number_of_teachers_primary = "DATA UIS - 2023",
  outdated_number_of_teachers_primary = 0,
  
  number_of_teachers_secondary = as.integer(bhs_teachers %>% filter(data=="teacher_secondary_education") %>% select(value)),
  source_number_of_teachers_secondary = "DATA UIS - 2023",
  outdated_number_of_teachers_secondary = 0,
  
  number_of_urban_schools = NA,
  source_number_of_urban_schools = "No data available",
  outdated_number_of_urban_schools = "No data available",
  
  number_of_rural_schools = NA,
  source_number_of_rural_schools = "No data available",
  outdated_number_of_rural_schools = "No data available"
  
)

# Removing unnecessary data
rm(bhs_schools, bhs_students, bhs_teachers)

#### BARBADOS - OK (WITH SOURCE AND OUTDATED DUMMY VARIABLE) #####

# [SOURCE]: https://mes.gov.bb/Welcome-Stamp/Barbados-Education-System.aspx - accessed in may 7 2025
# NUMBER OF SCHOOLS - 68 primary schools | 4 special schools | 12 nursery schools | 21 secondary schools = 105 schools

# Number of Schools
brb_schools <- 105
# Students of Barbados
brb_students <- df_unesco_cleaned %>% filter(country=="Barbados" & year==2023 & data %in% c("enrolment_preprimary_education", "enrolment_primary_education", "enrolment_secondary_education"))
# Teachers of Barbados
brb_teachers <- df_unesco_cleaned %>% filter(country=="Barbados" & year==2023 & data %in% c("teacher_preprimary_education", "teacher_primary_education", "teacher_secondary_education"))

# Data of Barbados
df_barbados <- data.frame(
  country = "Barbados",
  country_acronym = as.character(df_country_groups %>% filter(country_name=="Barbados") %>% select(country_acronym)),
  
  number_of_schools = as.integer(brb_schools),
  source_number_of_schools = "BARBADOS GOV - 2024",
  outdated_number_of_schools = 0,
  
  number_of_students = as.integer(sum(brb_students$value, na.rm=T)),
  source_number_of_students = "DATA UIS - 2023",
  outdated_number_of_students = 0,
  
  number_of_students_preprimary = as.integer(brb_students %>% filter(data=="enrolment_preprimary_education") %>% select(value)),
  source_number_of_students_preprimary = "DATA UIS - 2023",
  outdated_number_of_students_preprimary = 0,
  
  number_of_students_primary = as.integer(brb_students %>% filter(data=="enrolment_primary_education") %>% select(value)),
  source_number_of_students_primary = "DATA UIS - 2023",
  outdated_number_of_students_primary = 0,
  
  number_of_students_secondary = as.integer(brb_students %>% filter(data=="enrolment_secondary_education") %>% select(value)),
  source_number_of_students_secondary = "DATA UIS - 2023",
  outdated_number_of_students_secondary = 0,
  
  number_of_teachers = as.integer(sum(brb_teachers$value, na.rm=T)),
  source_number_of_teachers = "DATA UIS - 2023",
  outdated_number_of_teachers = 0,
  
  number_of_teachers_preprimary = as.integer(brb_teachers %>% filter(data=="teacher_preprimary_education") %>% select(value)),
  source_number_of_teachers_preprimary = "DATA UIS - 2023",
  outdated_number_of_teachers_preprimary = 0,
  
  number_of_teachers_primary = as.integer(brb_teachers %>% filter(data=="teacher_primary_education") %>% select(value)),
  source_number_of_teachers_primary = "DATA UIS - 2023",
  outdated_number_of_teachers_primary = 0,
  
  number_of_teachers_secondary = as.integer(brb_teachers %>% filter(data=="teacher_secondary_education") %>% select(value)),
  source_number_of_teachers_secondary = "DATA UIS - 2023",
  outdated_number_of_teachers_secondary = 0,
  
  number_of_urban_schools = NA,
  source_number_of_urban_schools = "No data available",
  outdated_number_of_urban_schools = "No data available",
  
  number_of_rural_schools = NA,
  source_number_of_rural_schools = "No data available",
  outdated_number_of_rural_schools = "No data available"
  
)

# Removing unnecessary data
rm(brb_schools, brb_students, brb_teachers)

#### BELIZE - OK (WITH SOURCE AND OUTDATED DUMMY VARIABLE) #####

# Number of Schools
blz_schools <- nrow(dt_blz_total)
# Students of Belize
blz_students <- df_unesco_cleaned %>% filter(country=="Belize" & year==2023 & data %in% c("enrolment_preprimary_education", "enrolment_primary_education", "enrolment_secondary_education"))
# Teachers of Belize
blz_teachers <- df_unesco_cleaned %>% filter(country=="Belize" & year==2023 & data %in% c("teacher_preprimary_education", "teacher_primary_education", "teacher_secondary_education"))

# Data of Belize
df_belize <- data.frame(
  country = "Belize",
  country_acronym = as.character(df_country_groups %>% filter(country_name=="Belize") %>% select(country_acronym)),
  
  number_of_schools = blz_schools,
  source_number_of_schools = "GEOSPATIAL DATA REPOSITORY - IDB",
  outdated_number_of_schools = 0,
  
  number_of_students = as.integer(sum(blz_students$value, na.rm=T)),
  source_number_of_students = "DATA UIS - 2023",
  outdated_number_of_students = 0,
  
  number_of_students_preprimary = as.integer(blz_students %>% filter(data=="enrolment_preprimary_education") %>% select(value)),
  source_number_of_students_preprimary = "DATA UIS - 2023",
  outdated_number_of_students_preprimary = 0,
  
  number_of_students_primary = as.integer(blz_students %>% filter(data=="enrolment_primary_education") %>% select(value)),
  source_number_of_students_primary = "DATA UIS - 2023",
  outdated_number_of_students_primary = 0,
  
  number_of_students_secondary = as.integer(blz_students %>% filter(data=="enrolment_secondary_education") %>% select(value)),
  source_number_of_students_secondary = "DATA UIS - 2023",
  outdated_number_of_students_secondary = 0,
  
  number_of_teachers = as.integer(sum(blz_teachers$value, na.rm=T)),
  source_number_of_teachers = "DATA UIS - 2023",
  outdated_number_of_teachers = 0,
  
  number_of_teachers_preprimary = as.integer(blz_teachers %>% filter(data=="teacher_preprimary_education") %>% select(value)),
  source_number_of_teachers_preprimary = "DATA UIS - 2023",
  outdated_number_of_teachers_preprimary = 0,
  
  number_of_teachers_primary = as.integer(blz_teachers %>% filter(data=="teacher_primary_education") %>% select(value)),
  source_number_of_teachers_primary = "DATA UIS - 2023",
  outdated_number_of_teachers_primary = 0,
  
  number_of_teachers_secondary = as.integer(blz_teachers %>% filter(data=="teacher_secondary_education") %>% select(value)),
  source_number_of_teachers_secondary = "DATA UIS - 2023",
  outdated_number_of_teachers_secondary = 0,
  
  number_of_urban_schools = nrow(dt_blz_total %>% filter(area==1)),
  source_number_of_urban_schools = "GEOSPATIAL DATA REPOSITORY - IDB",
  outdated_number_of_urban_schools = 0,
  
  number_of_rural_schools = nrow(dt_blz_total %>% filter(area==0)),
  source_number_of_rural_schools = "GEOSPATIAL DATA REPOSITORY - IDB",
  outdated_number_of_rural_schools = 0
  
)

# Removing unnecessary data
rm(blz_schools, blz_students, blz_teachers, dt_blz_total)

#### BOLIVIA - OK (WITH SOURCE AND OUTDATED DUMMY VARIABLE) #####

# Number of Schools
bol_schools <- nrow(dt_bol_total)
# Students of Bolivia
bol_students <- df_unesco_cleaned %>% filter(country=="Bolivia (Plurinational State of)" & year==2023 & data %in% c("enrolment_preprimary_education", "enrolment_primary_education", "enrolment_secondary_education"))
# Teachers of Bolivia
bol_teachers <- df_unesco_cleaned %>% filter(country=="Bolivia (Plurinational State of)" & year==2023 & data %in% c("teacher_preprimary_education", "teacher_primary_education", "teacher_secondary_education"))

# Data of Bolivia
df_bolivia <- data.frame(
  country = "Bolivia (Plurinational State of)",
  country_acronym = as.character(df_country_groups %>% filter(country_name=="Bolivia (Plurinational State of)") %>% select(country_acronym)),
  
  number_of_schools = bol_schools,
  source_number_of_schools = "GEOSPATIAL DATA REPOSITORY - IDB",
  outdated_number_of_schools = 0,
  
  number_of_students = as.integer(sum(bol_students$value, na.rm=T)),
  source_number_of_students = "DATA UIS - 2023",
  outdated_number_of_students = 0,
  
  number_of_students_preprimary = as.integer(bol_students %>% filter(data=="enrolment_preprimary_education") %>% select(value)),
  source_number_of_students_preprimary = "DATA UIS - 2023",
  outdated_number_of_students_preprimary = 0,
  
  number_of_students_primary = as.integer(bol_students %>% filter(data=="enrolment_primary_education") %>% select(value)),
  source_number_of_students_primary = "DATA UIS - 2023",
  outdated_number_of_students_primary = 0,
  
  number_of_students_secondary = as.integer(bol_students %>% filter(data=="enrolment_secondary_education") %>% select(value)),
  source_number_of_students_secondary = "DATA UIS - 2023",
  outdated_number_of_students_secondary = 0,
  
  number_of_teachers = as.integer(sum(bol_teachers$value, na.rm=T)),
  source_number_of_teachers = "DATA UIS - 2023",
  outdated_number_of_teachers = 0,
  
  number_of_teachers_preprimary = as.integer(bol_teachers %>% filter(data=="teacher_preprimary_education") %>% select(value)),
  source_number_of_teachers_preprimary = "DATA UIS - 2023",
  outdated_number_of_teachers_preprimary = 0,
  
  number_of_teachers_primary = as.integer(bol_teachers %>% filter(data=="teacher_primary_education") %>% select(value)),
  source_number_of_teachers_primary = "DATA UIS - 2023",
  outdated_number_of_teachers_primary = 0,
  
  number_of_teachers_secondary = as.integer(bol_teachers %>% filter(data=="teacher_secondary_education") %>% select(value)),
  source_number_of_teachers_secondary = "DATA UIS - 2023",
  outdated_number_of_teachers_secondary = 0,
  
  number_of_urban_schools = NA,
  source_number_of_urban_schools = "No data available",
  outdated_number_of_urban_schools = "No data available",
  
  number_of_rural_schools = NA,
  source_number_of_rural_schools = "No data available",
  outdated_number_of_rural_schools = "No data available"
  
)

# Removing unnecessary data
rm(bol_schools, bol_students, bol_teachers, dt_bol_total)

#### BRAZIL - OK (WITH SOURCE AND OUTDATED DUMMY VARIABLE) #####

# Downloading Brazilian data of 2024

# SOURCE: https://download.inep.gov.br/dados_abertos/microdados_censo_escolar_2024.zip
# In the link above it's possible to analyse the dictionary: dicionário_dados_educação_básica.xlsx

# Number of Schools
bra_schools <- nrow(dt_censo_2024_filtrada)
# Students of Brazil
bra_students <- dt_censo_2024_filtrada %>% filter(!(is.na(QT_MAT_BAS))) %>% mutate(country="Brazil") %>% select(country, QT_MAT_INF, QT_MAT_FUND_AI, QT_MAT_FUND_AF, QT_MAT_MED)
bra_students <- bra_students %>% mutate(QT_MAT_SEC = QT_MAT_FUND_AF + QT_MAT_MED) %>% select (country, QT_MAT_INF, QT_MAT_FUND_AI, QT_MAT_SEC)
colnames(bra_students) <- c("country", "enrolment_preprimary_education", "enrolment_primary_education", "enrolment_secondary_education")
bra_students <- reshape2::melt(data = bra_students, id.vars = "country", variable.name = "data")
bra_students <- bra_students %>% group_by(data) %>% reframe(value = sum(value, na.rm=T))
# Teachers of Brasil
bra_teachers <- dt_censo_2024_filtrada %>% filter(!(is.na(QT_DOC_BAS))) %>% mutate(country="Brazil") %>% select(country, QT_DOC_INF, QT_DOC_FUND_AI, QT_DOC_FUND_AF, QT_DOC_MED)
bra_teachers <- bra_teachers %>% mutate(QT_DOC_SEC = QT_DOC_FUND_AF + QT_DOC_MED) %>% select (country, QT_DOC_INF, QT_DOC_FUND_AI, QT_DOC_SEC)
colnames(bra_teachers) <- c("country", "teacher_preprimary_education", "teacher_primary_education", "teacher_secondary_education")
bra_teachers <- reshape2::melt(data = bra_teachers, id.vars = "country", variable.name = "data")
bra_teachers <- bra_teachers %>% group_by(data) %>% reframe(value = sum(value, na.rm=T))


# Data of Brazil
df_brazil <- data.frame(
  country = "Brazil",
  country_acronym = as.character(df_country_groups %>% filter(country_name=="Brazil") %>% select(country_acronym)),
  
  number_of_schools = bra_schools,
  source_number_of_schools = "INEP - 2024",
  outdated_number_of_schools = 0,
  
  number_of_students = sum(bra_students$value, na.rm=T),
  source_number_of_students = "INEP - 2024",
  outdated_number_of_students = 0,
  
  number_of_students_preprimary = as.integer(bra_students %>% filter(data=="enrolment_preprimary_education") %>% select(value)),
  source_number_of_students_preprimary = "INEP - 2024",
  outdated_number_of_students_preprimary = 0,
  
  number_of_students_primary = as.integer(bra_students %>% filter(data=="enrolment_primary_education") %>% select(value)),
  source_number_of_students_primary = "INEP - 2024",
  outdated_number_of_students_primary = 0,
  
  number_of_students_secondary = as.integer(bra_students %>% filter(data=="enrolment_secondary_education") %>% select(value)),
  source_number_of_students_secondary = "INEP - 2024",
  outdated_number_of_students_secondary = 0,
  
  number_of_teachers = sum(bra_teachers$value, na.rm=T),
  source_number_of_teachers = "INEP - 2024",
  outdated_number_of_teachers = 0,
  
  number_of_teachers_preprimary = as.integer(bra_teachers %>% filter(data=="teacher_preprimary_education") %>% select(value)),
  source_number_of_teachers_preprimary = "INEP - 2024",
  outdated_number_of_teachers_preprimary = 0,
  
  number_of_teachers_primary = as.integer(bra_teachers %>% filter(data=="teacher_primary_education") %>% select(value)),
  source_number_of_teachers_primary = "INEP - 2024",
  outdated_number_of_teachers_primary = 0,
  
  number_of_teachers_secondary = as.integer(bra_teachers %>% filter(data=="teacher_secondary_education") %>% select(value)),
  source_number_of_teachers_secondary = "INEP - 2024",
  outdated_number_of_teachers_secondary = 0,
  
  number_of_urban_schools = nrow(dt_censo_2024_filtrada[TP_LOCALIZACAO==1]),
  source_number_of_urban_schools = "INEP - 2024",
  outdated_number_of_urban_schools = 0,
  
  number_of_rural_schools = nrow(dt_censo_2024_filtrada[TP_LOCALIZACAO==2]),
  source_number_of_rural_schools = "INEP - 2024",
  outdated_number_of_rural_schools = 0
  
)

# Removing unnecessary data
rm(bra_schools, bra_students, bra_teachers, dt_censo_2024, dt_censo_2024_filtrada)

#### CHILE - OK (WITH SOURCE AND OUTDATED DUMMY VARIABLE) #####

# Number of Schools
chl_schools <- nrow(dt_chl_total)
# Students of Chile
chl_students <- df_unesco_cleaned %>% filter(country=="Chile" & year==2022 & data %in% c("enrolment_preprimary_education", "enrolment_primary_education", "enrolment_secondary_education"))
# Teachers of Chile
chl_teachers <- df_unesco_cleaned %>% filter(country=="Chile" & year==2022 & data %in% c("teacher_preprimary_education", "teacher_primary_education", "teacher_secondary_education"))

# Data of Chile
df_chile <- data.frame(
  country = "Chile",
  country_acronym = as.character(df_country_groups %>% filter(country_name=="Chile") %>% select(country_acronym)),
  
  number_of_schools = chl_schools,
  source_number_of_schools = "GEOSPATIAL DATA REPOSITORY - IDB",
  outdated_number_of_schools = 0,
  
  number_of_students = as.integer(sum(chl_students$value, na.rm=T)),
  source_number_of_students = "DATA UIS - 2022",
  outdated_number_of_students = 0,
  
  number_of_students_preprimary = as.integer(chl_students %>% filter(data=="enrolment_preprimary_education") %>% select(value)),
  source_number_of_students_preprimary = "DATA UIS - 2022",
  outdated_number_of_students_preprimary = 0,
  
  number_of_students_primary = as.integer(chl_students %>% filter(data=="enrolment_primary_education") %>% select(value)),
  source_number_of_students_primary = "DATA UIS - 2022",
  outdated_number_of_students_primary = 0,
  
  number_of_students_secondary = as.integer(chl_students %>% filter(data=="enrolment_secondary_education") %>% select(value)),
  source_number_of_students_secondary = "DATA UIS - 2022",
  outdated_number_of_students_secondary = 0,
  
  number_of_teachers = as.integer(sum(chl_teachers$value, na.rm=T)),
  source_number_of_teachers = "DATA UIS - 2022",
  outdated_number_of_teachers = 0,
  
  number_of_teachers_preprimary = as.integer(chl_teachers %>% filter(data=="teacher_preprimary_education") %>% select(value)),
  source_number_of_teachers_preprimary = "DATA UIS - 2022",
  outdated_number_of_teachers_preprimary = 0,
  
  number_of_teachers_primary = as.integer(chl_teachers %>% filter(data=="teacher_primary_education") %>% select(value)),
  source_number_of_teachers_primary = "DATA UIS - 2022",
  outdated_number_of_teachers_primary = 0,
  
  number_of_teachers_secondary = as.integer(chl_teachers %>% filter(data=="teacher_secondary_education") %>% select(value)),
  source_number_of_teachers_secondary = "DATA UIS - 2022",
  outdated_number_of_teachers_secondary = 0,
  
  number_of_urban_schools = nrow(dt_chl_total %>% filter(area==1)),
  source_number_of_urban_schools = "GEOSPATIAL DATA REPOSITORY - IDB",
  outdated_number_of_urban_schools = 0,
  
  number_of_rural_schools = nrow(dt_chl_total %>% filter(area==0)),
  source_number_of_rural_schools = "GEOSPATIAL DATA REPOSITORY - IDB",
  outdated_number_of_rural_schools = 0
  
)

# Removing unnecessary data
rm(chl_schools, chl_students, chl_teachers, dt_chl_total)

#### COLOMBIA - OK (WITH SOURCE AND OUTDATED DUMMY VARIABLE) ####

# Number of Schools
col_schools <- nrow(dt_col_total)
# Students of Colombia
col_students <- df_unesco_cleaned %>% filter(country=="Colombia" & year==2022 & data %in% c("enrolment_preprimary_education", "enrolment_primary_education", "enrolment_secondary_education"))
# Teachers of Colombia
col_teachers <- df_unesco_cleaned %>% filter(country=="Colombia" & year==2022 & data %in% c("teacher_preprimary_education", "teacher_primary_education", "teacher_secondary_education"))

# Data of Colombia
df_colombia <- data.frame(
  country = "Colombia",
  country_acronym = as.character(df_country_groups %>% filter(country_name=="Colombia") %>% select(country_acronym)),
  
  number_of_schools = col_schools,
  source_number_of_schools = "GEOSPATIAL DATA REPOSITORY - IDB",
  outdated_number_of_schools = 0,
  
  number_of_students = as.integer(sum(col_students$value, na.rm=T)),
  source_number_of_students = "DATA UIS - 2022",
  outdated_number_of_students = 0,
  
  number_of_students_preprimary = as.integer(col_students %>% filter(data=="enrolment_preprimary_education") %>% select(value)),
  source_number_of_students_preprimary = "DATA UIS - 2022",
  outdated_number_of_students_preprimary = 0,
  
  number_of_students_primary = as.integer(col_students %>% filter(data=="enrolment_primary_education") %>% select(value)),
  source_number_of_students_primary = "DATA UIS - 2022",
  outdated_number_of_students_primary = 0,
  
  number_of_students_secondary = as.integer(col_students %>% filter(data=="enrolment_secondary_education") %>% select(value)),
  source_number_of_students_secondary = "DATA UIS - 2022",
  outdated_number_of_students_secondary = 0,
  
  number_of_teachers = as.integer(sum(col_teachers$value, na.rm=T)),
  source_number_of_teachers = "DATA UIS - 2022",
  outdated_number_of_teachers = 0,
  
  number_of_teachers_preprimary = as.integer(col_teachers %>% filter(data=="teacher_preprimary_education") %>% select(value)),
  source_number_of_teachers_preprimary = "DATA UIS - 2022",
  outdated_number_of_teachers_preprimary = 0,
  
  number_of_teachers_primary = as.integer(col_teachers %>% filter(data=="teacher_primary_education") %>% select(value)),
  source_number_of_teachers_primary = "DATA UIS - 2022",
  outdated_number_of_teachers_primary = 0,
  
  number_of_teachers_secondary = as.integer(col_teachers %>% filter(data=="teacher_secondary_education") %>% select(value)),
  source_number_of_teachers_secondary = "DATA UIS - 2022",
  outdated_number_of_teachers_secondary = 0,
  
  number_of_urban_schools = nrow(dt_col_total %>% filter(area==1)),
  source_number_of_urban_schools = "GEOSPATIAL DATA REPOSITORY - IDB",
  outdated_number_of_urban_schools = 0,
  
  number_of_rural_schools = nrow(dt_col_total %>% filter(area==0)),
  source_number_of_rural_schools = "GEOSPATIAL DATA REPOSITORY - IDB",
  outdated_number_of_rural_schools = 0
  
)

# Removing unnecessary data
rm(col_schools, col_students, col_teachers, dt_col_total)

#### COSTA RICA - OK (WITH SOURCE AND OUTDATED DUMMY VARIABLE) ####

df_costa_rica_preescolar_independ <- readxl::read_xlsx(here("data", "raw_data", "costa_rica_2024.xlsx"), sheet = "Preescolar Independ.", skip=5)
df_costa_rica_preescolar_independ <- df_costa_rica_preescolar_independ %>% filter(!(is.na(`NOMBRE DE LA INSTITUCION`))) %>% mutate(TIPO = "preescolar_independ") %>% select(`NOMBRE DE LA INSTITUCION`, DEPENDENCIA, ZONA, TIPO)
nrow(df_costa_rica_preescolar_independ) # 253

df_costa_rica_ciclos_I_II <- readxl::read_xlsx(here("data", "raw_data", "costa_rica_2024.xlsx"), sheet = "I y II Ciclos", skip=5)
df_costa_rica_ciclos_I_II <- df_costa_rica_ciclos_I_II %>% filter(!(is.na(`NOMBRE DE LA INSTITUCION`))) %>% mutate(TIPO="ciclos_I_II") %>% select(`NOMBRE DE LA INSTITUCION`, DEPENDENCIA, ZONA, TIPO)
nrow(df_costa_rica_ciclos_I_II) # 4017

df_costa_rica_colegios <- readxl::read_xlsx(here("data", "raw_data", "costa_rica_2024.xlsx"), sheet = "Colegios", skip=5)
df_costa_rica_colegios <- df_costa_rica_colegios %>% filter(!(is.na(`NOMBRE DE LA INSTITUCION`))) %>% mutate(TIPO="ciclos_I_II") %>% select(`NOMBRE DE LA INSTITUCION`, DEPENDENCIA, ZONA, TIPO)
nrow(df_costa_rica_colegios) # 998

df_costa_rica_cee <- readxl::read_xlsx(here("data", "raw_data", "costa_rica_2024.xlsx"), sheet = "C.E.E.", skip=4)
df_costa_rica_cee <- df_costa_rica_cee %>% filter(!(is.na(`NOMBRE DE LA INSTITUCION`))) %>% mutate(TIPO="cee") %>% select(`NOMBRE DE LA INSTITUCION`, DEPENDENCIA, ZONA, TIPO)
nrow(df_costa_rica_cee) # 26

df_costa_rica_noturna <- readxl::read_xlsx(here("data", "raw_data", "costa_rica_2024.xlsx"), sheet = "Esc. Nocturna", skip=4)
df_costa_rica_noturna <- df_costa_rica_noturna %>% mutate(DEPENDENCIA=NA, TIPO="noturna") %>% filter(!(is.na(`NOMBRE DE LA INSTITUCION`))) %>% select(`NOMBRE DE LA INSTITUCION`, DEPENDENCIA, ZONA, TIPO)
nrow(df_costa_rica_noturna) # 3

df_costa_rica_caipad <- readxl::read_xlsx(here("data", "raw_data", "costa_rica_2024.xlsx"), sheet = "CAIPAD", skip=4)
df_costa_rica_caipad <- df_costa_rica_caipad %>% mutate(DEPENDENCIA=NA, TIPO="caipad") %>% filter(!(is.na(`NOMBRE DE LA INSTITUCION`))) %>% select(`NOMBRE DE LA INSTITUCION`, DEPENDENCIA, ZONA, TIPO)
df_costa_rica_caipad %>% group_by(ZONA) %>% tally()
nrow(df_costa_rica_caipad) # 28

df_costa_rica_ipec <- readxl::read_xlsx(here("data", "raw_data", "costa_rica_2024.xlsx"), sheet = "IPEC", skip=5)
df_costa_rica_ipec <- df_costa_rica_ipec %>% mutate(DEPENDENCIA=NA, TIPO="ipec") %>% filter(!(is.na(`NOMBRE DE LA INSTITUCION`))) %>% select(`NOMBRE DE LA INSTITUCION`, DEPENDENCIA, ZONA, TIPO)
df_costa_rica_ipec %>% group_by(ZONA) %>% tally()
nrow(df_costa_rica_ipec) # 35

df_costa_rica_cindea <- readxl::read_xlsx(here("data", "raw_data", "costa_rica_2024.xlsx"), sheet = "CINDEA", skip=5)
df_costa_rica_cindea <- df_costa_rica_cindea %>% mutate(TIPO="cindea") %>% filter(!(is.na(`NOMBRE DE LA INSTITUCION`))) %>% select(`NOMBRE DE LA INSTITUCION`, DEPENDENCIA, ZONA, TIPO)
df_costa_rica_cindea %>% group_by(ZONA) %>% tally()
nrow(df_costa_rica_cindea) # 201

df_costa_rica_coned <- readxl::read_xlsx(here("data", "raw_data", "costa_rica_2024.xlsx"), sheet = "CONED", skip=4)
df_costa_rica_coned <- df_costa_rica_coned %>% mutate(DEPENDENCIA=NA, TIPO="coned") %>% filter(!(is.na(`NOMBRE DE LA INSTITUCION`))) %>% select(`NOMBRE DE LA INSTITUCION`, DEPENDENCIA, ZONA, TIPO)
df_costa_rica_coned %>% group_by(ZONA) %>% tally()
nrow(df_costa_rica_coned) # 16

df_costa_rica_schools <- rbind(df_costa_rica_preescolar_independ, df_costa_rica_ciclos_I_II, df_costa_rica_colegios, df_costa_rica_cee, df_costa_rica_noturna, df_costa_rica_caipad, df_costa_rica_ipec, df_costa_rica_cindea, df_costa_rica_coned)
df_costa_rica_schools <- df_costa_rica_schools %>% filter(DEPENDENCIA == "PUB" | DEPENDENCIA == "SUB" | is.na(DEPENDENCIA))
rm(df_costa_rica_preescolar_independ, df_costa_rica_ciclos_I_II, df_costa_rica_colegios, df_costa_rica_cee, df_costa_rica_noturna, df_costa_rica_caipad, df_costa_rica_ipec, df_costa_rica_cindea, df_costa_rica_coned)

# Number of Schools
cri_schools <- nrow(df_costa_rica_schools)
# Students of Costa Rica
cri_students <- df_unesco_cleaned %>% filter(country=="Costa Rica" & year==2022 & data %in% c("enrolment_preprimary_education", "enrolment_primary_education", "enrolment_secondary_education"))
# Teachers of Costa Rica
cri_teachers <- df_unesco_cleaned %>% filter(country=="Costa Rica" & year==2022 & data %in% c("teacher_preprimary_education", "teacher_primary_education", "teacher_secondary_education"))

# Data of Costa Rica
df_costa_rica <- data.frame(
  country = "Costa Rica",
  country_acronym = as.character(df_country_groups %>% filter(country_name=="Costa Rica") %>% select(country_acronym)),
  
  number_of_schools = cri_schools,
  source_number_of_schools = "MEP.CR - 2024",
  outdated_number_of_schools = 0,
  
  number_of_students = as.integer(sum(cri_students$value, na.rm=T)),
  source_number_of_students = "DATA UIS - 2022",
  outdated_number_of_students = 0,
  
  number_of_students_preprimary = as.integer(cri_students %>% filter(data=="enrolment_preprimary_education") %>% select(value)),
  source_number_of_students_preprimary = "DATA UIS - 2022",
  outdated_number_of_students_preprimary = 0,
  
  number_of_students_primary = as.integer(cri_students %>% filter(data=="enrolment_primary_education") %>% select(value)),
  source_number_of_students_primary = "DATA UIS - 2022",
  outdated_number_of_students_primary = 0,
  
  number_of_students_secondary = as.integer(cri_students %>% filter(data=="enrolment_secondary_education") %>% select(value)),
  source_number_of_students_secondary = "DATA UIS - 2022",
  outdated_number_of_students_secondary = 0,
  
  number_of_teachers = as.integer(sum(cri_teachers$value, na.rm=T)),
  source_number_of_teachers = "DATA UIS - 2022",
  outdated_number_of_teachers = 0,
  
  number_of_teachers_preprimary = as.integer(cri_teachers %>% filter(data=="teacher_preprimary_education") %>% select(value)),
  source_number_of_teachers_preprimary = "DATA UIS - 2022",
  outdated_number_of_teachers_preprimary = 0,
  
  number_of_teachers_primary = as.integer(cri_teachers %>% filter(data=="teacher_primary_education") %>% select(value)),
  source_number_of_teachers_primary = "DATA UIS - 2022",
  outdated_number_of_teachers_primary = 0,
  
  number_of_teachers_secondary = as.integer(cri_teachers %>% filter(data=="teacher_secondary_education") %>% select(value)),
  source_number_of_teachers_secondary = "DATA UIS - 2022",
  outdated_number_of_teachers_secondary = 0,
  
  number_of_urban_schools = nrow(df_costa_rica_schools %>% filter(ZONA=="URB")),
  source_number_of_urban_schools = "MEP.CR - 2024",
  outdated_number_of_urban_schools = 0,
  
  number_of_rural_schools = nrow(df_costa_rica_schools %>% filter(ZONA=="RUR")),
  source_number_of_rural_schools = "MEP.CR - 2024",
  outdated_number_of_rural_schools = 0
  
)

# Removing unnecessary data
rm(cri_schools, cri_students, cri_teachers, df_costa_rica_schools)

#### DOMINICAN REPUBLIC - OK (WITH SOURCE AND OUTDATED DUMMY VARIABLE) ####

# Number of Schools
dom_schools <- nrow(dt_dom_total)
# Students of Dominican Republic
dom_students <- df_unesco_cleaned %>% filter(country=="Dominican Republic" & year==2023 & data %in% c("enrolment_preprimary_education", "enrolment_primary_education", "enrolment_secondary_education"))
# Teachers of Dominican Republic
dom_teachers <- df_unesco_cleaned %>% filter(country=="Dominican Republic" & year==2023 & data %in% c("teacher_preprimary_education", "teacher_primary_education", "teacher_secondary_education"))

# Data of Dominican Republic
df_dominican_republic <- data.frame(
  country = "Dominican Republic",
  country_acronym = as.character(df_country_groups %>% filter(country_name=="Dominican Republic") %>% select(country_acronym)),
  
  number_of_schools = dom_schools,
  source_number_of_schools = "GEOSPATIAL DATA REPOSITORY - IDB",
  outdated_number_of_schools = 0,
  
  number_of_students = as.integer(sum(dom_students$value, na.rm=T)),
  source_number_of_students = "DATA UIS - 2023",
  outdated_number_of_students = 0,
  
  number_of_students_preprimary = as.integer(dom_students %>% filter(data=="enrolment_preprimary_education") %>% select(value)),
  source_number_of_students_preprimary = "DATA UIS - 2023",
  outdated_number_of_students_preprimary = 0,
  
  number_of_students_primary = as.integer(dom_students %>% filter(data=="enrolment_primary_education") %>% select(value)),
  source_number_of_students_primary = "DATA UIS - 2023",
  outdated_number_of_students_primary = 0,
  
  number_of_students_secondary = as.integer(dom_students %>% filter(data=="enrolment_secondary_education") %>% select(value)),
  source_number_of_students_secondary = "DATA UIS - 2023",
  outdated_number_of_students_secondary = 0,
  
  number_of_teachers = as.integer(sum(dom_teachers$value, na.rm=T)),
  source_number_of_teachers = "DATA UIS - 2023",
  outdated_number_of_teachers = 0,
  
  number_of_teachers_preprimary = as.integer(dom_teachers %>% filter(data=="teacher_preprimary_education") %>% select(value)),
  source_number_of_teachers_preprimary = "DATA UIS - 2023",
  outdated_number_of_teachers_preprimary = 0,
  
  number_of_teachers_primary = as.integer(dom_teachers %>% filter(data=="teacher_primary_education") %>% select(value)),
  source_number_of_teachers_primary = "DATA UIS - 2023",
  outdated_number_of_teachers_primary = 0,
  
  number_of_teachers_secondary = as.integer(dom_teachers %>% filter(data=="teacher_secondary_education") %>% select(value)),
  source_number_of_teachers_secondary = "DATA UIS - 2023",
  outdated_number_of_teachers_secondary = 0,
  
  number_of_urban_schools = NA,
  source_number_of_urban_schools = "No data available",
  outdated_number_of_urban_schools = "No data available",
  
  number_of_rural_schools = NA,
  source_number_of_rural_schools = "No data available",
  outdated_number_of_rural_schools = "No data available"
  
)

# Removing unnecessary data
rm(dom_schools, dom_students, dom_teachers, dt_dom_total)

#### ECUADOR - OK (WITH SOURCE AND OUTDATED DUMMY VARIABLE) ####

# Number of Schools
ecu_schools <- nrow(dt_ecu_total)
# Students of Ecuador
ecu_students <- df_unesco_cleaned %>% filter(country=="Ecuador" & year==2023 & data %in% c("enrolment_preprimary_education", "enrolment_primary_education", "enrolment_secondary_education"))
# Teachers of Ecuador
ecu_teachers <- df_unesco_cleaned %>% filter(country=="Ecuador" & year==2023 & data %in% c("teacher_preprimary_education", "teacher_primary_education", "teacher_secondary_education"))

# Data of Ecuador
df_ecuador <- data.frame(
  country = "Ecuador",
  country_acronym = as.character(df_country_groups %>% filter(country_name=="Ecuador") %>% select(country_acronym)),
  
  number_of_schools = ecu_schools,
  source_number_of_schools = "GEOSPATIAL DATA REPOSITORY - IDB",
  outdated_number_of_schools = 0,
  
  number_of_students = as.integer(sum(ecu_students$value, na.rm=T)),
  source_number_of_students = "DATA UIS - 2023",
  outdated_number_of_students = 0,
  
  number_of_students_preprimary = as.integer(ecu_students %>% filter(data=="enrolment_preprimary_education") %>% select(value)),
  source_number_of_students_preprimary = "DATA UIS - 2023",
  outdated_number_of_students_preprimary = 0,
  
  number_of_students_primary = as.integer(ecu_students %>% filter(data=="enrolment_primary_education") %>% select(value)),
  source_number_of_students_primary = "DATA UIS - 2023",
  outdated_number_of_students_primary = 0,
  
  number_of_students_secondary = as.integer(ecu_students %>% filter(data=="enrolment_secondary_education") %>% select(value)),
  source_number_of_students_secondary = "DATA UIS - 2023",
  outdated_number_of_students_secondary = 0,
  
  number_of_teachers = as.integer(sum(ecu_teachers$value, na.rm=T)),
  source_number_of_teachers = "DATA UIS - 2023",
  outdated_number_of_teachers = 0,
  
  number_of_teachers_preprimary = as.integer(ecu_teachers %>% filter(data=="teacher_preprimary_education") %>% select(value)),
  source_number_of_teachers_preprimary = "DATA UIS - 2023",
  outdated_number_of_teachers_preprimary = 0,
  
  number_of_teachers_primary = as.integer(ecu_teachers %>% filter(data=="teacher_primary_education") %>% select(value)),
  source_number_of_teachers_primary = "DATA UIS - 2023",
  outdated_number_of_teachers_primary = 0,
  
  number_of_teachers_secondary = as.integer(ecu_teachers %>% filter(data=="teacher_secondary_education") %>% select(value)),
  source_number_of_teachers_secondary = "DATA UIS - 2023",
  outdated_number_of_teachers_secondary = 0,
  
  number_of_urban_schools = nrow(dt_ecu_total %>% filter(area==1)),
  source_number_of_urban_schools = "GEOSPATIAL DATA REPOSITORY - IDB",
  outdated_number_of_urban_schools = 0,
  
  number_of_rural_schools = nrow(dt_ecu_total %>% filter(area==0)),
  source_number_of_rural_schools = "GEOSPATIAL DATA REPOSITORY - IDB",
  outdated_number_of_rural_schools = 0
  
)

# Removing unnecessary data
rm(ecu_schools, ecu_students, ecu_teachers, dt_ecu_total)

#### EL SALVADOR - OK (WITH SOURCE AND OUTDATED DUMMY VARIABLE) ####

# Number of Schools
slv_schools <- nrow(dt_slv_total)
# Students of Ecuador
slv_students <- df_unesco_cleaned %>% filter(country=="El Salvador" & year==2023 & data %in% c("enrolment_preprimary_education", "enrolment_primary_education", "enrolment_secondary_education"))
# Teachers of Ecuador
slv_teachers <- df_unesco_cleaned %>% filter(country=="El Salvador" & year==2023 & data %in% c("teacher_preprimary_education", "teacher_primary_education", "teacher_secondary_education"))

# Data of El Salvador
df_el_salvador <- data.frame(
  country = "El Salvador",
  country_acronym = as.character(df_country_groups %>% filter(country_name=="El Salvador") %>% select(country_acronym)),
  
  number_of_schools = slv_schools,
  source_number_of_schools = "GEOSPATIAL DATA REPOSITORY - IDB",
  outdated_number_of_schools = 0,
  
  number_of_students = as.integer(sum(slv_students$value, na.rm=T)),
  source_number_of_students = "DATA UIS - 2023",
  outdated_number_of_students = 0,
  
  number_of_students_preprimary = as.integer(slv_students %>% filter(data=="enrolment_preprimary_education") %>% select(value)),
  source_number_of_students_preprimary = "DATA UIS - 2023",
  outdated_number_of_students_preprimary = 0,
  
  number_of_students_primary = as.integer(slv_students %>% filter(data=="enrolment_primary_education") %>% select(value)),
  source_number_of_students_primary = "DATA UIS - 2023",
  outdated_number_of_students_primary = 0,
  
  number_of_students_secondary = as.integer(slv_students %>% filter(data=="enrolment_secondary_education") %>% select(value)),
  source_number_of_students_secondary = "DATA UIS - 2023",
  outdated_number_of_students_secondary = 0,
  
  number_of_teachers = as.integer(sum(slv_teachers$value, na.rm=T)),
  source_number_of_teachers = "DATA UIS - 2023",
  outdated_number_of_teachers = 0,
  
  number_of_teachers_preprimary = as.integer(slv_teachers %>% filter(data=="teacher_preprimary_education") %>% select(value)),
  source_number_of_teachers_preprimary = "DATA UIS - 2023",
  outdated_number_of_teachers_preprimary = 0,
  
  number_of_teachers_primary = as.integer(slv_teachers %>% filter(data=="teacher_primary_education") %>% select(value)),
  source_number_of_teachers_primary = "DATA UIS - 2023",
  outdated_number_of_teachers_primary = 0,
  
  number_of_teachers_secondary = as.integer(slv_teachers %>% filter(data=="teacher_secondary_education") %>% select(value)),
  source_number_of_teachers_secondary = "DATA UIS - 2023",
  outdated_number_of_teachers_secondary = 0,
  
  number_of_urban_schools = NA,
  source_number_of_urban_schools = "No data available",
  outdated_number_of_urban_schools = "No data available",
  
  number_of_rural_schools = NA,
  source_number_of_rural_schools = "No data available",
  outdated_number_of_rural_schools = "No data available"
  
)

# Removing unnecessary data
rm(slv_schools, slv_students, slv_teachers, dt_slv_total)

#### GUATEMALA - OK (WITH SOURCE AND OUTDATED DUMMY VARIABLE) ####

# Number of Schools
gtm_schools <- nrow(dt_gtm_total)
# Students of Guatemala
gtm_students <- df_unesco_cleaned %>% filter(country=="Guatemala" & year==2023 & data %in% c("enrolment_preprimary_education", "enrolment_primary_education", "enrolment_secondary_education"))
# Teachers of Guatemala
gtm_teachers <- df_unesco_cleaned %>% filter(country=="Guatemala" & year==2023 & data %in% c("teacher_preprimary_education", "teacher_primary_education", "teacher_secondary_education"))

# Data of Guatemala
df_guatemala <- data.frame(
  country = "Guatemala",
  country_acronym = as.character(df_country_groups %>% filter(country_name=="Guatemala") %>% select(country_acronym)),
  
  number_of_schools = gtm_schools,
  source_number_of_schools = "GEOSPATIAL DATA REPOSITORY - IDB",
  outdated_number_of_schools = 0,
  
  number_of_students = as.integer(sum(gtm_students$value, na.rm=T)),
  source_number_of_students = "DATA UIS - 2023",
  outdated_number_of_students = 0,
  
  number_of_students_preprimary = as.integer(gtm_students %>% filter(data=="enrolment_preprimary_education") %>% select(value)),
  source_number_of_students_preprimary = "DATA UIS - 2023",
  outdated_number_of_students_preprimary = 0,
  
  number_of_students_primary = as.integer(gtm_students %>% filter(data=="enrolment_primary_education") %>% select(value)),
  source_number_of_students_primary = "DATA UIS - 2023",
  outdated_number_of_students_primary = 0,
  
  number_of_students_secondary = as.integer(gtm_students %>% filter(data=="enrolment_secondary_education") %>% select(value)),
  source_number_of_students_secondary = "DATA UIS - 2023",
  outdated_number_of_students_secondary = 0,
  
  number_of_teachers = as.integer(sum(gtm_teachers$value, na.rm=T)),
  source_number_of_teachers = "DATA UIS - 2023",
  outdated_number_of_teachers = 0,
  
  number_of_teachers_preprimary = as.integer(gtm_teachers %>% filter(data=="teacher_preprimary_education") %>% select(value)),
  source_number_of_teachers_preprimary = "DATA UIS - 2023",
  outdated_number_of_teachers_preprimary = 0,
  
  number_of_teachers_primary = as.integer(gtm_teachers %>% filter(data=="teacher_primary_education") %>% select(value)),
  source_number_of_teachers_primary = "DATA UIS - 2023",
  outdated_number_of_teachers_primary = 0,
  
  number_of_teachers_secondary = as.integer(gtm_teachers %>% filter(data=="teacher_secondary_education") %>% select(value)),
  source_number_of_teachers_secondary = "DATA UIS - 2023",
  outdated_number_of_teachers_secondary = 0,
  
  number_of_urban_schools = NA,
  source_number_of_urban_schools = "No data available",
  outdated_number_of_urban_schools = "No data available",
  
  number_of_rural_schools = NA,
  source_number_of_rural_schools = "No data available",
  outdated_number_of_rural_schools = "No data available"
  
)

# Removing unnecessary data
rm(gtm_schools, gtm_students, gtm_teachers, dt_gtm_total)

#### GUYANA - OK (WITH SOURCE AND OUTDATED DUMMY VARIABLE) ####

# Number of Schools
guy_schools <- nrow(dt_guy_total)
# Students of Guyana
guy_students <- df_unesco_cleaned %>% filter(country=="Guyana" & year==2023 & data %in% c("enrolment_preprimary_education", "enrolment_primary_education", "enrolment_secondary_education"))
# Teachers of Guyana
guy_teachers <- df_unesco_cleaned %>% filter(country=="Guyana" & year==2023 & data %in% c("teacher_preprimary_education", "teacher_primary_education", "teacher_secondary_education"))

# Data of Guyana
df_guyana <- data.frame(
  country = "Guyana",
  country_acronym = as.character(df_country_groups %>% filter(country_name=="Guyana") %>% select(country_acronym)),
  
  number_of_schools = guy_schools,
  source_number_of_schools = "GEOSPATIAL DATA REPOSITORY - IDB",
  outdated_number_of_schools = 0,
  
  number_of_students = as.integer(sum(guy_students$value, na.rm=T)),
  source_number_of_students = "DATA UIS - 2023",
  outdated_number_of_students = 0,
  
  number_of_students_preprimary = as.integer(guy_students %>% filter(data=="enrolment_preprimary_education") %>% select(value)),
  source_number_of_students_preprimary = "DATA UIS - 2023",
  outdated_number_of_students_preprimary = 0,
  
  number_of_students_primary = as.integer(guy_students %>% filter(data=="enrolment_primary_education") %>% select(value)),
  source_number_of_students_primary = "DATA UIS - 2023",
  outdated_number_of_students_primary = 0,
  
  number_of_students_secondary = as.integer(guy_students %>% filter(data=="enrolment_secondary_education") %>% select(value)),
  source_number_of_students_secondary = "DATA UIS - 2023",
  outdated_number_of_students_secondary = 0,
  
  number_of_teachers = as.integer(sum(guy_teachers$value, na.rm=T)),
  source_number_of_teachers = "DATA UIS - 2023",
  outdated_number_of_teachers = 0,
  
  number_of_teachers_preprimary = as.integer(guy_teachers %>% filter(data=="teacher_preprimary_education") %>% select(value)),
  source_number_of_teachers_preprimary = "DATA UIS - 2023",
  outdated_number_of_teachers_preprimary = 0,
  
  number_of_teachers_primary = as.integer(guy_teachers %>% filter(data=="teacher_primary_education") %>% select(value)),
  source_number_of_teachers_primary = "DATA UIS - 2023",
  outdated_number_of_teachers_primary = 0,
  
  number_of_teachers_secondary = as.integer(guy_teachers %>% filter(data=="teacher_secondary_education") %>% select(value)),
  source_number_of_teachers_secondary = "DATA UIS - 2023",
  outdated_number_of_teachers_secondary = 0,
  
  number_of_urban_schools = NA,
  source_number_of_urban_schools = "No data available",
  outdated_number_of_urban_schools = "No data available",
  
  number_of_rural_schools = NA,
  source_number_of_rural_schools = "No data available",
  outdated_number_of_rural_schools = "No data available"
  
)

# Removing unnecessary data
rm(guy_schools, guy_students, guy_teachers, dt_guy_total)

#### HAITI - OK (WITH SOURCE AND OUTDATED DUMMY VARIABLE) ####

# SOURCE - https://sigeee.menfp.gouv.ht/census/dashboard/ecole
# Accessed in 11 may 2025
# Number of Schools
hti_schools <- 269+201+288+289+331+502+532+218+283+511

# Data of Haiti
df_haiti <- data.frame(
  country = "Haiti",
  country_acronym = as.character(df_country_groups %>% filter(country_name=="Haiti") %>% select(country_acronym)),
  
  number_of_schools = as.integer(hti_schools),
  source_number_of_schools = "MENFP - 2023",
  outdated_number_of_schools = 0,
  
  number_of_students = as.integer(2841641),
  source_number_of_students = "MENFP - 2023",
  outdated_number_of_students = 0,
  
  number_of_students_preprimary = NA,
  source_number_of_students_preprimary = "No data available",
  outdated_number_of_students_preprimary = "No data available",
  
  number_of_students_primary = as.integer(df_cima_latam %>% filter(no_pais=="Haiti") %>% select(number_of_students_primary)),
  source_number_of_students_primary = "CIMA - 2013",
  outdated_number_of_students_primary = 1,
  
  number_of_students_secondary = as.integer(df_cima_latam %>% filter(no_pais=="Haiti") %>% select(number_of_students_secondary)),
  source_number_of_students_secondary = "CIMA - 2013",
  outdated_number_of_students_secondary = 1,
  
  number_of_teachers = as.integer(101070),
  source_number_of_teachers = "MENFP - 2023",
  outdated_number_of_teachers = 0,
  
  number_of_teachers_preprimary = NA,
  source_number_of_teachers_preprimary = "No data available",
  outdated_number_of_teachers_preprimary = "No data available",
  
  number_of_teachers_primary = as.integer(df_cima_latam %>% filter(no_pais=="Haiti") %>% select(number_of_teachers_primary)),
  source_number_of_teachers_primary = "CIMA - 2013",
  outdated_number_of_teachers_primary = 1,
  
  number_of_teachers_secondary = as.integer(df_cima_latam %>% filter(no_pais=="Haiti") %>% select(number_of_teachers_secondary)),
  source_number_of_teachers_secondary = "CIMA - 2013",
  outdated_number_of_teachers_secondary = 1,
  
  number_of_urban_schools = NA,
  source_number_of_urban_schools = "No data available",
  outdated_number_of_urban_schools = "No data available",
  
  number_of_rural_schools = NA,
  source_number_of_rural_schools = "No data available",
  outdated_number_of_rural_schools = "No data available"
  
)

# Removing unnecessary data
rm(hti_schools)

#### HONDURAS - OK (WITH SOURCE AND OUTDATED DUMMY VARIABLE) ####

# Number of Schools
hnd_schools <- nrow(dt_hnd_total)
# Students of Honduras
hnd_students <- df_unesco_cleaned %>% filter(country=="Honduras" & year==2022 & data %in% c("enrolment_preprimary_education", "enrolment_primary_education", "enrolment_secondary_education"))
# Teachers of Honduras
hnd_teachers <- df_unesco_cleaned %>% filter(country=="Honduras" & year==2022 & data %in% c("teacher_preprimary_education", "teacher_primary_education", "teacher_secondary_education"))

# Data of Honduras
df_honduras <- data.frame(
  country = "Honduras",
  country_acronym = as.character(df_country_groups %>% filter(country_name=="Honduras") %>% select(country_acronym)),
  
  number_of_schools = hnd_schools,
  source_number_of_schools = "GEOSPATIAL DATA REPOSITORY - IDB",
  outdated_number_of_schools = 0,
  
  number_of_students = as.integer(sum(hnd_students$value, na.rm=T)),
  source_number_of_students = "DATA UIS - 2022",
  outdated_number_of_students = 0,
  
  number_of_students_preprimary = as.integer(hnd_students %>% filter(data=="enrolment_preprimary_education") %>% select(value)),
  source_number_of_students_preprimary = "DATA UIS - 2022",
  outdated_number_of_students_preprimary = 0,
  
  number_of_students_primary = as.integer(hnd_students %>% filter(data=="enrolment_primary_education") %>% select(value)),
  source_number_of_students_primary = "DATA UIS - 2022",
  outdated_number_of_students_primary = 0,
  
  number_of_students_secondary = as.integer(hnd_students %>% filter(data=="enrolment_secondary_education") %>% select(value)),
  source_number_of_students_secondary = "DATA UIS - 2022",
  outdated_number_of_students_secondary = 0,
  
  number_of_teachers = as.integer(sum(hnd_teachers$value, na.rm=T)),
  source_number_of_teachers = "DATA UIS - 2022",
  outdated_number_of_teachers = 0,
  
  number_of_teachers_preprimary = as.integer(hnd_teachers %>% filter(data=="teacher_preprimary_education") %>% select(value)),
  source_number_of_teachers_preprimary = "DATA UIS - 2022",
  outdated_number_of_teachers_preprimary = 0,
  
  number_of_teachers_primary = as.integer(hnd_teachers %>% filter(data=="teacher_primary_education") %>% select(value)),
  source_number_of_teachers_primary = "DATA UIS - 2022",
  outdated_number_of_teachers_primary = 0,
  
  number_of_teachers_secondary = as.integer(hnd_teachers %>% filter(data=="teacher_secondary_education") %>% select(value)),
  source_number_of_teachers_secondary = "DATA UIS - 2022",
  outdated_number_of_teachers_secondary = 0,
  
  number_of_urban_schools = nrow(dt_hnd_total %>% filter(area==1)),
  source_number_of_urban_schools = "GEOSPATIAL DATA REPOSITORY - IDB",
  outdated_number_of_urban_schools = 0,
  
  number_of_rural_schools = nrow(dt_hnd_total %>% filter(area==0)),
  source_number_of_rural_schools = "GEOSPATIAL DATA REPOSITORY - IDB",
  outdated_number_of_rural_schools = 0
  
)

# Removing unnecessary data
rm(hnd_schools, hnd_students, hnd_teachers, dt_hnd_total)

#### JAMAICA - OK (WITH SOURCE AND OUTDATED DUMMY VARIABLE) ####

# Number of Schools
# SOURCE: https://statinja.gov.jm/demo_socialstats/education.aspx
jam_schools <- 948+49+118+49+49+65+80+41+59+39+69+87+76+44+123
# Students of Jamaica
jam_students <- df_unesco_cleaned %>% filter(country=="Jamaica" & year==2023 & data %in% c("enrolment_preprimary_education", "enrolment_primary_education", "enrolment_secondary_education"))
# Teachers of Jamaica
jam_teachers <- df_unesco_cleaned %>% filter(country=="Jamaica" & year==2023 & data %in% c("teacher_preprimary_education", "teacher_primary_education", "teacher_secondary_education"))

# Data of Jamaica
df_jamaica <- data.frame(
  country = "Jamaica",
  country_acronym = as.character(df_country_groups %>% filter(country_name=="Jamaica") %>% select(country_acronym)),
  
  number_of_schools = as.integer(jam_schools),
  source_number_of_schools = "STATISTICAL INSTITUTE OF JAMAICA - 2019",
  outdated_number_of_schools = 1,
  
  number_of_students = as.integer(sum(jam_students$value, na.rm=T)),
  source_number_of_students = "DATA UIS - 2023",
  outdated_number_of_students = 0,
  
  number_of_students_preprimary = as.integer(jam_students %>% filter(data=="enrolment_preprimary_education") %>% select(value)),
  source_number_of_students_preprimary = "DATA UIS - 2023",
  outdated_number_of_students_preprimary = 0,
  
  number_of_students_primary = as.integer(jam_students %>% filter(data=="enrolment_primary_education") %>% select(value)),
  source_number_of_students_primary = "DATA UIS - 2023",
  outdated_number_of_students_primary = 0,
  
  number_of_students_secondary = as.integer(jam_students %>% filter(data=="enrolment_secondary_education") %>% select(value)),
  source_number_of_students_secondary = "DATA UIS - 2023",
  outdated_number_of_students_secondary = 0,
  
  number_of_teachers = as.integer(sum(jam_teachers$value, na.rm=T)),
  source_number_of_teachers = "DATA UIS - 2023",
  outdated_number_of_teachers = 0,
  
  number_of_teachers_preprimary = as.integer(jam_teachers %>% filter(data=="teacher_preprimary_education") %>% select(value)),
  source_number_of_teachers_preprimary = "DATA UIS - 2023",
  outdated_number_of_teachers_preprimary = 0,
  
  number_of_teachers_primary = as.integer(jam_teachers %>% filter(data=="teacher_primary_education") %>% select(value)),
  source_number_of_teachers_primary = "DATA UIS - 2023",
  outdated_number_of_teachers_primary = 0,
  
  number_of_teachers_secondary = as.integer(jam_teachers %>% filter(data=="teacher_secondary_education") %>% select(value)),
  source_number_of_teachers_secondary = "DATA UIS - 2023",
  outdated_number_of_teachers_secondary = 0,
  
  number_of_urban_schools = NA,
  source_number_of_urban_schools = "No data available",
  outdated_number_of_urban_schools = "No data available",
  
  number_of_rural_schools = NA,
  source_number_of_rural_schools = "No data available",
  outdated_number_of_rural_schools = "No data available"
  
)

# Removing unnecessary data
rm(jam_schools, jam_students, jam_teachers)

#### MEXICO - OK (WITH SOURCE AND OUTDATED DUMMY VARIABLE) ####

# Number of Schools
mex_schools <- nrow(dt_mex_total)
# Students of Mexico
mex_students <- df_unesco_cleaned %>% filter(country=="Mexico" & year==2022 & data %in% c("enrolment_preprimary_education", "enrolment_primary_education", "enrolment_secondary_education"))
# Teachers of Mexico
mex_teachers <- df_unesco_cleaned %>% filter(country=="Mexico" & year==2022 & data %in% c("teacher_preprimary_education", "teacher_primary_education", "teacher_secondary_education"))

# Data of Mexico
df_mexico <- data.frame(
  country = "Mexico",
  country_acronym = as.character(df_country_groups %>% filter(country_name=="Mexico") %>% select(country_acronym)),
  
  number_of_schools = mex_schools,
  source_number_of_schools = "GEOSPATIAL DATA REPOSITORY - IDB",
  outdated_number_of_schools = 0,
  
  number_of_students = as.integer(sum(mex_students$value, na.rm=T)),
  source_number_of_students = "DATA UIS - 2022",
  outdated_number_of_students = 0,
  
  number_of_students_preprimary = as.integer(mex_students %>% filter(data=="enrolment_preprimary_education") %>% select(value)),
  source_number_of_students_preprimary = "DATA UIS - 2022",
  outdated_number_of_students_preprimary = 0,
  
  number_of_students_primary = as.integer(mex_students %>% filter(data=="enrolment_primary_education") %>% select(value)),
  source_number_of_students_primary = "DATA UIS - 2022",
  outdated_number_of_students_primary = 0,
  
  number_of_students_secondary = as.integer(mex_students %>% filter(data=="enrolment_secondary_education") %>% select(value)),
  source_number_of_students_secondary = "DATA UIS - 2022",
  outdated_number_of_students_secondary = 0,
  
  number_of_teachers = as.integer(sum(mex_teachers$value, na.rm=T)),
  source_number_of_teachers = "DATA UIS - 2022",
  outdated_number_of_teachers = 0,
  
  number_of_teachers_preprimary = as.integer(mex_teachers %>% filter(data=="teacher_preprimary_education") %>% select(value)),
  source_number_of_teachers_preprimary = "DATA UIS - 2022",
  outdated_number_of_teachers_preprimary = 0,
  
  number_of_teachers_primary = as.integer(mex_teachers %>% filter(data=="teacher_primary_education") %>% select(value)),
  source_number_of_teachers_primary = "DATA UIS - 2022",
  outdated_number_of_teachers_primary = 0,
  
  number_of_teachers_secondary = as.integer(mex_teachers %>% filter(data=="teacher_secondary_education") %>% select(value)),
  source_number_of_teachers_secondary = "DATA UIS - 2022",
  outdated_number_of_teachers_secondary = 0,
  
  number_of_urban_schools = NA,
  source_number_of_urban_schools = "No data available",
  outdated_number_of_urban_schools = "No data available",
  
  number_of_rural_schools = NA,
  source_number_of_rural_schools = "No data available",
  outdated_number_of_rural_schools = "No data available"
  
)

# Removing unnecessary data
rm(mex_schools, mex_students, mex_teachers, dt_mex_total)

#### NICARAGUA - OK (WITH SOURCE AND OUTDATED DUMMY VARIABLE) ####

# [SOURCE]: https://education-profiles.org/es/america-latina-y-el-caribe/nicaragua/~actores-no-estatales-en-la-educacion#:~:text=En%20Nicaragua%20existen%209%2C105%20centros,es%20%20871%20obligatoria%20y 
# Accessed in may 7 2025
# NUMBER OF SCHOOLS - 9.105 

# Number of Schools
nic_schools <- 9105
# Students of Nicaragua
nic_students <- df_unesco_cleaned %>% filter(country=="Nicaragua" & year==2023 & data %in% c("enrolment_preprimary_education", "enrolment_primary_education", "enrolment_secondary_education"))
# Teachers of Nicaragua
nic_teachers <- df_unesco_cleaned %>% filter(country=="Nicaragua" & year==2023 & data %in% c("teacher_preprimary_education", "teacher_primary_education", "teacher_secondary_education"))

# Data of Nicaragua
df_nicaragua <- data.frame(
  country = "Nicaragua",
  country_acronym = as.character(df_country_groups %>% filter(country_name=="Nicaragua") %>% select(country_acronym)),
  
  number_of_schools = as.integer(nic_schools),
  source_number_of_schools = "UNESCO: 2021",
  outdated_number_of_schools = 1,
  
  number_of_students = as.integer(sum(nic_students$value, na.rm=T)),
  source_number_of_students = "DATA UIS - 2023",
  outdated_number_of_students = 0,
  
  number_of_students_preprimary = as.integer(nic_students %>% filter(data=="enrolment_preprimary_education") %>% select(value)),
  source_number_of_students_preprimary = "DATA UIS - 2023",
  outdated_number_of_students_preprimary = 0,
  
  number_of_students_primary = as.integer(nic_students %>% filter(data=="enrolment_primary_education") %>% select(value)),
  source_number_of_students_primary = "DATA UIS - 2023",
  outdated_number_of_students_primary = 0,
  
  number_of_students_secondary = as.integer(nic_students %>% filter(data=="enrolment_secondary_education") %>% select(value)),
  source_number_of_students_secondary = "DATA UIS - 2023",
  outdated_number_of_students_secondary = 0,
  
  number_of_teachers = as.integer(sum(nic_teachers$value, na.rm=T)),
  source_number_of_teachers = "DATA UIS - 2023",
  outdated_number_of_teachers = 0,
  
  number_of_teachers_preprimary = as.integer(nic_teachers %>% filter(data=="teacher_preprimary_education") %>% select(value)),
  source_number_of_teachers_preprimary = "DATA UIS - 2023",
  outdated_number_of_teachers_preprimary = 0,
  
  number_of_teachers_primary = as.integer(nic_teachers %>% filter(data=="teacher_primary_education") %>% select(value)),
  source_number_of_teachers_primary = "DATA UIS - 2023",
  outdated_number_of_teachers_primary = 0,
  
  number_of_teachers_secondary = as.integer(nic_teachers %>% filter(data=="teacher_secondary_education") %>% select(value)),
  source_number_of_teachers_secondary = "DATA UIS - 2023",
  outdated_number_of_teachers_secondary = 0,
  
  number_of_urban_schools = NA,
  source_number_of_urban_schools = "No data available",
  outdated_number_of_urban_schools = "No data available",
  
  number_of_rural_schools = NA,
  source_number_of_rural_schools = "No data available",
  outdated_number_of_rural_schools = "No data available"
  
)

# Removing unnecessary data
rm(nic_schools, nic_students, nic_teachers)

#### PANAMA - OK (WITH SOURCE AND OUTDATED DUMMY VARIABLE) ####

# Number of Schools
pan_schools <- nrow(dt_pan_total)
# Students of Panama
pan_students <- df_unesco_cleaned %>% filter(country=="Panama" & year==2017 & data %in% c("enrolment_preprimary_education", "enrolment_primary_education", "enrolment_secondary_education"))
# Teachers of Panama
pan_teachers <- df_unesco_cleaned %>% filter(country=="Panama" & year==2017 & data %in% c("teacher_preprimary_education", "teacher_primary_education", "teacher_secondary_education"))

# Data of Panama
df_panama <- data.frame(
  country = "Panama",
  country_acronym = as.character(df_country_groups %>% filter(country_name=="Panama") %>% select(country_acronym)),
  
  number_of_schools = pan_schools,
  source_number_of_schools = "GEOSPATIAL DATA REPOSITORY - IDB",
  outdated_number_of_schools = 0,
  
  number_of_students = as.integer(sum(pan_students$value, na.rm=T)),
  source_number_of_students = "DATA UIS - 2017",
  outdated_number_of_students = 1,
  
  number_of_students_preprimary = as.integer(pan_students %>% filter(data=="enrolment_preprimary_education") %>% select(value)),
  source_number_of_students_preprimary = "DATA UIS - 2017",
  outdated_number_of_students_preprimary = 1,
  
  number_of_students_primary = as.integer(pan_students %>% filter(data=="enrolment_primary_education") %>% select(value)),
  source_number_of_students_primary = "DATA UIS - 2017",
  outdated_number_of_students_primary = 1,
  
  number_of_students_secondary = as.integer(pan_students %>% filter(data=="enrolment_secondary_education") %>% select(value)),
  source_number_of_students_secondary = "DATA UIS - 2017",
  outdated_number_of_students_secondary = 1,
  
  number_of_teachers = as.integer(sum(pan_teachers$value, na.rm=T)),
  source_number_of_teachers = "DATA UIS - 2017",
  outdated_number_of_teachers = 1,
  
  number_of_teachers_preprimary = as.integer(pan_teachers %>% filter(data=="teacher_preprimary_education") %>% select(value)),
  source_number_of_teachers_preprimary = "DATA UIS - 2017",
  outdated_number_of_teachers_preprimary = 1,
  
  number_of_teachers_primary = as.integer(pan_teachers %>% filter(data=="teacher_primary_education") %>% select(value)),
  source_number_of_teachers_primary = "DATA UIS - 2017",
  outdated_number_of_teachers_primary = 1,
  
  number_of_teachers_secondary = as.integer(pan_teachers %>% filter(data=="teacher_secondary_education") %>% select(value)),
  source_number_of_teachers_secondary = "DATA UIS - 2017",
  outdated_number_of_teachers_secondary = 1,
  
  number_of_urban_schools = nrow(dt_pan_total %>% filter(area==1)),
  source_number_of_urban_schools = "GEOSPATIAL DATA REPOSITORY - IDB",
  outdated_number_of_urban_schools = 0,
  
  number_of_rural_schools = nrow(dt_pan_total %>% filter(area==0)),
  source_number_of_rural_schools = "GEOSPATIAL DATA REPOSITORY - IDB",
  outdated_number_of_rural_schools = 0
  
)

# Removing unnecessary data
rm(pan_schools, pan_students, pan_teachers, dt_pan_total)

#### PARAGUAY - (WITH SOURCE AND OUTDATED DUMMY VARIABLE) ####

# Number of Schools
pry_schools <- nrow(dt_pry_total)
# Students of Paraguay
pry_students <- df_unesco_cleaned %>% filter(country=="Paraguay" & year==2023 & data %in% c("enrolment_preprimary_education", "enrolment_primary_education", "enrolment_secondary_education"))
# Teachers of Paraguay

# [SOURCE]:  https://observatorio.org.py/indicadores/indicador/234
# Accessed in Jul 25 2025

# Data of Paraguay
df_paraguay <- data.frame(
  country = "Paraguay",
  country_acronym = as.character(df_country_groups %>% filter(country_name=="Paraguay") %>% select(country_acronym)),
  
  number_of_schools = pry_schools,
  source_number_of_schools = "GEOSPATIAL DATA REPOSITORY - IDB",
  outdated_number_of_schools = 0,
  
  number_of_students = as.integer(sum(pry_students$value, na.rm=T)),
  source_number_of_students = "DATA UIS - 2023",
  outdated_number_of_students = 0,
  
  number_of_students_preprimary = as.integer(pry_students %>% filter(data=="enrolment_preprimary_education") %>% select(value)),
  source_number_of_students_preprimary = "DATA UIS - 2023",
  outdated_number_of_students_preprimary = 0,
  
  number_of_students_primary = as.integer(pry_students %>% filter(data=="enrolment_primary_education") %>% select(value)),
  source_number_of_students_primary = "DATA UIS - 2023",
  outdated_number_of_students_primary = 0,
  
  number_of_students_secondary = as.integer(pry_students %>% filter(data=="enrolment_secondary_education") %>% select(value)),
  source_number_of_students_secondary = "DATA UIS - 2023",
  outdated_number_of_students_secondary = 0,
  
  number_of_teachers = as.integer(round(64435.39, 0)),
  source_number_of_teachers = "MEC-DGPE, SIGMEC 2018",
  outdated_number_of_teachers = 0,
  
  number_of_teachers_preprimary = 1551+385+774+398+344+846+324+579+219+394+938+2171+151+152+322+165+91+31,
  source_number_of_teachers_preprimary = "MEC-DGPE, SIGMEC 2018",
  outdated_number_of_teachers_preprimary = 1,
  
  number_of_teachers_primary = (1518+731+1707+989+784+1761+765+1374+417+856+1610+3641+319+348+710+361+259+73)+(1691+726+1756+1055+724+1799+787+1384+432+970+1606+3782+346+317+702+355+218+68),
  source_number_of_teachers_primary = "MEC-DGPE, SIGMEC 2018",
  outdated_number_of_teachers_primary = 1,
  
  number_of_teachers_secondary = (2962+1297+2889+1572+1242+3032+1283+2148+758+1790+2720+6551+419+445+1193+558+314+88) + (3468+1130+2351+1507+1160+2442+994+1849+788+1618+2472+6658+467+409+756+478+271+70),
  source_number_of_teachers_secondary = "MEC-DGPE, SIGMEC 2018",
  outdated_number_of_teachers_secondary = 1,
  
  number_of_urban_schools = nrow(dt_pry_total %>% filter(area==1)),
  source_number_of_urban_schools = "GEOSPATIAL DATA REPOSITORY - IDB",
  outdated_number_of_urban_schools = 0,
  
  number_of_rural_schools = nrow(dt_pry_total %>% filter(area==0)),
  source_number_of_rural_schools = "GEOSPATIAL DATA REPOSITORY - IDB",
  outdated_number_of_rural_schools = 0
  
)

# Removing unnecessary data
rm(pry_schools, pry_students, dt_pry_total)

#### PERU - OK (WITH SOURCE AND OUTDATED DUMMY VARIABLE) ####

# Number of Schools
per_schools <- nrow(dt_per_total)
# Students of Peru
per_students <- df_unesco_cleaned %>% filter(country=="Peru" & year==2023 & data %in% c("enrolment_preprimary_education", "enrolment_primary_education", "enrolment_secondary_education"))
# Teachers of Peru
per_teachers <- df_unesco_cleaned %>% filter(country=="Peru" & year==2023 & data %in% c("teacher_preprimary_education", "teacher_primary_education", "teacher_secondary_education"))

# Data of Peru
df_peru <- data.frame(
  country = "Peru",
  country_acronym = as.character(df_country_groups %>% filter(country_name=="Peru") %>% select(country_acronym)),
  
  number_of_schools = per_schools,
  source_number_of_schools = "GEOSPATIAL DATA REPOSITORY - IDB",
  outdated_number_of_schools = 0,
  
  number_of_students = as.integer(sum(per_students$value, na.rm=T)),
  source_number_of_students = "DATA UIS - 2023",
  outdated_number_of_students = 0,
  
  number_of_students_preprimary = as.integer(per_students %>% filter(data=="enrolment_preprimary_education") %>% select(value)),
  source_number_of_students_preprimary = "DATA UIS - 2023",
  outdated_number_of_students_preprimary = 0,
  
  number_of_students_primary = as.integer(per_students %>% filter(data=="enrolment_primary_education") %>% select(value)),
  source_number_of_students_primary = "DATA UIS - 2023",
  outdated_number_of_students_primary = 0,
  
  number_of_students_secondary = as.integer(per_students %>% filter(data=="enrolment_secondary_education") %>% select(value)),
  source_number_of_students_secondary = "DATA UIS - 2023",
  outdated_number_of_students_secondary = 0,
  
  number_of_teachers = as.integer(sum(per_teachers$value, na.rm=T)),
  source_number_of_teachers = "DATA UIS - 2023",
  outdated_number_of_teachers = 0,
  
  number_of_teachers_preprimary = as.integer(per_teachers %>% filter(data=="teacher_preprimary_education") %>% select(value)),
  source_number_of_teachers_preprimary = "DATA UIS - 2023",
  outdated_number_of_teachers_preprimary = 0,
  
  number_of_teachers_primary = as.integer(per_teachers %>% filter(data=="teacher_primary_education") %>% select(value)),
  source_number_of_teachers_primary = "DATA UIS - 2023",
  outdated_number_of_teachers_primary = 0,
  
  number_of_teachers_secondary = as.integer(per_teachers %>% filter(data=="teacher_secondary_education") %>% select(value)),
  source_number_of_teachers_secondary = "DATA UIS - 2023",
  outdated_number_of_teachers_secondary = 0,
  
  number_of_urban_schools = nrow(dt_per_total %>% filter(area==1)),
  source_number_of_urban_schools = "GEOSPATIAL DATA REPOSITORY - IDB",
  outdated_number_of_urban_schools = 0,
  
  number_of_rural_schools = nrow(dt_per_total %>% filter(area==0)),
  source_number_of_rural_schools = "GEOSPATIAL DATA REPOSITORY - IDB",
  outdated_number_of_rural_schools = 0
  
)

# Removing unnecessary data
rm(per_schools, per_students, per_teachers, dt_per_total)

#### SURINAM - OK (WITH SOURCE AND OUTDATED DUMMY VARIABLE) ####

# Number of Schools
sur_schools <- nrow(dt_sur_total)
# Students of Suriname
sur_students <- df_unesco_cleaned %>% filter(country=="Suriname" & year==2023 & data %in% c("enrolment_preprimary_education", "enrolment_primary_education", "enrolment_secondary_education"))
# Teachers of Suriname
sur_teachers <- df_unesco_cleaned %>% filter(country=="Suriname" & year==2023 & data %in% c("teacher_preprimary_education", "teacher_primary_education", "teacher_secondary_education"))

# Data of Surinam
df_surinam <- data.frame(
  country = "Surinam",
  country_acronym = as.character(df_country_groups %>% filter(country_name=="Surinam") %>% select(country_acronym)),
  
  number_of_schools = sur_schools,
  source_number_of_schools = "GEOSPATIAL DATA REPOSITORY - IDB",
  outdated_number_of_schools = 0,
  
  number_of_students = as.integer(sum(sur_students$value, na.rm=T)),
  source_number_of_students = "DATA UIS - 2023",
  outdated_number_of_students = 0,
  
  number_of_students_preprimary = as.integer(sur_students %>% filter(data=="enrolment_preprimary_education") %>% select(value)),
  source_number_of_students_preprimary = "DATA UIS - 2023",
  outdated_number_of_students_preprimary = 0,
  
  number_of_students_primary = as.integer(sur_students %>% filter(data=="enrolment_primary_education") %>% select(value)),
  source_number_of_students_primary = "DATA UIS - 2023",
  outdated_number_of_students_primary = 0,
  
  number_of_students_secondary = as.integer(sur_students %>% filter(data=="enrolment_secondary_education") %>% select(value)),
  source_number_of_students_secondary = "DATA UIS - 2023",
  outdated_number_of_students_secondary = 0,
  
  number_of_teachers = as.integer(sum(sur_teachers$value, na.rm=T)),
  source_number_of_teachers = "DATA UIS - 2023",
  outdated_number_of_teachers = 0,
  
  number_of_teachers_preprimary = as.integer(sur_teachers %>% filter(data=="teacher_preprimary_education") %>% select(value)),
  source_number_of_teachers_preprimary = "DATA UIS - 2023",
  outdated_number_of_teachers_preprimary = 0,
  
  number_of_teachers_primary = as.integer(sur_teachers %>% filter(data=="teacher_primary_education") %>% select(value)),
  source_number_of_teachers_primary = "DATA UIS - 2023",
  outdated_number_of_teachers_primary = 0,
  
  number_of_teachers_secondary = as.integer(sur_teachers %>% filter(data=="teacher_secondary_education") %>% select(value)),
  source_number_of_teachers_secondary = "DATA UIS - 2023",
  outdated_number_of_teachers_secondary = 0,
  
  number_of_urban_schools = nrow(dt_sur_total %>% filter(area==1)),
  source_number_of_urban_schools = "GEOSPATIAL DATA REPOSITORY - IDB",
  outdated_number_of_urban_schools = 0,
  
  number_of_rural_schools = nrow(dt_sur_total %>% filter(area==0)),
  source_number_of_rural_schools = "GEOSPATIAL DATA REPOSITORY - IDB",
  outdated_number_of_rural_schools = 0
  
)

# Removing unnecessary data
rm(sur_schools, sur_students, sur_teachers, dt_sur_total)

#### TRINIDAD AND TOBAGO - OK (WITH SOURCE AND OUTDATED DUMMY VARIABLE) ####

# SOURCE: https://storage.moe.gov.tt/corporate/2022/12/2019-2020-EDUCATION-STATISTICS-DIGEST-ccd-1.pdf - Accessed in May 8 2025

# Early Childhood Care and Education (pre-primary): 151 [p. 4]
# Primary Education: 476 [p. 5]
# Secondary Education: 134 [p. 6]

# Number of Schools
tto_schools <- 151+476+134
# Students of Trinidad and Tobago
tto_students <- df_unesco_cleaned %>% filter(country=="Trinidad and Tobago" & year==2023 & data %in% c("enrolment_preprimary_education", "enrolment_primary_education", "enrolment_secondary_education"))
# Teachers of Trinidad and Tobago
tto_teachers <- df_unesco_cleaned %>% filter(country=="Trinidad and Tobago" & year==2023 & data %in% c("teacher_preprimary_education", "teacher_primary_education", "teacher_secondary_education"))

# Data of Trinidad and Tobago
df_trinidad_and_tobago <- data.frame(
  country = "Trinidad and Tobago",
  country_acronym = as.character(df_country_groups %>% filter(country_name=="Trinidad and Tobago") %>% select(country_acronym)),
  
  number_of_schools = as.integer(tto_schools),
  source_number_of_schools = "GOVERNMENT OF THE REPUBLIC OF TRINIDAD AND TOBAGO - 2019/2020",
  outdated_number_of_schools = 1,
  
  number_of_students = as.integer(sum(tto_students$value, na.rm=T)),
  source_number_of_students = "DATA UIS - 2023",
  outdated_number_of_students = 0,
  
  number_of_students_preprimary = as.integer(tto_students %>% filter(data=="enrolment_preprimary_education") %>% select(value)),
  source_number_of_students_preprimary = "DATA UIS - 2023",
  outdated_number_of_students_preprimary = 0,
  
  number_of_students_primary = as.integer(tto_students %>% filter(data=="enrolment_primary_education") %>% select(value)),
  source_number_of_students_primary = "DATA UIS - 2023",
  outdated_number_of_students_primary = 0,
  
  number_of_students_secondary = as.integer(tto_students %>% filter(data=="enrolment_secondary_education") %>% select(value)),
  source_number_of_students_secondary = "DATA UIS - 2023",
  outdated_number_of_students_secondary = 0,
  
  number_of_teachers = as.integer(sum(tto_teachers$value, na.rm=T)),
  source_number_of_teachers = "DATA UIS - 2023",
  outdated_number_of_teachers = 0,
  
  number_of_teachers_preprimary = as.integer(tto_teachers %>% filter(data=="teacher_preprimary_education") %>% select(value)),
  source_number_of_teachers_preprimary = "DATA UIS - 2023",
  outdated_number_of_teachers_preprimary = 0,
  
  number_of_teachers_primary = as.integer(tto_teachers %>% filter(data=="teacher_primary_education") %>% select(value)),
  source_number_of_teachers_primary = "DATA UIS - 2023",
  outdated_number_of_teachers_primary = 0,
  
  number_of_teachers_secondary = as.integer(tto_teachers %>% filter(data=="teacher_secondary_education") %>% select(value)),
  source_number_of_teachers_secondary = "DATA UIS - 2023",
  outdated_number_of_teachers_secondary = 0,
  
  number_of_urban_schools = NA,
  source_number_of_urban_schools = "No data available",
  outdated_number_of_urban_schools = "No data available",
  
  number_of_rural_schools = NA,
  source_number_of_rural_schools = "No data available",
  outdated_number_of_rural_schools = "No data available"
  
)

# Removing unnecessary data
rm(tto_schools, tto_students, tto_teachers)

#### URUGUAY - (WITH SOURCE AND OUTDATED DUMMY VARIABLE) ####

# Number of Schools
ury_schools <- nrow(dt_ury_total)
# Students of Uruguay
ury_students <- df_unesco_cleaned %>% filter(country=="Uruguay" & year==2022 & data %in% c("enrolment_preprimary_education", "enrolment_primary_education", "enrolment_secondary_education"))
# Teachers of Uruguay
ury_teachers <- df_unesco_cleaned %>% filter(country=="Uruguay" & year==2022 & data %in% c("teacher_preprimary_education", "teacher_primary_education", "teacher_secondary_education"))

# Data of Uruguay
df_uruguay <- data.frame(
  country = "Uruguay",
  country_acronym = as.character(df_country_groups %>% filter(country_name=="Uruguay") %>% select(country_acronym)),
  
  number_of_schools = ury_schools,
  source_number_of_schools = "GEOSPATIAL DATA REPOSITORY - IDB",
  outdated_number_of_schools = 0,
  
  number_of_students = as.integer(sum(ury_students$value, na.rm=T)),
  source_number_of_students = "DATA UIS - 2022",
  outdated_number_of_students = 0,
  
  number_of_students_preprimary = as.integer(ury_students %>% filter(data=="enrolment_preprimary_education") %>% select(value)),
  source_number_of_students_preprimary = "DATA UIS - 2022",
  outdated_number_of_students_preprimary = 0,
  
  number_of_students_primary = as.integer(ury_students %>% filter(data=="enrolment_primary_education") %>% select(value)),
  source_number_of_students_primary = "DATA UIS - 2022",
  outdated_number_of_students_primary = 0,
  
  number_of_students_secondary = as.integer(ury_students %>% filter(data=="enrolment_secondary_education") %>% select(value)),
  source_number_of_students_secondary = "DATA UIS - 2022",
  outdated_number_of_students_secondary = 0,
  
  number_of_teachers = as.integer(sum(ury_teachers$value, na.rm=T)),
  source_number_of_teachers = "DATA UIS - 2022",
  outdated_number_of_teachers = 0,
  
  number_of_teachers_preprimary = as.integer(ury_teachers %>% filter(data=="teacher_preprimary_education") %>% select(value)),
  source_number_of_teachers_preprimary = "DATA UIS - 2022",
  outdated_number_of_teachers_preprimary = 0,
  
  number_of_teachers_primary = as.integer(ury_teachers %>% filter(data=="teacher_primary_education") %>% select(value)),
  source_number_of_teachers_primary = "DATA UIS - 2022",
  outdated_number_of_teachers_primary = 0,
  
  number_of_teachers_secondary = as.integer(ury_teachers %>% filter(data=="teacher_secondary_education") %>% select(value)),
  source_number_of_teachers_secondary = "DATA UIS - 2022",
  outdated_number_of_teachers_secondary = 0,
  
  number_of_urban_schools = nrow(dt_ury_total %>% filter(area==1)),
  source_number_of_urban_schools = "GEOSPATIAL DATA REPOSITORY - IDB",
  outdated_number_of_urban_schools = 0,
  
  number_of_rural_schools = nrow(dt_ury_total %>% filter(area==0)),
  source_number_of_rural_schools = "GEOSPATIAL DATA REPOSITORY - IDB",
  outdated_number_of_rural_schools = 0
  
)

# Removing unnecessary data
rm(ury_schools, ury_students, ury_teachers, dt_ury_total)

#### VENEZUELA - OK (WITH SOURCE AND OUTDATED DUMMY VARIABLE) ####

# Students of Venezuela (Bolivarian Republic of)
ven_students <- df_unesco_cleaned %>% filter(country=="Venezuela (Bolivarian Republic of)" & year==2023 & data %in% c("enrolment_preprimary_education", "enrolment_primary_education", "enrolment_secondary_education"))
# Teachers of Venezuela (Bolivarian Republic of)
ven_teachers <- df_unesco_cleaned %>% filter(country=="Venezuela (Bolivarian Republic of)" & year==2023 & data %in% c("teacher_preprimary_education", "teacher_primary_education", "teacher_secondary_education"))

# Data of Venezuela (Bolivarian Republic of)
df_venezuela <- data.frame(
  country = "Venezuela (Bolivarian Republic of)",
  country_acronym = as.character(df_country_groups %>% filter(country_name=="Venezuela (Bolivarian Republic of)") %>% select(country_acronym)),
  
  number_of_schools = as.integer(df_cima_latam %>% filter(no_pais=="Venezuela, RB") %>% select(number_of_schools)),
  source_number_of_schools = "GEOSPATIAL DATA REPOSITORY - IDB",
  outdated_number_of_schools = 0,
  
  number_of_students = as.integer(sum(ven_students$value, na.rm=T)),
  source_number_of_students = "DATA UIS - 2023",
  outdated_number_of_students = 0,
  
  number_of_students_preprimary = as.integer(ven_students %>% filter(data=="enrolment_preprimary_education") %>% select(value)),
  source_number_of_students_preprimary = "DATA UIS - 2023",
  outdated_number_of_students_preprimary = 0,
  
  number_of_students_primary = as.integer(ven_students %>% filter(data=="enrolment_primary_education") %>% select(value)),
  source_number_of_students_primary = "DATA UIS - 2023",
  outdated_number_of_students_primary = 0,
  
  number_of_students_secondary = as.integer(ven_students %>% filter(data=="enrolment_secondary_education") %>% select(value)),
  source_number_of_students_secondary = "DATA UIS - 2023",
  outdated_number_of_students_secondary = 0,
  
  number_of_teachers = as.integer(sum(ven_teachers$value, na.rm=T)),
  source_number_of_teachers = "DATA UIS - 2023",
  outdated_number_of_teachers = 0,
  
  number_of_teachers_preprimary = as.integer(ven_teachers %>% filter(data=="teacher_preprimary_education") %>% select(value)),
  source_number_of_teachers_preprimary = "DATA UIS - 2023",
  outdated_number_of_teachers_preprimary = 0,
  
  number_of_teachers_primary = as.integer(ven_teachers %>% filter(data=="teacher_primary_education") %>% select(value)),
  source_number_of_teachers_primary = "DATA UIS - 2023",
  outdated_number_of_teachers_primary = 0,
  
  number_of_teachers_secondary = as.integer(ven_teachers %>% filter(data=="teacher_secondary_education") %>% select(value)),
  source_number_of_teachers_secondary = "DATA UIS - 2023",
  outdated_number_of_teachers_secondary = 0,
  
  number_of_urban_schools = NA,
  source_number_of_urban_schools = "No data available",
  outdated_number_of_urban_schools = "No data available",
  
  number_of_rural_schools = NA,
  source_number_of_rural_schools = "No data available",
  outdated_number_of_rural_schools = "No data available"
  
)

# Removing unnecessary data
rm(ven_students, ven_teachers)


#### BASE FINAL ####

# Binding dataframes
df_product1 <- rbind(
  df_argentina,
  df_bahamas,
  df_barbados,
  df_belize,
  df_bolivia,
  df_brazil,
  df_chile,
  df_colombia,
  df_costa_rica,
  df_dominican_republic,
  df_ecuador,
  df_el_salvador,
  df_guatemala,
  df_guyana,
  df_haiti,
  df_honduras,
  df_jamaica,
  df_mexico,
  df_nicaragua,
  df_panama,
  df_paraguay,
  df_peru,
  df_surinam,
  df_trinidad_and_tobago,
  df_uruguay,
  df_venezuela
)

# Remove dados desnecessários
rm(
  df_argentina,
  df_bahamas,
  df_barbados,
  df_belize,
  df_bolivia,
  df_brazil,
  df_chile,
  df_colombia,
  df_costa_rica,
  df_dominican_republic,
  df_ecuador,
  df_el_salvador,
  df_guatemala,
  df_guyana,
  df_haiti,
  df_honduras,
  df_jamaica,
  df_mexico,
  df_nicaragua,
  df_panama,
  df_paraguay,
  df_peru,
  df_surinam,
  df_trinidad_and_tobago,
  df_uruguay,
  df_venezuela
)

#data.table::fwrite(x = df_product1, file = here("data", "silver", "df_produto_1.csv"))

#----------------
#- Dados do BID -
#----------------

# 1. Get the names of subdirectories in "raw_data"
raw_dirs <- list.dirs(here::here("data", "raw_data", "BID"), recursive = FALSE, full.names = FALSE)
# 2. Function to import CSV files from each subdirectory
import_raw_data <- function() {
  # Create empty list to store imported data frames
  result <- list()
  
  # Define base directory path
  base_dir <- here::here("data", "raw_data", "BID")
  
  # Loop through each folder name in raw_dirs
  for (folder in raw_dirs) {
    
    # Skip "LAC" folder (it has no CSV)
    if (!(folder %in% c("LAC", "BRB", "TTO"))) {
      # Construct path to CSV file
      csv_path <- file.path(base_dir, folder, paste0(folder, ".csv"))
      
      # Define name for the data frame (e.g., df_ABC)
      df_name <- paste0("df_", folder)
      
      # Check if file exists before reading (optional but safer)
      if (file.exists(csv_path)) {
        # Read CSV using fread (handles large files efficiently)
        df <- data.table::fread(csv_path, fill = TRUE, encoding="Latin-1")
        
        # Store in the result list
        result[[df_name]] <- df
      } else {
        warning(glue::glue("CSV file not found: {csv_path}"))
      }
    }
  }
  
  # Return the list of data frames
  return(result)
}
country_dfs <- import_raw_data()
# 1. Function to clean dataframes
clean_raw_data <- function(df_list) {
  
  # Initialize an empty data frame to accumulate results
  df_clean <- data.frame()
  
  # Loop over each data frame in the input list
  for(df in df_list){
    
    # Extract country-level identifiers from the first row
    # Then group by these identifiers and summarise all numeric columns
    df <- df %>% 
      mutate(
        ADM0_EN = as.character(df[1, 1]),   # Country name from first row, first column
        ADM0_PCODE = as.character(df[1, 2]) # Country code from first row, second column
      ) %>% 
      group_by(ADM0_EN, ADM0_PCODE) %>%
      summarise(
        across(where(is.numeric), \(x) sum(x, na.rm = TRUE)),  # Sum all numeric columns
        .groups = "drop"  # Drop grouping after summarise
      )
    
    # Append the cleaned and summarised df to the master data frame
    df_clean <- bind_rows(df_clean, df)
  }
  
  # Drop unused location code columns, if present
  df_clean <- df_clean %>% 
    select(-c(cod_canton, codigo_distrito, codigo_departamento))
  
  # Return the cleaned and aggregated data
  return(df_clean)
}
clean_data <- clean_raw_data(country_dfs)

rm(country_dfs, df_cima_latam, df_country_groups, df_unesco, df_unesco_cleaned)
#data.table::fwrite(x = clean_data, file = here("data", "silver", "df_bid.csv"))


df_bid_latam <- clean_data %>% 
  mutate(
    primary_schools_cima = escuelas_primaria_total,
    secondary_schools_cima = ifelse(ADM0_EN %in% c("Chile", "Ecuador", "Honduras"), escuelas_media_total, escuelas_secundaria_total),
    source_cima_schools = "BID"
  ) %>% 
  rename(
    "country" = "ADM0_EN"
  ) %>% 
  select(
    country,
    primary_schools_cima,
    secondary_schools_cima, 
    source_cima_schools
  ) 

#googledrive::drive_download(file = "Escenarios costeo todos los países - marzo 24.xlsx", path = here("data", "raw_data", "cima_latam.xlsx"), overwrite = T)
df_cima_latam <- readxl::read_xlsx(here("data", "raw_data", "cima_latam.xlsx"), sheet = "Todo Latam", skip = 3)
df_cima_latam <- df_cima_latam %>% filter(!(is.na(País)) & País != "TOTALES") %>% select(País, `Total de escuelas por etapa (primaria)`, `Total de escuelas por etapa (sec)`)
colnames(df_cima_latam) <- c("country", "primary_schools_cima", "secondary_schools_cima")

df_cima_latam <- df_cima_latam %>% 
  mutate(
    country = case_when(
      country=="Argentina" ~ "Argentina",
      country=="Bahamas" ~ "Bahamas",
      country=="Barbados" ~ "Barbados",
      country=="Belize" ~ "Belize",
      country=="Bolivia" ~ "Bolivia (Plurinational State of)",
      country=="Brazil"	~ "Brazil",
      country=="Chile" ~ "Chile",
      country=="Colombia" ~	"Colombia",
      country=="Costa Rica" ~	"Costa Rica",
      country=="Dominican Republic" ~	"Dominican Republic",
      country=="Ecuador" ~ "Ecuador",
      country=="El Salvador" ~ "El Salvador",
      country=="Guatemala" ~ "Guatemala",
      country=="Guyana" ~	"Guyana",
      country=="Haiti" ~	"Haiti",
      country=="Honduras" ~	"Honduras",
      country=="Jamaica" ~	"Jamaica",
      country=="Mexico" ~	"Mexico",
      country=="Nicaragua" ~	"Nicaragua",
      country=="Panama" ~	"Panama",
      country=="Paraguay" ~	"Paraguay",
      country=="Peru" ~	 "Peru",
      country=="Suriname" ~ 	"Surinam",
      country=="Trinidad and Tobago" ~	"Trinidad and Tobago",
      country=="Uruguay" ~ "Uruguay",
      country=="Venezuela, RB" ~	"Venezuela (Bolivarian Republic of)"
    ),
    source_cima_schools = case_when(
      country=="Argentina" ~ "CIMA - 2015",
      country=="Bahamas" ~ "CIMA - 2008",
      country=="Barbados" ~ "CIMA - 2013",
      country=="Belize" ~ "CIMA",
      country=="Bolivia (Plurinational State of)" ~ "CIMA",
      country=="Brazil" ~ "CIMA - 2016",
      country=="Chile" ~ "CIMA - 2016",
      country=="Colombia" ~ "CIMA - 2015",
      country=="Costa Rica" ~ "CIMA - 2015",
      country=="Dominican Republic" ~ "CIMA - 2014",
      country=="Ecuador" ~ "CIMA - 2014",
      country=="El Salvador" ~ "CIMA - 2015",
      country=="Guatemala" ~ "CIMA - 2015",
      country=="Guyana" ~ "CIMA - 2011",
      country=="Haiti" ~ "CIMA - 2013",
      country=="Honduras" ~ "CIMA - 2012",
      country=="Jamaica" ~ "CIMA - 2014",
      country=="Mexico" ~ "CIMA - 2015",
      country=="Nicaragua" ~ "CIMA - 2015",
      country=="Panama" ~ "CIMA - 2013",
      country=="Paraguay" ~ "CIMA - 2013",
      country=="Peru" ~ "CIMA - 2016",
      country=="Surinam" ~ "CIMA",
      country=="Trinidad and Tobago" ~ "CIMA",
      country=="Uruguay" ~ "CIMA - 2015",
      country=="Venezuela (Bolivarian Republic of)" ~ "CIMA - 2012"
    )
  ) %>% 
  select(
    country, primary_schools_cima, secondary_schools_cima, source_cima_schools
  )


clean_data_formatted <- clean_data %>% 
  filter(
    ADM0_EN %in% c("Belize", "Colombia", "Guyana", "Paraguay")
  ) %>% 
  select(
    ADM0_EN,
    ADM0_PCODE, 
    escuelas_inicial_total, escuelas_primaria_total, escuelas_secundaria_total, escuelas_media_total, 
    matricula_inicial_total, matricula_primaria_total, matricula_secundaria_total, matricula_media_total, 
    docentes_inicial_total, docentes_primaria_total, docentes_secundaria_total, docentes_media_total, 
    escuelas_inicial_urbana, escuelas_inicial_rural,
    escuelas_primaria_urbana, escuelas_primaria_rural,
    escuelas_secundaria_urbana, escuelas_secundaria_rural,
    escuelas_media_urbana, escuelas_media_rural
  ) %>% 
  mutate(
    country = ADM0_EN,
    country_acronym = ADM0_PCODE,
    number_of_schools = escuelas_inicial_total+escuelas_primaria_total+escuelas_secundaria_total,
    source_number_of_schools = ifelse(is.na(number_of_schools), NA, "BID"),
    outdated_number_of_schools = ifelse(is.na(source_number_of_schools), NA, 0),
    number_of_students = matricula_inicial_total+matricula_primaria_total+matricula_secundaria_total,
    source_number_of_students = ifelse(is.na(number_of_students), NA, "BID"),
    outdated_number_of_students = ifelse(is.na(source_number_of_students), NA, 0),
    number_of_students_primary = matricula_primaria_total,
    source_number_of_students_primary = ifelse(is.na(number_of_students_primary), NA, "BID"),
    outdated_number_of_students_primary = ifelse(is.na(source_number_of_students_primary), NA, 0),
    number_of_students_secondary = matricula_secundaria_total,
    source_number_of_students_secondary = ifelse(is.na(number_of_students_secondary), NA, "BID"),
    outdated_number_of_students_secondary = ifelse(is.na(source_number_of_students_secondary), NA, 0),
    number_of_teachers = docentes_inicial_total+docentes_primaria_total+docentes_secundaria_total,
    source_number_of_teachers = ifelse(is.na(number_of_teachers), NA, "BID"),
    outdated_number_of_teachers = ifelse(is.na(source_number_of_teachers), NA, 0),
    number_of_teachers_primary = docentes_primaria_total,
    source_number_of_teachers_primary = ifelse(is.na(number_of_teachers_primary), NA, "BID"),
    outdated_number_of_teachers_primary = ifelse(is.na(source_number_of_teachers_primary), NA, 0),
    number_of_teachers_secondary = docentes_secundaria_total,
    source_number_of_teachers_secondary = ifelse(is.na(number_of_teachers_secondary), NA, "BID"),
    outdated_number_of_teachers_secondary = ifelse(is.na(source_number_of_teachers_secondary), NA, 0),
    #primary_schools_cima = escuelas_primaria_total,
    #secondary_schools_cima = ifelse(
    #  ADM0_EN %in% c("Chile", "Ecuador", "Honduras"), escuelas_media_total, escuelas_secundaria_total
    #),
    #source_cima_schools = ifelse(is.na(primary_schools_cima) | is.na(secondary_schools_cima), NA, "BID"),
    number_of_urban_schools = escuelas_inicial_urbana+escuelas_primaria_urbana+escuelas_secundaria_urbana,
    source_number_of_urban_schools = ifelse(is.na(number_of_urban_schools), NA, "BID"),
    outdated_number_of_urban_schools = ifelse(is.na(source_number_of_urban_schools), NA, 0),
    number_of_rural_schools = escuelas_inicial_rural+escuelas_primaria_rural+escuelas_secundaria_rural,
    source_number_of_rural_schools = ifelse(is.na(number_of_rural_schools), NA, "BID"),
    outdated_number_of_rural_schools = ifelse(is.na(source_number_of_rural_schools), NA, 0)
  ) %>% 
  select(
    country,
    country_acronym,
    number_of_schools,
    source_number_of_schools,
    outdated_number_of_schools,
    #primary_schools_cima,
    #secondary_schools_cima,
    #source_cima_schools,
    number_of_students,
    source_number_of_students,
    outdated_number_of_students,
    number_of_students_primary,
    source_number_of_students_primary,
    outdated_number_of_students_primary,
    number_of_students_secondary,
    source_number_of_students_secondary,
    outdated_number_of_students_secondary,
    number_of_teachers,
    source_number_of_teachers,
    outdated_number_of_teachers,
    number_of_teachers_primary,
    source_number_of_teachers_primary,
    outdated_number_of_teachers_primary,
    number_of_teachers_secondary,
    source_number_of_teachers_secondary,
    outdated_number_of_teachers_secondary,
    number_of_urban_schools,
    source_number_of_urban_schools,
    outdated_number_of_urban_schools,
    number_of_rural_schools,
    source_number_of_rural_schools,
    outdated_number_of_rural_schools
  )
# Preparing Paraguay data
clean_data_formatted <- clean_data_formatted %>% 
  mutate(
    number_of_teachers = ifelse(!(is.na(number_of_teachers)), number_of_teachers, 34694+34286),
    source_number_of_teachers = ifelse(!(is.na(source_number_of_teachers)), source_number_of_teachers, "Data Bank, World Bank - 2016"),
    outdated_number_of_teachers = ifelse(!(is.na(outdated_number_of_teachers)), outdated_number_of_teachers, 1),
    number_of_teachers_primary = ifelse(!(is.na(number_of_teachers_primary)), number_of_teachers_primary, 34694),
    source_number_of_teachers_primary = ifelse(!(is.na(source_number_of_teachers_primary)), source_number_of_teachers_primary, "Data Bank, World Bank - 2016"),
    outdated_number_of_teachers_primary = ifelse(!(is.na(outdated_number_of_teachers_primary)), outdated_number_of_teachers_primary, 1),
    number_of_teachers_secondary = ifelse(!(is.na(number_of_teachers_secondary)), number_of_teachers_secondary, 34286),
    source_number_of_teachers_secondary = ifelse(!(is.na(source_number_of_teachers_secondary)), source_number_of_teachers_secondary, "Data Bank, World Bank - 2016"),
    outdated_number_of_teachers_secondary = ifelse(!(is.na(outdated_number_of_teachers_secondary)),outdated_number_of_teachers_secondary, 1)
  )
# 2. Function to prepare final dataset 
prepare_final_data <- function(df_bid, df_p1) {
  
  # Reshape df_bid from wide to long format
  df_bid <- reshape2::melt(data = df_bid, id.vars = c("country_acronym"))
  colnames(df_bid) <- c("country_acronym", "variable", "bid_value")
  
  # Reshape df_p1 from wide to long format, dropping the 'country' column if present
  df_p1 <- reshape2::melt(data = df_p1 %>% dplyr::select(-country), id.vars = c("country_acronym"))
  colnames(df_p1) <- c("country_acronym", "variable", "p1_value")
  
  # Merge both data frames by country and variable
  # If bid_value is missing, use p1_value; otherwise use bid_value
  df_final <- df_p1 %>% 
    dplyr::left_join(y = df_bid, by = c("country_acronym", "variable")) %>% 
    dplyr::mutate(final_value = ifelse(is.na(bid_value), p1_value, bid_value)) %>% 
    dplyr::select(-c(p1_value, bid_value))
  
  # Reshape back to wide format: one row per country, variables as columns
  df_final <- reshape2::dcast(data = df_final, formula = country_acronym ~ variable, value.var = "final_value")
  
  return(df_final)
}
# Preparing final data
final_data <- prepare_final_data(df_bid = clean_data_formatted, df_p1 = df_product1)
final_data <- merge(x=final_data, y=df_product1 %>% select(country, country_acronym), by="country_acronym")

# Converte para numeric
final_data <- final_data %>% 
  mutate(
    number_of_schools = as.numeric(number_of_schools),
    number_of_students = as.numeric(number_of_students), 
    number_of_students_preprimary = as.numeric(number_of_students_preprimary),
    number_of_students_primary = as.numeric(number_of_students_primary),
    number_of_students_secondary = as.numeric(number_of_students_secondary),
    number_of_teachers = as.numeric(number_of_teachers),
    number_of_teachers_preprimary = as.numeric(number_of_teachers_preprimary),
    number_of_teachers_primary = as.numeric(number_of_teachers_primary),
    number_of_teachers_secondary = as.numeric(number_of_teachers_secondary),
    number_of_urban_schools = as.numeric(number_of_urban_schools),
    number_of_rural_schools = as.numeric(number_of_rural_schools)
  )

# Percentage of rural schools in Latin America
latam_rural <- final_data %>% 
  filter(!(is.na(number_of_urban_schools))) %>% 
  select(country, country_acronym, number_of_schools, number_of_urban_schools, number_of_rural_schools) 
latam_rural <- sum(as.numeric(latam_rural$number_of_rural_schools))/sum(as.numeric(latam_rural$number_of_schools))

df_rural <- final_data %>% 
  mutate(
    p_rural = ifelse(is.na(number_of_urban_schools), latam_rural, number_of_rural_schools/number_of_schools)
  ) %>% 
  select(country, p_rural)

# Including p_rural in df_product_2
final_data <- merge(x=final_data, y=df_rural, by="country")
# Removing df_rural and latam_rural
rm(df_rural, latam_rural)

final_data <- final_data %>% 
  mutate(number_of_rural_schools = ifelse(is.na(number_of_rural_schools), number_of_schools * p_rural, number_of_rural_schools)) %>% 
  select(-p_rural)


#rural_schools <- clean_data %>% 
#  filter(ADM0_PCODE %in% c('CHL', 'COL', 'CRI', 'DOM', 'ECU', 'GTM', 'HND', 'PAN', 'PER', 'PRY', 'SLV')) %>% 
#  mutate(number_of_rural_schools = ifelse(ADM0_EN %in% c("Chile", "Ecuador", "Honduras"), escuelas_inicial_rural+escuelas_primaria_rural+escuelas_media_rural, escuelas_inicial_rural+escuelas_primaria_rural+escuelas_secundaria_rural)) %>% 
#  select(ADM0_EN, number_of_rural_schools) %>% 
#  rename("country" = "ADM0_EN")

#final_data <- merge(x=final_data, y=rural_schools, by="country", all.x=T)
#final_data <- final_data %>% 
#  mutate(
#    number_of_rural_schools = ifelse(is.na(number_of_rural_schools.x), number_of_rural_schools.y, number_of_rural_schools.x)
#  ) %>% 
#  select(-c(number_of_rural_schools.x, number_of_rural_schools.y))

#---------------------
#- Inclui parâmetros - 
#---------------------

# Inclui subregiões na base de dados
final_data <- final_data %>% 
  mutate(
    subregion = case_when(
      country %in% c("Argentina", "Brazil", "Chile", "Paraguay", "Uruguay") ~ "Cono Sur",
      country %in% c("Bolivia (Plurinational State of)", "Colombia", "Ecuador", "Peru", "Venezuela (Bolivarian Republic of)") ~ "Grupo Andino",
      country %in% c("Bahamas", "Barbados", "Guyana", "Jamaica", "Surinam", "Trinidad and Tobago") ~ "Caribe",
      country %in% c("Belize", "Costa Rica", "El Salvador", "Guatemala", "Haiti", "Honduras", "Nicaragua", "Mexico", "Panama", "Dominican Republic") ~ "Centroamérica"
    )
  )

#### 3. Fiber coverage data ####

# Downloading file
#googledrive::drive_download(file = "2024-12-16_ITU_FIBER_COVERAGE_DATA.xlsx", path = here("data", "raw_data", "itu_fiber_coverage_data.xlsx"), overwrite = T)
itu_fiber_coverage_data <- readxl::read_xlsx(here("data", "raw_data", "itu_fiber_coverage_data.xlsx"), skip=1)

# Calculating population between 25km and 50km
itu_fiber_coverage_data_between_25_50 <- itu_fiber_coverage_data %>% filter(Indicator %in% c("Population reach 25km", "Population reach 50km"))
itu_fiber_coverage_data_between_25_50 <- reshape2::dcast(data = itu_fiber_coverage_data_between_25_50, formula = Country ~ Indicator, value.var = "Value")
itu_fiber_coverage_data_between_25_50 <- itu_fiber_coverage_data_between_25_50 %>% mutate(value_between_25_50=`Population reach 50km`-`Population reach 25km`)
itu_fiber_coverage_data <- merge(x=itu_fiber_coverage_data, y=itu_fiber_coverage_data_between_25_50, by="Country")

itu_fiber_coverage_data <- itu_fiber_coverage_data %>% 
  filter(Indicator %in% c("Population reach 50km")) %>% 
  mutate(
    country = case_when(
      Country=="Argentina" ~ "Argentina",
      Country=="Bahamas" ~ "Bahamas",
      Country=="Barbados" ~ "Barbados",
      Country=="Belize" ~ "Belize",
      Country=="Bolivia" ~ "Bolivia (Plurinational State of)",
      Country=="Brazil"	~ "Brazil",
      Country=="Chile" ~ "Chile",
      Country=="Colombia" ~	"Colombia",
      Country=="Costa Rica" ~	"Costa Rica",
      Country=="Dominican Republic" ~	"Dominican Republic",
      Country=="Ecuador" ~ "Ecuador",
      Country=="El Salvador" ~ "El Salvador",
      Country=="Guatemala" ~ "Guatemala",
      Country=="Guyana" ~	"Guyana",
      Country=="Haiti" ~	"Haiti",
      Country=="Honduras" ~	"Honduras",
      Country=="Jamaica" ~	"Jamaica",
      Country=="Mexico" ~	"Mexico",
      Country=="Nicaragua" ~	"Nicaragua",
      Country=="Panama" ~	"Panama",
      Country=="Paraguay" ~	"Paraguay",
      Country=="Peru" ~	 "Peru",
      Country=="Suriname" ~ 	"Surinam",
      Country=="Trinidad & Tobago" ~	"Trinidad and Tobago",
      Country=="Uruguay" ~ "Uruguay",
      Country=="Venezuela" ~	"Venezuela (Bolivarian Republic of)"
    ),
    itu_pop_below_50 = Value,
    itu_pop_above_50 = 1-Value,
    itu_pop_between_25_50 = value_between_25_50,
    
    source_itu = "ITU - Data source: https://bbmaps.itu.int/bbmaps/"
  ) %>% 
  select(country, itu_pop_below_50, itu_pop_above_50, itu_pop_between_25_50, source_itu)

#### 4. Teacher salary data ####
#googledrive::drive_download(file = "Cópia de Pesquisa de preços_Todas as Dimensões", path = here("data", "raw_data", "Pesquisa de preços_Dimensão Internet e Device.xlsx"), overwrite = T)
# Downloading and importing 
df_teacher_price <- readxl::read_xlsx(here("data", "raw_data", "Pesquisa de preços_Dimensão Internet e Device.xlsx"), sheet="Precos Teacher Training", range = "A1:C16")
df_teacher_price <- df_teacher_price %>% select(-`...3`)
colnames(df_teacher_price) <- c("País", "teacher_monthly_salary")
# Adjusting country names
df_teacher_price <- df_teacher_price %>% 
  filter(País != "Cuba") %>% 
  mutate(
    country = case_when(
      País=="Argentina" ~ "Argentina",
      País=="Bahamas" ~ "Bahamas",
      País=="Barbados" ~ "Barbados",
      País=="Belize" ~ "Belize",
      País=="Bolivia" ~ "Bolivia (Plurinational State of)",
      País=="Brasil"	~ "Brazil",
      País=="Chile" ~ "Chile",
      País=="Colômbia" ~	"Colombia",
      País=="Costa Rica" ~	"Costa Rica",
      País=="Dominican Republic" ~	"Dominican Republic",
      País=="Ecuador" ~ "Ecuador",
      País=="El Salvador" ~ "El Salvador",
      País=="Guatemala" ~ "Guatemala",
      País=="Guyana" ~	"Guyana",
      País=="Haiti" ~	"Haiti",
      País=="Honduras" ~	"Honduras",
      País=="Jamaica" ~	"Jamaica",
      País=="México" ~	"Mexico",
      País=="Nicaragua" ~	"Nicaragua",
      País=="Panama" ~	"Panama",
      País=="Paraguay" ~	"Paraguay",
      País=="Peru" ~	 "Peru",
      País=="Suriname" ~ 	"Surinam",
      País=="Trinidad & Tobago" ~	"Trinidad and Tobago",
      País=="Uruguay" ~ "Uruguay",
      País=="Venezuela" ~	"Venezuela (Bolivarian Republic of)"
    ),
    subregion = case_when(
      country %in% c("Argentina", "Brazil", "Chile", "Paraguay", "Uruguay") ~ "Cono Sur",
      country %in% c("Bolivia (Plurinational State of)", "Colombia", "Ecuador", "Peru", "Venezuela (Bolivarian Republic of)") ~ "Grupo Andino",
      country %in% c("Bahamas", "Barbados", "Guyana", "Jamaica", "Surinam", "Trinidad and Tobago") ~ "Caribe",
      country %in% c("Belize", "Costa Rica", "El Salvador", "Guatemala", "Haiti", "Honduras", "Nicaragua", "Mexico", "Panama", "Dominican Republic") ~ "Centroamérica"
    ),
    source_teacher_price = "Sheet Precos Teacher Training"
  ) %>% 
  select(country, subregion, teacher_monthly_salary, source_teacher_price)

#### 5. Consultant prices ####

# Downloading and importing
df_consultant_price <- readxl::read_xlsx(here("data", "raw_data", "Pesquisa de preços_Dimensão Internet e Device.xlsx"), sheet="Preços Governança", range = "A1:F20")
colnames(df_consultant_price) <- c("position", "organization", "country", "local_currency", "local_salary", "consultant_salary")
# Adjusting country names
df_consultant_price <- df_consultant_price %>% 
  mutate(
    country = case_when(
      country == "Chile" ~ "Chile",
      country == "México" ~ "Mexico",
      country == "Colômbia" ~ "Colombia",
      country == "Brasil" ~ "Brazil",
      country == "Argentina" ~ "Argentina",
      country == "Equador" ~ "Ecuador",
      country == "Paraguai" ~ "Paraguay"
    ),
    subregion = case_when(
      country %in% c("Argentina", "Brazil", "Chile", "Paraguay", "Uruguay") ~ "Cono Sur",
      country %in% c("Bolivia (Plurinational State of)", "Colombia", "Ecuador", "Peru", "Venezuela (Bolivarian Republic of)") ~ "Grupo Andino",
      country %in% c("Bahamas", "Barbados", "Guyana", "Jamaica", "Surinam", "Trinidad and Tobago") ~ "Caribe",
      country %in% c("Belize", "Costa Rica", "El Salvador", "Guatemala", "Haiti", "Honduras", "Nicaragua", "Mexico", "Panama", "Dominican Republic") ~ "Centroamérica"
    ),
    source_teacher_price = "Sheet Precos Teacher Training"
  ) %>% 
  select(country, subregion, position, consultant_salary)

#### 6. Internet and device price ####

# Downloading file and adjusting local price variable
df_internet_device_price <- readxl::read_xlsx(here("data", "raw_data", "Pesquisa de preços_Dimensão Internet e Device.xlsx"), sheet="Precos Internet e Devices", range = "A1:H344")
df_internet_device_price$`PREÇO (LOCAL)` <- round(df_internet_device_price$`PREÇO (LOCAL)`, 3)
df_internet_device_price$`PREÇO (US$) 20/03` <- round(as.numeric(df_internet_device_price$`PRECO AJUSTADO`), 3)
df_internet_device_price <- df_internet_device_price %>% select(PAÍS, CATEGORIA, ITEM, MOEDA, `PREÇO (LOCAL)`, `PRECO AJUSTADO`)
colnames(df_internet_device_price) <- c("PAÍS", "internet_device_category", "internet_device_item", "internet_device_currency", "internet_device_local_price", "internet_device_usdollar_price")

# Remove i5 devices 
df_internet_device_price <- df_internet_device_price %>% 
  filter(internet_device_item != "Notebook i5") %>% 
  mutate(
    country = case_when(
      PAÍS == "Argentina" ~ "Argentina",
      PAÍS == "Brasil" ~ "Brazil",
      PAÍS == "Chile" ~ "Chile",
      PAÍS == "Colômbia" ~ "Colombia",
      PAÍS == "Guiana" ~ "Guyana",
      PAÍS == "México" ~ "Mexico",
      PAÍS == "Paraguai" ~ "Paraguay",
      PAÍS == "República Dominicana" ~ "Dominican Republic",
      PAÍS == "Uruguai" ~ "Uruguay"
    ),
    subregion = case_when(
      country %in% c("Argentina", "Brazil", "Chile", "Paraguay", "Uruguay") ~ "Cono Sur",
      country %in% c("Bolivia (Plurinational State of)", "Colombia", "Ecuador", "Peru", "Venezuela (Bolivarian Republic of)") ~ "Grupo Andino",
      country %in% c("Bahamas", "Barbados", "Guyana", "Jamaica", "Surinam", "Trinidad and Tobago") ~ "Caribe",
      country %in% c("Belize", "Costa Rica", "El Salvador", "Guatemala", "Haiti", "Honduras", "Nicaragua", "Mexico", "Panama", "Dominican Republic") ~ "Centroamérica"
    ),
    source_internet_device_price = "Sheet Precos Internet e Devices"
  ) %>% select(
    country, subregion, internet_device_category, internet_device_item, internet_device_currency, internet_device_local_price, internet_device_usdollar_price, source_internet_device_price
  )

#### 7. RED/Platforms ####

# Downloading file and importing file
df_red_platform <- readxl::read_xlsx(here("data", "raw_data", "Pesquisa de preços_Dimensão Internet e Device.xlsx"), sheet=4, range = "A1:G3")
df_red_platform$MÉDIA <- gsub("\\$", "", df_red_platform$MÉDIA)
df_red_platform$MÉDIA <- gsub(",", "", df_red_platform$MÉDIA)

reference_price_management_platform <- as.numeric(df_red_platform %>% filter(TIPO=="Management Platform") %>% select(MÉDIA))
reference_price_teaching_learning <- as.numeric(df_red_platform %>% filter(TIPO=="Teaching and Learning") %>% select(MÉDIA))

#--------------------------------------------------------------
#- CALCULATING REFERENCE PRICES FOR INTERNET AND DEVICE ITEMS - 
#--------------------------------------------------------------
# Calculating reference percent for limit price
df_internet_device_price_limit_reference <- df_internet_device_price %>% 
  mutate(categoria = paste0(internet_device_category, " ", internet_device_item)) %>% 
  group_by(country, categoria) %>% 
  summarise(min = min(internet_device_usdollar_price), max = max(internet_device_usdollar_price)) %>% 
  mutate(diff = (max-min)/min, outlier = ifelse(diff>3, 1, 0)) %>% 
  filter(outlier == 0) 
internet_device_price_limit_reference <- mean(df_internet_device_price_limit_reference$diff)

# Function to calculate reference prices

my_function_sd_latam <- function(country_list, df_internet_device, type, internet_device_filter){
  
  final_dataframe <- data.frame()
  
  ifelse(
    type == "category",
    df_internet_device <- df_internet_device %>% filter(internet_device_category == internet_device_filter),
    df_internet_device <- df_internet_device %>% filter(internet_device_item == internet_device_filter)
  )
  
  # Calculating LATAM standard deviation
  latin_america_sd <- sd(df_internet_device$internet_device_usdollar_price)
  latin_america_mean <- mean(df_internet_device$internet_device_usdollar_price)
  
  for(selected_country in country_list){
    
    # Creating names for each country dataframe
    new_name <- paste0("df_", selected_country)
    # Identifying subregion
    selected_subregion <- as.character(df_product_1 %>% filter(country==selected_country) %>% select(subregion))
    
    # Calculating reference price
    reference_price <- case_when(
      selected_country %in% c(df_internet_device$country) ~ as.numeric(df_internet_device %>% filter(country==selected_country) %>% summarise(mean = mean(internet_device_usdollar_price)) %>% select(mean)),
      selected_subregion %in% c(df_internet_device$subregion) ~ as.numeric(df_internet_device %>% filter(subregion==selected_subregion) %>% group_by(subregion) %>% reframe(mean = mean(internet_device_usdollar_price)) %>% select(mean)),
      !(selected_country %in% c(df_internet_device$country)) & !(selected_subregion %in% c(df_internet_device$subregion)) ~ as.numeric(df_internet_device %>% summarise(mean = mean(internet_device_usdollar_price)) %>% select(mean))
    )
    
    # Creating dataframe
    new_df <- data.frame(
      country = selected_country,
      subregion = selected_subregion,
      reference_price = reference_price,
      latin_america_sd = latin_america_sd,
      "inferior_price" = reference_price-latin_america_sd,
      "superior_price" = reference_price+latin_america_sd
    )
    
    # Binding to final dataframe
    final_dataframe <- rbind(final_dataframe, new_df)
    
    
  }
  
  return(final_dataframe)
  
}
my_function_sd_subgr_outlier <- function(country_list, df_internet_device, type, internet_device_filter){
  
  final_dataframe <- data.frame()
  
  ifelse(
    type == "category",
    df_internet_device <- df_internet_device %>% filter(internet_device_category == internet_device_filter),
    df_internet_device <- df_internet_device %>% filter(internet_device_item == internet_device_filter)
  )
  
  # Calculating LATAM standard deviation
  latin_america_sd <- sd(df_internet_device$internet_device_usdollar_price)
  latin_america_mean <- mean(df_internet_device$internet_device_usdollar_price)
  
  # Removing outlier
  df_internet_device <- df_internet_device %>% 
    filter((internet_device_usdollar_price > (latin_america_mean-(2*latin_america_sd))) & (internet_device_usdollar_price < (latin_america_mean+(2*latin_america_sd))))
  
  for(selected_country in country_list){
    
    # Creating names for each country dataframe
    new_name <- paste0("df_", selected_country)
    # Identifying subregion
    selected_subregion <- as.character(df_product_1 %>% filter(country==selected_country) %>% select(subregion))
    
    # Calculating subregion standard deviation
    df_internet_device_subregion <- df_internet_device %>% filter(subregion==selected_subregion)
    subregion_sd <- sd(df_internet_device_subregion$internet_device_usdollar_price)
    reference_sd <- ifelse(
      is.na(subregion_sd), latin_america_sd, subregion_sd
    )
    
    # Calculating reference price
    reference_price <- case_when(
      selected_country %in% c(df_internet_device$country) ~ as.numeric(df_internet_device %>% filter(country==selected_country) %>% summarise(mean = mean(internet_device_usdollar_price)) %>% select(mean)),
      selected_subregion %in% c(df_internet_device$subregion) ~ as.numeric(df_internet_device %>% filter(subregion==selected_subregion) %>% group_by(subregion) %>% reframe(mean = mean(internet_device_usdollar_price)) %>% select(mean)),
      !(selected_country %in% c(df_internet_device$country)) & !(selected_subregion %in% c(df_internet_device$subregion)) ~ as.numeric(df_internet_device %>% summarise(mean = mean(internet_device_usdollar_price)) %>% select(mean))
    )
    
    # Creating dataframe
    new_df <- data.frame(
      country = selected_country,
      subregion = selected_subregion,
      reference_price = reference_price,
      reference_sd = reference_sd,
      "inferior_price" = reference_price-reference_sd,
      "superior_price" = reference_price+reference_sd
    )
    
    # Binding to final dataframe
    final_dataframe <- rbind(final_dataframe, new_df)
  }
  
  return(final_dataframe)
}
my_function_sd_subgr_outlier_alternative <- function(country_list, df_internet_device, type, internet_device_filter){
  
  final_dataframe <- data.frame()
  
  ifelse(
    type == "category",
    df_internet_device <- df_internet_device %>% filter(internet_device_category == internet_device_filter),
    df_internet_device <- df_internet_device %>% filter(internet_device_item == internet_device_filter)
  )
  
  # Calculating mean by country
  df_internet_device <- df_internet_device %>% 
    group_by(country, subregion) %>% 
    reframe(internet_device_usdollar_price = mean(internet_device_usdollar_price))
  
  # Calculating LATAM standard deviation
  latin_america_sd <- sd(df_internet_device$internet_device_usdollar_price)
  latin_america_mean <- mean(df_internet_device$internet_device_usdollar_price)
  
  # Removing outlier
  df_internet_device <- df_internet_device %>% 
    filter((internet_device_usdollar_price > (latin_america_mean-(2*latin_america_sd))) & (internet_device_usdollar_price < (latin_america_mean+(2*latin_america_sd))))
  
  for(selected_country in country_list){
    
    # Creating names for each country dataframe
    new_name <- paste0("df_", selected_country)
    # Identifying subregion
    selected_subregion <- as.character(df_product_1 %>% filter(country==selected_country) %>% select(subregion))
    
    # Calculating subregion standard deviation
    df_internet_device_subregion <- df_internet_device %>% filter(subregion==selected_subregion)
    subregion_sd <- sd(df_internet_device_subregion$internet_device_usdollar_price)
    reference_sd <- ifelse(
      is.na(subregion_sd), latin_america_sd, subregion_sd
    )
    
    # Calculating reference price
    reference_price <- case_when(
      selected_country %in% c(df_internet_device$country) ~ as.numeric(df_internet_device %>% filter(country==selected_country) %>% summarise(mean = mean(internet_device_usdollar_price)) %>% select(mean)),
      selected_subregion %in% c(df_internet_device$subregion) ~ as.numeric(df_internet_device %>% filter(subregion==selected_subregion) %>% group_by(subregion) %>% reframe(mean = mean(internet_device_usdollar_price)) %>% select(mean)),
      !(selected_country %in% c(df_internet_device$country)) & !(selected_subregion %in% c(df_internet_device$subregion)) ~ as.numeric(df_internet_device %>% summarise(mean = mean(internet_device_usdollar_price)) %>% select(mean))
    )
    
    # Creating dataframe
    new_df <- data.frame(
      country = selected_country,
      subregion = selected_subregion,
      reference_price = reference_price,
      reference_sd = reference_sd,
      "inferior_price" = reference_price-reference_sd,
      "superior_price" = reference_price+reference_sd
    )
    
    # Binding to final dataframe
    final_dataframe <- rbind(final_dataframe, new_df)
  }
  
  return(final_dataframe)
}
function_price_internet_device <- function(country_list, df_internet_device, type, internet_device_filter){
  
  final_dataframe <- data.frame()
  
  ifelse(
    type == "category",
    df_internet_device <- df_internet_device %>% filter(internet_device_category == internet_device_filter),
    df_internet_device <- df_internet_device %>% filter(internet_device_item == internet_device_filter)
  )
  
  # Calculating LATAM standard deviation and mean
  #latin_america_sd <- sd(df_internet_device$internet_device_usdollar_price)
  latin_america_mean <- mean(df_internet_device$internet_device_usdollar_price)
  
  for(selected_country in country_list){
    
    # Creating names for each country dataframe
    new_name <- paste0("df_", selected_country)
    # Identifying subregion
    selected_subregion <- as.character(final_data %>% filter(country==selected_country) %>% select(subregion))
    
    # Calculating reference price
    reference_price <- case_when(
      selected_country %in% c(df_internet_device$country) ~ as.numeric(df_internet_device %>% filter(country==selected_country) %>% summarise(mean = mean(internet_device_usdollar_price)) %>% select(mean)),
      selected_subregion %in% c(df_internet_device$subregion) ~ as.numeric(df_internet_device %>% filter(subregion==selected_subregion) %>% group_by(subregion) %>% reframe(mean = mean(internet_device_usdollar_price)) %>% select(mean)),
      !(selected_country %in% c(df_internet_device$country)) & !(selected_subregion %in% c(df_internet_device$subregion)) ~ as.numeric(df_internet_device %>% summarise(mean = mean(internet_device_usdollar_price)) %>% select(mean))
    )
    
    # Creating dataframe
    new_df <- data.frame(
      country = selected_country,
      subregion = selected_subregion,
      reference_price = reference_price,
      "inferior_price" = reference_price-(reference_price * internet_device_price_limit_reference),
      "superior_price" = reference_price+(reference_price * internet_device_price_limit_reference)
    )
    
    # Binding to final dataframe
    final_dataframe <- rbind(final_dataframe, new_df)
    
    
  }
  
  return(final_dataframe)
}

# Satellite Service
df_satellite_link <- function_price_internet_device(final_data$country, df_internet_device_price, "category", "Link Internet/Satélite")
colnames(df_satellite_link) <- c("country", "subregion", "reference_satellite_link_price", "reference_satellite_link_inferior_price", "reference_satellite_link_superior_price")
# Fiber Link
df_fiber_link <- function_price_internet_device(final_data$country, df_internet_device_price, "category", "Link Internet/Fibra")
colnames(df_fiber_link) <- c("country", "subregion", "reference_fiber_link_price", "reference_fiber_link_inferior_price", "reference_fiber_link_superior_price")
# Access Point
df_access_point <- function_price_internet_device(final_data$country, df_internet_device_price, "item", "Access Point")
colnames(df_access_point) <- c("country", "subregion", "reference_access_point_price", "reference_access_point_inferior_price", "reference_access_point_superior_price")
# Firewall
df_firewall <- function_price_internet_device(final_data$country, df_internet_device_price, "item", "Firewall")
colnames(df_firewall) <- c("country", "subregion", "reference_firewall_price", "reference_firewall_inferior_price", "reference_firewall_superior_price")
# Nobreak
df_nobreak <- function_price_internet_device(final_data$country, df_internet_device_price, "item", "Nobreak")
colnames(df_nobreak) <- c("country", "subregion", "reference_nobreak_price", "reference_nobreak_inferior_price", "reference_nobreak_superior_price")
# Switch
df_switch <- function_price_internet_device(final_data$country, df_internet_device_price, "item", "Switch")
colnames(df_switch) <- c("country", "subregion", "reference_switch_price", "reference_switch_inferior_price", "reference_switch_superior_price")
# Rack 8U
df_rack <- function_price_internet_device(final_data$country, df_internet_device_price, "item", "Rack 8U")
colnames(df_rack) <- c("country", "subregion", "reference_rack_price", "reference_rack_inferior_price", "reference_rack_superior_price")
# Multimedia Projector
df_multimedia_projector <- function_price_internet_device(final_data$country, df_internet_device_price, "item", "Projetor Multimídia")
colnames(df_multimedia_projector) <- c("country", "subregion", "reference_multimedia_projector_price", "reference_multimedia_projector_inferior_price", "reference_multimedia_projector_superior_price")
# Headphone
df_headphone <- function_price_internet_device(final_data$country, df_internet_device_price, "item", "Fone de ouvido")
colnames(df_headphone) <- c("country", "subregion", "reference_headphone_price", "reference_headphone_inferior_price", "reference_headphone_superior_price")
# Charging cart
df_charging_cart <- function_price_internet_device(final_data$country, df_internet_device_price, "item", "Carrinho de carregamento")
colnames(df_charging_cart) <- c("country", "subregion", "reference_charging_cart_price", "reference_charging_cart_inferior_price", "reference_charging_cart_superior_price")
# Desktop
df_desktop <- function_price_internet_device(final_data$country, df_internet_device_price, "item", "Desktop i3")
colnames(df_desktop) <- c("country", "subregion", "reference_desktop_price", "reference_desktop_inferior_price", "reference_desktop_superior_price")
# Tablet
df_tablet <- function_price_internet_device(final_data$country, df_internet_device_price, "item", "Tablet")
colnames(df_tablet) <- c("country", "subregion", "reference_tablet_price", "reference_tablet_inferior_price", "reference_tablet_superior_price")
# Laptop - Notebook i3
df_laptop <- function_price_internet_device(final_data$country, df_internet_device_price, "item", "Notebook i3")
colnames(df_laptop) <- c("country", "subregion", "reference_laptop_price", "reference_laptop_inferior_price", "reference_laptop_superior_price")
# Cloudbook
df_cloudbook <- function_price_internet_device(final_data$country, df_internet_device_price, "item", "Cloudbook")
colnames(df_cloudbook) <- c("country", "subregion", "reference_cloudbook_price", "reference_cloudbook_inferior_price", "reference_cloudbook_superior_price")

# Binding data
df_internet_device_final <- merge(x=df_satellite_link, y=df_fiber_link, by=c("country", "subregion"))
df_internet_device_final <- merge(x=df_internet_device_final, y=df_access_point, by=c("country", "subregion"))
df_internet_device_final <- merge(x=df_internet_device_final, y=df_firewall, by=c("country", "subregion"), all.x=T)
df_internet_device_final <- merge(x=df_internet_device_final, y=df_nobreak, by=c("country", "subregion"), all.x=T)
df_internet_device_final <- merge(x=df_internet_device_final, y=df_switch, by=c("country", "subregion"), all.x=T)
df_internet_device_final <- merge(x=df_internet_device_final, y=df_rack, by=c("country", "subregion"), all.x=T)
df_internet_device_final <- merge(x=df_internet_device_final, y=df_multimedia_projector, by=c("country", "subregion"), all.x=T)
df_internet_device_final <- merge(x=df_internet_device_final, y=df_headphone, by=c("country", "subregion"), all.x=T)
df_internet_device_final <- merge(x=df_internet_device_final, y=df_charging_cart, by=c("country", "subregion"), all.x=T)
df_internet_device_final <- merge(x=df_internet_device_final, y=df_desktop, by=c("country", "subregion"), all.x=T)
df_internet_device_final <- merge(x=df_internet_device_final, y=df_tablet, by=c("country", "subregion"), all.x=T)
df_internet_device_final <- merge(x=df_internet_device_final, y=df_laptop, by=c("country", "subregion"), all.x=T)
df_internet_device_final <- merge(x=df_internet_device_final, y=df_cloudbook, by=c("country", "subregion"), all.x=T)

#------------------------------------------------------------------
#- CALCULATING REFERENCE PRICES FOR TEACHER AND CONSULTANT SALARY - 
#------------------------------------------------------------------

# Function to calculate reference prices for teacher
my_function_teacher <- function(country_list, df_teacher_price){
  
  final_dataframe <- data.frame()
  
  # Calculating LATAM standard deviation
  latin_america_mean <- mean(df_teacher_price$teacher_monthly_salary)
  
  for(selected_country in country_list){
    
    # Creating names for each country dataframe
    new_name <- paste0("df_country_", selected_country)
    # Identifying subregion
    selected_subregion <- as.character(final_data %>% filter(country==selected_country) %>% select(subregion))
    
    # Calculating reference price
    reference_teacher_price <- case_when(
      selected_country %in% c(df_teacher_price$country) ~ as.numeric(df_teacher_price %>% filter(country==selected_country) %>% select(teacher_monthly_salary)),
      selected_subregion %in% c(df_teacher_price$subregion) ~ as.numeric(df_teacher_price %>% filter(subregion==selected_subregion) %>% group_by(subregion) %>% reframe(mean = mean(teacher_monthly_salary)) %>% select(mean)),
      !(selected_country %in% c(df_teacher_price$country)) & !(selected_subregion %in% c(df_teacher_price$subregion)) ~ as.numeric(df_teacher_price %>% summarise(mean = mean(teacher_monthly_salary)) %>% select(mean))
    )
    
    # Creating dataframe
    new_df <- data.frame(
      country = selected_country,
      subregion = selected_subregion,
      reference_teacher_price = reference_teacher_price
    )
    
    # Binding to final dataframe
    final_dataframe <- rbind(final_dataframe, new_df)
    
    
  }
  
  return(final_dataframe)
  
}
df_teacher <- my_function_teacher(final_data$country, df_teacher_price)

# Function to calculate reference prices for consultant
my_function_consultant <- function(country_list, df_consultant_price){
  
  final_dataframe <- data.frame()
  
  # Calculating LATAM standard deviation
  latin_america_mean <- mean(df_consultant_price$consultant_salary)
  
  for(selected_country in country_list){
    
    # Creating names for each country dataframe
    new_name <- paste0("df_country_", selected_country)
    # Identifying subregion
    selected_subregion <- as.character(final_data %>% filter(country==selected_country) %>% select(subregion))
    
    # Calculating reference price
    reference_consultant_price <- case_when(
      selected_country %in% c(df_consultant_price$country) ~ as.numeric(df_consultant_price %>% filter(country==selected_country) %>% summarise(mean = mean(consultant_salary)) %>% select(mean)),
      selected_subregion %in% c(df_consultant_price$subregion) ~ as.numeric(df_consultant_price %>% filter(subregion==selected_subregion) %>% group_by(subregion) %>% reframe(mean = mean(consultant_salary)) %>% select(mean)),
      !(selected_country %in% c(df_consultant_price$country)) & !(selected_subregion %in% c(df_consultant_price$subregion)) ~ as.numeric(df_consultant_price %>% summarise(mean = mean(consultant_salary)) %>% select(mean))
    )
    
    # Creating dataframe
    new_df <- data.frame(
      country = selected_country,
      subregion = selected_subregion,
      reference_consultant_price = reference_consultant_price
    )
    
    # Binding to final dataframe
    final_dataframe <- rbind(final_dataframe, new_df)
    
    
  }
  
  return(final_dataframe)
  
}
df_consultant <- my_function_consultant(final_data$country, df_consultant_price)

#--------------------------------
#- CREATING PRODUCT 2 DATAFRAME - 
#--------------------------------

# Creating dataframe with parameters and prices
df_product_2 <- final_data %>% select(country, subregion, number_of_schools, source_number_of_schools, number_of_students, source_number_of_students, number_of_students_primary, source_number_of_students_primary, number_of_students_secondary, source_number_of_students_secondary, number_of_teachers, source_number_of_teachers, number_of_rural_schools)
# Including CIMA data
df_product_2 <- merge(x=df_product_2, y=df_bid_latam, by="country", all.x=T)
df_product_2 <- merge(x=df_product_2, y=df_cima_latam, by="country", all.x=T)
df_product_2 <- df_product_2 %>% 
  mutate(
    primary_schools_cima = ifelse(is.na(primary_schools_cima.x), primary_schools_cima.y, primary_schools_cima.x),
    secondary_schools_cima = ifelse(is.na(secondary_schools_cima.x), secondary_schools_cima.y, secondary_schools_cima.x),
    source_cima_schools = ifelse(is.na(source_cima_schools.x), source_cima_schools.y, source_cima_schools.x),
  ) %>% 
  select(-c(primary_schools_cima.x, primary_schools_cima.y, secondary_schools_cima.x, secondary_schools_cima.y, source_cima_schools.x, source_cima_schools.y))

# Inclunding ITU fiber data
df_product_2 <- merge(x=df_product_2, y=itu_fiber_coverage_data, by="country", all.x=T)
# Including teacher salary data
df_product_2 <- merge(x=df_product_2, y=df_teacher_price, by=c("country", "subregion"), all.x=T)
# Including internet and device prices
df_product_2 <- merge(x=df_product_2, y=df_internet_device_final, by=c("country", "subregion"), all.x=T)
# Including teacher salary prices
df_product_2 <- merge(x=df_product_2, y=df_teacher, by=c("country", "subregion"), all.x=T)
# Including consultant salary prices
df_product_2 <- merge(x=df_product_2, y=df_consultant, by=c("country", "subregion"), all.x=T)
# Calculating average student per school 
df_product_2 <- df_product_2 %>% 
  mutate(
    avg_student_per_school = (as.numeric(number_of_students_primary)+as.numeric(number_of_students_secondary))/(primary_schools_cima+secondary_schools_cima),
    number_of_classroom = ((as.numeric(number_of_students_primary)/25) + (primary_schools_cima * 3)) + ((as.numeric(number_of_students_secondary)/35) + (secondary_schools_cima * 3)),
    number_of_graduated_teacher = number_of_teachers,
  )
# Convert character to numeric
df_product_2 <- df_product_2 %>% 
  mutate(
    number_of_schools = as.numeric(number_of_schools),
    number_of_students = as.numeric(number_of_students),
    number_of_students_primary = as.numeric(number_of_students_primary),
    number_of_students_secondary = as.numeric(number_of_students_secondary),
    number_of_teachers = as.numeric(number_of_teachers),
    number_of_graduated_teacher = as.numeric(number_of_graduated_teacher)
  )

#----------------------------
#- CALCULATING FINAL PRICES - 
#----------------------------

df_product_2 <- df_product_2 %>% 
  
  mutate(
    
    number_of_schools_satellite = itu_pop_above_50 * number_of_schools,
    number_of_schools_fiber_link = itu_pop_below_50 * number_of_schools,
    number_of_schools_fiber_expansion = itu_pop_between_25_50 * number_of_schools,
    
    # Classification capex/opex
    type_capex = 1,
    type_opex = 48,
    
    # Parameters
    
    # Projected expansion
    parameter_fiber_exp = 25,
    # 1 access point for each 2 schools
    parameter_access_point = 0.5,
    # 1 firewall per school 
    parameter_firewall = 1,
    # 1 nobreak per school
    parameter_nobreak = 1,
    # 1 switch per school
    parameter_switch = 1,
    # 1 rack per school
    parameter_rack = 1,
    
    # 1 device per 20 primary students (min scenario)
    parameter_min_scenario_primary = 1/20,
    # 1 device per 10 secondary students (min scenario)
    parameter_min_scenario_secondary = 1/10,
    
    # Required device (min scenario)
    parameter_required_device_min_scenario = (number_of_students_primary * parameter_min_scenario_primary) + (number_of_students_secondary * parameter_min_scenario_secondary),    
    
    # 1 device per 10 primary students (mild scenario)
    parameter_mild_scenario_primary = 1/10,
    # 1 device per 6 secondary students (mild scenario)
    parameter_mild_scenario_secondary = 1/6,
    
    # Required device (mild scenario)
    parameter_required_device_mild_scenario = (number_of_students_primary * parameter_mild_scenario_primary) + (number_of_students_secondary * parameter_mild_scenario_secondary),
    
    # 1 device per 6 primary students (comprehensive scenario)
    parameter_comprehensive_primary = 1/6,
    # 1 device per 2 secondary students (comprehensive scenario)
    parameter_comprehensive_secondary = 1/2,
    
    # Required device (comprehensive scenario)
    parameter_required_device_comprehensive_scenario = (number_of_students_primary * parameter_comprehensive_primary) + (number_of_students_secondary * parameter_comprehensive_secondary),
    
    # 1 device per teacher
    parameter_device_per_teacher = 1,
    # 1 device per school
    parameter_device_per_school = 1,
    
    # Total devices (min scenario)
    parameter_total_device_min_scenario = parameter_required_device_min_scenario + number_of_teachers,
    # 1 charging cart per 30 equipaments (min scenario)
    parameter_charging_cart_min_scenario = parameter_total_device_min_scenario/30,
    # 1 headphone per device (min scenario)
    parameter_headphone_min_scenario = 1 * parameter_total_device_min_scenario,
    
    # Total devices (mild scenario)
    parameter_total_device_mild_scenario = parameter_required_device_mild_scenario + number_of_teachers,
    # 1 charging cart per 30 equipaments (mild scenario)
    parameter_charging_cart_mild_scenario = parameter_total_device_mild_scenario/30,
    # 1 headphone per device (mild scenario)
    parameter_headphone_mild_scenario = 1 * parameter_total_device_mild_scenario,
    
    # Total devices (comprehensive scenario)
    parameter_total_comprehensive_scenario = parameter_required_device_comprehensive_scenario + number_of_teachers,
    # 1 charging cart per 30 equipaments (comprehensive scenario)
    parameter_charging_cart_comprehensive_scenario = parameter_total_comprehensive_scenario/30,
    # 1 headphone per device (comprehensive scenario)
    parameter_headphone_comprehensive_scenario = 1 * parameter_total_comprehensive_scenario,
    
    # 1 multimedia projector per classroom
    parameter_multimedia_projector = 1 * number_of_classroom,
    
    # Diagnostic tool development
    parameter_diagnostic_tool_development = 25,
    # Diagnostic tool hosting
    parameter_diagnostic_tool_hosting = 0.5,
    # Content production
    parameter_content_production_1 = 5,
    parameter_content_production_2 = 3,
    # LMS Platform Development
    parameter_platform_development = 3,
    # LMS Platform License
    parameter_platform_license = 0.01,
    # LMS Platform Maintenance 
    parameter_platform_maintenance = 1,
    # Specialist trainers
    parameter_teacher_class = 50,
    parameter_specialist_trainers = 2,
    # Course for trainers
    parameter_course_for_trainers = 1.5,
    # Central team (min)
    parameter_central_team_min = 6,
    # Central team (max)
    parameter_central_team_max = 11,
    # Regional team
    parameter_regional_team = number_of_schools /50,
    # Local team
    parameter_local_team = number_of_schools /5,
    # Platform
    parameter_platform = ifelse(
      number_of_students_primary+number_of_students_secondary < 100000, 3, ifelse(
        number_of_students_primary+number_of_students_secondary < 1000000, 1.5, 1
      )
    ),
    
    # Fiber Expansion
    reference_fiber_exp_inferior_price = 4000,
    reference_fiber_exp_superior_price = 15659,
    
    # Training design
    reference_price_training_design_scenario_online60 = 600000,
    
    # Teaching and Learning Platform
    reference_price_teaching_learning = reference_price_teaching_learning,
    
    # Management Platform
    reference_price_management_platform = reference_price_management_platform
    
  ) 

#------------------------
#- SCRIPT P/ BASE FINAL - 
#------------------------

df_product_2_final <- df_product_2 |> 
  select(country, subregion, number_of_schools, number_of_rural_schools, source_number_of_schools, number_of_students, source_number_of_students, number_of_students_primary, source_number_of_students_primary, number_of_students_secondary, source_number_of_students_secondary, number_of_teachers, source_number_of_teachers,  primary_schools_cima, secondary_schools_cima, source_cima_schools, avg_student_per_school, number_of_classroom, itu_pop_below_50, itu_pop_above_50, itu_pop_between_25_50, source_itu, teacher_monthly_salary, source_teacher_price, reference_satellite_link_price,  reference_fiber_link_price, reference_fiber_link_inferior_price, reference_fiber_link_superior_price, reference_access_point_price, reference_access_point_inferior_price, reference_access_point_superior_price, reference_firewall_price, reference_firewall_inferior_price, reference_firewall_superior_price, reference_nobreak_price, reference_nobreak_inferior_price, reference_nobreak_superior_price, reference_switch_price, reference_switch_inferior_price, reference_switch_superior_price, reference_rack_price, reference_rack_inferior_price, reference_rack_superior_price, reference_multimedia_projector_price, reference_multimedia_projector_inferior_price, reference_multimedia_projector_superior_price, reference_headphone_price, reference_headphone_inferior_price, reference_headphone_superior_price, reference_charging_cart_price, reference_charging_cart_inferior_price, reference_charging_cart_superior_price, reference_desktop_price, reference_desktop_inferior_price, reference_desktop_superior_price, reference_tablet_price, reference_tablet_inferior_price, reference_tablet_superior_price, reference_laptop_price, reference_laptop_inferior_price, reference_laptop_superior_price, reference_cloudbook_price, reference_cloudbook_inferior_price, reference_cloudbook_superior_price, reference_price_teaching_learning,  reference_price_management_platform,  reference_teacher_price, reference_consultant_price, number_of_graduated_teacher, number_of_schools_satellite, number_of_schools_fiber_link, number_of_schools_fiber_expansion, type_capex, type_opex, parameter_fiber_exp, parameter_access_point, parameter_firewall, parameter_nobreak, parameter_switch, parameter_rack, parameter_min_scenario_primary, parameter_min_scenario_secondary, parameter_required_device_min_scenario, parameter_mild_scenario_primary, parameter_mild_scenario_secondary, parameter_required_device_mild_scenario, parameter_comprehensive_primary, parameter_comprehensive_secondary, parameter_required_device_comprehensive_scenario, parameter_device_per_teacher, parameter_device_per_school, parameter_total_device_min_scenario, parameter_charging_cart_min_scenario, parameter_headphone_min_scenario, parameter_total_device_mild_scenario, parameter_charging_cart_mild_scenario, parameter_headphone_mild_scenario, parameter_total_comprehensive_scenario, parameter_charging_cart_comprehensive_scenario, parameter_headphone_comprehensive_scenario, parameter_multimedia_projector, parameter_diagnostic_tool_development, parameter_diagnostic_tool_hosting, parameter_content_production_1, parameter_content_production_2, parameter_platform_development, parameter_platform_license, parameter_platform_maintenance, parameter_teacher_class, parameter_specialist_trainers, parameter_course_for_trainers, parameter_central_team_min, parameter_central_team_max, parameter_regional_team, parameter_local_team, parameter_platform, reference_fiber_exp_inferior_price, reference_fiber_exp_superior_price)

df_product_2_final <- df_product_2_final |>
  # 1. Renomeia a coluna específica primeiro
  rename(reference_fiber_exp_price = reference_fiber_exp_superior_price) |>
  # 2. Remove todas as colunas que contenham "_inferior_price" ou "_superior_price"
  select(-contains(c("_inferior_price", "_superior_price"))) |>
  # 3. Adiciona as novas colunas de referência
  mutate(
    # Referências para Ferramentas de Diagnóstico
    reference_price_diagnostic_tool_development = reference_teacher_price * 25,
    reference_price_diagnostic_tool_hosting = reference_teacher_price * 0.5 * type_opex,
    # Referências para Plataforma LMS
    reference_price_lms_platform_development = reference_teacher_price * 3 * type_capex,
    reference_price_lms_platform_maintenance = reference_teacher_price * type_opex,
    # Referências para Produção de Conteúdo e Treinamento
    reference_price_content_production60 = reference_teacher_price * 5 * 3, # Esta é a base, multiplicadores vêm do df
    reference_price_training_design = 600000,
    reference_price_trainers60 = 2 * reference_teacher_price, # Esta é a base, multiplicador vem do df
    # Multiplicadores de Cenário
    reference_multiplier_inperson = 5,
    reference_multiplier40h = 0.7
  ) |>
  # Altera o valor da coluna parameter_regional_team para 1/50
  mutate(
    parameter_regional_team = 1/50
  ) |>
  mutate(reference_fiber_link_price = reference_fiber_link_price * 200,
         reference_price_teaching_learning = case_when(
           number_of_students > 1000000 ~ reference_price_teaching_learning, # Países com mais de 1 milhão: mesmo preço
           number_of_students >= 100000 & number_of_students <= 1000000 ~ reference_price_teaching_learning * 1.5, # Entre 100 mil e 1 milhão: 1.5X
           number_of_students < 100000 ~ reference_price_teaching_learning * 3, # Menos de 100 mil: 3X
         )
  ) |>
  # Define os valores de parameter_teaching_learning e parameter_management para 1
  mutate(
    parameter_teaching_learning = 1,
    parameter_management = 1,
    parameter_multimedia_projector = 1, 
    parameter_local_team_urban = 1/5,
    parameter_local_team_rural = 1/3,
  ) |>
  select(-parameter_platform)

df_product_2_final <- df_product_2_final |>
  mutate(
    reference_price_diagnostic_tool = reference_price_diagnostic_tool_development + 
      reference_price_diagnostic_tool_hosting,
    reference_price_lms = reference_price_lms_platform_development +
      reference_price_lms_platform_maintenance
  ) |>
  # Reordena as colunas para incluir as novas referências perto das existentes
  select(
    country, subregion, number_of_schools, source_number_of_schools, number_of_rural_schools,
    number_of_students, source_number_of_students, number_of_students_primary,
    source_number_of_students_primary, number_of_students_secondary,
    source_number_of_students_secondary, number_of_teachers, source_number_of_teachers,
    primary_schools_cima, secondary_schools_cima, source_cima_schools,
    #student_primary_databank_worldbank, student_secondary_databank_worldbank,source_databank_worldbank_students, 
    avg_student_per_school,number_of_rural_schools, number_of_classroom,
    itu_pop_below_50, itu_pop_above_50, itu_pop_between_25_50, source_itu,
    teacher_monthly_salary, source_teacher_price,
    
    # --- Colunas de Referência (agrupadas e ordenadas) ---
    reference_satellite_link_price,
    reference_fiber_link_price,
    reference_fiber_exp_price,
    reference_access_point_price,
    reference_firewall_price,
    reference_nobreak_price,
    reference_switch_price,
    reference_rack_price,
    reference_multimedia_projector_price,
    reference_headphone_price,
    reference_charging_cart_price,
    reference_desktop_price,
    reference_tablet_price,
    reference_laptop_price,
    reference_cloudbook_price,
    reference_price_teaching_learning,
    reference_price_management_platform,
    reference_consultant_price,
    reference_teacher_price,
    
    # Novas colunas de referência inseridas aqui, em ordem lógica
    number_of_graduated_teacher,
    reference_price_diagnostic_tool,
    reference_price_lms,
    reference_price_content_production60,
    reference_price_training_design,
    reference_price_trainers60,
    reference_multiplier_inperson,
    reference_multiplier40h,
    # --- Fim das colunas de Referência ---
    
    type_opex, parameter_fiber_exp, parameter_access_point,
    parameter_firewall, parameter_nobreak, parameter_switch, parameter_rack,
    parameter_min_scenario_primary, parameter_min_scenario_secondary, parameter_mild_scenario_primary,
    parameter_mild_scenario_secondary,
    parameter_comprehensive_primary, parameter_comprehensive_secondary, parameter_device_per_teacher,
    parameter_device_per_school,
    parameter_multimedia_projector,
    parameter_teacher_class,  parameter_teaching_learning, 
    parameter_management,parameter_central_team_min,
    parameter_central_team_max, parameter_regional_team,   parameter_local_team_urban,
    parameter_local_team_rural
  )



#------------------
#- AJUSTES FINAIS - 
#------------------

df_product_2_final$source_number_of_students <- gsub("DATA UIS", "Unesco", df_product_2_final$source_number_of_students)
df_product_2_final$source_number_of_students_primary <- gsub("DATA UIS", "Unesco", df_product_2_final$source_number_of_students_primary)
df_product_2_final$source_number_of_students_secondary <- gsub("DATA UIS", "Unesco", df_product_2_final$source_number_of_students_secondary)
df_product_2_final$source_number_of_teachers <- gsub("DATA UIS", "Unesco", df_product_2_final$source_number_of_teachers)

#lista_rural <- c("Bahamas","Barbados","Bolivia (Plurinational State of)","Dominican Republic","El Salvador","Guatemala","Guyana","Haiti","Jamaica","Mexico","Nicaragua","Trinidad and Tobago","Venezuela (Bolivarian Republic of)")
#df_product_2_final$rural_schools <- ifelse(
#  df_product_2_final$country %in% lista_rural, "Estimated based on Latam average", ""
#)

#data.table::fwrite(x=df_product_2_final, file=here("data", "silver", "df_product_2.csv"))
#writexl::write_xlsx(x=df_product_2_final, path = here("data", "silver", "df_product_2.xlsx"))



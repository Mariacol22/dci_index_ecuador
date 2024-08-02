# Titulo de la Sintaxis: 
# Prevalencia de desnutrición crónica en niñas/os menores de 1 años 
# Operación Estadística: 
# Encuesta Nacional sobre Desnutrición Infantil (ENDI 2022 - 2023)
# Paquetes disponibles desde CRAN


pacman::p_load(
  
  # Gestión de proyectos y archivos 
  here, # construye rutas a los archivos de su proyecto 
  rio, # importación / exportación de varios tipos de datos
  expss, # tablas, etiquetas y funciones de hojas de cálculo y estadísticas de 'SPSS'
  
  # Instalación y manejo de paquetes 
  pacman, # instalar y cargar paquetes 
  
  # Manejo general de los datos 
  tidyverse, # incluye paquetes para ordenar y presentar los datos 
  lubridate, # trabaja con fechas 
  pillar, # herramientas para manejar columnas de datos
  janitor, # Limpieza de datos y tablas
  sjlabelled, # para tratar etiquetas 
  epikit, # agregar categorías
  
  # Estadísticas
  summarytools, # herramientas para resumir datos de forma rápida y ordenada
  
  # Manejo de muestras complejas 
  srvyr, # estadística de resumen para datos de encuestas

  # Paquetes para cálculos específicos 
  anthro, # cálculo de puntuaciones z de antropometría infantil
  haven,
  openxlsx,
  highcharter,
  jsonlite,
  httr
)
# Limpieza del espacio de trabajo 
rm(list = ls(all = TRUE))

#==============================================================================#
#### Funciones ####
#==============================================================================#
# Función para calcular estadísticos para variables dicotómicas 
srvyr_prop <- function(design, x) {
  
  design %>% 
    summarise(
      props = survey_mean({{ x }},
                          proportion = TRUE, 
                          vartype = c("se", "ci", "cv"),
                          na.rm = T) * 100, 
      deff = survey_mean({{ x }}, 
                         deff = "replace", 
                         na.rm = T),
      Num = sum({{ x }}, na.rm = TRUE),
      Deno = sum(!is.na({{ x }}))) %>%
    mutate(desag = "Nacional") %>%
    select(
      Desag = desag,
      Props = props, 
      EE = props_se, 
      LI = props_low, 
      LS = props_upp, 
      CV = props_cv,
      Deff = deff_deff,
      Num = Num, 
      Deno = Deno
    ) 
  
} 

# Función para calcular estadísticos para variables dicotómicas 
# por desagregación 
srvyr_prop_by <- function(design, x, by) {
  
  design %>%
    group_by({{ by }}) %>%
    summarise(
      props = survey_mean({{ x }},
                          proportion = TRUE, 
                          vartype = c("se", "ci", "cv"),
                          na.rm = T) * 100, 
      deff = survey_mean({{ x }}, 
                         deff = "replace", 
                         na.rm = T),
      Num = sum({{ x }}, na.rm = TRUE),
      Deno = sum(!is.na({{ x }}))) %>%
    mutate(desag = {{ by }}) %>%
    select(
      Desag = desag,
      Props = props, 
      EE = props_se, 
      LI = props_low, 
      LS = props_upp, 
      CV = props_cv,
      Deff = deff_deff,
      Num = Num, 
      Deno = Deno
    ) 
  
}

#==============================================================================#
#### Carga de base de datos ####
setwd("C:/Users/Maria Colmenarez/Downloads/BDD_ENDI_R1_rds/BDD_ENDI_R1_rds/rds/")
getwd()
# Base personas
df_f1_personas <- read_dta("BDD_ENDI_R1_f1_personas.dta")
df_f1_personas <- as_tibble(df_f1_personas) 
df_f1_personas 
# Diccionario de variables 
# Cargar el archivo: Diccionario_ENDI.xlsx con la hoja ya especificada 
dicc_f1_per <- import("Diccionario_ENDI.xlsx", 
                      which = "f1_personas")
dicc_f1_per <- as_tibble(dicc_f1_per) 
dicc_f1_per



#==============================================================================#
#### Cálculo de variables antropométricas necesarias para el indicador ####
#==============================================================================#
# Estimación de la edad en días -----------------------------------------------#
df_f1_personas <- df_f1_personas %>% 
  mutate(dob = paste(f1_s5_2_3, f1_s5_2_2, f1_s5_2_1)) %>% 
  mutate(dov = paste(f1_s5_3_3, f1_s5_3_2, f1_s5_3_1)) %>% 
  mutate(dob = as_date(dob)) %>% 
  mutate(dov = as_date(dov)) %>% 
  mutate(edaddias = (dob %--% dov) / days(1)) %>% 
  mutate(edadmeses = trunc((dob %--% dov) / months(1))) %>% 
  mutate(edadanios = trunc((dob %--% dov) / years(1)))
df_f1_personas %>%
  descr(edaddias, 
        stats = c("common"),
        round.digits = 2)

# Estimación del peso (kg) ----------------------------------------------------#
# Validación de las 3 medidas del peso
df_f1_personas <- df_f1_personas %>%
  mutate(aux_peso = abs(f1_s5_4_1 - f1_s5_4_2)) 
df_f1_personas <- df_f1_personas %>%
  mutate(f1_s5_4_3 = case_when(
    aux_peso <= 0.5 & !is.na(f1_s5_4_3) ~ NA_real_,
    TRUE ~ f1_s5_4_3 
  ))
# Se calcula el peso en kg
# Distancia entre las tres medidas 
df_f1_personas <- df_f1_personas %>%
  mutate(d1 = abs(f1_s5_4_1 - f1_s5_4_2)) %>%
  mutate(d2 = abs(f1_s5_4_1 - f1_s5_4_3)) %>%
  mutate(d3 = abs(f1_s5_4_2 - f1_s5_4_3))
# Variable identificador 
# Distancia entre toma 1 y toma 2 es menor o igual a 0.5
df_f1_personas <- df_f1_personas %>%
  mutate(s = case_when(
    d1 <= 0.5 ~ 1,
    d1 > 0.5 ~ 0,
    TRUE ~ NA_real_
  )) 
# Promedio simple entre toma 1 y toma 2
df_f1_personas <- df_f1_personas %>% 
  mutate(peso = case_when(
    s == 1 ~ (f1_s5_4_1 + f1_s5_4_2) / 2,
    TRUE ~ NA_real_
  ))
df_f1_personas %>%
  descr(peso, 
        stats = c("common"),
        round.digits = 2) 
# Caso contrario, promedio de la menor distancia entre las 3 mediciones 
# Distancia mínima
df_f1_personas <- df_f1_personas %>% 
  mutate(dmin = case_when(
    (d1 <= d2 & d1 <= d3) |
      (!is.na(d1) & is.na(d2) & is.na(d3)) ~ d1,
    (d2 <= d1 & d2 <= d3) |
      (!is.na(d2) & is.na(d1) & is.na(d3)) ~ d2,
    (d3 <= d1 & d3 <= d2) |
      (!is.na(d3) & is.na(d1) & is.na(d2)) ~ d3, 
    TRUE ~ NA_real_
  ))
df_f1_personas <- df_f1_personas %>%
  mutate(peso = case_when(
    d3 == dmin ~ (f1_s5_4_2 + f1_s5_4_3) / 2, 
    d2 == dmin ~ (f1_s5_4_1 + f1_s5_4_3) / 2, 
    d1 == dmin ~ (f1_s5_4_1 + f1_s5_4_2) / 2,
    TRUE ~ peso
  )) 
df_f1_personas %>%
  descr(peso, 
        stats = c("common"),
        round.digits = 2) 

# Estimación de la talla (cm) -------------------------------------------------#
# Validación de las 3 medidas de la longitud
df_f1_personas <- df_f1_personas %>%
  mutate(aux_long = abs(f1_s5_5_1 - f1_s5_5_2))
df_f1_personas <- df_f1_personas %>%
  mutate(f1_s5_5_3 = case_when(
    aux_long <= 0.5 & !is.na(f1_s5_5_3) ~ NA_real_,
    TRUE ~ f1_s5_5_3
  ))
# Validación de las 3 medidas de la talla
df_f1_personas <- df_f1_personas %>%
  mutate(aux_tal = abs(f1_s5_6_1 - f1_s5_6_2))
df_f1_personas <- df_f1_personas %>%
  mutate(f1_s5_6_3 = case_when(
    aux_tal <= 0.5 & !is.na(f1_s5_6_3) ~ NA_real_,
    TRUE ~ f1_s5_6_3
  ))
# Se calcula la talla en cm.
# Consolido las tomas para longitud y talla
# Talla 1 
df_f1_personas <- df_f1_personas %>% 
  mutate(talla1 = case_when(
    is.na(f1_s5_5_1) & !is.na(f1_s5_6_1) ~ f1_s5_6_1,
    !is.na(f1_s5_5_1) & is.na(f1_s5_6_1) ~ f1_s5_5_1, 
    TRUE ~ NA_real_
  ))
# Talla 2
df_f1_personas <- df_f1_personas %>% 
  mutate(talla2 = case_when(
    is.na(f1_s5_5_2) & !is.na(f1_s5_6_2) ~ f1_s5_6_2,
    !is.na(f1_s5_5_2) & is.na(f1_s5_6_2) ~ f1_s5_5_2, 
    TRUE ~ NA_real_
  ))
# Talla 3 
df_f1_personas <- df_f1_personas %>% 
  mutate(talla3 = case_when(
    is.na(f1_s5_5_3) & !is.na(f1_s5_6_3) ~ f1_s5_6_3,
    !is.na(f1_s5_5_3) & is.na(f1_s5_6_3) ~ f1_s5_5_3, 
    TRUE ~ NA_real_
  ))
# Distancia entre las tres medidas
df_f1_personas <- df_f1_personas %>%
  mutate(d1_tal = abs(talla1 - talla2)) %>%
  mutate(d2_tal = abs(talla1 - talla3)) %>%
  mutate(d3_tal = abs(talla2 - talla3))
# Variable identificador 
# Distancia entre toma 1 y toma 2 es menor o igual a 0.5
df_f1_personas <- df_f1_personas %>%
  mutate(s_tal = case_when(
    d1_tal <= 0.5 ~ 1, 
    d1_tal > 0.5 ~ 0, 
    TRUE ~ NA_real_
  ))
# Promedio simple entre toma 1 y toma 2
df_f1_personas <- df_f1_personas %>% 
  mutate(talla = case_when(
    s_tal == 1 ~ (talla1 + talla2) / 2, 
    TRUE ~ NA_real_
  ))
df_f1_personas %>%
  descr(talla, 
        stats = c("common"),
        round.digits = 2)
# Caso contrario, promedio de la menor distancia entre las 3 mediciones
# Distancia mínima
df_f1_personas <- df_f1_personas %>% 
  mutate(dmin_tal = case_when(
    (d1_tal <= d2_tal & d1_tal <= d3_tal) |
      (!is.na(d1_tal) & is.na(d2_tal) & is.na(d3_tal)) ~ d1_tal,
    (d2_tal <= d1_tal & d2_tal <= d3_tal) |
      (!is.na(d2_tal) & is.na(d1_tal) & is.na(d3_tal)) ~ d2_tal,
    (d3_tal <= d1_tal & d3_tal <= d2_tal) |
      (!is.na(d3_tal) & is.na(d1_tal) & is.na(d2_tal)) ~ d3_tal, 
    TRUE ~ NA_real_
  ))
df_f1_personas <- df_f1_personas %>%
  mutate(talla = case_when(
    d3_tal == dmin_tal ~ (talla2 + talla3) / 2, 
    d2_tal == dmin_tal ~ (talla1 + talla3) / 2,
    d1_tal == dmin_tal ~ (talla1 + talla2) / 2,
    TRUE ~ talla
  )) 
df_f1_personas %>%
  descr(talla, 
        stats = c("common"),
        round.digits = 2)
# Sexo ------------------------------------------------------------------------#
df_f1_personas <- df_f1_personas %>%
  mutate(sexo = unlabel(f1_s1_2)) 
df_f1_personas %>%
  freq(sexo, cumul = F)

#==============================================================================#
#### Cálculo de puntuaciones z de antropometría infantil ####
#==============================================================================#
# Valoración de los z-scores 
df_f1_personas <- df_f1_personas %>% 
  mutate(anthro_zscores(
    sex = sexo,
    age = edaddias,
    weight = peso,
    lenhei = talla
  ))

#==============================================================================#
#### Construcción de las variables de desnutrición con los z-score ####
#==============================================================================#
# Desnutrición crónica para menores de1 años de edad ------------------------#
# Definición de la edad en días de la población menor a 5 años:
# Días = 365.25 *1 = 365.25
# Indicador 
df_f1_personas <- df_f1_personas %>%
  mutate(dcronica = case_when(
    (zlen >= -6 & zlen < -2) & (edaddias < 365.25 & !is.na(edaddias)) ~ 1,
    (zlen >= -2 & zlen <= 6) & (edaddias < 365.25 & !is.na(edaddias)) ~ 0, 
    TRUE ~ NA_real_ 
  ))
df_f1_personas %>% group_by(dcronica) %>%
  freq(area, cumul = F, report.nas = F)

#==============================================================================#
#### Desagregación ####
#==============================================================================#

# Para establecer las etiquetas como valores 
# Área
df_f1_personas <- df_f1_personas %>%
  mutate(area = as_label(area)) 
df_f1_personas %>%
  freq(area, cumul = F, report.nas = F)
# Región
df_f1_personas <- df_f1_personas %>%
  mutate(region = as_label(region)) 
df_f1_personas %>%
  freq(region, cumul = F, report.nas = F)
# Provincia
df_f1_personas <- df_f1_personas %>%
  mutate(prov = as_label(prov)) 
df_f1_personas %>%
  freq(prov, cumul = F, report.nas = F)
# Sexo
df_f1_personas <- df_f1_personas %>%
  mutate(f1_s1_2 = as_label(f1_s1_2)) 
df_f1_personas %>%
  freq(f1_s1_2, cumul = F, report.nas = F)
# Etnia
df_f1_personas <- df_f1_personas %>%
  mutate(etnia = as_label(etnia))
df_f1_personas %>%
  freq(etnia, cumul = F, report.nas = F)
# Grupos de edad
df_f1_personas <- df_f1_personas %>%
  mutate(grupo_edad_nin = as_label(grupo_edad_nin))
df_f1_personas %>%
  freq(grupo_edad_nin, cumul = F, report.nas = F)
#==============================================================================#
#### Declaración de encuesta ####
#==============================================================================#
survey_design <- df_f1_personas %>% as_survey_design(ids = "id_upm",
                                                     strata = "estrato",
                                                     weights = "fexp")
options(survey.lonely.psu = "adjust")


#### Resultados ponderados ####
#==============================================================================#

#Tablas de frecuencias solo afectados por DCI
columns_to_res <- c("area", "prov","region","f1_s1_2","etnia","grupo_edad_nin")
results_list <- list()
output_file <- "resultados_dci_v6.xlsx"
wb <- createWorkbook()
filtered_data <- df_f1_personas %>%
  filter(dcronica == 1)
weighted_data <- filtered_data %>%
  mutate(weighted_frequency =filtered_data$fexp)

for (col_name in columns_to_res) {
  
  freq_table <- freq(
    x = filtered_data[[col_name]],
    weights = filtered_data$fexp,
    cumul = FALSE,
    report.nas = FALSE
  )
  freq_df <- as.data.frame(freq_table)
  colnames(freq_df) <- c( "Frequency","Percent", "Cum")
  
  results_list[[col_name]] <- freq_df
}
  
  write_xlsx(results_list, path = output_file)

  #Valores con respecto a muestra nacional
  
  # Menores de 1 
  survey_design %>%
    srvyr_prop(dcronica)
  # Área
  survey_design %>%
    srvyr_prop_by(dcronica, area)
  # Región
  survey_design %>%
    srvyr_prop_by(dcronica, region)
  # Provincia
survey_design %>%
  srvyr_prop_by(dcronica, prov)
# Sexo
  survey_design %>%
    srvyr_prop_by(dcronica, f1_s1_2)
  
  # Etnia
  survey_design %>%
    srvyr_prop_by(dcronica, etnia)
  
  # Grupos_edad
  survey_design %>%
    srvyr_prop_by(dcronica, grupo_edad_nin)
  
  #==============================================================================#
  #### Mapa_PROVINCIAS  ####
  #==============================================================================#
  
  
  df<- survey_design %>%
    srvyr_prop_by(dcronica, prov)
  print(df)
  df<-select(df, c(Desag,Props))
  colnames(df)[colnames(df) == "Desag"] <- "prov"
  df$prov <- as.character(df$prov)
 df$prov[12] <- 'Los Rios'
  
 df$prov[13] <- 'Manabi'
  
 df$prov[20] <- 'Sucumbios'
  
 df$prov[2] <- 'Bolivar'
  
 df$prov[22] <- 'Santo Domingo De Los Tsachilas'
  
  mapa_ec <- 
    jsonlite::fromJSON("https://raw.githubusercontent.com/zpio/mapa-ecuador/main/ec-all.geo.json") %>% 
    as.data.frame()
  mapa_ec<- mapa_ec[mapa_ec$features.properties$name!= "Galapagos", ]
  
  mapa_ec<-na.omit(mapa_ec)
  
  provincias <- 
    data.frame(
      name= mapa_ec$features.properties$name,
      lon = mapa_ec$features.properties$longitude,
      lat = mapa_ec$features.properties$latitude
    ) %>% 
    filter(name != is.na(name)) %>% 
    mutate(across(everything(), as.character)) %>% 
    mutate(across(c(lon, lat), as.numeric))
  
  provincias<- provincias[order(provincias$name), ]
  
  joined_data <- left_join( df, provincias,by = c("prov" = "name"))
  provincias<- provincias[provincias$name!= "Galapagos", ]
  # Mapa con colores degradados predeterminados

  highchart() %>% 
    hc_add_series_map(
      map = mapa_ec,
      df = joined_data,
      name= 'Población',
      value = "Props",
      joinBy = c("name", "prov"),
      dataLabels = list(enabled = TRUE, 
                        format = '{point.name}'),
      states = list(hover = list(color='#04635b')),
      borderColor = "#FFFFFF"
    ) %>% 
    hc_mapNavigation(enabled = TRUE) %>% 
    hc_add_theme(hc_theme_smpl()) %>% 
    hc_colorAxis(minColor = "#5ad1c7", maxColor = "#434348") %>% 
    hc_legend(
      layout= 'vertical',
      align= 'right',
      verticalAlign= 'bottom'
    )  
    

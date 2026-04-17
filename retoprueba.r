setwd("~/Javi/data")

#install.packages("data.table")
#install.packages( "Rserve")

# Instalar librerias
library(Rserve)
library(ISLR)
library(caret)
library(dplyr)
library(tidyverse)
library(tidymodels)
library(themis)
library(ggplot2)
library(sf)
library(data.table) #para manejar grandes cantidades de datos
library(stargazer)
Rserve()

data = read_csv('reto_bdd.csv')

data = data %>% 
  filter(
    sexo == 1,
    !is.na(finan_viv)
  ) %>% 
  transmute(
    finan_viv = as.factor(finan_viv),
    educ = case_when(
      is.na(educ) | educ == 0 ~ 'Sin Educación', # También puedes incluir otra condición, si `educ == 0` debe ser 'Sin Educación'
      educ == 1 ~ 'Básica',
      educ == 2 ~ 'Media Superior',
      educ == 3 ~ 'Superior',
      TRUE ~ 'Sin Educación'
    ),
    educ = factor(
      x = educ,
      levels = c('Sin Educación', 'Básica', 'Media Superior', 'Superior')
    ),
    rango_edad = case_when(
      is.na(rango_edad) ~ 'Menor18',
      rango_edad == 1 ~ 'Edad18-29',
      rango_edad == 2 ~ 'Edad30-44',
      rango_edad == 3 ~ 'Edad45-64',
      rango_edad == 4 ~ 'Edad65ymas',
    ),
    rango_edad = factor(
      x = rango_edad,
      levels = c('Menor18', 'Edad18-29', 'Edad30-44', 'Edad45-64','Edad65ymas')
    ),
    soltera,
    total_hijos,
    # Ingresos trimestrales a mensuales
    ing_lab_mensual = ing_lab / 3,
    ing_tran_mensual = ing_tran / 3,
    ing_hog_mensual = ing_hog / 3,
    gasto_tarje_mensual = gastotri_tarje / 3,
    actividad_prim_main,
    actividad_ter_main,
    pres,
    tienetarjeta,
    jefe_propietario,
    ratio_aseq,
    ratio_canas,
    rezago,
    factor = frequency_weights(factor)
  )





## Brincar a segundo código para reemplazar NAs con K vecinos

datos = data %>% 
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))

# Aplicar undersampling manual
set.seed(123)
datos_balanced = datos %>%
  group_by(finan_viv) %>%
  sample_n(min(table(datos$finan_viv))) %>%
  ungroup()

# Separar los datos balanceados
data_split = datos_balanced %>% 
  initial_split(strata = finan_viv)

train = training(data_split)
test = testing(data_split)

# Hacer folds de remuestro
folds = vfold_cv(train, v = 10)

# Receta, ¿qué es lo que quieres hacer?
receta = recipe(
  formula = finan_viv ~ .,
  data = train
) %>% 
  # Se aplica logaritmo a esas variables, si es 0 se pone ese número chiquito
  step_log(soltera, total_hijos, ing_lab_mensual, ing_tran_mensual, ing_hog_mensual, gasto_tarje_mensual, , actividad_prim_main, actividad_ter_main, pres, tienetarjeta, jefe_propietario, ratio_aseq, ratio_canas, rezago, offset = 0.001) %>% 
  # Muta las variables a dummies, omitiendo una variable
  step_dummy(rango_edad, educ) %>% 
  # Normaliza las variables numéricas
  step_normalize(all_numeric_predictors())

# Creas el modelo
reg_logistica = logistic_reg() %>% 
  set_engine('glmnet') %>% 
  set_mode('classification') %>% 
  set_args(penalty = tune(), mixture = 1)  # Penalizacion del Lasso

# Creas el flujo de trabajo con la receta y el modelo
modelo_logistico_glmnet = workflow() %>% 
  add_recipe(receta) %>% 
  add_model(reg_logistica)

# Extraer los hiperparámetros
parametros = modelo_logistico_glmnet %>% 
  extract_parameter_set_dials()

# Crear una malla de calibración con el remuestro y los parámetros
calibracion = modelo_logistico_glmnet %>% 
  tune_grid(
    resamples = folds,
    param_info = parametros,
    grid = 200,
    metrics = metric_set(accuracy, roc_auc),
    control = control_grid(verbose = TRUE)
  )

# Ver métricas de calibración / Tambien se ver el accuract y roc_auc
metricas_calibracion = calibracion %>% 
  collect_metrics()

# Ver el mejor modelo
mejor_modelo = calibracion %>% 
  select_best(metric = 'accuracy')

# Ajustar el modelo final usando el mejor parámetro encontrado
ajuste_final = modelo_logistico_glmnet %>% 
  finalize_workflow(mejor_modelo) %>% 
  last_fit(data_split)

# Ver accuracy del modelo final
ajuste_final %>% 
  collect_metrics() %>%
  filter(.metric == "accuracy")

# Ver matriz de confusión
ajuste_final %>% 
  collect_predictions() %>% 
  conf_mat(truth = finan_viv, estimate = .pred_class)

# Extraer resultados
resultados = ajuste_final %>% 
  extract_workflow() %>% 
  augment(test)  # Asegúrate de usar el conjunto de prueba

# Extraer el modelo ajustado
modelo_final = ajuste_final %>% 
  extract_fit_parsnip()

# Coeficientes del modelo
coeficientes = modelo_final %>% 
  tidy()

print(coeficientes, n = 23)

# Graficar ingreso en el eje x y probabilidad en el eje y

# Calcular el promedio del ingreso
promedio_ingreso = mean(resultados$ing_hog_mensual, na.rm = TRUE)

# Graficar ingreso en el eje x y probabilidad en el eje y, con la línea del promedio
grafico1 = ggplot(
  resultados, 
  aes(
    x = log(ing_hog_mensual / 1000),           # Ingreso en miles para claridad
    y = .pred_1
    # color = finan_viv            # Descomenta si quieres colorear según finan_viv
  )
) +
  geom_point(alpha = 0.5) +       # Puntos para cada observación
  geom_smooth(method = "loess", color = "blue") + # Curva de tendencia
  geom_vline(xintercept = log(promedio_ingreso / 1000), color = "red", linetype = "dashed") + # Línea vertical para el promedio
  labs(
    title = "Probabilidad de Acceso a Financiamiento en función del Ingreso",
    x = "Ingreso (en miles)",
    y = "Probabilidad Predicha de Financiamiento"
    # color = "Financiamiento"     # Leyenda para indicar las clases, si es necesario
  ) +
  annotate("text", x = log(promedio_ingreso / 1000), y = 0.05, 
           label = paste("Promedio:", round(promedio_ingreso / 1000, 1), "mil"), color = "red", hjust = -0.1) +
  theme_minimal()

# Mostrar el gráfico
print(grafico1)


write_csv(resultados, "resultados_financiamiento.csv")




# Grafica de variables importantes (el efecto marginal promedio en la probabilidad de acceder a un financiamiento)

# Escoge una probabilidad de referencia, por ejemplo, 0.5 (opcionalmente puedes ajustarla si deseas)
p_referencia = 0.5

# Calcular epsilon como p * (1 - p)
epsilon = p_referencia * (1 - p_referencia)

# Multiplicar los coeficientes por epsilon para obtener el efecto en la probabilidad
coeficientes_importantes = coeficientes %>%
  filter(term != "(Intercept)") %>%
  mutate(
    term = factor(term, levels = term[order(estimate)]),  # Ordenar por coeficiente
    impacto = estimate * epsilon * 100  # Efecto en la probabilidad como porcentaje
  )

# Graficar los efectos marginales en la probabilidad
grafico2 = ggplot(coeficientes_importantes, aes(x = term, y = impacto)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +  # Girar el gráfico para que las barras sean horizontales
  labs(
    title = "Efecto Marginal Promedio de las Variables en la Probabilidad de Financiamiento",
    x = "Variables",
    y = "Cambio en la Probabilidad (%)"
  ) +
  theme_minimal()

# Mostrar el gráfico
print(grafico2)



######## Segundo Modelo (Demanda de Vivienda) ########
## tenencia (vivienda propia o no) = betas * variables

data2 = read_csv('reto_bdd.csv')

data2 = data2 %>% 
  filter(
    pres == 1
  ) %>% 
  transmute(
    viv_propia = as.factor(viv_propia),
    sexo,
    educ = case_when(
      is.na(educ) | educ == 0 ~ 'Sin Educación', # También puedes incluir otra condición, si `educ == 0` debe ser 'Sin Educación'
      educ == 1 ~ 'Básica',
      educ == 2 ~ 'Media Superior',
      educ == 3 ~ 'Superior',
      TRUE ~ 'Sin Educación'
    ),
    educ = factor(
      x = educ,
      levels = c('Sin Educación', 'Básica', 'Media Superior', 'Superior')
    ),
    rango_edad = case_when(
      is.na(rango_edad) ~ 'Menor18',
      rango_edad == 1 ~ 'Edad18-29',
      rango_edad == 2 ~ 'Edad30-44',
      rango_edad == 3 ~ 'Edad45-64',
      rango_edad == 4 ~ 'Edad65ymas',
    ),
    rango_edad = factor(
      x = rango_edad,
      levels = c('Menor18', 'Edad18-29', 'Edad30-44', 'Edad45-64','Edad65ymas')
    ),
    soltera,
    total_hijos,
    ing_hog, ## Poner que sea 0, y convertir a mensual per cápita (por hogar)
    actividad_prim_main,
    tienetarjeta,
    ratio_aseq, ## Poner que sea 0 los NAs
    rezago,
    factor = frequency_weights(factor)
  )

# Reemplazar NA's
datos2 = data2 %>% 
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))

# Separar los datos balanceados
data_split2 = datos2 %>% 
  initial_split(strata = viv_propia)

train2 = training(data_split2)
test2 = testing(data_split2)

# Hacer folds de remuestro
folds2 = vfold_cv(train2, v = 10)

# Receta, ¿qué es lo que quieres hacer?
receta2 = recipe(
  formula = viv_propia ~ .,
  data = train2
) %>% 
  # Se aplica logaritmo a esas variables, si es 0 se pone ese número chiquito
  step_log(total_hijos, ing_hog, ratio_aseq, offset = 0.001) %>% 
  # Muta las variables a dummies, omitiendo una variable
  step_dummy(rango_edad, educ) %>% 
  # Normaliza las variables numéricas
  step_normalize(all_numeric_predictors())

# Creas el modelo
reg_logistica2 = logistic_reg() %>% 
  set_engine('glmnet') %>% 
  set_mode('classification') %>% 
  set_args(penalty = tune(), mixture = 1)  # Penalizacion del Lasso

# Creas el flujo de trabajo con la receta y el modelo
modelo_logistico_glmnet2 = workflow() %>% 
  add_recipe(receta2) %>% 
  add_model(reg_logistica2)

# Extraer los hiperparámetros
parametros2 = modelo_logistico_glmnet2 %>% 
  extract_parameter_set_dials()

# Crear una malla de calibración con el remuestro y los parámetros
calibracion2 = modelo_logistico_glmnet2 %>% 
  tune_grid(
    resamples = folds2,
    param_info = parametros2,
    grid = 200,
    metrics = metric_set(accuracy, roc_auc),
    control = control_grid(verbose = TRUE)
  )

# Ver métricas de calibración / Tambien se ver el accuract y roc_auc
metricas_calibracion2 = calibracion2 %>% 
  collect_metrics()

# Ver el mejor modelo
mejor_modelo2 = calibracion2 %>% 
  select_best(metric = 'accuracy')

# Ajustar el modelo final usando el mejor parámetro encontrado
ajuste_final2 = modelo_logistico_glmnet2 %>% 
  finalize_workflow(mejor_modelo2) %>% 
  last_fit(data_split2)

# Ver accuracy del modelo final
ajuste_final2 %>% 
  collect_metrics() %>%
  filter(.metric == "accuracy")

# Ver matriz de confusión
ajuste_final2 %>% 
  collect_predictions() %>% 
  conf_mat(truth = viv_propia, estimate = .pred_class)

# Extraer resultados
resultados2 = ajuste_final2 %>% 
  extract_workflow() %>% 
  augment(test2)  # Asegúrate de usar el conjunto de prueba

# Extraer el modelo ajustado
modelo_final2 = ajuste_final2 %>% 
  extract_fit_parsnip()

# Coeficientes del modelo
coeficientes2 = modelo_final2 %>% 
  tidy()

print(coeficientes2)



# Hacer mapas de ingreso promedio mensual

## Cargamos datos de remuneraciones del censo económico a nivel municipal
remuneraciones = read_csv("C:/Users/Mariana/Documents/7mo SEM.LEC/OPTIMIZACION/mod_04_concentracion/asesorias/equipo_financiamiento_tenencia/datos/censo_economico/remuneraciones_personal_ocupado.csv")


## Calculamos el ingreso promedio mensual
remuneraciones = remuneraciones %>%
  mutate(
    ingreso_promedio_mensual = (`J000A Total de remuneraciones (millones de pesos)` * 1000000) / `H001A Personal ocupado total` / 12,
    CVEGEO = sprintf("%02s", Entidad) %>% substr(1, 2) %>% paste0(substr(Municipio, 1, 3))
    )

## Ahora sacamos la escolaridad promedio
escolaridad = read_csv(
  "C:/Users/Mariana/Documents/7mo SEM.LEC/OPTIMIZACION/mod_04_concentracion/asesorias/equipo_financiamiento_tenencia/datos/censo_pob_vivienda/censo_pob_2020_grado_escolaridad_municipal.csv"
  ) %>% 
  mutate(CVEGEO = sprintf("%02d%03d", ENTIDAD, MUN))

## Cargar el shapefile de municipios
municipios = st_read("C:/Users/Mariana/Documents/7mo SEM.LEC/OPTIMIZACION/mod_04_concentracion/asesorias/equipo_financiamiento_tenencia/datos/shape_municipios/00mun.shp")

municipios = municipios %>%
  left_join(
    remuneraciones %>% 
    select(CVEGEO, ingreso_promedio_mensual), by = "CVEGEO"
    ) %>%
  left_join(
    escolaridad %>% 
    select(CVEGEO, GRAPROES_F), by = "CVEGEO"
    )

## Coeficientes de la regresión logística (PRELIMINAR)
intercepto = 1.05 
beta_sexo = -0.157
beta_educ_basica = 0.0232
beta_educ_medsup = -0.00329
beta_ing_hog = 0.0805

## Función para calcular la probabilidad
calcula_probabilidad = function(escolaridad, ingreso) {
  odds = exp(intercepto + beta_sexo * 1 + beta_educ_basica * escolaridad + beta_ing_hog * ingreso)
  probabilidad = odds / (1 + odds)
  return(probabilidad)
}

## Calcular la probabilidad para cada municipio y agregarla al data frame
municipios$proba_vivienda_propia = mapply(
  calcula_probabilidad, 
  municipios$GRAPROES_F, 
  municipios$ingreso_promedio_mensual
)


## Graficar la probabilidad de tener vivienda propia a nivel municipal
mapa1 = municipios %>% 
  st_simplify(dTolerance = 1000) %>% 
  st_make_valid() %>% 
  ggplot() +
  geom_sf(aes(fill = proba_vivienda_propia)) +
  scale_fill_viridis_c(option = "viridis", name = "Probabilidad de Vivienda Propia") +
  labs(title = "Probabilidad de Tener Vivienda Propia a Nivel Municipal",
       subtitle = "Estimación basada en ingreso y nivel de educación",
       caption = "Fuente: Datos del censo de población y remuneraciones") +
  theme_minimal()

print(mapa1)


######## Primer modelo con k-vecinos ########

# Aplicar undersampling manual
set.seed(123)
datos_balanced = data %>%
  group_by(finan_viv) %>%
  sample_n(min(table(data$finan_viv))) %>%
  ungroup()

# Separar los datos balanceados
data_split <- datos_balanced %>% 
  initial_split(strata = finan_viv)

train = training(data_split)
test = testing(data_split)

# Hacer folds de remuestro
folds = vfold_cv(train, v = 10)

# Receta, ¿qué es lo que quieres hacer?
receta = recipe(
  formula = finan_viv ~ .,
  data = train
) %>% 
  step_impute_knn(all_predictors(), neighbors = 5) %>%  # Imputación KNN con 5 vecinos
  # Se aplica logaritmo a esas variables, si es 0 se pone ese número chiquito
  step_log(soltera, total_hijos, ing_lab, ing_tran, ing_hog, gastotri_tarje, actividad_prim_main, actividad_ter_main, pres, tienetarjeta, jefe_propietario, ratio_aseq, ratio_canas, rezago, offset = 0.001) %>% 
  # Muta las variables a dummies, omitiendo una variable
  step_dummy(rango_edad, educ) %>% 
  # Normaliza las variables numéricas
  step_normalize(all_numeric_predictors())

# Creas el modelo
reg_logistica = logistic_reg() %>% 
  set_engine('glmnet') %>% 
  set_mode('classification') %>% 
  set_args(penalty = tune(), mixture = 1)  # Penalizacion del Lasso

# Creas el flujo de trabajo con la receta y el modelo
modelo_logistico_glmnet = workflow() %>% 
  add_recipe(receta) %>% 
  add_model(reg_logistica)

# Extraer los hiperparámetros
parametros = modelo_logistico_glmnet %>% 
  extract_parameter_set_dials()

# Crear una malla de calibración con el remuestro y los parámetros
calibracion = modelo_logistico_glmnet %>% 
  tune_grid(
    resamples = folds,
    param_info = parametros,
    grid = 200,
    metrics = metric_set(accuracy, roc_auc),
    control = control_grid(verbose = TRUE)
  )

# Ver métricas de calibración / Tambien se ver el accuract y roc_auc
metricas_calibracion = calibracion %>% 
  collect_metrics()

# Ver el mejor modelo
mejor_modelo = calibracion %>% 
  select_best(metric = 'accuracy')

# Ajustar el modelo final usando el mejor parámetro encontrado
ajuste_final = modelo_logistico_glmnet %>% 
  finalize_workflow(mejor_modelo) %>% 
  last_fit(data_split)

# Ver accuracy del modelo final
ajuste_final %>% 
  collect_metrics() %>%
  filter(.metric == "accuracy")

# Ver matriz de confusión
ajuste_final %>% 
  collect_predictions() %>% 
  conf_mat(truth = finan_viv, estimate = .pred_class)

# Extraer resultados
resultados = ajuste_final %>% 
  extract_workflow() %>% 
  augment(test)  

# Extraer el modelo ajustado
modelo_final = ajuste_final %>% 
  extract_fit_parsnip()

# Coeficientes del modelo
coeficientes = modelo_final %>% 
  tidy()

print(coeficientes, n = 23)



# abrir datos de ensanut 100 k

ensanut_100k <- readRDS("clean_data/ensanut_100k.rds")

# alimentos presentes en ensanut 100k

alimentos <- as.data.frame(levels(as.factor(ensanut_100k$numero_alimento)))
colnames(alimentos) <- "food"

View(alimentos)


# Funcion para conocer porcentaje de consumo de alimentos en el pais

nutri_plot <- function(x = ensanut_100k, food){

  prees <- x %>%
    select(numero_alimento, edad_categorica) %>%
    group_by(numero_alimento, edad_categorica) %>%
    count() %>%
    filter(edad_categorica == "preescolares") %>%
    filter(numero_alimento == food)

  prees_total <- prees$n

  esco <- x %>%
    select(numero_alimento, edad_categorica) %>%
    group_by(numero_alimento, edad_categorica) %>%
    count() %>%
    filter(edad_categorica == "escolares") %>%
    filter(numero_alimento == food)

  esco_total <- esco$n

  adul <- x %>%
    select(numero_alimento, edad_categorica) %>%
    group_by(numero_alimento, edad_categorica) %>%
    count() %>%
    filter(edad_categorica == "adultos") %>%
    filter(numero_alimento == food)

  adul_total <- adul$n


  x %>%
    filter(numero_alimento == food) %>%
    select(dias_comio, edad_categorica) %>%
    group_by(dias_comio, edad_categorica) %>%
    count() %>%
    mutate(porcentaje = case_when(edad_categorica == "preescolares" ~ n/prees_total,
                                  edad_categorica == "escolares" ~ n/esco_total,
                                  edad_categorica == "adultos" ~ n/adul_total),
           edad_categorica = factor(edad_categorica, levels = c("preescolares",
                                                                "escolares", "adultos"))) %>%
    ggplot(aes(x = dias_comio, y = porcentaje, fill = edad_categorica)) +
    xlab("Consumo a la semana") +
    ylab("Porcentaje") +
    geom_col() +
    facet_wrap(.~edad_categorica) +
    theme_classic() +
    theme(legend.position = "none")
}

# funcion para obtener data frame de porcentaje de consumo de alimentos en el pais

nutri_df <- function(x = ensanut_100k, food){

  prees <- x %>%
    select(numero_alimento, edad_categorica) %>%
    group_by(numero_alimento, edad_categorica) %>%
    count() %>%
    filter(edad_categorica == "preescolares") %>%
    filter(numero_alimento == food)

  prees_total <- prees$n

  esco <- x %>%
    select(numero_alimento, edad_categorica) %>%
    group_by(numero_alimento, edad_categorica) %>%
    count() %>%
    filter(edad_categorica == "escolares") %>%
    filter(numero_alimento == food)

  esco_total <- esco$n

  adul <- x %>%
    select(numero_alimento, edad_categorica) %>%
    group_by(numero_alimento, edad_categorica) %>%
    count() %>%
    filter(edad_categorica == "adultos") %>%
    filter(numero_alimento == food)

  adul_total <- adul$n


  x %>%
    filter(numero_alimento == food) %>%
    select(dias_comio, edad_categorica) %>%
    group_by(dias_comio, edad_categorica) %>%
    count() %>%
    mutate(porcentaje = case_when(edad_categorica == "preescolares" ~ n/prees_total,
                                  edad_categorica == "escolares" ~ n/esco_total,
                                  edad_categorica == "adultos" ~ n/adul_total),
           edad_categorica = factor(edad_categorica, levels = c("preescolares",
                                                                "escolares", "adultos")))

    }

# Funcion para obtener frecuencia de consumo de alimentos por estado

nutri_state_plot <- function(x = ensanut_100k, food, state){

  prees <- x %>%
    select(numero_alimento, edad_categorica, descripcion_entidad) %>%
    group_by(numero_alimento, edad_categorica, descripcion_entidad) %>%
    count() %>%
    filter(edad_categorica == "preescolares") %>%
    filter(descripcion_entidad == state) %>%
    filter(numero_alimento == food)

  prees_total <- prees$n

  esco <- x %>%
    select(numero_alimento, edad_categorica, descripcion_entidad) %>%
    group_by(numero_alimento, edad_categorica, descripcion_entidad) %>%
    count() %>%
    filter(edad_categorica == "escolares") %>%
    filter(descripcion_entidad == state) %>%
    filter(numero_alimento == food)

  esco_total <- esco$n

  adul <- x %>%
    select(numero_alimento, edad_categorica, descripcion_entidad) %>%
    group_by(numero_alimento, edad_categorica, descripcion_entidad) %>%
    count() %>%
    filter(edad_categorica == "adultos") %>%
    filter(descripcion_entidad == state) %>%
    filter(numero_alimento == food)

  adul_total <- adul$n


  x %>%
    filter(numero_alimento == food) %>%
    filter(descripcion_entidad == state) %>%
    select(dias_comio, edad_categorica) %>%
    group_by(dias_comio, edad_categorica) %>%
    count() %>%
    mutate(porcentaje = case_when(edad_categorica == "preescolares" ~ n/prees_total,
                                  edad_categorica == "escolares" ~ n/esco_total,
                                  edad_categorica == "adultos" ~ n/adul_total),
           edad_categorica = factor(edad_categorica, levels = c("preescolares",
                                                                "escolares", "adultos"))) %>%
    ggplot(aes(x = dias_comio, y = porcentaje, fill = edad_categorica)) +
    xlab("Consumo a la semana") +
    ylab("Porcentaje") +
    geom_col() +
    facet_wrap(.~edad_categorica) +
    theme_classic() +
    theme(legend.position = "none")
}

nutri_state_df <- function(x = ensanut_100k, food, state){
  prees <- x %>%
    select(numero_alimento, edad_categorica, descripcion_entidad) %>%
    group_by(numero_alimento, edad_categorica, descripcion_entidad) %>%
    count() %>%
    filter(edad_categorica == "preescolares") %>%
    filter(descripcion_entidad == state) %>%
    filter(numero_alimento == food)

  prees_total <- prees$n

  esco <- x %>%
    select(numero_alimento, edad_categorica, descripcion_entidad) %>%
    group_by(numero_alimento, edad_categorica, descripcion_entidad) %>%
    count() %>%
    filter(edad_categorica == "escolares") %>%
    filter(descripcion_entidad == state) %>%
    filter(numero_alimento == food)

  esco_total <- esco$n

  adul <- x %>%
    select(numero_alimento, edad_categorica, descripcion_entidad) %>%
    group_by(numero_alimento, edad_categorica, descripcion_entidad) %>%
    count() %>%
    filter(edad_categorica == "adultos") %>%
    filter(descripcion_entidad == state) %>%
    filter(numero_alimento == food)

  adul_total <- adul$n


  x %>%
    filter(numero_alimento == food) %>%
    filter(descripcion_entidad == state) %>%
    select(dias_comio, edad_categorica) %>%
    group_by(dias_comio, edad_categorica) %>%
    count() %>%
    mutate(porcentaje = case_when(edad_categorica == "preescolares" ~ n/prees_total,
                                  edad_categorica == "escolares" ~ n/esco_total,
                                  edad_categorica == "adultos" ~ n/adul_total),
           edad_categorica = factor(edad_categorica, levels = c("preescolares",
                                                                "escolares", "adultos")))
}

# Funcion para obtener frecuencia de consumo de alimentos por entidad

nutri_city_plot <- function(x = ensanut_100k, food, city){

  prees <- x %>%
    select(numero_alimento, edad_categorica, descripcion_municipio) %>%
    group_by(numero_alimento, edad_categorica, descripcion_municipio) %>%
    count() %>%
    filter(edad_categorica == "preescolares") %>%
    filter(descripcion_municipio == city) %>%
    filter(numero_alimento == food)

  prees_total <- prees$n

  esco <- x %>%
    select(numero_alimento, edad_categorica, descripcion_municipio) %>%
    group_by(numero_alimento, edad_categorica, descripcion_municipio) %>%
    count() %>%
    filter(edad_categorica == "escolares") %>%
    filter(descripcion_municipio == city) %>%
    filter(numero_alimento == food)

  esco_total <- esco$n

  adul <- x %>%
    select(numero_alimento, edad_categorica, descripcion_municipio) %>%
    group_by(numero_alimento, edad_categorica, descripcion_municipio) %>%
    count() %>%
    filter(edad_categorica == "adultos") %>%
    filter(descripcion_municipio == city) %>%
    filter(numero_alimento == food)

  adul_total <- adul$n


  x %>%
    filter(numero_alimento == food) %>%
    filter(descripcion_municipio == city) %>%
    select(dias_comio, edad_categorica) %>%
    group_by(dias_comio, edad_categorica) %>%
    count() %>%
    mutate(porcentaje = case_when(edad_categorica == "preescolares" ~ n/prees_total,
                                  edad_categorica == "escolares" ~ n/esco_total,
                                  edad_categorica == "adultos" ~ n/adul_total),
           edad_categorica = factor(edad_categorica, levels = c("preescolares",
                                                                "escolares", "adultos"))) %>%
    ggplot(aes(x = dias_comio, y = porcentaje, fill = edad_categorica)) +
    xlab("Consumo a la semana") +
    ylab("Porcentaje") +
    geom_col() +
    facet_wrap(.~edad_categorica) +
    theme_classic() +
    theme(legend.position = "none")
}

nutri_city_df <- function(x = ensanut_100k, food, city){
  prees <- x %>%
    select(numero_alimento, edad_categorica, descripcion_municipio) %>%
    group_by(numero_alimento, edad_categorica, descripcion_municipio) %>%
    count() %>%
    filter(edad_categorica == "preescolares") %>%
    filter(descripcion_municipio == city) %>%
    filter(numero_alimento == food)

  prees_total <- prees$n

  esco <- x %>%
    select(numero_alimento, edad_categorica, descripcion_municipio) %>%
    group_by(numero_alimento, edad_categorica, descripcion_municipio) %>%
    count() %>%
    filter(edad_categorica == "escolares") %>%
    filter(descripcion_municipio == city) %>%
    filter(numero_alimento == food)

  esco_total <- esco$n

  adul <- x %>%
    select(numero_alimento, edad_categorica, descripcion_municipio) %>%
    group_by(numero_alimento, edad_categorica, descripcion_municipio) %>%
    count() %>%
    filter(edad_categorica == "adultos") %>%
    filter(descripcion_municipio == city) %>%
    filter(numero_alimento == food)

  adul_total <- adul$n


  x %>%
    filter(numero_alimento == food) %>%
    filter(descripcion_municipio == city) %>%
    select(dias_comio, edad_categorica) %>%
    group_by(dias_comio, edad_categorica) %>%
    count() %>%
    mutate(porcentaje = case_when(edad_categorica == "preescolares" ~ n/prees_total,
                                  edad_categorica == "escolares" ~ n/esco_total,
                                  edad_categorica == "adultos" ~ n/adul_total),
           edad_categorica = factor(edad_categorica, levels = c("preescolares",
                                                                "escolares", "adultos")))
}

# Funcion para obtener frecuencia de consumo de alimentos por localidad

nutri_town_plot <- function(x = ensanut_100k, food, town){

  prees <- x %>%
    select(numero_alimento, edad_categorica, descripcion_localidad) %>%
    group_by(numero_alimento, edad_categorica, descripcion_localidad) %>%
    count() %>%
    filter(edad_categorica == "preescolares") %>%
    filter(descripcion_localidad == town) %>%
    filter(numero_alimento == food)

  prees_total <- prees$n

  esco <- x %>%
    select(numero_alimento, edad_categorica, descripcion_localidad) %>%
    group_by(numero_alimento, edad_categorica, descripcion_localidad) %>%
    count() %>%
    filter(edad_categorica == "escolares") %>%
    filter(descripcion_localidad == town) %>%
    filter(numero_alimento == food)

  esco_total <- esco$n

  adul <- x %>%
    select(numero_alimento, edad_categorica, descripcion_localidad) %>%
    group_by(numero_alimento, edad_categorica, descripcion_localidad) %>%
    count() %>%
    filter(edad_categorica == "adultos") %>%
    filter(descripcion_localidad == town) %>%
    filter(numero_alimento == food)

  adul_total <- adul$n


  x %>%
    filter(numero_alimento == food) %>%
    filter(descripcion_localidad == town) %>%
    select(dias_comio, edad_categorica) %>%
    group_by(dias_comio, edad_categorica) %>%
    count() %>%
    mutate(porcentaje = case_when(edad_categorica == "preescolares" ~ n/prees_total,
                                  edad_categorica == "escolares" ~ n/esco_total,
                                  edad_categorica == "adultos" ~ n/adul_total),
           edad_categorica = factor(edad_categorica, levels = c("preescolares",
                                                                "escolares", "adultos"))) %>%
    ggplot(aes(x = dias_comio, y = porcentaje, fill = edad_categorica)) +
    xlab("Consumo a la semana") +
    ylab("Porcentaje") +
    geom_col() +
    facet_wrap(.~edad_categorica) +
    theme_classic() +
    theme(legend.position = "none")
}

nutri_town_df <- function(x = ensanut_100k, food, town){
  prees <- x %>%
    select(numero_alimento, edad_categorica, descripcion_localidad) %>%
    group_by(numero_alimento, edad_categorica, descripcion_localidad) %>%
    count() %>%
    filter(edad_categorica == "preescolares") %>%
    filter(descripcion_localidad == town) %>%
    filter(numero_alimento == food)

  prees_total <- prees$n

  esco <- x %>%
    select(numero_alimento, edad_categorica, descripcion_localidad) %>%
    group_by(numero_alimento, edad_categorica, descripcion_localidad) %>%
    count() %>%
    filter(edad_categorica == "escolares") %>%
    filter(descripcion_localidad == town) %>%
    filter(numero_alimento == food)

  esco_total <- esco$n

  adul <- x %>%
    select(numero_alimento, edad_categorica, descripcion_localidad) %>%
    group_by(numero_alimento, edad_categorica, descripcion_localidad) %>%
    count() %>%
    filter(edad_categorica == "adultos") %>%
    filter(descripcion_localidad == town) %>%
    filter(numero_alimento == food)

  adul_total <- adul$n


  x %>%
    filter(numero_alimento == food) %>%
    filter(descripcion_localidad == town) %>%
    select(dias_comio, edad_categorica) %>%
    group_by(dias_comio, edad_categorica) %>%
    count() %>%
    mutate(porcentaje = case_when(edad_categorica == "preescolares" ~ n/prees_total,
                                  edad_categorica == "escolares" ~ n/esco_total,
                                  edad_categorica == "adultos" ~ n/adul_total),
           edad_categorica = factor(edad_categorica, levels = c("preescolares",
                                                                "escolares", "adultos")))
}

# Funcion para obtener frecuencia de consumo de alimentos por region
# (norte, sur, centro, cdmx)

nutri_region_plot <- function(x = ensanut_100k, food, region){
  prees <- x %>%
    select(numero_alimento, edad_categorica, region_nutricion) %>%
    group_by(numero_alimento, edad_categorica, region_nutricion) %>%
    count() %>%
    filter(edad_categorica == "preescolares") %>%
    filter(region_nutricion == region) %>%
    filter(numero_alimento == food)

  prees_total <- prees$n

  esco <- x %>%
    select(numero_alimento, edad_categorica, region_nutricion) %>%
    group_by(numero_alimento, edad_categorica, region_nutricion) %>%
    count() %>%
    filter(edad_categorica == "escolares") %>%
    filter(region_nutricion == region) %>%
    filter(numero_alimento == food)

  esco_total <- esco$n

  adul <- x %>%
    select(numero_alimento, edad_categorica, region_nutricion) %>%
    group_by(numero_alimento, edad_categorica, region_nutricion) %>%
    count() %>%
    filter(edad_categorica == "adultos") %>%
    filter(region_nutricion == region) %>%
    filter(numero_alimento == food)

  adul_total <- adul$n


  x %>%
    filter(numero_alimento == food) %>%
    filter(region_nutricion == region) %>%
    select(dias_comio, edad_categorica) %>%
    group_by(dias_comio, edad_categorica) %>%
    count() %>%
    mutate(porcentaje = case_when(edad_categorica == "preescolares" ~ n/prees_total,
                                  edad_categorica == "escolares" ~ n/esco_total,
                                  edad_categorica == "adultos" ~ n/adul_total),
           edad_categorica = factor(edad_categorica, levels = c("preescolares",
                                                                "escolares", "adultos"))) %>%
    ggplot(aes(x = dias_comio, y = porcentaje, fill = edad_categorica)) +
    xlab("Consumo a la semana") +
    ylab("Porcentaje") +
    geom_col() +
    facet_wrap(.~edad_categorica) +
    theme_classic() +
    theme(legend.position = "none")
}

nutri_region_df <- function(x = ensanut_100k, food, region){
  prees <- x %>%
    select(numero_alimento, edad_categorica, region_nutricion) %>%
    group_by(numero_alimento, edad_categorica, region_nutricion) %>%
    count() %>%
    filter(edad_categorica == "preescolares") %>%
    filter(region_nutricion == region) %>%
    filter(numero_alimento == food)

  prees_total <- prees$n

  esco <- x %>%
    select(numero_alimento, edad_categorica, region_nutricion) %>%
    group_by(numero_alimento, edad_categorica, region_nutricion) %>%
    count() %>%
    filter(edad_categorica == "escolares") %>%
    filter(region_nutricion == region) %>%
    filter(numero_alimento == food)

  esco_total <- esco$n

  adul <- x %>%
    select(numero_alimento, edad_categorica, region_nutricion) %>%
    group_by(numero_alimento, edad_categorica, region_nutricion) %>%
    count() %>%
    filter(edad_categorica == "adultos") %>%
    filter(region_nutricion == region) %>%
    filter(numero_alimento == food)

  adul_total <- adul$n


  x %>%
    filter(numero_alimento == food) %>%
    filter(region_nutricion == region) %>%
    select(dias_comio, edad_categorica) %>%
    group_by(dias_comio, edad_categorica) %>%
    count() %>%
    mutate(porcentaje = case_when(edad_categorica == "preescolares" ~ n/prees_total,
                                  edad_categorica == "escolares" ~ n/esco_total,
                                  edad_categorica == "adultos" ~ n/adul_total),
           edad_categorica = factor(edad_categorica, levels = c("preescolares",
                                                                "escolares", "adultos")))
}

# Funcion para obtener frecuencia de consumo de alimentos por area
# (urbano, rural)

nutri_area_plot <- function(x = ensanut_100k, food, zone){
  prees <- x %>%
    select(numero_alimento, edad_categorica, area) %>%
    group_by(edad_categorica) %>%
    count() %>%
    filter(edad_categorica == "preescolares") %>%
    filter(area == zone) %>%
    filter(numero_alimento == food)

  prees_total <- prees$n

  esco <- x %>%
    select(numero_alimento, edad_categorica, area) %>%
    group_by(edad_categorica) %>%
    count() %>%
    filter(edad_categorica == "escolares") %>%
    filter(area == zone) %>%
    filter(numero_alimento == food)

  esco_total <- esco$n

  adul <- x %>%
    select(numero_alimento, edad_categorica, area) %>%
    group_by(edad_categorica) %>%
    count() %>%
    filter(edad_categorica == "adultos") %>%
    filter(area == zone) %>%
    filter(numero_alimento == food)

  adul_total <- adul$n


  x %>%
    filter(numero_alimento == food) %>%
    filter(area == zone) %>%
    select(dias_comio, edad_categorica) %>%
    group_by(edad_categorica) %>%
    count() %>%
    mutate(porcentaje = case_when(edad_categorica == "preescolares" ~ n/prees_total,
                                  edad_categorica == "escolares" ~ n/esco_total,
                                  edad_categorica == "adultos" ~ n/adul_total),
           edad_categorica = factor(edad_categorica, levels = c("preescolares",
                                                                "escolares", "adultos"))) %>%
    ggplot(aes(x = dias_comio, y = porcentaje, fill = edad_categorica)) +
    xlab("Consumo a la semana") +
    ylab("Porcentaje") +
    geom_col() +
    facet_wrap(.~edad_categorica) +
    theme_classic() +
    theme(legend.position = "none")
}

nutri_area_df <- function(x = ensanut_100k, food, zone){
  prees <- x %>%
    select(numero_alimento, edad_categorica, area) %>%
    group_by(edad_categorica) %>%
    count() %>%
    filter(edad_categorica == "preescolares") %>%
    filter(area == zone) %>%
    filter(numero_alimento == food)

  prees_total <- prees$n

  esco <- x %>%
    select(numero_alimento, edad_categorica, area) %>%
    group_by(edad_categorica) %>%
    count() %>%
    filter(edad_categorica == "escolares") %>%
    filter(area == zone) %>%
    filter(numero_alimento == food)

  esco_total <- esco$n

  adul <- x %>%
    select(numero_alimento, edad_categorica, area) %>%
    group_by(edad_categorica) %>%
    count() %>%
    filter(edad_categorica == "adultos") %>%
    filter(area == zone) %>%
    filter(numero_alimento == food)

  adul_total <- adul$n


  x %>%
    filter(numero_alimento == food) %>%
    filter(area == zone) %>%
    select(dias_comio, edad_categorica) %>%
    group_by(edad_categorica) %>%
    count() %>%
    mutate(porcentaje = case_when(edad_categorica == "preescolares" ~ n/prees_total,
                                  edad_categorica == "escolares" ~ n/esco_total,
                                  edad_categorica == "adultos" ~ n/adul_total),
           edad_categorica = factor(edad_categorica, levels = c("preescolares",
                                                                "escolares", "adultos")))
}

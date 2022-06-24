# este script corre
# con esto podemos saber que fruta se consume mas en la zona norte

ensanut_100k %>%
  filter(categoria == "frutas") %>%
  select(descripcion_entidad, descripcion_municipio, descripcion_localidad,
         sexo, numero_alimento, dias_comio, categoria, region_nutricion,
         area, edad_categorica) %>%
  filter(region_nutricion == "norte") %>%
  select(dias_comio, edad_categorica, numero_alimento) %>%
  mutate(consumo = ifelse(dias_comio=="0", 0,1)) %>%
  filter(consumo == 1) %>%
  group_by(numero_alimento, consumo) %>%
  count() %>%
  mutate(porcentaje = n/1810) %>%
  ggplot(aes(x = numero_alimento, y = porcentaje, fill = numero_alimento)) +
  xlab("Frutas") +
  ylab("Porcentaje") +
  geom_col() +
  theme_classic() +
  theme(legend.position = "none")+
  theme(axis.text.x=element_text(angle = 45, hjust = 1))

# dataframe con los datos de la zona norte

ensanut_100k %>%
  filter(categoria == "frutas") %>%
  select(descripcion_entidad, descripcion_municipio, descripcion_localidad,
         sexo, numero_alimento, dias_comio, categoria, region_nutricion,
         area, edad_categorica) %>%
  filter(region_nutricion == "norte") %>%
  select(dias_comio, edad_categorica, numero_alimento) %>%
  mutate(consumo = ifelse(dias_comio=="0", 0,1)) %>%
  filter(consumo == 1) %>%
  group_by(numero_alimento, consumo) %>%
  count() %>%
  mutate(porcentaje = n/1810)

# zona sur

ensanut_100k %>%
  filter(categoria == "frutas") %>%
  select(descripcion_entidad, descripcion_municipio, descripcion_localidad,
         sexo, numero_alimento, dias_comio, categoria, region_nutricion,
         area, edad_categorica) %>%
  filter(region_nutricion == "sur") %>%
  select(dias_comio, edad_categorica, numero_alimento) %>%
  mutate(consumo = ifelse(dias_comio=="0", 0,1)) %>%
  filter(consumo == 1) %>%
  group_by(numero_alimento, consumo) %>%
  count() %>%
  mutate(porcentaje = n/5820) %>%
  ggplot(aes(x = numero_alimento, y = porcentaje, fill = numero_alimento)) +
  xlab("Frutas") +
  ylab("Porcentaje") +
  geom_col() +
  theme_classic() +
  theme(legend.position = "none")+
  theme(axis.text.x=element_text(angle = 45, hjust = 1))

# dataframe con los datos de la zona sur

ensanut_100k %>%
  filter(categoria == "frutas") %>%
  select(descripcion_entidad, descripcion_municipio, descripcion_localidad,
         sexo, numero_alimento, dias_comio, categoria, region_nutricion,
         area, edad_categorica) %>%
  filter(region_nutricion == "sur") %>%
  select(dias_comio, edad_categorica, numero_alimento) %>%
  mutate(consumo = ifelse(dias_comio=="0", 0,1)) %>%
  filter(consumo == 1) %>%
  group_by(numero_alimento, consumo) %>%
  count() %>%
  mutate(porcentaje = n/5820)


#### datos normalizados ###

# zona norte

# grafo
ensanut_100k %>%
  filter(categoria == "frutas") %>%
  select(descripcion_entidad, descripcion_municipio, descripcion_localidad,
         sexo, numero_alimento, dias_comio, categoria, region_nutricion,
         area, edad_categorica) %>%
  filter(region_nutricion == "norte") %>%
  select(dias_comio, edad_categorica, numero_alimento) %>%
  mutate(consumo = ifelse(dias_comio=="0", 0,1)) %>%
  filter(consumo == 1) %>%
  group_by(numero_alimento, consumo) %>%
  count() %>%
  mutate(porcentaje = n/4483) %>%
  ggplot(aes(x = numero_alimento, y = porcentaje, fill = numero_alimento)) +
  xlab("Frutas") +
  ylab("Porcentaje") +
  geom_col() +
  theme_classic() +
  theme(legend.position = "none")+
  theme(axis.text.x=element_text(angle = 45, hjust = 1))

# data frame zona norte

ensanut_100k %>%
  filter(categoria == "frutas") %>%
  select(descripcion_entidad, descripcion_municipio, descripcion_localidad,
         sexo, numero_alimento, dias_comio, categoria, region_nutricion,
         area, edad_categorica) %>%
  filter(region_nutricion == "norte") %>%
  select(dias_comio, edad_categorica, numero_alimento) %>%
  mutate(consumo = ifelse(dias_comio=="0", 0,1)) %>%
  filter(consumo == 1) %>%
  group_by(numero_alimento, consumo) %>%
  count() %>%
  mutate(porcentaje = n/4483)

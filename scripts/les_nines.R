levels(as.factor(ensanut_100k$tamano_porcion))

mango <- ensanut_100k %>%
  filter(numero_alimento == "mango")

levels(as.factor(mango$tamano_porcion))

mango %>% select(tamano_porcion) %>%
  group_by(tamano_porcion) %>%
  count()

ensanut_100k %>%
  filter(categoria == "frutas",
         tamano_porcion %in% c("C", "E", "G", "M")) %>%
  select(tamano_porcion) %>%
  group_by(tamano_porcion) %>%
  count() # TOTAL = 33736



ensanut_100k %>%
  filter(categoria == "frutas",
         tamano_porcion %in% c("C", "E", "G", "M")) %>%
  select(descripcion_entidad, descripcion_municipio, descripcion_localidad,
         numero_alimento, dias_comio, tamano_porcion, categoria, region_nutricion,
         area, edad_categorica) %>%
  group_by(tamano_porcion) %>%
  count() # TOTAL = 33736



ensanut_100k %>%
  filter(categoria == "verduras",
         tamano_porcion %in% c("C", "E", "G", "M")) %>%
  select(tamano_porcion) %>%
  group_by(tamano_porcion) %>%
  count() # Total = 34699


# este sirve

ensanut_100k %>%
  filter(categoria == "frutas") %>%
  select(descripcion_entidad, descripcion_municipio, descripcion_localidad,
         numero_alimento, dias_comio, tamano_porcion, categoria, region_nutricion,
         area, edad_categorica) %>%
  select(numero_alimento, dias_comio) %>%
  group_by(numero_alimento, dias_comio) %>%
  count() %>%
  mutate(porcentaje = n/11754) %>%
  ggplot(aes(x = numero_alimento, y = porcentaje)) +
  geom_boxplot()

# quitando las porciones NA y vacias

ensanut_100k %>%
  filter(categoria == "frutas",
         tamano_porcion %in% c("C", "E", "G", "M")) %>%
  select(descripcion_entidad, descripcion_municipio, descripcion_localidad,
         numero_alimento, dias_comio, tamano_porcion, categoria, region_nutricion,
         area, edad_categorica) %>%
  select(numero_alimento, dias_comio) %>%
  group_by(numero_alimento, dias_comio) %>%
  count() %>%
  mutate(porcentaje = n/11754) %>%
  ggplot(aes(x = numero_alimento, y = porcentaje)) +
  geom_boxplot()

# haciendo esto con todas las categorias de alimentos

ensanut_100k %>%
  filter(tamano_porcion %in% c("C", "E", "G", "M")) %>%
  select(descripcion_entidad, descripcion_municipio, descripcion_localidad,
         numero_alimento, dias_comio, tamano_porcion, categoria, region_nutricion,
         area, edad_categorica) %>%
  select(categoria, dias_comio) %>%
  group_by(categoria, dias_comio) %>%
  count() %>%
  mutate(porcentaje = n/52000) %>%
  ggplot(aes(x = categoria, y = porcentaje, fill = categoria)) +
  geom_boxplot() +
  geom_jitter(aes(color = dias_comio), size = 5)+
  theme_classic()+
  guides(fill = FALSE)


# arreglando el total de la
# este es el bueno, se corrigio la poblacion

ensanut_100k %>%
  filter(tamano_porcion %in% c("C", "E", "G", "M")) %>%
  select(descripcion_entidad, descripcion_municipio, descripcion_localidad,
         numero_alimento, dias_comio, tamano_porcion, categoria, region_nutricion,
         area, edad_categorica) %>%
  select(categoria, dias_comio) %>%
  group_by(categoria, dias_comio) %>%
  count() %>%
  mutate(porcentaje = case_when(categoria == "bebidas" ~ n/51240,
                                categoria == "botanas_dulces_postres" ~ n/23608,
                                categoria == "carnes_embutido_huevo" ~ n/32858,
                                categoria == "cereales_tuberculos" ~ n/26553,
                                categoria == "comida_rapida" ~ n/4434,
                                categoria == "frutas" ~ n/33736,
                                categoria == "lacteos" ~ n/23973,
                                categoria == "leguminosas" ~ n/17356,
                                categoria == "miscelaneos" ~ n/4158,
                                categoria == "pescado_mariscos" ~ n/4522,
                                categoria == "productos_maiz" ~ n/12454,
                                categoria == "sopás_cremas_pastas" ~ n/17933,
                                categoria == "suplementos" ~ n/1654,
                                categoria == "verduras" ~ n/34699,)) %>%
  ggplot(aes(x = categoria, y = porcentaje, fill = categoria)) +
  geom_boxplot() +
  geom_jitter(aes(color = dias_comio), size = 5)+
  xlab("alimento")+
  ylab("consumo")+
  theme_classic()+
  theme(axis.text.x=element_text(angle = 45, hjust = 1))+
  guides(fill = FALSE)


ensanut_100k %>%
  filter(tamano_porcion %in% c("C", "E", "G", "M"),
         categoria == "verduras") %>%
  select(categoria, dias_comio) %>%
  group_by(categoria, dias_comio) %>%
  count() %>%
  ungroup(categoria, dias_comio) %>%
  summarise(total = sum(n))

# zona norte

ensanut_100k %>%
  filter(tamano_porcion %in% c("C", "E", "G", "M"),
         region_nutricion == "norte") %>%
  select(descripcion_entidad, descripcion_municipio, descripcion_localidad,
         numero_alimento, dias_comio, tamano_porcion, categoria, region_nutricion,
         area, edad_categorica) %>%
  select(categoria, dias_comio) %>%
  group_by(categoria, dias_comio) %>%
  count() %>%
  mutate(porcentaje = case_when(categoria == "bebidas" ~ n/7774,
                                categoria == "botanas_dulces_postres" ~ n/4343,
                                categoria == "carnes_embutido_huevo" ~ n/5602,
                                categoria == "cereales_tuberculos" ~ n/4672,
                                categoria == "comida_rapida" ~ n/1124,
                                categoria == "frutas" ~ n/4483,
                                categoria == "lacteos" ~ n/3563,
                                categoria == "leguminosas" ~ n/2772,
                                categoria == "miscelaneos" ~ n/786,
                                categoria == "pescado_mariscos" ~ n/630,
                                categoria == "productos_maiz" ~ n/1461,
                                categoria == "sopás_cremas_pastas" ~ n/2593,
                                categoria == "suplementos" ~ n/181,
                                categoria == "verduras" ~ n/5151,)) %>%
  ggplot(aes(x = categoria, y = porcentaje, fill = categoria)) +
  geom_boxplot() +
  geom_jitter(aes(color = dias_comio), size = 5)+
  xlab("alimento")+
  ylab("consumo")+
  theme_classic()+
  theme(axis.text.x=element_text(angle = 45, hjust = 1))+
  guides(fill = FALSE)


ensanut_100k %>%
  filter(tamano_porcion %in% c("C", "E", "G", "M"),
         region_nutricion == "norte",
         categoria == "verduras") %>%
  select(categoria, dias_comio) %>%
  group_by(categoria, dias_comio) %>%
  count() %>%
  ungroup(categoria, dias_comio) %>%
  summarise(total = sum(n))



## zona sur


ensanut_100k %>%
  filter(tamano_porcion %in% c("C", "E", "G", "M"),
         region_nutricion == "sur") %>%
  select(descripcion_entidad, descripcion_municipio, descripcion_localidad,
         numero_alimento, dias_comio, tamano_porcion, categoria, region_nutricion,
         area, edad_categorica) %>%
  select(categoria, dias_comio) %>%
  group_by(categoria, dias_comio) %>%
  count() %>%
  mutate(porcentaje = case_when(categoria == "bebidas" ~ n/25110,
                                categoria == "botanas_dulces_postres" ~ n/9173,
                                categoria == "carnes_embutido_huevo" ~ n/14992,
                                categoria == "cereales_tuberculos" ~ n/11228,
                                categoria == "comida_rapida" ~ n/1343,
                                categoria == "frutas" ~ n/15958,
                                categoria == "lacteos" ~ n/10656,
                                categoria == "leguminosas" ~ n/8433,
                                categoria == "miscelaneos" ~ n/1606,
                                categoria == "pescado_mariscos" ~ n/2354,
                                categoria == "productos_maiz" ~ n/6793,
                                categoria == "sopás_cremas_pastas" ~ n/8986,
                                categoria == "suplementos" ~ n/901,
                                categoria == "verduras" ~ n/14733,)) %>%
  ggplot(aes(x = categoria, y = porcentaje, fill = categoria)) +
  geom_boxplot() +
  geom_jitter(aes(color = dias_comio), size = 5)+
  xlab("alimento")+
  ylab("consumo")+
  theme_classic()+
  theme(axis.text.x=element_text(angle = 45, hjust = 1))+
  guides(fill = FALSE)


ensanut_100k %>%
  filter(tamano_porcion %in% c("C", "E", "G", "M"),
         region_nutricion == "sur",
         categoria == "verduras") %>%
  select(categoria, dias_comio) %>%
  group_by(categoria, dias_comio) %>%
  count() %>%
  ungroup(categoria, dias_comio) %>%
  summarise(total = sum(n))


### zona centro

ensanut_100k %>%
  filter(tamano_porcion %in% c("C", "E", "G", "M"),
         region_nutricion == "centro") %>%
  select(descripcion_entidad, descripcion_municipio, descripcion_localidad,
         numero_alimento, dias_comio, tamano_porcion, categoria, region_nutricion,
         area, edad_categorica) %>%
  select(categoria, dias_comio) %>%
  group_by(categoria, dias_comio) %>%
  count() %>%
  mutate(porcentaje = case_when(categoria == "bebidas" ~ n/15963,
                                categoria == "botanas_dulces_postres" ~ n/8673,
                                categoria == "carnes_embutido_huevo" ~ n/10568,
                                categoria == "cereales_tuberculos" ~ n/9224,
                                categoria == "comida_rapida" ~ n/1629,
                                categoria == "frutas" ~ n/11422,
                                categoria == "lacteos" ~ n/8309,
                                categoria == "leguminosas" ~ n/5456,
                                categoria == "miscelaneos" ~ n/1480,
                                categoria == "pescado_mariscos" ~ n/1326,
                                categoria == "productos_maiz" ~ n/3717,
                                categoria == "sopás_cremas_pastas" ~ n/5481,
                                categoria == "suplementos" ~ n/531,
                                categoria == "verduras" ~ n/12638)) %>%
  ggplot(aes(x = categoria, y = porcentaje, fill = categoria)) +
  geom_boxplot() +
  geom_jitter(aes(color = dias_comio), size = 5)+
  xlab("alimento")+
  ylab("consumo")+
  theme_classic()+
  theme(axis.text.x=element_text(angle = 45, hjust = 1))+
  guides(fill = FALSE)


ensanut_100k %>%
  filter(tamano_porcion %in% c("C", "E", "G", "M"),
         region_nutricion == "centro",
         categoria == "verduras") %>%
  select(categoria, dias_comio) %>%
  group_by(categoria, dias_comio) %>%
  count() %>%
  ungroup(categoria, dias_comio) %>%
  summarise(total = sum(n))


### agregando los ceros ###

# general



ensanut_100k %>%
  filter(tamano_porcion %in% c("C", "E", "G", "M")) %>%
  select(descripcion_entidad, descripcion_municipio, descripcion_localidad,
         numero_alimento, dias_comio, tamano_porcion, categoria, region_nutricion,
         area, edad_categorica) %>%
  select(categoria, dias_comio) %>%
  group_by(categoria, dias_comio) %>%
  count() %>%
  mutate(porcentaje = case_when(categoria == "bebidas" ~ n/51240,
                                categoria == "botanas_dulces_postres" ~ n/23608,
                                categoria == "carnes_embutido_huevo" ~ n/32858,
                                categoria == "cereales_tuberculos" ~ n/26553,
                                categoria == "comida_rapida" ~ n/4434,
                                categoria == "frutas" ~ n/33736,
                                categoria == "lacteos" ~ n/23973,
                                categoria == "leguminosas" ~ n/17356,
                                categoria == "miscelaneos" ~ n/4158,
                                categoria == "pescado_mariscos" ~ n/4522,
                                categoria == "productos_maiz" ~ n/12454,
                                categoria == "sopás_cremas_pastas" ~ n/17933,
                                categoria == "suplementos" ~ n/1654,
                                categoria == "verduras" ~ n/34699,)) %>%
  ggplot(aes(x = categoria, y = porcentaje, fill = categoria)) +
  geom_boxplot() +
  geom_jitter(aes(color = dias_comio), size = 5)+
  xlab("alimento")+
  ylab("consumo")+
  theme_classic()+
  theme(axis.text.x=element_text(angle = 45, hjust = 1))+
  guides(fill = FALSE)


ensanut_100k %>%
  filter(tamano_porcion %in% c("C", "E", "G", "M"),
         categoria == "verduras") %>%
  select(categoria, dias_comio) %>%
  group_by(categoria, dias_comio) %>%
  count() %>%
  ungroup(categoria, dias_comio) %>%
  summarise(total = sum(n))

# zona norte

ensanut_100k %>%
  filter(tamano_porcion %in% c("C", "E", "G", "M"),
         region_nutricion == "norte") %>%
  select(descripcion_entidad, descripcion_municipio, descripcion_localidad,
         numero_alimento, dias_comio, tamano_porcion, categoria, region_nutricion,
         area, edad_categorica) %>%
  select(categoria, dias_comio) %>%
  group_by(categoria, dias_comio) %>%
  count() %>%
  mutate(porcentaje = case_when(categoria == "bebidas" ~ n/7774,
                                categoria == "botanas_dulces_postres" ~ n/4343,
                                categoria == "carnes_embutido_huevo" ~ n/5602,
                                categoria == "cereales_tuberculos" ~ n/4672,
                                categoria == "comida_rapida" ~ n/1124,
                                categoria == "frutas" ~ n/4483,
                                categoria == "lacteos" ~ n/3563,
                                categoria == "leguminosas" ~ n/2772,
                                categoria == "miscelaneos" ~ n/786,
                                categoria == "pescado_mariscos" ~ n/630,
                                categoria == "productos_maiz" ~ n/1461,
                                categoria == "sopás_cremas_pastas" ~ n/2593,
                                categoria == "suplementos" ~ n/181,
                                categoria == "verduras" ~ n/5151,)) %>%
  ggplot(aes(x = categoria, y = porcentaje, fill = categoria)) +
  geom_boxplot() +
  geom_jitter(aes(color = dias_comio), size = 5)+
  xlab("alimento")+
  ylab("consumo")+
  theme_classic()+
  theme(axis.text.x=element_text(angle = 45, hjust = 1))+
  guides(fill = FALSE)


ensanut_100k %>%
  filter(tamano_porcion %in% c("C", "E", "G", "M"),
         region_nutricion == "norte",
         categoria == "verduras") %>%
  select(categoria, dias_comio) %>%
  group_by(categoria, dias_comio) %>%
  count() %>%
  ungroup(categoria, dias_comio) %>%
  summarise(total = sum(n))



## zona sur


ensanut_100k %>%
  filter(tamano_porcion %in% c("C", "E", "G", "M"),
         region_nutricion == "sur") %>%
  select(descripcion_entidad, descripcion_municipio, descripcion_localidad,
         numero_alimento, dias_comio, tamano_porcion, categoria, region_nutricion,
         area, edad_categorica) %>%
  select(categoria, dias_comio) %>%
  group_by(categoria, dias_comio) %>%
  count() %>%
  mutate(porcentaje = case_when(categoria == "bebidas" ~ n/25110,
                                categoria == "botanas_dulces_postres" ~ n/9173,
                                categoria == "carnes_embutido_huevo" ~ n/14992,
                                categoria == "cereales_tuberculos" ~ n/11228,
                                categoria == "comida_rapida" ~ n/1343,
                                categoria == "frutas" ~ n/15958,
                                categoria == "lacteos" ~ n/10656,
                                categoria == "leguminosas" ~ n/8433,
                                categoria == "miscelaneos" ~ n/1606,
                                categoria == "pescado_mariscos" ~ n/2354,
                                categoria == "productos_maiz" ~ n/6793,
                                categoria == "sopás_cremas_pastas" ~ n/8986,
                                categoria == "suplementos" ~ n/901,
                                categoria == "verduras" ~ n/14733,)) %>%
  ggplot(aes(x = categoria, y = porcentaje, fill = categoria)) +
  geom_boxplot() +
  geom_jitter(aes(color = dias_comio), size = 5)+
  xlab("alimento")+
  ylab("consumo")+
  theme_classic()+
  theme(axis.text.x=element_text(angle = 45, hjust = 1))+
  guides(fill = FALSE)


ensanut_100k %>%
  filter(tamano_porcion %in% c("C", "E", "G", "M"),
         region_nutricion == "sur",
         categoria == "verduras") %>%
  select(categoria, dias_comio) %>%
  group_by(categoria, dias_comio) %>%
  count() %>%
  ungroup(categoria, dias_comio) %>%
  summarise(total = sum(n))


### zona centro

ensanut_100k %>%
  filter(tamano_porcion %in% c("C", "E", "G", "M"),
         region_nutricion == "centro") %>%
  select(descripcion_entidad, descripcion_municipio, descripcion_localidad,
         numero_alimento, dias_comio, tamano_porcion, categoria, region_nutricion,
         area, edad_categorica) %>%
  select(categoria, dias_comio) %>%
  group_by(categoria, dias_comio) %>%
  count() %>%
  mutate(porcentaje = case_when(categoria == "bebidas" ~ n/15963,
                                categoria == "botanas_dulces_postres" ~ n/8673,
                                categoria == "carnes_embutido_huevo" ~ n/10568,
                                categoria == "cereales_tuberculos" ~ n/9224,
                                categoria == "comida_rapida" ~ n/1629,
                                categoria == "frutas" ~ n/11422,
                                categoria == "lacteos" ~ n/8309,
                                categoria == "leguminosas" ~ n/5456,
                                categoria == "miscelaneos" ~ n/1480,
                                categoria == "pescado_mariscos" ~ n/1326,
                                categoria == "productos_maiz" ~ n/3717,
                                categoria == "sopás_cremas_pastas" ~ n/5481,
                                categoria == "suplementos" ~ n/531,
                                categoria == "verduras" ~ n/12638)) %>%
  ggplot(aes(x = categoria, y = porcentaje, fill = categoria)) +
  geom_boxplot() +
  geom_jitter(aes(color = dias_comio), size = 5)+
  xlab("alimento")+
  ylab("consumo")+
  theme_classic()+
  theme(axis.text.x=element_text(angle = 45, hjust = 1))+
  guides(fill = FALSE)


ensanut_100k %>%
  filter(tamano_porcion %in% c("C", "E", "G", "M"),
         region_nutricion == "centro",
         categoria == "verduras") %>%
  select(categoria, dias_comio) %>%
  group_by(categoria, dias_comio) %>%
  count() %>%
  ungroup(categoria, dias_comio) %>%
  summarise(total = sum(n))



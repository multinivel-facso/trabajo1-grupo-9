# Librerias

pacman::p_load(
  haven,
  car,
  corrplot,
  dplyr,
  stargazer,
  lme4,
  texreg,
  reghelper,
  psych
) 


# Cargar base

library(haven)
WVS <- read_sav("input data/WVS_Cross-National_Wave_7_spss_v6_0.sav")
View(WVS)

# Limpieza

# Valores variables

count(WVS, B_COUNTRY) %>% #Países
  print(n = 100) 

count(WVS, Q288)  %>% #Grupo economico
  print(n = 24)  

count(WVS, Q260)  %>% #Género
  print(n = 24)  

count(WVS, Q250)  %>% #importancia de vivir en un país democratico
  print(n = 24)  

count(WVS, Q251)  %>% #Cuan democratico se gobierna el país
  print(n = 24)  


count(WVS, Q243)  %>% #Elecciones libres importantes para la democracia
  print(n = 24)  

count(WVS, Q246)  %>% #derechos civiles como protección ante el estado importantes para la democracia
  print(n = 24)  

count(WVS, Q249)  %>% #igualdad de genero en derechos importante para la democracia
  print(n = 24)  

#  seleccion de variables 
datos <- select(WVS, B_COUNTRY, Q288, Q260, Q250, Q251, Q243, Q246, Q249)%>%
  as.data.frame()
names(datos)

# Tratamiento casos perdidos

datos <- na.omit(datos)

# indice democracia liberal.

alpha(
  select(datos, Q243, Q246, Q249) # 0.73
)

datos <- datos %>%
  mutate(indice_dem = Q243 + Q246 + Q249)

# dummies

class(datos$Q260)

# Convertir Q284 a texto con sus etiquetas
datos <- datos %>%
  mutate(Q260 = as_factor(Q260))

# Luego crear la dummy
datos <- datos %>%
  mutate(mujer = ifelse(Q260 == "Female", 1, 0))

# Crear Q250 nivel 2

datos = datos %>%  
  group_by(B_COUNTRY) %>% 
  mutate(prom_imp_dem = mean(Q250, na.rm = TRUE)) # promedio de percecpción de importancia de la democracia

# crear Q251 nivel 2

datos = datos %>%  
  group_by(B_COUNTRY) %>% 
  mutate(Q251 = mean(Q251, na.rm = TRUE))

# Poner nombres a variables 

datos <- datos %>%        #ojo
  rename(
    país = B_COUNTRY,
    grp_econ = Q288,
    imp_dem = Q250,
    cuan_dem = Q251
  )

# guardar base

save(datos,file="procesamiento/data/data.RData")

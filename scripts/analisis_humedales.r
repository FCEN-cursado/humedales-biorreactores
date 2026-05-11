# ============================================================
#  ANÁLISIS ESTADÍSTICO - HUMEDALES ARTIFICIALES
#  4 tratamientos × 3 réplicas × 2 fechas (inicio / final)
#  Variables: conductividad (µS/cm) y pH
# ============================================================
# Cargar librerías
library(tidyverse)
library(ggpubr) # Facilita añadir estadísticas a los gráficos

# Cargar datos
df1 <- read.csv("../data/tp2-datos.csv")
view(df1)

# Convertir a factores (datos categoricos)
df1 <- df1 %>%
  mutate(
    Tratamiento = factor(Tratamiento),
    Replica     = factor(Replica),
    Fecha       = factor(Fecha, levels = c("inicio", "final"))
  )

# Filtrar solo los datos finales para comparar tratamientos entre sí
df_final <- df1 %>% filter(Fecha == "final")

# Gráfico de puntos con barras de error
ggplot(df_final, aes(x = Tratamiento, y = pH, fill = Tratamiento)) +
  geom_boxplot(alpha = 0.3) + 
  geom_jitter(width = 0.1, size = 2) + # Muestra las 3 réplicas reales
  theme_minimal() +
  labs(title = "pH al final del experimento por tratamiento",
       subtitle = "n=3 por grupo")

# 1. Ajustar el modelo lineal (ANOVA)
# analisis global de diferencias entre tratamientos al final del experimento
# nivel de significancia = probabilidad de obtener este resultado al azar
modelo_ph <- aov(pH ~ Tratamiento, data = df_final)


# 2. Ver el resumen (buscar el Pr(>F))
summary(modelo_ph)

# 3. Si el ANOVA es significativo (p < 0.05), correr Tukey
test_tukey <- TukeyHSD(modelo_ph)
print(test_tukey)

# Graficar comparaciones de Tukey
plot(test_tukey)

# Obtener las letras de tratamientos con diferencias significativas
library(multcompView)
letras <- multcompLetters4(modelo_ph, test_tukey)
print(letras)


# -----
# DELTAS: VARIACION TEMPORAL
# ------
library(tidyverse)

# Grafico gral pH antes y despues
ggplot(df1, aes(x = Fecha, y = pH, fill = Fecha)) +
  # Barras de promedio (con transparencia)
  stat_summary(fun = mean, geom = "bar", alpha = 0.4, color = "black") +
  # Barras de error (SD)
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), 
               geom = "errorbar", width = 0.2) +
  # Puntos individuales y líneas que los unen (opcional pero recomendado)
  geom_line(aes(group = Replica), color = "gray", alpha = 0.5) +
  geom_point(shape = 21, size = 3, alpha = 0.8) +
  # Facetado por tratamiento
  facet_wrap(~Tratamiento, nrow = 1) +
  # --- AQUÍ EL RANGO DEL EJE Y ---
  coord_cartesian(ylim = c(6, 9)) +
  # Estética limpia
  theme_bw() +
  theme(legend.position = "none", # Quitamos la leyenda porque el eje X ya lo dice
        strip.background = element_rect(fill = "white"), # Títulos de facetas limpios
        panel.grid.minor = element_blank()) +
  labs(title = "Variación de pH por Tratamiento",
       subtitle = "Rango visual optimizado (pH 6 - 9)",
       y = "pH",
       x = NULL)

# ---------
## ANALISIS delta PH
# --------
library(tidyverse)

df_delta = read.csv("../data/deltas.csv")
df_delta = read.csv("./data/deltas.csv")
view(df_delta)

# 2. Correr el ANOVA sobre el Delta
modelo_delta_ph <- aov(pH_dif ~ Tratamiento, data = df_delta)
summary(modelo_delta_ph)

# 3. Prueba de Tukey para ver quién cambió MÁS
tukey_delta_ph <- TukeyHSD(modelo_delta_ph)
phtuk = as.data.frame(tukey_delta_ph[1])

library(multcompView)
letras_ph <- multcompLetters4(modelo_delta_ph, tukey_delta_ph)
print(letras_ph)


par(mfrow = c(2,2))

# Gráfico del Delta de pH por tratamiento
ggplot(df_delta, aes(x = Tratamiento, y = pH_dif, fill = Tratamiento)) +
  # Línea de referencia en cero (donde todos empezaron)
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  # Barras de promedio del cambio
  stat_summary(fun = mean, geom = "bar", alpha = 0.6) +
  # Error estándar o Desviación
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +
  # Las 3 réplicas reales
  geom_jitter(width = 0.1, size = 3, shape = 21, color = "black") +
  theme_minimal() +
  labs(title = "Magnitud del cambio de pH por Tratamiento",
       y = "Delta pH (Final - Inicio)") +
  theme(legend.position = "none")


####
## Analisis conductividad
####
modelo_delta_conduct = aov(Conduct_dif ~ Tratamiento, data = df_delta)
summary(modelo_delta_conduct)
# Gráfico conductividad
ggplot(df_delta, aes(x = Tratamiento, y = Conduct_dif, fill = Tratamiento)) +
  # Línea de referencia en cero (donde todos empezaron)
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  # Barras de promedio del cambio
  stat_summary(fun = mean, geom = "bar", alpha = 0.6) +
  # Error estándar o Desviación
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +
  # Las 3 réplicas reales
  geom_jitter(width = 0.1, size = 3, shape = 21, color = "black") +
  theme_minimal() +
  labs(title = "Magnitud del cambio de conductividad por Tratamiento",
       y = "Delta conductividad (Final - Inicio)") +
  theme(legend.position = "none")


###
# NITRATOS Y FOSFATOS
###

modelo_nitratos <- aov(Nitratos ~ Tratamiento, data = df_delta)
summary(modelo_nitratos)
tukey_nitratos <- TukeyHSD(modelo_nitratos)
letras_nitratos <- multcompLetters4(modelo_nitratos, tukey_nitratos)
print(letras_nitratos)


ggplot(df_delta, aes(x = Tratamiento, y = Nitratos, fill = Tratamiento)) +
  # Barras de promedio
  stat_summary(fun = mean, geom = "bar", alpha = 0.6) +
  # Barras de error (Error Estándar)
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +
  # Puntos de las 3 réplicas
  geom_jitter(width = 0.1, size = 3, shape = 21, color = "black", alpha = 0.8) +
  # Estética
  theme_minimal() +
  #scale_fill_brewer(palette = "Set2") + # Colores distintos para nutrientes
  labs(title = "Concentración Final de Nitratos por Tratamiento",
       y = "Nitratos (mg/L)", 
       x = "Tratamiento") +
  theme(legend.position = "none")


modelo_fosfatos <- aov(Fosfatos ~ Tratamiento, data = df_delta)
summary(modelo_fosfatos)
tukey_fosfatos <- TukeyHSD(modelo_fosfatos)
letras_fosfatos <- multcompLetters4(modelo_fosfatos, tukey_fosfatoss)
print(letras_fosfatos)

ggplot(df_delta, aes(x = Tratamiento, y = Fosfatos, fill = Tratamiento)) +
  # Barras de promedio
  stat_summary(fun = mean, geom = "bar", alpha = 0.6) +
  # Barras de error (Error Estándar)
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +
  # Puntos de las 3 réplicas
  geom_jitter(width = 0.1, size = 3, shape = 21, color = "black", alpha = 0.8) +
  # Estética
  theme_minimal() +
  #scale_fill_brewer(palette = "Set2") + # Colores distintos para nutrientes
  labs(title = "Concentración Final de Fosfatos por Tratamiento",
       y = "Fosfatos (mg/L)", 
       x = "Tratamiento") +
  theme(legend.position = "none")


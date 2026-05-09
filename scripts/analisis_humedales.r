# ============================================================
#  ANÁLISIS ESTADÍSTICO - HUMEDALES ARTIFICIALES
#  4 tratamientos × 3 réplicas × 2 fechas (inicio / final)
#  Variables: conductividad (µS/cm) y pH
# ============================================================
# Cargar librerías
library(tidyverse)
library(ggpubr) # Facilita añadir estadísticas a los gráficos

# Cargar datos
df1 <- read.csv("tp2-datos.csv")
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

library(multcompView)
# Obtener las letras de tratamientos con diferencias significativas
letras <- multcompLetters4(modelo_ph, test_tukey)
print(letras)

####
# COMPARACION TRATAMIENTOS TIEMPO
ggplot(df1, aes(x = Fecha, y = pH, fill = Fecha)) +
  stat_summary(fun = mean, geom = "bar", alpha = 0.5) +
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), 
               geom = "errorbar", width = 0.2) +
  geom_jitter(width = 0.1, size = 2) +
  # Aquí ocurre la magia: crea un panel por Tratamiento
  facet_wrap(~Tratamiento, nrow = 1) + 
  theme_bw() +
  labs(title = "Evolución del pH por tratamiento",
       subtitle = "Comparación Inicio vs. Final (n=3)") +
  coord_cartesian(ylim = c(6, 9)) # Ajusta el rango del eje y para mejor visualización




library(tidyverse)

# Asegurar orden de los tiempos

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
       subtitle = "Rango visual optimizado (pH 6 - 10)",
       y = "pH",
       x = NULL)







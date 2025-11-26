#ANÁLISIS DE CLUSTERING K-MEANS

#install.packages(c("tidyverse", "cluster", "factoextra", "gridExtra"))

#librerias
suppressPackageStartupMessages({
  library(tidyverse)
  library(cluster)
  library(factoextra)
  library(gridExtra)
})

cat("\n")
cat("ANALISIS K-MEANS - Congestion de Trafico\n\n")


cat("► Cargando datos.\n")
datos <- read.csv("congestion.csv", stringsAsFactors = FALSE)
cat("  ✓ Dataset cargado:", format(nrow(datos), big.mark = ","), "filas x", ncol(datos), "columnas\n\n")


cat("► Preparando y escalando datos.\n")

#variables clustering
variables_clustering <- datos %>%
  select(Latitud, Longitud, Duration_hrs, Length_km, `Speed_km.h`, Peak_Time)

#borrar filas con NA
variables_clustering <- na.omit(variables_clustering)

#estandarizacion
datos_escalados <- scale(variables_clustering)
datos_escalados <- as.data.frame(datos_escalados)

cat("  ✓ Variables seleccionadas:", ncol(datos_escalados), "\n")
cat("  ✓ Datos escalados (media=0, sd=1)\n\n")

cat("► Determinando numero optimo de clusters...\n")

#muestra
set.seed(123)
n_muestra <- min(3000, nrow(datos_escalados))
muestra <- datos_escalados[sample(nrow(datos_escalados), n_muestra), ]

#elbow
cat("  - Calculando metodo Elbow...\n")
wss <- sapply(1:10, function(k) {
  kmeans(muestra, centers = k, nstart = 10, iter.max = 50)$tot.withinss
})

#silhouette
cat("  - Calculando metodo Silhouette...\n")
silhouette_avg <- sapply(2:10, function(k) {
  km <- kmeans(muestra, centers = k, nstart = 10, iter.max = 50)
  mean(silhouette(km$cluster, dist(muestra))[, 3])
})

#gap (simple)
cat("  - Calculando estadistico Gap...\n")
gap_stat <- clusGap(muestra, FUN = kmeans, nstart = 10, K.max = 8, B = 20)

#graficos
p_elbow <- ggplot(data.frame(k = 1:10, wss = wss), aes(x = k, y = wss)) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_point(color = "steelblue", size = 3) +
  scale_x_continuous(breaks = 1:10) +
  labs(title = "Metodo del Codo (Elbow)", x = "Numero de Clusters (k)", 
       y = "Suma de Cuadrados Intra-Cluster") +
  theme_minimal()

p_silhouette <- ggplot(data.frame(k = 2:10, sil = silhouette_avg), aes(x = k, y = sil)) +
  geom_line(color = "darkgreen", linewidth = 1) +
  geom_point(color = "darkgreen", size = 3) +
  scale_x_continuous(breaks = 2:10) +
  labs(title = "Metodo Silhouette", x = "Numero de Clusters (k)", 
       y = "Coeficiente Silhouette") +
  theme_minimal()

p_gap <- fviz_gap_stat(gap_stat) + 
  labs(title = "Estadistico Gap") + 
  theme_minimal()

#save
ggsave("01_metodo_elbow.png", p_elbow, width = 7, height = 5, dpi = 100)
ggsave("02_metodo_silhouette.png", p_silhouette, width = 7, height = 5, dpi = 100)
ggsave("03_metodo_gap.png", p_gap, width = 7, height = 5, dpi = 100)

#k optimo
k_silhouette <- which.max(silhouette_avg) + 1
k_gap <- maxSE(gap_stat$Tab[, "gap"], gap_stat$Tab[, "SE.sim"])


cat("  RESULTADOS:\n")
cat("    • Silhouette sugiere k =", k_silhouette, "\n")
cat("    • Gap sugiere k =", k_gap, "\n")


k_optimo <- k_silhouette
cat("\n  ★ K OPTIMO SELECCIONADO:", k_optimo, "\n\n")

#k means
cat("► Ejecutando K-Means con k =", k_optimo, "...\n")

set.seed(123)
kmeans_final <- kmeans(datos_escalados, centers = k_optimo, nstart = 25, iter.max = 100)

datos$Cluster <- as.factor(kmeans_final$cluster)

cat("  Clustering completado\n\n")

#distribucion
cat("  DISTRIBUCION DE CLUSTERS:\n")
cat("  .\n")
for (i in 1:k_optimo) {
  n <- sum(datos$Cluster == i)
  pct <- round(n / nrow(datos) * 100, 1)
  bar <- paste(rep("█", round(pct/2)), collapse = "")
  cat(sprintf("  Cluster %d: %6s (%5.1f%%) %s\n", i, format(n, big.mark = ","), pct, bar))
}
cat("\n")

#visualizaciones
cat("► Generando visualizaciones...\n")


set.seed(123)
idx_muestra <- sample(nrow(datos), min(5000, nrow(datos)))

#grafico1
cat("  - Grafico de clusters (PCA)...\n")
p_clusters <- fviz_cluster(
  list(data = datos_escalados[idx_muestra, ], cluster = kmeans_final$cluster[idx_muestra]),
  palette = "Set2", geom = "point", pointsize = 1, alpha = 0.6,
  ggtheme = theme_minimal()
) + labs(title = paste("Visualizacion de", k_optimo, "Clusters"))
ggsave("04_clusters_pca.png", p_clusters, width = 8, height = 6, dpi = 100)

#grafico2
cat("  - Boxplots por cluster...\n")
datos_box <- datos[idx_muestra, ] %>%
  select(Cluster, Duration_hrs, Length_km, `Speed_km.h`, Peak_Time) %>%
  pivot_longer(-Cluster, names_to = "Variable", values_to = "Valor")

p_boxplot <- ggplot(datos_box, aes(x = Cluster, y = Valor, fill = Cluster)) +
  geom_boxplot(alpha = 0.7, outlier.size = 0.5) +
  facet_wrap(~Variable, scales = "free_y") +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Distribucion de Variables por Cluster") +
  theme_minimal() + theme(legend.position = "none")
ggsave("05_boxplots.png", p_boxplot, width = 9, height = 7, dpi = 100)

#grafico3
cat("  - Scatter plot...\n")
p_scatter <- ggplot(datos[idx_muestra, ], aes(x = Duration_hrs, y = `Speed_km.h`, color = Cluster)) +
  geom_point(alpha = 0.5, size = 1) +
  scale_color_brewer(palette = "Set1") +
  labs(title = "Velocidad vs Duracion", x = "Duracion (hrs)", y = "Velocidad (km/h)") +
  theme_minimal()
ggsave("06_scatter.png", p_scatter, width = 8, height = 5, dpi = 100)

#grafico4
cat("  - Mapa geografico...\n")
p_mapa <- ggplot(datos[idx_muestra, ], aes(x = Longitud, y = Latitud, color = Cluster)) +
  geom_point(alpha = 0.4, size = 0.8) +
  scale_color_brewer(palette = "Set1") +
  labs(title = "Distribucion Geografica de Clusters") +
  theme_minimal()
ggsave("07_mapa_clusters.png", p_mapa, width = 8, height = 7, dpi = 100)

cat("  ✓ Visualizaciones guardadas\n\n")

#resumen
cat("► CARACTERISTICAS DE CADA CLUSTER:\n")
cat("..\n\n")

centroides <- datos %>%
  group_by(Cluster) %>%
  summarise(
    N = n(),
    Duracion = round(mean(Duration_hrs), 2),
    Longitud_km = round(mean(Length_km), 2),
    Velocidad = round(mean(`Speed_km.h`), 1),
    Hora_Pico = round(mean(Peak_Time), 1),
    .groups = "drop"
  )

for (i in 1:k_optimo) {
  c <- centroides[i, ]
  cat(sprintf("  CLUSTER %d (%s registros)\n", i, format(c$N, big.mark = ",")))
  cat(sprintf("    • Duracion promedio:  %.2f hrs\n", c$Duracion))
  cat(sprintf("    • Velocidad promedio: %.1f km/h\n", c$Velocidad))
  cat(sprintf("    • Longitud promedio:  %.2f km\n", c$Longitud_km))
  cat(sprintf("    • Hora pico promedio: %.1f hrs\n", c$Hora_Pico))
  
  #top comunas
  comunas <- datos %>% filter(Cluster == i) %>% 
    count(Commune, sort = TRUE) %>% head(3)
  cat("    • Comunas principales:", paste(comunas$Commune, collapse = ", "), "\n\n")
}

#save
cat("► Guardando archivos...\n")
write.csv(datos, "congestion_con_clusters.csv", row.names = FALSE)
write.csv(centroides, "centroides_clusters.csv", row.names = FALSE)

cat("  ✓ congestion_con_clusters.csv\n")
cat("  ✓ centroides_clusters.csv\n\n")

# resumen
cat("ANALISIS COMPLETADO\n")
cat("Graficos generados:\n")
cat(" - 01_metodo_elbow.png\n")
cat(" - 02_metodo_silhouette.png\n")
cat(" - 03_metodo_gap.png\n")
cat(" - 04_clusters_pca.png\n")
cat(" - 05_boxplots.png\n")
cat(" - 06_scatter.png\n")
cat(" - 07_mapa_clusters.png\n\n")

cat("Archivos guardados: congestion_con_clusters.csv, centroides_clusters.csv\n")
cat("K seleccionado:", k_optimo, "| Registros procesados:", format(nrow(datos), big.mark = ","), "\n\n")

cat("Distribucion por cluster:\n")
for (lvl in levels(as.factor(datos$Cluster))) {
    cat(" - Cluster", lvl, ":", sum(datos$Cluster == lvl), "\n")
}
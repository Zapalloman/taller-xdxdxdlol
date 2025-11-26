# Análisis K-Means - Congestión de Tráfico

## ¿Qué hace este script?

Agrupa los datos de congestión en **clusters** (grupos similares) usando el algoritmo K-Means.

---

## Cómo ejecutar

```r
# En terminal:
Rscript analisis_kmeans.R
```

---

## Pasos del análisis

| Paso | Qué hace |
|------|----------|
| 1. Cargar datos | Lee el archivo `congestion.csv` |
| 2. Escalar | Normaliza las variables para que tengan la misma escala |
| 3. Elegir K | Usa 3 métodos (Elbow, Silhouette, Gap) para encontrar el número óptimo de grupos |
| 4. K-Means | Agrupa los datos en K clusters |
| 5. Gráficos | Genera visualizaciones de los resultados |

---

## Archivos que genera

| Archivo | Descripción |
|---------|-------------|
| `01_metodo_elbow.png` | Gráfico para elegir K (buscar el "codo") |
| `02_metodo_silhouette.png` | Gráfico para elegir K (mayor = mejor) |
| `03_metodo_gap.png` | Gráfico estadístico para elegir K |
| `04_clusters_pca.png` | Visualización de los clusters en 2D |
| `05_boxplots.png` | Distribución de variables por cluster |
| `06_scatter.png` | Velocidad vs Duración por cluster |
| `07_mapa_clusters.png` | Ubicación geográfica de cada cluster |
| `congestion_con_clusters.csv` | Datos originales + columna Cluster |
| `centroides_clusters.csv` | Características promedio de cada cluster |

---

## Resultados obtenidos

**Se encontraron 2 clusters:**

| Cluster | % datos | Duración | Velocidad | Interpretación |
|---------|---------|----------|-----------|----------------|
| 1 | 87% | 0.59 hrs | 17.5 km/h | Congestiones **cortas** y frecuentes |
| 2 | 13% | 2.62 hrs | 19.3 km/h | Congestiones **largas** y severas |

---

## Requisitos

```r
install.packages(c("tidyverse", "cluster", "factoextra", "gridExtra"))
```

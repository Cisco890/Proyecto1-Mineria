"""Regenera Proyecto_Completo_Unificado.Rmd desde proyecto1.Rmd y Entrega2."""
from pathlib import Path

root = Path(__file__).resolve().parent
proyecto1 = (root / "proyecto1.Rmd").read_text(encoding="utf-8")
entrega2 = (root / "Proyecto_Entrega2_Presentacion_Resultados.Rmd").read_text(encoding="utf-8")
lines_p1 = proyecto1.splitlines()


def slice_between(start_marker, end_marker):
    out = []
    recording = False
    for line in lines_p1:
        if line.strip() == start_marker:
            recording = True
            out.append(line)
            continue
        if recording and end_marker and line.strip() == end_marker:
            break
        if recording:
            out.append(line)
    return "\n".join(out)


intro = slice_between("## Introducción", "## Descripción general de los datos")
desc_datos = slice_between("## Descripción general de los datos", "## Exploración de variables numéricas")
expl_num = slice_between("## Exploración de variables numéricas", "## Metodología y explicación de procedimientos")
metodologia = slice_between("## Metodología y explicación de procedimientos", "## Exploración de variables categóricas")
cat_expl = slice_between("## Exploración de variables categóricas", "## Relaciones entre las variables.")
relaciones = slice_between("## Relaciones entre las variables.", "# Análisis Exploratorio")
hipotesis = slice_between("## Preguntas de investigación basadas en hipótesis", "# Hallazgos del Análisis Exploratorio")
hallazgos = slice_between("# Hallazgos del Análisis Exploratorio", "# Plan de Siguientes Pasos")
plan = slice_between("# Plan de Siguientes Pasos", None)

header = r'''---
title: "Proyecto Completo — Violencia Intrafamiliar y Divorcios en Guatemala"
author:
  - Luis Pedro Lira (23669)
  - Fernando Rocha (23501)
  - Juan Francisco Martínez (23617)
  - Luis Gilberto González (23353)
  - Joel Antonio Jaquez (23369)
date: "`r format(Sys.Date(), '%Y-%m-%d')`"
output:
  html_document:
    toc: true
    toc_depth: 3
    number_sections: true
    df_print: paged
editor_options:
  markdown:
    wrap: sentence
---

```{r setup-global, include=FALSE}
# =============================================================================
# CONFIGURACIÓN GLOBAL DEL DOCUMENTO UNIFICADO
# Este archivo consolida exploración, preprocesamiento, modelado y comparación.
# Los demás .Rmd del repositorio se conservan como respaldo histórico.
# =============================================================================
knitr::opts_chunk$set(
  echo    = TRUE,
  message = FALSE,
  warning = FALSE,
  fig.width = 7,
  fig.height = 4.5
)
set.seed(123)

library(tidyverse)
library(readr)
library(janitor)
library(MASS)
library(broom)
library(knitr)
library(kableExtra)
library(ggplot2)
library(cluster)
library(ranger)
library(xgboost)

select <- dplyr::select
filter <- dplyr::filter
mutate <- dplyr::mutate
```

## Mapa del documento

| Parte | Contenido |
|---|---|
| **I** | Carga, limpieza, exploración, hipótesis y clustering departamental |
| **II** | Construcción de `datos_modelo` (22 departamentos, 2022) |
| **III** | LOOCV, modelos (NB, Poisson, RF, XGBoost) y comparación final |
| **IV** | Conclusiones, limitaciones y referencia a archivos del repositorio |

> Los demás `.Rmd` del proyecto se conservan como respaldo histórico; este archivo
> es el flujo recomendado de principio a fin.

# Parte I — Contexto, datos y exploración

## Nota sobre la estructura del repositorio

Este documento unifica el flujo completo del proyecto. Los scripts auxiliares
(`divorcios_combinacion.Rmd`, `violencia_combinacion.Rmd`, `crearcsv.Rmd`) siguen
disponibles para regenerar los CSV consolidados desde archivos `.sav` del INE.
Aquí se trabaja directamente con:

- `Datos/divorcios_total.csv`
- `Datos/violencia_total.csv`

```{r 01-carga-datos, include=FALSE}
# =============================================================================
# PASO 1: CARGA DE DATOS
# Los archivos consolidados contienen registros 2012-2022.
# Para el modelado departamental usamos únicamente el corte transversal 2022.
# =============================================================================
divorcios_raw <- read_csv("Datos/divorcios_total.csv", show_col_types = FALSE)
violencia_raw <- read_csv("Datos/violencia_total.csv", show_col_types = FALSE)

divorcios <- divorcios_raw %>% filter(year == 2022)
violencia <- violencia_raw %>% filter(year == 2022)
```

```{r 02-limpieza-microdatos, include=FALSE}
# =============================================================================
# PASO 2: LIMPIEZA DE MICRODATOS PARA EXPLORACIÓN
# Se recodifican códigos especiales del INE (99, 999) y se crean etiquetas
# interpretables para tablas y gráficos exploratorios.
# =============================================================================
violencia <- violencia %>%
  mutate(
    cod_depto = floor(hec_deptomcpio / 100),
    sexo_label = case_when(
      vic_sexo == 1 ~ "Hombre",
      vic_sexo == 2 ~ "Mujer",
      TRUE ~ "Ignorado"
    ),
    mes_nombre = factor(month.name[hec_mes], levels = month.name),
    vic_edad_clean = ifelse(vic_edad > 110, NA, vic_edad),
    total_hijos_clean = ifelse(total_hijos == 99, NA, total_hijos),
    est_civ_label = case_when(
      vic_est_civ == 1 ~ "Soltero(a)",
      vic_est_civ == 2 ~ "Casado(a)",
      vic_est_civ == 3 ~ "Unido(a)",
      TRUE ~ "Ignorado"
    ),
    relacion_label = case_when(
      vic_rel_agr == 1 ~ "Esposo/Conviviente",
      vic_rel_agr == 2 ~ "Ex-Esposo/Ex-Pareja",
      vic_rel_agr == 3 ~ "Padre/Madre",
      vic_rel_agr == 4 ~ "Hijo(a)",
      vic_rel_agr == 5 ~ "Hermano(a)",
      TRUE ~ "Otro/No especificado"
    )
  )

divorcios <- divorcios %>%
  mutate(
    cod_depto = DEPOCU,
    edad_hom_clean = ifelse(EDADHOM > 110, NA, EDADHOM),
    edad_muj_clean = ifelse(EDADMUJ > 110, NA, EDADMUJ),
    diferencia_edad = edad_hom_clean - edad_muj_clean,
    EDADHOM = ifelse(EDADHOM == 999, NA, EDADHOM),
    EDADMUJ = ifelse(EDADMUJ == 999, NA, EDADMUJ),
    nivel_edu_hom = case_when(
      ESCHOM == 1 ~ "Ninguno",
      ESCHOM %in% c(2, 3, 4) ~ "Primaria",
      ESCHOM %in% c(5, 6) ~ "Media/Diversificado",
      ESCHOM >= 7 & ESCHOM < 9 ~ "Superior",
      TRUE ~ "Ignorado"
    ),
    nivel_edu_muj = case_when(
      ESCMUJ == 1 ~ "Ninguno",
      ESCMUJ %in% c(2, 3, 4) ~ "Primaria",
      ESCMUJ %in% c(5, 6) ~ "Media/Diversificado",
      ESCMUJ >= 7 & ESCMUJ < 9 ~ "Superior",
      TRUE ~ "Ignorado"
    )
  )
```

```{r 03-resumen-carga}
cat("Registros violencia (2022):", nrow(violencia), "\n")
cat("Registros divorcios (2022):", nrow(divorcios), "\n")
```
'''

part2 = r'''

------------------------------------------------------------------------

# Parte II — Preparación del dataset departamental para modelado

A partir de los microdatos limpios de 2022 se construye `datos_modelo`, la
tabla analítica con **22 filas** (una por departamento). Esta misma tabla alimenta
todos los algoritmos de la Parte III.

## Variable respuesta

La variable objetivo es **`casos_divorcio`**: número total de divorcios registrados
por departamento en 2022. Es una variable **cuantitativa discreta de conteo**,
por lo que el problema se aborda como **regresión**, no clasificación.

```{r 04-agregacion-departamental, include=FALSE}
# =============================================================================
# PASO 3: AGREGACIÓN DEPARTAMENTAL
# =============================================================================
violencia_proc <- violencia %>%
  filter(!is.na(cod_depto), cod_depto != 99) %>%
  group_by(cod_depto) %>%
  summarise(
    casos_violencia   = n(),
    edad_victima_prom = mean(vic_edad_clean, na.rm = TRUE),
    porc_urbano       = mean(hec_area == 1, na.rm = TRUE),
    .groups = "drop"
  )

divorcios_proc <- divorcios %>%
  filter(!is.na(cod_depto), cod_depto != 99) %>%
  group_by(cod_depto) %>%
  summarise(casos_divorcio = n(), .groups = "drop")
```

```{r 05-construir-datos-modelo, include=FALSE}
# =============================================================================
# PASO 4: DATASET FINAL DE MODELADO
# - es_capital controla el outlier estructural de Guatemala
# - variables _z estandarizan predictores continuos
# =============================================================================
datos_modelo <- violencia_proc %>%
  inner_join(divorcios_proc, by = "cod_depto") %>%
  mutate(
    es_capital          = as.integer(cod_depto == 1),
    casos_violencia_z   = as.numeric(scale(casos_violencia)),
    edad_victima_prom_z = as.numeric(scale(edad_victima_prom)),
    porc_urbano_z       = as.numeric(scale(porc_urbano))
  )

stopifnot(nrow(datos_modelo) == 22)
```

```{r 06-funcion-metricas, include=FALSE}
# =============================================================================
# FUNCIÓN COMÚN DE MÉTRICAS PARA TODOS LOS ALGORITMOS
# =============================================================================
calcular_metricas <- function(observado, predicho) {
  tibble(
    MAE  = mean(abs(observado - predicho), na.rm = TRUE),
    RMSE = sqrt(mean((observado - predicho)^2, na.rm = TRUE)),
    MAPE = mean(
      abs((observado - predicho) / ifelse(observado == 0, NA, observado)),
      na.rm = TRUE
    ) * 100
  )
}
```

```{r 07-vista-datos-modelo}
datos_modelo %>%
  select(cod_depto, casos_divorcio, casos_violencia,
         edad_victima_prom, porc_urbano, es_capital) %>%
  kable(digits = 2,
        caption = "Dataset departamental final (2022)")
```

------------------------------------------------------------------------

# Parte III — Validación, modelado y comparación final
'''

idx = entrega2.find("# Actividad 5:")
modeling_body = entrega2[idx:]
modeling_body = modeling_body.replace("# Actividad 5:", "## 5.1")
modeling_body = modeling_body.replace("# Actividad 6:", "## 5.2")
modeling_body = modeling_body.replace("# Actividad 7:", "## 5.3")
modeling_body = modeling_body.replace("# Actividad 8:", "## 5.4")
modeling_body = modeling_body.replace("## 7.1 Regresión Binomial Negativa", "### Regresión Binomial Negativa")
modeling_body = modeling_body.replace("## Modelo de referencia: Regresión Poisson", "### Modelo de referencia: Regresión Poisson")
modeling_body = modeling_body.replace("## Modelo de apoyo: Random Forest", "### Modelo de apoyo: Random Forest")
modeling_body = modeling_body.replace("## Benchmark predictivo: XGBoost", "### Benchmark predictivo: XGBoost")

SECTION_52_REPLACEMENT = r'''## 5.2 Preprocesamiento y preparación de datos

> **Nota:** `datos_modelo` ya fue construido en la **Parte II** (chunks
> `04-agregacion-departamental` y `05-construir-datos-modelo`). Aquí se
> documentan las decisiones metodológicas y se verifica que el objeto esté
> listo antes de entrenar los algoritmos.

### Decisiones aplicadas

1. **Filtro temporal:** solo registros de 2022 para un corte transversal limpio.
2. **Limpieza INE:** códigos 99/999 recodificados a `NA`; se excluye `cod_depto == 99`.
3. **Agregación departamental:** conteos y promedios desde microdatos individuales.
4. **Covariables seleccionadas:** `casos_violencia`, `edad_victima_prom`, `porc_urbano`.
   La variable `hijos_prom` se descartó por correlación baja con la respuesta (r ≈ 0.095).
5. **Transformaciones finales:** z-score en predictores continuos y dummy `es_capital`
   para controlar el outlier estructural de Guatemala.

```{r 5.2-verificar-dataset}
cat("Microdatos divorcios (2022):", nrow(divorcios), "\n")
cat("Microdatos violencia (2022):", nrow(violencia), "\n")
cat("Departamentos en datos_modelo:", nrow(datos_modelo), "\n")
stopifnot(nrow(datos_modelo) == 22)
stopifnot(exists("calcular_metricas"))
cat("Función calcular_metricas() disponible para todos los algoritmos.\n")
```

```{r 5.2-vista-datos-modelo}
datos_modelo %>%
  select(cod_depto, casos_divorcio, casos_violencia,
         edad_victima_prom, porc_urbano, es_capital) %>%
  kable(
    digits = 2,
    caption = "Dataset de modelado: variables originales por departamento (2022)"
  )
```

```{r 5.2-resumen-datos-modelo}
datos_modelo %>%
  select(casos_divorcio, casos_violencia, edad_victima_prom, porc_urbano) %>%
  summary()
```

El dataset final tiene **22 observaciones** (una por departamento), columnas
`_z` estandarizadas y sin valores faltantes en covariables. Las métricas
**MAE**, **RMSE** y **MAPE** se calculan con `calcular_metricas()` definida
en la Parte II; Poisson y Binomial Negativa añaden **AIC** y **BIC** para
comparar variantes.

------------------------------------------------------------------------'''

start_52 = modeling_body.find("## 5.2 Preprocesamiento")
end_53 = modeling_body.find("## 5.3 Creación")
if start_52 >= 0 and end_53 > start_52:
    modeling_body = modeling_body[:start_52] + SECTION_52_REPLACEMENT + "\n\n" + modeling_body[end_53:]

HIP2_OLD = """```{r}
mean(divorcios$EDADHOM, na.rm = TRUE) - 
mean(divorcios$EDADMUJ, na.rm = TRUE)
```

```{r}
diferencia <- divorcios$EDADHOM - divorcios$EDADMUJ
summary(diferencia)
```

```{r}
divorcios <- divorcios %>%
  mutate(
    EDADHOM = ifelse(EDADHOM == 999, NA, EDADHOM),
    EDADMUJ = ifelse(EDADMUJ == 999, NA, EDADMUJ)
  )

```

```{r}
diferencia <- divorcios$EDADHOM - divorcios$EDADMUJ
summary(diferencia[!is.na(diferencia)])
```

```{r}
library(tidyr)

edades_long <- divorcios %>%
  select(EDADHOM, EDADMUJ) %>%
  pivot_longer(cols = everything(),
               names_to = "Sexo",
               values_to = "Edad") %>%
  filter(!is.na(Edad))
```

```{r}
library(ggplot2)

ggplot(edades_long, aes(x = Sexo, y = Edad)) +"""

HIP2_NEW = """```{r hipotesis-2-diferencia-edad}
# La limpieza de edades (999 → NA) ya se aplicó en 02-limpieza-microdatos
summary(divorcios$diferencia_edad)
mean(divorcios$edad_hom_clean, na.rm = TRUE) -
  mean(divorcios$edad_muj_clean, na.rm = TRUE)
```

```{r hipotesis-2-boxplot-edades}
edades_long <- divorcios %>%
  select(edad_hom_clean, edad_muj_clean) %>%
  pivot_longer(
    cols = everything(),
    names_to = "Sexo",
    values_to = "Edad",
    names_transform = list(Sexo = ~ recode(.x,
      edad_hom_clean = "Hombre (EDADHOM)",
      edad_muj_clean = "Mujer (EDADMUJ)"
    ))
  ) %>%
  filter(!is.na(Edad))

ggplot(edades_long, aes(x = Sexo, y = Edad)) +"""

HIP3_OLD_START = "```{r}\nviolencia_dep <- violencia %>%\n  group_by(depto_mcpio)"
HIP3_NEW = """```{r hipotesis-3-agregacion-departamental}
# Agregación departamental con la misma regla que en Parte II (cod_depto válido)
violencia_dep <- violencia %>%
  filter(!is.na(cod_depto), cod_depto != 99) %>%
  group_by(cod_depto) %>%
  summarise(casos_violencia = n(), .groups = "drop")

divorcios_dep <- divorcios %>%
  filter(!is.na(cod_depto), cod_depto != 99) %>%
  group_by(cod_depto) %>%
  summarise(casos_divorcio = n(), .groups = "drop")

datos_dep <- inner_join(violencia_dep, divorcios_dep, by = "cod_depto")
stopifnot(nrow(datos_dep) == 22)

datos_dep %>% arrange(desc(casos_violencia)) %>% print(n = 22)
```

```{r hipotesis-3-correlacion}"""

conclusion = r'''

------------------------------------------------------------------------

# Parte IV — Conclusiones generales del proyecto

## Síntesis metodológica

El proyecto integra cuatro etapas complementarias:

1. **Construcción y limpieza de datos** a partir de fuentes oficiales del INE.
2. **Exploración descriptiva e hipótesis** sobre patrones sociodemográficos.
3. **Modelado predictivo departamental** con validación LOOCV.
4. **Comparación entre enfoques estadísticos y de aprendizaje automático**.

## Síntesis de resultados

- La violencia intrafamiliar concentra sus víctimas principalmente en mujeres adultas jóvenes.
- Los divorcios muestran homogamia etaria moderada, con tendencia a que el hombre sea ligeramente mayor.
- A nivel departamental existe una asociación positiva entre violencia y divorcios, pero fuertemente condicionada por la capital.
- El modelo principal seleccionado es **Regresión Binomial Negativa (NB_1)** por parsimonia, interpretabilidad y adecuación a conteos con sobredispersión.
- Random Forest y XGBoost aportan contraste predictivo, pero no reemplazan al modelo principal dado n = 22.

## Limitaciones

- Muestra pequeña (22 departamentos).
- Análisis agregado, no causal a nivel individual.
- Ausencia de normalización por población departamental.
- Riesgo de sobreajuste en modelos flexibles.

## Archivos relacionados en el repositorio

| Archivo | Función |
|---|---|
| `proyecto1.Rmd` | Exploración original |
| `Pre-avance3.Rmd` | Avance intermedio de modelado |
| `Avances3_Modelos_Final.Rmd` | Modelado y discusión previa |
| `Proyecto_Entrega2_Presentacion_Resultados.Rmd` | Entrega de resultados |
| `exploratoria_correlacion.Rmd` | Matrices de correlación |
| `divorcios_combinacion.Rmd` | Pipeline SPSS divorcios |
| `violencia_combinacion.Rmd` | Pipeline SPSS violencia |
| `crearcsv.Rmd` | Conversión Excel → CSV |

Este archivo **`Proyecto_Completo_Unificado.Rmd`** es el punto de entrada recomendado
para reproducir el análisis completo de principio a fin.
'''

hipotesis = hipotesis.replace(HIP2_OLD, HIP2_NEW)
if HIP3_OLD_START in hipotesis:
    i0 = hipotesis.find(HIP3_OLD_START)
    i1 = hipotesis.find("#### Resultado e interpretación", i0)
    hip3_tail = hipotesis[i1:]
    hip3_intro = hipotesis[:i0]
    hipotesis = hip3_intro + HIP3_NEW + """
cor(datos_dep$casos_violencia, datos_dep$casos_divorcio)
cor.test(datos_dep$casos_violencia, datos_dep$casos_divorcio)
```
""" + hip3_tail

parts = [header, intro, desc_datos, expl_num, metodologia, cat_expl, relaciones,
         hipotesis, hallazgos, plan, part2, modeling_body, conclusion]

out_path = root / "Proyecto_Completo_Unificado.Rmd"
out_path.write_text("\n\n".join(p for p in parts if p.strip()), encoding="utf-8")
print(f"Written {out_path} ({len(out_path.read_text(encoding='utf-8').splitlines())} lines)")

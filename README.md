# SoundSpectrumInfluencesADP_data

## 📋 Description

Experimental data repository and analysis scripts for the study "Sound Spectrum Influences Auditory Distance Perception in Highly Reverberant Environments".

**Authors**: Damian Payo<sup>1,2,3</sup>, Lucas Gonzalez Delbon<sup>1,3</sup>, Ramiro O. Vergara<sup>1,3</sup>, Manuel C. Eguia<sup>1,3</sup>

<sup>1</sup> CONICET | <sup>2</sup> UCASAL | <sup>3</sup> Lab. Acústica y Percepción Sonora, UNQ

## 📁 Repository Contents

### `Piloto_Mayo_Salta/`
Data from the pilot study conducted in Salta (May 2023) to determine the optimal filter cutoff frequency.

- **`Data/`**: Raw pilot data
- **`Limpieza_de_Datos.R`** and **`Limpieza_de_Datos_2.0.R`**: Preprocessing scripts
- **`Sample_Size_Calc.Rmd`**: Sample size calculation
- **`graph/`**: Generated plots

Two cutoff frequencies (1 kHz and 4 kHz) were evaluated. Goodness-of-fit and effect size analyses determined **1 kHz** (η² = 0.74) to be used in the main experiment.

### `Junio_Bernal/`
Main experiment data with **1 kHz cutoff filter** (June-July 2023, UNQ Bernal).

**Participants**: 23 valid subjects (2 excluded)  
**Design**: 4 blocks × 12 distances × 3 repetitions = 144 trials/subject

#### Experimental conditions:
- **WW** (Wide-Wide): Direct and reverberant unfiltered
- **LL** (Low-Low): Direct and reverberant filtered (< 1 kHz)
- **LW** (Low-Wide): Direct filtered, reverberant unfiltered
- **WL** (Wide-Low): Direct unfiltered, reverberant filtered

#### Files:
- **`Data_1k/`**: 69 CSV files (one per subject-condition-block) with raw responses
- **`Estadistica_Junio.Rmd`**: Main statistical analysis with LMEMs
- **`Analisis_Potencia_Junio.Rmd`**: Statistical power analysis
- **`Resultados_Indicadores_Junio.Rmd`**: Perceptual performance metrics
- **`orden_bloques.csv`**: Randomized presentation order per subject
- **`Indicadores/`**: Computed indicator results
- **Figures** (`.eps`, `.png`): Distance curves, logarithmic bias, boxplots, distributions

### `Septiembre_Bernal/`
Replication and extension experiment (September 2023, UNQ Bernal).

**Participants**: 22 valid subjects  
**Variations**: Inclusion of Salta data for cross-group comparative analysis

#### Files:
- **`Data_1k_DR/`**: Data with refined structure
- **`Estadistica_Septiembre.Rmd`**: Statistical analysis
- **`Analisis_Potencia.Rmd`**: Power analysis
- **`Comparación_Distancias.Rmd`**: Comparison across distances and groups
- **`Revisión_Sujetos.Rmd`**: Individual participant validation
- **`Resultados_Indicadores.Rmd`**: Performance indicators
- **`orden_bloques.csv`**: Condition counterbalancing
- **Figures**: Comparative visual analyses

## 🔬 Experimental Data

### Stimuli
- **Base**: White noise bursts (0.2 s, 50 ms raised cosine ramps, 20 Hz - 20 kHz)
- **Processing**: Convolution with BRIRs (Binaural Room Impulse Responses)
- **Filtering**: Single-pole Butterworth low-pass filter at 1 kHz applied independently to direct and reverberant components
- **Environment**: Empty water tank (RT60 ≈ 8 s)
- **Distances**: 12 logarithmically-spaced positions (1.5 - 26 m)

### CSV File Structure

Each file contains responses from one subject in one block-condition:

| Column | Description |
|---------|-------------|
| `nsub` | Participant numeric ID |
| `distancia` | Physical source distance (meters) |
| `respuesta` | Perceived distance reported by participant (meters) |
| `condicion` | Condition code: 0=WW, 1=LL, 2=LW, 3=WL |
| `bloque` | Block number (corrected with `orden_bloques.csv`) |
| `trial_Order` | Sequential trial order within block |

### Total Participants

**N = 69** (64 valid after exclusions)
- 36 males, 32 females, 1 non-binary
- Age: 18-45 years (M = 27.55, SD = 7.31)
- **Salta** (UCASAL): 28 participants (26 valid)
- **Bernal** (UNQ): 41 participants (38 valid)

**Exclusion criteria** (5 subjects):
- Reported hearing impairment (1)
- Tinnitus (2)
- Incomplete block (1)
- Non-externalized or unrealistic responses (1)

## 📊 Statistical Analyses

R Markdown scripts implement:

### Main models
- **LMEMs** (Linear Mixed-Effects Models) using `lme4`
- **Dependent variable**: `log(perceived_distance)`
- **Fixed effects**: `log(source_distance)`, `condition`, interaction
- **Random effects**: By-participant intercept and slope, by-condition slope

### Analyzed metrics
- **Perceived distance**: Direct responses on logarithmic scale
- **Signed Logarithmic Bias (SLB)**: `log(perceived) - log(source)` (overestimation/underestimation)
- **Slope (a)**: Exponent of compression model `r' = k·r^a`
- **Within-subject variance**: Dispersion by condition × distance

### Procedures
- **Outlier detection**: MAD (Median Absolute Deviation, threshold = 3) using `Routliers`
- **Post-hoc comparisons**: EMMs with Bonferroni correction (`emmeans`)
- **Effect size**: η²p (partial eta squared)
- **Goodness-of-fit**: Marginal and conditional R² (`MuMIn`)
- **Group comparison**: AIC, BIC, likelihood-ratio tests (Salta vs. Bernal)

## 💻 Requirements

### Software
- **R** ≥ 4.0
- **RStudio** (recommended)

### R Packages
```r
install.packages(c(
  "readr", "dplyr", "tidyverse", "data.table", "here",
  "lme4", "nlme", "lmerTest", "emmeans", "car", "MuMIn", 
  "sjstats", "Routliers", "effectsize",
  "ggplot2", "ggpubr", "plotrix", "showtext"
))
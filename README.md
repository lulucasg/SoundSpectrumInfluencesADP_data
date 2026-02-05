# SoundSpectrumInfluencesADP_data

## 📋 Description

Experimental data repository and analysis scripts for the study "Sound Spectrum Influences Auditory Distance Perception in Highly Reverberant Environments".

**Authors**: Damian Payo<sup>1,2,3</sup>, Lucas Gonzalez Delbon<sup>1,3</sup>, Ramiro O. Vergara<sup>1,3</sup>, Manuel C. Eguia<sup>1,3</sup>

<sup>1</sup> CONICET | <sup>2</sup> UCASAL | <sup>3</sup> Lab. Acústica y Percepción Sonora, UNQ

## 📁 Repository Contents

### `Septiembre_Bernal/`
Main experiment data (September 2023, UNQ Bernal).

**Participants**: Combined dataset from Salta and Bernal locations  
**Design**: 4 conditions (WW, LL, LW, WL) × 11-12 distances × 3 repetitions

#### Experimental conditions:
- **WW** (Wide-Wide): Direct and reverberant unfiltered
- **LL** (Low-Low): Direct and reverberant filtered (< 1 kHz)
- **LW** (Low-Wide): Direct filtered, reverberant unfiltered
- **WL** (Wide-Low): Direct unfiltered, reverberant filtered

#### Files:
- **`Data_1k_DR/`**: Raw experimental data (CSV files)
- **`Estadistica_Septiembre_Respuestas.Rmd`**: Statistical analysis of perceived distances
- **`Estadistica_Septiembre_Sesgo.Rmd`**: Logarithmic bias analysis
- **`Estadistica_Septiembre_11d.Rmd`**: Analysis for 11-distance subset
- **`Resultados_Indicadores.Rmd`**: Perceptual performance indicators
- **`orden_bloques.csv`**: Randomized block presentation order
- **`Indicadores/`**: Computed performance metrics
- **`Figuras/`**: Generated figures for publication (`.eps`, `.png`)

### `Graficos_Espectros/`
Spectral analysis and broadband level (BL) / direct-to-reverberant ratio (DRR) visualizations.

#### Files:
- **`Gráficos_Espectros.Rmd`**: R Markdown script for spectral plots
- **`BL_DRR.eps`**: Combined BL and DRR figure
- **`Data/`**: Data files for spectral analysis

## 🔬 Experimental Data

### Stimuli
- **Base**: White noise bursts (0.2 s, 50 ms raised cosine ramps, 20 Hz - 20 kHz)
- **Processing**: Convolution with BRIRs (Binaural Room Impulse Responses)
- **Filtering**: Single-pole Butterworth low-pass filter at 1 kHz applied independently to direct and reverberant components
- **Environment**: Empty water tank (RT60 ≈ 8 s)
- **Distances**: 11-12 logarithmically-spaced positions

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
- **Dependent variable**: `log(perceived_distance)` or `log_bias`
- **Fixed effects**: `log(source_distance)`, `condition`, `group`, interactions
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
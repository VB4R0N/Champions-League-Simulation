# Champions League Matchday Monte Carlo Simulation ⚽🎲

![R Version](https://img.shields.io/badge/R-4.3.1-blue) 
[![Twitter Thread](https://img.shields.io/badge/Results-Twitter%20Thread-1DA1F2)](https://x.com/Ronny_12_BSC/status/1884246647213023629)

A model simulating the final Champions League matchday **50,000 times** to predict league standings and qualification probabilities. Includes an interactive Shiny dashboard for exploring results.

---

## 📌 Key Features
- **Monte Carlo simulations** of match outcomes using betting odds and historical score distributions
- **Interactive Shiny dashboard** for visualizing team-specific probabilities
- **Reproducible workflows** with full code and data transparency (included csv-files)

---

## 🧮 Methodology

### 1. Probability Estimation
- **Match Outcome Probabilities**: Derived from betting odds ([oddschecker.com](https://www.oddschecker.com/)) using implied probability calculations
- **Score Distributions**: Conditional probabilities based on 5+ years of historical match data ([footystats.org](https://footystats.org/))

### 2. Simulation Engine
```r
for (sim in 1:50000) {
  # 1. Reset league table
  # 2. Simulate match outcomes using:
  #    - Odds-derived win/draw probabilities
  #    - Historical score distributions
  # 3. Update league standings
  # 4. Aggregate results
}

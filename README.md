# US Personal Consumption Expenditures – Forecasting Comparison 

Welcome!  
This repository documents the workflow and code for **Part 2** of the assignment, where we evaluate and compare three forecasting approaches on the **seasonally-adjusted US Personal Consumption Expenditures (PCE)** time series.

---

## 📂 Data

* **File:** `PCE.csv`  
* **Series:** Monthly, seasonally adjusted PCE (billions of USD)  
* **Frequency:** Monthly  
* **Period Covered:** See `PCE.csv` header

---

## 🎯 Objectives

1. **Model Comparison**  
   Evaluate and compare the predictive accuracy of **three** model classes:

   | # | Model Class | Candidate Options |
   |---|-------------|-------------------|
   | 1 | **Simple Forecast** | `average`, `naïve`, `seasonal naïve`, **or** `drift` |
   | 2 | **Exponential Smoothing** | `ses`, `holt`, `hw/a`, `hw/m` |
   | 3 | **ARIMA** | Automated or manually specified |

2. **Best Model Selection**  
   Identify the model with the **highest predictive performance** based on chosen error metrics.

3. **Forecast October 2024 PCE**  
   Use the best-performing model to estimate PCE for **2024-10**.

4. **Rolling One-Step-Ahead Test**  
   Re-evaluate model ranking with a **rolling-origin one-step-ahead** forecast **without** re-optimising parameters.

---

## 📝 Report Requirements (`.docx`, ≤ 50 % of total grade)

Your Word document must clearly cover:

1. **Data & Pre-processing**  
   * Handling of missing values  
   * Train-test split strategy  
   * Any transformations (e.g., logs, differencing)

2. **Modelling Decisions**  
   * Parameter choices / auto-selection criteria  
   * Software packages & functions used

3. **Performance Evaluation**  
   * Error metrics (e.g., MAE, RMSE, MAPE)  
   * Tabular comparison & discussion  
   * Single plot overlaying **actual vs. predicted** values for **all three models**

4. **Forecast for 2024-10**  
   * Point forecast and short explanatory paragraph

5. **Rolling Test Results**  
   * Brief description of rolling-window setup  
   * Comparison table/figure and commentary

---

## 💾 Code Submission (equal weight with report)

* **If using R Markdown:**  
  * Submit the `.Rmd` file **and** the knitted `.docx`.

* **If writing the report manually:**  
  * Submit the `.R` (or `.ipynb`) script(s) used to run the analysis.  
  * Make sure the script reproduces every table and figure in the Word report.

> **Pro tip:** Set a seed for reproducibility and comment liberally.

---

## 🛠 Recommended Workflow

1. **Clone** or download this repository  
2. Drop `PCE.csv` into `/data`  
3. Run or adapt `analysis.Rmd` (or your own scripts) in `/analysis`  
4. Export figures to `/figures`  
5. Compile the Word report in `/report`

---

## ✔️ Marking Checklist

| Section | Points |
|---------|--------|
| Pre-processing rationale | 10 |
| Model specification & tuning | 15 |
| Performance comparison & interpretation | 15 |
| Forecast for 2024-10 | 5 |
| Rolling test results | 5 |

---

## 🤝 Contributing

Found a typo or have a suggestion? Feel free to open an issue or pull request.

---

### Licence

© 2025 [Your Name / Institution] – released for educational purposes.

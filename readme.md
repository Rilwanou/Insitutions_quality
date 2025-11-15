# Quality of Institutions and Economic Development

This repository contains the code, report, and interactive Shiny dashboard for a research project from the National Institute of Statistics and Applied Economics (INSEA, Rabat-Morocco) on the link between institutional quality and economic development.

This project was supervised by Mr. TAAMOUTI.

## Project Objective

The main objective of this project is to empirically analyze the impact of institutions on economic development, based on the seminal paper by Acemoglu, Johnson, and Robinson (2001), "The Colonial Origins of Comparative Development".

The project is divided into two main parts:
1.  A summary and synthesis of the AJR (2001) paper.
2.  An empirical replication and extension of their findings, presented in an interactive dashboard.

## Launching the Interactive Dashboard

This repository uses `renv` to manage dependencies. All required packages (Shiny, AER, ggplot2, etc.) are listed in the `renv.lock` file.

### Option 1: Using RStudio (Recommended)

1.  Clone this repository.
2.  Open the `Institutions_quality.Rproj` file in RStudio.
3.  In the R **Console**, run `renv::restore()`. This will install all the correct package versions.
4.  Open the file `src/model/app.R`.
5.  Click the **"Run App"** button at the top of the RStudio editor.

### Option 2: Using the Terminal

1.  Clone this repository.
2.  Open your Terminal (cmd, PowerShell, or bash).
3.  Navigate to the project's **root directory**:
    ```bash
    cd path/to/Institutions_quality
    ```
4.  Run the following command to launch the app (this assumes `R` is in your system's PATH):
    ```bash
    R -e "shiny::runApp('src/model', launch.browser = TRUE)"
    ```

## Repository Structure

* `/data`: Contains the processed data (`data.xlsx`) used for the analysis.
* `/src/model`: Contains the core logic:
    * `global.R`: The script that loads data and pre-calculates all 50+ regression models and graphics.
    * `app.R`: The Shiny application script (UI and Server) that displays the results.
* `/rapport`: Contains the final PDF report (`Rapport.pdf`) detailing the methodology.
* `renv.lock`: The `renv` lockfile listing all package dependencies.
* `README.md`: This file.

## Key Variables (Snapshot)

This table describes the main variables created in `global.R` and used in the analysis.

| Variable Name | Description (Original Name) |
| :--- | :--- |
| `log_gdp_ppp_1995` | Log GDP per capita (PPP) in 1995 |
| `avg_prot_risk` | Average protection against expropriation risk 1985-1995 |
| `log_mortality` | Log of European settler mortality |
| `Latitude` | Absolute latitude divided by 90 |
| `log_output_worker_1995`| Log output per worker in 1995 |
| `log_gdp_ppp_2019` | Log GDP per capita (PPP) in 2019 (Project Extension) |
| `log_output_worker_2019`| Log output per worker in 2019 (Project Extension) |
| `asia_dummy` | Dummy variable for Asian countries |
| `africa_dummy` | Dummy variable for African countries |
| `neo_europes_dummy` | Dummy variable for "Neo-Europes" (USA, CAN, AUS, NZ) |
| `other_regions_dummy`| Dummy variable for other regions (e.g., Malta) |

## Empirical Analysis Summary

Our empirical work focuses on replicating **Table 4** from the original AJR (2001) paper.

### Data
* The primary dataset is from La Porta et al. (1999), "The Quality of Government," which was also used by AJR.
* **Modification:** The original paper excluded three countries (Hong Kong, Tanzania, Venezuela) from some regressions due to missing "log output per worker" data in the Penn World Table (PWT).
* We supplement this by sourcing the "log output per worker" variable from the **World Bank database**. This allows us to include all 64 countries in our analysis.

### Replications
1.  **Replication (1995 Data):** We first replicate the original IV regressions using 1995 data, as in the paper.
2.  **Extension (2019 Data):** We then update the analysis by running the same regressions using more recent GDP data from 2019.

### Findings
Our results confirm the original conclusion: the quality of institutions (proxied by `avg_prot_risk`) has a robust and significant positive impact on long-term economic performance. This relationship holds even when using updated data from 2019.

## Authors
* MAHAMAN LAWAL Rilwanou
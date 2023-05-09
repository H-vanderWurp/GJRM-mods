# GJRM-mods

This repository can reproduce results from my dissertation entitled "Two-dimensional Regression Modelling with Copula Dependencies and a Focus on Count Data and Sports Applications".
Each chapter or subchapter is represented by a single folder as follows:
- Simulation 1: Simulation study from Chapter 2.2 and publication van der Wurp et al. (2020)
  - "Main File.R" contains simulation study and analysis.
  - Some results are given to save computational time. However, the computational time is not that relevant here.
- Bundesliga: Application of Chapter 2.3, unpublished before.
  - "Main File.R" contains all preparation and calculation for the Bundesliga predictions. Results stored in "results.rData" to save computational time. Raw data given in /Spieltagsdaten/.
- Simulation 2: Simulation study from Chapter 3.2 and publication van der Wurp & Groll (2022)
  - "Simulations.R" contains all needed material to start the simulation study. However, the study was performed on the cluster of the department. Parallelisation is strongly recommended.
  - "SimResults.R" contains everything to analyse the given sim results, which are stored in /Results/ (.zip format, unpack first).
- FIFA: Application of Chapter 4. Published in van der Wurp et al. (2020) and in van der Wurp & Groll (2022).
  - The files "Chapter 4.3.1 no penalty.R", "Chapter 4.3.2 equl penalty.R", "Chapter 4.3.3 lasso penalty.R" and "Chapter 4.3.5 both penalties.R" lead through the chapter.
  - Results are stored for 4.3.3 and 4.3.5 to save computational time in /res_pen2/ (4.3.3) and /res_penboth/ (4.3.5)
- CaseStudy: Application of Chapter 5. Published in van der Wurp & Groll (2023).
  - "Master Fitter.R" contains all fitting processes separated by leagues. "Master Fitter (globally).R" contains all fitting processes without differenciating by league.
  - All results are stored in /Results/ (corresponding to "Master Fitter.R") and /Results (globally)/ (corresponding to "Master Fitter (globally).R"). Both in .zip format, unpack first.
  - /Fitting Functions/ and /Fitting Functions (globally)/ contain all necessary functions that both master fitter files need. In some cases (especially copulaLASSO and copula-fat, in which "fat" stands for both penalties at the same time (see 4.3.5)) modified versions were used for parallelisation on the department's cluster. Those versions are not published, as other parallelisation approaches will work differently.
  - "Examples_and_tables.R" creates tables for Chapter 5. 

The main file with all GJRM-changes are included in "GJRM changes - 2023.R". Please note that the underlying GJRM-package is used in version 0.2-3, which needs to be installed from the archive: https://cran.r-project.org/src/contrib/Archive/GJRM/

If any errors occur or some files are missing (which might happen, especially with the vast amount of result files), feel free to contact me via vanderwurp@statistik.tu-dortmund.de.

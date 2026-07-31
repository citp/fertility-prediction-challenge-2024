## SPECS & VERSION NUMBERS

| Component            | Specification                 |
| -------------------- | ----------------------------- |
| **Computer**         | 2020 MacBook Pro              |
| **Processor**        | 2 GHz Quad-Core Intel Core i5 |
| **Memory**           | 16 GB 3733 MHz LPDDR4X        |
| **Operating System** | macOS Sequoia 15.6.1          |
| **Disk Space**       | 512 GB SSD                    |
| **R**                | 4.3.3                         |


| Package         | Version |
| --------------- | ------- |
| **groundhog**   | 3.2.3   |
| **here**        | 1.0.1   |
| **rmarkdown**   | 2.26    |
| **knitr**       | 1.46    |
| **tidyverse**   | 2.0.0   |
| **haven**       | 2.5.4   |
| **data.table**  | 1.15.4  |
| **tidymodels**  | 1.2.0   |
| **xgboost**     | 1.7.7.1 |
| **kableExtra**  | 1.4.0   |
| **ggthemes**    | 5.1.0   |
| **ggflowchart** | 1.0.0   |
| **ggridges**    | 0.5.6   |

Note: Groundhog will automatically install the correct versions of all other R packages required by our code.

## TABLE OF CONTENTS

### Code

With the exception of `00_run_all.R`, all files create tex files in the numbers/timing/ directory to indicate how long it took the code to run. Each of the files creates one tex file except for 04_training.R, which creates 14. With the computational infrastructure described in the Specs section, the code takes a bit more than 7 hours to run.

- **`00_run_all.R`** 
	
  This file allows you to run all code in one command line, with fertility-prediction-challenge-2024 as the working directory. See the instructions below for how to use this file. 
	
	
	
- **`01_outcome_time_shift.Rmd`**
	
  This file reads data/PreFer_train_data.csv, data/PreFer_train_supplementary_data.csv, and data/PreFer_train_background_data.csv.
	
  This file creates data/intermediate_files/outcome_2018to2020.csv.
	
  The csv file contains fertility outcomes between 2018 and 2020.
	
	
	
- **`02_feature_time_shift.R`** 
	
  This file reads data/PreFer_train_data.csv, data/PreFer_train_supplementary_data.csv, and data/intermediate_files/outcome_2018to2020.csv.
	
  This file creates data/intermediate_files/train_data_for_2018to2020.csv.
	
  The csv file contains predictive features for outcomes between 2018 and 2020.
	
	
	
- **`03_clean_df.R`**
	
  This file reads data/PreFer_train_data.csv, data/PreFer_train_background_data.csv, data/intermediate_files/train_data_for_2018to2020.csv, data/PreFer_holdout_data.csv (if preds_holdout == TRUE), and data/PreFer_holdout_background_data.csv (if preds_holdout == TRUE).
	
  This file creates 18 (if preds_holdout == FALSE)  or 27 (additional 9 files if preds_holdout == TRUE) RDS files in data/intermediate_files/cleaned_dfs. These files are different cleaned versions of the datasets.
	
	
	
- **`04_training.R`**
	
  This file reads 18 files (the ones created if preds_holdout == FALSE) from data/intermediate_files/cleaned_dfs. 
	
  This file creates and then later reads data/intermediate_files/folds.RDS. The file contains information on how the official training set is randomly assigned to five cross validation folds.
	
  If preds_cv == TRUE, this file creates 14 files in data/intermediate_files/preds_cv. These files are predictions made for each individual in the CV folds.
	
  This file always creates 14 files in data/intermediate_files/models. These are different model objects.
	
	
	
- **`05_observation_count.R`**
	
  This file reads data/PreFer_train_outcome.csv, data/intermediate_files/outcome_2018to2020.csv, data/PreFer_holdout_outcome.csv, and data/intermediate_files/train_data_for_2018to2020.csv.
	
  This file creates figures/figure2_data.csv, which contains the numbers mentioned in Figure 2.
	
  This file creates tables/table1.tex, which contains the Latex code for Table 1.
	
  This file creates two tex files in numbers/section2_2. The files contain the two numbers mentioned in that section, counting the number of individuals in each time period.
	
  This file creates one tex file in numbers/section4_3. The file contains the second number mentioned in that section, counting the number of individuals with outcomes between 2021 and 2023.
	
  This file creates up to two tex files in numbers/section4_7_1. The files contains the two numbers mentioned in that section, counting the number of individuals in the original training and holdout sets. The latter number is generated only if preds_holdout == TRUE.
	
  This file always creates two tex files in numbers/section2_3. The files contain the fourth and fifth numbers in that section. The numbers are the percentage of participant new children in the two time periods.
  
  This file always creates two tex files in numbers/section3_2. The files contain the two percentages in that section. The numbers are the percentage of partnered people and the percentage of partnered people who have partner data available.
	
	
	
- **`06_missingness_count.R`**
	
  This file reads data/PreFer_train_data.csv, data/PreFer_train_supplementary_data.csv, data/intermediate_files/cleaned_dfs/cleaned_train_2021to2023_full_features_final.RDS, data/intermediate_files/cleaned_dfs/cleaned_train_2018to2020_full_features_final.RDS, data/PreFer_train_outcome.csv, data/intermediate_files/otucome_2018to2020.
	
  This file creates a tex file in numbers/section4_3. The file counts the original numbe of features in the original training file.
	
  This file creates three tex file in numbers/section2_3. Two of the numbers represent the average missing rate in the two time periods, respectively. 
  
  This file creates a tex file in numbers/section_a_1_1. The file counts the number of people in time shifted data without a core survey up to 2017.
	
  This file creates three jpg files in the figures folder. One figure features collection histograms that show missingness rates across features. The other two feature two flowcharts that illustrate the data exclusion process for the original data and the time shifted data.
	
	
	
- **`07_feature_table.R`**
	
  This file reads data/table_a1.csv.
	
  This file outputs table/table_a1.tex. The table describes the features used in the winning xgboost model.
	
	
	
- **`08_all_preds.R`**
	
  This file reads data/intermediate_files/folds.RDS and 14 RDS files from data/intermediate_files/preds_cv. If preds_holdout == TRUE, it will also read data/PreFer_holdout_data.csv, data/PreFer_holdout_outcome.csv, data/PreFer_train_outcome.csv, 9 RDS files from data/intermediate_files/cleaned_dfs (the 9 not read by training.R), and 14 RDS files from data/intermediate_files/models.
	
  This file creates data/intermediate_files/preds.RDS, which contains all predictions by all models, as well training set means and outcomes.
	
	
	
- **`09_metrics.R`**
	
  This file establishes several custom made performance metric functions in the tidymodels style.
	
	
	
- **`10_eval.R`**
	
  This file reads data/intermediate_files/preds.RDS.
	
  This file creates 5 jpg files in the figures folder and 8 tex files in the tables folder to illustrate model performance or improvements inperformance.
	
  This file creates 18 tex files in numbers/section5_1 and 12 tex files in numbers/section5_3, representing the performance measures mentioned in those sections
  
  
  
### Data

You will need to add the following data files to your local fertility-prediction-challenge-2024/data/ directory to run the code, with the exception of `table_a1.csv` which we have already posted in this repository. 

Prepared PreFer survey datasets are available via SANE: https://odissei-data.nl/facility/secure-analysis-environment-sane/.

Alternatively, if you want to prepare the data files yourself, the LISS panel data is available at https://www.lissdata.nl/how-it-works-archive, and scripts for creating PreFer datasets from LISS data are available on the project page in the LISS data archive at https://doi.org/10.57990/f3ge-3a61. 

⚠️ **Warning:** Please do not post the PreFer datasets publicly, as they contain sensitive personal information. If you use the same file names we used below, these files are already included in the `.gitignore` file to ensure they will not be posted publicly.

- **`data/PreFer_train_data.csv`**  
  Predictive features for the official PreFer training set (provided by PreFer organizers).

- **`data/PreFer_train_outcome.csv`**  
  Fertility outcome for the official training set (provided by PreFer organizers).

- **`data/PreFer_train_supplementary_data.csv`**  
  Predictive features for people not in the official training/holdout set but who participated in at least one core survey by 2020 (provided by PreFer organizers).

- **`data/PreFer_train_background_data.csv`**  
  Additional background data on members of the official training set and their households (provided by PreFer organizers).

- **`data/PreFer_holdout_data.csv`**  
  Predictive features for the official PreFer holdout set (provided by PreFer organizers).

- **`data/PreFer_holdout_outcome.csv`**  
  Fertility outcome for the official holdout set (provided by PreFer organizers).

- **`data/PreFer_holdout_background_data.csv`**  
  Additional background data on members of the holdout set and their households (provided by PreFer organizers).

- **`table_a1.csv`**  
  Prepared manually to describe features in our winning model.
  
  
  
## INSTRUCTIONS FOR REPRODUCING RESULTS

We have successfully reproduced the results for cross-validated performance using two different Mac computers. We will test reproducibility for holdout and holdout + cross-validated combined performance when we are ready to assess performance on the holdout set after the journal review process. We have not tested reproducibility on Windows or Linux.

**Please follow these steps to reproduce our results:**

1. Clone the this repository to your local machine.

2. Download all necessary data files to fertility-prediction-challenge-2024/data/ as described above.

3. Install R 4.3.3 using the appropriate CRAN link for your computer. For example, see here for an Apple silicon Mac (M1/M2): https://cran.r-project.org/bin/macosx/big-sur-arm64/base/. Other versions of R might also work.

4. Install Groundhog 3.2.3 by running the code below in R. Other versions of Groundhog might also work.
    ```
    remotes::install_version("groundhog", version = "3.2.3", repos = "https://cloud.r-project.org")
    ```

5. Open a terminal window on Mac, or Command Prompt/PowerShell on Windows, and set your working directory to the cloned repository. For example: 
    ```
    cd /path/to/the/folder/where/you/cloned/fertility-prediction-challenge-2024
    ```

6. Choose the desired command from the list below and run it in the terminal.

    For cross-validated (CV) performance, run:
	
    ```
    Rscript code/00_run_all.R --preds_cv TRUE --preds_holdout FALSE
    ```
	
    For holdout performance, run:
	
    ```
    Rscript code/00_run_all.R --preds_cv FALSE --preds_holdout TRUE
    ```
	
    For performance in combined CV and holdout data, run:
	
    ```
    Rscript code/00_run_all.R --preds_cv TRUE --preds_holdout TRUE
    ```

**Notes:** 
- Groundhog will automatically handle dependencies within R, but you might receive errors indicating you need to install system dependencies. For example, the coauthor testing the reproducibility of this code on a different computer had to install pandoc (just type "brew install pandoc" in the terminal).
- You should expect to see warnings like the following as the code runs; these warnings are not a concern.
    ```
    → A | warning: ! There are new levels in a factor: `NA`.
    There were issues with some computations   A: x1475
    ```

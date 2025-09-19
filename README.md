Computer: 2020 MacBook Pro
Processor: 2 GHz Quad-Core Intel Core i5
Memory: 16 GB 3733 MHz LPDDR4X
OS: MacOS Sequoia 15.6.1
Disk Space: 512GB
Programming Language: R 4.3.3
Packages:
groundhog 3.2.0
here 1.01
rmarkdown 2.26
knitr 1.46
tidyverse 2.0.0
haven 2.5.4
data.table 1.15.4
tidymodels 1.2.0
xgboost 1.7.7.1
kableExtra 1.4.0
ggthemes 5.1.0
ggflowchart 1.0.0
ggridges 0.5.6




TABLE OF CONTENTS



DATA



	data/PreFer_train_data.csv
	
	Provided by PreFer organizers, this file contains predictive features for the official PreFer training set.
	
	
	
	data/PreFer_train_outcome.csv

	Provided by PreFer organizers, this file contains the fertility outcome for the official training set.



	data/PreFer_train_supplementary_data.csv
	
	Provided by PreFer organizers, this file contains predictive features for people not in the official PreFer training or holdout set but participated in at least one core survey by 2020.
	
	
	
	PreFer_train_background_data.csv
	
	Provided by the PreFer organizers, this file contains additional background data on members of the official training set and their households.
	
	
	
	data/PreFer_holdout_data.csv
	
	Provided by PreFer organizers, this file contains predictive features for the official PreFer holdout set.
	
	
	
	data/PreFer_holdout_outcome.csv

	Provided by PreFer organizers, this file contains the fertility outcome for the official holdout set.



	PreFer_holdout_background_data.csv
	
	Provided by the PreFer organizers, this file contains additional background data on members of the official holdout set and their households.
	
	
	
	table_a1.csv
	
	We prepared the table by hand to describe features in our winning model
	
	
	
CODE



	With the exception of 00_run_all.R, all files create tex files in numbers/timing to indicate how long it took the code to run. Each of the file create one tex files except for 04_training.R, which creates 14. With the computational infrastructure described above, the code takes a bit more than 7 hours to run.



	00_run_all.R 
	
	This file allows you to run all code in one command line, with fertility-prediction-challenge-2024 as the working directory. 
	
	For CV performance, run:
	
	Rscript code/00_run_all.R --preds_cv TRUE --preds_holdout FALSE
	
	For holdout performance, run:
	
	Rscript code/00_run_all.R --preds_cv FALSE --preds_holdout TRUE
	
	For performance in combined CV and holdout data, run:
	
	Rscript code/00_run_all.R --preds_cv TRUE --preds_holdout TRUE

	
	
	01_outcome_time_shift.Rmd 
	
	This file reads data/PreFer_train_data.csv, data/PreFer_train_supplementary_data.csv, and data/PreFer_train_background_data.csv.
	
	This file creates data/intermediate_files/outcome_2018to2020.csv.
	
	The csv file contains fertility outcomes between 2018 and 2020.
	
	
	
	02_feature_time_shift.R 
	
	This file reads data/PreFer_train_data.csv, data/PreFer_train_supplementary_data.csv, and data/intermediate_files/outcome_2018to2020.csv.
	
	This file creates data/intermediate_files/train_data_for_2018to2020.csv.
	
	The csv file contains predictive features for outcomes between 2018 and 2020.
	
	
	
	03_clean_df.R
	
	This file reads data/PreFer_train_data.csv, data/PreFer_train_background_data.csv, data/intermediate_files/train_data_for_2018to2020.csv, data/PreFer_holdout_data.csv (if preds_holdout == TRUE), and data/PreFer_holdout_background_data.csv (if preds_holdout == TRUE).
	
	This file creates 18 (if preds_holdout == FALSE)  or 27 (additional 9 files if preds_holdout == TRUE) RDS files in data/intermediate_files/cleaned_dfs. These files are different cleaned versions of the datasets.
	
	
	
	04_training.R
	
	This file reads 18 files (the ones created if preds_holdout == FALSE) from data/intermediate_files/cleaned_dfs. 
	
	This file creates and then later reads data/intermediate_files/folds.RDS. The file contains information on how the official training set is randomly assigned to five cross validation folds.
	
	If preds_cv == TRUE, this file creates 14 files in data/intermediate_files/preds_cv. These files are predictions made for each individual in the CV folds.
	
	This file always creates 14 files in data/intermediate_files/models. These are different model objects.
	
	
	
	05_observation_count.R
	
	This file reads data/PreFer_train_outcome.csv, data/intermediate_files/outcome_2018to2020.csv, data/PreFer_holdout_outcome.csv, and data/intermediate_files/train_data_for_2018to2020.csv.
	
	This file creates figures/figure2_data.csv, which contains the numbers mentioned in Figure 2.
	
	This file creates tables/table1.tex, which contains the Latex code for Table 1.
	
	This file creates two tex files in numbers/section2_2. The files contain the two numbers mentioned in that section, counting the number of individuals in each time period.
	
	This file creates one tex file in numbers/section4_3. The file contains the second number mentioned in that section, counting the number of individuals with outcomes between 2021 and 2023.
	
	This file creates up to two tex files in numbers/section4_7_1. The files contains the two numbers mentioned in that section, counting the number of individuals in the original training and holdout sets. The latter number is generated only if preds_holdout == TRUE
	
	This file always creates two tex files in numbers/section2_3. The files contain the fourth and fifth numbers in that section. The numbers are the percentage of participant new children in the two time periods.
	
	
	
	06_missingness_count.R
	
	This file reads data/PreFer_train_data.csv, data/PreFer_train_supplementary_data.csv, data/intermediate_files/cleaned_dfs/cleaned_train_2021to2023_full_features_final.RDS, data/intermediate_files/cleaned_dfs/cleaned_train_2018to2020_full_features_final.RDS, data/PreFer_train_outcome.csv, data/intermediate_files/otucome_2018to2020.
	
	This file creates a tex files in numbers/section4_3. The file counts the original numbe of features in the original training file.
	
	This file creates three tex file in numbers/section2_3. Two of the numbers represent the average missing rate in the two time periods, respectively. One of the numbers count the number of people in time shifted data without a core survey up to 2017.
	
	This file creates two png files in the figures folder. One figure features collection histograms that show missingness rates across features. The other figure figures a flowchart that illustrate the data exclusion process.
	
	
	
	07_feature_table.R
	
	This file reads data/table_a1.csv.
	
	This file outputs table/table_a1.tex. The table describes the features used in the winning xgboost model.
	
	
	
	08_all_preds.R
	
	This file reads data/intermediate_files/folds.RDS and 14 RDS files from data/intermediate_files/preds_cv. If preds_holdout == TRUE, it will also read data/PreFer_holdout_data.csv, data/PreFer_holdout_outcome.csv, data/PreFer_train_outcome.csv, 9 RDS files from data/intermediate_files/cleaned_dfs (the 9 not read by training.R), and 14 RDS files from data/intermediate_files/models
	
	This file creates data/intermediate_files/preds.RDS, which contains all predictions by all models, as well training set means and outcomes.
	
	
	
	09_metrics.R
	
	This file establishes several custom made performance metric functions in the tidymodels style
	
	
	
	10_eval.R
	
	This file reads data/intermediate_files/preds.RDS
	
	This file creates 5 png files in the figures folder and 8 tex files in the tables folder to illustrate model performance or improvements inperformance.
	
	This file creates 18 tex files in numbers/section5_1 and 12 tex files in numbers/section5_3, representing the performance measures mentioned in those sections
	
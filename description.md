# Description of submission

## Summary 

XGBoost with the following strategies: (1) Expanded sample size with "time-shifted" data, (2) Merged in data from the partner's survey for households where both partners participated, (3) Combined data from related features into "scales". 

## Details 

(1) We roughly tripled the amount of training data using a "time-shift" strategy. By adapting the outcome calculation code which was generously provided by the PreFer organizing team, we calculated whether suitably aged people in the training and supplementary data had children between 2018 and 2020, thus creating additional outcome data. For these rows of additional outcome data, we recoded features from year t-minus-1, year t-minus-2, etc., to have the same name as the equivalent features at year t-minus-1, year t-minus-2, etc. in the original data. For example, in a time-shifted row, cf17j128 is renamed as cf20m128 in order to correspond with data 3 years later. To help account for temporal distribution shift, we include a feature that is an indicator of whether the row comes from the time-shifted data or original data.

(2) For households where both partners participated in the survey, we merge in the partner's fertility intentions from 2019 and 2020 (cf19l128 to cf19l130 and cf20m128 to cf20m130), plus the partner's answers to questions about how many kids they have. 

(3) We generate "scales" in the feature data by averaging related features together. Our scales are: Feelings toward current child, gendered religiosity, attitudes about traditional fertility, attitudes about traditional motherhood, attitudes about traditional fatherhood, attitudes about traditional marriage, attitudes toward working mothers, and sexism.

We choose the hyperparameters for our XGBoost model via grid-search hyperparameter tuning with 5-fold cross-validation. 


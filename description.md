# Description of submission

## The Model

We fit an xgboost model with 66 hand-picked variables, which are converted to 43 predictors. An additional predictor is whether the observation is time-shifted (see the section below).

## The Data

We roughly tripled the amount of training data using a time-shift strategy. By adapting outcome_time_shift.Rmd generously provided by the PreFer organizing team, we calculated whether suitably aged people in the training and supplementary data had children between 2018 and 2020. We then found earlier versions of our predictors and surmised that these earlier predictors predict childbirths between 2018 and 2020 in much the same way our predictors predict childbirths between 2021 and 2023. We then time-shifted those earlier measures to create additional rows in our training data.
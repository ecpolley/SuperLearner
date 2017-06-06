# SuperLearner: Prediction model ensembling method

[![Join the chat at https://gitter.im/SuperLearner](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/SuperLearner?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/SuperLearner)](http://cran.r-project.org/web/packages/SuperLearner)
[![Downloads](http://cranlogs.r-pkg.org/badges/SuperLearner)](http://cran.rstudio.com/package=SuperLearner)
[![Build Status: travis-ci](https://travis-ci.org/ecpolley/SuperLearner.svg?branch=master)](https://travis-ci.org/ecpolley/SuperLearner)
[![Build Status: appveyor](https://ci.appveyor.com/api/projects/status/github/ecpolley/SuperLearner?branch=master&svg=true)](https://ci.appveyor.com/project/ecpolley/superlearner)
[![codecov](https://codecov.io/gh/ecpolley/SuperLearner/branch/master/graph/badge.svg)](https://codecov.io/gh/ecpolley/SuperLearner)

This is the current version of the SuperLearner R package (version 2.*).

**Features**

* Automatic optimal predictor ensembling via cross-validation with one line of code.
* Dozens of algorithms: XGBoost, Random Forest, GBM, Lasso, SVM, BART, KNN, Decision Trees, Neural Networks, and more.
* Integrates with [caret](http://github.com/topepo/caret) to support even more algorithms.
* Includes framework to quickly add custom algorithms to the ensemble.
* Visualize the performance of each algorithm using built-in plotting.
* Easily check multiple hyperparameter configurations for each algorithm in the ensemble.
* Add new algorithms or change the default parameters for existing ones.
* Screen variables (feature selection) based on univariate association, Random Forest, Elastic Net, et al. or custom screening algorithms.
* Multicore and multinode parallelization for scalability.
* External cross-validation to estimate the performance of the ensembling predictor.
* Ensemble can optimize for any target metric: mean-squared error, AUC, log likelihood, etc.
* Includes framework to provide custom loss functions and stacking algorithms.

### Install the development version from GitHub:

```r
# install.packages("devtools")
devtools::install_github("ecpolley/SuperLearner")
```

### Install the current release from CRAN:
```r
install.packages("SuperLearner")
```

[devtools]: https://github.com/hadley/devtools
[CRAN]: https://cran.r-project.org/web/packages/SuperLearner/index.html

## Examples 

SuperLearner makes it trivial to run many algorithms and use the best one or an ensemble.

```r
data(Boston, package = "MASS")

set.seed(1)

sl_lib = c("SL.xgboost", "SL.randomForest", "SL.glmnet", "SL.nnet", "SL.ksvm",
           "SL.bartMachine", "SL.kernelKnn", "SL.rpartPrune", "SL.lm", "SL.mean")

# Fit XGBoost, RF, Lasso, Neural Net, SVM, BART, K-nearest neighbors, Decision Tree, 
# OLS, and simple mean; create automatic ensemble.
result = SuperLearner(Y = Boston$medv, X = Boston[, -14], SL.library = sl_lib)

# Review performance of each algorithm and ensemble weights.
result

# Use external (aka nested) cross-validation to estimate ensemble accuracy.
# This will take a while to run.
result2 = CV.SuperLearner(Y = Boston$medv, X = Boston[, -14], SL.library = sl_lib)

# Plot performance of individual algorithms and compare to the ensemble.
plot(result2) + theme_minimal()

# Hyperparameter optimization --
# Fit elastic net with 5 different alphas: 0, 0.2, 0.4, 0.6, 0.8, 1.0.
# 0 corresponds to ridge and 1 to lasso.
enet = create.Learner("SL.glmnet", detailed_names = T,
                      tune = list(alpha = seq(0, 1, length.out = 5)))

sl_lib2 = c("SL.mean", "SL.lm", enet$names)

enet_sl = SuperLearner(Y = Boston$medv, X = Boston[, -14], SL.library = sl_lib2)

# Identify the best-performing alpha value or use the automatic ensemble.
enet_sl
```

For more detailed examples please review the vignette:

```r
vignette(package = "SuperLearner")
```

## References 

Polley EC, van der Laan MJ (2010) Super Learner in Prediction. U.C. Berkeley Division of Biostatistics Working Paper Series. Paper 226. <http://biostats.bepress.com/ucbbiostat/paper266/>

van der Laan, M. J., Polley, E. C. and Hubbard, A. E. (2007) Super Learner. Statistical Applications of Genetics and Molecular Biology, 6, article 25. <http://www.degruyter.com/view/j/sagmb.2007.6.issue-1/sagmb.2007.6.1.1309/sagmb.2007.6.1.1309.xml>

van der Laan, M. J., & Rose, S. (2011). Targeted learning: causal inference for observational and experimental data. Springer Science & Business Media. <http://www.targetedlearningbook.com>

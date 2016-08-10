# SuperLearner: automatic optimal model ensembling

[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/SuperLearner)](http://cran.r-project.org/web/packages/SuperLearner)
[![Downloads](http://cranlogs.r-pkg.org/badges/SuperLearner)](http://cran.rstudio.com/package=SuperLearner)
[![Build Status](https://travis-ci.org/ecpolley/SuperLearner.svg?branch=master)](https://travis-ci.org/ecpolley/SuperLearner)
[![codecov](https://codecov.io/gh/ecpolley/SuperLearner/branch/master/graph/badge.svg)](https://codecov.io/gh/ecpolley/SuperLearner)

This is the current version of the SuperLearner R package (version 2.*).

**Features**
* Automatic optimal model averaging via cross-validation.
* Dozens of algorithms including Random Forest, GBM, XGBoost, BART, Elastic Net, and Neural Networks.
* Integrates with [caret](http://github.com/topepo/caret) to support even more models.
* Visualize the performance of each algorithm using built-in plotting.
* Easily incorporate multiple hyperparameter configurations for each algorithm into the ensemble.
* Add new algorithms or change the default parameters for existing ones.
* Screen variables based on univariate association, Random Forest, Elastic Net, et al. or a custom screener.
* Multi-core and multi-node parallelization for scalability.
* External cross-validation to estimate the performance of the ensembling procedure.
* Ensemble can optimize for any target metric: mean-squared error, AUC, log likelihood, etc.
* Proven to produce an optimal ensemble through semi-parametric asymptotic statistics.

### Install the development version from GitHub:

```
if (!require("devtools")) install.packages("devtools")
devtools::install_github("ecpolley/SuperLearner")
```

### Install the current release from CRAN:
```
install.packages("SuperLearner")
```

[devtools]: https://github.com/hadley/devtools
[CRAN]: https://cran.r-project.org/web/packages/SuperLearner/index.html

## References

van der Laan, M. J., Polley, E. C. and Hubbard, A. E. (2008) Super Learner, Statistical Applications of Genetics and Molecular Biology, 6, article 25. http://www.bepress.com/sagmb/vol6/iss1/art25

van der Laan, M. J., & Rose, S. (2011). Targeted learning: causal inference for observational and experimental data. Springer Science & Business Media.

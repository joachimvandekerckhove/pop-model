Partially observable predictor models for identifying cognitive markers
================
2025-02-19

- [Load processed results](#load-processed-results)
- [Define colors for models](#define-colors-for-models)
- [Cross-Validation Results](#cross-validation-results)
- [Full Information Results](#full-information-results)

These results belong to “Partially observable predictor models for
identifying cognitive markers” by Zita Oravecz, Martin Sliwinski, Sharon
H. Kim, Lindy Williams, Mindy J. Katz, and Joachim Vandekerckhove.

Note that due to randomness in the MCMC procedure, these results may not
exactly match the results in the paper.

### Load processed results

``` r
results <- readRDS(here::here("tmp/processed_results.rds"))
```

### Define colors for models

``` r
model_colors <- c(
  "Manifest only" = "#009E73",
  "Manifest + latent descriptors" = "#E69F00",
  "Manifest + latent process" = "#0072B2"
)
```

### Cross-Validation Results

#### ROC Curves

![](results_files/figure-gfm/roc-plot-1.png)<!-- -->

#### AUC Results

|               | Model                         | AUC                       | Folds |
|:--------------|:------------------------------|:--------------------------|------:|
| manifest      | Manifest only                 | 0.5640 \[0.4938, 0.6342\] |    10 |
| descriptive   | Manifest + latent descriptors | 0.7371 \[0.6717, 0.8026\] |    10 |
| process_model | Manifest + latent process     | 0.7346 \[0.6680, 0.8013\] |    10 |

Cross-validation AUC results with 95% confidence intervals

### Full Information Results

#### Coefficient estimates for Manifest only

|                  |    mean |     sd |    2.5% |   97.5% |
|:-----------------|--------:|-------:|--------:|--------:|
| intercept_latent | -1.1898 | 0.2562 | -1.7038 | -0.7059 |
| coeff_age        |  0.2921 | 0.1265 |  0.0426 |  0.5394 |
| coeff_gender     | -0.2303 | 0.2808 | -0.7640 |  0.3287 |
| coeff_educ       |  0.0972 | 0.1383 | -0.1695 |  0.3744 |
| coeff_black      |  0.7985 | 0.2968 |  0.2196 |  1.3916 |
| coeff_hisp       |  0.6117 | 0.4340 | -0.2352 |  1.4633 |

#### Coefficient estimates for Manifest + latent descriptors

#### Coefficient estimates for Manifest + latent process

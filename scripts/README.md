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
| manifest      | Manifest only                 | 0.5637 \[0.4937, 0.6338\] |    10 |
| descriptive   | Manifest + latent descriptors | 0.7387 \[0.6733, 0.8040\] |    10 |
| process_model | Manifest + latent process     | 0.7320 \[0.6656, 0.7984\] |    10 |

Cross-validation AUC results with 95% confidence intervals

### Full Information Results

#### Coefficient estimates for Manifest only

|                  |    mean |     sd |    2.5% |   97.5% |
|:-----------------|--------:|-------:|--------:|--------:|
| intercept_latent | -1.0929 | 0.2385 | -1.5635 | -0.6409 |
| coeff_age        |  0.2800 | 0.1227 |  0.0416 |  0.5187 |
| coeff_gender     | -0.2362 | 0.2656 | -0.7567 |  0.2784 |
| coeff_educ       |  0.0736 | 0.1337 | -0.1897 |  0.3352 |
| coeff_black      |  0.6776 | 0.2706 |  0.1432 |  1.2092 |
| coeff_hisp       |  0.4521 | 0.3923 | -0.3256 |  1.1959 |

#### Coefficient estimates for Manifest + latent descriptors

|                  |    mean |     sd |    2.5% |   97.5% |
|:-----------------|--------:|-------:|--------:|--------:|
| intercept_latent | -3.5015 | 0.4950 | -4.4893 | -2.5385 |
| coeff_mrt        |  0.7043 | 0.1695 |  0.3737 |  1.0408 |
| coeff_srt        |  0.5201 | 0.6455 | -0.7190 |  1.7877 |
| coeff_age        |  0.1847 | 0.1314 | -0.0712 |  0.4474 |
| coeff_gender     | -0.2445 | 0.2766 | -0.7844 |  0.3028 |
| coeff_educ       |  0.1617 | 0.1390 | -0.1121 |  0.4337 |
| coeff_black      |  0.4358 | 0.2902 | -0.1217 |  1.0078 |
| coeff_hisp       |  0.2706 | 0.4094 | -0.5523 |  1.0585 |

#### Coefficient estimates for Manifest + latent process

|                  |    mean |     sd |    2.5% |   97.5% |
|:-----------------|--------:|-------:|--------:|--------:|
| intercept_latent | -2.8133 | 0.5604 | -3.9182 | -1.7310 |
| coeff_asymptote  |  0.6378 | 0.2305 |  0.1920 |  1.1019 |
| coeff_iiv        |  0.8407 | 0.5696 | -0.2588 |  1.9777 |
| coeff_gain       | -0.0313 | 0.1615 | -0.3539 |  0.2825 |
| coeff_learning   | -1.4148 | 0.6116 | -2.6657 | -0.2586 |
| coeff_age        |  0.1932 | 0.1369 | -0.0706 |  0.4659 |
| coeff_gender     | -0.1990 | 0.2788 | -0.7408 |  0.3571 |
| coeff_educ       |  0.1899 | 0.1422 | -0.0860 |  0.4665 |
| coeff_black      |  0.4871 | 0.2944 | -0.0913 |  1.0602 |
| coeff_hisp       |  0.2981 | 0.4190 | -0.5329 |  1.1034 |

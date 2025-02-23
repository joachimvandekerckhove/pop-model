#! /usr/bin/env Rscript

# This code belongs "to Partially observable predictor models for identifying
# cognitive markers" by Zita Oravecz, Martin Sliwinski, Sharon H. Kim, Lindy
# Williams, Mindy J. Katz, and Joachim Vandekerckhove.
# 
# The data used in this project are not publicly available and are not included
# in this repository.  The code can be read but not run.
# 
# Copyright (C) 2024 Oravecz and Vandekerckhove
# 
# This program comes with ABSOLUTELY NO WARRANTY.
# This is free software, and you are welcome to redistribute it
# under the conditions described in the GNU GPL v3.0 license.
# Visit https://www.gnu.org/licenses/gpl-3.0.en.html for details.
# 
# To read the analysis code, access mainFullInfo.R in this directory.

library(here)

here::i_am("scripts/mainFullInfo.R")

source(here::here("src/init.R"))

# Get model_type from command line
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1) {
	stop("One argument must be supplied (model_type).", call. = FALSE)
}
model_type <- args[1]

# Set up model
switch(model_type,
       "process_model" = {
           model <- "pop.process_model"
           parameters <- c("intercept_latent",
                           "mu_asymptote", "mu_iiv", "mu_gain", "mu_learning",
                           "sd_asymptote", "sd_iiv", "sd_gain", "sd_learning",
                           "coeff_asymptote", "coeff_iiv", "coeff_gain", "coeff_learning",
                           "coeff_age", "coeff_gender", "coeff_educ", "coeff_black",
                           "coeff_hisp", "pi")
       },
       "descriptive" = {
           model <- "pop.descriptive"
           parameters <- c("intercept_latent",
                           "mu_mrt", "mu_srt",
                           "sd_mrt", "sd_srt",
                           "coeff_mrt", "coeff_srt",
                           "coeff_age", "coeff_gender", "coeff_educ", "coeff_black",
                           "coeff_hisp", "pi")
       },
       "manifest" = {
           model <- "pop.manifest"
           parameters <- c("intercept_latent",
                           "coeff_age", "coeff_gender", "coeff_educ", "coeff_black",
                           "coeff_hisp", "pi")
       },
       stop(sprintf("Invalid model_type '%s'. Must be one of: process_model, descriptive, manifest", 
                   model_type)))

# Use external Stan model file
stanFile <- here::here("src/stan", paste0(model, ".stan"))

# Source helper functions
source(here::here("src/helpers.R"))

# Load data and make into list for Stan
load(here::here("data/EAS316.Rdata"))

# Prepare data for Stan model
data <- prepare_stan_data(eas_subset_stan, totalN, nrPersons, X, L, nrCov, nrCovL, MCIStatus, 
                         train = rep(TRUE, nrPersons))

# Run model
cat("Starting full-info model\n")

expmodel <- stan(stanFile,
                 data = data,
                 pars = parameters,
                 control = list(adapt_delta   = 0.99,
                                max_treedepth = 14),
                 chains  =    8 ,
                 warmup  = 4000 ,
                 iter    = 6000 )

savefile <- here::here(sprintf("tmp/fi/ss_%s.Rdata", model_type))

save.image(savefile)

cat(sprintf("Full-info model %s finished. Results saved to %s\n",
            model_type, savefile))

postprocess_full_info_model(savefile)
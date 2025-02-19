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

here::i_am("scripts/mainCrossVal.R")

source(here::here("src/init.R"))

# Get fold index from command line
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2) {
	stop("Two arguments must be supplied (model_type, fold).", call. = FALSE)
}
model_type <- args[1]
fold <- as.numeric(args[2])

switch(model_type,
       "process_model" = {
           model <- "pop.process_model"
           parameters <- c("pi")
       },
       "descriptive" = {
           model <- "pop.descriptive"
           parameters <- c("pi")
       },
       "manifest" = {
           model <- "pop.manifest"
           parameters <- c("pi")
       },
       stop(sprintf("Invalid model_type '%s'. Must be one of: process_model, descriptive, manifest", 
                   model_type)))

# Use external Stan model file
stanFile = here::here("src/stan", paste0(model, ".stan"))

# Load data for Stan
load(here::here("data/EAS316.Rdata"))

# Set up cross-validation
nFolds <- 10
folds <- stratify_folds(criterion = (MCIStatus == 1),
                        num_folds = nFolds)

# Prepare data for Stan model with training set for this fold
data <- prepare_stan_data(eas_subset_stan, totalN, nrPersons, X, L, nrCov, nrCovL, MCIStatus,
                         train = folds[[fold]])

cat(" Starting fold", fold, "of", nFolds, "\n")

expmodel <- stan(stanFile,
                data = data,
                pars = parameters,
                control = list(adapt_delta   = 0.99,
                               max_treedepth = 14),
                chains  =    4 ,
                warmup  = 2000 ,
                iter    = 2500 )

savefile <- here::here(sprintf("tmp/cv/ss_%s_fold%i.Rdata", model_type, fold))

save.image(savefile)

cat(sprintf("Cross-validation model %s, fold %i finished. Results saved to %s\n",
            model_type, fold, savefile))

# Helper functions for cross-validation and data processing
# Part of "Partially observable predictor models for identifying cognitive markers"
# Copyright (C) 2024 Oravecz and Vandekerckhove

# Cross-validation functions are courtesy of Ambiorix Labs. All rights reserved.

## Preprocessing

#' Split data into stratified folds for cross-validation
#' @param criterion Logical vector indicating the stratification criterion
#' @param num_folds Number of folds to create
#' @param seed Random seed for reproducibility
#' @return List of logical vectors indicating training sets for each fold
stratify_folds <- function(criterion, num_folds, seed = 42) {
  num_cases <- length(criterion)
  cases <- 1:num_cases
  folds <- vector("list", num_folds)
  
  if (!is.logical(criterion)) {
    stop('I think you did it wrong.  Criterion must be a logical.')
  }
  
  set.seed(seed)  # Set seed for reproducibility
  
  trues = cases[criterion]
  shuffled_cases <- sample(trues)
  nc = length(trues)
  
  fold_sizes <- rep(floor(nc / num_folds), num_folds)
  if ((nc %% num_folds) > 0) {
    fold_sizes[1:(nc %% num_folds)] <- 
      fold_sizes[1:(nc %% num_folds)] + 1
  }
  
  current_index <- 1
  for (i in 1:num_folds) {
    fold_indices <- current_index:(current_index + fold_sizes[i] - 1)
    folds[[i]] <- shuffled_cases[fold_indices]
    current_index <- current_index + fold_sizes[i]
  }
  
  falses = cases[!criterion]
  shuffled_cases <- sample(falses)
  nc = length(falses)
  
  fold_sizes <- rep(floor(nc / num_folds), num_folds)
  if ((nc %% num_folds) > 0) {
    fold_sizes[1:(nc %% num_folds)] <- 
      fold_sizes[1:(nc %% num_folds)] + 1
  }
  
  current_index <- 1
  for (i in 1:num_folds) {
    fold_indices <- current_index:(current_index + fold_sizes[i] - 1)
    folds[[i]] <- sort(c(folds[[i]], shuffled_cases[fold_indices]))
    current_index <- current_index + fold_sizes[i]
  }
  
  if (!(length(unlist(folds)) == length(criterion)) |
      !(length(unique(unlist(folds))) == length(criterion))) {
    stop('I think I did it wrong but I don\'t know what happened :(')
  }
  
  indices_to_logical <- function(indices, length) {
    logical_vector <- rep(TRUE, length)
    logical_vector[indices] <- FALSE
    return(logical_vector)
  }
  
  # Apply the function to each element in the list
  logical_folds <- lapply(folds, indices_to_logical, length = num_cases)
  
  if (!(sum(!unlist(logical_folds)) == length(criterion))) {
    stop('I think I did it wrong at the end :(')
  }
  
  return(logical_folds)
}

#' Prepare data list for Stan model
#' @param eas_subset_stan Data frame containing EAS data
#' @param totalN Total number of observations
#' @param nrPersons Number of persons
#' @param X Matrix of person predictors
#' @param L Matrix of logistic covariates
#' @param nrCov Number of covariates
#' @param nrCovL Number of logistic covariates
#' @param MCIStatus Vector of MCI status
#' @param train Optional vector indicating training set (default NULL)
#' @return List containing prepared data for Stan model
prepare_stan_data <- function(eas_subset_stan, totalN, nrPersons, X, L, nrCov, nrCovL, MCIStatus, train = NULL) {
    # Base data list with all required variables
    data_list <- list(
        N = totalN,
        P = nrPersons,
        K = if(!is.null(train)) sum(train) else nrPersons,
        X = X,
        L = L,
        nX = nrCov,
        nL = nrCovL,
        RT = eas_subset_stan$day_mean_rt/1000,
        personIndex = eas_subset_stan$PID,
        dayIndex = eas_subset_stan$studyday_rc,
        nrAssess = eas_subset_stan$no_asss_day,
        MCIStatus = MCIStatus,
        train = if(!is.null(train)) train else rep(TRUE, nrPersons)
    )
    
    data_list
} 

## Postprocessing

#' Load predictions from a single fold
#' @param model_type Type of model
#' @param fold Fold number
#' @param use_cache Whether to use cached results (default TRUE)
#' @return Data frame of predictions or NULL if fold not found
load_fold_predictions <- function(model_type, fold, use_cache = TRUE) {
    # Set up cache directory
    cache_dir <- here::here("tmp/cache")
    if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)
    
    # Define cache file path
    cache_file <- file.path(cache_dir, sprintf("%s_fold%i_predictions.rds", model_type, fold))
    
    # Check cache first
    if (use_cache && file.exists(cache_file)) {
        return(readRDS(cache_file))
    }
    
    savefile <- here::here("tmp/cv", sprintf("ss_%s_fold%i.Rdata", model_type, fold))
    if (!file.exists(savefile)) return(NULL)
    
    # Load fold results into new environment
    e <- new.env()
    cat(sprintf("Loading fold %i for model %s from %s\n", fold, model_type, savefile))
    load(savefile, envir = e)
    
    # Extract predictions for test set
    test_indices <- which(!e$data$train)
    
    # Get pi from transformed parameters
    pi <- rstan::get_posterior_mean(e$expmodel, pars="pi")[,1]
    
    # Repeat fold number to match length
    fold_col <- rep(fold, length(test_indices))
    
    predictions <- data.frame(
        fold = fold_col,
        mci_status = e$data$MCIStatus[test_indices],
        pi_mean = pi[test_indices]  # Using posterior mean
    )
    
    rm(e); gc()
    
    # Store in cache if enabled
    if (use_cache) {
        saveRDS(predictions, cache_file)
    }
    
    predictions
}

#' Collect predictions from all folds
#' @param model_type Type of model
#' @param nfolds Number of folds
#' @return Data frame of all predictions
collect_cv_predictions <- function(model_type, nfolds = 10) {
    # Collect predictions from all available folds
    all_preds <- lapply(1:nfolds, function(fold) {
        pred <- load_fold_predictions(model_type, fold)
        if (is.null(pred)) {
            warning(sprintf("Missing predictions for fold %d", fold))
            return(NULL)
        }
        pred
    })
    
    # Remove NULL entries and combine
    valid_preds <- all_preds[!sapply(all_preds, is.null)]
    
    if (length(valid_preds) == 0) {
        stop("No CV results found. Please run cross-validation first.")
    }
    
    # Check that all valid predictions have the same structure
    ncols <- unique(sapply(valid_preds, ncol))
    if (length(ncols) > 1) {
        stop("Inconsistent prediction structure across folds")
    }
    
    predictions <- do.call(rbind, valid_preds)
    predictions
}

#' Compute ROC and AUC from predictions
#' @param predictions Data frame with mci_status and pi_mean columns
#' @return List containing ROC object and AUC statistics
compute_roc_auc <- function(predictions) {
    roc_obj <- pROC::roc(predictions$mci_status ~ predictions$pi_mean)
    ci <- pROC::ci.auc(roc_obj)
    
    list(
        roc = roc_obj,
        auc = as.numeric(roc_obj$auc),
        ci_lower = ci[1],
        ci_upper = ci[3]
    )
}

#' Plot ROC curve with confidence intervals
#' @param roc_obj ROC object from pROC
#' @param model_type Type of model for plot title
plot_roc <- function(roc_obj, model_type) {
    plot(roc_obj, main = sprintf("ROC Curve for %s", model_type))
    pROC::ci.sp(roc_obj, col = "grey90")  # Add CI bands
}

#' Process cross-validation results
#' @param model_type Type of model (process_model, descriptive, or manifest)
#' @param plot Whether to create an ROC plot (default TRUE)
#' @param nfolds Number of folds (default 10)
#' @return List containing AUC statistics and number of folds
process_cv_results <- function(model_type, plot = TRUE, nfolds = 10) {
    predictions <- collect_cv_predictions(model_type, nfolds)
    results <- compute_roc_auc(predictions)
    
    if (plot) {
        plot_roc(results$roc, model_type)
    }
    
    list(
        auc = results$auc,
        ci_lower = results$ci_lower,
        ci_upper = results$ci_upper,
        n_folds = length(unique(predictions$fold))
    )
}

#' Load predictions from full info model
#' @param model_type Type of model
#' @return Data frame of predictions or NULL if not found
load_fullinfo_predictions <- function(model_type) {
    savefile <- here::here("tmp/fi", sprintf("ss_%s.Rdata", model_type))
    if (!file.exists(savefile)) return(NULL)
    
    # Load results into new environment
    e <- new.env()
    load(savefile, envir = e)
    
    predictions <- data.frame(
        mci_status = e$data$MCIStatus,
        pi_mean = e$expmodel@sim$samples[[1]]$pi  # Using first chain
    )
    
    rm(e); gc()
    predictions
}

#' Extract coefficient statistics from Stan model
#' @param model Stan model object
#' @return Data frame of coefficient statistics or NULL if extraction fails
extract_coefficient_stats <- function(model) {
    # Check if model contains samples
    if (!("sim" %in% slotNames(model)) || is.null(model@sim)) {
        warning("Stan model does not contain samples")
        return(NULL)
    }
    
    tryCatch({
        # Extract all samples
        samples <- rstan::extract(model)
        
        # Find coefficient parameters
        coeff_params <- grep("^coeff", names(samples), value = TRUE)
        
        if (length(coeff_params) == 0) {
            warning("No coefficient parameters found in model")
            return(NULL)
        }
        
        # Compute statistics for each coefficient
        stats <- lapply(coeff_params, function(param) {
            samples_vec <- samples[[param]]
            c(
                mean = mean(samples_vec),
                sd = sd(samples_vec),
                q2.5 = quantile(samples_vec, 0.025),
                q97.5 = quantile(samples_vec, 0.975)
            )
        })
        
        # Create data frame
        stats_df <- do.call(rbind, stats)
        rownames(stats_df) <- coeff_params
        as.data.frame(stats_df)
    }, error = function(e) {
        warning("Failed to extract coefficient statistics: ", e$message)
        NULL
    })
}

#' Process full information model results
#' @param savefile Path to the saved model results
#' @return Data frame of coefficient statistics or NULL if processing fails
postprocess_full_info_model <- function(savefile) {
    if (!file.exists(savefile)) {
        warning("No full information results found at: ", savefile)
        return(NULL)
    }
    
    # Load results into new environment
    e <- new.env()
    tryCatch({
        load(savefile, envir = e)
        
        if (!exists("expmodel", envir = e)) {
            warning("No model found in results file")
            return(NULL)
        }
        
        # Extract coefficient statistics
        coeff_stats <- extract_coefficient_stats(e$expmodel)
        if (is.null(coeff_stats)) {
            warning("Failed to extract statistics from model")
            return(NULL)
        }
        
        coeff_stats
    }, error = function(err) {
        warning("Error processing model results: ", err$message)
        NULL
    }, finally = {
        rm(e)
        gc()
    })
}
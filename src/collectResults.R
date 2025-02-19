#! /usr/bin/env Rscript

# Collect and save results for plotting and tables
# Part of "Partially observable predictor models for identifying cognitive markers"

library(here)
library(pROC)

here::i_am("src/collectResults.R")
source(here::here("src/helpers.R"))

# Model types to process
model_types <- c("manifest", "descriptive", "process_model")

# Define model labels (needed for plots and tables)
model_labels <- c(
    manifest = "Manifest only",
    descriptive = "Manifest + latent descriptors",
    process_model = "Manifest + latent process"
)

# Process each model type and collect data
plot_data <- list()
ci_data <- list()
cv_results <- list()
fi_results <- list()

for (model_type in model_types) {
    cat(sprintf("\n=== Processing %s ===\n", model_type))
    
    # Process cross-validation results
    cv <- tryCatch({
        predictions <- collect_cv_predictions(model_type)
        roc_obj <- pROC::roc(predictions$mci_status ~ predictions$pi_mean, 
                            direction = "<")
        ci <- pROC::ci.auc(roc_obj)
        
        # Store curve data for plotting
        plot_data[[model_type]] <- data.frame(
            FPR = 1 - roc_obj$specificities,
            TPR = roc_obj$sensitivities,
            Model = model_labels[model_type]
        )
        
        # Store CI data for plotting
        ci_coords <- ci.se(roc_obj, specificities = roc_obj$specificities)
        ci_data[[model_type]] <- data.frame(
            FPR = 1 - roc_obj$specificities,
            lower = ci_coords[,1],
            upper = ci_coords[,3],
            Model = model_labels[model_type]
        )
        
        # Store CV results for table
        cv_results[[model_type]] <- list(
            model = model_labels[model_type],
            auc = as.numeric(roc_obj$auc),
            ci_lower = ci[1],
            ci_upper = ci[3],
            n_folds = length(unique(predictions$fold))
        )
        
    }, error = function(e) {
        cat(sprintf("CV processing failed: %s\n", e$message))
        NULL
    })
    
    # Process full information results
    fi <- tryCatch({
        fi_results[[model_type]] <- postprocess_full_info_model(
            here::here(sprintf("tmp/fi/ss_%s.Rdata", model_type))
        )
    }, error = function(e) {
        cat(sprintf("Full info processing failed: %s\n", e$message))
        NULL
    })
}

# Combine plot data
plot_data <- do.call(rbind, plot_data)
ci_data <- do.call(rbind, ci_data)

# Save all results in a single file
results <- list(
    plot_data = plot_data,
    ci_data = ci_data,
    cv_results = cv_results,
    fi_results = fi_results,
    model_labels = model_labels
)

saveRDS(results, here::here("tmp/processed_results.rds")) 
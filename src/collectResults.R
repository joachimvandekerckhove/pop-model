#! /usr/bin/env Rscript

# Collect and save results for plotting and tables
# Part of "Partially observable predictor models for identifying cognitive markers"

library(here)
library(pROC)
library(ggplot2)

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

model_colors <- c(
  "Manifest only" = "#009E73",
  "Manifest + latent descriptors" = "#E69F00",
  "Manifest + latent process" = "#0072B2"
)

roc_plot <- ggplot() +
  geom_ribbon(data = results$ci_data,
              aes(x = FPR,
                  ymin = lower,
                  ymax = upper,
                  fill = Model,
                  group = Model),
              alpha = 0.2) +
  geom_line(data = results$plot_data,
            aes(x = FPR,
                y = TPR,
                color = Model,
                group = Model),
            linewidth = 1) +
  geom_abline(intercept = 0,
              slope = 1,
              linetype = "dashed",
              color = "gray50") +
  scale_color_manual(values = model_colors) +
  scale_fill_manual(values = model_colors) +
  labs(x = "False positive rate",
       y = "True positive rate") +
  coord_equal() +
  theme_minimal() +
  theme(
    text = element_text(family = "serif", color = "black"),
    axis.title = element_text(size = 26, color = "black"),
    axis.text = element_text(size = 22, color = "black"),
    legend.title = element_text(size = 22, color = "black"),
    legend.text = element_text(size = 22, color = "black"),
    legend.position = c(0.975, 0.05),
    legend.justification = c(1, 0),
    legend.direction = "vertical",
    legend.box = "vertical",
    legend.margin = margin(t = 5, r = 5, b = 5, l = 5),
    legend.background = element_rect(fill = "white", color = NA),
    legend.box.background = element_rect(fill = "white", color = NA)
  )

# Save the plot
ggsave(here::here("figures/cv_roc_curves.pdf"), 
       roc_plot, 
       width = 8, 
       height = 8) 
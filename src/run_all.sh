#!/bin/bash
# Run all models (FI and CV) with parallelization
# Part of "Partially observable predictor models for identifying cognitive markers"

set -e  # Exit on error

# Create necessary directories
mkdir -p tmp/fi tmp/cv figures /tmp/stan_cache

# Model types
# MODELS=("manifest" "descriptive" "process_model")
MODELS=("process_model")
NUM_FOLDS=10

# Function to run full info model
run_fi() {
    local model=$1
    local base_dir="tmp/fi"
    local cache_dir="/tmp/stan_cache/fi_${model}"
    
    # Create directories
    mkdir -p "$cache_dir"
    
    echo "Running full info model: $model"
    if ! STAN_CACHE_DIR="$cache_dir" Rscript scripts/mainFullInfo.R "$model" 2>&1 | sed "s/^/[FI-$model] /"; then
        echo "[FI-$model] ERROR: Model failed to run properly"
        return 1
    fi
    
    # Ensure consistent file naming
    local base_path="${base_dir}/ss_${model}"
    if [ -f "${base_path}" ]; then
        mv "${base_path}" "${base_path}.Rdata"
    fi
}

# Function to run CV fold
run_cv_fold() {
    local model=$1
    local fold=$2
    local cache_dir="/tmp/stan_cache/cv_${model}_${fold}"
    
    # Create cache directory
    mkdir -p "$cache_dir"
    
    echo "Running CV fold $fold for model: $model"
    if ! STAN_CACHE_DIR="$cache_dir" Rscript scripts/mainCrossVal.R "$model" "$fold" 2>&1 | sed "s/^/[CV-$model-$fold] /"; then
        echo "[CV-$model-$fold] ERROR: Fold failed to run properly"
        return 1
    fi
}

# echo "=== Starting Full Information Models ==="
# # Run all FI models in parallel
# export -f run_fi
# parallel --jobs 3 run_fi ::: "${MODELS[@]}" || true

echo "=== Starting Cross-Validation Models ==="
# Run CV folds for each model (one model at a time, but folds in parallel)
export -f run_cv_fold
for model in "${MODELS[@]}"; do
    echo "Running CV folds for $model"
    parallel --jobs 5 run_cv_fold "$model" ::: $(seq 1 $NUM_FOLDS) || true
done

echo "=== Processing Results ==="
# Process all results
if ! Rscript scripts/postprocess.R; then
    echo "WARNING: Results processing failed"
fi

echo "=== All Done ==="
echo "Results saved in (if processing succeeded):"
echo "- tmp/processed_results.rds (numerical results)"
echo "- figures/cv_roc_curves.pdf (ROC plots)"

# Optionally clean up Stan cache
# rm -rf tmp/stan_cache  # Uncomment to clean up cache after running 
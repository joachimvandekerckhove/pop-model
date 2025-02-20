# Makefile for POP models paper
#
# Targets:
#   manifest-cv:             Run the manifest model cross-validation.
#   descriptive-cv:          Run the descriptive model cross-validation.
#   process-model-cv:        Run the process model cross-validation.
#   all-cv:                  Run all cross-validation models.
#   manifest-fi:             Run the manifest model full information.
#   descriptive-fi:          Run the descriptive model full information.
#   process-model-fi:        Run the process model full information.
#   all-fi:                  Run all full information models.
#   all:                     Run all models.
#   collect-results:         Collect the results for plotting and tables.
#   generate-report:         Generate the markdown report.
#   clone-repo:              Clone the repository.
#   container-check:         Check if we are in the container.
#   help:                    Display this help message.
#
# Example Usage:
#   make manifest-fi
#   make process-model-cv
#   make collect-results
#   make generate-report
#   make clone-repo
#   make help

# Directory structure
TMP_DIR = tmp
CACHE_DIR = $(TMP_DIR)/cache
CV_DIR = $(TMP_DIR)/cv
FI_DIR = $(TMP_DIR)/fi
FIG_DIR = figures

# Number of folds for cross-validation
NUM_FOLDS = 10

# Job control - each model uses 4 cores, limit to 5 parallel jobs to stay under 20 cores
JOBS := -j 5
.NOTPARALLEL: generate-report collect-results

# Generate CV fold numbers
FOLDS := $(shell seq 1 $(NUM_FOLDS))

# Generate CV files for each model type
MANIFEST_CV_FILES := $(foreach fold,$(FOLDS),$(CV_DIR)/ss_manifest_fold$(fold).Rdata)
DESCRIPTIVE_CV_FILES := $(foreach fold,$(FOLDS),$(CV_DIR)/ss_descriptive_fold$(fold).Rdata)
PROCESS_CV_FILES := $(foreach fold,$(FOLDS),$(CV_DIR)/ss_process_model_fold$(fold).Rdata)

# Create required directories
# Outputs: Creates directories if they don't exist
$(TMP_DIR) $(CACHE_DIR) $(CV_DIR) $(FI_DIR) $(FIG_DIR):
	mkdir -p $@

# Rules for manifest model CV
$(CV_DIR)/ss_manifest_fold%.Rdata: scripts/mainCrossVal.R src/stan/manifest.stan | $(CV_DIR)
	Rscript $< manifest $*

# Rules for descriptive model CV
$(CV_DIR)/ss_descriptive_fold%.Rdata: scripts/mainCrossVal.R src/stan/descriptive.stan | $(CV_DIR)
	Rscript $< descriptive $*

# Rules for process model CV
$(CV_DIR)/ss_process_model_fold%.Rdata: scripts/mainCrossVal.R src/stan/process_model.stan | $(CV_DIR)
	Rscript $< process_model $*

# Model-specific CV targets with job control
manifest-cv: $(MANIFEST_CV_FILES)
.PHONY: manifest-cv
.NOTPARALLEL: manifest-cv

descriptive-cv: $(DESCRIPTIVE_CV_FILES)
.PHONY: descriptive-cv
.NOTPARALLEL: descriptive-cv

process-model-cv: $(PROCESS_CV_FILES)
.PHONY: process-model-cv
.NOTPARALLEL: process-model-cv

# Run CV models sequentially but allow parallelism within each model type
all-cv:
	$(MAKE) $(JOBS) manifest-cv
	$(MAKE) $(JOBS) descriptive-cv
	$(MAKE) $(JOBS) process-model-cv

# Full info model rules
$(FI_DIR)/ss_%.Rdata: scripts/mainFullInfo.R src/stan/%.stan | $(FI_DIR)
	Rscript $< $*

manifest-fi: $(FI_DIR)/ss_manifest.Rdata
descriptive-fi: $(FI_DIR)/ss_descriptive.Rdata
process-model-fi: $(FI_DIR)/ss_process_model.Rdata

# Run FI models sequentially
all-fi:
	$(MAKE) manifest-fi
	$(MAKE) descriptive-fi
	$(MAKE) process-model-fi

all: all-cv all-fi

# Results processing
$(TMP_DIR)/processed_results.rds: src/collectResults.R \
    $(MANIFEST_CV_FILES) $(DESCRIPTIVE_CV_FILES) $(PROCESS_CV_FILES) \
    $(FI_DIR)/ss_manifest.Rdata $(FI_DIR)/ss_descriptive.Rdata $(FI_DIR)/ss_process_model.Rdata | $(TMP_DIR)
	Rscript $<

collect-results: $(TMP_DIR)/processed_results.rds

# Generate markdown output
scripts/README.md: scripts/results.Rmd $(TMP_DIR)/processed_results.rds
	mkdir -p scripts/results_files/figure-gfm
	cd scripts && Rscript -e "rmarkdown::render('results.Rmd', output_format='github_document', output_file='README.md')"

# Generate report (markdown only)
generate-report: scripts/README.md

# Phony targets
.PHONY: all all-cv all-fi manifest-cv descriptive-cv process-model-cv \
        manifest-fi descriptive-fi process-model-fi generate-report collect-results \
        container-check clone-repo

# Check if we are in the container
container-check:
	if [ -f /.dockerenv ]; then \
		echo "Running in container"; \
	else \
		echo "Not running in container"; \
		exit 1; \
	fi

# Clone the repository
clone-repo: container-check
	git clone https://github.com/joachimvandekerckhove/pop-models.git
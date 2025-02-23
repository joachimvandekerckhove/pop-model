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
CV_DIR = $(TMP_DIR)/cv
FI_DIR = $(TMP_DIR)/fi
CACHE_DIR = $(TMP_DIR)/cache
FIG_DIR = figures

# Source files
MAINCV_SCRIPT := scripts/mainCrossVal.R
MAINFI_SCRIPT := scripts/mainFullInfo.R
MANIFEST_STAN := src/stan/pop.manifest.stan
DESCRIPTIVE_STAN := src/stan/pop.descriptive.stan
PROCESS_STAN := src/stan/pop.process_model.stan

# Output file lists
MANIFEST_CV_FILES := $(foreach n,$(shell seq 1 10),$(CV_DIR)/ss_manifest_fold$(n).Rdata)
DESCRIPTIVE_CV_FILES := $(foreach n,$(shell seq 1 10),$(CV_DIR)/ss_descriptive_fold$(n).Rdata)
PROCESS_CV_FILES := $(foreach n,$(shell seq 1 10),$(CV_DIR)/ss_process_model_fold$(n).Rdata)

# Output files for FI
MANIFEST_FI := $(FI_DIR)/ss_manifest.Rdata
DESCRIPTIVE_FI := $(FI_DIR)/ss_descriptive.Rdata
PROCESS_FI := $(FI_DIR)/ss_process_model.Rdata

# Create required directories
$(TMP_DIR) $(CV_DIR) $(FI_DIR) $(CACHE_DIR) $(FIG_DIR):
	mkdir -p $@

# Initialize semaphore for job control (max 6 concurrent jobs)
.ONESHELL:
init-sem:
	@parallel --semaphore --init --jobs 6

# Pattern rules for CV files
$(CV_DIR)/ss_manifest_fold%.Rdata: $(MAINCV_SCRIPT) $(MANIFEST_STAN) | $(CV_DIR)
	@echo "Running manifest fold $*"
	@sem -j 6 "Rscript $(MAINCV_SCRIPT) manifest $*"

$(CV_DIR)/ss_descriptive_fold%.Rdata: $(MAINCV_SCRIPT) $(DESCRIPTIVE_STAN) | $(CV_DIR)
	@echo "Running descriptive fold $*"
	@sem -j 6 "Rscript $(MAINCV_SCRIPT) descriptive $*"

$(CV_DIR)/ss_process_model_fold%.Rdata: $(MAINCV_SCRIPT) $(PROCESS_STAN) | $(CV_DIR)
	@echo "Running process model fold $*"
	@sem -j 6 "Rscript $(MAINCV_SCRIPT) process_model $*"

# Rules for FI files
$(FI_DIR)/ss_manifest.Rdata: $(MAINFI_SCRIPT) $(MANIFEST_STAN) | $(FI_DIR)
	@echo "Running manifest full information model"
	@sem -j 6 "Rscript $(MAINFI_SCRIPT) manifest"

$(FI_DIR)/ss_descriptive.Rdata: $(MAINFI_SCRIPT) $(DESCRIPTIVE_STAN) | $(FI_DIR)
	@echo "Running descriptive full information model"
	@sem -j 6 "Rscript $(MAINFI_SCRIPT) descriptive"

$(FI_DIR)/ss_process_model.Rdata: $(MAINFI_SCRIPT) $(PROCESS_STAN) | $(FI_DIR)
	@echo "Running process model full information model"
	@sem -j 6 "Rscript $(MAINFI_SCRIPT) process_model"

# High-level targets
manifest-cv: | $(CV_DIR)
	@echo "Running manifest cross-validation"
	parallel --jobs 6 --halt now,fail=1 --progress "Rscript $(MAINCV_SCRIPT) manifest {}" ::: $(shell seq 1 10)

descriptive-cv: | $(CV_DIR)
	@echo "Running descriptive cross-validation"
	parallel --jobs 6 --halt now,fail=1 --progress "Rscript $(MAINCV_SCRIPT) descriptive {}" ::: $(shell seq 1 10)

process-model-cv: | $(CV_DIR)
	@echo "Running process model cross-validation"
	parallel --jobs 6 --halt now,fail=1 --progress "Rscript $(MAINCV_SCRIPT) process_model {}" ::: $(shell seq 1 10)

manifest-fi: $(FI_DIR)/ss_manifest.Rdata
descriptive-fi: $(FI_DIR)/ss_descriptive.Rdata
process-model-fi: $(FI_DIR)/ss_process_model.Rdata

all-cv: manifest-cv descriptive-cv process-model-cv
all-fi: manifest-fi descriptive-fi process-model-fi
all: all-fi all-cv

# Results processing
$(TMP_DIR)/processed_results.rds: src/collectResults.R | $(TMP_DIR)
	@echo "Collecting results"
	Rscript $<

collect-results: $(TMP_DIR)/processed_results.rds

# Figures
$(FIG_DIR)/cv_roc_curves.pdf: $(TMP_DIR)/processed_results.rds  src/collectResults.R | $(FIG_DIR)
	@echo "Generating ROC curves"
	Rscript src/collectResults.R

roc-curves: $(FIG_DIR)/cv_roc_curves.pdf

# Report generation
generate-report: collect-results roc-curves
	@echo "Generating report"
	mkdir -p scripts/results_files/figure-gfm
	cd scripts && Rscript -e "rmarkdown::render('results.Rmd', output_format='github_document', output_file='README.md')"

# Container check
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

# Help target
help:
	@echo "Targets:"
	@echo "  manifest-cv:        Run the manifest model cross-validation"
	@echo "  descriptive-cv:     Run the descriptive model cross-validation"
	@echo "  process-model-cv:   Run the process model cross-validation"
	@echo "  all-cv:             Run all cross-validation models"
	@echo "  manifest-fi:        Run the manifest model full information"
	@echo "  descriptive-fi:     Run the descriptive model full information"
	@echo "  process-model-fi:   Run the process model full information"
	@echo "  all-fi:             Run all full information models"
	@echo "  all-models:         Run all models"
	@echo "  collect-results:    Collect results for plotting and tables"
	@echo "  generate-report:    Generate the markdown report"
	@echo "  clone-repo:         Clone the repository"
	@echo "  container-check:    Check if we are in the container"
	@echo "  help:               Display this help message"

# Mark targets that don't create files
.PHONY: all all-cv all-fi manifest-cv descriptive-cv process-model-cv \
        manifest-fi descriptive-fi process-model-fi collect-results generate-report roc-curves
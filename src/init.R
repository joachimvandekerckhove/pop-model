library(rstan)
library(parallel)
library(here)
# Set seed for reproducibility
set.seed(123)

# Set up parallel computing in Stan
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# Ensure tmp directory exists
tmp_dir <- here::here("tmp")
if (!dir.exists(tmp_dir)) {
  dir.create(tmp_dir)
}

if (!dir.exists(here::here("tmp/fi"))) {
  dir.create(here::here("tmp/fi"))
}

if (!dir.exists(here::here("tmp/cv"))) {
  dir.create(here::here("tmp/cv"))
}

if (!dir.exists(here::here("figures"))) {
  dir.create(here::here("figures"))
}

# clearing memory
rm(list=ls())

# Source helper functions
source(here::here("src/helpers.R"))


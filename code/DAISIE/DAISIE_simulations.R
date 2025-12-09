#######################
# DAISIE 
# Model Simulations                            
#######################

# Load required libraries
# devtools::install_github("rsetienne/DAISIE", ref = "develop")
library(readxl)
library(tidyverse)
library(DAISIE)
library(showtext)
font_add(family = "Gill Sans", regular = "/System/Library/Fonts/Supplemental/GillSans.ttc")
showtext_auto()

# Set a seed for reproducibility
set.seed(18)

# Read the Excel files with model parameters for native and possibly native 
models_parameters <- read_excel("../outputs/Models/DAISIE_Mascarenes_best_rep_parameters.xlsx")

# Simulations
## x1000 for models with (1) native lineages and (2) native + possibly native lineages
## Note: want 1000 simulations that have at least 1 non-endemic native (stac4) 
## https://cran.r-project.org/web/packages/DAISIE/vignettes/stac_key.html
## So we will overshoot and then filter to this condition

# Filter helper functions

# Function to check if a replicate has any STAC 4 species
has_stac4 <- function(replicate_data) {
  species_data <- replicate_data[-1]
  any(sapply(species_data, function(x) x$stac == 4))
}

# Function to count how many replicates contain each STAC class
count_stac <- function(dataset, label) {
  for (stac_val in 1:4) {
    count <- sum(sapply(dataset, function(rep) {
      species_data <- rep[-1]
      any(sapply(species_data, function(x) x$stac == stac_val))
    }))
    message(label, " - STAC ", stac_val, ": ", count)
  }
}

# Run and filter simuluations

n_replicates_total <- 1500
target_replicates <- 1000

# ----- Native -----
# ML parameters: 0.572479273493838, 0.180861981687593, Inf, 0.000557097388019495, 0.394908505798182
Mascarenes_native_sims <- DAISIE_sim(
  time = 8.9,
  M = 4000,
  pars = c(0.572479273493838, 0.180861981687593, Inf, 0.000557097388019495, 0.394908505798182),
  replicates= n_replicates_total,
  plot_sims = FALSE
)

# review stacs across replicates
count_stac(Mascarenes_native_sims, "Native")

# Filter for simulations that include at least one non-endemic native (STAC 4)
Mascarenes_native_sims_stac4 <- sapply(Mascarenes_native_sims, has_stac4)
Mascarenes_native_sims_filt <- Mascarenes_native_sims[Mascarenes_native_sims_stac4]
message("Native model: found ", sum(Mascarenes_native_sims_stac4), " of ", n_replicates_total, " replicates with ≥1 STAC 4.")

# Select first 1000 passing replicates (or all if fewer than 1000)
Mascarenes_native_sims_filt_1000 <- Mascarenes_native_sims_filt[1:min(target_replicates, length(Mascarenes_native_sims_filt))]

# Save filtered simulations
save(Mascarenes_native_sims_filt_1000, file = "../outputs/Models/Mascarenes_native_sims_filt_1000.RData")

# plot and save DDT
pdf(file = "../outputs/Models/Mascarenes_native_DTT_raw.pdf", width = 8, height = 8.5)
par(family = "Gill Sans")  
DAISIE_plot_sims(Mascarenes_native_sims_filt_1000)
dev.off()

# ----- Native + Possibly Native -----
# ML parameters: 0.577418945873205, 0.477318630479429, Inf, 0.0013615281087589, 0.0891234857776986
Mascarenes_nativeposs_sims <- DAISIE_sim(
  time = 8.9,
  M = 4000,
  pars = c(0.577418945873205, 0.477318630479429, Inf, 0.0013615281087589, 0.0891234857776986),
  replicates= n_replicates_total,
  plot_sims = FALSE
)

# review stacs across replicates
count_stac(Mascarenes_nativeposs_sims, "Nativeposs")

# Filter for simulations that include at least one non-endemic native (STAC 4)
Mascarenes_nativeposs_sims_stac4 <- sapply(Mascarenes_nativeposs_sims, has_stac4)
Mascarenes_nativeposs_sims_filt <- Mascarenes_nativeposs_sims[Mascarenes_nativeposs_sims_stac4]
message("Native model: found ", sum(Mascarenes_nativeposs_sims_stac4), " of ", n_replicates_total, " replicates with ≥1 STAC 4.")

# Select first 1000 passing replicates (or all if fewer than 1000)
Mascarenes_nativeposs_sims_filt_1000 <- Mascarenes_nativeposs_sims_filt[1:min(target_replicates, length(Mascarenes_nativeposs_sims_filt))]

# Save filtered simulations
save(Mascarenes_nativeposs_sims_filt_1000, file = "../outputs/Models/Mascarenes_nativeposs_sims_filt_1000.RData")

# plot and save DDT
pdf(file = "../outputs/Models/Mascarenes_nativeposs_DTT_raw.pdf", width = 8, height = 8.5)
par(family = "Gill Sans")  
DAISIE_plot_sims(Mascarenes_nativeposs_sims_filt_1000)
dev.off()

## Remove if everything works

# Review the simulations

# Function to count replicates by STAC value  
count_stac <- function(dataset, label) {  
  for (stac_val in 1:4) {  
    count <- sum(sapply(dataset, function(rep) {  
      species_data <- rep[-1]  
      any(sapply(species_data, function(x) x$stac == stac_val))  
    }))  
    print(paste(label, "- STAC", stac_val, ":", count))  
  }  
}  

# Function to check if a replicate has any STAC 4 species
has_stac4 <- function(replicate_data) {
  species_data <- replicate_data[-1]
  any(sapply(species_data, function(x) x$stac == 4))
}

# Count replicates by STAC value (1, 2, 3, 4) for both datasets  
count_stac(Mascarenes_native_sims, "Native")  
count_stac(Mascarenes_nativeposs_sims, "Nativeposs") 

# Identify replicates with at least one non-endemic species (STAC 4) 
Mascarenes_native_sims_stac4 <- sapply(Mascarenes_native_sims, has_stac4)  
Mascarenes_nativeposs_sims_stac4 <- sapply(Mascarenes_nativeposs_sims, has_stac4)  

# Count replicates with STAC 4
sum(Mascarenes_native_sims_stac4)  
sum(Mascarenes_nativeposs_sims_stac4) 

# Get a list of replicate numbers with STAC 4 for both datasets  
native_sims_stac4_replicate_numbers <- which(Mascarenes_native_sims_stac4)  
nativeposs_sims_stac4_replicate_numbers <- which(Mascarenes_nativeposs_sims_stac4)



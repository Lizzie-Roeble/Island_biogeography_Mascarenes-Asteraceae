######################
# DAISIE: 
# Goodness-of-fit     
######################

# Load required libraries
library(DAISIE)
library(ggplot2)
library(patchwork)

# Load simulations: 1000 simulations filtered on condition of >= 1 nonendemic
load("../outputs/Models/Mascarenes_native_sims_filt_1000.RData")
load("../outputs/Models/Mascarenes_nativeposs_sims_filt_1000.RData")

island_replicates_native <- Mascarenes_native_sims_filt_1000
island_replicates_nativeposs <- Mascarenes_nativeposs_sims_filt_1000


# ------- Observed diversity values -------

# Native only
number_species_native <-  60
colonisations_native <-  15
endemic_species_native <-  58
non_endemic_species_native <-  2
endemic_clades_native <- 6 # cladogenetic clades / radiations
endemic_singletons_native <- 7  # also known as anagenetic spp
size_largest_clade_native <-  14

# Native + Possibly native
number_species_nativeposs <-  66
colonisations_nativeposs <-  21
endemic_species_nativeposs <-  59
non_endemic_species_nativeposs <-  7
endemic_clades_nativeposs <-  6 # cladogenetic clades / radiations
endemic_singletons_nativeposs <-  8 # also known as anagenetic spp
size_largest_clade_nativeposs <-  14

# ------- Function to process simulation results -------

process_simulation <- function(island_replicates) {
  replicates <- length(island_replicates)
  
  # Initialize vectors
  number_colonists <- numeric(replicates)
  number_spec <- numeric(replicates)
  number_stac2 <- numeric(replicates)
  number_stac3 <- numeric(replicates)
  number_stac4 <- numeric(replicates)
  number_colonisations_including_stac3s <- numeric(replicates)
  number_stac2_singletons <- numeric(replicates)
  size_largest_clade <- numeric(replicates)
  size_smallest_clade <- numeric(replicates)
  
  size_largest_clade[] <- NA
  size_smallest_clade[] <- NA
  
  for(i in 1:replicates) {
    the_island <- island_replicates[[i]]
    number_colonists[i] <- the_island[[1]]$stt_all[nrow(the_island[[1]]$stt_all), "present"]
    
    if(number_colonists[i] == 0) next
    
    u_island <- unlist(the_island)
    number_spec[i] <- sum(the_island[[1]]$stt_all[nrow(the_island[[1]]$stt_all), c(2,3,4)])
    number_stac2[i] <- sum(u_island[names(u_island) == "stac"] == 2)
    number_stac3[i] <- sum(u_island[names(u_island) == "stac"] == 3)
    number_stac4[i] <- the_island[[1]]$stt_all[nrow(the_island[[1]]$stt_all), 2]
    
    if(number_stac2[i] > 0) {
      store_stac2s_brts <- vector("list", number_stac2[i])
      store_all_brts <- vector("list", length(the_island) - 1)
      stac2_counter <- 0
      all_counter <- 0
      for (isl in 1:(length(the_island) - 1)) {
        isl_data <- the_island[[isl + 1]]
        all_counter <- all_counter + 1
        store_all_brts[[all_counter]] <- isl_data$branching_times
        if(isl_data$stac == 2) {
          stac2_counter <- stac2_counter + 1
          store_stac2s_brts[[stac2_counter]] <- isl_data$branching_times
        }
      }
      if(stac2_counter < length(store_stac2s_brts)) store_stac2s_brts <- store_stac2s_brts[1:stac2_counter]
      if(all_counter < length(store_all_brts)) store_all_brts <- store_all_brts[1:all_counter]
      stac2_lengths <- lengths(store_stac2s_brts)
      number_stac2_singletons[i] <- sum(stac2_lengths == 2)
      size_largest_clade[i] <- max(stac2_lengths) - 1
      size_smallest_clade[i] <- min(lengths(store_all_brts)) - 1
    }
    
    number_colonisations_including_stac3s[i] <- number_colonists[i]
    if(number_stac3[i] > 0) {
      count_stac3_cols <- 0
      for (isl in 1:(length(the_island) - 1)) {
        if(the_island[[isl + 1]]$stac == 3) {
          count_stac3_cols <- count_stac3_cols + length(the_island[[isl + 1]]$all_colonisations) - 1
        }
      }
      number_colonisations_including_stac3s[i] <- number_colonisations_including_stac3s[i] + count_stac3_cols
    }
  }
  
  return(list(
    number_spec = number_spec,
    number_colonists = number_colonists,
    number_stac2 = number_stac2,
    number_stac3 = number_stac3,
    number_stac4 = number_stac4,
    number_stac2_singletons = number_stac2_singletons,
    number_colonisations_including_stac3s = number_colonisations_including_stac3s,
    number_endemic_species = number_spec - number_stac4,
    size_largest_clade = size_largest_clade,
    size_smallest_clade = size_smallest_clade
  ))
}

# ------- Process both Mascarenes datasets -------

results_native <- process_simulation(island_replicates_native)
results_nativeposs <- process_simulation(island_replicates_nativeposs)

# ------- Histogram of simulated vs observed -------

# Create individual plots for native
# Number of species (native)
native_species <- ggplot(data.frame(value = results_native$number_spec), aes(x = value)) +
  geom_histogram(fill = "grey", color = NA, alpha = 0.7, bins = 20) +
  geom_vline(xintercept = median(results_native$number_spec, na.rm = TRUE), color = "black", linewidth = 1) +
  geom_vline(xintercept = number_species_native, 
             color = "red", linewidth = 1) +
  labs(x = "Number of species", y = "Frequency") +
  theme_classic(base_family = "Gill Sans") +
  theme(plot.margin = margin(5, 5, 5, 5))
native_species

# Colonization events (native)
native_colonists <- ggplot(data.frame(value = results_native$number_colonists), aes(x = value)) +
  geom_histogram(fill = "grey", color = NA, alpha = 0.7, bins = 10) +
  geom_vline(xintercept = median(results_native$number_colonists, na.rm = TRUE), color = "black", linewidth = 1) +
  geom_vline(xintercept = colonisations_native, 
             color = "red", linewidth = 1) +
  labs(x = "Number of colonization events", y = "Frequency") +
  theme_classic(base_family = "Gill Sans") +
  theme(plot.margin = margin(5, 5, 5, 5))
native_colonists 

# Size of largest clade (native)
native_largest_clade <- ggplot(data.frame(value = results_native$size_largest_clade), aes(x = value)) +
  geom_histogram(fill = "grey", color = NA, alpha = 0.7, bins = 8) +
  geom_vline(xintercept = median(results_native$size_largest_clade, na.rm = TRUE), 
             color = "black", linewidth = 1) +
  geom_vline(xintercept = size_largest_clade_native, 
             color = "red", linewidth = 1) +
  labs(x = "Size of largest clade", y = "Frequency") +
  theme_classic(base_family = "Gill Sans") +
  theme(plot.margin = margin(5, 5, 5, 5))
native_largest_clade

# Endemic clades (native)
native_endemic_clades <- ggplot(data.frame(value = results_native$number_stac2), aes(x = value)) +
  geom_histogram(fill = "grey", color = NA, alpha = 0.7, bins = 12) +
  geom_vline(xintercept = median(results_native$number_stac2, na.rm = TRUE), color = "black", linewidth = 1) +
  geom_vline(xintercept = endemic_clades_native, 
             color = "red", linewidth = 1) +
  labs(x = "Number of endemic clades", y = "Frequency") +
  theme_classic(base_family = "Gill Sans") +
  theme(plot.margin = margin(5, 5, 5, 5))
native_endemic_clades

# Endemic singletons (native)
native_endemic_singletons <- ggplot(data.frame(value = results_native$number_stac2_singletons), aes(x = value)) +
  geom_histogram(fill = "grey", color = NA, alpha = 0.7, bins = 7) +
  geom_vline(xintercept = median(results_native$number_stac2_singletons, na.rm = TRUE), color = "black", linewidth = 1) +
  geom_vline(xintercept = endemic_singletons_native, 
             color = "red", linewidth = 1) +
  labs(x = "Endemic singletons", y = "Frequency") +
  scale_x_continuous(breaks = seq(0, 10, by = 2)) +
  theme_classic(base_family = "Gill Sans") +
  theme(plot.margin = margin(5, 5, 5, 5))
native_endemic_singletons

# Non-endemic species (native)
native_non_endemic <- ggplot(data.frame(value = results_native$number_stac4), aes(x = value)) +
  geom_histogram(fill = "grey", color = NA, alpha = 0.7, bins = 4) +
  geom_vline(xintercept = median(results_native$number_stac4, na.rm = TRUE), color = "black", linewidth = 1) +
  geom_vline(xintercept = non_endemic_species_native, 
             color = "red", linewidth = 1) +
  labs(x = "Number of non-endemic species", y = "Frequency") +
  theme_classic(base_family = "Gill Sans") +
  theme(plot.margin = margin(5, 5, 5, 5))
native_non_endemic

# Create individual plots for native and possibly native
# Number of species (nativeposs)
nativeposs_species <- ggplot(data.frame(value = results_nativeposs$number_spec), aes(x = value)) +
  geom_histogram(fill = "grey", color = NA, alpha = 0.7, bins = 20) +
  geom_vline(xintercept = median(results_nativeposs$number_spec, na.rm = TRUE), color = "black", linewidth = 1) +
  geom_vline(xintercept = number_species_nativeposs, 
             color = "red", linewidth = 1) +
  labs(x = "Number of species", y = "Frequency") +
  theme_classic(base_family = "Gill Sans") +
  theme(plot.margin = margin(5, 5, 5, 5))
nativeposs_species

# Colonization events (nativeposs)
nativeposs_colonists <- ggplot(data.frame(value = results_nativeposs$number_colonists), aes(x = value)) +
  geom_histogram(fill = "grey", color = NA, alpha = 0.7, bins = 10) +
  geom_vline(xintercept = median(results_nativeposs$number_colonists, na.rm = TRUE), color = "black", linewidth = 1) +
  geom_vline(xintercept = colonisations_nativeposs, 
             color = "red", linewidth = 1) +
  labs(x = "Number of colonization events", y = "Frequency") +
  theme_classic(base_family = "Gill Sans") +
  theme(plot.margin = margin(5, 5, 5, 5))
nativeposs_colonists 

# Size of largest clade (nativeposs)
nativeposs_largest_clade <- ggplot(data.frame(value = results_nativeposs$size_largest_clade), aes(x = value)) +
  geom_histogram(fill = "grey", color = NA, alpha = 0.7, bins = 8) +
  geom_vline(xintercept = median(results_nativeposs$size_largest_clade, na.rm = TRUE), 
             color = "black", linewidth = 1) +
  geom_vline(xintercept = size_largest_clade_nativeposs, 
             color = "red", linewidth = 1) +
  labs(x = "Size of largest clade", y = "Frequency") +
  theme_classic(base_family = "Gill Sans") +
  theme(plot.margin = margin(5, 5, 5, 5))
nativeposs_largest_clade

# Endemic clades (nativeposs)
nativeposs_endemic_clades <- ggplot(data.frame(value = results_native$number_stac2), aes(x = value)) +
  geom_histogram(fill = "grey", color = NA, alpha = 0.7, bins = 12) +
  geom_vline(xintercept = median(results_nativeposs$number_stac2, na.rm = TRUE), color = "black", linewidth = 1) +
  geom_vline(xintercept = endemic_clades_nativeposs, 
             color = "red", linewidth = 1) +
  labs(x = "Number of endemic clades", y = "Frequency") +
  theme_classic(base_family = "Gill Sans") +
  theme(plot.margin = margin(5, 5, 5, 5))
nativeposs_endemic_clades

# Endemic singletons (nativeposs)
nativeposs_endemic_singletons <- ggplot(data.frame(value = results_nativeposs$number_stac2_singletons), aes(x = value)) +
  geom_histogram(fill = "grey", color = NA, alpha = 0.7, bins = 6) +
  geom_vline(xintercept = median(results_nativeposs$number_stac2_singletons, na.rm = TRUE), color = "black", linewidth = 1) +
  geom_vline(xintercept = endemic_singletons_nativeposs, 
             color = "red", linewidth = 1) +
  labs(x = "Endemic singletons", y = "Frequency") +
  scale_x_continuous(breaks = seq(0, 10, by = 2)) +
  theme_classic(base_family = "Gill Sans") +
  theme(plot.margin = margin(5, 5, 5, 5))
nativeposs_endemic_singletons

# Non-endemic species (nativeposs)
nativeposs_non_endemic <- ggplot(data.frame(value = results_nativeposs$number_stac4), aes(x = value)) +
  geom_histogram(fill = "grey", color = NA, alpha = 0.7, bins = 6) +
  geom_vline(xintercept = median(results_nativeposs$number_stac4, na.rm = TRUE), color = "black", linewidth = 1) +
  geom_vline(xintercept = non_endemic_species_nativeposs, 
             color = "red", linewidth = 1) +
  labs(x = "Number of non-endemic species", y = "Frequency") +
  theme_classic(base_family = "Gill Sans") +
  theme(plot.margin = margin(5, 5, 5, 5))
nativeposs_non_endemic

# Combine
goodness_fit_native <- native_species + native_colonists + native_largest_clade + native_endemic_clades + native_endemic_singletons + native_non_endemic  +
  plot_annotation(
    title = "DAISIE Goodness-of-Fit",
    subtitle = "Native",
    theme = theme(
      plot.title = element_text(family = "Gill Sans", face = "bold", size = 16, hjust = 0.5),
      plot.subtitle = element_text(family = "Gill Sans", size = 16, hjust = 0.5)))
goodness_fit_native

goodness_fit_nativeposs <- nativeposs_species + nativeposs_colonists + nativeposs_largest_clade + nativeposs_endemic_clades + nativeposs_endemic_singletons + nativeposs_non_endemic +
  plot_annotation(
    title = "DAISIE Goodness-of-Fit",
    subtitle = "Native + Possibly Native",
    theme = theme(
      plot.title = element_text(family = "Gill Sans", face = "bold", size = 16, hjust = 0.5),
      plot.subtitle = element_text(family = "Gill Sans", size = 16, hjust = 0.5)))
goodness_fit_nativeposs

# Save the plots 
ggsave("../outputs/Models/DAISIE_goodness_of_fit_native.pdf", goodness_fit_native, 
       width = 11, height = 7, units = "in")
ggsave("../outputs/Models/DAISIE_goodness_of_fit_native.png", goodness_fit_native, 
       width = 11, height = 7, units = "in")

ggsave("../outputs/Models/DAISIE_goodness_of_fit_nativeposs.pdf", goodness_fit_nativeposs, 
       width = 11, height = 7, units = "in")
ggsave("../outputs/Models/DAISIE_goodness_of_fit_nativeposs.png", goodness_fit_nativeposs, 
       width = 11, height = 7, units = "in")

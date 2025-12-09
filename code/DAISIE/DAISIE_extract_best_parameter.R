######################
# DAISIE Models      #
# Mascarenes         #
# Extract best rep   #
######################

library(readxl)
library(writexl)
library(dplyr)

# 1. File paths
file_native <- "../outputs/Models/DAISIE_ML_results_native.xlsx"
file_nativeposs <- "../outputs/Models/DAISIE_ML_results_nativeposs.xlsx"
output_path <- "../outputs/Models/"

# 2. Load data
native_all <- read_excel(file_native)
nativeposs_all <- read_excel(file_nativeposs)

# 3. Find best replicate (highest log-likelihood) for each dataset
best_native <- native_all %>% 
  filter(loglik == max(loglik)) %>% 
  slice(1)
best_nativeposs <- nativeposs_all %>% 
  filter(loglik == max(loglik)) %>% 
  slice(1)

# 4. Create a simple comparison table with just parameter values
comparison_table <- data.frame(
  Parameter = c("Cladogenesis (clado)", "Extinction (mu)", "Carrying capacity (K)", 
                "Colonization (gamma)", "Anagenesis (ana)", "Number of parameters (n_pars)", 
                "Log-likelihood"),
  Native = c(best_native$clado, best_native$mu, best_native$K, best_native$gamma, 
             best_native$ana, best_native$n_pars, best_native$loglik),
  NativePoss = c(best_nativeposs$clado, best_nativeposs$mu, best_nativeposs$K, 
                 best_nativeposs$gamma, best_nativeposs$ana, best_nativeposs$n_pars, 
                 best_nativeposs$loglik)
)

# 5. Save table to Excel
write_xlsx(comparison_table, paste0(output_path, "DAISIE_Mascarenes_best_rep_parameters.xlsx"))



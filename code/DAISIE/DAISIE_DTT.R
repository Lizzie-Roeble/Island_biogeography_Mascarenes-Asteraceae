###############################
# DAISIE 
# Diversity Through Time (DTT)                           
###############################

# Load required libraries
library(DAISIE)
library(showtext)
font_add(family = "Gill Sans", regular = "/System/Library/Fonts/Supplemental/GillSans.ttc")
showtext_auto()

# Load simulations: 1000 simulations filtered on condition of >= 1 nonendemic
load("../outputs/Models/Mascarenes_native_sims_filt_1000.RData")
load("../outputs/Models/Mascarenes_nativeposs_sims_filt_1000.RData")
Mascarenes_native_sims_filt_1000 <- Mascarenes_native_sims_filt_1000
Mascarenes_nativeposs_sims_filt_1000 <- Mascarenes_nativeposs_sims_filt_1000

# Plot DTT for native
pdf(file = "../outputs/Models/Mascarenes_native_DTT_raw.pdf", width = 8, height = 8.5)
par(family = "Gill Sans")  
DAISIE_plot_sims(Mascarenes_native_sims_filt_1000)
dev.off()

# Plot DTT for native + possibly native
pdf(file = "../outputs/Models/Mascarenes_nativeposs_DTT_raw.pdf", width = 8, height = 8.5)
par(family = "Gill Sans")  
DAISIE_plot_sims(Mascarenes_nativeposs_sims_filt_1000)
dev.off()


######################################
# Estimating Mascarenres mainland pool
######################################

# Regions: Madagascar, Indian Ocean basin, Tropical Africa
# Continent: Africa (included Madagascar)
get_wgsrpd3_codes("Africa")
# Regions: Northeast Tropical Africa, East Tropical Africa, South Tropical Africa, Southern Africa, Western Indian Ocean, Indian Subcontinent?

# install World Checklist of Vascular Plants (WCVP) 
install.packages("rWCVP")
library(rWCVP)
library(rWCVPdata)

# African Continent
mainland_Afr_cklist <- wcvp_summary("Asteraceae", taxon_rank = "family", area = get_wgsrpd3_codes("Africa"), grouping_var = "family")
# total native: 5313

# East African Regions (lvl2)
mainland_eAfr_cklist <- wcvp_summary("Asteraceae", taxon_rank = "family", area = get_wgsrpd3_codes(c("Northeast Tropical Africa", "East Tropical Africa", "South Tropical Africa", "Southern Africa", "Western Indian Ocean")), grouping_var = "family")
# total native: 4108

# East African Regions + Indian Subcontinent (lvl2)
mainland_eAfrInd_cklist <- wcvp_summary("Asteraceae", taxon_rank = "family", area = get_wgsrpd3_codes(c("Northeast Tropical Africa", "East Tropical Africa", "South Tropical Africa", "Southern Africa", "Western Indian Ocean", "Indian Subcontinent")), grouping_var = "family")
# total native: 5343

# Madagascar (+ Indian Ocean Islands - careful need to subtract Mascarenes taxa)
mainland_Mad_cklist <- wcvp_summary("Asteraceae", taxon_rank = "family", area = get_wgsrpd3_codes(c("Western Indian Ocean")), grouping_var = "family")
# total native: 581

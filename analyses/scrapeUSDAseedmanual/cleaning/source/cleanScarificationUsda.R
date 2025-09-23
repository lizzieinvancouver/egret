## Started 10 Feb by Mao ##
## checked 26 July 2025 by Deirdre
d$scarifTypeGen <- d$pretreatment
d$scarifTypeSpe <- d$pretreatment
d$scarifTypeGen[!grepl("scarification|Scarification|H2SO4|Mech|Heat|water|Abrasion|Acid|acid", d$scarifTypeGen)] <- NA
d$scarifTypeSpe[!grepl("scarification|Scarification|H2SO4|Mech|Heat|water|Abrasion|Acid|acid", d$scarifTypeSpe)] <- NA

# Clean general scarification column
d$scarifTypeGen[which(grepl("H2SO4|Acid|acid", d$scarifTypeGen))] <- "chemical"
d$scarifTypeGen[which(grepl("water", d$scarifTypeGen))] <- "soaking"
d$scarifTypeGen[which(grepl("Mech|Abrasion|Nicking", d$scarifTypeGen))] <- "mechanical"
d$scarifTypeGen[which(d$pretreatment == "Heat")] <- "mechanical"
d$scarifTypeGen[which(d$pretreatment == "No scarification")] <- NA
d$scarifTypeGen[which(grepl("scarification|Scarification", d$scarifTypeGen))] <- "unknownScarification"
# Rubus spectabilis has general scarification, will find
d$scarifTypeGen[which(d$scarifTypeGen == "Scarification & 2 mon stratification")] <- "chemical"
d$scarifTypeGen[which(d$scarifTypeGen == "Scarification & 4 mon stratification")] <- "chemical"
d$scarifTypeGen[which(d$scarifTypeGen == "Scarification & 6 mon stratification")] <- "chemical"
d$scarifTypeGen[which(d$scarifTypeGen == "Nicking")] <- "mechanical"
d$scarifTypeGen[which(d$scarifTypeGen == "Hot water")] <- "soaking"

# Clean specific scarification column
d$scarifTypeSpe[which(d$scarifTypeSpe == "H2SO4")] <- "checmical - H2SO4"
d$scarifTypeSpe[which(d$scarifTypeSpe == "sulfuric acid")] <- "checmical - H2SO4"

d$scarifTypeSpe[which(grepl("Mech", d$scarifTypeSpe))] <- "mechanical - unknown"
d$scarifTypeSpe[which(d$scarifTypeSpe == "Heat")] <- "mechanical - heat"
d$scarifTypeSpe[which(d$pregermination_treatment_hot_water_soak_C == "77-100")] <- "soaking - 77°C-100°C hot water"
d$scarifTypeSpe[which(d$pregermination_treatment_hot_water_soak_C == "71-91")] <- "soaking - 71°C-91°C hot water"
d$scarifTypeSpe[which(d$pregermination_treatment_hot_water_soak_C == "90")] <- "soaking - 90°C hot water"
d$scarifTypeSpe[which(d$pregermination_treatment_hot_water_soak_C == "85")] <- "soaking - 85°C hot water"
d$scarifTypeSpe[which(d$pregermination_treatment_hot_water_soak_C == "80")] <- "soaking - 80°C hot water"
d$scarifTypeSpe[which(d$pregermination_treatment_hot_water_soak_C == "71")] <- "soaking - 71°C hot water"
d$scarifTypeSpe[which(d$pregermination_treatment_hot_water_soak_C == "120")] <- "soaking - 120°C hot water"
d$scarifTypeSpe[which(d$pregermination_treatment_hot_water_soak_C == "100")] <- "soaking - 100°C hot water"
d$scarifTypeSpe[which(d$pregermination_treatment_hot_water_soak_C == "70")] <- "soaking - 70°C hot water"
d$scarifTypeSpe[which(d$scarifTypeSpe == "Hot water")] <- "soaking - hot water"
d$scarifTypeSpe[which(d$scarifTypeSpe == "Warm water")] <- "soaking - warm water"
d$scarifTypeSpe[which(d$scarifTypeSpe == "Abrasion")] <- "mechanical - abrasion"
d$scarifTypeSpe[which(d$scarifTypeSpe == "Acid soak")] <- "chemical - acid"
d$scarifTypeSpe[which(d$scarifTypeSpe == "Stratification")] <- "unknowStratification"
d$scarifTypeSpe[which(d$scarifTypeSpe == "Acid scarification")] <- "checmical - acid"
d$scarifTypeSpe[which(d$scarifTypeSpe == "Scarification & 2-mon stratification")] <- "chemical - acid"
d$scarifTypeSpe[which(d$scarifTypeSpe == "Scarification & 4-mon stratification")] <- "chemical - acid"
d$scarifTypeSpe[which(d$scarifTypeSpe == "Scarification & 6-mon stratification")] <- "chemical - acid"
d$scarifTypeSpe[which(d$scarifTypeSpe == "Nicking")] <- "mechanical - nicking"
d$scarifTypeSpe[which(d$scarifTypeSpe == "No scarification")] <- NA

# page 1063: adding acid scarif
d$scarifTypeSpe[which(d$pdf_page_number == "1063" & d$species_name == "sitchensis" & d$cold_stratification_days == "90")] <- "chemical - H2SO4"
d$pregermination_treatment_time_minutes[which(d$pdf_page_number == "1063" & d$species_name == "sitchensis" & d$cold_stratification_days == "90")] <- "10"


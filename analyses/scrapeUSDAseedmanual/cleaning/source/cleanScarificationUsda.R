## Started 10 Feb by Mao ##
## checked 26 July 2025 by Deirdre

d$pretreatmentScarifTypeGen <- d$pretreatment
d$pretreatmentScarifTypeSpe <- d$pretreatment
d$pretreatmentScarifTypeGen[!grepl("scarification|Scarification|H2SO4|Mech|Heat|water|Abrasion|Acid|acid", d$pretreatmentScarifTypeGen)] <- NA
d$pretreatmentScarifTypeSpe[!grepl("scarification|Scarification|H2SO4|Mech|Heat|water|Abrasion|Acid|acid", d$pretreatmentScarifTypeSpe)] <- NA

# Clean general scarification column
d$pretreatmentScarifTypeGen[which(grepl("H2SO4|Acid|acid", d$pretreatmentScarifTypeGen))] <- "chemical"
d$pretreatmentScarifTypeGen[which(grepl("water", d$pretreatmentScarifTypeGen))] <- "soaking"
d$pretreatmentScarifTypeGen[which(grepl("Mech|Abrasion|Nicking", d$pretreatmentScarifTypeGen))] <- "mechanical"
d$pretreatmentScarifTypeGen[which(d$pretreatment == "Heat")] <- "mechanical"
d$pretreatmentScarifTypeGen[which(d$pretreatment == "No scarification")] <- NA
d$pretreatmentScarifTypeGen[which(grepl("scarification|Scarification", d$pretreatmentScarifTypeGen))] <- "unknownScarification"
# Rubus spectabilis has general scarification, will find
d$pretreatmentScarifTypeGen[which(d$pretreatmentScarifTypeGen == "Scarification & 2 mon stratification")] <- "chemical"
d$pretreatmentScarifTypeGen[which(d$pretreatmentScarifTypeGen == "Scarification & 4 mon stratification")] <- "chemical"
d$pretreatmentScarifTypeGen[which(d$pretreatmentScarifTypeGen == "Scarification & 6 mon stratification")] <- "chemical"
d$pretreatmentScarifTypeGen[which(d$pretreatmentScarifTypeGen == "Nicking")] <- "mechanical"

# Clean specific scarification column
d$pretreatmentScarifTypeSpe[which(d$pretreatmentScarifTypeSpe == "H2SO4")] <- "checmical - H2SO4"
d$pretreatmentScarifTypeSpe[which(d$pretreatmentScarifTypeSpe == "sulfuric acid")] <- "checmical - H2SO4"

d$pretreatmentScarifTypeSpe[which(grepl("Mech", d$pretreatmentScarifTypeSpe))] <- "mechanical - unknown"
d$pretreatmentScarifTypeSpe[which(d$pretreatmentScarifTypeSpe == "Heat")] <- "mechanical - heat"
d$pretreatmentScarifTypeSpe[which(d$pregermination_treatment_hot_water_soak_C == "77-100")] <- "soaking - 77°C-100°C hot water"
d$pretreatmentScarifTypeSpe[which(d$pregermination_treatment_hot_water_soak_C == "71-91")] <- "soaking - 71°C-91°C hot water"
d$pretreatmentScarifTypeSpe[which(d$pregermination_treatment_hot_water_soak_C == "90")] <- "soaking - 90°C hot water"
d$pretreatmentScarifTypeSpe[which(d$pregermination_treatment_hot_water_soak_C == "85")] <- "soaking - 85°C hot water"
d$pretreatmentScarifTypeSpe[which(d$pregermination_treatment_hot_water_soak_C == "80")] <- "soaking - 80°C hot water"
d$pretreatmentScarifTypeSpe[which(d$pregermination_treatment_hot_water_soak_C == "71")] <- "soaking - 71°C hot water"
d$pretreatmentScarifTypeSpe[which(d$pregermination_treatment_hot_water_soak_C == "120")] <- "soaking - 120°C hot water"
d$pretreatmentScarifTypeSpe[which(d$pregermination_treatment_hot_water_soak_C == "100")] <- "soaking - 100°C hot water"
d$pretreatmentScarifTypeSpe[which(d$pregermination_treatment_hot_water_soak_C == "70")] <- "soaking - 70°C hot water"
d$pretreatmentScarifTypeSpe[which(d$pretreatmentScarifTypeSpe == "Hot water")] <- "soaking - hot water"
d$pretreatmentScarifTypeSpe[which(d$pretreatmentScarifTypeSpe == "Warm water")] <- "soaking - warm water"
d$pretreatmentScarifTypeSpe[which(d$pretreatmentScarifTypeSpe == "Abrasion")] <- "mechanical - abrasion"
d$pretreatmentScarifTypeSpe[which(d$pretreatmentScarifTypeSpe == "Acid soak")] <- "chemical - acid"
d$pretreatmentScarifTypeSpe[which(d$pretreatmentScarifTypeSpe == "Stratification")] <- "unknowStratification"
d$pretreatmentScarifTypeSpe[which(d$pretreatmentScarifTypeSpe == "Acid scarification")] <- "checmical - acid"
d$pretreatmentScarifTypeSpe[which(d$pretreatmentScarifTypeSpe == "Scarification & 2-mon stratification")] <- "chemical - acid"
d$pretreatmentScarifTypeSpe[which(d$pretreatmentScarifTypeSpe == "Scarification & 4-mon stratification")] <- "chemical - acid"
d$pretreatmentScarifTypeSpe[which(d$pretreatmentScarifTypeSpe == "Scarification & 6-mon stratification")] <- "chemical - acid"
d$pretreatmentScarifTypeSpe[which(d$pretreatmentScarifTypeSpe == "Nicking")] <- "mechanical - nicking"
d$pretreatmentScarifTypeSpe[which(d$pretreatmentScarifTypeSpe == "No scarification")] <- NA

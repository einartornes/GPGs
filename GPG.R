
# Script for å kartlegge øremerket bistand til globale fellesgoder -----------------------------------------

# Basert på Development intiatives artikkel "Measuring aid to global public goods".
# URL: http://devinit.org/wp-content/uploads/2016/07/Measuring-aid-to-global-public-goods-GPGs-Discussion-paper-July-2016.pdf
# Metode: Kriteriene fra artikkelen for hva som regnes som globale fellesgoder er i egne exelark (hver rad er en kriteriekombinasjon):
# - Partnerkriterier: fra eget excelark
# - Sektorkriterier: fra eget excelark
# - Policymarkørkriterier: ikke eget excelark
# Merk: Norfund-kapitaliseringen er ikke med her.

# Laster inn pakker og bistandsdata ---------------------------------------

library(readxl)
library(writexl)
library(tidyverse)
library(noraid)

df <- noraid::oda

df <- df %>%
  filter(Year == 2019)

# GPG-variabel basert på sektorkriterier ---------------------------------------------------------

sektorkriterier <- read_excel(path = "Sektorkriterier.xlsx")

# Inkluderer sektorkriteriene i df. Merk at geografikriteriet gjenstår.

df <- left_join(df, sektorkriterier, by = c(
  "DAC Main sector (code+name)" = "DAC Main sector (code+name)",
  "DAC Sub sector (code+name)" = "DAC Sub sector (code+name)"))

# Justerer sektorkriteriet basert på geografikriteriet

df <- df %>%
  mutate(`Sector_GPG theme` = case_when(
    Sector_country_level_criteria == "Global unspecified only" & `Recipient country` == "Global Unspecified" ~ `Sector_GPG theme`,
    Sector_country_level_criteria == "Global/regional unspecified only" & `Income category` == "Unspecified" ~ `Sector_GPG theme`,
    Sector_country_level_criteria == "All" ~ `Sector_GPG theme`,
    TRUE ~ as.character(NA)))
                                    


# GPG-variabel basert på partnerkriterier --------------------------------------------------------

partnerkriterier <- read_excel(path = "Partnerkriterier.xlsx")

# Inkluderer partnerkriteriene i df. Merk at geografikriteriet gjenstår.

df <- left_join(df, partnerkriterier, by = c("Agreement partner" = "Agreement partner"))

df <- df %>%
  mutate(`Partner_GPG theme` = case_when(
    Partner_country_level_criteria == "Global unspecified only" & `Recipient country` == "Global Unspecified" ~ `Partner_GPG theme`,
    Partner_country_level_criteria == "Global/regional unspecified only" & `Income category` == "Unspecified" ~ `Partner_GPG theme`,
    Partner_country_level_criteria == "All" ~ `Partner_GPG theme`,
    TRUE ~ as.character(NA)))


# GPG-variabel basert på policymarkørkriterier ---------------------------------------------------

df <- df %>%
  mutate(PM_GPG_theme = case_when(
    `PM - Climate change mitigation` == "Main objective" ~ "Environment",
    `PM - Bio-diversity` == "Main objective" & `DAC Main sector (code+name)` == "410 - General environment protection" ~ "Environment",
    `PM - Desertification` == "Main objective" & `Recipient country` == "Global Unspecified" ~ "Environment",
    `PM - Research and experimental development` == "Main objective" & `Recipient country` == "Global Unspecified" ~ "Research",
    TRUE ~ as.character(NA)))


# Samlet GPG-variabel basert på sektor-, partner-, og poliycmarkørkriterier-----------------------

# Prioriterer GPG-tema i rekkefølge i tilfelle kriteriene har gitt ulik GPG-tema: 1. Sektor, 2. parter, 3. PM.
df <- df %>%
  mutate("GPG_final" = `Sector_GPG theme`) %>%
  mutate("GPG_final" = if_else(is.na(GPG_final), `Partner_GPG theme`, GPG_final)) %>%
  mutate("GPG_final" = if_else(is.na(GPG_final), PM_GPG_theme, GPG_final))


# Eksporterer til excel
write_xlsx(df, "GPG.xlsx")
  
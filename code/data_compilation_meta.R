# Data compilation for meta-analysis

# 0 - Load packages and data ----
library(here)
library(janitor)
library(aqp)
library(soilDB)
library(DBI)
library(RSQLite)
library(tidyverse)

coop_data <- read.csv(here("data_raw", "dsp4sh_soc_df_KAD.csv")) %>%
  clean_names()
updated_treatments <- read.csv(here("data_processed", "05_meta_df_alt.csv"))
prism <- read.csv(here("data_processed", "dsp4sh_soc_prism3.csv"))
dsp4sh6 <- dbConnect(SQLite(), here("data_raw", "dsp4sh6.db"))

# 1 - Dataframe of project information ----
# Clean up PRISM data for joining
prism_clean <- prism %>%
  select(DSP_Pedon_ID, pedon_x, pedon_y, PRISM_ppt_30yr_normal_800mM4_annual_bil, PRISM_tmean_30yr_normal_800mM5_annual_bil) %>%
  clean_names() %>%
  distinct() %>%
  rename(map = prism_ppt_30yr_normal_800m_m4_annual_bil,
         mat = prism_tmean_30yr_normal_800m_m5_annual_bil)

# Make dataframe of project overview with climate information from PRISM
project_dat <- coop_data %>%
  select(project, dsp_pedon_id, dsp_plot_id, label, trt, explanation, lu, till,  
         soil, soil_sampled_name, pedon_x, pedon_y) %>%
  left_join(prism_clean, by=c("dsp_pedon_id", "pedon_x", "pedon_y")) %>%
  distinct(dsp_pedon_id, .keep_all=TRUE)

# Include soil suborders
dsp4sh_soils <- project_dat %>%
  distinct(soil) %>%
  pull()
dsp4sh_osd <- fetchOSD(dsp4sh_soils)
dsp4sh_suborders <- site(dsp4sh_osd) %>%
  select(id, suborder) %>%
  mutate(id = str_to_title(id),
         suborder = str_to_title(suborder)) 

project_dat_soil <- project_dat %>%
  left_join(dsp4sh_suborders, by=c("soil" = "id")) %>%
  relocate(suborder, .after=soil)

write_csv(project_dat_soil, here("data_processed", "project_data_meta.csv"))

# Make reduced dataframe of site climate information for later joining
site_clim <- project_dat_soil %>%
  select(project, soil, suborder, dsp_pedon_id, pedon_x, pedon_y, mat, map)

# Fix Washington State, NC State, Minnesota respiration data units ----
# Washington State reported data in incorrect units
# From Katherine Naasko: "I assumed (wrongly) that the output from respiration calculation equation was mg CO2 per g, instead of mg CO2 per the whole (20 g) sample – my mistake. I then went on to convert my output in mg CO2 per g of soil across 4 days into “mg CO2 per kg soil per day” by multiplying the respiration value by (1000 g/ 1 kg) and then dividing by 4 (to get per day values). 

# NC State, University of Minnesota, Kansas State, and UConn all corrected results for soil mass - need to multiply by 20 g soil to get values back up to mg CO2 per trap.

# I strongly suspect that Illinois also corrected for soil mass (waiting on response from cooperators)

coop_data_washcorr <- coop_data %>%
  mutate(soil_respiration = case_when(project=="WashingtonState" ~ (soil_respiration / 250) *(4/3),
                                      project=="NCState" ~ soil_respiration*20,
                                      project=="UnivOfMinnesota" ~ soil_respiration*20,
                                      project=="UConn" ~ soil_respiration*20,
                                      project=="KansasState" ~ soil_respiration*20,
                                      .default=soil_respiration))

# 2 - Pull in texture and pH data from KSSL ----

# Convert tables into dataframes
db <- lapply(setNames(nm = dbListTables(dsp4sh6)), dbReadTable, conn = dsp4sh6)

# pull out KSSL data
kssl <- db$kssllabmst %>% 
  clean_names()

text_ph <- kssl %>%
  select(natural_key, clay_tot_psa, silt_tot_psa, sand_tot_psa, tex_psda, ph_h2o) %>%
  rename(kssl_labsampnum = natural_key)

# 3 - Attach climate data and KSSL to horizon data ----
# Attach climate data and clay% to horizon data
horizon_clim <- coop_data_washcorr %>%
  left_join(select(site_clim, dsp_pedon_id, project, soil, suborder, mat, map), 
            by=c("dsp_pedon_id", "project", "soil")) %>%
  left_join(text_ph, by="kssl_labsampnum") # join in KSSL data

write_csv(horizon_clim, here("data_processed", "soc_horizon_clim_meta.csv"))

# 4 - Dataframe of indicator data in surface horizons only ----
# Want average values across 0-10 cm (i.e. average 0-5 and 5-10 for each pedon) for all indicators
surf_all <- horizon_clim %>%
  filter(hrzdep_b == "5" | hrzdep_b=="10") %>%
  group_by(dsp_pedon_id) %>%
  summarize(across(c(soc_pct,bulk_density, tn_pct:yoder_agg_stab_mwd, p_h:ace, 
                     sand_pct, silt_pct, clay_pct, sand_tot_psa, silt_tot_psa, clay_tot_psa, ph_h2o), 
                   ~mean(.x, na.rm=TRUE))) %>%
  left_join(project_dat_soil, by="dsp_pedon_id") %>%
  mutate(clay_combined = ifelse(is.na(clay_pct), clay_tot_psa, clay_pct),
         clay_src = ifelse(is.na(clay_pct), 
                           ifelse(is.na(clay_tot_psa), "NA", "kssl"), "coop"),
         ph_combined = ifelse(is.na(p_h), ph_h2o, p_h),
         ph_src = ifelse(is.na(p_h), 
                         ifelse(is.na(ph_h2o), "NA", "kssl"), "coop")) %>%
  left_join(dplyr::select(updated_treatments, dsp_pedon_id, label, trt, lu), 
            by="dsp_pedon_id", suffix=c("_original", "_updated")) %>%
  mutate(label = ifelse(is.na(label_updated), label_original, label_updated),
         trt = ifelse(is.na(trt_updated), trt_original, trt_updated),
         lu = ifelse(is.na(lu_updated), lu_original, lu_updated))

ggplot(surf_all, aes(x=project, y=soil_respiration, fill=label)) +
  geom_boxplot() +
  theme_classic()

# Clean out some of the really high soil enzyme values, fix land use labels
surf <- surf_all %>%
  mutate(label = ifelse(dsp_pedon_id=="PaM2-1" | dsp_pedon_id=="PaM2-2", "Ref", label),
         alkaline_phosphatase = case_when(
           dsp_pedon_id == "WoCT3-2" ~ NA,
           dsp_pedon_id == "BAU9-1" ~ NA,
           .default = alkaline_phosphatase),
         bglucosaminidase = case_when(
           dsp_pedon_id =="BAU9-1" ~ NA,
           .default = bglucosaminidase)) %>%
  mutate(lu = ifelse(lu=="FORAGE" | lu=="RANGE", "GRASS", lu)) %>%
  dplyr::select(-label_original, -label_updated, -trt_original, -trt_updated, 
         -lu_original, -lu_updated) %>%
  relocate(project, dsp_pedon_id, dsp_plot_id, pedon_x, pedon_y, soil, soil_sampled_name, 
           suborder,label, trt, explanation, lu, till, mat, map)

write_csv(surf, here::here("data_processed", "surface_horizons_meta.csv"))

# What this data should be used for: analyzing sensitivity of indicators in surface horizons
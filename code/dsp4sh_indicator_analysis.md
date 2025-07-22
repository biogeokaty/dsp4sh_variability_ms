DSP4SH - Soil Health Indicator Variability and Management Sensitivity
================
Katy Dynarski
2025-7-22

# Project Background Information

## Maps and tables about DSP4SH projects

``` r
# Make dataframe for adding project data annotation to map, with just one point for each project label combination, text label describing project

# Get unique soil series
project_soil <- project %>%
  distinct(project, soil) %>%
  group_by(project) %>%
  mutate(count = paste("soil",seq(n()), sep="_")) %>%
  pivot_wider(names_from=count, values_from=soil) %>%
  unite("soils", soil_1:soil_3, sep=", ", na.rm=TRUE)

# Make annotation dataframe that includes project, soil, and xy coords
project_annotate <- project %>% 
  group_by(project, label) %>%
  mutate(avg_lat = mean(pedon_y, na.rm=TRUE),
         avg_long = mean(pedon_x, na.rm=TRUE)) %>%
  distinct(project, label, avg_lat, avg_long) %>%
  left_join(project_soil, by="project")

# Make a dataframe with only one point per project to make the map labels
project_labs <- project_annotate %>%
  group_by(project) %>%
  filter(label=="BAU") %>%
  mutate(name_long = case_when(project=="Illinois" ~ "University of Illinois",
                               project=="OregonState" ~ "Oregon State University",
                               project=="UConn" ~ "University of Connecticut",
                               project=="UTRGV" ~ "University of Texas - Rio Grande Valley",
                               project=="KansasState" ~ "Kansas State University",
                               project=="NCState" ~ "North Carolina State University",
                               project=="TexasA&MPt-1" ~ "Texas A&M - 1",
                               project=="TexasA&MPt-2" ~ "Texas A&M - 2",
                               project=="UnivOfMinnesota" ~ "University of Minnesota", 
                               project=="WashingtonState" ~ "Washington State University")
         )
```

Make map:

``` r
# Download map of USA from maps() and convert to sf
usa <- st_as_sf(maps::map("state", plot = FALSE, fill = TRUE))

# Make map
ggplot(data=usa) +
  geom_sf(fill=NA) +
  coord_sf(xlim=c(-125.0, -66.93457), ylim=c(23.5, 49.384358)) + # set bounding box around CONUS
  annotation_north_arrow(location="bl", which_north="true", height=unit(.25, "in"), width=unit(.25, "in"),
                         pad_x = unit(0.4, "in"), pad_y = unit(0.25, "in"), style=north_arrow_fancy_orienteering) + # add north arrow
  annotation_scale(location = "bl") +
  geom_point(data=project_annotate, aes(x=avg_long, y=avg_lat, color=project)) +
  geom_label_repel(data=project_labs, aes(x=avg_long, y=avg_lat, label=name_long),
                   min.segment.length = 0, seed = 42, box.padding = 0.5) +
  scale_color_paletteer_d("rcartocolor::Safe") +
  theme_classic() +
  easy_remove_axes() +
  theme(legend.position="none")
```

    ## Scale on map varies by more than 10%, scale bar may be inaccurate

![](dsp4sh_indicator_analysis_files/figure-gfm/Figure%201%20map%20of%20DSP4SH%20sites-1.png)<!-- -->

``` r
# ggsave(here("figs", "ms_figs_alt", "fig1_project_map.png"),  width=8, height=5, units="in", dpi=500)
```

Generate overview table of project information:

``` r
# Average climate data - to be added in to project overview table
site_clim_sum <- project %>%
  group_by(project) %>%
  summarize(across(mat:map, ~ mean(.x, na.rm = TRUE))) %>%
  mutate(mat = round(mat, 1),
         map = round(map, 0))

project_table <- project_annotate %>%
  group_by(project) %>%
  dplyr::select(project, soils, label) %>%
  ungroup() %>%
  left_join(site_clim_sum, by="project") %>%
  relocate(label, .after=map) %>%
  arrange(factor(project, levels=project_plotting_order), label)
flextable(project_table)
```

<img src="dsp4sh_indicator_analysis_files/figure-gfm/Table 1 dsp4sh project overview table-1.png" width="913" />

``` r
# write_csv(project_table, here("figs", "ms_figs_alt", "table1_project_table.csv"))
```

Table with more detail on each project treatment:

``` r
treatment_table <- surf %>%
  dplyr::select(project, label, lu, trt, dsp_plot_id) %>%
  distinct() %>%
  group_by(project, label, lu, trt) %>%
  count() %>%
  rename("n_plots" = "n") %>%
  arrange(project)
# write_csv(treatment_table, here("figs", "ms_figs", "tablesupp1_project_treatment_table.csv"))
```

## Environmental data tables

Table of range in environmental conditions - this is not for a table but
is referenced directly in the text:

``` r
site_range_table <- project %>%
  summarize(across(where(is.numeric), min_max)) %>%
  mutate(across(where(is.numeric), ~round(.x, 2))) %>%
  transmute(lat_range= paste(pedon_y_min, pedon_y_max, sep="-"),
         long_range = paste(pedon_x_min, pedon_x_max, sep="-"),
         mat_range = paste(mat_min, mat_max, sep="-"),
         map_range = paste(map_min, map_max, sep="-")) %>%
  pivot_longer(everything())
flextable(site_range_table)
```

<img src="dsp4sh_indicator_analysis_files/figure-gfm/range of environmental characteristics across all sites-1.png" width="410" />

Range in indicator values across all sites:

``` r
indicator_range_table <- surf_long %>%
  group_by(indicator) %>%
  summarize(across(where(is.numeric), min_max)) %>%
  mutate(across(where(is.numeric), ~round(.x, 2)))
flextable(indicator_range_table)
```

<img src="dsp4sh_indicator_analysis_files/figure-gfm/range in indicator values across all sites-1.png" width="1893" />

Range in indicator values in reference sites only:

``` r
indicator_range_table_ref <- surf_long %>%
  filter(label=="Ref") %>%
  group_by(indicator) %>%
  summarize(across(where(is.numeric), min_max)) %>%
  mutate(across(where(is.numeric), ~round(.x, 2)))
flextable(indicator_range_table_ref)
```

<img src="dsp4sh_indicator_analysis_files/figure-gfm/range in indicator values in reference sites only-1.png" width="1893" />

``` r
# How many projects had clay data?
clay_n <- surf %>%
  group_by(project, label) %>%
  dplyr::select(project, label, clay_combined) %>%
  na.omit() %>%
  count()

# Calculate site mean clay%
site_clay <- surf %>%
  group_by(project, label) %>%
  summarize(clay_mean = mean(clay_combined, na.rm=TRUE))
```

    ## `summarise()` has grouped output by 'project'. You can override using the
    ## `.groups` argument.

``` r
clay_min_max <- site_clay %>%
  ungroup() %>%
  na.omit() %>%
  summarize(across(where(is.numeric), min_max)) %>%
  mutate(across(where(is.numeric), ~round(.x, 2)))
flextable(clay_min_max)
```

<img src="dsp4sh_indicator_analysis_files/figure-gfm/range in surface soil clay content and bulk density-1.png" width="492" />

``` r
site_bd <- surf %>%
  group_by(project, label) %>%
  summarize(bd_mean = mean(bulk_density, na.rm=TRUE)) %>%
  arrange(bd_mean)
```

    ## `summarise()` has grouped output by 'project'. You can override using the
    ## `.groups` argument.

``` r
bd_min_max <- site_bd %>%
  ungroup() %>%
  na.omit() %>%
  summarize(across(where(is.numeric), min_max)) %>%
  mutate(across(where(is.numeric), ~round(.x, 2)))
flextable(bd_min_max)
```

<img src="dsp4sh_indicator_analysis_files/figure-gfm/range in surface soil clay content and bulk density-2.png" width="451" />

# Analysis of soil health indicator variability across DSP4SH projects

For most of the analyses here, I will be looking at indicator values in
the 0-10 cm depth increment only (0-5 cm and 5-10 cm values were
averaged to get 0-10 cm values).

## Summary tables of indicator values

Mean indicator values in Reference, SHM, and BAU systems:

``` r
indicator_summary_wide <- surf_long %>%
  group_by(project, label, indicator) %>%
  summarize(across(value, mean_sd_cv)) %>%
  na.omit() %>%
  unite("summary", value_mean:value_sd)  %>%
  pivot_wider(names_from=indicator, values_from=summary:value_cv) %>%
  arrange(factor(project, levels=project_plotting_order), label) %>%
  relocate(project, label, summary_bulk_density, summary_kssl_wsa, summary_yoder_agg_stab_mwd, summary_soc_pct, summary_ace, summary_pox_c, summary_soil_respiration, summary_bglucosidase, summary_bglucosaminidase, summary_acid_phosphatase, summary_alkaline_phosphatase, summary_arylsulfatase)
```

    ## `summarise()` has grouped output by 'project', 'label'. You can override using
    ## the `.groups` argument.

``` r
flextable(indicator_summary_wide)
```

<img src="dsp4sh_indicator_analysis_files/figure-gfm/Table S1 mean indicator values in all systems-1.png" width="8789" />

``` r
# write_csv(indicator_summary_wide, here("figs", "ms_figs_alt", "tablesupp1_indicator_summary.csv"))
```

Range in mean indicator values across projects (which indicators were
the most variable?):

``` r
indicator_mean_range <- surf_long %>%
  group_by(project, label, indicator) %>%
  summarize(across(value, mean_sd)) %>%
  na.omit() %>%
  ungroup() %>%
  group_by(indicator) %>%
  summarize(across(value_mean, min_max)) %>%
  mutate(fold = value_mean_max/value_mean_min)
```

    ## `summarise()` has grouped output by 'project', 'label'. You can override using
    ## the `.groups` argument.

``` r
flextable(indicator_mean_range)
```

<img src="dsp4sh_indicator_analysis_files/figure-gfm/range in mean indicator values across projects-1.png" width="1047" />

Calculate the CV for overall dataset (group only by indicator, not by
project or treatment):

``` r
indicator_cv_range <- surf_long %>%
  group_by(indicator, label) %>%
  summarize(across(value, mean_sd_cv)) %>%
  mutate(indicator=factor(indicator, levels=indicator_plotting_order),
         value_cv=round(value_cv, 0)) %>%
  arrange(indicator)
```

    ## `summarise()` has grouped output by 'indicator'. You can override using the
    ## `.groups` argument.

``` r
flextable(indicator_cv_range)
```

<img src="dsp4sh_indicator_analysis_files/figure-gfm/cv for overall dataset grouped by indicator-1.png" width="972" />

``` r
# write_csv(indicator_cv_range, here("figs", "ms_figs_alt", "table2_cv_range.csv"))
```

## Use a correlation analysis to look at the structure and covariance of the soil health metrics

``` r
indicators_only <- surf %>%
  dplyr::select(bulk_density, soc_pct, kssl_wsa:yoder_agg_stab_mwd, soil_respiration:acid_phosphatase, arylsulfatase:ace)
indicators_normalized <- scale(indicators_only)

# Plot correlation matrix - full version
corr_matrix <- cor(indicators_normalized, use="pairwise.complete.obs")
corr_pmat <- ggcorrplot::cor_pmat(indicators_normalized)
ggcorrplot(corr_matrix, p.mat=corr_pmat, hc.order=TRUE, type="lower", 
           lab=TRUE, lab_size= 8/.pt, insig="blank",
           colors = c("#6D9EC1", "white", "#E46726")) +
  scale_x_discrete(labels=indicator_labs) +
  scale_y_discrete(labels=indicator_labs) +
  theme_katy() +
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        axis.title=element_blank())
```

![](dsp4sh_indicator_analysis_files/figure-gfm/Figure%20S1%20correlation%20matrix-1.png)<!-- -->

``` r
# ggsave(here("figs", "ms_figs_alt", "figsupp1_surface_indicator_corrplot.png"), width=140, height=100, units="mm", dpi=500)
```

## Principal Components Analysis of indicator data

There are a lot of missing values in the indicator data because not
every metric was measured by every cooperator. PCA doesn’t handle
missing values, so I used the missMDA package to impute missing data.
The package imputes missing values so that the imputed values have no
weight on the results (i.e. just allows the analysis to be completed).

Impute missing data and run PCA of soil health indicators:

``` r
nb <- estim_ncpPCA(indicators_only,method.cv = "Kfold", verbose = FALSE) # estimate the number of components from incomplete data
nb$ncp
```

    ## [1] 5

``` r
res.comp <- imputePCA(indicators_only, ncp = nb$ncp)
imp_ind <- res.comp$completeObs
imp_pca <- prcomp(imp_ind, scale.=TRUE)
summary(imp_pca)
```

    ## Importance of components:
    ##                           PC1    PC2    PC3     PC4     PC5     PC6     PC7
    ## Standard deviation     1.9815 1.5062 1.3019 1.06708 0.95656 0.71308 0.61707
    ## Proportion of Variance 0.3272 0.1890 0.1412 0.09489 0.07625 0.04237 0.03173
    ## Cumulative Proportion  0.3272 0.5162 0.6575 0.75236 0.82861 0.87098 0.90271
    ##                            PC8     PC9    PC10    PC11    PC12
    ## Standard deviation     0.58952 0.57287 0.50191 0.40313 0.27806
    ## Proportion of Variance 0.02896 0.02735 0.02099 0.01354 0.00644
    ## Cumulative Proportion  0.93167 0.95902 0.98001 0.99356 1.00000

Plot PCA results:

``` r
# plot PCA biplot colored by soil series
pca_soil <- autoplot(imp_pca, data=surf, colour="soil", size=2, alpha=0.7,
                     loadings.label = TRUE, loadings.label.size = 3,
                     loadings.label.colour = 'black', loadings.color="black", 
                     loadings.label.repel=TRUE) +
  scale_color_paletteer_d("palettetown::deoxys", name="Soil Series") +
  stat_ellipse(aes(color=soil))+
  geom_hline(yintercept=0, linetype="dashed") +
  geom_vline(xintercept=0, linetype="dashed") +
  theme_classic()
pca_soil
```

![](dsp4sh_indicator_analysis_files/figure-gfm/plot%20pca%20results-1.png)<!-- -->

``` r
# plot PCA biplot colored by suborder
pca_suborder <- autoplot(imp_pca, data=surf, colour="suborder", size=2, alpha=0.7,
                     loadings.label = TRUE, loadings.label.size = 3,
                     loadings.label.colour = 'black', loadings.color="black", 
                     loadings.label.repel=TRUE) +
  scale_color_paletteer_d("palettetown::deoxys", name="Soil Suborder") +
    stat_ellipse(aes(color=suborder))+
  geom_hline(yintercept=0, linetype="dashed") +
  geom_vline(xintercept=0, linetype="dashed") +
  theme_classic()
pca_suborder
```

![](dsp4sh_indicator_analysis_files/figure-gfm/plot%20pca%20results-2.png)<!-- -->

``` r
# is suborder the same as project
pca_project <- autoplot(imp_pca, data=surf, colour="project", size=2, alpha=0.7,
                     loadings.label = TRUE, loadings.label.size = 3,
                     loadings.label.colour = 'black', loadings.color="black", 
                     loadings.label.repel=TRUE) +
  scale_color_paletteer_d("palettetown::deoxys", name="Project") +
    stat_ellipse(aes(color=project))+
  geom_hline(yintercept=0, linetype="dashed") +
  geom_vline(xintercept=0, linetype="dashed") +
  theme_classic()
pca_project
```

![](dsp4sh_indicator_analysis_files/figure-gfm/plot%20pca%20results-3.png)<!-- -->

``` r
# plot pca biplot colored by management
pca_mgmt <- autoplot(imp_pca, data=surf, colour="label", size=2, alpha=0.7,
                     loadings.label = TRUE, loadings.label.size = 3,
                     loadings.label.colour = 'black', loadings.color="black", 
                     loadings.label.repel=TRUE) +
  scale_color_manual(values=c("#FED789FF","#72874EFF","#476F84FF"),
                     breaks=c("BAU", "SHM", "Ref"), 
                     name="Management") +
  stat_ellipse(aes(color=label))+
  geom_hline(yintercept=0, linetype="dashed") +
  geom_vline(xintercept=0, linetype="dashed") +
  theme_classic()
pca_mgmt   
```

![](dsp4sh_indicator_analysis_files/figure-gfm/plot%20pca%20results-4.png)<!-- -->

``` r
# Use adonis2() function in vegan package
permanova_suborder <- adonis2(imp_ind ~ suborder, data=surf, method="eu") %>%
  broom::tidy() %>%
  mutate(model = "soil_suborder")
```

    ## Warning: The column names SumOfSqs and R2 in ANOVA output were not recognized or
    ## transformed.

``` r
permanova_soil <- adonis2(imp_ind ~ soil, data=surf, method="eu") %>%
  broom::tidy()
```

    ## Warning: The column names SumOfSqs and R2 in ANOVA output were not recognized or
    ## transformed.

``` r
permanova_project <- adonis2(imp_ind ~ project, data=surf, method="eu") %>%
  broom::tidy()
```

    ## Warning: The column names SumOfSqs and R2 in ANOVA output were not recognized or
    ## transformed.

``` r
permanova_label <- adonis2(imp_ind ~ label, data=surf, method="eu") %>%
  broom::tidy() %>%
  mutate(model = "treatment")
```

    ## Warning: The column names SumOfSqs and R2 in ANOVA output were not recognized or
    ## transformed.

``` r
permanova_table <- bind_rows(permanova_suborder, permanova_label) %>%
  rename(sum_sq = "SumOfSqs",
         r2 = "R2") %>%
  relocate(model, .before = term) %>%
  mutate(r2 = round(r2, 2),
         statistic = round(statistic, 1))
flextable(permanova_table)
```

<img src="dsp4sh_indicator_analysis_files/figure-gfm/permanova-1.png" width="1141" />

``` r
# write_csv(permanova_table, here("figs", "ms_figs_alt", "tablesupp2_permanova_table.csv"))
```

Clustering is significant by soil series, suborder, and management.
Soils explain more variability than management.

``` r
# Put soil and management groupings together in a grid
pca_suborder / pca_mgmt + 
  plot_annotation(tag_levels = 'A')  & 
  theme(plot.tag = element_text(face="bold"))
```

![](dsp4sh_indicator_analysis_files/figure-gfm/Figure%202%20PCA-1.png)<!-- -->

``` r
# ggsave(here::here("figs", "ms_figs_alt","fig2_indicator_pca_grid.png"), width=6.5, height=10, units="in", dpi=500)
# ggsave(here::here("figs", "ms_figs_alt","fig2.pdf"), width=6.5, height=10, units="in", dpi=1000)
```

## Identify environmental covariates of indicators in reference sites using glmulti and feature importance

Select best model predicting each indicator at reference sites only
using glmulti.

``` r
multi_ref <- surf_long %>%
  filter(label=="Ref") %>%
  group_by(indicator) %>%
  nest() %>%
  mutate(multi_obj = purrr::map(data, ~glmulti("value", c("mat", "map", "lu", "clay_combined"), data=.x,
                                      level = 1, maxsize = 4, confsetsize = 128))) %>%
  mutate(weight = purrr::map(multi_obj, ~(weightable(.x) %>% slice_head(n=1)))) %>%
  transmute(indicator, data, weight) %>%
  unnest(cols = c(weight))
```

    ## Initialization...
    ## TASK: Exhaustive screening of candidate set.
    ## Fitting...
    ## Completed.
    ## Initialization...
    ## TASK: Exhaustive screening of candidate set.
    ## Fitting...
    ## Completed.
    ## Initialization...
    ## TASK: Exhaustive screening of candidate set.
    ## Fitting...
    ## Completed.
    ## Initialization...
    ## TASK: Exhaustive screening of candidate set.
    ## Fitting...
    ## Completed.
    ## Initialization...
    ## TASK: Exhaustive screening of candidate set.
    ## Fitting...
    ## Completed.
    ## Initialization...
    ## TASK: Exhaustive screening of candidate set.
    ## Fitting...
    ## Completed.
    ## Initialization...
    ## TASK: Exhaustive screening of candidate set.
    ## Fitting...
    ## Completed.
    ## Initialization...
    ## TASK: Exhaustive screening of candidate set.
    ## Fitting...
    ## Completed.
    ## Initialization...
    ## TASK: Exhaustive screening of candidate set.
    ## Fitting...
    ## Completed.
    ## Initialization...
    ## TASK: Exhaustive screening of candidate set.
    ## Fitting...
    ## Completed.
    ## Initialization...
    ## TASK: Exhaustive screening of candidate set.
    ## Fitting...
    ## Completed.
    ## Initialization...
    ## TASK: Exhaustive screening of candidate set.
    ## Fitting...
    ## Completed.

``` r
# Calculate linear models
multi_ref_lm <- multi_ref %>%
  mutate(lm_obj = purrr::map(data, ~lm(model, data=.x)),
         lm_tidy = purrr::map(lm_obj, broom::glance),
         lm_coefs = purrr::map(lm_obj, broom::tidy))

# calculate feature importance for significant models
multi_ref_featimp <- multi_ref_lm %>%
  unnest(cols = c(lm_tidy)) %>%
  filter(p.value < 0.05) %>%
  mutate(vars = purrr::map(lm_obj, ~ .x$model %>% dplyr::select(-value)),
         dep_var = purrr::map(lm_obj, ~.x$model %>% dplyr::select(value)),
         mod_featimp = purrr::pmap(list(lm_obj, vars, dep_var),
                                    \(lm_obj, vars, dep_var) Predictor$new(lm_obj, data=vars, y=dep_var)),
         imp = purrr::map(mod_featimp, ~FeatureImp$new(.x, loss="mse", n.repetitions=1000)),
         imp_data = purrr::map(imp, ~.x$results)) 
```

Make top model for each indicator into a nice table:

``` r
multi_ref_table <- multi_ref_lm %>%
  ungroup() %>%
  transmute(indicator, model, lm_tidy) %>%
  unnest(cols = c(lm_tidy)) %>%
  filter(p.value < 0.05) %>%
  mutate(adj.r.squared = round(adj.r.squared, 2),
         p.value = ifelse(round(p.value,3)==0, "<0.001", round(p.value,3)),
         AIC = round(AIC, 0)) %>%
  arrange(desc(adj.r.squared)) %>%
  dplyr::select(indicator, model, adj.r.squared, p.value, AIC, nobs) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)),
         indicator_print = str_replace_all(indicator, indicator_labs))
flextable(multi_ref_table)
```

<img src="dsp4sh_indicator_analysis_files/figure-gfm/top models for each indicator - ref sites-1.png" width="1340" />

Pull out coefficients for all models to go in Table S2:

``` r
multi_ref_coef <- multi_ref_lm %>%
  ungroup() %>%
  select(indicator, model, lm_coefs) %>%
  unnest(cols=c(lm_coefs)) %>%
  filter(term!="(Intercept)") %>%
  select(indicator, model, term, estimate) %>%
  mutate(estimate = round(estimate, 1),
         sign = ifelse(estimate < 0, "negative", "positive"))
flextable(multi_ref_coef)
```

<img src="dsp4sh_indicator_analysis_files/figure-gfm/table s3 coefficients for all lms-1.png" width="1091" />

``` r
multi_ref_summary <- left_join(multi_ref_table, multi_ref_coef, by=c("indicator", "model")) %>%
  relocate(indicator_print, .before=indicator) %>%
  select(-indicator)
flextable(multi_ref_summary)
```

<img src="dsp4sh_indicator_analysis_files/figure-gfm/table s3 coefficients for all lms-2.png" width="1639" />

``` r
# write_csv(multi_ref_summary, here::here("figs", "ms_figs_alt", "tablesupp3_multi_ref_lm_coefs.csv"))
```

Pull out feature importance and plot:

``` r
multi_ref_featimp_df <- multi_ref_featimp %>%
  ungroup() %>%
  select(indicator, imp_data) %>%
  unnest(cols=c(imp_data)) %>%
  mutate(importance = round(importance, 1))

# Plot
var_fill_ref2 <- c("mat" = "#35978f",
                   "map" = "#80cdc1",
                   "lu" = "#dfc27d",
                   "clay_combined" = "#bf812d")

ggplot(multi_ref_featimp_df, aes(fill=feature, 
                                 y=importance, x=fct_relevel(indicator, rev(indicator_plotting_order)))) + 
  geom_bar(position="stack", stat="identity")+
  geom_text(aes(label = importance), size=2, position=position_stack(vjust = 0.5)) +
  labs(x="Indicator", y="Feature importance (mean squared error)") +
  scale_x_discrete(labels=indicator_labs) +
  scale_fill_manual(name="Predictor variable", 
                    values=var_fill_ref2,
                    labels=c("Soil Clay %", "Land Use", "MAP", "MAT"),
                    guide = guide_legend(reverse = TRUE)) +
  theme_katy() +
  coord_flip()
```

    ## Warning: 6 unknown levels in `f`: alkaline_phosphatase, acid_phosphatase,
    ## bglucosaminidase, bglucosidase, pox_c, and yoder_agg_stab_mwd
    ## 6 unknown levels in `f`: alkaline_phosphatase, acid_phosphatase,
    ## bglucosaminidase, bglucosidase, pox_c, and yoder_agg_stab_mwd

![](dsp4sh_indicator_analysis_files/figure-gfm/Figure%203%20feature%20importance-1.png)<!-- -->

``` r
# ggsave(here::here("figs", "ms_figs_alt","fig3_indicator_ref_mse.png"), height=100, width=140, units="mm", dpi=500)
# ggsave(here::here("figs", "ms_figs_alt","fig3.pdf"), height=100, width=140, units="mm", dpi=1000)
```

``` r
multi_ref_featimp_df %>%
  group_by(indicator) %>%
  arrange(indicator, desc(importance)) %>%
  slice_head(n=1) %>%
  flextable()
```

<img src="dsp4sh_indicator_analysis_files/figure-gfm/feature importance ranking-1.png" width="1270" />

# Use meta-analysis methods to determine indicator sensitivity to management across all projects

We’re going to treat the DSP4SH database like a meta-analysis and use
functions in the metafor() package to calculate the effect size of SHM
and Ref treatments on soil health indicators. This version of the
analysis uses data that is slightly altered from what cooperators
originally provided - the names of some treatments have been changed to
combine treatments that are reasonably similar for accurate effect size
calculation.

Changes:

- Kansas State Ref “Native Rangeland” treatments were combined into one

- NC State BAU (Wheat 1-3) and SHM (Hay 1-3) were recoded as just
  “wheat” and “hay” so each replicate wouldn’t be treated as a treatment

- All Oregon State no-till grass projects were coded to land use =
  “GRASS” (previously, two were coded as “CROP” and one was coded as
  “GRASS” leading to separation of these treatments)

- Texas A&M BAU and SHM treatment labels were condensed

- Washington State labels were condensed

## Calculate effect size for individual treatments and plot indicator effect sizes

Prep data for effect size calculation:

``` r
# Pivot data longer
meta_long <- surf %>%
  dplyr::select(project, dsp_pedon_id, soil, label, lu, till, trt,  
         soc_pct, bulk_density, kssl_wsa:yoder_agg_stab_mwd, 
         soil_respiration:acid_phosphatase, arylsulfatase:ace) %>%
  pivot_longer(soc_pct:ace, names_to="indicator", values_to="value")

# Make a new treatment variable with two values, BAU and ASP. ASP will contain all of the SHM and Ref soils.
meta_long2 <- meta_long %>%
  mutate(treat = ifelse(label=="BAU", "BAU", "ASP"))

# Calculate mean, sd, and n for BAU soils - need this for effect size calculation input
bau_means <- meta_long2 %>%
  filter(treat=="BAU") %>%
  filter(dsp_pedon_id!="BAU9-1") %>% # exclude pedon BAU9-1 from analysis - it's from WashingtonState study and has implausibly high enzyme activity
  group_by(project, indicator) %>%
  nest() %>%
  mutate(
    mean_bau = map_dbl(data, ~mean(.$value, na.rm = TRUE)),
    sd_bau = map_dbl(data, ~sd(.$value, na.rm = TRUE)),
    n_bau = map_dbl(data, ~sum(!is.na(.$value)))) %>%
  select(indicator, project, mean_bau, sd_bau, n_bau) %>%
  ungroup() 

# Calculate mean, sd, and n for Ref and SHM
nested2 <- meta_long2 %>%
  filter(treat=="ASP") %>%
  group_by(indicator, label, project, lu, till, trt) %>%
  nest() %>%
  mutate(
    mean_asp = map_dbl(data, ~mean(.$value, na.rm = TRUE)),
    sd_asp = map_dbl(data, ~sd(.$value, na.rm = TRUE)),
    n_asp = map_dbl(data, ~sum(!is.na(.$value)))) %>%
  select(indicator, label, project, lu, till, trt, mean_asp, sd_asp, n_asp) %>%
  ungroup()

# Put data together. This will be input for escalc() function to calculate effect sizes
es_in_big <- nested2 %>%
  left_join(bau_means, by=c("indicator", "project")) %>%
  na.omit()
```

Calculate effect sizes:

``` r
es_asp1 <- escalc(n1i = n_asp, n2i = n_bau, m1i = mean_asp, m2i = mean_bau, 
                  sd1i = sd_asp, sd2i = sd_bau, data = es_in_big, measure = "ROM")

# Average climate data - to be added in to project overview table
site_clim_sum <- project %>%
  group_by(project) %>%
  dplyr::summarize(across(mat:map, ~ mean(.x, na.rm = TRUE))) %>%
  mutate(mat = round(mat, 1),
         map = round(map, 0))

# Calculate site mean clay%
site_clay <- surf %>%
  group_by(project, label) %>%
  dplyr::summarize(clay_mean = mean(clay_combined, na.rm=TRUE))
```

    ## `summarise()` has grouped output by 'project'. You can override using the
    ## `.groups` argument.

``` r
# Join climate and clay% data to effect size data - these will be used as moderator variables for meta-regression later on
es_asp <- es_asp1 %>%
  na.omit() %>% # omit any rows with NA - this should just be places where NaNs were calculated
  unite("proj_trt", project, trt, remove = FALSE) %>%
  dplyr::rename(till_orig = till) %>%
  mutate(till = ifelse(till_orig == "Till", "No-till", till_orig)) %>% # fix Texas A&M data that was mislabeled - no-till treatment was coded as Till
  relocate(till, .before=till_orig) %>%
  left_join(site_clim_sum, by="project") %>% # add in climate data
  left_join(site_clay, by=c("project", "label")) # add in clay content
```

Once effect sizes are calculated, they can be used to calculate a random
effects model to determine significance of effect sizes.

``` r
# nest data by indicators and treatment contrasts, calculate random-effects models, extract data from model
es_asp_rma_nomod <- es_asp %>%
  group_by(indicator, label) %>%
  nest() %>%
  mutate(rma_obj = purrr::map(data, ~rma(yi, vi, slab = proj_trt, data=.x, method="REML"))) %>%
  mutate(rma_tidy = purrr::map(rma_obj, broom::tidy)) %>%
  # make dataframe with effect sizes and variances for individual studies
  mutate(study_df = purrr::map(rma_obj, ~data.frame(es = .x$yi,
                                             se= sqrt(.x$vi),
                                             type = "study",
                                             study=.x$slab))) %>%
  # make dataframe with overall random effects model estimate and standard error
  mutate(summary_df = purrr::map(rma_obj, ~data.frame(es = .x$b,
                                               se=.x$se,
                                               type= "summary",
                                               study="summary"))) %>%
  # put two dfs together into one that can be used to make a forest plot
  mutate(plot_df = purrr::map2(study_df, summary_df, rbind))
```

Generate forest plots for each indicator, and a table of which
indicators are significantly influenced by treatment:

``` r
# pull out plotting data
es_asp_rma_plot_df <- es_asp_rma_nomod %>%
  select(indicator, label, plot_df, rma_tidy) %>%
  ungroup() %>%
  transmute(indicator, label, plot_df, rma_tidy) %>%
  unnest(cols = c(plot_df, rma_tidy), names_sep = "_") %>%
  select(indicator, label, plot_df_es, plot_df_se, plot_df_type, plot_df_study, rma_tidy_p.value) %>%
  dplyr::rename(es = plot_df_es,
         se = plot_df_se,
         type = plot_df_type,
         study = plot_df_study,
         summary_pval = rma_tidy_p.value) %>%
  mutate(study = case_when(type=="summary" ~ ifelse(label=="SHM", "summary - SHM", "summary - Ref"),
                           type=="study" ~ study))

# make vectors of indicator/label combination
indicator_label2 <- es_asp_rma_plot_df %>% distinct(indicator, label)
indicators2 <- indicator_label2 %>% pull(indicator) %>% as.character
labels2 <- indicator_label2 %>% pull(label) %>% as.character

# Make nicer treatment labels
study_labs <- c(
  "KansasState_Native Rangeland" = "Kansas State - Native Rangeland",
  "NCState_forest" = "NC State - Forest",
  "OregonState_Forest" = "Oregon State - Forest",
  "OregonState_Hazelnut" = "Oregon State - Hazelnut Orchard",
  "UnivOfMinnesota_Native rangeland" = "Minnesota - Native Rangeland",
  "UTRGV_Reference" = "UTRGV - Native Treeline",
  "WashingtonState_PGL" = "Washington State - Perennial Grassland",
  "Illinois_Ref" = "Illinois - Restored Forest",
  "KansasState_No-Till  with cover crop -  corn, oats, or wheat." = "Kansas State - No Till Cover Crop",
  "KansasState_No-till - diverse rotation" = "Kansas State - No Till Diverse Rotation",
  "NCState_hay" = "NC State - Hay",
  "WashingtonState_ASP" = "Washington State - No Till Wheat",
  "UnivOfMinnesota_No till cover crop no fertilizer" = "Minnesota - No Till Cover Crop",
  "Illinois_NT" = "Illinois - No Till Corn-Soybean Rotation",
  "TexasA&MPt-1_No Till - Mixed" = "Texas A&M 1 - No Till Mixed Crops",
  "TexasA&MPt-1_No Till - Rye" = "Texas A&M 1 - No Till Rye",
  "TexasA&MPt-1_Rangland" = "Texas A&M 1 - Native Rangeland",
  "TexasA&MPt-2_no-till fallow after sorghum" = "Texas A&M 2 - No Till Sorghum with Fallow",
  "TexasA&MPt-2_no-till fallow after wheat" = "Texas A&M 2 - No Till Wheat with Fallow",
  "TexasA&MPt-2_no-till wheat" = "Texas A&M 2 - No Till Wheat",
  "TexasA&MPt-2_Native with introduced species" = "Texas A&M 2 - Perennial Forage",
  "TexasA&MPt-2_Native Rangeland" = "Texas A&M 2 - Native Rangeland",
  "OregonState_Vineyard" = "Oregon State - Vineyard",
  "OregonState_No-till" = "Oregon State - No Till Grass",
  "UConn_Hayland" = "UConn - Hayland",
  "UConn_Lawn" = "UConn - Lawn",
  "UConn_No-till silage corn" = "UConn - No Till Corn",
  "UConn_Managed forest" = "UConn - Managed Forest",
  "UConn_Unmanaged forest" = "UConn - Unmanaged Forest"
)

# make plots - should have the same overall effect sizes for each indicator, but more lines to show the different treatments
plot_list3 <- purrr::map(.x = indicators,
                  .f = ~{
                    es_asp_rma_plot_df %>% 
                      filter(indicator == .x) %>%
                      ggplot(aes(x=factor(study, levels=study),y=es,
                                 ymax=es+se,ymin=es-se,size=factor(type),colour=factor(type))) + 
                      geom_hline(yintercept=0, lty=2,linewidth=1) +
                      geom_pointrange(size=0.8, linewidth=0.8) +
                      coord_flip() + 
                      scale_size_manual(values=c(0.5,1)) + 
                      scale_x_discrete(labels=study_labs, limits=rev) +
                      facet_wrap(~label, scales="free", ncol=1) +
                      labs(x="Study", y="Log response ratio",
                           title=glue::glue({unique(filter(indicator_labs_df, indicator==.x)$label)})) + 
                      scale_colour_manual(values=c("grey","black")) + 
                      theme_katy() +
                      theme(legend.position="none")
                    
                  })

plot_list3
```

    ## [[1]]

![](dsp4sh_indicator_analysis_files/figure-gfm/forest%20plots%20and%20table%20of%20significant%20indicators-1.png)<!-- -->

    ## 
    ## [[2]]

![](dsp4sh_indicator_analysis_files/figure-gfm/forest%20plots%20and%20table%20of%20significant%20indicators-2.png)<!-- -->

    ## 
    ## [[3]]

![](dsp4sh_indicator_analysis_files/figure-gfm/forest%20plots%20and%20table%20of%20significant%20indicators-3.png)<!-- -->

    ## 
    ## [[4]]

![](dsp4sh_indicator_analysis_files/figure-gfm/forest%20plots%20and%20table%20of%20significant%20indicators-4.png)<!-- -->

    ## 
    ## [[5]]

![](dsp4sh_indicator_analysis_files/figure-gfm/forest%20plots%20and%20table%20of%20significant%20indicators-5.png)<!-- -->

    ## 
    ## [[6]]

![](dsp4sh_indicator_analysis_files/figure-gfm/forest%20plots%20and%20table%20of%20significant%20indicators-6.png)<!-- -->

    ## 
    ## [[7]]

![](dsp4sh_indicator_analysis_files/figure-gfm/forest%20plots%20and%20table%20of%20significant%20indicators-7.png)<!-- -->

    ## 
    ## [[8]]

![](dsp4sh_indicator_analysis_files/figure-gfm/forest%20plots%20and%20table%20of%20significant%20indicators-8.png)<!-- -->

    ## 
    ## [[9]]

![](dsp4sh_indicator_analysis_files/figure-gfm/forest%20plots%20and%20table%20of%20significant%20indicators-9.png)<!-- -->

    ## 
    ## [[10]]

![](dsp4sh_indicator_analysis_files/figure-gfm/forest%20plots%20and%20table%20of%20significant%20indicators-10.png)<!-- -->

    ## 
    ## [[11]]

![](dsp4sh_indicator_analysis_files/figure-gfm/forest%20plots%20and%20table%20of%20significant%20indicators-11.png)<!-- -->

    ## 
    ## [[12]]

![](dsp4sh_indicator_analysis_files/figure-gfm/forest%20plots%20and%20table%20of%20significant%20indicators-12.png)<!-- -->

``` r
# Save plots
# purrr::map(.x = indicators,
#                   .f = ~{
#                     es_asp_rma_plot_df %>% 
#                       filter(indicator == .x) %>%
#                       ggplot(aes(x=factor(study, levels=study),y=es,
#                                  ymax=es+se,ymin=es-se,size=factor(type),colour=factor(type))) + 
#                       geom_hline(yintercept=0, lty=2,linewidth=1) +
#                       geom_pointrange(size=0.8, linewidth=0.8) +
#                       coord_flip() + 
#                       scale_size_manual(values=c(0.5,1)) + 
#                       scale_x_discrete(labels=study_labs, limits=rev) +
#                       facet_wrap(~label, scales="free", ncol=1) +
#                       labs(x="Study", y="Log response ratio",
#                            title=glue::glue({unique(filter(indicator_labs_df, indicator==.x)$label)})) + 
#                       scale_colour_manual(values=c("grey","black")) + 
#                       theme_katy() +
#                       theme(legend.position="none")
#                     
#                     ggsave(here::here("figs","ms_figs_alt",glue::glue("figsupp_es_", .x,".png")), 
#                            width=120, height=160, units="mm", dpi=500)
#                     
#                   })
```

Plot the overall effect size for each indicator:

``` r
# make dataframe for plotting
es_rma_summary <- es_asp_rma_plot_df %>%
  filter(type=="summary") %>%
  filter(indicator!="soc_stock_100cm") %>%
  filter(indicator!="soc_stock_0_30cm") %>%
  mutate(sig = ifelse(summary_pval<0.05, "significant", "not significant")) %>%
  select(-study)

ggplot(es_rma_summary, aes(x=fct_relevel(indicator, rev(indicator_plotting_order)), 
                           y=es, ymax=es+se, ymin=es-se, color=label)) + 
  geom_pointrange(size=0.8, linewidth=0.8) +
  geom_text(aes(label=ifelse(sig=="significant", "*", "")), 
            color="black", position=position_nudge(x=0.25), size=3.5) +
  coord_flip() + 
  geom_hline(yintercept=0, lty=2,linewidth=0.8) +
  labs(x="Indicator", y="Log response ratio") +
  scale_x_discrete(labels=indicator_labs) +
  scale_color_manual(values=c("#72874EFF","#476F84FF"),
                     breaks=c("SHM", "Ref"), 
                    name="Management") +
  theme_katy()
```

![](dsp4sh_indicator_analysis_files/figure-gfm/Figure%204%20effect%20sizes-1.png)<!-- -->

``` r
# ggsave(here::here("figs", "ms_figs_alt", "fig4_indicator_effect_sizes.png"), width=140, height=100, units="mm", dpi=500)
# ggsave(here::here("figs", "ms_figs_alt", "fig4.pdf"), width=140, height=100, units="mm", dpi=1000)
```

Make table with n for interpretability:

``` r
es_rma_n_df <- es_asp_rma_nomod %>%
  select(indicator, label, plot_df) %>%
  filter(indicator!="soc_stock_0_30cm") %>%
  filter(indicator!="soc_stock_100cm") %>%
  ungroup() %>%
  transmute(indicator, label, plot_df) %>%
  unnest(cols = c(plot_df)) %>%
  dplyr::count(indicator, label, type) %>%
  filter(type=="study") %>%
  select(-type) %>%
  mutate(indicator=factor(indicator, levels=indicator_plotting_order)) %>%
  arrange(indicator, label)
flextable(es_rma_n_df)
```

<img src="dsp4sh_indicator_analysis_files/figure-gfm/table with observation n for random-effects models-1.png" width="628" />

``` r
rma_report_table <- es_asp_rma_nomod %>%
  mutate(report_df = purrr::map(rma_obj, ~data.frame(tau2 = .x$tau2,
                                              i2 = .x$I2,
                                              h2 = .x$H2,
                                              q_stat = .x$QE,
                                              q_pval = .x$QEp,
                                              k = .x$k))) %>%
  select(indicator, label, rma_tidy, report_df) %>%
  ungroup() %>%
  transmute(indicator, label, rma_tidy, report_df) %>%
  unnest(cols = c(rma_tidy, report_df)) %>%
  select(-term, -type, -statistic) %>%
  mutate(estimate = round(estimate, 2),
         std.error = round(std.error, 2),
         p.value = ifelse(round(p.value, 3) == 0, "<0.001", round(p.value, 3)),
         tau2 = round(tau2, 2),
         i2 = round(i2, 1),
         h2 = round(h2, 1),
         q_stat = round(q_stat, 1),
         q_pval = ifelse(round(q_pval, 3) == 0, "<0.001", round(q_pval, 3)),
         q_sig = ifelse(q_pval<0.05, "significant", "not significant"),
         indicator = factor(indicator, levels=indicator_plotting_order)) %>%
  arrange(indicator, label) %>%
  relocate(k, .after=label)
flextable(rma_report_table)
```

<img src="dsp4sh_indicator_analysis_files/figure-gfm/Table 2 summary of random effects models-1.png" width="1992" />

``` r
# write_csv(rma_report_table, here::here("figs", "ms_figs_alt","table3_rma_report_table.csv"))
```

## Calculate random-effects models with moderating variables

Our previous random-effects models only determined if indicators were
significantly influenced by treatment. However, those models didn’t
include any moderating variables and couldn’t tell us what variables
influenced the response of indicators to treatment. Some potential
moderating variables of interest are MAT, MAP, land use, and tillage.

### MAT as a moderator variable

Calculate random-effects model with MAT as a moderator variable,
generate table of results, and generate table of indicators for which
MAT is a significant moderator variable:

``` r
# Calculate random-effects model
es_asp_rma_mat <- es_asp %>%
  group_by(indicator) %>%
  nest() %>%
  mutate(rma_obj = purrr::map(data, ~rma(yi, vi, slab = project, mod = ~ mat, data=.x, method="REML"))) %>%
  mutate(rma_tidy = purrr::map(rma_obj, broom::tidy))

# Make table for reporting, also include QM test of moderators
mat_rma_report_table <- es_asp_rma_mat %>%
  mutate(report_df = purrr::map(rma_obj, ~data.frame(tau2 = .x$tau2,
                                              i2 = .x$I2,
                                              h2 = .x$H2,
                                              r2 = .x$R2,
                                              q_stat = .x$QE,
                                              q_pval = .x$QEp,
                                              q_mod = .x$QM,
                                              q_mod_pval = .x$QMp))) %>%
  select(indicator, rma_tidy, report_df) %>%
  ungroup() %>%
  transmute(indicator, rma_tidy, report_df) %>%
  unnest(cols = c(rma_tidy, report_df)) %>%
  filter(term != "intercept") %>%
  filter(indicator!="soc_stock_0_30cm") %>%
  filter(indicator!="soc_stock_100cm") %>%
  select(-term, -type) %>%
  mutate(q_sig = ifelse(q_pval<0.05, "significant", "not significant"),
         q_mod_sig = ifelse(q_mod_pval<0.05, "significant", "not significant"),
         estimate = round(estimate, 2),
         std.error = round(std.error, 2),
         statistic = round(statistic, 2),
         p.value = ifelse(round(p.value, 3) ==0, "<0.001", round(p.value, 3)),
         tau2 = round(tau2, 2),
         i2 = round(i2, 1),
         h2 = round(h2, 1),
         r2 = round(r2, 2),
         q_stat = round(q_stat, 1),
         q_pval = ifelse(round(q_pval, 3)==0, "<0.001", round(q_pval, 3)),
         q_mod = round(q_mod, 1),
         q_mod_pval = ifelse(round(q_mod_pval, 3)==0, "<0.001", round(q_mod_pval, 3)),
         indicator = factor(indicator, levels=indicator_plotting_order)) %>%
  arrange(indicator)
flextable(mat_rma_report_table)
```

<img src="dsp4sh_indicator_analysis_files/figure-gfm/random effects model with mat as a moderator variable-1.png" width="2513" />

``` r
# Extract significant moderator variables
mat_rma_report_table_sig <- mat_rma_report_table %>%
  filter(q_mod_sig=="significant") 
flextable(mat_rma_report_table_sig)
```

<img src="dsp4sh_indicator_analysis_files/figure-gfm/random effects model with mat as a moderator variable-2.png" width="2391" />

Plot relationship between MAT and indicator variables for which it is a
significant predictor:

``` r
# MAT - Significant for bglucosidase and arysulfatase

# Make vector of indicators 
mat_indicators <- c("bglucosidase","arylsulfatase")

# Plot with map function
mat_plots <- purrr::map(.x = mat_indicators,
                 .f = ~{
                   es_asp %>% 
                     filter(indicator == .x) %>%
                     ggplot(aes(x=mat, y=yi)) +
                     geom_point(aes(colour=label)) +
                     geom_smooth(method="lm", formula = y~x, color="black") +
                     geom_hline(yintercept=0, linetype="dashed") +
                     stat_cor(aes(label = paste(after_stat(rr.label), after_stat(p.label), sep = "~`,`~")),
                              size= 8/.pt) +
                     scale_colour_manual(values=c("#72874EFF","#476F84FF"),
                                         breaks=c("SHM", "Ref"),
                                         name="Management") +
                     labs(x=expression("Mean annual temperature"~(degree*C)), y="Log response ratio",
                          title=glue::glue({filter(indicator_labs_df, indicator==.x)$label})) + 
                     theme_katy()
                 })

# Now make into a panel grid
mat_bg <- pluck(mat_plots, 1)
mat_as <- pluck(mat_plots, 2)

mat_bg + mat_as + plot_layout(guides = 'collect', axes="collect") +
  plot_annotation(tag_levels = 'A')  & 
  theme(plot.tag = element_text(face="bold"))
```

![](dsp4sh_indicator_analysis_files/figure-gfm/Figure%20S14%20mat%20mod-1.png)<!-- -->

``` r
# ggsave(here::here("figs", "ms_figs_alt", "figsupp14_mat_mod_reg.png"), width=180, height=100, units="mm", dpi=500)
```

### MAP as a moderator variable

Calculate random-effects model with MAP as a moderator variable,
generate table of results, and generate table of indicators for which
MAP is a significant moderator variable:

``` r
# Calculate random-effects model
es_asp_rma_map <- es_asp %>%
  group_by(indicator) %>%
  nest() %>%
  mutate(rma_obj = purrr::map(data, ~rma(yi, vi, slab = project, mod = ~ map, data=.x, method="REML"))) %>%
  mutate(rma_tidy = purrr::map(rma_obj, broom::tidy))

# Make table for reporting, also include QM test of moderators
map_rma_report_table <- es_asp_rma_map %>%
  mutate(report_df = purrr::map(rma_obj, ~data.frame(tau2 = .x$tau2,
                                              i2 = .x$I2,
                                              h2 = .x$H2,
                                              r2 = .x$R2,
                                              q_stat = .x$QE,
                                              q_pval = .x$QEp,
                                              q_mod = .x$QM,
                                              q_mod_pval = .x$QMp))) %>%
  select(indicator, rma_tidy, report_df) %>%
  ungroup() %>%
  transmute(indicator, rma_tidy, report_df) %>%
  unnest(cols = c(rma_tidy, report_df)) %>%
  filter(term != "intercept") %>%
  filter(indicator!="soc_stock_0_30cm") %>%
  filter(indicator!="soc_stock_100cm") %>%
  select(-term, -type) %>%
  mutate(q_sig = ifelse(q_pval<0.05, "significant", "not significant"),
         q_mod_sig = ifelse(q_mod_pval<0.05, "significant", "not significant"),
         estimate = round(estimate, 2),
         std.error = round(std.error, 2),
         statistic = round(statistic, 2),
         p.value = ifelse(round(p.value, 3) ==0, "<0.001", round(p.value, 3)),
         tau2 = round(tau2, 2),
         i2 = round(i2, 1),
         h2 = round(h2, 1),
         r2 = round(r2, 2),
         q_stat = round(q_stat, 1),
         q_pval = ifelse(round(q_pval, 3)==0, "<0.001", round(q_pval, 3)),
         q_mod = round(q_mod, 1),
         q_mod_pval = ifelse(round(q_mod_pval, 3)==0, "<0.001", round(q_mod_pval, 3)),
         indicator = factor(indicator, levels=indicator_plotting_order)) %>%
  arrange(indicator)
flextable(map_rma_report_table)
```

<img src="dsp4sh_indicator_analysis_files/figure-gfm/random effects model with map as a moderator variable-1.png" width="2513" />

``` r
# Extract significant moderator variables
map_rma_report_table_sig <- map_rma_report_table %>%
  filter(q_mod_sig=="significant") 
flextable(map_rma_report_table_sig)
```

<img src="dsp4sh_indicator_analysis_files/figure-gfm/random effects model with map as a moderator variable-2.png" width="2508" />

Plot relationship between MAP and indicator variables for which it is a
significant predictor:

``` r
# MAP is significant for alkP and arysulfatase

# Make vector of indicators 
map_indicators <- c("bulk_density","alkaline_phosphatase","arylsulfatase")

# Plot with map function
map_plots <- purrr::map(.x = map_indicators,
                 .f = ~{
                   es_asp %>% 
                     filter(indicator == .x) %>%
                     ggplot(aes(x=map, y=yi)) +
                     geom_point(aes(colour=label)) +
                     geom_smooth(method="lm", formula = y~x, color="black") +
                     geom_hline(yintercept=0, linetype="dashed") +
                     stat_cor(aes(label = paste(after_stat(rr.label), after_stat(p.label), sep = "~`,`~")),
                              size= 8/.pt) +
                     scale_colour_manual(values=c("#72874EFF","#476F84FF"),
                                         breaks=c("SHM", "Ref"),
                                         name="Management") +
                     labs(x="Mean annual precipitation (mm)", y="Log response ratio",
                          title=glue::glue({filter(indicator_labs_df, indicator==.x)$label})) + 
                     theme_katy()
                 })

# Now make into a panel grid
map_bd <- pluck(map_plots, 1)
map_alkp <- pluck(map_plots, 2)
map_as <- pluck(map_plots, 3)

map_bd + map_alkp + map_as + plot_layout(guides = 'collect', axes="collect") +
  plot_annotation(tag_levels = 'A')  & 
  theme(plot.tag = element_text(face="bold"))
```

![](dsp4sh_indicator_analysis_files/figure-gfm/Figure%20S15%20map%20mod-1.png)<!-- -->

``` r
# ggsave(here::here("figs", "ms_figs_alt", "figsupp15_map_mod_reg.png"), width=180, height=100, units="mm", dpi=500)
```

### Land use as a moderator variable

Calculate random-effects model with land use as a moderator variable,
generate table of results, and generate table of indicators for which
land use is a significant moderator variable:

``` r
# Calculate random-effects model
es_asp_rma_lu <- es_asp %>%
  group_by(indicator) %>%
  nest() %>%
  mutate(rma_obj = purrr::map(data, ~rma(yi, vi, slab = project, mod = ~ factor(lu), data=.x, method="REML"))) %>%
  mutate(rma_tidy = purrr::map(rma_obj, broom::tidy))

# Make table for reporting, also include QM test of moderators
lu_rma_report_table <- es_asp_rma_lu %>%
  mutate(report_df = purrr::map(rma_obj, ~data.frame(tau2 = .x$tau2,
                                              i2 = .x$I2,
                                              h2 = .x$H2,
                                              r2 = .x$R2,
                                              q_stat = .x$QE,
                                              q_pval = .x$QEp,
                                              q_mod = .x$QM,
                                              q_mod_pval = .x$QMp))) %>%
  select(indicator, rma_tidy, report_df) %>%
  ungroup() %>%
  transmute(indicator, rma_tidy, report_df) %>%
  unnest(cols = c(rma_tidy, report_df)) %>%
  filter(term != "intercept") %>%
  select(-type) %>%
  mutate(q_sig = ifelse(q_pval<0.05, "significant", "not significant"),
         q_mod_sig = ifelse(q_mod_pval<0.05, "significant", "not significant"),
         estimate = round(estimate, 2),
         std.error = round(std.error, 2),
         statistic = round(statistic, 2),
         p.value = ifelse(round(p.value, 3) ==0, "<0.001", round(p.value, 3)),
         tau2 = round(tau2, 2),
         i2 = round(i2, 1),
         h2 = round(h2, 1),
         r2 = round(r2, 2),
         q_stat = round(q_stat, 1),
         q_pval = ifelse(round(q_pval, 3)==0, "<0.001", round(q_pval, 3)),
         q_mod = round(q_mod, 1),
         q_mod_pval = ifelse(round(q_mod_pval, 3)==0, "<0.001", round(q_mod_pval, 3)),
         indicator = factor(indicator, levels=indicator_plotting_order)) %>%
  arrange(indicator)
flextable(lu_rma_report_table)
```

<img src="dsp4sh_indicator_analysis_files/figure-gfm/random effects model with land use as a moderator variable-1.png" width="2816" />

``` r
# Extract significant moderator variables
lu_rma_report_table_sig <- lu_rma_report_table %>%
  filter(q_mod_sig=="significant") 
flextable(lu_rma_report_table_sig)
```

<img src="dsp4sh_indicator_analysis_files/figure-gfm/random effects model with land use as a moderator variable-2.png" width="2811" />

Plot relationship between land use and indicator variables for which it
is a significant predictor:

``` r
lu_indicators_df <- lu_rma_report_table_sig %>%
  distinct(indicator)
lu_indicators <- as.vector(lu_indicators_df$indicator)

lu_plots <- purrr::map(.x = lu_indicators,
                .f = ~{
                  es_asp %>% 
                    filter(indicator == .x) %>%
                    ggplot(aes(x=lu, y=yi, fill=label)) +
                    geom_boxplot(fatten=1.5, lwd=0.3, outlier.size=0.8) +
                    geom_hline(yintercept=0, linetype="dashed") +
                    labs(x="Land use", y="Log response ratio",
                         title=glue::glue({filter(indicator_labs_df, indicator==.x)$label})) + 
                    scale_fill_manual(values=c("#72874EFF","#476F84FF"),
                                         breaks=c("SHM", "Ref"),
                                         name="Management") +
                    theme_katy() +
                    theme(axis.text.x=element_text(angle=45, hjust=1))
                })

pluck(lu_plots, 1) + pluck(lu_plots, 2) + pluck(lu_plots, 3) + pluck(lu_plots, 4) + pluck(lu_plots, 5) + 
  plot_layout(axes="collect", guides = 'collect') +
  plot_annotation(tag_levels = 'A')  & 
  theme(plot.tag = element_text(face="bold")) 
```

![](dsp4sh_indicator_analysis_files/figure-gfm/Figure%20S16%20lu%20mod-1.png)<!-- -->

``` r
# ggsave(here::here("figs", "ms_figs_alt", "figsupp16_lu_mod_box.png"), width=180, height=150, units="mm", dpi=500)
```

``` r
# Calculate random-effects model
es_asp_rma_clay <- es_asp %>%
  group_by(indicator) %>%
  nest() %>%
  mutate(rma_obj = purrr::map(data, ~rma(yi, vi, slab = project, mod = ~ clay_mean, data=.x, method="REML"))) %>%
  mutate(rma_tidy = purrr::map(rma_obj, broom::tidy))

# Make table for reporting, also include QM test of moderators
clay_rma_report_table <- es_asp_rma_clay %>%
  mutate(report_df = purrr::map(rma_obj, ~data.frame(tau2 = .x$tau2,
                                              i2 = .x$I2,
                                              h2 = .x$H2,
                                              r2 = .x$R2,
                                              q_stat = .x$QE,
                                              q_pval = .x$QEp,
                                              q_mod = .x$QM,
                                              q_mod_pval = .x$QMp))) %>%
  select(indicator, rma_tidy, report_df) %>%
  ungroup() %>%
  transmute(indicator, rma_tidy, report_df) %>%
  unnest(cols = c(rma_tidy, report_df)) %>%
  filter(term != "intercept") %>%
  filter(indicator!="soc_stock_0_30cm") %>%
  filter(indicator!="soc_stock_100cm") %>%
  select(-term, -type) %>%
  mutate(q_sig = ifelse(q_pval<0.05, "significant", "not significant"),
         q_mod_sig = ifelse(q_mod_pval<0.05, "significant", "not significant"),
         estimate = round(estimate, 2),
         std.error = round(std.error, 2),
         statistic = round(statistic, 2),
         p.value = ifelse(round(p.value, 3) ==0, "<0.001", round(p.value, 3)),
         tau2 = round(tau2, 2),
         i2 = round(i2, 1),
         h2 = round(h2, 1),
         r2 = round(r2, 2),
         q_stat = round(q_stat, 1),
         q_pval = ifelse(round(q_pval, 3)==0, "<0.001", round(q_pval, 3)),
         q_mod = round(q_mod, 1),
         q_mod_pval = ifelse(round(q_mod_pval, 3)==0, "<0.001", round(q_mod_pval, 3)),
         indicator = factor(indicator, levels=indicator_plotting_order)) %>%
  arrange(indicator)
flextable(clay_rma_report_table)
```

<img src="dsp4sh_indicator_analysis_files/figure-gfm/random effects model with clay as a moderator variable-1.png" width="2513" />

``` r
clay_rma_report_table_sig <- clay_rma_report_table %>%
  filter(q_mod_sig=="significant") 
flextable(clay_rma_report_table_sig)
```

<img src="dsp4sh_indicator_analysis_files/figure-gfm/random effects model with clay as a moderator variable-2.png" width="2513" />

QM test is significant for multiple sieve aggregate stability and soil
respiration.

``` r
# Clay is significant for yoder mwd and soil respiration

clay_mwd <- ggplot(es_asp %>% filter(indicator=="yoder_agg_stab_mwd"), aes(x=clay_mean, y=yi)) +
  geom_point(aes(colour=label)) +
  geom_smooth(method="lm", formula = y~x, color="black") +
  geom_hline(yintercept=0, linetype="dashed") +
  stat_cor(aes(label = paste(after_stat(rr.label), after_stat(p.label), sep = "~`,`~")),
           label.y=0.92,
           size= 8/.pt) +
  geom_hline(yintercept=0, linetype="dashed") +
  labs(x="Site mean clay (%)", y="Log response ratio", title="Multiple sieve aggregate MWD") + 
  scale_colour_manual(values=c("#72874EFF","#476F84FF"),
                      breaks=c("SHM", "Ref"),
                      name="Management") +
  theme_katy()

clay_resp <- ggplot(es_asp %>% filter(indicator=="soil_respiration"), aes(x=clay_mean, y=yi)) +
  geom_point(aes(colour=label)) +
  geom_smooth(method="lm", formula = y~x, color="black") +
  geom_hline(yintercept=0, linetype="dashed") +
  stat_cor(aes(label = paste(after_stat(rr.label), after_stat(p.label), sep = "~`,`~")),
           label.y=0.92,
           size= 8/.pt) +
  geom_hline(yintercept=0, linetype="dashed") +
  labs(x="Site mean clay (%)", y="Log response ratio", title="Soil respiration") + 
  scale_colour_manual(values=c("#72874EFF","#476F84FF"),
                      breaks=c("SHM", "Ref"),
                      name="Management") +
  theme_katy()

clay_mwd + clay_resp +
  plot_layout(axes="collect", guides = 'collect') +
  plot_annotation(tag_levels = 'A')  & 
  theme(plot.tag = element_text(face="bold")) 
```

![](dsp4sh_indicator_analysis_files/figure-gfm/Figure%20S17%20clay%20mod-1.png)<!-- -->

``` r
# ggsave(here::here("figs", "ms_figs_alt", "figsupp17_clay_mod.png"),  width=180, height=100, units="mm", dpi=500)
```

### Compile all significant moderator variables into one table

``` r
# First step is to add an "term" column to MAT and MAP tables so that they will match the LU and TILL tables
mat2 <- mat_rma_report_table_sig %>%
  mutate(term = "MAT") %>%
  relocate(term, .after = indicator) %>%
  mutate(indicator = str_replace_all(indicator, indicator_labs))

map2 <- map_rma_report_table_sig %>%
  mutate(term = "MAP") %>%
  relocate(term, .after = indicator) %>%
  mutate(indicator = str_replace_all(indicator, indicator_labs))

lu2 <- lu_rma_report_table_sig %>%
  mutate(term = str_to_title(term)) %>%
  mutate(term = str_replace(term, "Factor\\(Lu\\)", "LU - ")) %>%
  mutate(indicator = str_replace_all(indicator, indicator_labs))

clay2 <- clay_rma_report_table_sig %>%
  mutate(term = "Clay") %>%
  mutate(indicator = str_replace_all(indicator, indicator_labs))

sig_mod_var_report_table <- rbind(mat2,
                                  map2,
                                  lu2,
                                  clay2
                                  ) %>%
  select(indicator, term, estimate, std.error, p.value, i2, r2, q_stat, q_pval, q_mod, q_mod_pval)
flextable(sig_mod_var_report_table)
```

<img src="dsp4sh_indicator_analysis_files/figure-gfm/table s4 significant moderator variables-1.png" width="1714" />

``` r
# write_csv(sig_mod_var_report_table, here::here("figs", "ms_figs_alt","tablesupp4_sig_mod_var_report_table.csv"))
```

# Using SHAPE scores to standardize response to management

Run linear mixed model for significance of management in SHAPE scores
(accounting for differences between projects) for each indicator:

``` r
# Pull out data
shape_lmer_df <- shape_spatial_mean_scores %>%
  dplyr::rename(score_mean_poxc = score_mean_ac) %>%
  pivot_longer(score_mean_soc:score_mean_kssl_was, names_to="indicator", values_to="shape_score") %>%
  mutate(indicator = case_when(grepl("_soc", indicator) ~ "soc",
                              grepl("_poxc", indicator) ~ "poxc",
                              grepl("_resp", indicator) ~ "resp",
                              grepl("_ace", indicator) ~ "ace",
                              grepl("kssl_was", indicator) ~ "wsa")) %>%
  left_join(dplyr::select(project, dsp_pedon_id, project, label, trt), by="dsp_pedon_id")

# Make summary table
shape_summary <- shape_lmer_df %>%
  mutate(label=factor(label, levels=c("BAU", "SHM", "Ref"))) %>%
  group_by(indicator, label) %>%
  dplyr::summarize(across(shape_score, mean_sd)) %>%
  na.omit() %>%
  unite("summary", shape_score_mean:shape_score_sd) %>%
  pivot_wider(names_from=indicator, values_from=summary) %>%
  relocate(label, wsa, soc, ace, poxc, resp)
```

    ## `summarise()` has grouped output by 'indicator'. You can override using the
    ## `.groups` argument.

``` r
flextable(shape_summary)
```

<img src="dsp4sh_indicator_analysis_files/figure-gfm/SHAPE score summary table-1.png" width="968" />

``` r
# write_csv(shape_summary, here::here("figs", "ms_figs_alt", "tablesupp5_shape_summary.csv"))
```

``` r
# Run lmer for all indicators
shape_lmer <- shape_lmer_df %>%
  mutate(label=factor(label, levels=c("BAU", "SHM", "Ref"))) %>%
  group_by(indicator) %>%
  nest() %>%
  mutate(lmer = purrr::map(data, ~lmer(shape_score ~ label + (1|project), data = .x)),
  drop1 = purrr::map(data, .f = ~{
    drop1(lmer(shape_score ~ label + (1|project), REML=FALSE, data = .x)) %>%
      select(-NumDF, -DenDF)
    })) %>%
  mutate(hsd = purrr::map(lmer, .f = ~{
    glht(.x, linfct = mcp(label = 'Tukey'))
  })) %>%
  mutate(cld = purrr::map(hsd, cld))

# Extract dataframe of lmer summary
shape_lmer_sum <- shape_lmer %>%
  mutate(tidy_lmer = purrr::map(lmer, broom.mixed::tidy),
  tidy_drop = purrr::map(drop1, broom.mixed::tidy))%>%
  select(indicator, tidy_lmer, tidy_drop) %>%
  unnest(cols=c(tidy_lmer, tidy_drop), names_sep="_") %>%
  mutate(sig = case_when(tidy_drop_p.value < 0.05 ~ "significant",
                         tidy_drop_p.value > 0.05 ~ "not_significant"))

# Extract significance letters
shape_letters <- shape_lmer %>%
  mutate(letters = purrr::map(cld, ~{
    pluck(.x, "mcletters","Letters") %>%
      data.frame() %>%
      rownames_to_column(var="label") %>%
      dplyr::rename("letter" = ".")
    })) %>%
  select(indicator, letters) %>%
  unnest(cols=c(letters))
```

Plot SHAPE scores for each indicator, grouped by management:

``` r
# Boxplot of SHAPE scores by management
ggplot(shape_lmer_df, aes(x=factor(label, levels=c("BAU", "SHM", "Ref")), 
                          y=shape_score, 
                          fill=factor(label, levels=c("BAU", "SHM", "Ref")))) +
  geom_boxplot(fatten=1.5, lwd=0.3, outlier.size=0.8) +
  facet_wrap(vars(factor(indicator, levels=c("wsa", "soc", "ace", "poxc", "resp"), 
                         labels=c("Single sieve aggregate stability", "SOC", "ACE protein", 
                                  "POX-C", "Soil respiration"))),
             scales="free_y") +
  geom_text(data=shape_letters, aes(y=1.1, label=letter), size=8/.pt) +
  labs(y="SHAPEv1.0S Score (Peer Group Percentile)",
       x="Management") +
  scale_fill_manual(values=c("#FED789FF","#72874EFF","#476F84FF")) +
  theme_katy() +
  theme(legend.position="none")
```

    ## Warning: Removed 193 rows containing non-finite outside the scale range
    ## (`stat_boxplot()`).

![](dsp4sh_indicator_analysis_files/figure-gfm/Figure%205%20SHAPE%20boxplot-1.png)<!-- -->

``` r
# ggsave(here::here("figs", "ms_figs_alt", "fig5_shape_scores.png"), height=120, width=140, units="mm", dpi=500)
# ggsave(here::here("figs", "ms_figs_alt", "fig5.pdf"), height=120, width=140, units="mm", dpi=1000)
```

#============================================================#
# # #        Hand Strength Review - KEY PINCH      # # #
#============================================================#

## Script Information ----------------------------------------------------

# Date: 15th May 2025

# Authors:
## Olivia Hartrick, Buckinghamshire Healthcare NHS Trust
## Rebecca Turner, UK Centre for Ecology & Hydrology

# Copyright Rebecca Turner, Olivia Hartrick 2025

#-----------------------------------------------------------------------#


#===================================================================#
# 1 Functions & packages ----
#===================================================================#

#devtools::install_github("becky-turner/csrtools")
# Load CSRtools
library(csrtools)

# Load other dependencies
library(dplyr)
library(tidyr)
library(metafor)
library(readr)
library(ggplot2)
library(janitor)

#===============================================================#
# 2 Plot study means ----
#===============================================================#
getwd()
# Import & clean means data
data <- read_csv("/data/notebooks/rstudio-testbook/grip_strength_review/LC-review-20250515/KEYPINCH/yy-data/keypinch.csv", na="")

data <- clean_names(data)
# Clean input data--------
colnames(data)
## What are the names of interventions?
table(data$intervention)

# Fix spelling errors
data <- data %>%
  mutate("intervention" = ifelse(intervention=="Trapezeictomy", "Trapeziectomy", intervention))

data$author_arm <- paste(data$study_author," ","(",data$study_arm,")", sep="")

# Create JR subset
jr_subset <- data %>% filter(intervention == "Joint Replacement")
# Create Endoscopic subset
trap_subset <- data %>% filter(intervention == "Trapeziectomy")

#==========================================================#
# 2 Pooled effects ----
#==========================================================#

# ALL DATA---------#
summary(data)
# Define column names and time points
outcome_means <- c("mean_1month", "mean_3months", "mean_6months","mean_12months")
outcome_sds <- c("sd_1month", "sd_3months", "sd_6months", "sd_12months")
time_points <- c("1", "3", "6", "12")
time_unit <- "months"

# Run pooled effect size analysis for all interventions
str(data)
intervention <- "All"

(
  pooled_smc_all <- pooled_smc_tp(data = data,
                                  outcome_means = outcome_means,
                                  outcome_sds = outcome_sds,
                                  baseline_mean = "mean_baseline",
                                  baseline_sd = "sd_baseline",
                                  sample_size = "n_in_arm",
                                  time_points = time_points,
                                  time_unit = time_unit,
                                  intervention = intervention)
)

# JR subset
# Run pooled effect size analysis for jr subset
intervention <- "Joint Replacement"
# Define column names and time points
outcome_means <- c("mean_1month", "mean_1_5months", "mean_3months", "mean_6months","mean_12months")
outcome_sds <- c("sd_1month", "sd_1_5months", "sd_3months", "sd_6months", "sd_12months")
time_points <- c("1", "1.5", "3", "6", "12")
time_unit <- "months"

(
  pooled_smc_jr <- pooled_smc_tp(data = jr_subset,
                                   outcome_means = outcome_means,
                                   outcome_sds = outcome_sds,
                                   baseline_mean = "mean_baseline",
                                   baseline_sd = "sd_baseline",
                                   sample_size = "n_in_arm",
                                   time_points = time_points,
                                   time_unit = time_unit,
                                   intervention = intervention)
)



# Trapeziectomy subset

# Run pooled effect size analysis for trapeziectomy interventions
intervention <- "Trapeziectomy"
# Define column names and time points
outcome_means <- c("mean_1month", "mean_3months", "mean_6months","mean_12months")
outcome_sds <- c("sd_1month", "sd_3months", "sd_6months", "sd_12months")
time_points <- c("1", "3", "6", "12")
time_unit <- "months"


(
  pooled_smc_trap <- pooled_smc_tp(data = trap_subset,
                                         outcome_means = outcome_means,
                                         outcome_sds = outcome_sds,
                                         baseline_mean = "mean_baseline",
                                         baseline_sd = "sd_baseline",
                                         sample_size = "n_in_arm",
                                         time_points = time_points,
                                         time_unit = time_unit,
                                         intervention = intervention)
)


# Combine datasets
pooled_effects_summary <- rbind(pooled_smc_all, pooled_smc_jr, pooled_smc_trap)

#Plot combined datasets
pooled_effects_summary %>%
  ggplot(aes(x=time_point, y=smc, colour=intervention)) +
  geom_line(alpha=0.5, linewidth = 0.8) +
  geom_point(alpha=0.5, size = 1.5) +
  scale_x_continuous(breaks = seq(0, 12, by = 3), limits = c(0, 12)) +
  theme_minimal() +
  # Specify plot text size
  theme(text = element_text(size = 14),
        plot.background = element_rect(fill = "white", colour = NA)) +
  # Specify what the label text is
  labs( x="Months", y="Pooled Effect Size")  +
  scale_colour_manual(name = "Intervention Type", values = c("black", "red3", "deepskyblue2"))





#=============================================================#
# 4 INTERVENTIONS - CUMULATIVE SMC---------


## Calculate SMC trap_results----

## Impute means for missing values in subset
dat1 <- trap_subset

# Define the columns to work on (columns 11 to 31)
impute_cols <- colnames(dat1)[13:22]

dat1 <- impute_missing(data = dat1, impute_cols, sample_size = "n_in_arm")

# Re-assign to open vs endoscopic subsets
trap_subset2 <- dat1

# 1. Split into subgroups
subgroups <- split(trap_subset2, trap_subset2$intervention)

# 2. Run cumulative SMC for each subgroup
trap_results <- lapply(names(subgroups), function(subgroup_name) {

  data_sub <- subgroups[[subgroup_name]]

  # Run your existing smc_tp() steps here
  smc_list <- list(
    smc_tp(data = data_sub, mean_from = "mean_baseline", sd_from = "sd_baseline",
           outcome_mean = "mean_1month", outcome_sd = "sd_1month",
           sample_size = "mean_1month_sample", study_id = "author_arm", time_point = "1"),

    smc_tp(data = data_sub, mean_from = "mean_1month", sd_from = "sd_1month",
           outcome_mean = "mean_3months", outcome_sd = "sd_3months",
           sample_size = "mean_3months_sample", study_id = "author_arm", time_point = "3"),

    smc_tp(data = data_sub, mean_from = "mean_3months", sd_from = "sd_3months",
           outcome_mean = "mean_6months", outcome_sd = "sd_6months",
           sample_size = "mean_6months_sample", study_id = "author_arm", time_point = "6"),

    smc_tp(data = data_sub, mean_from = "mean_6months", sd_from = "sd_6months",
           outcome_mean = "mean_12months", outcome_sd = "sd_12months",
           sample_size = "mean_12months_sample", study_id = "author_arm", time_point = "12")

  )


  df <- do.call(rbind, smc_list)
  df$time_point <- as.numeric(df$time_point)
  df <- df[order(df$time_point), ]
  df$smc_cum <- cumsum(df$smc)
  df <- cumulative_ci(df, method = "wald")
  df <- rbind(0,df)
  df$intervention <- subgroup_name

  return(df)
})

## Calculate SMC jr_results
## Impute means for missing values in endoscopic subset
dat2 <- jr_subset

# Define columns to carry out imputations
impute_cols <- colnames(dat2)[13:222]
dat2 <- impute_missing(data = dat2, impute_cols,
                       sample_size = "n_in_arm")

# Re-assign
jr_subset2 <- dat2

# TRANSFORM TO LIST OBJECT
subgroups <- split(jr_subset2, jr_subset2$intervention)
subgroups <- clean_names(subgroups)

# 2. Run cumulative SMC for each subgroup
jr_results <- lapply(names(subgroups), function(subgroup_name) {

  data_sub <- subgroups[[subgroup_name]]

  # Run your existing smc_tp() steps here
  smc_list <- list(
    smc_tp(data = data_sub, mean_from = "mean_baseline", sd_from = "sd_baseline",
           outcome_mean = "mean_1month", outcome_sd = "sd_1month",
           sample_size = "mean_1month_sample", study_id = "author_arm", time_point = "1"),

    #smc_tp(data = data_sub, mean_from = "mean_1month", sd_from = "sd_1month",
    #       outcome_mean = "mean_1_5months", outcome_sd = "sd_1_5months",
    #       sample_size = "mean_1_5months_sample", study_id = "author_arm", time_point = "1.5"),

    smc_tp(data = data_sub, mean_from = "mean_1month", sd_from = "sd_1month",
           outcome_mean = "mean_3months", outcome_sd = "sd_3months",
           sample_size = "mean_3months_sample", study_id = "author_arm", time_point = "3"),

    smc_tp(data = data_sub, mean_from = "mean_3months", sd_from = "sd_3months",
           outcome_mean = "mean_6months", outcome_sd = "sd_6months",
           sample_size = "mean_6months_sample", study_id = "author_arm", time_point = "6"),

    smc_tp(data = data_sub, mean_from = "mean_6months", sd_from = "sd_6months",
           outcome_mean = "mean_12months", outcome_sd = "sd_12months",
           sample_size = "mean_12months_sample", study_id = "author_arm", time_point = "12")

  )


  df <- do.call(rbind, smc_list)
  df$time_point <- as.numeric(df$time_point)
  df <- df[order(df$time_point), ]
  df$smc_cum <- cumsum(df$smc)
  df <- cumulative_ci(df, method = "wald")
  df <- rbind(0,df)
  df$intervention <- subgroup_name

  return(df)
})

# 3. Combine all subgroups
trap_results <- do.call(rbind, trap_results)
jr_results <- do.call(rbind, jr_results)
smc_data <- rbind(trap_results, jr_results)

smc_data

# PLOT------------

timepoints <- c(0,1,3,6,12)
cols <- c(joint_replacement = "red",Trapeziectomy = "lightblue")
unique(smc_data$intervention)
smc_data %>%
  ggplot(aes(x = time_point, y = smc_cum,
             color = intervention,
             fill = intervention)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.5) +
  geom_ribbon(aes(ymin = lower_ci_wald, ymax = upper_ci_wald,
                  fill = intervention), alpha = 0.2) +
  scale_color_manual(name = NULL,
                     values = cols,
                     labels = c("Joint Replacement", "Trapeziectomy")) +
  scale_fill_manual(name   = NULL,
                    values = cols,
                    labels = c("Joint Replacement", "Trapeziectomy")) +
  scale_x_continuous(breaks = c(0,2,4,6,8,10,12)) +
  theme_minimal() +
  labs(x = "Months", y = "Cumulative SMC",
       color = "", fill = "") +
  theme(legend.position = "bottom", legend.text = element_text(size = 10))

#ggsave("/data/notebooks/rstudio-testbook/grip_strength_review/LC-review-20250515/KEYPINCH/aa-intervention-csmc-plots/grip_csmc_intervention_macro_v1.jpeg", dpi=400)


### PLOT SMOOTH CUMULATIVE SMCs---

#timepoints <- c(0,1,3,6,12)
# Fix col for plotting
smc_data <- smc_data %>% mutate(intervention = ifelse(intervention == "joint_replacement", "jr", "trapeziectomy"))
cols <- c(jr = "red", trapeziectomy = "lightblue")



# 1. Smooth CSMC curves by subgroup
smc_smooth <- smooth_csmc(smc_data,
                          csmc = "smc_cum",
                          lower_ci = "lower_ci_wald",
                          upper_ci = "upper_ci_wald",
                          intervention = "intervention",
                          span = 0.4,
                          time_series = seq(0, 12, 0.01))  # Adjust to match time range

# 2. Prepare ribbons for confidence intervals
smc_ribbon <- smc_smooth %>%
  filter(parameter %in% c("lower", "upper")) %>%
  pivot_wider(names_from = parameter,
              values_from = smoothed_value,
              id_cols = c(time_point, intervention)) %>%
  rename(ymin = lower, ymax = upper)

# 3. Prepare lines for smoothed SMC
smc_lines <- smc_smooth %>%
  filter(parameter == "smc")

# 4. Plot
ggplot() +
  # Confidence ribbon
  geom_ribbon(data = smc_ribbon,
              aes(x = time_point, ymin = ymin, ymax = ymax,
                  fill = intervention, group = intervention),
              alpha = 0.3) +
  # Smoothed SMC lines
  geom_line(data = smc_lines,
            aes(x = time_point, y = smoothed_value,
                color = intervention, group = intervention),
            size = 1) +
  # Raw SMC points
  geom_point(data = smc_data,
             aes(x = time_point, y = smc_cum, color = intervention),
             size = 2, shape = 16) +
  scale_color_manual(name = NULL,
                     values = cols,
                     labels = c("Joint Replacement", "Trapeziectomy")) +
  scale_fill_manual(name   = NULL,
                    values = cols,
                    labels = c("Joint Replacement", "Trapeziectomy")) +
  scale_x_continuous(breaks = c(0,2,4,6,8,10,12)) +
  labs(x = "Months", y = "Cumulative SMC") +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size=10))

#ggsave("/data/notebooks/rstudio-testbook/grip_strength_review/LC-review-20250515/KEYPINCH/aa-intervention-csmc-plots/grip_csmc_intervention_macro_v2.jpeg", dpi=400)


#============================================================#
# 3 SUB GROUPS ----
#============================================================#

#========================================#
## Types of Trapeziectomy----
#========================================#
## Impute means for missing values in subset
dat1 <- trap_subset
colnames(dat1)
# Define the columns to work on (columns 11 to 31)
impute_cols <- colnames(dat1)[13:22]

dat1 <- impute_missing(data = dat1, impute_cols, sample_size = "n_in_arm")

# Re-assign to open vs endoscopic subsets
trap_subset2 <- dat1

# 1. Split into subgroups
subgroups <- split(trap_subset2, trap_subset2$type_of_trapeziectomy)

# 2. Run cumulative SMC for each subgroup
subgroup_results <- lapply(names(subgroups), function(subgroup_name) {

  data_sub <- subgroups[[subgroup_name]]

  # Run your existing smc_tp() steps here
  smc_list <- list(
    smc_tp(data = data_sub, mean_from = "mean_baseline", sd_from = "sd_baseline",
           outcome_mean = "mean_1month", outcome_sd = "sd_1month",
           sample_size = "mean_1month_sample", study_id = "author_arm", time_point = "1"),


    smc_tp(data = data_sub, mean_from = "mean_1month", sd_from = "sd_1month",
           outcome_mean = "mean_3months", outcome_sd = "sd_3months",
           sample_size = "mean_3months_sample", study_id = "author_arm", time_point = "3"),

    smc_tp(data = data_sub, mean_from = "mean_3months", sd_from = "sd_3months",
           outcome_mean = "mean_6months", outcome_sd = "sd_6months",
           sample_size = "mean_6months_sample", study_id = "author_arm", time_point = "6"),

    smc_tp(data = data_sub, mean_from = "mean_6months", sd_from = "sd_6months",
           outcome_mean = "mean_12months", outcome_sd = "sd_12months",
           sample_size = "mean_12months_sample", study_id = "author_arm", time_point = "12")

  )


  df <- do.call(rbind, smc_list)
  df$time_point <- as.numeric(df$time_point)
  df <- df[order(df$time_point), ]
  df$smc_cum <- cumsum(df$smc)
  df <- cumulative_ci(df, method = "wald")
  df <- rbind(0,df)
  df$type_of_trapeziectomy <- subgroup_name

  return(df)
})

# 3. Combine all subgroups
smc_data <- do.call(rbind, subgroup_results)

#timepoints <- c(0,1,3,6,12)
cols <- RColorBrewer::brewer.pal(n = length(unique(smc_data$type_of_trapeziectomy)), name = "Set2")
#cols <- RColorBrewer::brewer.pal(n = max(n_subgroups, 5), name = "Set2")
unique(smc_data$type_of_trapeziectomy)
smc_data %>%
  ggplot(aes(x = time_point, y = smc_cum,
             color = type_of_trapeziectomy)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.5) +
  geom_ribbon(aes(ymin = lower_ci_wald, ymax = upper_ci_wald,
                  fill = type_of_trapeziectomy), alpha = 0.2) +
  scale_color_manual(
    values = setNames(cols,
                      unique(smc_data$immobilisation_protocol))) +
  scale_fill_manual(values = cols) +
  scale_x_continuous(breaks = c(0,2,4,6,8,10,12)) +
  theme_minimal() +
  labs(x = "Months", y = "Cumulative SMC",
       color = "", fill = "") +
  theme(legend.position = "bottom", legend.text = element_text(size = 10))

#ggsave("/data/notebooks/rstudio-testbook/grip_strength_review/LC-review-20250515/KEYPINCH/bb-subgroup-csmc-plots/keypinch_csmc_subgroups_type_of_trapeziectomy_v1.jpeg", dpi=400)


### PLOT SMOOTH CUMULATIVE SMCs---

#timepoints <- c(0,1,3,6,12)
cols <- RColorBrewer::brewer.pal(n = length(unique(smc_data$type_of_trapeziectomy)), name = "Set2")

# 1. Smooth CSMC curves by subgroup
smc_smooth <- smooth_csmc(smc_data,
                          csmc = "smc_cum",
                          lower_ci = "lower_ci_wald",
                          upper_ci = "upper_ci_wald",
                          intervention = "type_of_trapeziectomy",  # <- key change here
                          span = 0.4,
                          time_series = seq(0, 12, 0.01))  # Adjust to match time range

# 2. Prepare ribbons for confidence intervals
smc_ribbon <- smc_smooth %>%
  filter(parameter %in% c("lower", "upper")) %>%
  pivot_wider(names_from = parameter,
              values_from = smoothed_value,
              id_cols = c(time_point, intervention)) %>%
  rename(ymin = lower, ymax = upper)

# 3. Prepare lines for smoothed SMC
smc_lines <- smc_smooth %>%
  filter(parameter == "smc")

# 4. Plot
ggplot() +
  # Confidence ribbon
  geom_ribbon(data = smc_ribbon,
              aes(x = time_point, ymin = ymin, ymax = ymax,
                  fill = intervention, group = intervention),
              alpha = 0.3) +
  # Smoothed SMC lines
  geom_line(data = smc_lines,
            aes(x = time_point, y = smoothed_value,
                color = intervention, group = intervention),
            size = 1) +
  # Raw SMC points
  geom_point(data = smc_data,
             aes(x = time_point, y = smc_cum, color = intervention),
             size = 2, shape = 16) +
  scale_color_manual(
    values = setNames(cols,
                      unique(smc_data$type_of_trapeziectomy))) +
  scale_fill_manual(values = cols) +
  scale_x_continuous(breaks = c(0,2,4,6,8,10,12)) +
  labs(x = "Months", y = "Cumulative standardised mean change") +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size=10))

#ggsave("/data/notebooks/rstudio-testbook/grip_strength_review/LC-review-20250515/KEYPINCH/bb-subgroup-csmc-plots/keypinch_csmc_subgroups_type_of_trapeziectomy_v2.jpeg", dpi=400)


#==================================#
## Immobilisation----
#==================================#

# 1. Split into subgroups
subgroups <- split(trap_subset2, trap_subset2$immobilisation_protocol)

# 2. Run cumulative SMC for each subgroup
subgroup_results <- lapply(names(subgroups), function(subgroup_name) {

  data_sub <- subgroups[[subgroup_name]]

  # Run your existing smc_tp() steps here
  smc_list <- list(
    smc_tp(data = data_sub, mean_from = "mean_baseline", sd_from = "sd_baseline",
           outcome_mean = "mean_1month", outcome_sd = "sd_1month",
           sample_size = "mean_1month_sample", study_id = "author_arm", time_point = "1"),

    smc_tp(data = data_sub, mean_from = "mean_1month", sd_from = "sd_1month",
           outcome_mean = "mean_3months", outcome_sd = "sd_3months",
           sample_size = "mean_3months_sample", study_id = "author_arm", time_point = "3"),

    smc_tp(data = data_sub, mean_from = "mean_3months", sd_from = "sd_3months",
           outcome_mean = "mean_6months", outcome_sd = "sd_6months",
           sample_size = "mean_6months_sample", study_id = "author_arm", time_point = "6"),

    smc_tp(data = data_sub, mean_from = "mean_6months", sd_from = "sd_6months",
           outcome_mean = "mean_12months", outcome_sd = "sd_12months",
           sample_size = "mean_12months_sample", study_id = "author_arm", time_point = "12")

  )


  df <- do.call(rbind, smc_list)
  df$time_point <- as.numeric(df$time_point)
  df <- df[order(df$time_point), ]
  df$smc_cum <- cumsum(df$smc)
  df <- cumulative_ci(df, method = "wald")
  df <- rbind(0,df)
  df$immobilisation_protocol <- subgroup_name

  return(df)
})

# 3. Combine all subgroups
smc_data <- do.call(rbind, subgroup_results)


# REMOVE NR GROUP---
smc_data <- smc_data %>% filter(!immobilisation_protocol == "NR")
smc_data <- smc_data %>%
  mutate(label = factor(immobilisation_protocol,
                        levels  = c("<1 week", "2-4 weeks", ">4 weeks"),
                        ordered = TRUE))


#timepoints <- c(0,1,3,6,12)
cols <- setNames(RColorBrewer::brewer.pal(length(unique(smc_data$label)), "Set2"),
                 levels(smc_data$label))

unique(smc_data$label)
smc_data %>%
  ggplot(aes(x = time_point, y = smc_cum,
             color = label)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.5) +
  geom_ribbon(aes(ymin = lower_ci_wald, ymax = upper_ci_wald,
                  fill = label), alpha = 0.2) +
  scale_color_manual(values = cols, breaks = names(cols)) +
  scale_fill_manual(values = cols, breaks = names(cols)) +
  scale_x_continuous(breaks = c(0,2,4,6,8,12)) +
  theme_minimal() +
  labs(x = "Months", y = "Cumulative SMC",
       color = "", fill = "") +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 10))

#ggsave("/data/notebooks/rstudio-testbook/grip_strength_review/LC-review-20250515/KEYPINCH/bb-subgroup-csmc-plots/keypinch_csmc_subgroups_immobilisation_v1.jpeg", dpi=400)


### PLOT SMOOTH CUMULATIVE SMCs---

#timepoints <- c(0,1,3,6,12)
#cols <- RColorBrewer::brewer.pal(n = length(unique(smc_data$immobilisation_protocol)), name = "Set2")

# 1. Smooth CSMC curves by subgroup
smc_smooth <- smooth_csmc(smc_data,
                          csmc = "smc_cum",
                          lower_ci = "lower_ci_wald",
                          upper_ci = "upper_ci_wald",
                          intervention = "immobilisation_protocol",  #
                          span = 0.4,
                          time_series = seq(0, 12, 0.01))  # Adjust to match time range

smc_smooth <- smc_smooth %>%
  mutate(label = factor(intervention,
                        levels  = c("<1 week", "2-4 weeks", ">4 weeks"),
                        ordered = TRUE))

# 2. Prepare ribbons for confidence intervals
smc_ribbon <- smc_smooth %>%
  filter(parameter %in% c("lower", "upper")) %>%
  pivot_wider(names_from = parameter,
              values_from = smoothed_value,
              id_cols = c(time_point, intervention)) %>%
  rename(ymin = lower, ymax = upper)

# 3. Prepare lines for smoothed SMC
smc_lines <- smc_smooth %>%
  filter(parameter == "smc")

cols <- setNames(RColorBrewer::brewer.pal(length(unique(smc_data$label)), "Set2"),
                 levels(smc_data$label))
# 4. Plot
ggplot() +
  # Confidence ribbon
  geom_ribbon(data = smc_ribbon,
              aes(x = time_point, ymin = ymin, ymax = ymax,
                  fill = intervention, group = intervention),
              alpha = 0.3) +
  # Smoothed SMC lines
  geom_line(data = smc_lines,
            aes(x = time_point, y = smoothed_value,
                color = intervention, group = intervention),
            size = 1) +
  # Raw SMC points
  geom_point(data = smc_data,
             aes(x = time_point, y = smc_cum, color = intervention),
             size = 2, shape = 16) +
  scale_color_manual(values = cols, breaks = names(cols)) +
  scale_fill_manual(values = cols, breaks = names(cols))  +
  scale_x_continuous(breaks = c(0,2,4,6,8,10,12)) +
  labs(x = "Months", y = "Cumulative SMC") +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 10))

#ggsave("/data/notebooks/rstudio-testbook/grip_strength_review/LC-review-20250515/KEYPINCH/bb-subgroup-csmc-plots/keypinch_csmc_subgroups_immobilisation_v2.jpeg", dpi=400)


#=====================================#
## Types of joint replacement----
#====================================#

## Impute means for missing values in endoscopic subset
dat2 <- jr_subset
colnames(dat2)
# Define columns to carry out imputations
impute_cols <- colnames(dat2)[13:22]
dat2 <- impute_missing(data = dat2, impute_cols,
                       sample_size = "n_in_arm")

# Re-assign
jr_subset2 <- dat2


### Dual mobility----

# 1. Split into subgroups
subgroups <- split(jr_subset2, jr_subset2$dual_mobility)
subgroups <- clean_names(subgroups)

# 2. Run cumulative SMC for each subgroup
subgroup_results <- lapply(names(subgroups), function(subgroup_name) {

  data_sub <- subgroups[[subgroup_name]]

  # Run your existing smc_tp() steps here
  smc_list <- list(
    smc_tp(data = data_sub, mean_from = "mean_baseline", sd_from = "sd_baseline",
           outcome_mean = "mean_1month", outcome_sd = "sd_1month",
           sample_size = "mean_1month_sample", study_id = "author_arm", time_point = "1"),

    smc_tp(data = data_sub, mean_from = "mean_1month", sd_from = "sd_1month",
           outcome_mean = "mean_3months", outcome_sd = "sd_3months",
           sample_size = "mean_3months_sample", study_id = "author_arm", time_point = "3"),

    smc_tp(data = data_sub, mean_from = "mean_3months", sd_from = "sd_3months",
           outcome_mean = "mean_6months", outcome_sd = "sd_6months",
           sample_size = "mean_6months_sample", study_id = "author_arm", time_point = "6"),

    smc_tp(data = data_sub, mean_from = "mean_6months", sd_from = "sd_6months",
           outcome_mean = "mean_12months", outcome_sd = "sd_12months",
           sample_size = "mean_12months_sample", study_id = "author_arm", time_point = "12")

  )


  df <- do.call(rbind, smc_list)
  df$time_point <- as.numeric(df$time_point)
  df <- df[order(df$time_point), ]
  df$smc_cum <- cumsum(df$smc)
  df <- cumulative_ci(df, method = "wald")
  df <- rbind(0,df)
  df$dual_mobility <- subgroup_name

  return(df)
})

# 3. Combine all subgroups
smc_data <- do.call(rbind, subgroup_results)

#timepoints <- c(0,1,3,6,12)
unique(smc_data$dual_mobility)

cols <- setNames(RColorBrewer::brewer.pal(length(unique(smc_data$dual_mobility)), "Set2"),
                 levels(smc_data$dual_mobility))

# Plot
smc_data %>%
  ggplot(aes(x = time_point, y = smc_cum,
             color = dual_mobility)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.5) +
  geom_ribbon(aes(ymin = lower_ci_wald, ymax = upper_ci_wald,
                  fill = dual_mobility), alpha = 0.2) +
  scale_color_manual(values = cols, labels=c("Dual mobility", "Single mobility")) +
  scale_fill_manual(values = cols, labels=c("Dual mobility", "Single mobility")) +
  scale_x_continuous(breaks = c(0,2,4,6,8,12)) +
  theme_minimal() +
  labs(x = "Months", y = "Cumulative SMC",
       color = "", fill = "") +
  theme(legend.position = "bottom",
        legend.text = element_text(size=10))

#ggsave("/data/notebooks/rstudio-testbook/grip_strength_review/LC-review-20250515/KEYPINCH/bb-subgroup-csmc-plots/keypinch_csmc_subgroups_dual_mobility_v1.jpeg", dpi=400)


### PLOT SMOOTH CUMULATIVE SMCs---

#timepoints <- c(0,1,3,6,12)
cols <- RColorBrewer::brewer.pal(n = length(unique(smc_data$dual_mobility)), name = "Set2")

# Fix col for plotting
smc_data <- smc_data %>% mutate(dual_mobility = ifelse(dual_mobility == "dual_mobility", "dual", "single"))


# 1. Smooth CSMC curves by subgroup
smc_smooth <- smooth_csmc(smc_data,
                          csmc = "smc_cum",
                          lower_ci = "lower_ci_wald",
                          upper_ci = "upper_ci_wald",
                          intervention = "dual_mobility",  #
                          span = 0.4,
                          time_series = seq(0, 12, 0.01))  # Adjust to match time range

# 2. Prepare ribbons for confidence intervals
smc_ribbon <- smc_smooth %>%
  filter(parameter %in% c("lower", "upper")) %>%
  pivot_wider(names_from = parameter,
              values_from = smoothed_value,
              id_cols = c(time_point, intervention)) %>%
  rename(ymin = lower, ymax = upper)

# 3. Prepare lines for smoothed SMC
smc_lines <- smc_smooth %>%
  filter(parameter == "smc")
unique(smc_ribbon$intervention)
# 4. Plot
ggplot() +
  # Confidence ribbon
  geom_ribbon(data = smc_ribbon,
              aes(x = time_point, ymin = ymin, ymax = ymax,
                  fill = intervention, group = intervention),
              alpha = 0.3) +
  # Smoothed SMC lines
  geom_line(data = smc_lines,
            aes(x = time_point, y = smoothed_value,
                color = intervention, group = intervention),
            size = 1) +
  # Raw SMC points
  geom_point(data = smc_data,
             aes(x = time_point, y = smc_cum, color = intervention),
             size = 2, shape = 16) +
  scale_color_manual(
    values = setNames(cols,
                      unique(smc_data$dual_mobility)),
    labels=c("Dual mobility", "Single mobility")) +
  scale_fill_manual(values = cols, labels=c("Dual mobility", "Single mobility")) +
  scale_x_continuous(breaks = c(0,2,4,6,8,10,12)) +
  labs(x = "Months", y = "Cumulative SMC") +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 10))

#ggsave("/data/notebooks/rstudio-testbook/grip_strength_review/LC-review-20250515/KEYPINCH/bb-subgroup-csmc-plots/keypinch_csmc_subgroups_dual_mobility_v2.jpeg", dpi=400)



### Cemented----

# 1. Split into subgroups
subgroups <- split(jr_subset2, jr_subset2$cemented)
subgroups <- clean_names(subgroups)

# 2. Run cumulative SMC for each subgroup
subgroup_results <- lapply(names(subgroups), function(subgroup_name) {

  data_sub <- subgroups[[subgroup_name]]

  # Run your existing smc_tp() steps here
  smc_list <- list(
    smc_tp(data = data_sub, mean_from = "mean_baseline", sd_from = "sd_baseline",
           outcome_mean = "mean_1month", outcome_sd = "sd_1month",
           sample_size = "mean_1month_sample", study_id = "author_arm", time_point = "1"),

    smc_tp(data = data_sub, mean_from = "mean_1month", sd_from = "sd_1month",
           outcome_mean = "mean_3months", outcome_sd = "sd_3months",
           sample_size = "mean_3months_sample", study_id = "author_arm", time_point = "3"),

    smc_tp(data = data_sub, mean_from = "mean_3months", sd_from = "sd_3months",
           outcome_mean = "mean_6months", outcome_sd = "sd_6months",
           sample_size = "mean_6months_sample", study_id = "author_arm", time_point = "6"),

    smc_tp(data = data_sub, mean_from = "mean_6months", sd_from = "sd_6months",
           outcome_mean = "mean_12months", outcome_sd = "sd_12months",
           sample_size = "mean_12months_sample", study_id = "author_arm", time_point = "12")

  )

  df <- do.call(rbind, smc_list)
  df$time_point <- as.numeric(df$time_point)
  df <- df[order(df$time_point), ]
  df$smc_cum <- cumsum(df$smc)
  df <- cumulative_ci(df, method = "wald")
  df <- rbind(0,df)
  df$cemented <- subgroup_name

  return(df)
})

# 3. Combine all subgroups
smc_data <- do.call(rbind, subgroup_results)

#timepoints <- c(0,1,3,6,12)
unique(smc_data$cemented)
# Plot
smc_data %>%
  ggplot(aes(x = time_point, y = smc_cum,
             color = cemented)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.5) +
  geom_ribbon(aes(ymin = lower_ci_wald, ymax = upper_ci_wald,
                  fill = cemented), alpha = 0.2) +
  scale_color_manual(values = cols, labels=c("Uncemented")) +
  scale_fill_manual(values = cols, labels=c("Uncemented")) +
  scale_x_continuous(breaks = c(0,2,4,6,8,12)) +
  theme_minimal() +
  labs(x = "Months", y = "Cumulative SMC",
       color = "", fill = "") +
  theme(legend.position = "bottom",
        legend.text = element_text(size=10))

#ggsave("/data/notebooks/rstudio-testbook/grip_strength_review/LC-review-20250515/KEYPINCH/bb-subgroup-csmc-plots/keypinch_csmc_subgroups_cemented_v1.jpeg", dpi=400)


### PLOT SMOOTH CUMULATIVE SMCs---

#timepoints <- c(0,1,3,6,12)
cols <- RColorBrewer::brewer.pal(n = length(unique(smc_data$cemented)), name = "Set2")

# 1. Smooth CSMC curves by subgroup
smc_smooth <- smooth_csmc(smc_data,
                          csmc = "smc_cum",
                          lower_ci = "lower_ci_wald",
                          upper_ci = "upper_ci_wald",
                          intervention = "cemented",  #
                          span = 0.4,
                          time_series = seq(0, 12, 0.01))  # Adjust to match time range

# 2. Prepare ribbons for confidence intervals
smc_ribbon <- smc_smooth %>%
  filter(parameter %in% c("lower", "upper")) %>%
  pivot_wider(names_from = parameter,
              values_from = smoothed_value,
              id_cols = c(time_point, intervention)) %>%
  rename(ymin = lower, ymax = upper)

# 3. Prepare lines for smoothed SMC
smc_lines <- smc_smooth %>%
  filter(parameter == "smc")
unique(smc_ribbon$intervention)
# 4. Plot
ggplot() +
  # Confidence ribbon
  geom_ribbon(data = smc_ribbon,
              aes(x = time_point, ymin = ymin, ymax = ymax,
                  fill = intervention, group = intervention),
              alpha = 0.3) +
  # Smoothed SMC lines
  geom_line(data = smc_lines,
            aes(x = time_point, y = smoothed_value,
                color = intervention, group = intervention),
            size = 1) +
  # Raw SMC points
  geom_point(data = smc_data,
             aes(x = time_point, y = smc_cum, color = intervention),
             size = 2, shape = 16) +
  scale_color_manual(
    values = setNames(cols,
                      unique(smc_data$cemented)),
    labels=c("Uncemented")) +
  scale_fill_manual(values = cols, labels=c("Uncemented")) +
  scale_x_continuous(breaks = c(0,2,4,6,8,10,12)) +
  labs(x = "Months", y = "Cumulative SMC") +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 10))

#ggsave("/data/notebooks/rstudio-testbook/grip_strength_review/LC-review-20250515/KEYPINCH/bb-subgroup-csmc-plots/keypinch_csmc_subgroups_cemented_v2.jpeg", dpi=400)

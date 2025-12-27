library(data.table)
library(dplyr)

group_file <- "C:/Users/amand/Desktop/Master thesis/Summary-of-Groups.csv"
group_codes <- fread(group_file)

group_map <- group_codes %>%
  select(Kodas, Pavadinimas) %>%
  mutate(Kodas = as.integer(Kodas)) %>%
  group_by(Kodas) %>%
  summarise(Pavadinimas = Pavadinimas[1]) %>%
  ungroup()

clean_data <- function(file_path, year, group_map) {
  
  duom <- fread(file_path,
                na.strings = c("", "NA", "-", "null", "Null"))
  
  cols_to_remove <- c(
    "_type","_id","_revision","_page.next",
    "key_e","key_l","nuts",
    "dbu_metinis" 
  )
  
  duom[, (intersect(cols_to_remove, names(duom))) := NULL]
  
  if ("daro_laiko_dalis" %in% names(duom)) {
    setnames(duom, "daro_laiko_dalis", "darbo_laiko_dalis")
  }
  if ("darbo_laiko_dalis" %in% names(duom) & is.character(duom$darbo_laiko_dalis)) {
    # 2018 dublikatas - jei yra du stulpeliai
    duom[, darbo_laiko_dalis := suppressWarnings(as.numeric(darbo_laiko_dalis))]
  }
  
  if (is.character(duom$amzius)) {
    duom[, amzius := case_when(
      amzius == "20-29" ~ 24.5,
      amzius == "30-39" ~ 34.5,
      amzius == "40-49" ~ 44.5,
      amzius == "50-59" ~ 54.5,
      amzius == "60+"   ~ 65,
      TRUE ~ suppressWarnings(as.numeric(amzius))
    )]
  }
  
  duom[, profesija := as.integer(profesija)]
  
  duom <- merge(
    duom, group_map,
    by.x = "profesija",
    by.y = "Kodas",
    all.x = TRUE,
    allow.cartesian = FALSE
  )
  
  duom[, year := year]
  
  return(duom)
}

du2014 <- clean_data("C:/Users/amand/Desktop/Master thesis/ankstesni metai/DarboUzmokestis2014.csv", 2014, group_map)
du2018 <- clean_data("C:/Users/amand/Desktop/Master thesis/ankstesni metai/DarboUzmokestis2018.csv", 2018, group_map)
du2022 <- clean_data("C:/Users/amand/Desktop/Master thesis/DarboUzmokestis2022.csv", 2022, group_map)

du2014[, bdu_val := bdu_val / 3.4528]

all_data <- rbindlist(list(du2014, du2018, du2022), fill = TRUE)


numeric_cols <- c("bdu_val","stazas","amzius","virsvalandziu_sk",
                  "apmoketos_val","darbo_laiko_dalis")

for (col in numeric_cols) {
  if (col %in% names(all_data)) {
    all_data[[col]] <- suppressWarnings(as.numeric(all_data[[col]]))
  }
}


all_data <- all_data[
  bdu_val > 0 & bdu_val < 200 &
    stazas >= 0 & stazas <= 50 &
    amzius >= 15 & amzius <= 80
]

all_data$virsvalandziu_sk[is.na(all_data$virsvalandziu_sk)] <- 0
all_data$darbo_laiko_dalis[is.na(all_data$darbo_laiko_dalis)] <- 1
all_data$issilavinimas[is.na(all_data$issilavinimas)] <- "G2"
all_data$issilavinimas <- factor(all_data$issilavinimas)

data22 <- all_data[year == 2022]

prof_stats22 <- data22 %>%
  group_by(Pavadinimas) %>%
  summarise(
    n = n(),
    n_f = sum(lytis == "F"),
    n_m = sum(lytis == "M"),
    prop_f = n_f / n
  ) %>%
  filter(
    n_f >= 100,
    n_m >= 100,
    prop_f >= 0.30     
  )

valid_profs <- prof_stats22$Pavadinimas

cat("Selected professions 2022 m.:\n")
print(valid_profs)

filtered_data <- all_data[Pavadinimas %in% valid_profs]

cat("Observation number:", nrow(filtered_data), "\n")



data22 <- all_data[year == 2022]

prof_gap_2022 <- data22 %>%
  group_by(Pavadinimas) %>%
  summarise(
    n      = n(),
    n_f    = sum(lytis == "F"),
    n_m    = sum(lytis == "M"),
    mean_f = mean(bdu_val[lytis == "F"], na.rm = TRUE),
    mean_m = mean(bdu_val[lytis == "M"], na.rm = TRUE)
  ) %>%
  mutate(
    gap_pct = 100 * (mean_m - mean_f) / ((mean_m + mean_f) / 2), 
    ratio_f = n_f / n,
    ratio_m = n_m / n
  )

prof_gap_2022_filtered <- prof_gap_2022 %>%
  filter(
    n_f >= 100,
    n_m >= 100,
    ratio_f >= 0.30,
    ratio_m >= 0.30
  ) %>%
  arrange(desc(abs(gap_pct)))

top5_profs <- prof_gap_2022_filtered$Pavadinimas[1:5]

cat("TOP 5 professions:\n")
print(top5_profs)

filtered_data <- all_data[Pavadinimas %in% top5_profs]

cat("Number of records after TOP5 filtering:", nrow(filtered_data), "\n")

# =====================================================================
# Preliminary data analysis (EDA)
# =====================================================================

library(dplyr)
library(ggplot2)
library(data.table)
library(tidyr)
library(broom)
library(scales)

avg_wages <- filtered_data %>%
  group_by(year, lytis) %>%
  summarise(
    mean_wage = mean(bdu_val, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

ggplot(avg_wages, aes(x = year, y = mean_wage, color = lytis, group = lytis)) +
  geom_line(size = 1.4) +
  geom_point(size = 3) +
  labs(
    title = "Average hourly wage by gender (2014–2018–2022)",
    x = "Year",
    y = "Average hourly wage (EUR)",
    color = "Gender"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom")


gap_year <- filtered_data %>%
  group_by(year) %>%
  summarise(
    mean_f = mean(bdu_val[lytis == "F"], na.rm = TRUE),
    mean_m = mean(bdu_val[lytis == "M"], na.rm = TRUE)
  ) %>%
  mutate(
    gap_abs = mean_m - mean_f,
    gap_pct = 100 * (mean_m - mean_f) / ((mean_m + mean_f) / 2)
  )

print(gap_year)

avg_prof <- filtered_data %>%
  group_by(Pavadinimas, year, lytis) %>%
  summarise(
    mean_wage = mean(bdu_val, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

ggplot(avg_prof, aes(x = year, y = mean_wage, color = lytis, group = lytis)) +
  geom_line(size = 1.2) +
  geom_point(size = 2.6) +
  facet_wrap(~ Pavadinimas, scales = "free_y") +
  labs(
    title = "Hourly wage trends by gender and profession",
    x = "Year",
    y = "Hourly wage (EUR)"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom")

gap_prof_year <- filtered_data %>%
  group_by(Pavadinimas, year) %>%
  summarise(
    mean_f = mean(bdu_val[lytis == "F"], na.rm = TRUE),
    mean_m = mean(bdu_val[lytis == "M"], na.rm = TRUE),
    n_f = sum(lytis == "F"),
    n_m = sum(lytis == "M"),
    .groups = "drop"
  ) %>%
  mutate(
    gap_abs = mean_m - mean_f,
    gap_pct = 100 * (mean_m - mean_f) / ((mean_m + mean_f)/2)
  ) %>%
  arrange(Pavadinimas, year)

print(gap_prof_year)

edu2022 <- filtered_data[year == 2022] %>%
  group_by(Pavadinimas, issilavinimas, lytis) %>%
  summarise(
    mean_wage = mean(bdu_val, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  filter(n >= 20)  # atmetame labai mažas imtis

ggplot(edu2022, aes(x = issilavinimas, y = mean_wage, fill = lytis)) +
  geom_col(position = position_dodge(width = 0.75)) +
  facet_wrap(~ Pavadinimas, scales = "free_y") +
  labs(
    title = "Average hourly wage by education and gender (2022)",
    x = "Education group",
    y = "Hourly wage (EUR)",
    fill = "Gender"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom")

gap_edu_2022 <- filtered_data %>%
  filter(year == 2022) %>%
  group_by(Pavadinimas, issilavinimas) %>%
  summarise(
    mean_f = mean(bdu_val[lytis == "F"], na.rm = TRUE),
    mean_m = mean(bdu_val[lytis == "M"], na.rm = TRUE),
    n_f = sum(lytis == "F"),
    n_m = sum(lytis == "M"),
    .groups = "drop"
  ) %>%
  filter(n_f >= 20, n_m >= 20) %>%
  mutate(
    gap_abs = mean_m - mean_f,
    gap_pct = 100 * (mean_m - mean_f) / ((mean_m + mean_f)/2)
  ) %>%
  arrange(Pavadinimas, issilavinimas)

print(gap_edu_2022)


edu_share <- filtered_data %>%
  filter(year == 2022) %>%
  mutate(high_edu = issilavinimas %in% c("G4")) %>%
  group_by(lytis) %>%
  summarise(
    n = n(),
    high = sum(high_edu),
    share_high = round(100 * high / n, 1)
  )

print(edu_share)


exp2022 <- filtered_data[year == 2022] %>%
  mutate(
    exp_group = case_when(
      stazas <= 1 ~ "≤1",
      stazas <= 5 ~ "2–5",
      stazas <= 10 ~ "6–10",
      stazas <= 20 ~ "11–20",
      TRUE ~ "20+"
    )
  )

exp_summary <- exp2022 %>%
  group_by(Pavadinimas, exp_group, lytis) %>%
  summarise(
    mean_wage = mean(bdu_val, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  filter(n >= 15)

ggplot(exp_summary, aes(x = exp_group, y = mean_wage, color = lytis, group = lytis)) +
  geom_line(size = 1.1) +
  geom_point(size = 2.7) +
  facet_wrap(~ Pavadinimas, scales = "free_y") +
  labs(
    title = "Hourly wage by gender and experience group (2022)",
    x = "Experience group",
    y = "Hourly wage (EUR)"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom")

exp_gap_2022 <- filtered_data %>%
  filter(year == 2022) %>%
  mutate(
    exp_group = case_when(
      stazas <= 1 ~ "≤1",
      stazas <= 5 ~ "2–5",
      stazas <= 10 ~ "6–10",
      stazas <= 20 ~ "11–20",
      TRUE ~ "20+"
    )
  ) %>%
  group_by(Pavadinimas, exp_group) %>%
  summarise(
    mean_f = mean(bdu_val[lytis == "F"], na.rm = TRUE),
    mean_m = mean(bdu_val[lytis == "M"], na.rm = TRUE),
    n_f = sum(lytis == "F"),
    n_m = sum(lytis == "M"),
    .groups = "drop"
  ) %>%
  filter(n_f >= 15, n_m >= 15) %>%
  mutate(
    gap_abs = mean_m - mean_f,
    gap_pct = 100 * (mean_m - mean_f) / ((mean_m + mean_f)/2)
  ) %>%
  arrange(Pavadinimas, exp_group)

print(exp_gap_2022)

reality_results <- list()

for (p in unique(exp2022$Pavadinimas)) {
  
  df <- exp2022 %>% filter(Pavadinimas == p)
  
  women <- df %>% filter(lytis == "F", issilavinimas == "G4", stazas > 10)
  men   <- df %>% filter(lytis == "M", issilavinimas %in% c("G2", "G3"), stazas <= 10)
  
  if (nrow(women) >= 20 & nrow(men) >= 20) {
    reality_results[[p]] <- tidy(
      t.test(women$bdu_val, men$bdu_val, alternative = "less")
    )
  }
}

print(reality_results)

gap_master <- list(
  "Gap by year" = gap_year,
  "Gap by profession & year" = gap_prof_year,
  "Gap by education (2022)" = gap_edu_2022,
  "Gap by experience (2022)" = exp_gap_2022
)

gap_master


anova_model <- aov(bdu_val ~ lytis + issilavinimas + im_dydis +
                     darbo_laiko_dalis + stazas + amzius +
                     Pavadinimas,
                   data = exp2022)

summary(anova_model)

reality_tests <- list()

for (p in unique(filtered_data$Pavadinimas)) {
  
  df <- filtered_data %>% 
    filter(year == 2022, Pavadinimas == p)
  
  women <- df %>%
    filter(lytis == "F", 
           issilavinimas == "G4",
           stazas > 10)
  
  men <- df %>%
    filter(lytis == "M",
           issilavinimas %in% c("G1", "G2", "G3"),
           stazas <= 10)
  
  if (nrow(women) >= 20 & nrow(men) >= 20) {
    reality_tests[[p]] <- broom::tidy(
      t.test(women$bdu_val, men$bdu_val, alternative = "less")
    )
  }
}

print(reality_tests)

# =========================================================
# TWO-WAY ANOVA: 
# =========================================================
length(unique(anova_data$Pavadinimas))
levels(anova_data$Pavadinimas)

library(car)
library(emmeans)

anova_data <- filtered_data %>%
  filter(year == 2022) %>%
  mutate(
    lytis = factor(lytis),
    Pavadinimas = factor(Pavadinimas)
  )

group_sizes <- anova_data %>%
  group_by(lytis, Pavadinimas) %>%
  summarise(n = n(), .groups = "drop")

print(group_sizes)
summary(group_sizes$n)

anova_2way <- aov(
  bdu_val ~ lytis * Pavadinimas,
  data = anova_data
)

summary(anova_2way)

anova_tab <- summary(anova_2way)[[1]] %>% as.data.frame()

anova_tab$eta_sq <- anova_tab$`Sum Sq` / sum(anova_tab$`Sum Sq`)

anova_tab

res <- residuals(anova_2way)

hist(
  res,
  breaks = 50,
  main = "Residual histogram (Two-way ANOVA)",
  xlab = "Residuals"
)

qqnorm(res)
qqline(res, col = "red", lwd = 2)


levene_test <- leveneTest(
  bdu_val ~ interaction(lytis, Pavadinimas),
  data = anova_data
)

print(levene_test)

lm_2way <- lm(bdu_val ~ lytis * Pavadinimas, data = anova_data)

robust_anova <- Anova(
  lm_2way,
  type = 2,
  white.adjust = "hc3"
)

print(robust_anova)

emm <- emmeans(anova_2way, ~ lytis | Pavadinimas)
pairs(emm)


#===========================================================================
# Regressionbased analysis of the gender pay gap
#===========================================================================

#MULTIVARIARE REGRESSION

filtered_data <- all_data[Pavadinimas %in% valid_profs]

library(dplyr)
duomenys22 <- filtered_data %>%
  filter(year == 2022) %>%
  mutate(
    log_wage = log(bdu_val),
    lytis_num = ifelse(lytis == "M", 1, 0),
    part_time = ifelse(darbo_laiko_dalis < 1, 1, 0)
  ) %>%
  select(
    log_wage, bdu_val, lytis, lytis_num,
    age_group = amzius,      # naudosi: jei nori grupuoti, pakeisim
    stazas,
    issilavinimas, im_dydis,
    Pavadinimas,
    part_time, premijos
  )

duomenys22 <- duomenys22 %>%
  mutate(
    issilavinimas = factor(issilavinimas),
    im_dydis = factor(im_dydis),
    Pavadinimas = factor(Pavadinimas),
    part_time = factor(part_time, levels=c(0,1), labels=c("Full","Part"))
  )

duomenys22 <- duomenys22 %>%
  mutate(
    age_group = case_when(
      age_group <= 24 ~ "20–24",
      age_group <= 34 ~ "25–34",
      age_group <= 44 ~ "35–44",
      age_group <= 54 ~ "45–54",
      TRUE ~ "55+"
    ),
    age_group = factor(age_group, ordered = TRUE)
  )

reg_model <- lm(
  log_wage ~ lytis_num + age_group + stazas +
    issilavinimas + im_dydis + Pavadinimas +
    part_time + premijos,
  data = duomenys22
)

summary(reg_model)

library(dplyr)
library(broom)

reg_df <- tidy(reg_model)

reg_df_clean <- reg_df %>%
  filter(!grepl("^Pavadinimas", term))


reg_df_clean <- reg_df_clean %>%
  mutate(
    estimate_se = sprintf("%.3f (%.3f)", estimate, std.error),
    p_stars = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      TRUE ~ ""
    )
  ) %>%
  select(term, estimate_se, p.value, p_stars)

print(reg_df_clean)

metrics <- data.frame(
  N = nrow(reg_model$model),
  R2 = round(summary(reg_model)$r.squared, 3),
  Adj_R2 = round(summary(reg_model)$adj.r.squared, 3),
  RMSE = round(sqrt(mean(reg_model$residuals^2)), 3),
  F_stat = round(summary(reg_model)$fstatistic[1], 3)
)

print(metrics)

resid <- residuals(reg_model)

hist(
  resid,
  breaks = 40,
  main = "Residual histogram",
  xlab = "Residuals",
  col = "lightgray",
  border = "white"
)

qqnorm(resid)
qqline(resid, col = "red", lwd = 2)


install.packages("moments") 
library(moments)

skew <- skewness(residuals(reg_model))
kurt <- kurtosis(residuals(reg_model))    
ex_kurt <- kurtosis(residuals(reg_model)) - 3

skew
kurt
ex_kurt

#install.packages("lmtest")
library(lmtest)

bptest(reg_model)

library(sandwich)

robust_se <- vcovHC(reg_model, type = "HC3")

coeftest(reg_model, vcov = robust_se)


coeftest(reg_model)                    
coeftest(reg_model, vcov = robust_se)  


install.packages("car")

library(car)

vif_values <- vif(reg_model)
vif_values

cook <- cooks.distance(reg_model)
cutoff <- 4 / length(cook)
cutoff


plot(
  cook,
  type = "h",
  main = "Cook's distance",
  ylab = "Cook's distance",
  xlab = "Observation index"
)
abline(h = cutoff, col = "red", lty = 2)

sum(cook > cutoff)
sum(cook > 1)


# OAXACA–BLINDER DECOMPOZICIJA

library(dplyr)
library(oaxaca)

duomenys22 <- filtered_data[filtered_data$year == 2022, ]

if (!"premijos" %in% names(duomenys22)) {
  duomenys22$premijos <- 0
}


du_oax <- duomenys22 %>%
  mutate(
    log_wage = log(bdu_val),
    lytis_bin = ifelse(lytis == "M", 1, 0),
    part_time = ifelse(darbo_laiko_dalis < 1, 1, 0),
    age_group = cut(
      amzius,
      breaks = c(15, 25, 35, 45, 55, 65, 81),
      labels = c("16–24", "25–34", "35–44", "45–54", "55–64", "65+"),
      right = FALSE,
      include.lowest = TRUE
    )
  )


freq_prof <- table(du_oax$Pavadinimas)
daznos <- names(freq_prof[freq_prof >= 200])

du_oax <- du_oax %>% 
  filter(Pavadinimas %in% daznos) %>% 
  mutate(
    prof_group = factor(Pavadinimas)
  )


du_oax <- du_oax %>%
  mutate(
    issilavinimas = factor(issilavinimas),
    im_dydis      = factor(im_dydis),
    age_group     = factor(age_group, ordered = FALSE),
    part_time     = as.numeric(part_time),
    lytis_bin     = as.numeric(lytis_bin)
  )

vars_for_oax <- c(
  "log_wage", "lytis_bin", "age_group", "stazas",
  "issilavinimas", "im_dydis", "prof_group", "part_time", "premijos"
)

du_oax <- du_oax[complete.cases(du_oax[, ..vars_for_oax])]


cat("Observation number:", nrow(du_oax), "\n")
print(table(du_oax$lytis_bin))


oax_model <- oaxaca(
  formula = log_wage ~ age_group + stazas + issilavinimas +
    im_dydis + prof_group + part_time + premijos | lytis_bin,
  data = du_oax,
  R = 200
)

cat("\n================ OAXACA–BLINDER – summary ================\n")
oax_sum <- summary(oax_model)
print(oax_sum)

-
  overall <- oax_sum$twofold$overall

explained_gap    <- overall[1, "coef(explained)"]
unexplained_gap  <- overall[1, "coef(unexplained)"]
total_gap        <- explained_gap + unexplained_gap

explained_share   <- explained_gap   / total_gap * 100
unexplained_share <- unexplained_gap / total_gap * 100

cat("\n===== GENERAL DIFFERENCE AND PARTS =====\n")
cat("Total male–female log(wage) gap:", round(total_gap, 3), "\n")
cat("Explained part (endowments, X):", 
    round(explained_gap, 3), sprintf(" (%.1f%%)\n", explained_share))
cat("Unexplained part (coefficients, discrimination):",
    round(unexplained_gap, 3), sprintf(" (%.1f%%)\n", unexplained_share))

library(ggplot2)

oax_stack_df <- data.frame(
  Component = c("Explained", "Unexplained"),
  Value = c(abs(as.numeric(explained_gap)), abs(as.numeric(unexplained_gap)))
)

ggplot(oax_stack_df, aes(x = "", y = Value, fill = Component)) +
  geom_col(width = 0.5) +
  coord_flip() +
  labs(
    title = "Decomposition of the total gender wage gap",
    y = "Log wage gap",
    x = ""
  ) +
  theme_minimal()

# QUANTILE REGRESSION 

library(dplyr)
library(quantreg)

if (!"premijos" %in% names(filtered_data)) {
  filtered_data$premijos <- 0
}

du_qr <- filtered_data[
  year == 2022 & Pavadinimas %in% valid_profs
]


du_qr <- du_qr %>%
  as.data.frame() %>%  
  filter(
    !is.na(bdu_val),
    bdu_val > 0,
    !is.na(lytis),
    !is.na(amzius),
    !is.na(stazas)
  ) %>%
  mutate(
    log_wage = log(bdu_val),

    lytis_bin = ifelse(lytis == "M", 1, 0),

    age_group = cut(
      amzius,
      breaks = c(15, 25, 35, 45, 55, 65, 81),
      labels = c("16–24", "25–34", "35–44", "45–54", "55–64", "65+"),
      right  = FALSE,
      include.lowest = TRUE
    ),

    part_time = ifelse(darbo_laiko_dalis < 1, 1, 0)
  ) %>%
  mutate(
    age_group     = factor(age_group),
    issilavinimas = factor(issilavinimas),
    im_dydis      = factor(im_dydis),
    Pavadinimas   = factor(Pavadinimas),
    part_time     = as.numeric(part_time),
    lytis_bin     = as.numeric(lytis_bin)
  ) %>%
  droplevels()

cat("🧾 Number of rows in quantile regression data:", nrow(du_qr), "\n")
cat("Gender distribution in the QR sample:\n")
print(table(du_qr$lytis_bin))

if (nrow(du_qr) == 0) {
  stop("du_qr is empty - check filters / valid_profs.")
}

candidate_vars <- c(
  "lytis_bin", "age_group", "stazas",
  "issilavinimas", "im_dydis",
  "Pavadinimas", "part_time", "premijos"
)

varying_vars <- candidate_vars[
  sapply(du_qr[candidate_vars], function(x) dplyr::n_distinct(x, na.rm = TRUE) > 1)
]

cat("\nWe will use the following variables in the model:\n")
print(varying_vars)

if (length(varying_vars) == 0) {
  stop("No variables found with >1 unique value - something is wrong with the data.")
}

qr_formula <- as.formula(
  paste("log_wage ~", paste(varying_vars, collapse = " + "))
)

cat("\nQR formula used:\n")
print(qr_formula)

taus <- c(0.10, 0.25, 0.50, 0.75, 0.90)

qr_models <- lapply(taus, function(tau) {
  rq(qr_formula, data = du_qr, tau = tau)
})
names(qr_models) <- paste0("tau_", taus)

get_gender_coef <- function(model) {
  s <- summary(model, se = "nid")
  cf <- s$coefficients
  if ("lytis_bin" %in% rownames(cf)) {
    out <- cf["lytis_bin", ]
  } else {
    out <- c(NA, NA, NA, NA)
    names(out) <- c("Value", "Std. Error", "t value", "Pr(>|t|)")
  }
  return(out)
}

gender_qr_list <- lapply(qr_models, get_gender_coef)

gender_qr_df <- do.call(rbind, lapply(seq_along(gender_qr_list), function(i) {
  row <- gender_qr_list[[i]]
  data.frame(
    tau      = taus[i],
    estimate = as.numeric(row[1]),
    se       = as.numeric(row[2]),
    t        = as.numeric(row[3]),
    p_value  = as.numeric(row[4])
  )
}))

gender_qr_df <- gender_qr_df %>%
  mutate(
    stars = case_when(
      p_value < 0.001 ~ "***",
      p_value < 0.01  ~ "**",
      p_value < 0.05  ~ "*",
      p_value < 0.1   ~ ".",
      TRUE ~ ""
    )
  )

cat("\n================ GENDER RATIO BY QUANTILE ================\n")
print(
  gender_qr_df %>%
    mutate(
      estimate = round(estimate, 3),
      se       = round(se, 3),
      t        = round(t, 2),
      p_value  = signif(p_value, 3)
    )
)

cat("\nBrief interpretation:\n")
apply(gender_qr_df, 1, function(r) {
  cat(
    sprintf("Tau = %.2f: gender (M vs F) koef. = %.3f %s\n",
            as.numeric(r["tau"]),
            as.numeric(r["estimate"]),
            r["stars"])
  )
})

library(ggplot2)
library(dplyr)

gender_qr_df <- gender_qr_df %>%
  mutate(
    percent_gap = (exp(estimate) - 1) * 100
  )

ggplot(gender_qr_df, aes(x = tau, y = percent_gap)) +
  geom_line(size = 1.2, color = "#2C3E50") +
  geom_point(size = 3, color = "#E74C3C") +
  geom_text(aes(label = paste0(round(percent_gap, 1), "%")),
            vjust = -1, size = 4) +
  labs(
    title = "Gender pay gap across salary quantiles",
    x = "Quantile (τ)",
    y = "Pay gap %"
  ) +
  theme_minimal(base_size = 14) +
  ylim(min(gender_qr_df$percent_gap) - 2,
       max(gender_qr_df$percent_gap) + 2)


#=====================================================================
# Prediction of Individual Hourly Wages
#=====================================================================

# XGBOOST

library(data.table)
library(dplyr)
library(caret)
library(xgboost)

data22 <- filtered_data[year == 2022]


model_data <- data22 %>%
  select(
    bdu_val,           
    lytis,
    amzius,
    stazas,
    issilavinimas,
    virsvalandziu_sk,
    darbo_laiko_dalis,
    Pavadinimas,
    im_dydzio_kodas,
    sutarties_rusis,
    apmoketos_val,
    atostogu_dienos,
    premijos,
    bdu_spalio
  )

model_data <- model_data %>%
  mutate(
    lytis           = factor(lytis),
    issilavinimas   = factor(issilavinimas),
    Pavadinimas     = factor(Pavadinimas),
    evrk_2z         = factor(evrk_2z),
    im_dydzio_kodas = factor(im_dydzio_kodas),
    sutarties_rusis = factor(sutarties_rusis)
  )

num_vars <- c(
  "amzius", "stazas", "virsvalandziu_sk", "darbo_laiko_dalis",
  "savaiciu_sk", "apmoketos_val", "atostogu_dienos",
  "premijos", "priemokos", "virsvalandziu_bdu", "bdu_spalio"
)

for (v in num_vars) {
  model_data[[v]] <- suppressWarnings(as.numeric(model_data[[v]]))
}

model_data <- model_data %>%
  mutate(log_bdu = log(bdu_val)) %>%
  filter(is.finite(log_bdu))

model_data <- na.omit(model_data)

cat("Number of rows in model_data:", nrow(model_data), "\n")

set.seed(123)
train_idx <- createDataPartition(model_data$log_bdu, p = 0.7, list = FALSE)

train_data <- model_data[train_idx, ]
test_data  <- model_data[-train_idx, ]

x_train <- model.matrix(
  log_bdu ~ . - bdu_val,
  data = train_data
)[, -1]   

x_test <- model.matrix(
  log_bdu ~ . - bdu_val,
  data = test_data
)[, -1]

y_train <- train_data$log_bdu
y_test  <- test_data$log_bdu

dtrain <- xgb.DMatrix(data = x_train, label = y_train)
dtest  <- xgb.DMatrix(data = x_test,  label = y_test)

params <- list(
  objective = "reg:squarederror",
  eval_metric = "rmse",
  max_depth = 6,
  eta = 0.08,
  subsample = 0.8,
  colsample_bytree = 0.8
)

set.seed(123)
xgb_full <- xgb.train(
  params = params,
  data = dtrain,
  nrounds = 800,
  early_stopping_rounds = 40,
  watchlist = list(train = dtrain, test = dtest),
  print_every_n = 50
)

log_pred <- predict(xgb_full, newdata = dtest)
pred <- exp(log_pred)  # €/val

y_test_eur <- test_data$bdu_val

xgb_rmse  <- sqrt(mean((pred - y_test_eur)^2))
xgb_r2    <- cor(pred, y_test_eur)^2
xgb_mape  <- mean(abs((pred - y_test_eur) / y_test_eur)) * 100

cat("XGBoost RMSE:", xgb_rmse, "\n")
cat("XGBoost R²:", xgb_r2, "\n")
cat("XGBoost MAPE:", xgb_mape, "%\n")

imp <- xgb.importance(model = xgb_full)
print(head(imp, 20))

imp <- xgb.importance(model = xgb_full)
head(imp, 20)

library(ggplot2)

plot_data <- data.frame(
  actual = y_test_eur,
  predicted = pred
)

ggplot(plot_data, aes(x = actual, y = predicted)) +
  geom_point(alpha = 0.3) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  labs(
    title = "Actual vs Predicted Wages (XGBoost)",
    x = "Actual wage (EUR/hour)",
    y = "Predicted wage (EUR/hour)"
  ) +
  theme_minimal()


library(SHAPforxgboost)
library(ggplot2)
shap_values <- shap.values(
  xgb_model = xgb_full,
  X_train = x_train
)
shap_long <- shap.prep(
  shap_contrib = shap_values$shap_score,
  X_train = x_train
)
shap.plot.summary(shap_long)


# RANDOM FPREST
library(randomForest)
library(dplyr)
library(caret)

data22 <- filtered_data[year == 2022]

model_data <- data22 %>%
  select(
    bdu_val,           
    lytis,
    amzius,
    stazas,
    issilavinimas,
    virsvalandziu_sk,
    darbo_laiko_dalis,
    Pavadinimas,
    im_dydzio_kodas,
    sutarties_rusis,
    apmoketos_val,
    atostogu_dienos,
    premijos,
    bdu_spalio
  )

model_data <- model_data %>%
  mutate(
    lytis = factor(lytis),
    issilavinimas = factor(issilavinimas),
    Pavadinimas = factor(Pavadinimas),
    evrk_2z = factor(evrk_2z),
    im_dydzio_kodas = factor(im_dydzio_kodas),
    sutarties_rusis = factor(sutarties_rusis)
  )

num_vars <- c(
  "amzius", "stazas", "virsvalandziu_sk",
  "darbo_laiko_dalis", "savaiciu_sk", "apmoketos_val",
  "atostogu_dienos", "premijos", "priemokos",
  "virsvalandziu_bdu", "bdu_spalio"
)

for(v in num_vars){
  model_data[[v]] <- suppressWarnings(as.numeric(model_data[[v]]))
}

# Log-transform
model_data$log_bdu <- log(model_data$bdu_val)

model_data <- na.omit(model_data)

cat("Number of rows for RF model:", nrow(model_data), "\n")

set.seed(123)
train_idx <- createDataPartition(model_data$log_bdu, p = 0.7, list = FALSE)

train_data <- model_data[train_idx, ]
test_data  <- model_data[-train_idx, ]

rf_model <- randomForest(
  log_bdu ~ . - bdu_val,
  data = train_data,
  ntree = 500,
  mtry = 6,
  importance = TRUE
)

print(rf_model)

log_pred <- predict(rf_model, newdata = test_data)
pred <- exp(log_pred)

y_test_eur <- test_data$bdu_val

rf_rmse <- sqrt(mean((pred - y_test_eur)^2))
rf_r2   <- cor(pred, y_test_eur)^2
rf_mape <- mean(abs((pred - y_test_eur)/y_test_eur))*100

cat("RF RMSE:", rf_rmse, "\n")
cat("RF R²:", rf_r2, "\n")
cat("RF MAPE:", rf_mape, "%\n")

importance(rf_model)[1:20, ]
varImpPlot(rf_model, n.var = 20)

library(ggplot2)

plot_data <- data.frame(
  actual = y_test_eur,
  predicted = pred
)

ggplot(plot_data, aes(x = actual, y = predicted)) +
  geom_point(alpha = 0.3) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  labs(
    title = "Actual vs Predicted Wages (Random Forest)",
    x = "Actual wage (EUR/hour)",
    y = "Predicted wage (EUR/hour)"
  ) +
  theme_minimal()

library(randomForest)
library(ggplot2)

imp <- importance(rf_model)
imp_clean <- imp[complete.cases(imp), , drop = FALSE]
top_n <- min(20, nrow(imp_clean))
if (top_n == 0) stop("No valid importance values found (all NA/NaN).")

imp_top <- imp_clean[
  order(imp_clean[, "IncNodePurity"], decreasing = TRUE),
  , drop = FALSE
][seq_len(top_n), , drop = FALSE]

imp_df <- data.frame(
  Feature = rownames(imp_top),
  Importance = imp_top[, "IncNodePurity"],
  row.names = NULL
)

ggplot(imp_df, aes(x = Importance, y = reorder(Feature, Importance))) +
  geom_col(fill = "steelblue") +
  labs(
    title = "Feature Importance (Random Forest)",
    subtitle = "Increase in Node Purity",
    x = "Importance",
    y = ""
  ) +
  theme_minimal(base_size = 12)


# SVR 

library(e1071)
library(dplyr)
library(caret)

data22 <- filtered_data[year == 2022]

model_data <- data22 %>%
  select(
    bdu_val,           
    lytis,
    amzius,
    stazas,
    issilavinimas,
    virsvalandziu_sk,
    darbo_laiko_dalis,
    Pavadinimas,
    im_dydzio_kodas,
    sutarties_rusis,
    apmoketos_val,
    atostogu_dienos,
    premijos,
    bdu_spalio
  )

cat_cols <- c("lytis","issilavinimas","Pavadinimas",
              "im_dydzio_kodas","sutarties_rusis")

model_data[cat_cols] <- lapply(model_data[cat_cols], factor)

num_cols <- c("amzius","stazas","virsvalandziu_sk","darbo_laiko_dalis",
              "apmoketos_val","atostogu_dienos","premijos","bdu_spalio")

for (v in num_cols) {
  model_data[[v]] <- suppressWarnings(as.numeric(model_data[[v]]))
}

# Log transform
model_data$log_bdu <- log(model_data$bdu_val)

model_data <- na.omit(model_data)

X <- model.matrix(log_bdu ~ . - bdu_val, data = model_data)[, -1]
y <- model_data$log_bdu

set.seed(123)
train_idx <- createDataPartition(y, p = 0.7, list = FALSE)

X_train <- X[train_idx, ]
X_test  <- X[-train_idx, ]
y_train <- y[train_idx]
y_test  <- y[-train_idx]


svr_model <- svm(
  x = X_train,
  y = y_train,
  type = "eps-regression",
  kernel = "radial",
  cost = 8,
  gamma = 1 / ncol(X_train),
  epsilon = 0.1,
  scale = TRUE
)

y_pred_log <- predict(svr_model, X_test)
y_pred <- exp(y_pred_log)
y_test_eur <- exp(y_test)

svr_rmse <- sqrt(mean((y_pred - y_test_eur)^2))
svr_r2   <- cor(y_pred, y_test_eur)^2
svr_mape <- mean(abs((y_pred - y_test_eur) / y_test_eur))*100

cat("SVR RMSE:", svr_rmse, "\n")
cat("SVR R²:", svr_r2, "\n")
cat("SVR MAPE:", svr_mape, "%\n")

library(ggplot2)

svr_results <- data.frame(
  Actual = y_test_eur,
  Predicted = y_pred
)

ggplot(svr_results, aes(x = Actual, y = Predicted)) +
  geom_point(alpha = 0.4, size = 1.2) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(
    title = "Actual vs Predicted Wages (SVR)",
    x = "Actual wage (EUR/hour)",
    y = "Predicted wage (EUR/hour)"
  ) +
  theme_minimal()


library(caret)

perm_importance_svr <- function(model, X_test, y_test_log, nrep = 5) {
  
  base_pred <- predict(model, X_test)
  base_rmse <- sqrt(mean((exp(base_pred) - exp(y_test_log))^2))
  
  imp <- sapply(1:ncol(X_test), function(j) {
    rmse_rep <- replicate(nrep, {
      X_perm <- X_test
      X_perm[, j] <- sample(X_perm[, j])
      pred_perm <- predict(model, X_perm)
      sqrt(mean((exp(pred_perm) - exp(y_test_log))^2))
    })
    mean(rmse_rep - base_rmse)
  })
  
  data.frame(
    Feature = colnames(X_test),
    Importance = imp
  )
}

svr_imp <- perm_importance_svr(
  model = svr_model,
  X_test = X_test,
  y_test_log = y_test,
  nrep = 5
)

svr_imp <- svr_imp[order(svr_imp$Importance, decreasing = TRUE), ]
svr_imp_top <- head(svr_imp, 15)

ggplot(svr_imp_top, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Feature Importance (SVR)",
    x = "Feature",
    y = "Increase in RMSE after permutation"
  ) +
  theme_minimal()



#==============================================================
# Predicting Gender Differences in Wages at the Occupational Level
#==============================================================

# Random Forest

library(dplyr)
library(randomForest)
library(caret)
library(ggplot2)

data22 <- filtered_data[year == 2022]

agg_prof_rf <- function(df, min_n = 100) {
  df %>%
    group_by(Pavadinimas) %>%
    summarise(
      mean_wage = mean(bdu_val, na.rm = TRUE),
      mean_age = mean(amzius, na.rm = TRUE),
      mean_tenure = mean(stazas, na.rm = TRUE),
      mean_ot = mean(virsvalandziu_sk, na.rm = TRUE),
      mean_worktime = mean(darbo_laiko_dalis, na.rm = TRUE),
      mean_paid_hours = mean(apmoketos_val, na.rm = TRUE),
      mean_premijos = mean(premijos, na.rm = TRUE),
      n = n(),
      .groups = "drop"
    ) %>%
    filter(n >= min_n) %>%
    as.data.frame()
}

prof_m <- agg_prof_rf(filter(data22, lytis == "M"), min_n = 100)
prof_f <- agg_prof_rf(filter(data22, lytis == "F"), min_n = 100)

cat("Number of professions (men):", nrow(prof_m), "\n")
cat("Number of professions (women):", nrow(prof_f), "\n")

rf_model_fun <- function(prof_data, mtry_val = 3) {
  
  y <- prof_data$mean_wage
  X <- prof_data %>% select(-Pavadinimas, -n, -mean_wage)
  
  set.seed(123)
  idx <- createDataPartition(y, p = 0.7, list = FALSE)
  
  X_train <- X[idx, ]
  X_test  <- X[-idx, ]
  y_train <- y[idx]
  y_test  <- y[-idx]

  rf <- randomForest(
    x = X_train,
    y = y_train,
    ntree = 500,
    mtry = mtry_val,
    importance = TRUE
  )

  pred <- predict(rf, newdata = X_test)

  results_df <- data.frame(
    actual = y_test,
    predicted = pred
  )
  
  list(
    model = rf,
    results = results_df,
    RMSE = sqrt(mean((pred - y_test)^2)),
    R2   = cor(pred, y_test)^2,
    MAPE = mean(abs((pred - y_test) / y_test)) * 100
  )
}

rf_m <- rf_model_fun(prof_m)
rf_f <- rf_model_fun(prof_f)

rf_results <- data.frame(
  Lytis = c("Vyrai", "Moterys"),
  RMSE = c(rf_m$RMSE, rf_f$RMSE),
  R2   = c(rf_m$R2,   rf_f$R2),
  MAPE = c(rf_m$MAPE, rf_f$MAPE)
)

print(rf_results)

# ============================================================
# 7. ACTUAL vs PREDICTED GRAFIKAI
# ============================================================

# ---- Vyrai ----
ggplot(rf_m$results, aes(x = actual, y = predicted)) +
  geom_point(alpha = 0.7) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  labs(
    title = "Actual vs Predicted – Men (Random Forest)",
    x = "The real average salary of the profession",
    y = "Predicted  average salary for the professio"
  ) +
  theme_minimal()

# ---- Moterys ----
ggplot(rf_f$results, aes(x = actual, y = predicted)) +
  geom_point(alpha = 0.7) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  labs(
    title = "Actual vs Predicted – Women (Random Forest)",
    x = "The real average salary of the profession",
    y = "Predicted  average salary for the professio"
  ) +
  theme_minimal()


library(ggplot2)

plot_varimp <- function(rf_model, title_txt) {
  imp <- importance(rf_model, type = 1)
  imp_df <- data.frame(
    Variable = rownames(imp),
    Importance = imp[, 1]
  )
  
  ggplot(imp_df, aes(x = reorder(Variable, Importance), y = Importance)) +
    geom_col(fill = "steelblue") +
    coord_flip() +
    labs(
      title = title_txt,
      x = "Kintamasis",
      y = "% IncMSE"
    ) +
    theme_minimal()
}

plot_varimp(rf_m$model, "The Importance of Variables – Men (Random Forest)")
plot_varimp(rf_f$model, "The Importance of Variables – Women (Random Forest)")


# XGBOOST 

library(dplyr)
library(caret)
library(xgboost)
library(ggplot2)

xgb_model_fun <- function(prof_data) {
  
  y <- prof_data$mean_wage
  X <- prof_data %>% select(-Pavadinimas, -n, -mean_wage)

  set.seed(123)
  idx <- createDataPartition(y, p = 0.7, list = FALSE)
  
  X_train <- as.matrix(X[idx, ])
  X_test  <- as.matrix(X[-idx, ])
  y_train <- y[idx]
  y_test  <- y[-idx]
  
  dtrain <- xgb.DMatrix(data = X_train, label = y_train)
  dtest  <- xgb.DMatrix(data = X_test,  label = y_test)
  
  params <- list(
    booster = "gbtree",
    objective = "reg:squarederror",
    eta = 0.05,
    max_depth = 4,
    subsample = 0.8,
    colsample_bytree = 0.8
  )
  
  xgb <- xgb.train(
    params = params,
    data = dtrain,
    nrounds = 500,
    watchlist = list(train = dtrain, test = dtest),
    verbose = 0
  )

  pred <- predict(xgb, dtest)
  
  results_df <- data.frame(
    actual = y_test,
    predicted = pred
  )
  
  list(
    model = xgb,
    results = results_df,
    RMSE = sqrt(mean((pred - y_test)^2)),
    R2   = cor(pred, y_test)^2,
    MAPE = mean(abs((pred - y_test) / y_test)) * 100
  )
}

xgb_m <- xgb_model_fun(prof_m)
xgb_f <- xgb_model_fun(prof_f)

xgb_results <- data.frame(
  Lytis = c("Vyrai", "Moterys"),
  RMSE = c(xgb_m$RMSE, xgb_f$RMSE),
  R2   = c(xgb_m$R2,   xgb_f$R2),
  MAPE = c(xgb_m$MAPE, xgb_f$MAPE)
)

print(xgb_results)



ggplot(xgb_m$results, aes(actual, predicted)) +
  geom_point(alpha = 0.7) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "Actual vs Predicted – Men (XGBoost)",
    x = "The real average salary of the profession",
    y = "Predicted  average salary for the profession"
  ) +
  theme_minimal()
ggplot(xgb_f$results, aes(actual, predicted)) +
  geom_point(alpha = 0.7) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "Actual vs Predicted – Women (XGBoost)",
    x = "The real average salary of the profession",
    y = "Predicted  average salary for the professio"
  ) +
  theme_minimal()

plot_xgb_varimp <- function(xgb_model, title_txt) {
  
  imp <- xgb.importance(model = xgb_model)
  
  ggplot(imp, aes(x = reorder(Feature, Gain), y = Gain)) +
    geom_col(fill = "blue") +
    coord_flip() +
    labs(
      title = title_txt,
      x = "Feature",
      y = "Gain"
    ) +
    theme_minimal()
}

plot_xgb_varimp(xgb_m$model, "The importance of variables – Men (XGBoost)")
plot_xgb_varimp(xgb_f$model, "The importance of variables – Women (XGBoost)")


library(SHAPforxgboost)
library(ggplot2)
library(dplyr)

X_m <- prof_m %>%
  select(-Pavadinimas, -n, -mean_wage) %>%
  as.matrix()

X_f <- prof_f %>%
  select(-Pavadinimas, -n, -mean_wage) %>%
  as.matrix()

shap_m <- shap.values(
  xgb_model = xgb_m$model,
  X_train = X_m
)

shap_long_m <- shap.prep(
  shap_contrib = shap_m$shap_score,
  X_train = X_m
)

shap_f <- shap.values(
  xgb_model = xgb_f$model,
  X_train = X_f
)

shap_long_f <- shap.prep(
  shap_contrib = shap_f$shap_score,
  X_train = X_f
)

shap.plot.summary(
  shap_long_m,
  scientific = FALSE
) +
  ggtitle("SHAP – Importance of Variables (Men)")


shap.plot.summary(
  shap_long_f,
  scientific = FALSE
) +
  ggtitle("SHAP – Importance of Variables (Women)")


# SVR

library(dplyr)
library(caret)
library(e1071)
library(ggplot2)

svr_model_fun <- function(prof_data,
                          kernel_type = "radial",
                          cost_val = 10,
                          gamma_val = 0.1,
                          epsilon_val = 0.1) {
  
  y <- prof_data$mean_wage
  X <- prof_data %>% select(-Pavadinimas, -n, -mean_wage)

  set.seed(123)
  idx <- createDataPartition(y, p = 0.7, list = FALSE)
  
  X_train <- X[idx, ]
  X_test  <- X[-idx, ]
  y_train <- y[idx]
  y_test  <- y[-idx]

  preproc <- preProcess(X_train, method = c("center", "scale"))
  X_train_sc <- predict(preproc, X_train)
  X_test_sc  <- predict(preproc, X_test)

  svr <- svm(
    x = X_train_sc,
    y = y_train,
    type = "eps-regression",
    kernel = kernel_type,
    cost = cost_val,
    gamma = gamma_val,
    epsilon = epsilon_val
  )

  pred <- predict(svr, X_test_sc)
  
  results_df <- data.frame(
    actual = y_test,
    predicted = pred
  )
  
  list(
    model = svr,
    preproc = preproc,
    results = results_df,
    RMSE = sqrt(mean((pred - y_test)^2)),
    R2   = cor(pred, y_test)^2,
    MAPE = mean(abs((pred - y_test) / y_test)) * 100
  )
}

svr_m <- svr_model_fun(prof_m)
svr_f <- svr_model_fun(prof_f)

svr_results <- data.frame(
  Lytis = c("Vyrai", "Moterys"),
  RMSE = c(svr_m$RMSE, svr_f$RMSE),
  R2   = c(svr_m$R2,   svr_f$R2),
  MAPE = c(svr_m$MAPE, svr_f$MAPE)
)

print(svr_results)


ctrl <- trainControl(method = "cv", number = 5)

svr_tuned <- train(
  mean_wage ~ .,
  data = prof_m %>% select(-Pavadinimas, -n),
  method = "svmRadial",
  preProcess = c("center", "scale"),
  trControl = ctrl,
  tuneLength = 10
)

print(svr_tuned)

svr_tuned_f <- train(
  mean_wage ~ .,
  data = prof_f %>% select(-Pavadinimas, -n),
  method = "svmRadial",
  preProcess = c("center", "scale"),
  trControl = ctrl,
  tuneLength = 10
)
print(svr_tuned_f)

svr_imp_m <- varImp(svr_tuned, scale = TRUE)
svr_imp_m_df <- as.data.frame(svr_imp_m$importance)
svr_imp_m_df$Variable <- rownames(svr_imp_m_df)

ggplot(svr_imp_m_df,
       aes(x = reorder(Variable, Overall), y = Overall)) +
  geom_col(fill = "blue") +
  coord_flip() +
  labs(
    title = "Feature Importance – Men (SVR)",
    x = "Variable",
    y = "Relative importance"
  ) +
  theme_minimal()

svr_imp_f <- varImp(svr_tuned_f, scale = TRUE)
svr_imp_f_df <- as.data.frame(svr_imp_f$importance)
svr_imp_f_df$Variable <- rownames(svr_imp_f_df)

ggplot(svr_imp_f_df,
       aes(x = reorder(Variable, Overall), y = Overall)) +
  geom_col(fill = "blue") +
  coord_flip() +
  labs(
    title = "Feature Importance – Women (SVR)",
    x = "Variable",
    y = "Relative importance"
  ) +
  theme_minimal()

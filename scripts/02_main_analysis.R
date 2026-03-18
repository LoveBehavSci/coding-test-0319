# Main analysis module for the Booth coding test.

# =========================
# Setup
# =========================
required_pkgs <- c("tidyverse", "broom", "scales")
missing_pkgs <- setdiff(required_pkgs, rownames(installed.packages()))
if (length(missing_pkgs) > 0) {
  stop("Please install: ", paste(missing_pkgs, collapse = ", "), call. = FALSE)
}

suppressPackageStartupMessages({
  library(tidyverse)
  library(broom)
  library(scales)
})

options(dplyr.summarise.inform = FALSE, scipen = 999)

dir.create(file.path("output", "figures"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path("output", "tables"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path("data", "derived"), recursive = TRUE, showWarnings = FALSE)

# =========================
# 1) Load cleaned data
# =========================
paths <- list(
  respondent = file.path("data", "derived", "clean_respondent_level.csv"),
  primary = file.path("data", "derived", "clean_primary_sample.csv"),
  strict = file.path("data", "derived", "clean_strict_sample.csv"),
  long = file.path("data", "derived", "clean_feelings_long.csv"),
  audit = file.path("data", "derived", "cleaning_audit_objects.rds")
)

missing_files <- names(paths)[!file.exists(unlist(paths))]
if (length(missing_files) > 0) {
  stop("Missing cleaned file(s): ", paste(missing_files, collapse = ", "), call. = FALSE)
}

respondent_all <- readr::read_csv(paths$respondent, show_col_types = FALSE)
primary <- readr::read_csv(paths$primary, show_col_types = FALSE)
strict <- readr::read_csv(paths$strict, show_col_types = FALSE)
feelings_long <- readr::read_csv(paths$long, show_col_types = FALSE)
audit_objects <- readRDS(paths$audit)

core_required_vars <- c(
  "self_blame_c", "mutual_order_pref", "mutual_avg", "unilateral_avg",
  "feelings_neither", "feelings_youalone", "feelings_themalone",
  "feelings_youaloneforgiven", "feelings_bothyoufirst", "feelings_boththemfirst"
)
missing_core <- setdiff(core_required_vars, names(primary))
if (length(missing_core) > 0) {
  stop("Primary file is missing core analysis variables: ", paste(missing_core, collapse = ", "), call. = FALSE)
}

optional_vars <- c("binary1_choice", "binary2_choice", "scenario_type", "real_conflict")
missing_optional <- setdiff(optional_vars, names(primary))
if (length(missing_optional) > 0) {
  message("Optional variable(s) not found; related checks will be skipped: ",
          paste(missing_optional, collapse = ", "))
}

# =========================
# 2) Descriptive backbone
# =========================
# Start with the within-person outcome profile before moving to the blame slope.
continuous_vars <- c(
  "blame_1", "self_blame_c", "mutual_order_pref", "mutual_avg", "unilateral_avg",
  "feelings_neither", "feelings_youalone", "feelings_themalone",
  "feelings_youaloneforgiven", "feelings_bothyoufirst", "feelings_boththemfirst"
)

descriptive_summary <- primary %>%
  summarise(across(all_of(continuous_vars), list(
    n = ~sum(!is.na(.x)),
    mean = ~mean(.x, na.rm = TRUE),
    sd = ~sd(.x, na.rm = TRUE),
    median = ~median(.x, na.rm = TRUE),
    min = ~min(.x, na.rm = TRUE),
    max = ~max(.x, na.rm = TRUE)
  ))) %>%
  pivot_longer(
    everything(),
    names_to = c("variable", ".value"),
    names_pattern = "^(.*)_(n|mean|sd|median|min|max)$"
  ) %>%
  arrange(match(variable, continuous_vars))

outcome_order <- c(
  "feelings_neither",
  "feelings_youalone",
  "feelings_themalone",
  "feelings_youaloneforgiven",
  "feelings_bothyoufirst",
  "feelings_boththemfirst"
)

outcome_labels <- c(
  feelings_neither = "Neither apologizes",
  feelings_youalone = "I apologize first, other does not",
  feelings_themalone = "Other apologizes first, I do not",
  feelings_youaloneforgiven = "I apologize first, other forgives",
  feelings_bothyoufirst = "I apologize first, then other",
  feelings_boththemfirst = "Other apologizes first, then I"
)

desc_outcomes <- primary %>%
  select(all_of(outcome_order)) %>%
  pivot_longer(everything(), names_to = "outcome_var", values_to = "feeling") %>%
  group_by(outcome_var) %>%
  summarise(
    n = sum(!is.na(feeling)),
    mean = mean(feeling, na.rm = TRUE),
    sd = sd(feeling, na.rm = TRUE),
    se = sd / sqrt(n),
    ci_low = mean - qt(0.975, pmax(n - 1, 1)) * se,
    ci_high = mean + qt(0.975, pmax(n - 1, 1)) * se,
    .groups = "drop"
  ) %>%
  mutate(
    outcome_var = factor(outcome_var, levels = outcome_order),
    outcome_label = factor(outcome_labels[as.character(outcome_var)], levels = outcome_labels[outcome_order])
  )

fig_descriptive <- ggplot(desc_outcomes, aes(x = outcome_label, y = mean)) +
  geom_hline(yintercept = 0, linewidth = 0.3, color = "grey70") +
  geom_point(size = 2.2, color = "grey20") +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.15, color = "grey20") +
  coord_flip() +
  labs(
    x = NULL,
    y = "Mean feeling rating (-30 to 30)",
    title = "Primary sample: average feelings across apology outcomes",
    subtitle = "Error bars are 95% confidence intervals"
  ) +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank())

# =========================
# 3) Focused paired contrasts
# =========================
# Keep the paired contrasts narrow.
paired_contrast <- function(data, x, y, x_label, y_label) {
  dat <- data %>% select(x = all_of(x), y = all_of(y)) %>% drop_na()
  if (nrow(dat) < 2) {
    return(tibble(
      contrast_label = paste0(x_label, " — minus — ", y_label),
      first_var = x,
      second_var = y,
      first_label = x_label,
      second_label = y_label,
      n = nrow(dat),
      first_mean = NA_real_,
      second_mean = NA_real_,
      mean_diff_first_minus_second = NA_real_,
      ci_low_diff = NA_real_,
      ci_high_diff = NA_real_,
      t_statistic = NA_real_,
      df = NA_real_,
      p_value = NA_real_,
      dz_first_minus_second = NA_real_
    ))
  }

  diff <- dat$x - dat$y
  tt <- t.test(dat$x, dat$y, paired = TRUE)

  tibble(
    contrast_label = paste0(x_label, " — minus — ", y_label),
    first_var = x,
    second_var = y,
    first_label = x_label,
    second_label = y_label,
    n = nrow(dat),
    first_mean = mean(dat$x),
    second_mean = mean(dat$y),
    mean_diff_first_minus_second = mean(diff),
    ci_low_diff = unname(tt$conf.int[1]),
    ci_high_diff = unname(tt$conf.int[2]),
    t_statistic = unname(tt$statistic),
    df = unname(tt$parameter),
    p_value = tt$p.value,
    dz_first_minus_second = ifelse(sd(diff) == 0, NA_real_, mean(diff) / sd(diff))
  )
}

paired_results <- bind_rows(
  paired_contrast(
    primary,
    "feelings_boththemfirst",
    "feelings_bothyoufirst",
    "Other apologizes first, then self",
    "Self apologizes first, then other"
  ),
  paired_contrast(
    primary,
    "mutual_avg",
    "feelings_neither",
    "Average mutual apology rating",
    "Neither apologizes"
  ),
  paired_contrast(
    primary,
    "feelings_bothyoufirst",
    "feelings_youaloneforgiven",
    "Self apologizes first, then other",
    "Self apologizes, other forgives (no apology)"
  )
)

# =========================
# 4) Main hypothesis test
# =========================
main_formula <- mutual_order_pref ~ self_blame_c
mod_primary <- lm(main_formula, data = primary)

main_glance <- broom::glance(mod_primary)
main_term <- broom::tidy(mod_primary, conf.int = TRUE) %>%
  filter(term == "self_blame_c") %>%
  mutate(
    sample = "primary",
    n = nobs(mod_primary),
    r_squared = main_glance$r.squared,
    adj_r_squared = main_glance$adj.r.squared,
    slope_per_10_blame_points = estimate * 10,
    std_beta = estimate * sd(primary$self_blame_c, na.rm = TRUE) / sd(primary$mutual_order_pref, na.rm = TRUE)
  )

main_regression_summary <- main_term %>%
  transmute(
    sample, n,
    slope = estimate,
    se = std.error,
    t = statistic,
    p = p.value,
    ci_low = conf.low,
    ci_high = conf.high,
    r_squared,
    adj_r_squared,
    std_beta,
    effect_per_10_blame_points = slope_per_10_blame_points
  )

fig_main <- ggplot(primary, aes(x = self_blame_c, y = mutual_order_pref)) +
  geom_hline(yintercept = 0, linewidth = 0.3, color = "grey70") +
  geom_vline(xintercept = 0, linewidth = 0.3, color = "grey80") +
  geom_point(alpha = 0.75, size = 2, color = "grey30") +
  geom_smooth(method = "lm", se = TRUE, linewidth = 0.8, color = "black", fill = alpha("grey50", 0.2)) +
  labs(
    x = "Centered self-blame (blame_1 - 50)",
    y = "Relative preference for other-first mutual apology",
    title = "Main test: blame and mutual apology-order preference"
  ) +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank())

# =========================
# 5) Robustness checks
# =========================
# Use the strict sample as a check, not the headline analysis.
mod_strict <- lm(main_formula, data = strict)
strict_term <- broom::tidy(mod_strict, conf.int = TRUE) %>%
  filter(term == "self_blame_c") %>%
  mutate(
    sample = "strict",
    n = nobs(mod_strict),
    r_squared = broom::glance(mod_strict)$r.squared,
    adj_r_squared = broom::glance(mod_strict)$adj.r.squared,
    slope_per_10_blame_points = estimate * 10
  )

main_vs_strict <- bind_rows(main_term, strict_term) %>%
  transmute(
    sample, n,
    slope = estimate,
    se = std.error,
    t = statistic,
    p = p.value,
    ci_low = conf.low,
    ci_high = conf.high,
    r_squared,
    effect_per_10_blame_points = slope_per_10_blame_points
  )

# Supporting directional check, not a second main test.
binary_directional_note <- tibble(
  note = "Supporting directional check: binary1 choice (self-first mutual vs neither), not the main mutual-order contrast."
)

binary_directional_check <- tibble()
if ("binary1_choice" %in% names(primary)) {
  binary_validation_data <- primary %>%
    filter(binary1_choice %in% c("self_first_mutual", "neither")) %>%
    mutate(binary1_self_first = if_else(binary1_choice == "self_first_mutual", 1, 0))

  if (nrow(binary_validation_data) >= 10 && n_distinct(binary_validation_data$binary1_self_first) == 2) {
    mod_binary <- glm(binary1_self_first ~ self_blame_c, data = binary_validation_data, family = binomial())
    b <- broom::tidy(mod_binary) %>% filter(term == "self_blame_c")

    binary_directional_check <- b %>%
      mutate(
        n = nobs(mod_binary),
        odds_ratio = exp(estimate),
        conf.low = estimate - 1.96 * std.error,
        conf.high = estimate + 1.96 * std.error,
        or_conf.low = exp(conf.low),
        or_conf.high = exp(conf.high)
      ) %>%
      transmute(
        term, n,
        logit_slope = estimate,
        se = std.error,
        z = statistic,
        p = p.value,
        odds_ratio,
        or_ci_low = or_conf.low,
        or_ci_high = or_conf.high
      )
  } else {
    message("Skipping binary directional check: insufficient usable variation in binary1_choice.")
  }
} else {
  message("Skipping binary directional check: binary1_choice is not available.")
}

# Optional subgroup check: real conflicts only.
real_subgroup <- tibble()
if ("scenario_type" %in% names(primary)) {
  primary_real <- primary %>% filter(scenario_type == "real")
  if (nrow(primary_real) >= 20) {
    mod_real <- lm(main_formula, data = primary_real)
    real_subgroup <- broom::tidy(mod_real, conf.int = TRUE) %>%
      filter(term == "self_blame_c") %>%
      mutate(
        sample = "primary_real_only",
        n = nobs(mod_real),
        r_squared = broom::glance(mod_real)$r.squared,
        slope_per_10_blame_points = estimate * 10
      ) %>%
      transmute(
        sample, n,
        slope = estimate,
        se = std.error,
        t = statistic,
        p = p.value,
        ci_low = conf.low,
        ci_high = conf.high,
        r_squared,
        effect_per_10_blame_points = slope_per_10_blame_points
      )
  }
} else {
  message("Skipping real-conflict subgroup check: scenario_type is not available.")
}

# Optional nonparametric check.
spearman_check <- primary %>%
  select(self_blame_c, mutual_order_pref) %>%
  drop_na() %>%
  summarise(
    n = n(),
    rho = suppressWarnings(cor(self_blame_c, mutual_order_pref, method = "spearman")),
    p = suppressWarnings(cor.test(self_blame_c, mutual_order_pref, method = "spearman", exact = FALSE)$p.value)
  )

# =========================
# 6) Save compact result objects
# =========================
result_tables <- list(
  descriptive_summary = descriptive_summary,
  desc_outcomes = desc_outcomes,
  paired_results = paired_results,
  main_model = main_regression_summary,
  main_vs_strict = main_vs_strict,
  binary_directional_check = binary_directional_check,
  binary_directional_note = binary_directional_note,
  real_subgroup = real_subgroup,
  spearman_check = spearman_check,
  sample_flow_from_cleaning = audit_objects$sample_flow
)

analysis_objects <- list(
  data_info = list(
    n_all = nrow(respondent_all),
    n_primary = nrow(primary),
    n_strict = nrow(strict)
  ),
  tables = result_tables,
  models = list(
    mod_primary = mod_primary,
    mod_strict = mod_strict
  ),
  figures = list(
    fig_descriptive = fig_descriptive,
    fig_main = fig_main
  )
)

# =========================
# 7) Save key outputs
# =========================
ggsave(
  filename = file.path("output", "figures", "figure_01_primary_outcome_profile.png"),
  plot = fig_descriptive,
  width = 7.2, height = 4.8, dpi = 300
)

ggsave(
  filename = file.path("output", "figures", "figure_02_main_blame_mutual_order.png"),
  plot = fig_main,
  width = 6.8, height = 4.8, dpi = 300
)

readr::write_csv(descriptive_summary, file.path("output", "tables", "table_descriptive_summary_primary.csv"))
readr::write_csv(desc_outcomes, file.path("output", "tables", "table_outcome_profile_primary.csv"))
readr::write_csv(paired_results, file.path("output", "tables", "table_paired_contrasts_primary.csv"))
readr::write_csv(main_regression_summary, file.path("output", "tables", "table_main_regression_primary.csv"))
readr::write_csv(main_vs_strict, file.path("output", "tables", "table_main_vs_strict_slope.csv"))
if (nrow(binary_directional_check) > 0) {
  readr::write_csv(binary_directional_check, file.path("output", "tables", "table_binary_directional_check.csv"))
}
if (nrow(real_subgroup) > 0) {
  readr::write_csv(real_subgroup, file.path("output", "tables", "table_real_conflict_subgroup_check.csv"))
}
readr::write_csv(spearman_check, file.path("output", "tables", "table_spearman_check.csv"))

saveRDS(analysis_objects, file.path("data", "derived", "analysis_results_objects.rds"))

cat("\nAnalysis module finished.\n")
cat("Primary N:", nrow(primary), "| Strict N:", nrow(strict), "\n")

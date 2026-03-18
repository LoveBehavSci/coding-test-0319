# Cleaning module for the Booth coding test.

# =========================
# Setup
# =========================
required_pkgs <- c("tidyverse", "readxl")
missing_pkgs <- setdiff(required_pkgs, rownames(installed.packages()))
if (length(missing_pkgs) > 0) {
  stop("Please install: ", paste(missing_pkgs, collapse = ", "), call. = FALSE)
}

suppressPackageStartupMessages({
  library(tidyverse)
  library(readxl)
})

options(dplyr.summarise.inform = FALSE, scipen = 999)

# Simple name cleaner to avoid extra dependencies.
clean_names_simple <- function(x) {
  out <- tolower(x)
  out <- gsub("[^a-z0-9]+", "_", out)
  out <- gsub("^_+|_+$", "", out)
  out <- gsub("_+", "_", out)
  out
}

to_bool <- function(x) {
  if (is.logical(x)) return(x)
  x <- tolower(trimws(as.character(x)))
  case_when(
    x %in% c("true", "1", "yes", "y", "finished", "complete") ~ TRUE,
    x %in% c("false", "0", "no", "n", "unfinished", "incomplete") ~ FALSE,
    TRUE ~ NA
  )
}

first_present <- function(nm, candidates) {
  hit <- intersect(candidates, nm)
  if (length(hit) == 0) NA_character_ else hit[[1]]
}

# =========================
# Import workbook
# =========================
candidate_paths <- c(
  file.path("data", "raw", "Data - Winter 2026.xlsx"),
  file.path("Behavioral RP Recruiting Winter 26-selected", "Data - Winter 2026.xlsx"),
  "Data - Winter 2026.xlsx"
)

workbook_path <- candidate_paths[file.exists(candidate_paths)][1]
if (length(workbook_path) == 0 || is.na(workbook_path)) {
  stop("Could not locate the raw workbook. Update `candidate_paths`.", call. = FALSE)
}

sheet_names <- excel_sheets(workbook_path)

data_sheet <- sheet_names[str_detect(tolower(sheet_names), "^data$|response|raw")][1]
if (is.na(data_sheet)) data_sheet <- sheet_names[1]

codebook_sheet <- sheet_names[str_detect(tolower(sheet_names), "key|codebook|variable|meaning")][1]

raw_df <- read_excel(workbook_path, sheet = data_sheet)
names(raw_df) <- clean_names_simple(names(raw_df))

codebook_df <- tibble()
if (!is.na(codebook_sheet)) {
  codebook_df <- read_excel(workbook_path, sheet = codebook_sheet)
  names(codebook_df) <- clean_names_simple(names(codebook_df))
}

cat("Workbook:", workbook_path, "\n")
cat("Data sheet:", data_sheet, "\n")
cat("Rows x cols:", nrow(raw_df), "x", ncol(raw_df), "\n")

# =========================
# Identify key variables
# =========================
nm <- names(raw_df)

vars <- list(
  response_id = first_present(nm, c("response_id", "responseid")),
  consent = first_present(nm, c("consent")),
  finished = first_present(nm, c("finished")),
  progress = first_present(nm, c("progress")),
  duration = first_present(nm, c("duration_in_seconds", "duration_seconds")),
  passedattn = first_present(nm, c("passedattn")),
  attention_text = first_present(nm, c("attention_3_text", "attention_text")),
  real_imaginary = first_present(nm, c("real_imaginary")),
  describe = first_present(nm, c("describe")),
  comments = first_present(nm, c("comments")),
  feelings_exp = first_present(nm, c("feelings_exp")),
  f_youalone = first_present(nm, c("feelings_youalone")),
  f_bothyoufirst = first_present(nm, c("feelings_bothyoufirst")),
  f_themalone = first_present(nm, c("feelings_themalone")),
  f_boththemfirst = first_present(nm, c("feelings_boththemfirst")),
  f_neither = first_present(nm, c("feelings_neither")),
  f_youaloneforgiven = first_present(nm, c("feelings_youaloneforgiven")),
  outcome_binary1 = first_present(nm, c("outcome_binary1")),
  outcome_binary2 = first_present(nm, c("outcome_binary2")),
  blame = first_present(nm, c("blame_1", "blame")),
  age = first_present(nm, c("age")),
  sex = first_present(nm, c("sex")),
  target_sex = first_present(nm, c("target_sex")),
  do1 = first_present(nm, c("feelings_do_1")),
  do2 = first_present(nm, c("feelings_do_2")),
  do3 = first_present(nm, c("feelings_do_3")),
  do4 = first_present(nm, c("feelings_do_4")),
  do5 = first_present(nm, c("feelings_do_5")),
  do6 = first_present(nm, c("feelings_do_6"))
)

required <- c("consent", "finished", "progress", "passedattn", "attention_text", "real_imaginary", "f_bothyoufirst", "f_boththemfirst", "blame")
missing_required <- required[map_lgl(vars[required], is.na)]
if (length(missing_required) > 0) {
  stop("Missing key variable(s): ", paste(missing_required, collapse = ", "), call. = FALSE)
}

variable_map <- enframe(vars, name = "concept", value = "column_name")

# =========================
# Build standardized frame
# =========================
df <- raw_df |>
  mutate(
    respondent_id = if (!is.na(vars$response_id)) as.character(.data[[vars$response_id]]) else paste0("row_", row_number()),
    consent = as.character(.data[[vars$consent]]),
    finished = to_bool(.data[[vars$finished]]),
    progress = suppressWarnings(as.numeric(.data[[vars$progress]])),
    duration_sec = if (!is.na(vars$duration)) suppressWarnings(as.numeric(.data[[vars$duration]])) else NA_real_,
    passedattn_raw = as.character(.data[[vars$passedattn]]),
    attention_text_raw = as.character(.data[[vars$attention_text]]),
    attention_text_std = str_to_lower(str_trim(attention_text_raw)),
    real_imaginary = as.character(.data[[vars$real_imaginary]]),
    describe = if (!is.na(vars$describe)) as.character(.data[[vars$describe]]) else NA_character_,
    comments = if (!is.na(vars$comments)) as.character(.data[[vars$comments]]) else NA_character_,
    feelings_exp = if (!is.na(vars$feelings_exp)) as.character(.data[[vars$feelings_exp]]) else NA_character_,
    feelings_youalone = suppressWarnings(as.numeric(.data[[vars$f_youalone]])),
    feelings_bothyoufirst = suppressWarnings(as.numeric(.data[[vars$f_bothyoufirst]])),
    feelings_themalone = suppressWarnings(as.numeric(.data[[vars$f_themalone]])),
    feelings_boththemfirst = suppressWarnings(as.numeric(.data[[vars$f_boththemfirst]])),
    feelings_neither = suppressWarnings(as.numeric(.data[[vars$f_neither]])),
    feelings_youaloneforgiven = suppressWarnings(as.numeric(.data[[vars$f_youaloneforgiven]])),
    outcome_binary1 = as.character(.data[[vars$outcome_binary1]]),
    outcome_binary2 = as.character(.data[[vars$outcome_binary2]]),
    blame_1 = suppressWarnings(as.numeric(.data[[vars$blame]])),
    age = if (!is.na(vars$age)) suppressWarnings(as.numeric(.data[[vars$age]])) else NA_real_,
    sex = if (!is.na(vars$sex)) as.character(.data[[vars$sex]]) else NA_character_,
    target_sex = if (!is.na(vars$target_sex)) as.character(.data[[vars$target_sex]]) else NA_character_,
    feelings_do_1 = if (!is.na(vars$do1)) suppressWarnings(as.numeric(.data[[vars$do1]])) else NA_real_,
    feelings_do_2 = if (!is.na(vars$do2)) suppressWarnings(as.numeric(.data[[vars$do2]])) else NA_real_,
    feelings_do_3 = if (!is.na(vars$do3)) suppressWarnings(as.numeric(.data[[vars$do3]])) else NA_real_,
    feelings_do_4 = if (!is.na(vars$do4)) suppressWarnings(as.numeric(.data[[vars$do4]])) else NA_real_,
    feelings_do_5 = if (!is.na(vars$do5)) suppressWarnings(as.numeric(.data[[vars$do5]])) else NA_real_,
    feelings_do_6 = if (!is.na(vars$do6)) suppressWarnings(as.numeric(.data[[vars$do6]])) else NA_real_
  )

# =========================
# Audit ranges and levels
# =========================
range_audit <- tribble(
  ~var, ~expected_min, ~expected_max,
  "feelings_youalone", -30, 30,
  "feelings_bothyoufirst", -30, 30,
  "feelings_themalone", -30, 30,
  "feelings_boththemfirst", -30, 30,
  "feelings_neither", -30, 30,
  "feelings_youaloneforgiven", -30, 30,
  "blame_1", 0, 100,
  "age", 18, 100
) |>
  rowwise() |>
  mutate(
    n_missing = sum(is.na(df[[var]])),
    observed_min = suppressWarnings(min(df[[var]], na.rm = TRUE)),
    observed_max = suppressWarnings(max(df[[var]], na.rm = TRUE)),
    out_of_range_n = sum(!is.na(df[[var]]) & (df[[var]] < expected_min | df[[var]] > expected_max))
  ) |>
  ungroup()

level_audit <- list(
  consent = count(df, consent, sort = TRUE),
  finished = count(df, finished, sort = TRUE),
  passedattn_raw = count(df, passedattn_raw, sort = TRUE),
  real_imaginary = count(df, real_imaginary, sort = TRUE),
  outcome_binary1 = count(df, outcome_binary1, sort = TRUE),
  outcome_binary2 = count(df, outcome_binary2, sort = TRUE),
  attention_text_raw = count(df, attention_text_raw, sort = TRUE)
)

# =========================
# Verify attention check
# =========================
# Rebuild the attention flag from typed text before using the supplied pass/fail field.
df <- df |>
  mutate(
    attention_pass_rebuilt = attention_text_std == "banana",
    attention_pass_supplied = case_when(
      str_to_lower(str_trim(passedattn_raw)) %in% c("yes", "pass", "passed", "true", "1") ~ TRUE,
      str_to_lower(str_trim(passedattn_raw)) %in% c("no", "fail", "failed", "false", "0") ~ FALSE,
      TRUE ~ NA
    ),
    attention_match = attention_pass_rebuilt == attention_pass_supplied,
    attention_pass = attention_pass_rebuilt
  )

attention_audit <- list(
  discrepancy = count(df, attention_pass_rebuilt, attention_pass_supplied, attention_match, sort = TRUE),
  mismatches = df |> filter(!is.na(attention_match), !attention_match) |>
    select(respondent_id, attention_text_raw, passedattn_raw, attention_pass_rebuilt, attention_pass_supplied)
)

# =========================
# Quality flags and missingness
# =========================
# Keep flags separate from exclusions so sample construction stays readable.
df <- df |>
  mutate(
    flag_unfinished = !finished,
    flag_attention_fail = !attention_pass,
    flag_missing_main = is.na(blame_1) | is.na(feelings_bothyoufirst) | is.na(feelings_boththemfirst),
    flag_age_implausible = !is.na(age) & (age < 18 | age > 100),
    flag_clickthrough_comment = str_detect(str_to_lower(coalesce(comments, "")), "clicking through|don't use these data|dont use these data|just clicking")
  )

missingness_summary <- df |>
  summarise(across(
    c(finished, progress, attention_pass, real_imaginary, blame_1,
      feelings_bothyoufirst, feelings_boththemfirst, outcome_binary1, outcome_binary2,
      age, describe, feelings_exp, comments),
    ~ sum(is.na(.x))
  )) |>
  pivot_longer(everything(), names_to = "variable", values_to = "n_missing") |>
  mutate(pct_missing = 100 * n_missing / nrow(df)) |>
  arrange(desc(pct_missing))

quality_flag_summary <- df |>
  summarise(
    n_unfinished = sum(flag_unfinished, na.rm = TRUE),
    n_attention_fail = sum(flag_attention_fail, na.rm = TRUE),
    n_missing_main = sum(flag_missing_main, na.rm = TRUE),
    n_age_implausible = sum(flag_age_implausible, na.rm = TRUE),
    n_clickthrough = sum(flag_clickthrough_comment, na.rm = TRUE)
  )

# =========================
# Construct samples
# =========================
df <- df |>
  mutate(
    include_primary = consent == "AGREE" & finished & attention_pass & !flag_missing_main,
    include_strict = include_primary & !flag_age_implausible & !flag_clickthrough_comment
  )

sample_flow <- tibble(
  step = c(
    "Raw rows",
    "Consent == AGREE",
    "Finished == TRUE",
    "Attention pass",
    "Main vars observed",
    "Primary sample",
    "Strict sample"
  ),
  n = c(
    nrow(df),
    sum(df$consent == "AGREE", na.rm = TRUE),
    sum(df$consent == "AGREE" & df$finished, na.rm = TRUE),
    sum(df$consent == "AGREE" & df$finished & df$attention_pass, na.rm = TRUE),
    sum(df$consent == "AGREE" & df$finished & df$attention_pass & !df$flag_missing_main, na.rm = TRUE),
    sum(df$include_primary, na.rm = TRUE),
    sum(df$include_strict, na.rm = TRUE)
  )
) |>
  mutate(dropped_from_previous = lag(n, default = first(n)) - n)

# =========================
# Construct analysis variables
# =========================
df_clean <- df |>
  mutate(
    # Main planned moderator and outcome
    self_blame_c = blame_1 - 50,
    mutual_order_pref = feelings_boththemfirst - feelings_bothyoufirst,

    # Supporting summaries
    mutual_avg = (feelings_boththemfirst + feelings_bothyoufirst) / 2,
    unilateral_avg = (feelings_youalone + feelings_themalone) / 2,

    # Simple recodes used later in figures and checks
    scenario_type = case_when(
      str_detect(real_imaginary, regex("real argument/conflict", ignore_case = TRUE)) ~ "real",
      str_detect(real_imaginary, regex("imagining myself", ignore_case = TRUE)) ~ "imagined",
      TRUE ~ NA_character_
    ),
    real_conflict = case_when(
      scenario_type == "real" ~ 1,
      scenario_type == "imagined" ~ 0,
      TRUE ~ NA_real_
    ),
    binary1_choice = case_when(
      str_detect(outcome_binary1, regex("^i apologize first, then", ignore_case = TRUE)) ~ "self_first_mutual",
      str_detect(outcome_binary1, regex("^neither i nor", ignore_case = TRUE)) ~ "neither",
      TRUE ~ NA_character_
    ),
    binary2_choice = case_when(
      str_detect(outcome_binary2, regex("does not apologize after", ignore_case = TRUE)) ~ "self_only",
      str_detect(outcome_binary2, regex("^neither i nor", ignore_case = TRUE)) ~ "neither",
      TRUE ~ NA_character_
    )
  ) |>
  mutate(
    scenario_type = factor(scenario_type, levels = c("real", "imagined")),
    binary1_choice = factor(binary1_choice, levels = c("self_first_mutual", "neither")),
    binary2_choice = factor(binary2_choice, levels = c("self_only", "neither"))
  )

primary_sample <- df_clean |> filter(include_primary)
strict_sample <- df_clean |> filter(include_strict)

# =========================
# Long format for repeated outcomes
# =========================
# Long file for repeated-outcome plots and summaries.
outcome_lookup <- tribble(
  ~outcome_var, ~outcome_label, ~family, ~order_label,
  "feelings_youalone", "I apologize first, other does not", "unilateral", "self_first",
  "feelings_bothyoufirst", "I apologize first, then other apologizes", "mutual", "self_first",
  "feelings_themalone", "Other apologizes first, I do not", "unilateral", "other_first",
  "feelings_boththemfirst", "Other apologizes first, then I apologize", "mutual", "other_first",
  "feelings_neither", "Neither apologizes", "neither", "none",
  "feelings_youaloneforgiven", "I apologize first, other forgives (no apology)", "forgiven_only", "self_first"
)

feelings_long <- df_clean |>
  select(
    respondent_id, include_primary, include_strict,
    self_blame_c, blame_1, scenario_type, real_conflict,
    all_of(outcome_lookup$outcome_var)
  ) |>
  pivot_longer(
    cols = all_of(outcome_lookup$outcome_var),
    names_to = "outcome_var",
    values_to = "feeling_score"
  ) |>
  left_join(outcome_lookup, by = "outcome_var")

# Small check used in the appendix write-up.
within_subject_check <- list(
  n_rows = nrow(df_clean),
  n_with_all_6_feelings = sum(rowSums(is.na(df_clean[, outcome_lookup$outcome_var])) == 0),
  do_1_5_permutation_ok = {
    do_df <- df_clean |>
      filter(!is.na(feelings_do_1), !is.na(feelings_do_2), !is.na(feelings_do_3), !is.na(feelings_do_4), !is.na(feelings_do_5))
    do_vals <- as.matrix(do_df[, c("feelings_do_1", "feelings_do_2", "feelings_do_3", "feelings_do_4", "feelings_do_5")])
    ok <- apply(do_vals, 1, function(x) all(sort(x) == 1:5))
    tibble(n_ok = sum(ok), n_checked = length(ok))
  }
)

# =========================
# Export essentials
# =========================
dir.create(file.path("data", "derived"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path("output", "tables"), recursive = TRUE, showWarnings = FALSE)

write_csv(df_clean, file.path("data", "derived", "clean_respondent_level.csv"))
write_csv(primary_sample, file.path("data", "derived", "clean_primary_sample.csv"))
write_csv(strict_sample, file.path("data", "derived", "clean_strict_sample.csv"))
write_csv(feelings_long, file.path("data", "derived", "clean_feelings_long.csv"))
write_csv(sample_flow, file.path("output", "tables", "sample_flow.csv"))

# Save compact audit objects for the appendix and report.
audit_objects <- list(
  variable_map = variable_map,
  range_audit = range_audit,
  level_audit = level_audit,
  attention_audit = attention_audit,
  missingness_summary = missingness_summary,
  quality_flag_summary = quality_flag_summary,
  sample_flow = sample_flow,
  within_subject_check = within_subject_check
)

saveRDS(audit_objects, file.path("data", "derived", "cleaning_audit_objects.rds"))

cat("\nCleaning module finished.\n")
cat("Primary N:", nrow(primary_sample), "| Strict N:", nrow(strict_sample), "\n")

#!/usr/bin/env Rscript

if (!requireNamespace("pacman", quietly = TRUE)) {
  stop("Package 'pacman' must be installed before running this script.")
}

pacman::p_load(
  tidyverse,
  haven,
  fs,
  glue
)

# File paths and optional ESS benchmark input.
input_file <- fs::path("data", "POLITSOLID_Pretest_interim_06032026.sav")
output_dir <- fs::path("output", "univariate_checks")
benchmark_file <- fs::path("benchmarks", "ess_benchmarks.csv")
has_cld3 <- requireNamespace("cld3", quietly = TRUE)

fs::dir_create(output_dir, recurse = TRUE)

# SPSS uses labelled values such as 88/99 for nonresponse. We detect them by label
# because the exact codes vary across items.
nonresponse_regex <- regex(
  "prefer not|do not know|don't know|blank or null|no answer",
  ignore_case = TRUE
)

# Extract nonresponse codes from value labels so they can be recoded to NA.
labelled_nonresponse_values <- function(x) {
  attr(x, "labels") %>%
    `%||%`(c()) %>%
    enframe(name = "label", value = "value") %>%
    filter(str_detect(label, nonresponse_regex)) %>%
    pull(value) %>%
    as.numeric()
}

# Standardize vectors from SPSS into either numeric or text representations.
clean_vector <- function(x, as = c("numeric", "text")) {
  as <- match.arg(as)

  if (inherits(x, "POSIXt")) {
    return(x)
  }

  if (is.character(x)) {
    cleaned_text <- x %>% str_squish() %>% na_if("")
    return(if (as == "text") cleaned_text else suppressWarnings(as.numeric(cleaned_text)))
  }

  if (is.logical(x)) {
    return(if (as == "numeric") as.numeric(x) else as.character(x))
  }

  y <- x
  nonresponse_values <- labelled_nonresponse_values(y)

  if (length(nonresponse_values) > 0) {
    y[y %in% nonresponse_values] <- NA
  }

  if (inherits(y, c("haven_labelled", "labelled"))) {
    if (as == "numeric") {
      y <- haven::zap_labels(y)
    } else {
      y <- haven::as_factor(y, levels = "labels")
    }
  }

  if (as == "numeric") {
    suppressWarnings(as.numeric(y))
  } else {
    as.character(y) %>%
      str_squish() %>%
      na_if("")
  }
}

# Collapse country-specific columns, for example income and vote-open fields.
coalesce_clean_columns <- function(data, columns, as = c("numeric", "text")) {
  as <- match.arg(as)

  columns %>%
    map(~ clean_vector(data[[.x]], as = as)) %>%
    reduce(coalesce)
}

# Row means should return NA if all component items are missing.
row_mean_or_na <- function(data) {
  out <- rowMeans(data, na.rm = TRUE)
  out[rowSums(!is.na(data)) == 0] <- NA_real_
  out
}

# Dominant category is used for the "more than 50%" checks.
mode_summary <- function(x) {
  summary_table <- x %>%
    tibble(value = .) %>%
    filter(!is.na(value)) %>%
    count(value, sort = TRUE, name = "n") %>%
    mutate(share = n / sum(n)) %>%
    slice(1)

  if (nrow(summary_table) == 0) {
    return(
      tibble(
        dominant_category = NA_character_,
        dominant_n = NA_integer_,
        dominant_share = NA_real_
      )
    )
  }

  summary_table %>%
    transmute(
      dominant_category = as.character(value),
      dominant_n = n,
      dominant_share = share
    )
}

# Build one compact summary table for either all respondents or one country.
variable_summary <- function(data, variables, group_value = "Overall") {
  variables %>%
    map_dfr(function(var_name) {
      vector_numeric <- clean_vector(data[[var_name]], as = "numeric")
      vector_character <- clean_vector(data[[var_name]], as = "text")
      non_missing_numeric <- vector_numeric[!is.na(vector_numeric)]
      non_missing_character <- vector_character[!is.na(vector_character)]
      n_distinct_values <- n_distinct(non_missing_character)
      dominant <- mode_summary(vector_character)

      tibble(
        group = group_value,
        variable = var_name,
        label = attr(data[[var_name]], "label") %||% var_name,
        n = length(vector_character),
        n_missing = sum(is.na(vector_character)),
        missing_pct = mean(is.na(vector_character)) * 100,
        n_non_missing = sum(!is.na(vector_character)),
        n_distinct = n_distinct_values,
        mean = if (length(non_missing_numeric) > 0) mean(non_missing_numeric) else NA_real_,
        sd = if (length(non_missing_numeric) > 1) sd(non_missing_numeric) else NA_real_,
        median = if (length(non_missing_numeric) > 0) median(non_missing_numeric) else NA_real_,
        min = if (length(non_missing_numeric) > 0) min(non_missing_numeric) else NA_real_,
        max = if (length(non_missing_numeric) > 0) max(non_missing_numeric) else NA_real_,
        dominant_category = dominant$dominant_category %||% NA_character_,
        dominant_n = dominant$dominant_n %||% NA_integer_,
        dominant_share = dominant$dominant_share %||% NA_real_
      )
    })
}

# Expected survey language by country. `cld3` returns ISO language codes.
country_language_map <- tribble(
  ~country, ~expected_language_code, ~expected_language,
  "Croatia", "hr", "Croatian",
  "France", "fr", "French",
  "Germany", "de", "German",
  "Italy", "it", "Italian",
  "Poland", "pl", "Polish",
  "Spain", "es", "Spanish",
  "Sweden", "sv", "Swedish"
)

# Use cld3 when installed; otherwise keep the review file but mark detection as unavailable.
detect_language_cld3 <- function(text) {
  token_n <- str_count(text, boundary("word"))

  if (token_n < 3) {
    return(
      tibble(
        detected_language_code = NA_character_,
        token_n = token_n,
        detection_status = "too_short"
      )
    )
  }

  if (!has_cld3) {
    return(
      tibble(
        detected_language_code = NA_character_,
        token_n = token_n,
        detection_status = "cld3_not_installed"
      )
    )
  }

  tibble(
    detected_language_code = cld3::detect_language(text),
    token_n = token_n,
    detection_status = "detected"
  )
}

# Read the SPSS file and create a cleaned analysis dataset.
raw_data <- read_sav(input_file)

country_numeric <- clean_vector(raw_data$Country, as = "numeric")

analysis_data <- raw_data %>%
  mutate(across(where(is.character), ~ .x %>% str_squish() %>% na_if(""))) %>%
  filter(!is.na(country_numeric)) %>%
  mutate(
    country = clean_vector(Country, as = "text"),
    field_method = clean_vector(FIELD_METHOD, as = "text"),
    device_type = clean_vector(Sniffer_device_type_final, as = "text"),
    device_group = case_when(
      device_type %in% c("Smartphone", "Tablet") ~ "Mobile",
      device_type == "Laptop/PC" ~ "Desktop",
      TRUE ~ NA_character_
    ),
    voteprsp_open = {
      voteprsp_open_data <- pick(
        voteprspopen_HR,
        voteprspopen_FR,
        voteprspopen_DE,
        voteprspopen_IT,
        voteprspopen_PL,
        voteprspopen_ES,
        voteprspopen_SE
      )
      coalesce_clean_columns(voteprsp_open_data, names(voteprsp_open_data), as = "text")
    },
    hincome = {
      hincome_data <- pick(
        hincome_HR,
        hincome_FR,
        hincome_DE,
        hincome_IT,
        hincome_PL,
        hincome_ES,
        hincome_SE
      )
      coalesce_clean_columns(hincome_data, names(hincome_data), as = "numeric")
    },
    comment = clean_vector(comment, as = "text"),
    plsol_priority_mean = {
      plsol_priority_data <- pick(plsolchld, plsolold, plsolpoor) %>%
        mutate(across(everything(), ~ clean_vector(.x, as = "numeric")))
      row_mean_or_na(plsol_priority_data)
    }
  )

# Define variable groups used by the requested checks.
metadata_vars <- names(analysis_data) %>%
  keep(~ str_detect(.x, "^(Respondent_Serial|Respondent_ID|DataCollection_|FIELD_METHOD|Country$|Country_adjective$|Sniffer_|LPS_|LPSSS_|REGION_)")) %>%
  union(
    names(analysis_data) %>%
      keep(~ str_detect(.x, "(_capture_Order$|_Codes(_\\d+)?$|_Ask$)"))
  ) %>%
  union(c("country", "field_method", "device_type", "device_group", "Rand_GROUP", "id_order"))

open_text_vars <- c(
  "voteprspopen_HR",
  "voteprspopen_FR",
  "voteprspopen_DE",
  "voteprspopen_IT",
  "voteprspopen_PL",
  "voteprspopen_ES",
  "voteprspopen_SE",
  "voteprsp_open",
  "comment"
)

country_specific_vote_vars <- c(
  "voteprsp_HR",
  "voteprsp_FR",
  "voteprsp_DE",
  "voteprsp_IT",
  "voteprsp_PL",
  "voteprsp_ES",
  "voteprsp_SE"
)

sociodemographic_vars <- c(
  "Respondent_Gender",
  "Respondent_Age",
  "EducationAchievedKP",
  "ISCEDRecodeKP",
  "ISCEDKP",
  "EmploymentStatusKP",
  "EMP01KP",
  "MARITALSTATUSKP",
  "econhour",
  "eduyrs",
  "partner",
  "livtogr",
  "hhsize",
  "Hhchldu18",
  "hincome",
  "hincshare",
  "fresil",
  "citiz",
  "borncntry",
  "yrinccntry",
  "bornfath",
  "bornmoth"
) %>%
  keep(~ .x %in% names(analysis_data))

substantive_vars <- names(analysis_data) %>%
  setdiff(metadata_vars) %>%
  setdiff(open_text_vars) %>%
  setdiff(country_specific_vote_vars) %>%
  setdiff(c(
    "hincome_HR",
    "hincome_FR",
    "hincome_DE",
    "hincome_IT",
    "hincome_PL",
    "hincome_ES",
    "hincome_SE"
  ))

core_vars <- substantive_vars %>%
  setdiff(sociodemographic_vars)

# 1. Item-level summaries: missingness, dominant categories, and descriptive stats.
item_missing_overall <- variable_summary(analysis_data, substantive_vars) %>%
  arrange(desc(missing_pct), variable)

item_missing_by_country <- analysis_data %>%
  group_split(country, .keep = TRUE) %>%
  map_dfr(~ variable_summary(.x, substantive_vars, group_value = first(.x$country))) %>%
  arrange(group, desc(missing_pct), variable)

categorical_dominance_overall <- item_missing_overall %>%
  filter(n_distinct <= 15, !is.na(dominant_share)) %>%
  arrange(desc(dominant_share), variable)

categorical_dominance_by_country <- item_missing_by_country %>%
  filter(n_distinct <= 15, !is.na(dominant_share)) %>%
  arrange(group, desc(dominant_share), variable)

core_summary <- variable_summary(analysis_data, core_vars) %>%
  arrange(variable)

sociodemographic_summary <- variable_summary(analysis_data, sociodemographic_vars) %>%
  arrange(variable)

vote_var_map <- tribble(
  ~country, ~variable, ~label,
  "Croatia", "voteprsp_HR", "Prospective vote choice",
  "France", "voteprsp_FR", "Prospective vote choice",
  "Germany", "voteprsp_DE", "Prospective vote choice",
  "Italy", "voteprsp_IT", "Prospective vote choice",
  "Poland", "voteprsp_PL", "Prospective vote choice",
  "Spain", "voteprsp_ES", "Prospective vote choice",
  "Sweden", "voteprsp_SE", "Prospective vote choice"
)

vote_choice_checks <- vote_var_map %>%
  mutate(
    metrics = pmap(
      list(country, variable, label),
      function(country_name, var_name, label_text) {
        country_data <- analysis_data %>%
          filter(country == country_name)

        vector_character <- clean_vector(country_data[[var_name]], as = "text")
        dominant <- mode_summary(vector_character)

        tibble(
          country = country_name,
          variable = var_name,
          label = label_text,
          n = length(vector_character),
          n_missing = sum(is.na(vector_character)),
          missing_pct = mean(is.na(vector_character)) * 100,
          dominant_category = dominant$dominant_category %||% NA_character_,
          dominant_n = dominant$dominant_n %||% NA_integer_,
          dominant_share = dominant$dominant_share %||% NA_real_
        )
      }
    )
  ) %>%
  pull(metrics) %>%
  list_rbind()

# 2. Requested pattern check: solidarity should decline from municipality to country to EU.
plsol1_check <- analysis_data %>%
  transmute(
    country,
    respondent_id = Respondent_ID,
    plsolmuni = clean_vector(plsolmuni, as = "numeric"),
    plsolcntry = clean_vector(plsolcntry, as = "numeric"),
    plsoleu = clean_vector(plsoleu, as = "numeric")
  ) %>%
  mutate(
    complete_case = if_all(c(plsolmuni, plsolcntry, plsoleu), ~ !is.na(.x)),
    village_to_country_diff = plsolmuni - plsolcntry,
    country_to_eu_diff = plsolcntry - plsoleu,
    village_to_eu_diff = plsolmuni - plsoleu,
    weak_decline = case_when(
      complete_case ~ plsolmuni >= plsolcntry & plsolcntry >= plsoleu,
      TRUE ~ NA
    ),
    strict_decline = case_when(
      complete_case ~ plsolmuni > plsolcntry & plsolcntry > plsoleu,
      TRUE ~ NA
    ),
    country_above_village = case_when(
      complete_case ~ plsolcntry > plsolmuni,
      TRUE ~ NA
    ),
    eu_above_country = case_when(
      complete_case ~ plsoleu > plsolcntry,
      TRUE ~ NA
    )
  )

plsol1_summary <- bind_rows(
  plsol1_check %>%
    summarise(
      country = "Overall",
      n_complete = sum(complete_case),
      mean_plsolmuni = mean(plsolmuni, na.rm = TRUE),
      mean_plsolcntry = mean(plsolcntry, na.rm = TRUE),
      mean_plsoleu = mean(plsoleu, na.rm = TRUE),
      mean_village_to_country_diff = mean(village_to_country_diff, na.rm = TRUE),
      mean_country_to_eu_diff = mean(country_to_eu_diff, na.rm = TRUE),
      share_weak_decline = mean(weak_decline, na.rm = TRUE),
      share_strict_decline = mean(strict_decline, na.rm = TRUE),
      share_country_above_village = mean(country_above_village, na.rm = TRUE),
      share_eu_above_country = mean(eu_above_country, na.rm = TRUE)
    ),
  plsol1_check %>%
    group_by(country) %>%
    summarise(
      n_complete = sum(complete_case),
      mean_plsolmuni = mean(plsolmuni, na.rm = TRUE),
      mean_plsolcntry = mean(plsolcntry, na.rm = TRUE),
      mean_plsoleu = mean(plsoleu, na.rm = TRUE),
      mean_village_to_country_diff = mean(village_to_country_diff, na.rm = TRUE),
      mean_country_to_eu_diff = mean(country_to_eu_diff, na.rm = TRUE),
      share_weak_decline = mean(weak_decline, na.rm = TRUE),
      share_strict_decline = mean(strict_decline, na.rm = TRUE),
      share_country_above_village = mean(country_above_village, na.rm = TRUE),
      share_eu_above_country = mean(eu_above_country, na.rm = TRUE),
      .groups = "drop"
    )
)

# 3. Requested pattern check: children, older people, and the poor should score above immigrants.
plsol2_check <- analysis_data %>%
  transmute(
    country,
    respondent_id = Respondent_ID,
    plsolchld = clean_vector(plsolchld, as = "numeric"),
    plsolold = clean_vector(plsolold, as = "numeric"),
    plsolpoor = clean_vector(plsolpoor, as = "numeric"),
    plsolimmeu = clean_vector(plsolimmeu, as = "numeric"),
    plsolimmnoneu = clean_vector(plsolimmnoneu, as = "numeric")
  ) %>%
  mutate(
    priority_mean = row_mean_or_na(pick(plsolchld, plsolold, plsolpoor)),
    all_priority_over_eu = case_when(
      if_all(c(plsolchld, plsolold, plsolpoor, plsolimmeu), ~ !is.na(.x)) ~
        plsolchld >= plsolimmeu & plsolold >= plsolimmeu & plsolpoor >= plsolimmeu,
      TRUE ~ NA
    ),
    all_priority_over_noneu = case_when(
      if_all(c(plsolchld, plsolold, plsolpoor, plsolimmnoneu), ~ !is.na(.x)) ~
        plsolchld >= plsolimmnoneu & plsolold >= plsolimmnoneu & plsolpoor >= plsolimmnoneu,
      TRUE ~ NA
    ),
    eu_gap = priority_mean - plsolimmeu,
    noneu_gap = priority_mean - plsolimmnoneu,
    child_minus_eu = plsolchld - plsolimmeu,
    old_minus_eu = plsolold - plsolimmeu,
    poor_minus_eu = plsolpoor - plsolimmeu,
    child_minus_noneu = plsolchld - plsolimmnoneu,
    old_minus_noneu = plsolold - plsolimmnoneu,
    poor_minus_noneu = plsolpoor - plsolimmnoneu
  )

plsol2_summary <- bind_rows(
  plsol2_check %>%
    summarise(
      country = "Overall",
      mean_plsolchld = mean(plsolchld, na.rm = TRUE),
      mean_plsolold = mean(plsolold, na.rm = TRUE),
      mean_plsolpoor = mean(plsolpoor, na.rm = TRUE),
      mean_plsolimmeu = mean(plsolimmeu, na.rm = TRUE),
      mean_plsolimmnoneu = mean(plsolimmnoneu, na.rm = TRUE),
      mean_priority = mean(priority_mean, na.rm = TRUE),
      mean_gap_vs_eu = mean(eu_gap, na.rm = TRUE),
      mean_gap_vs_noneu = mean(noneu_gap, na.rm = TRUE),
      share_all_priority_over_eu = mean(all_priority_over_eu, na.rm = TRUE),
      share_all_priority_over_noneu = mean(all_priority_over_noneu, na.rm = TRUE)
    ),
  plsol2_check %>%
    group_by(country) %>%
    summarise(
      mean_plsolchld = mean(plsolchld, na.rm = TRUE),
      mean_plsolold = mean(plsolold, na.rm = TRUE),
      mean_plsolpoor = mean(plsolpoor, na.rm = TRUE),
      mean_plsolimmeu = mean(plsolimmeu, na.rm = TRUE),
      mean_plsolimmnoneu = mean(plsolimmnoneu, na.rm = TRUE),
      mean_priority = mean(priority_mean, na.rm = TRUE),
      mean_gap_vs_eu = mean(eu_gap, na.rm = TRUE),
      mean_gap_vs_noneu = mean(noneu_gap, na.rm = TRUE),
      share_all_priority_over_eu = mean(all_priority_over_eu, na.rm = TRUE),
      share_all_priority_over_noneu = mean(all_priority_over_noneu, na.rm = TRUE),
      .groups = "drop"
    )
)

# 4. Requested sociodemographic sense check by country.
sociodemographic_alignment <- analysis_data %>%
  transmute(
    country,
    gender = clean_vector(Respondent_Gender, as = "text"),
    age = clean_vector(Respondent_Age, as = "numeric"),
    hincome = clean_vector(hincome, as = "numeric")
  ) %>%
  group_by(country) %>%
  summarise(
    n = n(),
    female_share = mean(gender == "Female", na.rm = TRUE),
    male_share = mean(gender == "Male", na.rm = TRUE),
    another_gender_share = mean(gender == "In another way", na.rm = TRUE),
    median_age = median(age, na.rm = TRUE),
    mean_age = mean(age, na.rm = TRUE),
    age_under_30_share = mean(age < 30, na.rm = TRUE),
    age_60_plus_share = mean(age >= 60, na.rm = TRUE),
    median_income_decile = median(hincome, na.rm = TRUE),
    low_income_share = mean(hincome <= 3, na.rm = TRUE),
    high_income_share = mean(hincome >= 8, na.rm = TRUE),
    gender_flag = if_else(
      pmax(female_share, male_share, na.rm = TRUE) > 0.60,
      "possible_gender_imbalance",
      "ok"
    ),
    age_flag = if_else(
      median_age < 30 | median_age > 60,
      "possible_age_skew",
      "ok"
    ),
    income_flag = if_else(
      median_income_decile <= 3 | median_income_decile >= 8,
      "possible_income_skew",
      "ok"
    ),
    .groups = "drop"
  ) %>%
  arrange(country)

# 5. Requested open-text language review. Uses cld3 if installed, otherwise leaves a manual-review placeholder.
open_text_review <- analysis_data %>%
  select(country, Respondent_ID, voteprsp_open, comment) %>%
  rename(respondent_id = Respondent_ID) %>%
  pivot_longer(
    cols = c(voteprsp_open, comment),
    names_to = "text_field",
    values_to = "text"
  ) %>%
  filter(!is.na(text)) %>%
  left_join(country_language_map, by = "country") %>%
  mutate(
    text_length = str_length(text),
    detection = map(text, detect_language_cld3)
  ) %>%
  unnest(detection) %>%
  mutate(
    detected_language = case_match(
      detected_language_code,
      "hr" ~ "Croatian",
      "fr" ~ "French",
      "de" ~ "German",
      "it" ~ "Italian",
      "pl" ~ "Polish",
      "es" ~ "Spanish",
      "sv" ~ "Swedish",
      "en" ~ "English",
      .default = NA_character_
    ),
    review_flag = case_when(
      detection_status == "too_short" ~ "manual_review_short_text",
      detection_status == "cld3_not_installed" ~ "manual_review_install_cld3",
      detected_language_code != expected_language_code ~ "possible_language_mismatch",
      TRUE ~ "ok"
    )
  ) %>%
  select(
    country,
    respondent_id,
    text_field,
    text,
    text_length,
    expected_language,
    detected_language,
    token_n,
    detection_status,
    review_flag
  ) %>%
  arrange(country, text_field, desc(review_flag), respondent_id)

eleven_point_vars <- c(
  "lifesat",
  "satdem",
  "trstparl",
  "trstmil",
  "trstjudi",
  "trstpltcns",
  "trstparts",
  "trstmuni",
  "trstgov",
  "trsteucom",
  "psqhlth",
  "psqschl",
  "psqtxath",
  "psqtrnsp",
  "psqbnft",
  "psqloc",
  "soctrst",
  "percfair",
  "perchelp",
  "leftright",
  "euintgr",
  "taxwelf",
  "immicullife"
) %>%
  keep(~ .x %in% names(analysis_data))

# 6. Requested device-mode check for long 11-point scales.
device_long <- analysis_data %>%
  filter(!is.na(device_group)) %>%
  select(country, device_group, all_of(eleven_point_vars)) %>%
  mutate(across(all_of(eleven_point_vars), ~ clean_vector(.x, as = "numeric"))) %>%
  pivot_longer(
    cols = all_of(eleven_point_vars),
    names_to = "variable",
    values_to = "score"
  ) %>%
  filter(!is.na(score))

device_item_comparison <- device_long %>%
  group_by(country, device_group, variable) %>%
  summarise(
    n = n(),
    mean_score = mean(score),
    high_point_share = mean(score >= 9),
    top_box_share = mean(score == 11),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = device_group,
    values_from = c(n, mean_score, high_point_share, top_box_share)
  ) %>%
  mutate(
    mean_score_gap_mobile_minus_desktop = mean_score_Mobile - mean_score_Desktop,
    high_point_gap_mobile_minus_desktop = high_point_share_Mobile - high_point_share_Desktop,
    top_box_gap_mobile_minus_desktop = top_box_share_Mobile - top_box_share_Desktop
  ) %>%
  arrange(country, variable)

device_overall_comparison <- device_long %>%
  group_by(country, device_group) %>%
  summarise(
    n_responses = n(),
    mean_score = mean(score),
    high_point_share = mean(score >= 9),
    top_box_share = mean(score == 11),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = device_group,
    values_from = c(n_responses, mean_score, high_point_share, top_box_share)
  ) %>%
  mutate(
    mean_score_gap_mobile_minus_desktop = mean_score_Mobile - mean_score_Desktop,
    high_point_gap_mobile_minus_desktop = high_point_share_Mobile - high_point_share_Desktop,
    top_box_gap_mobile_minus_desktop = top_box_share_Mobile - top_box_share_Desktop
  ) %>%
  arrange(country)

# 7. ESS-ready summary table. If a benchmark file is added later, the script joins it automatically.
ess_ready_summary <- analysis_data %>%
  transmute(
    country,
    polint = clean_vector(polint, as = "numeric"),
    poldisfam = clean_vector(poldisfam, as = "numeric"),
    lifesat = clean_vector(lifesat, as = "numeric"),
    satdem = clean_vector(satdem, as = "numeric"),
    trstparl = clean_vector(trstparl, as = "numeric"),
    trstjudi = clean_vector(trstjudi, as = "numeric"),
    trstpltcns = clean_vector(trstpltcns, as = "numeric"),
    trstparts = clean_vector(trstparts, as = "numeric"),
    trstgov = clean_vector(trstgov, as = "numeric"),
    leftright = clean_vector(leftright, as = "numeric"),
    euintgr = clean_vector(euintgr, as = "numeric"),
    immicullife = clean_vector(immicullife, as = "numeric")
  ) %>%
  pivot_longer(
    cols = -country,
    names_to = "variable",
    values_to = "value"
  ) %>%
  filter(!is.na(value)) %>%
  group_by(country, variable) %>%
  summarise(
    n = n(),
    mean_pretest = mean(value),
    sd_pretest = sd(value),
    median_pretest = median(value),
    .groups = "drop"
  )

if (fs::file_exists(benchmark_file)) {
  ess_benchmarks <- readr::read_csv(benchmark_file, show_col_types = FALSE)

  ess_comparison <- ess_ready_summary %>%
    left_join(ess_benchmarks, by = c("country", "variable")) %>%
    mutate(
      ess_mean_gap = mean_pretest - ess_mean,
      ess_sd_gap = sd_pretest - ess_sd
    )
} else {
  ess_comparison <- ess_ready_summary %>%
    mutate(
      ess_mean = NA_real_,
      ess_sd = NA_real_,
      ess_source = NA_character_,
      ess_mean_gap = NA_real_,
      ess_sd_gap = NA_real_
    )

  ess_template <- ess_ready_summary %>%
    distinct(country, variable) %>%
    mutate(
      ess_mean = NA_real_,
      ess_sd = NA_real_,
      ess_source = NA_character_
    )

  write_csv(
    ess_template,
    fs::path(output_dir, "ess_benchmark_template.csv")
  )
}

overview_lines <- c(
  glue("Raw rows in .sav: {nrow(raw_data)}"),
  glue("Rows retained for analysis (non-missing country): {nrow(analysis_data)}"),
  glue("Countries in retained sample: {analysis_data %>% count(country) %>% pull(country) %>% paste(collapse = ', ')}"),
  glue(
    "Items with more than 5% missing overall: {item_missing_overall %>% filter(missing_pct > 5) %>% nrow()}"
  ),
  glue(
    "Categorical items with one category above 50% overall: {categorical_dominance_overall %>% filter(dominant_share > 0.5) %>% nrow()}"
  ),
  glue(
    "plsol1 weak decline share overall: {round(plsol1_summary %>% filter(country == 'Overall') %>% pull(share_weak_decline) * 100, 1)}%"
  ),
  glue(
    "plsol2 share where children/older/poor are all at least as high as EU immigrants: {round(plsol2_summary %>% filter(country == 'Overall') %>% pull(share_all_priority_over_eu) * 100, 1)}%"
  ),
  glue(
    "plsol2 share where children/older/poor are all at least as high as non-EU immigrants: {round(plsol2_summary %>% filter(country == 'Overall') %>% pull(share_all_priority_over_noneu) * 100, 1)}%"
  ),
  glue(
    "Open-text entries flagged for manual review or possible mismatch: {open_text_review %>% filter(review_flag != 'ok') %>% nrow()}"
  ),
  glue(
    "ESS benchmark file present: {fs::file_exists(benchmark_file)}"
  )
)

write_csv(item_missing_overall, fs::path(output_dir, "item_summary_overall.csv"))
write_csv(item_missing_by_country, fs::path(output_dir, "item_summary_by_country.csv"))
write_csv(categorical_dominance_overall, fs::path(output_dir, "categorical_dominance_overall.csv"))
write_csv(categorical_dominance_by_country, fs::path(output_dir, "categorical_dominance_by_country.csv"))
write_csv(core_summary, fs::path(output_dir, "core_variable_summary.csv"))
write_csv(sociodemographic_summary, fs::path(output_dir, "sociodemographic_summary.csv"))
write_csv(vote_choice_checks, fs::path(output_dir, "vote_choice_checks_by_country.csv"))
write_csv(plsol1_check, fs::path(output_dir, "plsol1_respondent_checks.csv"))
write_csv(plsol1_summary, fs::path(output_dir, "plsol1_summary.csv"))
write_csv(plsol2_check, fs::path(output_dir, "plsol2_respondent_checks.csv"))
write_csv(plsol2_summary, fs::path(output_dir, "plsol2_summary.csv"))
write_csv(sociodemographic_alignment, fs::path(output_dir, "sociodemographic_alignment.csv"))
write_csv(open_text_review, fs::path(output_dir, "open_text_language_review.csv"))
write_csv(device_item_comparison, fs::path(output_dir, "device_item_comparison.csv"))
write_csv(device_overall_comparison, fs::path(output_dir, "device_overall_comparison.csv"))
write_csv(ess_comparison, fs::path(output_dir, "ess_comparison_ready.csv"))
write_lines(overview_lines, fs::path(output_dir, "overview.txt"))

message(glue("Finished. Outputs written to: {output_dir}"))

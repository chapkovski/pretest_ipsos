`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}

required_packages <- c(
  "tidyverse",
  "haven",
  "broom",
  "gt",
  "patchwork",
  "scales",
  "MASS",
  "sandwich",
  "lmtest",
  "knitr"
)

load_analysis_packages <- function() {
  missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]

  if (length(missing_packages) > 0) {
    stop(
      "Missing required packages: ",
      paste(missing_packages, collapse = ", "),
      call. = FALSE
    )
  }

  invisible(lapply(required_packages, library, character.only = TRUE))
}

nonresponse_regex <- stringr::regex(
  "prefer not|do not know|don't know|blank or null|no answer|refusal|not available|does not apply|not calculated",
  ignore_case = TRUE
)

labelled_nonresponse_values <- function(x) {
  attr(x, "labels") %>%
    `%||%`(c()) %>%
    tibble::enframe(name = "label", value = "value") %>%
    dplyr::filter(stringr::str_detect(label, nonresponse_regex)) %>%
    dplyr::pull(value) %>%
    as.numeric()
}

clean_vector <- function(x, as = c("numeric", "text")) {
  as <- match.arg(as)

  if (inherits(x, "POSIXt")) {
    return(x)
  }

  if (is.character(x)) {
    cleaned_text <- x %>% stringr::str_squish() %>% dplyr::na_if("")
    return(if (as == "text") cleaned_text else suppressWarnings(as.numeric(cleaned_text)))
  }

  if (is.logical(x)) {
    return(if (as == "numeric") as.numeric(x) else as.character(x))
  }

  values_to_drop <- labelled_nonresponse_values(x)
  y <- x

  if (length(values_to_drop) > 0) {
    y[y %in% values_to_drop] <- NA
  }

  if (inherits(y, c("haven_labelled", "labelled"))) {
    y <- if (as == "numeric") haven::zap_labels(y) else haven::as_factor(y, levels = "labels")
  }

  if (as == "numeric") {
    suppressWarnings(as.numeric(y))
  } else {
    as.character(y) %>%
      stringr::str_squish() %>%
      dplyr::na_if("")
  }
}

coalesce_clean_columns <- function(data, columns, as = c("numeric", "text")) {
  as <- match.arg(as)

  if (length(columns) == 0) {
    return(rep(NA, nrow(data)))
  }

  purrr::map(columns, ~ clean_vector(data[[.x]], as = as)) %>%
    purrr::reduce(dplyr::coalesce)
}

row_mean_or_na <- function(data) {
  out <- rowMeans(data, na.rm = TRUE)
  out[rowSums(!is.na(data)) == 0] <- NA_real_
  out
}

row_sum_or_na <- function(data) {
  out <- rowSums(data, na.rm = TRUE)
  out[rowSums(!is.na(data)) == 0] <- NA_real_
  out
}

rescale_01 <- function(x, min_value, max_value) {
  dplyr::if_else(
    is.na(x),
    NA_real_,
    (x - min_value) / (max_value - min_value)
  )
}

reverse_scale <- function(x, min_value, max_value) {
  dplyr::if_else(is.na(x), NA_real_, max_value + min_value - x)
}

standardize <- function(x) {
  if (all(is.na(x))) {
    return(x)
  }

  out <- as.numeric(scale(x))
  out[is.nan(out)] <- 0
  out
}

safe_percent_rank <- function(x) {
  ranks <- rep(NA_real_, length(x))
  ok <- !is.na(x)

  if (sum(ok) == 1) {
    ranks[ok] <- 0.5
    return(ranks)
  }

  if (sum(ok) > 1) {
    ranks[ok] <- dplyr::percent_rank(x[ok])
  }

  ranks
}

prepare_numeric_matrix <- function(data, vars) {
  data %>%
    dplyr::select(dplyr::all_of(vars)) %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), as.numeric))
}

cohens_alpha <- function(data, vars) {
  mat <- prepare_numeric_matrix(data, vars)
  mat <- mat[rowSums(!is.na(mat)) > 1, , drop = FALSE]

  if (nrow(mat) < 2 || ncol(mat) < 2) {
    return(NA_real_)
  }

  complete_mat <- stats::na.omit(mat)

  if (nrow(complete_mat) < 2) {
    return(NA_real_)
  }

  item_variance <- sum(apply(complete_mat, 2, stats::var))
  total_variance <- stats::var(rowSums(complete_mat))

  if (is.na(total_variance) || total_variance == 0) {
    return(NA_real_)
  }

  k <- ncol(complete_mat)
  (k / (k - 1)) * (1 - (item_variance / total_variance))
}

build_analysis_data <- function(input_file = fs::path("data", "POLITSOLID_Pretest_interim_11032026.sav")) {
  raw_data <- haven::read_sav(input_file)

  income_columns <- names(raw_data)[stringr::str_detect(names(raw_data), "^hincome_[A-Z]{2}$")]
  region_columns <- names(raw_data)[stringr::str_detect(names(raw_data), "^REGION_[A-Z]{2}$")]

  analysis_data <- raw_data %>%
    dplyr::mutate(dplyr::across(where(is.character), ~ .x %>% stringr::str_squish() %>% dplyr::na_if(""))) %>%
    dplyr::mutate(country_numeric = clean_vector(Country, as = "numeric")) %>%
    dplyr::filter(!is.na(country_numeric)) %>%
    dplyr::mutate(
      country = clean_vector(Country, as = "text"),
      region = coalesce_clean_columns(dplyr::pick(dplyr::any_of(region_columns)), region_columns, as = "text"),
      respondent_id = dplyr::row_number(),
      gender = clean_vector(Respondent_Gender, as = "text"),
      gender_binary = dplyr::case_when(
        gender == "Male" ~ 0,
        gender == "Female" ~ 1,
        TRUE ~ NA_real_
      ),
      age = clean_vector(Respondent_Age, as = "numeric"),
      education_level = clean_vector(ISCEDRecodeKP, as = "text"),
      education_num = clean_vector(ISCEDRecodeKP, as = "numeric"),
      employment_status = clean_vector(EmploymentStatusKP, as = "text"),
      employment_num = clean_vector(EmploymentStatusKP, as = "numeric"),
      migrant_background = dplyr::case_when(
        clean_vector(borncntry, as = "numeric") == 2 ~ 1,
        clean_vector(borncntry, as = "numeric") == 1 ~ 0,
        TRUE ~ NA_real_
      ),
      children_u18 = clean_vector(Hhchldu18, as = "numeric"),
      dependants = dplyr::case_when(
        is.na(children_u18) ~ NA_real_,
        children_u18 > 0 ~ 1,
        TRUE ~ 0
      ),
      hhsize = clean_vector(hhsize, as = "numeric"),
      hincome = coalesce_clean_columns(dplyr::pick(dplyr::any_of(income_columns)), income_columns, as = "numeric"),
      income_rank_country = dplyr::if_else(
        is.na(hincome),
        NA_real_,
        ave(hincome, country, FUN = safe_percent_rank)
      ),
      income_z_country = dplyr::if_else(
        is.na(hincome),
        NA_real_,
        ave(hincome, country, FUN = standardize)
      ),
      hincshare = clean_vector(hincshare, as = "numeric"),
      fresil = clean_vector(fresil, as = "numeric"),
      ecohhld = clean_vector(ecohhld, as = "numeric"),
      econat = clean_vector(econat, as = "numeric"),
      slfstatus = clean_vector(slfstatus, as = "numeric"),
      leftright = clean_vector(leftright, as = "numeric"),
      left_orientation = reverse_scale(leftright, 1, 11),
      ideology_right = leftright,
      poleconred = clean_vector(poleconred, as = "numeric"),
      poleconsmr = clean_vector(poleconsmr, as = "numeric"),
      less_gov_reverse = reverse_scale(poleconsmr, 1, 5),
      ideology_left_index = row_mean_or_na(tibble::tibble(
        poleconred = rescale_01(poleconred, 1, 5),
        less_gov_reverse = rescale_01(less_gov_reverse, 1, 5),
        left_orientation = rescale_01(left_orientation, 1, 11)
      )),
      objective_resources_index = row_mean_or_na(tibble::tibble(
        hincome = rescale_01(hincome, 1, 10),
        slfstatus = rescale_01(slfstatus, 1, 10)
      )),
      perceived_capacity_index = row_mean_or_na(tibble::tibble(
        fresil = rescale_01(fresil, 1, 5),
        ecohhld = rescale_01(ecohhld, 1, 4)
      )),
      risk_unable_to_cope = clean_vector(prskcope, as = "numeric"),
      risk_unemployment = clean_vector(prskunemp, as = "numeric"),
      risk_unable_to_work = clean_vector(prskunwork, as = "numeric"),
      risk_care = clean_vector(prskcare, as = "numeric"),
      perceived_risk_index = row_mean_or_na(tibble::tibble(
        risk_unable_to_cope = rescale_01(risk_unable_to_cope, 1, 4),
        risk_unemployment = rescale_01(risk_unemployment, 1, 4),
        risk_unable_to_work = rescale_01(risk_unable_to_work, 1, 4),
        risk_care = rescale_01(risk_care, 1, 4)
      )),
      pubpay_pension = dplyr::case_when(clean_vector(pubpaypens, as = "numeric") == 1 ~ 1, clean_vector(pubpaypens, as = "numeric") == 2 ~ 0, TRUE ~ NA_real_),
      pubpay_child = dplyr::case_when(clean_vector(pubpaychild, as = "numeric") == 1 ~ 1, clean_vector(pubpaychild, as = "numeric") == 2 ~ 0, TRUE ~ NA_real_),
      pubpay_unemp = dplyr::case_when(clean_vector(pubpayunemp, as = "numeric") == 1 ~ 1, clean_vector(pubpayunemp, as = "numeric") == 2 ~ 0, TRUE ~ NA_real_),
      pubpay_disab = dplyr::case_when(clean_vector(pubpaydisab, as = "numeric") == 1 ~ 1, clean_vector(pubpaydisab, as = "numeric") == 2 ~ 0, TRUE ~ NA_real_),
      pubpay_rent = dplyr::case_when(clean_vector(pubpayrent, as = "numeric") == 1 ~ 1, clean_vector(pubpayrent, as = "numeric") == 2 ~ 0, TRUE ~ NA_real_),
      pubpay_mininc = dplyr::case_when(clean_vector(pubpaymininc, as = "numeric") == 1 ~ 1, clean_vector(pubpaymininc, as = "numeric") == 2 ~ 0, TRUE ~ NA_real_),
      objective_risk_count = row_sum_or_na(tibble::tibble(
        pubpay_pension = pubpay_pension,
        pubpay_child = pubpay_child,
        pubpay_unemp = pubpay_unemp,
        pubpay_disab = pubpay_disab,
        pubpay_rent = pubpay_rent,
        pubpay_mininc = pubpay_mininc
      )),
      unpaid_or_inactive = dplyr::case_when(
        employment_num == 1 ~ 0,
        employment_num %in% 2:8 ~ 1,
        TRUE ~ NA_real_
      ),
      objective_need_index = row_mean_or_na(tibble::tibble(
        objective_risk_count = rescale_01(objective_risk_count, 0, 6),
        unpaid_or_inactive = unpaid_or_inactive
      )),
      soctrst = clean_vector(soctrst, as = "numeric"),
      social_trust = reverse_scale(soctrst, 1, 11),
      percfair = clean_vector(percfair, as = "numeric"),
      perchelp = clean_vector(perchelp, as = "numeric"),
      social_trust_index = row_mean_or_na(tibble::tibble(
        social_trust = rescale_01(social_trust, 1, 11),
        percfair = rescale_01(percfair, 1, 11),
        perchelp = rescale_01(perchelp, 1, 11)
      )),
      rednrmdsc = clean_vector(rednrmdsc, as = "numeric"),
      taxnorm = clean_vector(taxnorm, as = "numeric"),
      fraudnorm = clean_vector(fraudnorm, as = "numeric"),
      norms_legitimacy_index = row_mean_or_na(tibble::tibble(
        rednrmdsc = rescale_01(rednrmdsc, 1, 5),
        taxnorm = rescale_01(reverse_scale(taxnorm, 1, 5), 1, 5),
        fraudnorm = rescale_01(reverse_scale(fraudnorm, 1, 5), 1, 5)
      )),
      satdem = clean_vector(satdem, as = "numeric"),
      trstparl = clean_vector(trstparl, as = "numeric"),
      trstjudi = clean_vector(trstjudi, as = "numeric"),
      trstpltcns = clean_vector(trstpltcns, as = "numeric"),
      trstparts = clean_vector(trstparts, as = "numeric"),
      trstmuni = clean_vector(trstmuni, as = "numeric"),
      trstgov = clean_vector(trstgov, as = "numeric"),
      trsteucom = clean_vector(trsteucom, as = "numeric"),
      institutional_trust_index = row_mean_or_na(tibble::tibble(
        trstparl = rescale_01(trstparl, 1, 11),
        trstjudi = rescale_01(trstjudi, 1, 11),
        trstpltcns = rescale_01(trstpltcns, 1, 11),
        trstparts = rescale_01(trstparts, 1, 11),
        trstmuni = rescale_01(trstmuni, 1, 11),
        trstgov = rescale_01(trstgov, 1, 11),
        trsteucom = rescale_01(trsteucom, 1, 11),
        satdem = rescale_01(satdem, 1, 11)
      )),
      pssatgen = clean_vector(pssatgen, as = "numeric"),
      psqhlth = clean_vector(psqhlth, as = "numeric"),
      psqschl = clean_vector(psqschl, as = "numeric"),
      psqtxath = clean_vector(psqtxath, as = "numeric"),
      psqtrnsp = clean_vector(psqtrnsp, as = "numeric"),
      psqbnft = clean_vector(psqbnft, as = "numeric"),
      psqloc = clean_vector(psqloc, as = "numeric"),
      psefair = clean_vector(psefair, as = "numeric"),
      pseeffi = clean_vector(pseeffi, as = "numeric"),
      psehelp = clean_vector(psehelp, as = "numeric"),
      pseintgrt = clean_vector(pseintgrt, as = "numeric"),
      service_quality_index = row_mean_or_na(tibble::tibble(
        pssatgen = rescale_01(pssatgen, 1, 5),
        psqhlth = rescale_01(psqhlth, 1, 11),
        psqschl = rescale_01(psqschl, 1, 11),
        psqtxath = rescale_01(psqtxath, 1, 11),
        psqtrnsp = rescale_01(psqtrnsp, 1, 11),
        psqbnft = rescale_01(psqbnft, 1, 11),
        psqloc = rescale_01(psqloc, 1, 11),
        psefair = rescale_01(psefair, 1, 5),
        pseeffi = rescale_01(pseeffi, 1, 5),
        psehelp = rescale_01(psehelp, 1, 5),
        pseintgrt = rescale_01(pseintgrt, 1, 5)
      )),
      perceived_governance_index = row_mean_or_na(tibble::tibble(
        service_quality_index = service_quality_index,
        institutional_trust_index = institutional_trust_index
      )),
      plsolmuni = clean_vector(plsolmuni, as = "numeric"),
      plsolcntry = clean_vector(plsolcntry, as = "numeric"),
      plsoleu = clean_vector(plsoleu, as = "numeric"),
      plsolchld = clean_vector(plsolchld, as = "numeric"),
      plsolold = clean_vector(plsolold, as = "numeric"),
      plsolpoor = clean_vector(plsolpoor, as = "numeric"),
      plsolimmeu = clean_vector(plsolimmeu, as = "numeric"),
      plsolimmnoneu = clean_vector(plsolimmnoneu, as = "numeric"),
      solidarity_territorial = row_mean_or_na(tibble::tibble(plsolmuni = plsolmuni, plsolcntry = plsolcntry, plsoleu = plsoleu)),
      solidarity_deserving = row_mean_or_na(tibble::tibble(plsolchld = plsolchld, plsolold = plsolold, plsolpoor = plsolpoor)),
      solidarity_migrant = row_mean_or_na(tibble::tibble(plsolimmeu = plsolimmeu, plsolimmnoneu = plsolimmnoneu)),
      solidarity_group = row_mean_or_na(tibble::tibble(
        plsolchld = plsolchld,
        plsolold = plsolold,
        plsolpoor = plsolpoor,
        plsolimmeu = plsolimmeu,
        plsolimmnoneu = plsolimmnoneu
      )),
      solidarity_all = row_mean_or_na(tibble::tibble(
        plsolmuni = plsolmuni,
        plsolcntry = plsolcntry,
        plsoleu = plsoleu,
        plsolchld = plsolchld,
        plsolold = plsolold,
        plsolpoor = plsolpoor,
        plsolimmeu = plsolimmeu,
        plsolimmnoneu = plsolimmnoneu
      )),
      welfare_chauvinism = solidarity_deserving - solidarity_migrant,
      scgvold = clean_vector(scgvold, as = "numeric"),
      scgvchld = clean_vector(scgvchld, as = "numeric"),
      scgvpoor = clean_vector(scgvpoor, as = "numeric"),
      scgveuimm = clean_vector(scgveuimm, as = "numeric"),
      scgvnoneuimm = clean_vector(scgvnoneuimm, as = "numeric"),
      governance_scope_deserving = row_mean_or_na(tibble::tibble(scgvchld = scgvchld, scgvold = scgvold, scgvpoor = scgvpoor)),
      governance_scope_migrant = row_mean_or_na(tibble::tibble(scgveuimm = scgveuimm, scgvnoneuimm = scgvnoneuimm)),
      governance_scope_gap = governance_scope_deserving - governance_scope_migrant
    ) %>%
    dplyr::mutate(
      dplyr::across(
        c(
          ideology_left_index,
          objective_resources_index,
          perceived_capacity_index,
          objective_need_index,
          perceived_risk_index,
          norms_legitimacy_index,
          social_trust_index,
          institutional_trust_index,
          service_quality_index,
          perceived_governance_index,
          income_rank_country,
          income_z_country,
          fresil,
          ecohhld,
          ideology_right,
          dependants
        ),
        standardize,
        .names = "{.col}_z"
      )
    )

  analysis_data
}

attach_optional_context <- function(data, context_file, by = c("country", "region")) {
  if (is.null(context_file) || identical(context_file, "") || !file.exists(context_file)) {
    attr(data, "context_loaded") <- FALSE
    attr(data, "context_file") <- context_file
    return(data)
  }

  context_data <- readr::read_csv(context_file, show_col_types = FALSE)

  join_keys <- by[by %in% names(context_data)]

  if (length(join_keys) == 0) {
    stop("Context file exists but has none of the required join columns: ", paste(by, collapse = ", "))
  }

  joined <- dplyr::left_join(data, context_data, by = join_keys)
  attr(joined, "context_loaded") <- TRUE
  attr(joined, "context_file") <- context_file
  joined
}

model_sample <- function(data, outcome, predictors) {
  formula_vars <- all.vars(stats::as.formula(paste(outcome, "~", paste(predictors, collapse = " + "))))
  keep_vars <- unique(c(outcome, formula_vars))
  data %>%
    dplyr::select(dplyr::all_of(keep_vars)) %>%
    tidyr::drop_na()
}

robust_tidy_lm <- function(model, conf_level = 0.95) {
  vcov_mat <- sandwich::vcovHC(model, type = "HC3")

  if (any(!is.finite(diag(vcov_mat)))) {
    vcov_mat <- sandwich::vcovHC(model, type = "HC1")
  }

  test <- lmtest::coeftest(model, vcov. = vcov_mat)
  crit <- stats::qnorm(1 - (1 - conf_level) / 2)

  tibble::tibble(
    term = rownames(test),
    estimate = test[, "Estimate"],
    std.error = test[, "Std. Error"],
    statistic = test[, "t value"],
    p.value = test[, "Pr(>|t|)"],
    conf.low = estimate - crit * std.error,
    conf.high = estimate + crit * std.error
  )
}

fit_ols <- function(data, formula) {
  stats::lm(formula = formula, data = data)
}

fit_ordered <- function(data, formula) {
  outcome_name <- all.vars(formula)[1]
  ordered_data <- data
  ordered_data[[outcome_name]] <- ordered(ordered_data[[outcome_name]])
  MASS::polr(formula = formula, data = ordered_data, Hess = TRUE, method = "logistic")
}

ordered_tidy <- function(model, conf_level = 0.95) {
  out <- broom::tidy(model, conf.int = TRUE, conf.level = conf_level) %>%
    dplyr::filter(!stringr::str_detect(term, "\\|"))

  if (!"p.value" %in% names(out) && "statistic" %in% names(out)) {
    out <- dplyr::mutate(
      out,
      p.value = 2 * stats::pnorm(abs(statistic), lower.tail = FALSE)
    )
  }

  out
}

build_formula <- function(outcome, predictors, controls = character()) {
  stats::as.formula(
    paste(outcome, "~", paste(c(predictors, controls), collapse = " + "))
  )
}

make_hypothesis_table <- function(...) {
  dplyr::bind_rows(...)
}

coef_plot_data <- function(tidy_data, label_map = NULL, keep_terms = NULL, hypothesis = NULL) {
  out <- tidy_data

  if (!is.null(keep_terms)) {
    out <- dplyr::filter(out, term %in% keep_terms)
  }

  if (!is.null(label_map)) {
    out <- dplyr::left_join(out, label_map, by = "term") %>%
      dplyr::mutate(term_label = dplyr::coalesce(term_label, term))
  } else {
    out <- dplyr::mutate(out, term_label = term)
  }

  if (!is.null(hypothesis)) {
    out <- dplyr::mutate(out, hypothesis = hypothesis)
  }

  out
}

render_table <- function(data, digits = 3) {
  knitr::kable(data, digits = digits)
}

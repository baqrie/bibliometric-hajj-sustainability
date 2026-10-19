# =====================================================================
# 05 — REVISION DIAGNOSTICS (major revision, round 1)
# Needs only dplyr/readr/stringr/tidyr (no bibliometrix)
# Reads:  db/final_corpus_v428.csv            (raw keywords: needed for the
#                                              Scopus index-term audit)
#         db/final_corpus_normalized_428.csv
# Writes (identifiers and numbers only):
#   results/revision/reproduction_check.csv   manuscript figure vs recomputed
#   results/revision/citation_tests.csv       M7: MWU, Cliff's delta + CI
#   results/revision/citation_regression.csv  M7: quasi-Poisson model
#   results/revision/retrieval_flags_428.csv  M8/M17: per-record anchor source
#   results/revision/retrieval_sensitivity.csv M17: EVENT/CITY under variants
#   results/revision/umrah_screen_candidates.csv M4: records to read
#   results/revision/validation_sample_key.csv M1: sampled EIDs + auto label
#   results/revision/subgroup_growth.csv      S7
#   results/revision/subgroup_growth_2025.csv S7: to 2025 and over a common window [8 Oct 2026]
#   results/revision/strict_rule_sensitivity.csv R6-MC2: EVENT = term in title or author keywords [8 Oct 2026]
#   results/revision/strict_rule_flags_428.csv   per-record strict label [8 Oct 2026]
# Writes (contain Scopus abstracts -- local only, never commit):
#   db/validation_workbook_blinded.csv        M1/M19 coding sheet for coders
#
# Not covered here (see README "Revision analyses"):
#   M3 corpus diagnostics (33/428, 3/428, 15.7%) -- 08_corpus_diagnostics.R
#   M2 map sensitivity -- 06_map_sensitivity.R (bibliometrix) + VOSviewer by hand
#   M1/M19 validation results -- 07_validation_analysis.R, after coding
# =====================================================================
suppressPackageStartupMessages({
  library(dplyr); library(readr); library(stringr); library(tidyr)
})

OUT <- "results/revision"
dir.create(OUT, showWarnings = FALSE, recursive = TRUE)
RETRIEVAL_YEAR <- 2026
SEED <- 2026

raw  <- read_csv("db/final_corpus_v428.csv", show_col_types = FALSE, guess_max = Inf)
norm <- read_csv("db/final_corpus_normalized_428.csv", show_col_types = FALSE, guess_max = Inf)
stopifnot(nrow(raw) == 428, nrow(norm) == 428, identical(raw$EID, norm$EID))

d <- norm |>
  mutate(TC  = as.numeric(`Cited by`), TC = if_else(is.na(TC), 0, TC),
         PY  = as.integer(Year),
         AGE = RETRIEVAL_YEAR - PY + 1,          # the paper's age definition
         TCY = TC / AGE,                         # citations per year of age
         EVENT = SUBGROUP == "EVENT")

# ---------------------------------------------------------------------
# A. REPRODUCTION CHECK -- numbers that do not need bibliometrix
#    target = value printed in the submitted manuscript
# ---------------------------------------------------------------------
cliff <- function(a, b) mean(sign(outer(a, b, "-")))
skew_g1 <- function(x) {                      # bias-corrected sample skewness
  n <- length(x); m <- mean(x); s2 <- mean((x - m)^2); m3 <- mean((x - m)^3)
  (m3 / s2^1.5) * sqrt(n * (n - 1)) / (n - 2)
}
yr  <- table(d$PY)
ev  <- d$TC[d$EVENT];  ct <- d$TC[!d$EVENT]
mw_raw <- wilcox.test(ev, ct, exact = FALSE, correct = TRUE)
mw_age <- wilcox.test(d$TCY[d$EVENT], d$TCY[!d$EVENT], exact = FALSE, correct = TRUE)
src_n  <- n_distinct(str_to_upper(str_squish(d$`Source title`)))
rp_n   <- sum(!is.na(d$`Correspondence Address`) & nzchar(str_trim(d$`Correspondence Address`)))
growth <- ((as.numeric(yr[length(yr)]) / as.numeric(yr[1]))^(1 / (max(d$PY) - min(d$PY))) - 1) * 100

chk <- tribble(
  ~item,                               ~target, ~value,
  "Documents",                          428,    nrow(d),
  "Articles",                           405,    sum(d$`Document Type` == "Article"),
  "Reviews",                            23,     sum(d$`Document Type` == "Review"),
  "Sources (case-normalized)",          220,    src_n,
  "Total citations",                    8736,   sum(d$TC),
  "Mean citations",                     20.41,  round(mean(d$TC), 2),
  "Median citations",                   9,      median(d$TC),
  "Max citations",                      220,    max(d$TC),
  "Docs cited >= 10",                   210,    sum(d$TC >= 10),
  "Docs cited >= 50",                   43,     sum(d$TC >= 50),
  "Annual growth rate (%)",             8.48,   round(growth, 2),
  "Document average age",               5.55,   round(mean(RETRIEVAL_YEAR - d$PY), 2),
  "Docs 2016",                          24,     as.numeric(yr["2016"]),
  "Docs 2018",                          8,      as.numeric(yr["2018"]),
  "Docs 2025 (peak)",                   75,     as.numeric(yr["2025"]),
  "Docs 2026 (partial)",                39,     as.numeric(yr["2026"]),
  "Share 2021+ (%)",                    63.8,   round(100 * mean(d$PY >= 2021), 1),
  "EVENT n",                            121,    sum(d$EVENT),
  "CITY n",                             307,    sum(!d$EVENT),
  "EVENT median TC",                    11,     median(ev),
  "CITY median TC",                     8,      median(ct),
  "EVENT mean TC",                      21.7,   round(mean(ev), 1),
  "CITY mean TC",                       19.9,   round(mean(ct), 1),
  "EVENT docs 2021-2026",               66,     sum(d$EVENT & d$PY >= 2021),
  "CITY docs 2021-2026",                207,    sum(!d$EVENT & d$PY >= 2021),
  "EVENT first year",                   1981,   min(d$PY[d$EVENT]),
  "CITY first year",                    1993,   min(d$PY[!d$EVENT]),
  "Mann-Whitney U (raw, EVENT)",        18904,  unname(mw_raw$statistic),
  "Mann-Whitney p (raw)",               0.774,  round(mw_raw$p.value, 3),
  "Mann-Whitney p (age-normalized)",    0.253,  round(mw_age$p.value, 3),
  "Cliff's delta (raw)",                0.02,   round(cliff(ev, ct), 2),
  "Skewness EVENT",                     3.36,   round(skew_g1(ev), 2),
  "Skewness CITY",                      3.44,   round(skew_g1(ct), 2),
  "Records with corresponding address (Table 4 denominator)", 383, rp_n
) |> mutate(status = if_else(abs(value - target) < 1e-9, "MATCH", "DIFF"))
write_csv(chk, file.path(OUT, "reproduction_check.csv"))

# Table 7 (most globally cited), NTC = TC / mean TC of same publication year
t7 <- d |> group_by(PY) |> mutate(NTC = TC / mean(TC)) |> ungroup() |>
  arrange(desc(TC)) |> slice_head(n = 11) |>
  transmute(EID, first_author = str_extract(Authors, "^[^;]+"), PY, TC,
            TC_per_year = round(TCY, 2), NTC = round(NTC, 3))
write_csv(t7, file.path(OUT, "table7_recomputed.csv"))

# ---------------------------------------------------------------------
# B. M7 -- citation caution: CI for Cliff's delta, 2026 sensitivity,
#    regression controlling for age and document type
# ---------------------------------------------------------------------
boot_ci <- function(a, b, R = 2000) {
  set.seed(SEED)
  q <- replicate(R, cliff(sample(a, replace = TRUE), sample(b, replace = TRUE)))
  unname(quantile(q, c(.025, .975)))
}
cit_row <- function(label, keep, var, grp = d$EVENT) {
  a <- d[[var]][keep & grp]; b <- d[[var]][keep & !grp]
  w <- wilcox.test(a, b, exact = FALSE, correct = TRUE); ci <- boot_ci(a, b)
  tibble(comparison = label, measure = var, n_event = length(a), n_city = length(b),
         median_event = median(a), median_city = median(b),
         U_event = unname(w$statistic), p = round(w$p.value, 4),
         cliffs_delta = round(cliff(a, b), 3),
         delta_ci_low = round(ci[1], 3), delta_ci_high = round(ci[2], 3))
}
all_rows <- rep(TRUE, nrow(d)); no2026 <- d$PY < RETRIEVAL_YEAR
cit <- bind_rows(
  cit_row("all records",        all_rows, "TC"),
  cit_row("all records",        all_rows, "TCY"),
  cit_row("excluding 2026",     no2026,   "TC"),
  cit_row("excluding 2026",     no2026,   "TCY"))
write_csv(cit, file.path(OUT, "citation_tests.csv"))

# Quasi-Poisson GLM (handles overdispersion without extra packages):
# TC ~ subgroup + log(age) + document type; exp(coef) = citation rate ratio
fit <- glm(TC ~ SUBGROUP + log(AGE) + `Document Type`,
           family = quasipoisson, data = d |> mutate(SUBGROUP = relevel(factor(SUBGROUP), "CITY")))
co <- summary(fit)$coefficients
reg <- tibble(term = rownames(co), estimate = co[, 1], se = co[, 2], p = co[, 4]) |>
  mutate(rate_ratio = exp(estimate), rr_low = exp(estimate - 1.96 * se),
         rr_high = exp(estimate + 1.96 * se), dispersion = summary(fit)$dispersion) |>
  mutate(across(where(is.numeric), ~ round(.x, 4)))
write_csv(reg, file.path(OUT, "citation_regression.csv"))

# ---------------------------------------------------------------------
# C. M8 / M17 -- where does each record's pilgrimage/holy-city anchor
#    come from? Author text (title/abstract/author keywords) or ONLY a
#    Scopus-assigned index keyword (e.g. "Makkah [Saudi Arabia]", which
#    Scopus also attaches to studies elsewhere in Makkah Province).
# ---------------------------------------------------------------------
lw <- function(x) str_to_lower(tidyr::replace_na(x, ""))
CORE_PAT <- "\\bhajj\\b|\\bumrah\\b|\\bumra\\b|makkah|mecca|muzdalifah|jamarat|masjid al-haram|al-madinah al-munawwarah|prophet's mosque|tawaf|ihram|kaaba|ka'bah"
mad_anchor <- function(s) str_detect(s, "\\bmadinah\\b|\\bmedina\\b|\\bmadina\\b") &
                          str_detect(s, "saudi|\\bhijaz\\b|\\bhejaz\\b|\\bhajj\\b|\\bumrah\\b|pilgrim")
TAD <- paste(lw(raw$Title), lw(raw$Abstract), lw(raw$`Author Keywords`))
IDk <- lw(raw$`Index Keywords`)
anchor_author <- str_detect(TAD, CORE_PAT) | mad_anchor(TAD)
anchor_index  <- str_detect(IDk, CORE_PAT) | mad_anchor(IDk)
stopifnot(all(anchor_author | anchor_index))          # every record has an anchor

flags <- tibble(
  EID = raw$EID, Year = raw$Year, SUBGROUP = raw$SUBGROUP,
  ANCHOR_SOURCE = if_else(anchor_author, "AUTHOR_TEXT", "INDEX_KEYWORD_ONLY"),
  JEDDAH_IN_TITLE = str_detect(lw(raw$Title), "jedd|jidd"),
  JEDDAH_SUBJECT  = str_detect(TAD, "jedd|jidd"),         # title/abstract/author kw
  JEDDAH_AFFIL_ONLY = str_detect(lw(raw$Affiliations), "jedd|jidd") & !str_detect(TAD, "jedd|jidd"))
write_csv(flags, file.path(OUT, "retrieval_flags_428.csv"))

sens_row <- function(label, keep) {
  e <- sum(keep & raw$SUBGROUP == "EVENT"); c <- sum(keep & raw$SUBGROUP == "CITY")
  tibble(variant = label, n = e + c, EVENT = e, CITY = c, ratio_city_to_event = round(c / e, 2))
}
sens <- bind_rows(
  sens_row("Primary corpus", rep(TRUE, 428)),
  sens_row("Anchor in author text only (drop index-keyword-only records)", anchor_author),
  sens_row("Drop records with Jeddah in title", !flags$JEDDAH_IN_TITLE),
  sens_row("Drop records with Jeddah as subject (title/abstract/author kw)", !flags$JEDDAH_SUBJECT))
write_csv(sens, file.path(OUT, "retrieval_sensitivity.csv"))

# ---------------------------------------------------------------------
# D. M4 -- Umrah footprint screen. Produces a READING LIST; the decision
#    (does any record quantify Umrah's environmental footprint?) is manual.
# ---------------------------------------------------------------------
TAK <- paste(TAD, IDk)
FOOT_PAT <- "carbon footprint|environmental footprint|ecological footprint|emission|life[- ]cycle|\\blca\\b|waste|water (demand|consumption|use)|energy (demand|consumption|use)|resource (use|consumption)|environmental degradation"
um <- str_detect(TAK, "\\bumrah?\\b") & str_detect(TAK, FOOT_PAT)
um_tab <- tibble(EID = raw$EID, Year = raw$Year, SUBGROUP = raw$SUBGROUP, Title = raw$Title,
                 matched_terms = str_extract_all(TAK, FOOT_PAT) |> vapply(\(x) paste(unique(x), collapse = "; "), ""))[um, ]

UMRAH_DEC <- "results/umrah_screen_decisions.csv"
if (file.exists(UMRAH_DEC)) {
  um_dec <- read_csv(UMRAH_DEC, col_types = cols(.default = col_character()))
  um_tab <- left_join(um_tab, dplyr::select(um_dec, any_of(c("EID", "decision", "reason", "read_by"))), by = "EID")
  um_open  <- setdiff(um_tab$EID, um_dec$EID[!is.na(um_dec$decision) & um_dec$decision != ""])
  um_extra <- setdiff(um_dec$EID, um_tab$EID)
  if (length(um_open))  warning("Umrah screen: no decision yet for ", paste(um_open, collapse = ", "))
  if (length(um_extra)) warning("Umrah screen: the decision file lists EIDs the screen no longer finds: ",
                                paste(um_extra, collapse = ", "))
} else {
  um_tab <- mutate(um_tab, decision = "", reason = "", read_by = "")
}
write_csv(um_tab, file.path(OUT, "umrah_screen_candidates.csv"), na = "")

# ---------------------------------------------------------------------
# E. M1 / M19 -- validation sample: 60 automated EVENT + 60 automated CITY,
#    allocated across publication-year bands proportionally within each
#    subgroup (largest-remainder rounding), fixed seed.
# ---------------------------------------------------------------------
BANDS <- c(-Inf, 2010, 2015, 2020, 2023, Inf)
BAND_LAB <- c("<=2010", "2011-2015", "2016-2020", "2021-2023", "2024-2026")
d$BAND <- cut(d$PY, BANDS, labels = BAND_LAB)
alloc <- function(counts, total) {
  q <- counts / sum(counts) * total; a <- floor(q)
  a[order(q - a, decreasing = TRUE)[seq_len(total - sum(a))]] <- a[order(q - a, decreasing = TRUE)[seq_len(total - sum(a))]] + 1
  a
}
set.seed(SEED)
samp <- bind_rows(lapply(c("EVENT", "CITY"), function(g) {
  sub <- d |> filter(SUBGROUP == g)
  k <- alloc(table(sub$BAND), 60)
  bind_rows(lapply(names(k), function(b) sub |> filter(BAND == b) |> slice_sample(n = k[[b]])))
}))
stopifnot(nrow(samp) == 120, sum(samp$SUBGROUP == "EVENT") == 60)
set.seed(SEED + 1)
samp <- samp |> slice_sample(prop = 1) |> mutate(CODING_ID = sprintf("V%03d", row_number()))
key_new <- samp |> transmute(CODING_ID, EID, Year = PY, BAND, AUTO_LABEL = SUBGROUP,
                             sampling_fraction = if_else(SUBGROUP == "EVENT", 60/121, 60/307))
#
# The sample is FROZEN: the coder workbooks were built from the
# existing key, so a re-run must never overwrite it. If the re-drawn sample
# differs (e.g. another R or dplyr version), keep the frozen key, write the
# re-drawn one next to it for the record, and carry on.
KEY_PATH <- file.path(OUT, "validation_sample_key.csv")
if (file.exists(KEY_PATH)) {
  key_old <- read_csv(KEY_PATH, show_col_types = FALSE)
  same <- nrow(key_old) == nrow(key_new) &&
    identical(key_old$CODING_ID, key_new$CODING_ID) && identical(key_old$EID, key_new$EID) &&
    identical(key_old$AUTO_LABEL, key_new$AUTO_LABEL)
  if (same) {
    cat("\n[05] Validation sample re-drawn identically; the frozen key is unchanged.\n")
  } else {
    write_csv(key_new, file.path(OUT, "validation_sample_key_REDRAWN_DIFFERS.csv"))
    warning("[05] The re-drawn validation sample differs from the frozen key. The frozen key was KEPT ",
            "(07 uses it); the re-drawn sample is in validation_sample_key_REDRAWN_DIFFERS.csv. ",
            "Report the sample as frozen, not as re-drawable.", immediate. = TRUE)
  }
} else {
  write_csv(key_new, KEY_PATH)
  blank <- c("A_label", "A_hybrid_direction", "A_relevance", "A_eligibility", "A_confidence",
             "B_label", "B_hybrid_direction", "B_relevance", "B_eligibility", "B_confidence")
  wb <- samp |> transmute(CODING_ID, Year = PY, Title, Abstract, `Author Keywords`)
  for (b in blank) wb[[b]] <- ""
  write_csv(wb, "db/validation_workbook_blinded.csv", na = "")   # no AUTO_LABEL, no EID
}

# ---------------------------------------------------------------------
# F. S7 -- subgroup growth (bibliometrix formula, first to last year)
#    plus a 2010-2025 comparison that avoids the 1981 single-record start
#    and the partial 2026 year.
# ---------------------------------------------------------------------
cagr <- function(y, y0, y1) {
  n0 <- sum(y == y0); n1 <- sum(y == y1)
  if (n0 == 0 || n1 == 0) return(NA_real_)
  ((n1 / n0)^(1 / (y1 - y0)) - 1) * 100
}
grow <- bind_rows(lapply(c("EVENT", "CITY", "ALL"), function(g) {
  y <- if (g == "ALL") d$PY else d$PY[d$SUBGROUP == g]
  tibble(group = g, first_year = min(y), last_year = max(y),
         growth_first_to_last = round(cagr(y, min(y), max(y)), 2),
         docs_2010_2015 = sum(y %in% 2010:2015), docs_2020_2025 = sum(y %in% 2020:2025),
         share_2021_plus = round(100 * mean(y >= 2021), 1))
}))
write_csv(grow, file.path(OUT, "subgroup_growth.csv"))

# ---------------------------------------------------------------------
# G. R6-MC2 -- stricter anchor. EVENT_STRICT = the stage-5
#    event term appears in the title or the author keywords (raw text), so
#    abstract-only and index-keyword-only mentions no longer count. 
# ---------------------------------------------------------------------
EVENT_PAT <- "\\bhajj\\b|\\bumrah\\b|\\bumra\\b|pilgrim|mass gathering|tawaf|muzdalifah|jamarat|ihram|\\barafat\\b|\\barafah\\b"
stopifnot(identical(if_else(str_detect(paste(lw(raw$Title), lw(raw$Abstract), lw(raw$`Author Keywords`),
                                             lw(raw$`Index Keywords`)), EVENT_PAT), "EVENT", "CITY"), raw$SUBGROUP))
d$EVENT_STRICT <- str_detect(paste(lw(raw$Title), lw(raw$`Author Keywords`)), EVENT_PAT)
stopifnot(all(!d$EVENT_STRICT | d$EVENT))                 # strict EVENT is a subset of rule EVENT
write_csv(tibble(EID = raw$EID, SUBGROUP = raw$SUBGROUP,
                 SUBGROUP_STRICT = if_else(d$EVENT_STRICT, "EVENT", "CITY")),
          file.path(OUT, "strict_rule_flags_428.csv"))
fit_s <- glm(TC ~ GROUP + log(AGE) + `Document Type`, family = quasipoisson,
             data = d |> mutate(GROUP = relevel(factor(if_else(EVENT_STRICT, "EVENT", "CITY")), "CITY")))
co_s <- summary(fit_s)$coefficients["GROUPEVENT", ]
strict <- bind_rows(
  tibble(part = "counts", rule = c("rule (title, abstract, author or index keywords)", "strict (title or author keywords)"),
         EVENT = c(sum(d$EVENT), sum(d$EVENT_STRICT)), CITY = 428 - c(sum(d$EVENT), sum(d$EVENT_STRICT))) |>
    mutate(ratio_city_to_event = round(CITY / EVENT, 2),
           event_median_TC = c(median(d$TC[d$EVENT]), median(d$TC[d$EVENT_STRICT])),
           city_median_TC  = c(median(d$TC[!d$EVENT]), median(d$TC[!d$EVENT_STRICT]))),
  bind_rows(cit_row("strict split, all records", all_rows, "TC",  d$EVENT_STRICT),
            cit_row("strict split, all records", all_rows, "TCY", d$EVENT_STRICT),
            cit_row("strict split, excluding 2026", no2026, "TC", d$EVENT_STRICT)) |>
    mutate(part = "citation tests"),
  tibble(part = "quasi-Poisson rate ratio, strict EVENT vs CITY", rule = "TC ~ group + log(age) + document type",
         rate_ratio = round(exp(co_s[["Estimate"]]), 3),
         rr_low = round(exp(co_s[["Estimate"]] - 1.96 * co_s[["Std. Error"]]), 3),
         rr_high = round(exp(co_s[["Estimate"]] + 1.96 * co_s[["Std. Error"]]), 3),
         p = round(co_s[[4]], 4)))
write_csv(strict, file.path(OUT, "strict_rule_sensitivity.csv"), na = "")

# ---------------------------------------------------------------------
# H. S7 / R6-m4 -- growth without the partial year 2026, and
#    over a window both groups share.
# ---------------------------------------------------------------------
pois_growth <- function(y, y0, y1) {
  cnt <- tibble(PY = y0:y1) |> left_join(tibble(PY = y) |> count(PY), by = "PY") |> mutate(n = replace_na(n, 0))
  if (sum(cnt$n) == 0) return(NA_real_)
  (exp(coef(glm(n ~ PY, family = poisson, data = cnt))[["PY"]]) - 1) * 100
}
ev_y <- d$PY[d$EVENT]; ct_y <- d$PY[!d$EVENT]
W0 <- max(min(ev_y), min(ct_y))
while (W0 < 2025 && !(any(ev_y == W0) && any(ct_y == W0))) W0 <- W0 + 1   # first common year with records in both groups
grow25 <- bind_rows(lapply(c("ALL", "EVENT", "CITY"), function(g) {
  y <- switch(g, ALL = d$PY, EVENT = ev_y, CITY = ct_y)
  tibble(group = g,
         first_year = min(y), docs_first_year = sum(y == min(y)),
         growth_first_to_2026 = round(cagr(y, min(y), 2026), 2),
         docs_2025 = sum(y == 2025),
         growth_first_to_2025 = round(cagr(y, min(y), 2025), 2),
         common_window = paste0(W0, "-2025"), docs_window_start = sum(y == W0),
         growth_common_window_endpoints = round(cagr(y, W0, 2025), 2),
         growth_common_window_poisson = round(pois_growth(y, W0, 2025), 2))
}))
write_csv(grow25, file.path(OUT, "subgroup_growth_2025.csv"))

# ---------------------------------------------------------------------
cat("\n[05] Reproduction check:\n"); print(as.data.frame(chk), row.names = FALSE)
cat("\n[05] Citation tests (M7):\n");  print(as.data.frame(cit), row.names = FALSE)
cat("\n[05] Retrieval sensitivity (M17):\n"); print(as.data.frame(sens), row.names = FALSE)
cat("\n[05] Umrah reading list (M4):", sum(um), "records\n")
print(table(decision = ifelse(is.na(um_tab$decision) | um_tab$decision == "", "(none yet)", um_tab$decision)))
cat("[05] Validation sample (M1): 120 records; allocation by band:\n")
print(table(samp$SUBGROUP, samp$BAND))
cat("\n[05] Subgroup growth (S7):\n"); print(as.data.frame(grow), row.names = FALSE)
cat("\n[05] Growth to 2025 and over a common window:\n"); print(as.data.frame(grow25), row.names = FALSE)
cat("\n[05] Strict-anchor sensitivity (R6-MC2):\n"); print(as.data.frame(strict), row.names = FALSE)

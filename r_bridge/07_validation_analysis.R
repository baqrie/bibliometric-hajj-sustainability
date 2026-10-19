# =====================================================================
# 07 — VALIDATION ANALYSIS
#
# Reads:  db/coding/validation_coder_A.xlsx, db/coding/validation_coder_B.xlsx
#         results/revision/validation_sample_key.csv       (auto labels)
#         db/final_corpus_v428.csv                          (raw text: checks + strict rule)
#         results/revision/retrieval_flags_428.csv          (anchor source, optional)
#         db/coding/adjudication.csv                        (pass 2)
#         results/revision/expected_validation_values_2026-10-07.csv (optional check)
# Writes (labels and numbers only, no abstracts -- safe to share):
#   results/revision/validation_key_check.csv       (key vs coder sheets vs corpus)
#   results/revision/validation_reliability.csv
#   results/revision/validation_validity.csv        (pass 2)
#   results/revision/validation_confusion.csv       (pass 2)
#   results/revision/validation_eligibility.csv     (pass 2)
#   results/revision/validation_records.csv         (pass 2, per record)
#   results/revision/validation_text_values.csv     (pass 2, every number the text cites)
#
# Needs: dplyr, readr, tidyr, stringr, readxl (no bibliometrix). readxl replaces
# openxlsx here: openxlsx stops with "Workbook has no worksheets" on Coder B's
# file, whose XML uses namespace prefixes (x:sheet) that its parser does not read.
# =====================================================================
suppressPackageStartupMessages({
  library(dplyr); library(readr); library(tidyr); library(stringr); library(readxl)
})

OUT   <- "results/revision"
CODE  <- "db/coding"
SEED  <- 2026
BOOT  <- 2000
N_STRATUM <- c(EVENT = 121, CITY = 307)   # corpus sizes of the two strata
N_CORPUS  <- sum(N_STRATUM)
CODER_A_FILE <- file.path(CODE, "validation_coder_A.xlsx")      # Coder A = U.S.S.
CODER_B_FILE <- file.path(CODE, "validation_coder_B.xlsx")      # Coder B = F.M.
ADJ        <- file.path(CODE, "adjudication.csv")
CORPUS_RAW <- "db/final_corpus_v428.csv"
EXPECTED   <- file.path(OUT, "expected_validation_values_2026-10-07.csv")
# The subgroup rule, verbatim from 01_screening_pipeline.R, stage 5.
EVENT_PAT <- "\\bhajj\\b|\\bumrah\\b|\\bumra\\b|pilgrim|mass gathering|tawaf|muzdalifah|jamarat|ihram|\\barafat\\b|\\barafah\\b"

LAB3 <- c("EVENT", "CITY", "HYBRID")
DIR  <- c("LEAN_EVENT", "LEAN_CITY")
REL  <- c("PRIMARY", "INDIRECT", "INCIDENTAL")           # ordered
ELI  <- c("ELIGIBLE", "BORDERLINE", "FALSE_POSITIVE")    # ordered
SCO  <- c("PILGRIMAGE", "HOLY_CITY", "REGION_ONLY", "NONE")
# scope_link records a FACT, not a decision: does the record's own text
# tie it to the pilgrimage, to Makkah/Madinah as a study site, or only to
# the surrounding region (e.g. Jeddah, Makkah Province)? NONE = outside
# the review scope

# Title key for matching coder sheets to corpus records. openxlsx can
# return non-ASCII characters as XML entities (&#956;) and Scopus titles
# can carry tags (<sub>), so entities, tags and every non-alphanumeric
# character are dropped before comparing.
title_key <- function(x) x |> replace_na("") |> str_replace_all("&#?[A-Za-z0-9]+;", "") |>
  str_replace_all("<[^>]+>", "") |> str_to_lower() |> str_replace_all("[^a-z0-9]", "")

# ---------------------------------------------------------------------
# 1. Read and check the coder sheets
# ---------------------------------------------------------------------
read_coder <- function(path, who) {
  if (!file.exists(path)) stop("Missing coder sheet: ", path)
  x <- as.data.frame(read_excel(path, sheet = "Coding", col_types = "text"), check.names = FALSE)
  x <- x[!is.na(x$CODING_ID) & x$CODING_ID != "V000", ]   # V000 is the worked example row
  need <- c("CODING_ID", "Year", "Title", "label", "hybrid_direction", "relevance", "eligibility",
            "scope_link", "confidence")
  miss <- setdiff(need, names(x))
  if (length(miss)) stop(path, " lacks columns: ", paste(miss, collapse = ", "))
  x <- x |> transmute(CODING_ID = trimws(CODING_ID),
                      Year_sheet = as.integer(Year),
                      TITLE_KEY = title_key(Title),
                      label = toupper(trimws(label)),
                      hybrid_direction = toupper(trimws(hybrid_direction)),
                      relevance = toupper(trimws(relevance)),
                      eligibility = toupper(trimws(eligibility)),
                      scope_link = toupper(trimws(scope_link)),
                      confidence = toupper(trimws(confidence))) |>
    mutate(across(c(label, hybrid_direction, relevance, eligibility, scope_link, confidence), ~ na_if(.x, "")))
  # Completeness and allowed values: stop early with a readable message.
  bad <- c(
    sprintf("%s: %d records with no label", who, sum(is.na(x$label))),
    sprintf("%s: labels outside EVENT/CITY/HYBRID: %s", who,
            paste(unique(na.omit(x$label[!x$label %in% LAB3])), collapse = ", ")),
    sprintf("%s: HYBRID without a direction: %s", who,
            paste(x$CODING_ID[x$label %in% "HYBRID" & !x$hybrid_direction %in% DIR], collapse = ", ")),
    sprintf("%s: relevance outside %s: %s", who, paste(REL, collapse = "/"),
            paste(unique(x$relevance[!x$relevance %in% REL]), collapse = ", ")),
    sprintf("%s: eligibility outside %s: %s", who, paste(ELI, collapse = "/"),
            paste(unique(x$eligibility[!x$eligibility %in% ELI]), collapse = ", ")),
    sprintf("%s: scope_link outside %s: %s", who, paste(SCO, collapse = "/"),
            paste(unique(x$scope_link[!x$scope_link %in% SCO]), collapse = ", ")))
  bad <- bad[!grepl(": 0 records|: $", bad)]
  if (length(bad)) stop("Fix the coder sheet first:\n  ", paste(bad, collapse = "\n  "))
  x
}

key <- read_csv(file.path(OUT, "validation_sample_key.csv"), show_col_types = FALSE)
A <- read_coder(CODER_A_FILE, "Coder A")
B <- read_coder(CODER_B_FILE, "Coder B")
stopifnot(nrow(key) == 120, !anyDuplicated(key$CODING_ID), !anyDuplicated(key$EID),
          setequal(A$CODING_ID, key$CODING_ID), setequal(B$CODING_ID, key$CODING_ID),
          sum(key$AUTO_LABEL == "EVENT") == 60, sum(key$AUTO_LABEL == "CITY") == 60)

# ---------------------------------------------------------------------
# 1b. Key integrity. The coders saw CODING_IDs only; the key
#     links each CODING_ID to an EID. Check that the key points to the
#     records the coders actually read (same title and year), and that its
#     AUTO_LABEL is what the rule gives on the corpus text.
# ---------------------------------------------------------------------
lw <- function(x) str_to_lower(replace_na(as.character(x), ""))
has_corpus <- file.exists(CORPUS_RAW)
if (has_corpus) {
  raw <- read_csv(CORPUS_RAW, show_col_types = FALSE, guess_max = Inf)
  stopifnot(nrow(raw) == 428)
  rk <- raw |> transmute(EID,
                         TITLE_KEY_CORPUS = title_key(Title),
                         Year_corpus = as.integer(Year),
                         SUBGROUP_CORPUS = SUBGROUP,
                         RULE_RECOMPUTED = if_else(str_detect(paste(lw(Title), lw(Abstract), lw(`Author Keywords`),
                                                                    lw(`Index Keywords`)), EVENT_PAT), "EVENT", "CITY"),
                         STRICT_LABEL = if_else(str_detect(paste(lw(Title), lw(`Author Keywords`)), EVENT_PAT),
                                                "EVENT", "CITY"))
  kc <- key |> select(CODING_ID, EID, Year, AUTO_LABEL) |>
    left_join(rk, by = "EID") |>
    left_join(A |> select(CODING_ID, A_title = TITLE_KEY, A_year = Year_sheet), by = "CODING_ID") |>
    left_join(B |> select(CODING_ID, B_title = TITLE_KEY, B_year = Year_sheet), by = "CODING_ID") |>
    mutate(in_corpus     = !is.na(TITLE_KEY_CORPUS),
           title_A_ok    = in_corpus & A_title == TITLE_KEY_CORPUS,
           title_B_ok    = in_corpus & B_title == TITLE_KEY_CORPUS,
           year_ok       = in_corpus & Year == Year_corpus & A_year == Year & B_year == Year,
           auto_label_ok = in_corpus & AUTO_LABEL == SUBGROUP_CORPUS & AUTO_LABEL == RULE_RECOMPUTED)
  write_csv(kc |> select(CODING_ID, EID, Year, AUTO_LABEL, RULE_RECOMPUTED, STRICT_LABEL,
                         in_corpus, title_A_ok, title_B_ok, year_ok, auto_label_ok),
            file.path(OUT, "validation_key_check.csv"))
  bad_key <- kc |> filter(!(in_corpus & title_A_ok & title_B_ok & year_ok & auto_label_ok))
  if (nrow(bad_key)) {
    print(as.data.frame(bad_key |> select(CODING_ID, EID, in_corpus, title_A_ok, title_B_ok, year_ok, auto_label_ok)),
          row.names = FALSE)
    stop("validation_sample_key.csv does not match the coder sheets or the corpus for the records above. ",
         "Do not continue: restore the frozen key (the one the coder workbooks were built from).")
  }
  cat("\n[07] Key check passed: all 120 CODING_IDs point to the records the coders read,",
      "and every AUTO_LABEL equals the rule recomputed on the corpus text.\n")
  key <- key |> left_join(rk |> select(EID, STRICT_LABEL), by = "EID")
} else {
  warning("[07] ", CORPUS_RAW, " not found: key checks and the strict-rule results are skipped.")
}

collapse2 <- function(lab, dir) ifelse(lab == "HYBRID", ifelse(dir == "LEAN_EVENT", "EVENT", "CITY"), lab)
AB <- key |>
  left_join(A |> select(-Year_sheet, -TITLE_KEY) |> rename_with(~ paste0("A_", .x), -CODING_ID), by = "CODING_ID") |>
  left_join(B |> select(-Year_sheet, -TITLE_KEY) |> rename_with(~ paste0("B_", .x), -CODING_ID), by = "CODING_ID") |>
  mutate(A_bin = collapse2(A_label, A_hybrid_direction),
         B_bin = collapse2(B_label, B_hybrid_direction))

# ---------------------------------------------------------------------
# 2. Coder agreement: Cohen's kappa (unweighted; linear-weighted for the
#    two ordered variables). Two 95% CIs: the large-sample standard error
#    of Fleiss, Cohen & Everitt (1969), which the paper reports, and a
#    record-level bootstrap.
# ---------------------------------------------------------------------
kappa <- function(a, b, levels, weighted = FALSE) {
  a <- factor(a, levels); b <- factor(b, levels)
  t <- table(a, b); n <- sum(t); p <- t / n
  k <- length(levels)
  w <- if (weighted) 1 - abs(outer(seq_len(k), seq_len(k), "-")) / (k - 1) else diag(k)
  po <- sum(w * p)
  pe <- sum(w * outer(rowSums(p), colSums(p)))
  if (isTRUE(all.equal(pe, 1))) return(NA_real_)   # no variation: kappa undefined
  (po - pe) / (1 - pe)
}
# Fleiss, Cohen & Everitt (1969) large-sample SE of unweighted kappa:
#   SE = sqrt((A + B - C) / n) / (1 - pe), with
#   A = sum_i p_ii [1 - (p_i. + p_.i)(1 - k)]^2
#   B = (1 - k)^2 sum_{i != j} p_ij (p_.i + p_j.)^2
#   C = [k - pe (1 - k)]^2
kappa_se_fce <- function(a, b, levels) {
  a <- factor(a, levels); b <- factor(b, levels)
  p <- table(a, b) / length(a); r <- rowSums(p); cl <- colSums(p)
  po <- sum(diag(p)); pe <- sum(r * cl); k <- (po - pe) / (1 - pe)
  Aterm <- sum(diag(p) * (1 - (r + cl) * (1 - k))^2)
  off <- outer(cl, r, "+")                       # element [i, j] = p_.i + p_j.
  Bterm <- (1 - k)^2 * sum((p * off^2)[row(p) != col(p)])
  Cterm <- (k - pe * (1 - k))^2
  sqrt((Aterm + Bterm - Cterm) / length(a)) / (1 - pe)
}
rel_row <- function(variable, a, b, levels, weighted = FALSE) {
  set.seed(SEED)
  n <- length(a)
  bs <- replicate(BOOT, { i <- sample.int(n, n, replace = TRUE); kappa(a[i], b[i], levels, weighted) })
  ci <- quantile(bs, c(.025, .975), na.rm = TRUE)
  k  <- kappa(a, b, levels, weighted)
  se <- if (weighted) NA_real_ else kappa_se_fce(a, b, levels)
  tibble(variable, statistic = if (weighted) "linear-weighted kappa" else "Cohen's kappa",
         n = n, agreements = sum(a == b), observed_agreement = round(mean(a == b), 3),
         kappa = round(k, 3),
         ci_low_fce = round(k - 1.96 * se, 3), ci_high_fce = round(k + 1.96 * se, 3),
         ci_low_boot = round(ci[[1]], 3), ci_high_boot = round(ci[[2]], 3))
}
reliab <- bind_rows(
  rel_row("Analytical object, 3 labels (EVENT/CITY/HYBRID) [kappa3]", AB$A_label, AB$B_label, LAB3),
  rel_row("Analytical object, collapsed to EVENT/CITY [kappa2]", AB$A_bin, AB$B_bin, c("EVENT", "CITY")),
  rel_row("Environmental relevance (PRIMARY/INDIRECT/INCIDENTAL)", AB$A_relevance, AB$B_relevance, REL),
  rel_row("Environmental relevance (ordered)", AB$A_relevance, AB$B_relevance, REL, weighted = TRUE),
  rel_row("Environmental eligibility (ELIGIBLE/BORDERLINE/FALSE_POSITIVE)", AB$A_eligibility, AB$B_eligibility, ELI),
  rel_row("Environmental eligibility (ordered)", AB$A_eligibility, AB$B_eligibility, ELI, weighted = TRUE),
  rel_row("Scope link (PILGRIMAGE/HOLY_CITY/REGION_ONLY/NONE)", AB$A_scope_link, AB$B_scope_link, SCO))
write_csv(reliab, file.path(OUT, "validation_reliability.csv"))

# Disagreement counts and HYBRID specific agreement (cited in Section 3.3.1)
dis_vals <- with(AB, (A_label != B_label) + (A_relevance != B_relevance) + (A_eligibility != B_eligibility) +
                   (A_scope_link != B_scope_link) +
                   (A_label == "HYBRID" & B_label == "HYBRID" & A_hybrid_direction != B_hybrid_direction))
spec_hybrid <- 2 * sum(AB$A_label == "HYBRID" & AB$B_label == "HYBRID") /
  (sum(AB$A_label == "HYBRID") + sum(AB$B_label == "HYBRID"))
cat("\n[07] Coder agreement (Coder A = U.S.S., Coder B = F.M.; kappa describes the coders, not the rule):\n")
print(as.data.frame(reliab), row.names = FALSE)
cat(sprintf("[07] Records with a disputed value: %d; disputed values: %d; specific agreement on HYBRID: %.2f\n",
            sum(dis_vals > 0), sum(dis_vals), spec_hybrid))

# ---------------------------------------------------------------------
# 3. Adjudication sheet. Agreements are pre-filled; the adjudicator fills
#    the blanks (and may overwrite any pre-filled value, with a note).
# ---------------------------------------------------------------------
if (!file.exists(ADJ)) {
  agree <- function(a, b) ifelse(a == b, a, "")
  adj <- AB |> transmute(
    CODING_ID,
    A_label, B_label, A_hybrid_direction, B_hybrid_direction,
    A_relevance, B_relevance, A_eligibility, B_eligibility, A_scope_link, B_scope_link,
    consensus_label       = agree(A_label, B_label),
    consensus_direction   = ifelse(A_label == "HYBRID" & B_label == "HYBRID",
                                   agree(A_hybrid_direction, B_hybrid_direction), ""),
    consensus_relevance   = agree(A_relevance, B_relevance),
    consensus_eligibility = agree(A_eligibility, B_eligibility),
    consensus_scope_link  = agree(A_scope_link, B_scope_link),
    route = ifelse(consensus_label != "" & consensus_relevance != "" & consensus_eligibility != "" &
                     consensus_scope_link != "" &
                     (consensus_label != "HYBRID" | consensus_direction != ""),
                   "agreed", ""),
    adjudication_note = "")
  write_csv(adj, ADJ, na = "")
  cat("\n[07] Wrote", ADJ, "-", sum(adj$route != "agreed"), "records need adjudication.\n",
      "     Fill consensus_* for those rows, set route to 'discussion' or 'third_reader',\n",
      "     add a note, then run this script again for the validity results.\n")
  quit(save = "no", status = 0)
}

# ---------------------------------------------------------------------
# 4. Validity of the automated rule against the consensus (pass 2)
# ---------------------------------------------------------------------
adj <- read_csv(ADJ, show_col_types = FALSE, col_types = cols(.default = "c")) |>
  mutate(across(starts_with("consensus_"), ~ toupper(trimws(replace_na(.x, "")))))
open_rows <- adj |> filter(consensus_label == "" | consensus_relevance == "" | consensus_eligibility == "" |
                           consensus_scope_link == "" |
                           (consensus_label == "HYBRID" & !consensus_direction %in% DIR))
if (nrow(open_rows)) stop("Adjudication incomplete for: ", paste(open_rows$CODING_ID, collapse = ", "))
stopifnot(all(adj$consensus_label %in% LAB3), all(adj$consensus_relevance %in% REL),
          all(adj$consensus_eligibility %in% ELI), all(adj$consensus_scope_link %in% SCO),
          setequal(adj$CODING_ID, key$CODING_ID),
          all(adj$consensus_direction[adj$consensus_label != "HYBRID"] == ""))

flags <- if (file.exists(file.path(OUT, "retrieval_flags_428.csv")))
  read_csv(file.path(OUT, "retrieval_flags_428.csv"), show_col_types = FALSE) |>
    select(EID, ANCHOR_SOURCE, JEDDAH_SUBJECT) else NULL

V <- key |>
  inner_join(adj |> select(CODING_ID, starts_with("consensus_"), route), by = "CODING_ID") |>
  mutate(IN_SCOPE = consensus_scope_link != "NONE",
         TRUE_BIN = collapse2(consensus_label, consensus_direction),
         ENVIRONMENTAL = consensus_eligibility %in% c("ELIGIBLE", "BORDERLINE"),
         RULE_CORRECT = if_else(IN_SCOPE, TRUE_BIN == AUTO_LABEL, NA))
if (!is.null(flags)) V <- V |> left_join(flags, by = "EID")
has_strict <- "STRICT_LABEL" %in% names(V)

# Design-weighted projection. n_h counts every sampled record of the
# stratum (out-of-scope draws included), so each record stands for N_h/n_h
# corpus records and the projected parts add up to 428.
project <- function(v) {
  nE <- sum(v$AUTO_LABEL == "EVENT"); nC <- sum(v$AUTO_LABEL == "CITY")
  wE <- N_STRATUM[["EVENT"]] / nE;     wC <- N_STRATUM[["CITY"]] / nC
  s  <- v[v$IN_SCOPE, ]
  cnt <- function(rule, truth, d = s) sum(d$AUTO_LABEL == rule & d$TRUE_BIN == truth)
  tp <- cnt("EVENT", "EVENT"); fp <- cnt("EVENT", "CITY"); fn <- cnt("CITY", "EVENT"); tn <- cnt("CITY", "CITY")
  TP <- tp * wE; FP <- fp * wE; FN <- fn * wC; TN <- tn * wC
  prec <- tp / (tp + fp); rec <- TP / (TP + FN)
  w3 <- function(lab) sum(s$consensus_label == lab & s$AUTO_LABEL == "EVENT") * wE +
                      sum(s$consensus_label == lab & s$AUTO_LABEL == "CITY") * wC
  oos <- sum(!v$IN_SCOPE & v$AUTO_LABEL == "EVENT") * wE + sum(!v$IN_SCOPE & v$AUTO_LABEL == "CITY") * wC
  nonE <- sum(s$AUTO_LABEL == "EVENT" & !s$ENVIRONMENTAL); nonC <- sum(s$AUTO_LABEL == "CITY" & !s$ENVIRONMENTAL)
  inE <- sum(s$AUTO_LABEL == "EVENT"); inC <- sum(s$AUTO_LABEL == "CITY")
  env <- s[s$ENVIRONMENTAL, ]
  out <- c(
    precision_EVENT = prec,
    autoCITY_correct = tn / (fn + tn),
    stratum_error_autoEVENT = 1 - prec,
    stratum_error_autoCITY  = fn / (fn + tn),
    recall_EVENT = rec,
    specificity = TN / (TN + FP),
    accuracy = (TP + TN) / (TP + FP + FN + TN),
    F1_EVENT = 2 * prec * rec / (prec + rec),
    corrected_EVENT = TP + FN,
    corrected_CITY  = FP + TN,
    corrected_ratio_CITY_to_EVENT = (FP + TN) / (TP + FN),
    weighted_EVENT_3cat  = w3("EVENT"),
    weighted_HYBRID_3cat = w3("HYBRID"),
    weighted_CITY_3cat   = w3("CITY"),
    out_of_scope_projected = oos,
    out_of_scope_share = oos / N_CORPUS,
    nonenv_share_autoEVENT = nonE / inE,
    nonenv_share_autoCITY  = nonC / inC,
    nonenv_share_corpus = (nonE * wE + nonC * wC) / N_CORPUS,
    ratio_rule_environmental_only = (sum(env$AUTO_LABEL == "CITY") * wC) / (sum(env$AUTO_LABEL == "EVENT") * wE),
    ratio_corrected_environmental_only =
      (cnt("EVENT", "CITY", env) * wE + cnt("CITY", "CITY", env) * wC) /
      (cnt("EVENT", "EVENT", env) * wE + cnt("CITY", "EVENT", env) * wC))
  if (has_strict) {
    stp <- sum(s$STRICT_LABEL == "EVENT" & s$TRUE_BIN == "EVENT")
    sfp <- sum(s$STRICT_LABEL == "EVENT" & s$TRUE_BIN == "CITY")
    sfnE <- sum(s$AUTO_LABEL == "EVENT" & s$STRICT_LABEL == "CITY" & s$TRUE_BIN == "EVENT")
    out <- c(out, strict_precision_EVENT = stp / (stp + sfp),
             strict_recall_EVENT = (stp * wE) / (stp * wE + sfnE * wE + fn * wC))
  }
  out
}
est <- project(V)
set.seed(SEED)
ve <- V |> filter(AUTO_LABEL == "EVENT"); vc <- V |> filter(AUTO_LABEL == "CITY")
bs <- replicate(BOOT, project(bind_rows(ve[sample.int(nrow(ve), replace = TRUE), ],   # stratified
                                        vc[sample.int(nrow(vc), replace = TRUE), ])))
basis <- c(precision_EVENT = "within auto-EVENT stratum, in-scope records (unweighted)",
           autoCITY_correct = "within auto-CITY stratum, in-scope records (unweighted)",
           stratum_error_autoEVENT = "within auto-EVENT stratum, in-scope records (unweighted)",
           stratum_error_autoCITY = "within auto-CITY stratum, in-scope records (unweighted)",
           nonenv_share_autoEVENT = "within auto-EVENT stratum, in-scope records (unweighted)",
           nonenv_share_autoCITY = "within auto-CITY stratum, in-scope records (unweighted)",
           strict_precision_EVENT = "within strict-EVENT records, in scope (unweighted)")
validity <- tibble(
  measure = names(est), estimate = round(unname(est), 3),
  ci_low  = round(apply(bs, 1, quantile, .025, na.rm = TRUE), 3),
  ci_high = round(apply(bs, 1, quantile, .975, na.rm = TRUE), 3)) |>
  mutate(basis = coalesce(unname(basis[measure]), "projected to the corpus with stratum weights 121/60 and 307/60"))
write_csv(validity, file.path(OUT, "validation_validity.csv"))

# Confusion matrix, in-scope records, plus the out-of-scope count per stratum
conf <- V |> mutate(consensus = if_else(IN_SCOPE, TRUE_BIN, "OUT_OF_SCOPE")) |>
  count(AUTO_LABEL, consensus) |>
  pivot_wider(names_from = consensus, values_from = n, values_fill = 0, names_prefix = "consensus_")
write_csv(conf, file.path(OUT, "validation_confusion.csv"))

# M19: eligibility by consensus object (in-scope), and scope link by stratum
n_h <- V |> count(AUTO_LABEL, name = "n_sampled")
n_in <- V |> filter(IN_SCOPE) |> count(AUTO_LABEL, name = "n_in_scope")
elig_tab <- V |> filter(IN_SCOPE) |> count(AUTO_LABEL, TRUE_BIN, consensus_eligibility) |>
  left_join(n_h, by = "AUTO_LABEL") |> left_join(n_in, by = "AUTO_LABEL") |>
  mutate(share_of_in_scope = round(n / n_in_scope, 3),
         projected_corpus_n = round(n * N_STRATUM[AUTO_LABEL] / n_sampled, 1),
         table = "eligibility x object (in scope)")
scope_tab <- V |> count(AUTO_LABEL, consensus_scope_link) |>
  left_join(n_h, by = "AUTO_LABEL") |>
  mutate(share_of_stratum = round(n / n_sampled, 3),
         projected_corpus_n = round(n * N_STRATUM[AUTO_LABEL] / n_sampled, 1),
         table = "scope link by stratum (all sampled)")
elig_tab <- bind_rows(elig_tab, scope_tab)
if ("ANCHOR_SOURCE" %in% names(V)) {
  elig_anchor <- V |> count(AUTO_LABEL, ANCHOR_SOURCE, consensus_scope_link, consensus_eligibility, name = "n") |>
    mutate(table = "anchor source x scope link x eligibility")
  elig_tab <- bind_rows(elig_tab, elig_anchor)
}
write_csv(elig_tab, file.path(OUT, "validation_eligibility.csv"))

write_csv(V |> select(CODING_ID, EID, Year, BAND, AUTO_LABEL, any_of("STRICT_LABEL"), IN_SCOPE, TRUE_BIN, RULE_CORRECT,
                      consensus_label, consensus_direction, consensus_relevance,
                      consensus_eligibility, consensus_scope_link, route,
                      any_of(c("ANCHOR_SOURCE", "JEDDAH_SUBJECT"))),
          file.path(OUT, "validation_records.csv"))

# ---------------------------------------------------------------------
# 5. Every number the text cites, in one file
# ---------------------------------------------------------------------
s_in <- V |> filter(IN_SCOPE)
err  <- s_in |> filter(AUTO_LABEL == "EVENT", TRUE_BIN == "CITY")
vrow <- function(m) validity |> filter(measure == m)
tv <- function(item, value, m = NULL, used_in = "") {
  if (!is.null(m)) { r <- vrow(m); tibble(item, value = round(value, 3), ci_low = r$ci_low, ci_high = r$ci_high, used_in) }
  else tibble(item, value = round(value, 3), ci_low = NA_real_, ci_high = NA_real_, used_in)
}
krow <- function(pat) reliab |> filter(grepl(pat, variable, fixed = TRUE), statistic == "Cohen's kappa")
kv <- function(item, pat) { r <- krow(pat); tibble(item, value = r$kappa, ci_low = r$ci_low_fce, ci_high = r$ci_high_fce, used_in = "R-19 (FCE 1969 CI)") }
text_values <- bind_rows(
  kv("kappa: subgroup label (3 labels)", "[kappa3]"),
  kv("kappa: relevance", "Environmental relevance (PRIMARY"),
  kv("kappa: eligibility", "Environmental eligibility (ELIGIBLE"),
  kv("kappa: scope link", "Scope link"),
  tv("specific agreement on HYBRID", spec_hybrid, used_in = "R-19"),
  tv("records with a disputed value", sum(dis_vals > 0), used_in = "R-19"),
  tv("disputed values", sum(dis_vals), used_in = "reconciliation log"),
  tv("in-scope records", nrow(s_in), used_in = "R-19"),
  tv("in-scope records, auto-EVENT", sum(s_in$AUTO_LABEL == "EVENT"), used_in = "R-19"),
  tv("in-scope records, auto-CITY", sum(s_in$AUTO_LABEL == "CITY"), used_in = "R-19"),
  tv("rule EVENT label correct (n)", sum(s_in$AUTO_LABEL == "EVENT" & s_in$TRUE_BIN == "EVENT"), used_in = "R-19"),
  tv("rule EVENT precision", est[["precision_EVENT"]], "precision_EVENT", "R-19"),
  tv("rule CITY label correct (n)", sum(s_in$AUTO_LABEL == "CITY" & s_in$TRUE_BIN == "CITY"), used_in = "R-19"),
  tv("rule errors (auto EVENT, consensus CITY)", nrow(err), used_in = "R-19"),
  tv("rule errors: consensus CITY (pilgrimage only as context)", sum(err$consensus_label == "CITY"), used_in = "R-19"),
  tv("rule errors: consensus HYBRID leaning to the city", sum(err$consensus_label == "HYBRID"), used_in = "R-19"),
  tv("weighted recall", est[["recall_EVENT"]], "recall_EVENT", "R-19"),
  tv("weighted specificity", est[["specificity"]], "specificity", "R-19"),
  tv("weighted accuracy", est[["accuracy"]], "accuracy", "R-19"),
  tv("weighted F1", est[["F1_EVENT"]], "F1_EVENT", "R-19"),
  tv("weighted count EVENT (3 categories)", est[["weighted_EVENT_3cat"]], "weighted_EVENT_3cat", "R-19"),
  tv("weighted count HYBRID (3 categories)", est[["weighted_HYBRID_3cat"]], "weighted_HYBRID_3cat", "R-19"),
  tv("weighted count CITY (3 categories)", est[["weighted_CITY_3cat"]], "weighted_CITY_3cat", "R-19"),
  tv("corrected EVENT (HYBRID by lean)", est[["corrected_EVENT"]], "corrected_EVENT", "R-18 Table 8"),
  tv("corrected CITY (HYBRID by lean)", est[["corrected_CITY"]], "corrected_CITY", "R-18 Table 8"),
  tv("corrected host-city-to-event ratio", est[["corrected_ratio_CITY_to_EVENT"]], "corrected_ratio_CITY_to_EVENT", "A-1, R-14, R-19"),
  tv("no environmental analysis, auto-EVENT (n of in-scope)", sum(s_in$AUTO_LABEL == "EVENT" & !s_in$ENVIRONMENTAL), used_in = "R-19"),
  tv("no environmental analysis, auto-CITY (n of in-scope)", sum(s_in$AUTO_LABEL == "CITY" & !s_in$ENVIRONMENTAL), used_in = "R-19"),
  tv("no environmental analysis, share of event group", est[["nonenv_share_autoEVENT"]], "nonenv_share_autoEVENT", "R-19, F-1, D-1, D-5"),
  tv("no environmental analysis, share of host-city group", est[["nonenv_share_autoCITY"]], "nonenv_share_autoCITY", "R-19, F-1, D-1, D-5"),
  tv("no environmental analysis, share of corpus", est[["nonenv_share_corpus"]], "nonenv_share_corpus", "C-5"),
  tv("rule ratio, environmental records only", est[["ratio_rule_environmental_only"]], "ratio_rule_environmental_only", "R-19"),
  tv("corrected ratio, environmental records only", est[["ratio_corrected_environmental_only"]], "ratio_corrected_environmental_only", "Table S5 only"),
  tv("out of scope (n sampled)", sum(!V$IN_SCOPE), used_in = "R-19"),
  tv("out of scope, projected records", est[["out_of_scope_projected"]], "out_of_scope_projected", "R-18, R-19"),
  tv("out of scope, share of corpus", est[["out_of_scope_share"]], "out_of_scope_share", "R-19, C-5"),
  tv("REGION_ONLY among sampled auto-CITY records", sum(V$AUTO_LABEL == "CITY" & V$consensus_scope_link == "REGION_ONLY"),
     used_in = "M-1 note"))
if (has_strict) text_values <- bind_rows(text_values,
  tv("strict rule precision", est[["strict_precision_EVENT"]], "strict_precision_EVENT", "R-19"),
  tv("strict rule weighted recall", est[["strict_recall_EVENT"]], "strict_recall_EVENT", "R-19"))
if (file.exists(EXPECTED)) {
  ex <- read_csv(EXPECTED, show_col_types = FALSE) |> select(item, expected_7oct = value)
  text_values <- text_values |> left_join(ex, by = "item") |>
    mutate(check = case_when(is.na(expected_7oct) ~ "", abs(value - expected_7oct) < 0.0015 ~ "MATCH", TRUE ~ "DIFF"))
}
write_csv(text_values, file.path(OUT, "validation_text_values.csv"), na = "")

cat("\n[07] Confusion matrix (rows = automated rule, columns = consensus; raw sample counts):\n")
print(as.data.frame(conf), row.names = FALSE)
cat("\n[07] Rule validity (in-scope records; report these, not kappa, for the rule):\n")
print(as.data.frame(validity), row.names = FALSE)
cat("\n[07] Misclassified in-scope records (read these to find the error mechanism):\n")
print(as.data.frame(V |> filter(IN_SCOPE, !RULE_CORRECT) |> select(CODING_ID, EID, AUTO_LABEL, TRUE_BIN, consensus_label)),
      row.names = FALSE)
cat("\n[07] Out-of-scope records (excluded from validity, reported separately):\n")
print(as.data.frame(V |> filter(!IN_SCOPE) |> select(CODING_ID, EID, AUTO_LABEL, consensus_label)), row.names = FALSE)
cat("\n[07] Numbers for the text (results/revision/validation_text_values.csv):\n")
print(as.data.frame(text_values), row.names = FALSE)
if ("check" %in% names(text_values) && any(text_values$check == "DIFF"))
  cat("\n[07] WARNING: some values differ from the 7 Oct reconciliation workbook; see the 'check' column.\n")

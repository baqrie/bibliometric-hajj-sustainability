# =====================================================================
# 00 — RUN THE REVISION PIPELINE IN ORDER   (added 8 Oct 2026)
#
# Run from the repo root:
#   Rscript r_bridge/00_run_pipeline.R              # every step, in order
#   Rscript r_bridge/00_run_pipeline.R 05 07        # only the named steps
#   Rscript r_bridge/00_run_pipeline.R --check      # preflight checks only
#   Rscript r_bridge/00_run_pipeline.R 09           # only the optional step 09 (if its inputs are ready)
#
# Each step runs in its own Rscript process, with its stdout/stderr written to a log file in results/revision/logs/.
#
# Steps that need bibliometrix (run_analysis, 04, 06, vosViewer) are
# skipped with a message when bibliometrix is not installed.
# =====================================================================
args <- commandArgs(trailingOnly = TRUE)
CHECK_ONLY <- "--check" %in% args
ONLY <- setdiff(args, "--check")

LOG_DIR <- "results/revision/logs"
dir.create(LOG_DIR, showWarnings = FALSE, recursive = TRUE)
RSCRIPT <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")
RAW_EXPORT <- c("db/scopus_export_Jul 16-2026_new_query_1691.csv",
                "db/scopus_export_Jul_16-2026_new_query_1691.csv")

KNOWN_MD5 <- c(raw_export = "39bb6bfc4326170edb38a9d34fcf85c5",
               coder_A    = "9410c45769f63645dd9a15aeb2d50598",
               coder_B    = "fc872ea4a2a2c53b35e4031f17a15b98")

say <- function(...) cat(sprintf(...), "\n", sep = "")
first_existing <- function(p) { p <- p[file.exists(p)]; if (length(p)) p[1] else NA_character_ }
has_pkg <- function(p) requireNamespace(p, quietly = TRUE)

steps <- list(
  list(id = "01", script = "r_bridge/01_screening_pipeline.R", bib = FALSE,
       inputs  = list(RAW_EXPORT, "results/triage_madinah_ruleM_decisions.csv"),
       outputs = c("db/final_corpus_v428.csv", "results/screening_log_1691.csv")),
  list(id = "02", script = "r_bridge/02_keyword_normalization_v3.R", bib = FALSE,
       inputs  = list("db/final_corpus_v428.csv", "results/normalize_terms.txt", "results/normalize_geographic.txt"),
       outputs = c("db/final_corpus_normalized_428.csv")),
  list(id = "05", script = "r_bridge/05_revision_diagnostics.R", bib = FALSE,
       inputs  = list("db/final_corpus_v428.csv", "db/final_corpus_normalized_428.csv"),
       outputs = c("results/revision/reproduction_check.csv", "results/revision/validation_sample_key.csv",
                   "results/revision/retrieval_flags_428.csv", "results/revision/strict_rule_sensitivity.csv",
                   "results/revision/subgroup_growth_2025.csv")),
  list(id = "07", script = "r_bridge/07_validation_analysis.R", bib = FALSE,
       inputs  = list("db/coding/validation_coder_A.xlsx", "db/coding/validation_coder_B.xlsx",
                      "db/coding/adjudication.csv", "results/revision/validation_sample_key.csv",
                      "db/final_corpus_v428.csv"),
       outputs = c("results/revision/validation_reliability.csv", "results/revision/validation_validity.csv",
                   "results/revision/validation_text_values.csv", "results/revision/validation_records.csv")),
  list(id = "08", script = "r_bridge/08_corpus_diagnostics.R", bib = FALSE,
       inputs  = list("db/final_corpus_v428.csv"),
       outputs = c("results/revision/corpus_diagnostics_summary.csv")),
  list(id = "run_analysis", script = "r_bridge/run_analysis.R", bib = TRUE,
       inputs  = list("db/final_corpus_normalized_428.csv", "results/normalize_terms.txt",
                      "results/normalize_geographic.txt", "results/author_thesaurus.txt",
                      "results/affiliation_thesaurus.txt"),
       outputs = c("results/manuscript_tables_pyexport_428.xlsx", "results/verify_stats_428.json",
                   "results/img/pyexport_428/thematic_map_DE_.png")),
  list(id = "04", script = "r_bridge/04_RQ1_performance.R", bib = TRUE,
       inputs  = list("db/final_corpus_normalized_428.csv", "results/excluded_terms.txt",
                      "results/affiliation_thesaurus.txt"),
       outputs = c("results/RQ1_tables.xlsx")),
  list(id = "06", script = "r_bridge/06_map_sensitivity.R", bib = TRUE,
       inputs  = list("db/final_corpus_normalized_428.csv", "results/revision/retrieval_flags_428.csv"),
       outputs = c("results/revision/map_sensitivity.csv", "results/revision/mca_inertia.csv")),
  list(id = "vosViewer", script = "r_bridge/vosViewer.R", bib = TRUE,
       inputs  = list("db/final_corpus_normalized_428.csv", "results/author_thesaurus.txt"),
       outputs = c("db/vosviewer_source_428.csv")),
 
  list(id = "09", script = "r_bridge/09_focused46_agreement.R", bib = FALSE, optional = TRUE,
       inputs  = list("db/coding/focused46_coder_A.xlsx",
                      "results/revision/focused46_key.csv", "results/revision/retrieval_flags_428.csv"),
       outputs = c("results/revision/focused46_agreement.csv")))
ids <- vapply(steps, `[[`, "", "id")
if (length(ONLY)) {
  bad <- setdiff(ONLY, ids)
  if (length(bad)) stop("Unknown step(s): ", paste(bad, collapse = ", "), ". Steps: ", paste(ids, collapse = ", "))
  steps <- steps[ids %in% ONLY]
}

# ---------------------------------------------------------------------
# Preflight
# ---------------------------------------------------------------------
say("== Preflight (%s) ==", format(Sys.time(), "%Y-%m-%d %H:%M"))
say("R %s; working directory %s", getRversion(), getwd())
if (!dir.exists("r_bridge")) stop("Run this from the repo root (the folder that contains r_bridge/).")
if (!isTRUE(l10n_info()$`UTF-8`))
  warning("The R session is not in a UTF-8 locale. 02 contains a UTF-8 pattern; run R in a UTF-8 locale ",
          "(the default on macOS, and on Windows from R 4.2).", immediate. = TRUE)
need <- c("dplyr", "readr", "stringr", "tidyr", "readxl", "openxlsx", "jsonlite", "ggplot2")
miss <- need[!vapply(need, has_pkg, logical(1))]
if (length(miss)) say("MISSING packages: %s  -> install.packages(c(%s))", paste(miss, collapse = ", "),
                      paste0('"', miss, '"', collapse = ", "))
BIB_OK <- has_pkg("bibliometrix")
if (BIB_OK) {
  v <- as.character(packageVersion("bibliometrix"))
  say("bibliometrix %s%s", v, if (v != "5.4.1") "  (the paper used 5.4.1; map numbers may differ slightly)" else "")
} else say("bibliometrix not installed: run_analysis, 04, 06 and vosViewer will be skipped.")
md5_note <- function(path, known, label) {
  if (is.na(path) || !file.exists(path)) return(invisible())
  m <- unname(tools::md5sum(path))
  say("%-11s %s  %s", label, if (identical(m, known)) "same file as the 8 Oct audit" else "DIFFERENT from the 8 Oct audit file", path)
}
md5_note(first_existing(RAW_EXPORT), KNOWN_MD5[["raw_export"]], "raw export")
md5_note("db/coding/validation_coder_A.xlsx", KNOWN_MD5[["coder_A"]], "coder A")
md5_note("db/coding/validation_coder_B.xlsx", KNOWN_MD5[["coder_B"]], "coder B")
for (s in steps) {
  miss_in <- vapply(s$inputs, function(p) if (is.na(first_existing(p))) p[length(p)] else NA_character_, "")
  miss_in <- miss_in[!is.na(miss_in)]
  # outputs of earlier steps count as available when those steps are in this run
  made_before <- unlist(lapply(steps[seq_len(which(vapply(steps, `[[`, "", "id") == s$id) - 1)], `[[`, "outputs"))
  miss_in <- setdiff(miss_in, made_before)
  if (length(miss_in)) say("step %-12s missing input(s): %s", s$id, paste(miss_in, collapse = "; "))
}
if (file.exists("results/revision/validation_sample_key.csv")) {
  bk <- file.path("results/revision", sprintf("validation_sample_key_backup_%s.csv", format(Sys.Date(), "%Y%m%d")))
  if (!file.exists(bk)) file.copy("results/revision/validation_sample_key.csv", bk)
  say("Frozen validation key backed up to %s", bk)
}
if (CHECK_ONLY) { say("Preflight only (--check): nothing was run."); quit(save = "no", status = 0) }

# ---------------------------------------------------------------------
# Run
# ---------------------------------------------------------------------
status <- character(0)
for (s in steps) {
  if (s$bib && !BIB_OK) { status[s$id] <- "skipped (no bibliometrix)"; say("\n== %s: skipped (bibliometrix not installed)", s$id); next }
  if (isTRUE(s$optional) && any(vapply(s$inputs, function(p) is.na(first_existing(p)), logical(1)))) {
    status[s$id] <- "skipped (inputs not ready)"; say("\n== %s: skipped (its input files are not in place yet)", s$id); next }
  log <- file.path(LOG_DIR, paste0(s$id, ".log"))
  say("\n== %s: %s  (log: %s)", s$id, s$script, log)
  t0 <- Sys.time()
  rc <- system2(RSCRIPT, shQuote(s$script), stdout = log, stderr = log)
  out <- readLines(log, warn = FALSE)
  cat(paste0("   ", tail(out, 12)), sep = "\n")
  missing_out <- s$outputs[!file.exists(s$outputs)]
  if (!identical(rc, 0L) || length(missing_out)) {
    status[s$id] <- "FAILED"
    say("\n!! Step %s failed (exit code %s)%s. Read %s, fix, then rerun from this step:",
        s$id, rc, if (length(missing_out)) paste0("; missing output(s): ", paste(missing_out, collapse = ", ")) else "", log)
    say("   Rscript r_bridge/00_run_pipeline.R %s", paste(vapply(steps[which(vapply(steps, `[[`, "", "id") == s$id):length(steps)], `[[`, "", "id"), collapse = " "))
    break
  }
  status[s$id] <- sprintf("ok (%.0f s)", as.numeric(difftime(Sys.time(), t0, units = "secs")))
}

# ---------------------------------------------------------------------
# Summary of the checks that matter for the revision
# ---------------------------------------------------------------------
say("\n== Summary ==")
for (k in names(status)) say("  %-12s %s", k, status[[k]])
rc_file <- "results/revision/reproduction_check.csv"
if (file.exists(rc_file)) {
  rc <- read.csv(rc_file)
  d <- rc[rc$status != "MATCH", c("item", "target", "value")]
  say("\n05 reproduction check: %d of %d printed numbers match.", sum(rc$status == "MATCH"), nrow(rc))
  if (nrow(d)) print(d, row.names = FALSE)
}
if (file.exists("results/revision/validation_sample_key_REDRAWN_DIFFERS.csv"))
  say("\nNOTE: 05 re-drew a different validation sample; the frozen key was kept (see the 05 log).")
tv_file <- "results/revision/validation_text_values.csv"
if (file.exists(tv_file)) {
  tv <- read.csv(tv_file)
  if ("check" %in% names(tv)) {
    say("\n07 values vs the 7 Oct reconciliation workbook: %d MATCH, %d DIFF.", sum(tv$check == "MATCH"), sum(tv$check == "DIFF"))
    if (any(tv$check == "DIFF")) print(tv[tv$check == "DIFF", c("item", "value", "expected_7oct")], row.names = FALSE)
  }
}
say("\nSend back: results/revision/*.csv and results/revision/logs/*.log (no db/ files).")

# =====================================================================
# 09b — FOCUSED SAMPLE (46 articles): 
# =====================================================================
suppressPackageStartupMessages({
  library(dplyr); library(readr); library(readxl); library(tidyr); library(stringr); library(tibble)
})
OUT <- "results/revision"; DBC <- "db/coding"
first_existing <- function(p) { p <- p[file.exists(p)]; if (length(p)) p[1] else NA_character_ }

P0A <- file.path(DBC, "focused46_coder_A.xlsx")
P0B <- file.path(DBC, "focused46_coder_B.xlsx")
P1A <- first_existing(file.path(DBC, "round1", c("focused46_coder_A_checked.xlsx", "focused46_coder_101126_G_checked.xlsx")))
P1B <- first_existing(file.path(DBC, "round1", c("focused46_coder_B_checked.xlsx", "focused46_coder_101126_C_checked.xlsx")))
WS  <- file.path(DBC, "focused46_adjudication_worksheet_v3.csv")
KEY <- file.path(OUT, "focused46_key.csv")
for (p in c(P0A, P0B, P1A, P1B, WS, KEY))
  if (is.na(p) || !file.exists(p)) stop("Missing input: ", p, ". See the header of this script.")

CODES <- c("object_side", "hybrid_lean", "eligibility", "season_unit", "ritual_site", "sites_named",
           "optimization", "footprint", "intervention_evaluated")
MAIN  <- c("object_side", "eligibility", "season_unit", "ritual_site", "optimization", "footprint", "intervention_evaluated")
YN <- c("YES", "NO")
ALLOWED <- list(object_side = c("EVENT", "CITY", "HYBRID"), hybrid_lean = c("LEAN_EVENT", "LEAN_CITY", "NA"),
                eligibility = c("ELIGIBLE", "BORDERLINE", "FALSE_POSITIVE"), season_unit = YN, ritual_site = YN,
                optimization = YN, footprint = YN, intervention_evaluated = YN)
SITES <- c("GRAND_MOSQUE", "MINA", "ARAFAT", "MUZDALIFAH")
ALIAS <- c(object = "object_side", main_object = "object_side", lean = "hybrid_lean", season = "season_unit",
           ritual = "ritual_site", sites = "sites_named", intervention = "intervention_evaluated",
           opt = "optimization", elig = "eligibility")

key <- read_csv(KEY, col_types = cols(.default = "c"))
stopifnot(nrow(key) == 46, setequal(key$ID, paste0("S", 1:46)))

norm_sites <- function(x) {
  x <- toupper(str_squish(x)); x[is.na(x) | x == ""] <- NA_character_
  vapply(x, function(s) {
    if (is.na(s) || s == "NA") return(s)
    parts <- str_trim(unlist(strsplit(gsub("[,/]", ";", s), ";")))
    paste(sort(unique(parts[parts != ""])), collapse = "; ")
  }, character(1), USE.NAMES = FALSE)
}
norm_code <- function(v, x) if (v == "sites_named") norm_sites(x) else toupper(str_squish(x))

# Codes only: the evidence, confidence and author_check columns are not read here
# (round 1 wrote CHANGE for CHANGED in author_check; that column plays no part in the codes).
read_codes <- function(path, who) {
  d <- read_excel(path, sheet = "Coding", col_types = "text")
  miss <- setdiff(c("ID", "EID", CODES), names(d))
  if (length(miss)) stop(who, ": missing column(s) ", paste(miss, collapse = ", "))
  d <- d |> filter(str_detect(coalesce(ID, ""), "^S[0-9]+$"), ID != "S0") |> select(ID, EID, all_of(CODES))
  for (v in CODES) d[[v]] <- norm_code(v, d[[v]])
  if (anyDuplicated(d$ID) || !setequal(d$ID, key$ID)) stop(who, ": the sheet must hold S1 to S46 exactly once.")
  chk <- inner_join(d |> select(ID, EID), key |> select(ID, EID_key = EID), by = "ID")
  if (any(chk$EID != chk$EID_key)) stop(who, ": EID changed for ", paste(chk$ID[chk$EID != chk$EID_key], collapse = ", "))
  d |> select(-EID) |> pivot_longer(all_of(CODES), names_to = "variable", values_to = "value")
}
r0 <- read_codes(P0A, "Round 0, coder A") |> rename(A0 = value) |>
  inner_join(read_codes(P0B, "Round 0, coder B") |> rename(B0 = value), by = c("ID", "variable")) |>
  inner_join(read_codes(P1A, "Round 1, coder A") |> rename(A1 = value), by = c("ID", "variable")) |>
  inner_join(read_codes(P1B, "Round 1, coder B") |> rename(B1 = value), by = c("ID", "variable"))
stopifnot(nrow(r0) == 46 * length(CODES))

# A disagreement, counted as in step 09: hybrid_lean only when one side says HYBRID,
# sites_named only when one side says ritual_site YES.
wide <- function(d, a, b) d |> select(ID, variable, all_of(c(a, b))) |>
  pivot_wider(names_from = variable, values_from = all_of(c(a, b)))
is_dispute <- function(d, a, b) {
  w <- wide(d, a, b)
  out <- list()
  for (v in CODES) {
    x <- w[[paste0(a, "_", v)]]; y <- w[[paste0(b, "_", v)]]
    dis <- coalesce(x, "") != coalesce(y, "")
    is_v <- function(col, val) coalesce(w[[col]] == val, FALSE)
    if (v == "hybrid_lean") dis <- dis & (is_v(paste0(a, "_object_side"), "HYBRID") | is_v(paste0(b, "_object_side"), "HYBRID"))
    if (v == "sites_named") dis <- dis & (is_v(paste0(a, "_ritual_site"), "YES") | is_v(paste0(b, "_ritual_site"), "YES"))
    out[[v]] <- tibble(ID = w$ID, variable = v, dispute = dis)
  }
  bind_rows(out)
}
r0 <- r0 |> left_join(is_dispute(r0, "A0", "B0") |> rename(dispute0 = dispute), by = c("ID", "variable")) |>
  left_join(is_dispute(r0, "A1", "B1") |> rename(dispute1 = dispute), by = c("ID", "variable"))

# ---------------------------------------------------------------------
# Round-2 worksheet
# ---------------------------------------------------------------------
ws <- read_csv(WS, col_types = cols(.default = "c"), na = character())   # "NA" is a code (not applicable), not a missing value
names(ws) <- tolower(str_trim(names(ws)))
fcol <- intersect(c("final_code", "final"), names(ws))
if (!all(c("id", "variable") %in% names(ws)) || !length(fcol))
  stop("The worksheet needs the columns ID, variable and final_code (or final).")
ws <- ws |> transmute(ID = str_trim(id), variable = str_trim(tolower(variable)), final_ws = .data[[fcol[1]]],
                      reason = if ("reason" %in% names(ws)) reason else NA_character_,
                      page = if ("pdf_page_checked" %in% names(ws)) pdf_page_checked else NA_character_,
                      decided_by = if ("decided_by" %in% names(ws)) decided_by else NA_character_)
ws$variable <- ifelse(ws$variable %in% names(ALIAS), ALIAS[ws$variable], ws$variable)
bad_var <- setdiff(unique(ws$variable), CODES)
if (length(bad_var)) stop("Unknown variable name(s) in the worksheet: ", paste(bad_var, collapse = ", "),
                          ". Use: ", paste(CODES, collapse = ", "))
bad_id <- setdiff(unique(ws$ID), key$ID)
if (length(bad_id)) stop("Unknown article ID(s) in the worksheet: ", paste(bad_id, collapse = ", "))
dup <- ws |> count(ID, variable) |> filter(n > 1)
if (nrow(dup)) stop("The worksheet has more than one row for: ",
                    paste(paste(dup$ID, dup$variable), collapse = "; "), ". Keep one final row per field.")
ws$final_ws <- mapply(norm_code, ws$variable, ws$final_ws, USE.NAMES = FALSE)
empty <- ws |> filter(is.na(final_ws) | final_ws == "")
if (nrow(empty)) stop(nrow(empty), " worksheet row(s) have no final code: ",
                      paste(paste(empty$ID, empty$variable), collapse = "; "))

# ---------------------------------------------------------------------
# Final codes
# ---------------------------------------------------------------------
same <- function(x, y) coalesce(x, "") == coalesce(y, "")
f <- r0 |> left_join(ws, by = c("ID", "variable")) |>
  mutate(final = case_when(!is.na(final_ws) ~ final_ws,
                           !dispute1 ~ A1,
                           TRUE ~ NA_character_),
         source = case_when(!is.na(final_ws) ~ "round 2 (both authors together)",
                            !dispute0 & same(A1, A0) & same(B1, B0) ~ "round 0 (agreed)",
                            TRUE ~ "round 1 (separate re-checks)"))
# lean and site list only where they apply, as in step 09
fw <- f |> select(ID, variable, final) |> pivot_wider(names_from = variable, values_from = final)
fw <- fw |> mutate(hybrid_lean = if_else(coalesce(object_side == "HYBRID", FALSE), hybrid_lean, "NA"),
                   sites_named = if_else(coalesce(ritual_site == "YES", FALSE), sites_named, "NA"))
open <- fw |> pivot_longer(all_of(CODES), names_to = "variable", values_to = "final") |> filter(is.na(final)) |>
  left_join(f |> select(ID, variable, A1, B1), by = c("ID", "variable"))
if (nrow(open)) {
  print(as.data.frame(open |> select(ID, variable, A1, B1)), row.names = FALSE)
  stop(nrow(open), " field(s) have no final code (listed above): still disputed after round 1, or empty in both ",
       "round-1 files. Add a row with its final code to the worksheet.")
}
problems <- character(0)
for (v in names(ALLOWED)) {
  bad <- which(is.na(fw[[v]]) | !(fw[[v]] %in% ALLOWED[[v]]))
  for (i in bad) problems <- c(problems, sprintf("%s %s = %s (not an allowed value)", fw$ID[i], v, fw[[v]][i]))
}
for (i in seq_len(nrow(fw))) {
  if (fw$object_side[i] == "HYBRID" && !(fw$hybrid_lean[i] %in% c("LEAN_EVENT", "LEAN_CITY")))
    problems <- c(problems, sprintf("%s: HYBRID needs a lean (LEAN_EVENT or LEAN_CITY)", fw$ID[i]))
  if (fw$ritual_site[i] == "YES") {
    parts <- if (is.na(fw$sites_named[i])) character(0) else str_trim(strsplit(fw$sites_named[i], ";")[[1]])
    if (!length(parts) || identical(fw$sites_named[i], "NA") || any(!parts %in% SITES))
      problems <- c(problems, sprintf("%s: ritual_site YES needs sites from GRAND_MOSQUE; MINA; ARAFAT; MUZDALIFAH", fw$ID[i]))
  }
}
if (length(problems)) { cat(problems, sep = "\n"); stop(length(problems), " final-code problem(s); fix the worksheet.") }

f <- f |> select(-final) |> left_join(fw |> pivot_longer(all_of(CODES), names_to = "variable", values_to = "final"),
                                      by = c("ID", "variable"))
note <- function(r, p) {
  out <- ifelse(is.na(r) | r == "", "", r)
  ifelse(is.na(p) | p == "", out, ifelse(out == "", paste0("p. ", p), paste0(out, "; p. ", p)))
}
adj <- f |> mutate(n = as.integer(sub("S", "", ID)), vi = match(variable, CODES)) |> arrange(n, vi) |>
  transmute(ID, variable, A = A0, B = B0, final,
            decided_by = case_when(!is.na(decided_by) & decided_by != "" ~ decided_by,
                                   source == "round 2 (both authors together)" ~ "U.S.S. and F.M. (round 2)",
                                   source == "round 1 (separate re-checks)" ~ "U.S.S. and F.M. (round 1, separate re-checks)",
                                   TRUE ~ "agreed in round 0"),
            note = if_else(source == "round 2 (both authors together)", note(reason, page), ""))
write_csv(adj, file.path(DBC, "focused46_adjudication.csv"), na = "")

# ---------------------------------------------------------------------
# Log (codes only) and counts for Section 2.8
# ---------------------------------------------------------------------
log <- f |> filter(!same(final, A0) | !same(final, B0) | !same(A1, A0) | !same(B1, B0)) |>
  mutate(n = as.integer(sub("S", "", ID)), vi = match(variable, CODES)) |> arrange(n, vi) |>
  transmute(ID, variable, round0_A = A0, round0_B = B0, round1_A = A1, round1_B = B1, final, decided_in = source)
dir.create(OUT, showWarnings = FALSE, recursive = TRUE)
write_csv(log, file.path(OUT, "focused46_reconciliation_log.csv"), na = "")

core <- f$variable %in% MAIN
cnt <- function(x) sum(x, na.rm = TRUE)
counts <- tribble(
  ~quantity, ~all_9_fields, ~seven_core_variables,
  "round 0: disputed fields", cnt(f$dispute0), cnt(f$dispute0 & core),
  "round 0: articles with a disputed field", n_distinct(f$ID[f$dispute0]), n_distinct(f$ID[f$dispute0 & core]),
  "round 1: fields changed by coder A", cnt(!same(f$A1, f$A0)), cnt(!same(f$A1, f$A0) & core),
  "round 1: fields changed by coder B", cnt(!same(f$B1, f$B0)), cnt(!same(f$B1, f$B0) & core),
  "round 1: round-0 agreed fields changed", cnt(!f$dispute0 & (!same(f$A1, f$A0) | !same(f$B1, f$B0))), cnt(!f$dispute0 & core & (!same(f$A1, f$A0) | !same(f$B1, f$B0))),
  "round 1: disputed fields left", cnt(f$dispute1), cnt(f$dispute1 & core),
  "round 1: articles with a disputed field left", n_distinct(f$ID[f$dispute1]), n_distinct(f$ID[f$dispute1 & core]),
  "round 2: worksheet rows", nrow(ws), cnt(ws$variable %in% MAIN),
  "round 2: worksheet rows on fields agreed after round 1 (sweep)", cnt(!is.na(f$final_ws) & !f$dispute1), cnt(!is.na(f$final_ws) & !f$dispute1 & core),
  "final codes that differ from coder A's round-0 code", cnt(!same(f$final, f$A0)), cnt(!same(f$final, f$A0) & core),
  "final codes that differ from coder B's round-0 code", cnt(!same(f$final, f$B0)), cnt(!same(f$final, f$B0) & core),
  "final codes that differ from a round-0 agreed code", cnt(!f$dispute0 & !same(f$final, f$A0)), cnt(!f$dispute0 & core & !same(f$final, f$A0)))
write_csv(counts, file.path(OUT, "focused46_reconciliation_counts.csv"), na = "")
cat("[09b] Final codes for all 46 articles written to db/coding/focused46_adjudication.csv (", nrow(adj), " rows).\n", sep = "")
print(as.data.frame(counts), row.names = FALSE)
cat("[09b] Now run step 09: Rscript r_bridge/00_run_pipeline.R 09\n")

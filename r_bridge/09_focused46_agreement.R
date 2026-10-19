# =====================================================================
# 09 — FOCUSED SAMPLE (46 articles)
# =====================================================================
suppressPackageStartupMessages({
  library(dplyr); library(readr); library(readxl); library(tidyr); library(stringr); library(tibble)
})
OUT <- "results/revision"; DBC <- "db/coding"
dir.create(OUT, showWarnings = FALSE, recursive = TRUE)
first_existing <- function(p) { p <- p[file.exists(p)]; if (length(p)) p[1] else NA_character_ }

PATH_A <- first_existing(file.path(DBC, "focused46_coder_A.xlsx"))
PATH_B <- first_existing(file.path(DBC, "focused46_coder_B.xlsx"))
KEY    <- file.path(OUT, "focused46_key.csv")
FLAGS  <- file.path(OUT, "retrieval_flags_428.csv")
VALREC <- file.path(OUT, "validation_records.csv")
ADJ    <- file.path(DBC, "focused46_adjudication.csv")
JULY   <- file.path(DBC, "rq4_coding_46_FINAL.csv")
SUBSET <- file.path(OUT, "focused46_subset20.csv")
for (p in c(PATH_A, KEY, FLAGS))
  if (is.na(p) || !file.exists(p)) stop("Missing input: ", p, ". See the header of this script.")

CODES <- c("object_side", "hybrid_lean", "eligibility", "season_unit", "ritual_site", "sites_named",
           "optimization", "footprint", "intervention_evaluated")
EVID  <- c(object_side = "object_evidence", hybrid_lean = "object_evidence", eligibility = "eligibility_evidence",
           season_unit = "season_evidence", ritual_site = "ritual_site_evidence", sites_named = "ritual_site_evidence",
           optimization = "optimization_evidence", footprint = "footprint_evidence",
           intervention_evaluated = "intervention_evidence")
YN <- c("YES", "NO")
ALLOWED <- list(object_side = c("EVENT", "CITY", "HYBRID"), hybrid_lean = c("LEAN_EVENT", "LEAN_CITY", "NA"),
                eligibility = c("ELIGIBLE", "BORDERLINE", "FALSE_POSITIVE"), season_unit = YN, ritual_site = YN,
                optimization = YN, footprint = YN, intervention_evaluated = YN)
SITES <- c("GRAND_MOSQUE", "MINA", "ARAFAT", "MUZDALIFAH")

key <- read_csv(KEY, col_types = cols(.default = "c"))
stopifnot(nrow(key) == 46, setequal(key$ID, paste0("S", 1:46)))
flags <- read_csv(FLAGS, col_types = cols(.default = "c")) |> select(EID, rule_label = SUBGROUP)
keyl <- key |> left_join(flags, by = "EID") |> mutate(n = as.integer(sub("S", "", ID))) |> arrange(n)
stopifnot(!any(is.na(keyl$rule_label)))

# The fallback subset must be the seeded draw (made on 11 Oct 2026, before any new coding).
sub_ids <- NULL
if (file.exists(SUBSET)) {
  sub <- read_csv(SUBSET, col_types = cols(.default = "c"))
  set.seed(2026)
  redraw <- c(sample(keyl$ID[keyl$rule_label == "EVENT"], 12), sample(keyl$ID[keyl$rule_label == "CITY"], 8))
  if (!setequal(redraw, sub$ID)) stop("focused46_subset20.csv does not match its seeded draw (set.seed(2026)). Do not edit it.")
  sub_ids <- sub$ID
}

# ---------------------------------------------------------------------
# 1. Read and check both codings
# ---------------------------------------------------------------------
norm_sites <- function(x) {
  x <- toupper(str_squish(x)); x[is.na(x) | x == ""] <- NA_character_
  vapply(x, function(s) {
    if (is.na(s) || s == "NA") return(s)
    parts <- str_trim(unlist(strsplit(gsub("[,/]", ";", s), ";")))
    paste(sort(unique(parts[parts != ""])), collapse = "; ")
  }, character(1), USE.NAMES = FALSE)
}
MAIN <- c("object_side", "eligibility", "season_unit", "ritual_site", "optimization", "footprint", "intervention_evaluated")
read_coder <- function(path, who, partial = FALSE) {
  d <- read_excel(path, sheet = "Coding", col_types = "text")
  need <- c("ID", "EID", CODES, unique(EVID), "study_area", "author_check", "author_note")
  miss <- setdiff(need, names(d))
  if (length(miss)) stop(who, ": missing column(s) ", paste(miss, collapse = ", "), ". Do not rename columns.")
  d <- d |> filter(str_detect(coalesce(ID, ""), "^S[0-9]+$"), ID != "S0") |> select(all_of(need))
  for (v in setdiff(CODES, "sites_named")) d[[v]] <- toupper(str_squish(d[[v]]))
  d$sites_named <- norm_sites(d$sites_named)
  d$author_check <- toupper(str_squish(d$author_check))
  if (anyDuplicated(d$ID) || !all(d$ID %in% key$ID)) stop(who, ": unknown or repeated article IDs.")
  if (partial) {
    coded <- rowSums(!is.na(as.matrix(d[, MAIN]))) > 0      # rows left empty were not coded
    d <- d[coded, ]
  } else if (!setequal(d$ID, key$ID)) stop(who, ": the sheet must hold S1 to S46 exactly once.")
  chk <- inner_join(d |> select(ID, EID), key |> select(ID, EID_key = EID), by = "ID")
  if (any(chk$EID != chk$EID_key)) stop(who, ": EID changed for ", paste(chk$ID[chk$EID != chk$EID_key], collapse = ", "))
  d
}
A <- read_coder(PATH_A, "Coder A")
MODE <- "single"; B <- NULL
if (!is.na(PATH_B)) {
  B <- read_coder(PATH_B, "Coder B", partial = TRUE)
  if (setequal(B$ID, key$ID)) {
    MODE <- "full"
  } else if (!is.null(sub_ids) && all(sub_ids %in% B$ID)) {
    MODE <- "subset"
  } else {
    stop("Coder B covers ", nrow(B), " articles: neither all 46 nor all 20 of the drawn subset.",
         if (!is.null(sub_ids)) paste0(" Subset articles not yet coded: ", paste(setdiff(sub_ids, B$ID), collapse = ", ")) else "")
  }
}

problems <- list()
add_problem <- function(who, id, variable, value, issue)
  problems[[length(problems) + 1]] <<- tibble(coder = who, ID = id, variable = variable, value = value, issue = issue)
check_coder <- function(d, who) {
  for (v in names(ALLOWED)) {
    bad <- which(is.na(d[[v]]) | !(d[[v]] %in% ALLOWED[[v]]))
    for (i in bad) add_problem(who, d$ID[i], v, d[[v]][i], "empty or not an allowed value")
  }
  for (i in seq_len(nrow(d))) {
    os <- d$object_side[i]; hl <- d$hybrid_lean[i]; rs <- d$ritual_site[i]; sn <- d$sites_named[i]
    if (identical(os, "HYBRID") && !(hl %in% c("LEAN_EVENT", "LEAN_CITY"))) add_problem(who, d$ID[i], "hybrid_lean", hl, "HYBRID needs a lean")
    if (os %in% c("EVENT", "CITY") && !identical(hl, "NA")) add_problem(who, d$ID[i], "hybrid_lean", hl, "must be NA unless HYBRID")
    if (identical(rs, "YES")) {
      parts <- if (is.na(sn)) character(0) else str_trim(strsplit(sn, ";")[[1]])
      if (!length(parts) || identical(sn, "NA") || any(!parts %in% SITES)) add_problem(who, d$ID[i], "sites_named", sn, "list sites from GRAND_MOSQUE; MINA; ARAFAT; MUZDALIFAH")
    }
    if (identical(rs, "NO") && !identical(sn, "NA")) add_problem(who, d$ID[i], "sites_named", sn, "must be NA when ritual_site is NO")
    if (!(d$author_check[i] %in% c("AGREE", "CHANGED"))) add_problem(who, d$ID[i], "author_check", d$author_check[i], "AGREE or CHANGED")
  }
}
check_coder(A, "A"); if (!is.null(B)) check_coder(B, "B")
if (length(problems)) {
  pr <- bind_rows(problems); write_csv(pr, file.path(DBC, "focused46_coding_problems.csv"), na = "")
  print(as.data.frame(pr), row.names = FALSE)
  stop(nrow(pr), " coding problem(s); see db/coding/focused46_coding_problems.csv. Fix the workbooks and rerun.")
}
cat(sprintf("[09] Mode: %s. Coder A: %d articles; Coder B: %d. All values valid.\n", MODE, nrow(A), if (is.null(B)) 0L else nrow(B)))

# ---------------------------------------------------------------------
# 2. Agreement (Cohen's kappa with the Fleiss-Cohen-Everitt 1969 SE, as in 07)
# ---------------------------------------------------------------------
kappa <- function(a, b, levels) {
  a <- factor(a, levels); b <- factor(b, levels)
  p <- table(a, b) / length(a); po <- sum(diag(p)); pe <- sum(rowSums(p) * colSums(p))
  if (isTRUE(all.equal(pe, 1))) return(NA_real_)
  (po - pe) / (1 - pe)
}
kappa_se_fce <- function(a, b, levels) {
  a <- factor(a, levels); b <- factor(b, levels)
  p <- table(a, b) / length(a); r <- rowSums(p); cl <- colSums(p)
  po <- sum(diag(p)); pe <- sum(r * cl); if (isTRUE(all.equal(pe, 1))) return(NA_real_)
  k <- (po - pe) / (1 - pe)
  Aterm <- sum(diag(p) * (1 - (r + cl) * (1 - k))^2)
  off <- outer(cl, r, "+")
  Bterm <- (1 - k)^2 * sum((p * off^2)[row(p) != col(p)])
  Cterm <- (k - pe * (1 - k))^2
  sqrt(max(Aterm + Bterm - Cterm, 0) / length(a)) / (1 - pe)
}
by_lean <- function(side, lean) ifelse(side == "HYBRID", ifelse(lean == "LEAN_EVENT", "EVENT", "CITY"), side)
env_yn  <- function(e) ifelse(e == "FALSE_POSITIVE", "NO", "YES")
agree_row <- function(variable, a, b, levels, binary = FALSE) {
  k <- kappa(a, b, levels); se <- kappa_se_fce(a, b, levels); po <- mean(a == b)
  tibble(variable, levels = paste(levels, collapse = "/"), n = length(a), agreements = sum(a == b),
         observed_agreement = round(po, 3), kappa = round(k, 3),
         ci_low_fce = round(pmax(-1, k - 1.96 * se), 3), ci_high_fce = round(pmin(1, k + 1.96 * se), 3),
         pabak = if (binary) round(2 * po - 1, 3) else NA_real_,
         yes_A = if (binary) sum(a == "YES") else NA_integer_, yes_B = if (binary) sum(b == "YES") else NA_integer_)
}
agree_all <- function(d, set_label) bind_rows(
  agree_row("Main object, 3 labels (EVENT/CITY/HYBRID)", d$object_side_A, d$object_side_B, ALLOWED$object_side),
  agree_row("Main object, hybrids by lean (EVENT/CITY)",
            by_lean(d$object_side_A, d$hybrid_lean_A), by_lean(d$object_side_B, d$hybrid_lean_B), c("EVENT", "CITY")),
  agree_row("Environmental eligibility, 3 levels", d$eligibility_A, d$eligibility_B, ALLOWED$eligibility),
  agree_row("Environmental analysis (ELIGIBLE or BORDERLINE vs FALSE_POSITIVE)",
            env_yn(d$eligibility_A), env_yn(d$eligibility_B), YN, binary = TRUE),
  agree_row("Pilgrimage period as time unit", d$season_unit_A, d$season_unit_B, YN, binary = TRUE),
  agree_row("Ritual site as study site", d$ritual_site_A, d$ritual_site_B, YN, binary = TRUE),
  agree_row("Formal optimization method", d$optimization_A, d$optimization_B, YN, binary = TRUE),
  agree_row("Footprint quantified", d$footprint_A, d$footprint_B, YN, binary = TRUE),
  agree_row("Implemented measure evaluated with observed data", d$intervention_evaluated_A, d$intervention_evaluated_B, YN, binary = TRUE)) |>
  mutate(set = set_label, .before = 1)
if (MODE == "single") {
  AB <- NULL
  agreement <- tibble(set = "none", variable = "(no second coding: single-coding mode; final codes are Coder A's)")
  cat("\n[09] Single-coding mode: no Coder B workbook, so no agreement statistics.\n")
} else {
  AB <- inner_join(A, B, by = c("ID", "EID"), suffix = c("_A", "_B"))   # every article coded by both
  primary <- if (MODE == "subset") sub_ids else key$ID
  agreement <- agree_all(AB |> filter(ID %in% primary),
                         if (MODE == "subset") "random subset of 20 (seed 2026)" else "all 46 articles")
  extra <- setdiff(AB$ID, primary)
  if (length(extra))
    agreement <- bind_rows(agreement, agree_all(AB, sprintf("all %d articles coded by both (subset plus %d)", nrow(AB), length(extra))))
  cat(sprintf("\n[09] Agreement between the two codings (%s):\n", agreement$set[1]))
  print(as.data.frame(agreement |> filter(set == set[1]) |> select(variable, n, agreements, observed_agreement, kappa, ci_low_fce, ci_high_fce, pabak)), row.names = FALSE)
}
write_csv(agreement, file.path(OUT, "focused46_agreement.csv"), na = "")

# ---------------------------------------------------------------------
# 3. Disagreements (internal file: holds short quotes from the articles)
# ---------------------------------------------------------------------
dis <- if (is.null(AB)) tibble(ID = character(), variable = character()) else bind_rows(lapply(CODES, function(v) {
  a <- AB[[paste0(v, "_A")]]; b <- AB[[paste0(v, "_B")]]
  idx <- which(coalesce(a, "") != coalesce(b, ""))
  if (v == "hybrid_lean") idx <- idx[AB$object_side_A[idx] == "HYBRID" | AB$object_side_B[idx] == "HYBRID"]
  if (v == "sites_named") idx <- idx[AB$ritual_site_A[idx] == "YES" | AB$ritual_site_B[idx] == "YES"]
  if (!length(idx)) return(NULL)
  tibble(ID = AB$ID[idx], variable = v, A = a[idx], B = b[idx],
         A_evidence = AB[[paste0(EVID[[v]], "_A")]][idx], B_evidence = AB[[paste0(EVID[[v]], "_B")]][idx],
         A_note = AB$author_note_A[idx], B_note = AB$author_note_B[idx])
}))
if (nrow(dis)) dis <- dis |> arrange(as.integer(sub("S", "", ID)), match(variable, CODES))
if (!is.null(AB)) {
  write_csv(dis, file.path(DBC, "focused46_disagreements.csv"), na = "")
  cat(sprintf("\n[09] %d disagreement(s) on %d article(s) written to db/coding/focused46_disagreements.csv\n",
              nrow(dis), n_distinct(dis$ID)))
}

if (!file.exists(ADJ)) {
  if (nrow(dis)) {
    write_csv(dis |> select(ID, variable, A, B) |> mutate(final = "", decided_by = "", note = ""),
              file.path(DBC, "focused46_adjudication_TEMPLATE.csv"), na = "")
    cat("[09] No adjudication file yet. Fill db/coding/focused46_adjudication_TEMPLATE.csv (final, decided_by, note),\n",
        "     save it as db/coding/focused46_adjudication.csv and rerun to get the final codes.\n", sep = "")
    quit(save = "no", status = 0)
  }
  adj <- tibble(ID = character(), variable = character(), final = character())
} else {
  adj <- read_csv(ADJ, col_types = cols(.default = "c")) |>
    mutate(variable = str_trim(variable), final = if_else(variable == "sites_named", norm_sites(final), toupper(str_squish(final))))
}

# ---------------------------------------------------------------------
# 4. Final codes: agreed value, or the adjudicated one
# ---------------------------------------------------------------------
final <- A |> select(ID, EID, all_of(CODES))              # Coder A for every article
final$coding <- "single (Coder A)"
if (!is.null(AB)) {
  pos <- match(AB$ID, final$ID)
  for (v in CODES) {
    a <- AB[[paste0(v, "_A")]]; b <- AB[[paste0(v, "_B")]]
    f <- ifelse(coalesce(a, "") == coalesce(b, ""), a, NA_character_)
    ad <- adj |> filter(variable == v)
    hit <- match(AB$ID, ad$ID)
    f[!is.na(hit)] <- ad$final[hit[!is.na(hit)]]
    final[[v]][pos] <- f
  }
  final$coding[pos] <- "double (reconciled)"
}
# a lean or site list is only needed when the final object is HYBRID / the final site code is YES
final <- final |> mutate(hybrid_lean = if_else(object_side == "HYBRID", hybrid_lean, "NA"),
                         sites_named = if_else(ritual_site == "YES", sites_named, "NA"))
valid_final <- function(v, x) {
  if (is.na(x) || x == "") return(FALSE)
  if (v == "sites_named") return(x == "NA" || all(str_trim(strsplit(x, ";")[[1]]) %in% SITES))
  x %in% ALLOWED[[v]]
}
long <- final |> pivot_longer(all_of(CODES), names_to = "variable", values_to = "value")
miss <- long[!mapply(valid_final, long$variable, long$value), ]
if (nrow(miss)) {
  print(as.data.frame(miss), row.names = FALSE)
  stop(nrow(miss), " final code(s) missing or invalid: add them to db/coding/focused46_adjudication.csv.")
}
final <- final |>
  mutate(object_by_lean = by_lean(object_side, hybrid_lean), environmental = env_yn(eligibility)) |>
  left_join(flags, by = "EID") |> left_join(key |> select(ID, route), by = "ID") |>
  arrange(as.integer(sub("S", "", ID)))
stopifnot(!any(is.na(final$rule_label)))
write_csv(final |> select(ID, EID, rule_label, route, coding, object_side, hybrid_lean, object_by_lean, eligibility, environmental,
                          season_unit, ritual_site, sites_named, optimization, footprint, intervention_evaluated),
          file.path(OUT, "focused46_final_codes.csv"), na = "")
cat("[09] Final codes written to results/revision/focused46_final_codes.csv\n")

# ---------------------------------------------------------------------
# 5. Numbers the text cites (sample-level; never corpus rates)
# ---------------------------------------------------------------------
ids <- function(x) paste(final$ID[x], collapse = " ")
tv <- list()
add <- function(item, value, used_in, ids_ = "") tv[[length(tv) + 1]] <<- tibble(item, value = as.character(value), used_in, ids = ids_)
env <- final$environmental == "YES"
add("coding mode", MODE, "M-10, Table S2")
add("articles coded by both coders", sum(final$coding == "double (reconciled)"), "M-10, Table S2", ids(final$coding == "double (reconciled)"))
add("articles by rule label: EVENT / CITY", paste(sum(final$rule_label == "EVENT"), sum(final$rule_label == "CITY"), sep = " / "), "F-2, M-9")
add("main object: EVENT / CITY / HYBRID", paste(sum(final$object_side == "EVENT"), sum(final$object_side == "CITY"), sum(final$object_side == "HYBRID"), sep = " / "), "F-2")
add("main object with hybrids by lean: EVENT / CITY", paste(sum(final$object_by_lean == "EVENT"), sum(final$object_by_lean == "CITY"), sep = " / "), "F-2, D-2")
add("rule EVENT but city-side by object", sum(final$rule_label == "EVENT" & final$object_by_lean == "CITY"), "F-2", ids(final$rule_label == "EVENT" & final$object_by_lean == "CITY"))
add("rule CITY but event-side by object", sum(final$rule_label == "CITY" & final$object_by_lean == "EVENT"), "F-2", ids(final$rule_label == "CITY" & final$object_by_lean == "EVENT"))
add("no environmental analysis (FALSE_POSITIVE), all", sum(!env), "F-1, D-5, C-4", ids(!env))
add("no environmental analysis, rule EVENT (of rule EVENT)", paste0(sum(!env & final$rule_label == "EVENT"), " of ", sum(final$rule_label == "EVENT")), "F-1, C-4")
add("no environmental analysis, rule CITY (of rule CITY)", paste0(sum(!env & final$rule_label == "CITY"), " of ", sum(final$rule_label == "CITY")), "F-1")
add("no environmental analysis, event by object (of event by object)", paste0(sum(!env & final$object_by_lean == "EVENT"), " of ", sum(final$object_by_lean == "EVENT")), "F-1, C-4")
add("no environmental analysis, city by object (of city by object)", paste0(sum(!env & final$object_by_lean == "CITY"), " of ", sum(final$object_by_lean == "CITY")), "F-1, D-5")
add("no environmental analysis, entered through a citation route", sum(!env & str_detect(final$route, "^RULE")), "F-1 note", ids(!env & str_detect(final$route, "^RULE")))
add("BORDERLINE", sum(final$eligibility == "BORDERLINE"), "F-1 note", ids(final$eligibility == "BORDERLINE"))
cityobj <- env & final$object_side %in% c("CITY", "HYBRID")
add("season as time unit, environmental city or hybrid by object (n of m)", paste0(sum(cityobj & final$season_unit == "YES"), " of ", sum(cityobj)), "D-4, A-1, C-4", ids(cityobj & final$season_unit == "YES"))
add("season as time unit, environmental city by object only (n of m)", paste0(sum(env & final$object_side == "CITY" & final$season_unit == "YES"), " of ", sum(env & final$object_side == "CITY")), "D-4")
add("season as time unit, environmental event by object (n of m)", paste0(sum(env & final$object_side == "EVENT" & final$season_unit == "YES"), " of ", sum(env & final$object_side == "EVENT")), "D-4")
add("season as time unit, all (rule EVENT / rule CITY)", paste(sum(final$season_unit == "YES" & final$rule_label == "EVENT"), sum(final$season_unit == "YES" & final$rule_label == "CITY"), sep = " / "), "D-4")
rs <- final$ritual_site == "YES"
add("ritual site as study site", sum(rs), "D-3a, C-4", ids(rs))
add("ritual site as study site, environmental", sum(rs & env), "D-3a", ids(rs & env))
add("ritual site studies set in the pilgrimage period (of environmental ritual-site studies)", paste0(sum(rs & env & final$season_unit == "YES"), " of ", sum(rs & env)), "D-3a")
add("formal optimization, all", sum(final$optimization == "YES"), "D-2 (Gap 1)", ids(final$optimization == "YES"))
add("formal optimization, event by object (of event by object)", paste0(sum(final$optimization == "YES" & final$object_by_lean == "EVENT"), " of ", sum(final$object_by_lean == "EVENT")), "D-2 (Gap 1)", ids(final$optimization == "YES" & final$object_by_lean == "EVENT"))
add("formal optimization, event by rule (of rule EVENT)", paste0(sum(final$optimization == "YES" & final$rule_label == "EVENT"), " of ", sum(final$rule_label == "EVENT")), "D-2 (Gap 1)")
add("footprint quantified: event / city by object", paste(sum(final$footprint == "YES" & final$object_by_lean == "EVENT"), sum(final$footprint == "YES" & final$object_by_lean == "CITY"), sep = " / "), "§4, D-6", ids(final$footprint == "YES"))
add("implemented measure evaluated with observed data", sum(final$intervention_evaluated == "YES"), "F-3, D-5, C-4", ids(final$intervention_evaluated == "YES"))
if (file.exists(JULY)) {
  july <- read_csv(JULY, col_types = cols(.default = "c")) |> select(ID, object_env_system)
  rs_obj <- final |> filter(rs & env) |> left_join(july, by = "ID") |> count(object_env_system)
  add("environmental object of ritual-site studies (July coding)", paste(paste0(rs_obj$object_env_system, " ", rs_obj$n), collapse = "; "), "D-3a, C-4")
}
tv <- bind_rows(tv)
write_csv(tv, file.path(OUT, "focused46_text_values.csv"), na = "")
cat("\n[09] Numbers for the text (sample-level):\n"); print(as.data.frame(tv |> select(item, value, used_in)), row.names = FALSE)

# ---------------------------------------------------------------------
# 6. Articles that are also in the validation sample (independent check)
# ---------------------------------------------------------------------
if (file.exists(VALREC)) {
  vr <- read_csv(VALREC, col_types = cols(.default = "c")) |>
    transmute(EID, CODING_ID, val_object = by_lean(consensus_label, coalesce(consensus_direction, "")),
              val_eligibility = consensus_eligibility, val_environmental = env_yn(consensus_eligibility))
  ov <- final |> inner_join(vr, by = "EID") |>
    transmute(ID, CODING_ID, object_by_lean, val_object, same_object = object_by_lean == val_object,
              eligibility, val_eligibility, same_environmental = environmental == val_environmental)
  write_csv(ov, file.path(OUT, "focused46_vs_validation.csv"), na = "")
  cat(sprintf("\n[09] %d articles are also in the validation sample: object agrees on %d, environmental status on %d.\n",
              nrow(ov), sum(ov$same_object), sum(ov$same_environmental)))
}

# ---------------------------------------------------------------------
# 7. Final codes against the July codes (if the July file is given)
# ---------------------------------------------------------------------
if (file.exists(JULY)) {
  j <- read_csv(JULY, col_types = cols(.default = "c")) |>
    transmute(ID, july_environmental = if_else(tolower(object_env_system) == "none", "NO", "YES"),
              july_season = toupper(season_focus), july_ritual = if_else(tolower(ritual_site) == "none", "NO", "YES"),
              july_optimization = if_else(method == "optimization", "YES", "NO"), july_footprint = toupper(quantifies_footprint))
  cmp <- final |> select(ID, environmental, season_unit, ritual_site, optimization, footprint) |> inner_join(j, by = "ID")
  vs <- tibble(variable = c("environmental analysis", "season", "ritual site", "optimization", "footprint"),
               same = c(sum(cmp$environmental == cmp$july_environmental), sum(cmp$season_unit == cmp$july_season),
                        sum(cmp$ritual_site == cmp$july_ritual), sum(cmp$optimization == cmp$july_optimization),
                        sum(cmp$footprint == cmp$july_footprint)), n = nrow(cmp),
               changed_ids = c(ids(match(cmp$ID[cmp$environmental != cmp$july_environmental], final$ID)),
                               ids(match(cmp$ID[cmp$season_unit != cmp$july_season], final$ID)),
                               ids(match(cmp$ID[cmp$ritual_site != cmp$july_ritual], final$ID)),
                               ids(match(cmp$ID[cmp$optimization != cmp$july_optimization], final$ID)),
                               ids(match(cmp$ID[cmp$footprint != cmp$july_footprint], final$ID))))
  write_csv(vs, file.path(OUT, "focused46_vs_july.csv"), na = "")
  cat("\n[09] Final codes against the July codes:\n"); print(as.data.frame(vs), row.names = FALSE)
}
cat("\n[09] Done.\n")

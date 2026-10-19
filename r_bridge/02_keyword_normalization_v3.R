# =====================================================================
# 02 — KEYWORD NORMALIZATION   (run AFTER 01_screening_pipeline.R)
# Applies to `Author Keywords` (DE) and `Index Keywords` (ID), in the raw
# Scopus-header CSV format -- so the output stays loadable by convert2df()
# just like the input, matching what r_bridge/run_analysis.R expects.
#
# Run from the repo root: Rscript r_bridge/02_keyword_normalization_v3.R
# Reads:  db/final_corpus_v428.csv             (01's pre-normalization output)
#         results/normalize_terms.txt, results/normalize_geographic.txt
# Writes: db/final_corpus_normalized_428.csv   (the frozen analytic corpus;
#         contains Scopus abstracts, so it is not redistributed)
# =====================================================================

library(readr)

IN_CSV  <- "db/final_corpus_v428.csv"
OUT_CSV <- "db/final_corpus_normalized_428.csv"

df <- read_csv(IN_CSV, show_col_types = FALSE)
stopifnot(nrow(df) == 428)

normalize_keywords <- function(x) {
  x <- toupper(replace(x, is.na(x), ""))

  # ---- 1. Strip Scopus HTML sub/superscript tags -------------------
  for (tag in c("<SUB>","</SUB>","<SUP>","</SUP>")) x <- gsub(tag, "", x, fixed = TRUE)

  # ---- 2a. Bracketed geographic index forms -------------------------
  # Scopus geo forms: "MAKKAH [SAUDI ARABIA]"  -> a SPACE precedes "[".
  # Chemical names:   "BENZO[A]PYRENE"          -> NO space precedes "[".
  # The trailing-bracket rule in 2b exploits that difference so PAH names
  # (BENZO[A]PYRENE, DIBENZ[A,H]ANTHRACENE, INDENO[1,2,3 CD]PYRENE ...) survive.
  # Special cases that must run BEFORE the generic strip:
  bracket_map <- c(
    "MAKKAH [SAUDI ARABIA]"        = "MAKKAH",
    "MECCA [SAUDI ARABIA]"         = "MAKKAH",
    "MEDINA [MADINAH]"             = "MADINAH",
    "JIDDAH [MAKKAH (PRV)]"        = "JEDDAH",
    "JEDDAH [SAUDI ARABIA]"        = "JEDDAH",
    "RIYADH [SAUDI ARABIA]"        = "RIYADH",
    "RIYADH [RIYADH (PRV)]"        = "RIYADH",
    "EASTERN PROVINCE [SAUDI ARABIA]" = "EASTERN PROVINCE",
    "TABUK [SAUDI ARABIA]"         = "TABUK",
    "MEDINA [WESTERN AUSTRALIA]"   = "__DROP__"   # namesake: Medina, WA — not the holy city
  )
  for (k in names(bracket_map)) x <- gsub(k, bracket_map[[k]], x, fixed = TRUE)

  # ---- 2b. Generic: strip a TRAILING " [REGION]" only (space-anchored) ----
  #   "ALEXANDRIA [EGYPT]" -> "ALEXANDRIA";  "BENZO[A]PYRENE" -> unchanged
  strip_geo <- function(v) sub("\\s+\\[[^]]+\\]$", "", v)

  # ---- 3. Token-wise synonym merges (never bleed across ";") --------
  merges <- c(
    # Makkah
    "\\bMECCA CITY\\b" = "MAKKAH", "\\bMECCA\\b" = "MAKKAH",
    "MAKKAH (CITY|REGION|PROVINCE)\\b" = "MAKKAH",
    "MAKKAH AL[- ]MUKARRAMAH( REGION)?" = "MAKKAH",
    "MAKKAH[ ,—-]+SAUDI ARABIA" = "MAKKAH", "SAUDI ARABIA, MAKKAH" = "MAKKAH",
    # Madinah
    "AL[- ]MADINAH AL[- ]MUNAWARAH( PROVINCE)?" = "MADINAH",
    "\\bAL[- ]MADINAH\\b" = "MADINAH", "\\bAL MEDINA\\b" = "MADINAH",
    "\\bMADINAH CITY\\b" = "MADINAH", "SAUDI ARABIA, MEDINA" = "MADINAH",
    "\\bMEDINA\\b" = "MADINAH",
    # Hajj / Umrah
    "\\bHAJ\\b" = "HAJJ", "\\bTHE HAJJ\\b" = "HAJJ", "HAJJ PILGRIMAGE" = "HAJJ",
    "HAJJ AND UMRAH" = "HAJJ", "\\bUMRA\\b" = "UMRAH",
    # Saudi
    "KINGDOM OF SAUDI ARABIA" = "SAUDI ARABIA", "\\bKSA\\b" = "SAUDI ARABIA",
    "WESTERN SAUDI ARABIA" = "SAUDI ARABIA",
    "(SAUDI|MAKKAH) VISION 2030" = "VISION 2030",
    # NOTE: the MSW family is handled by EXACT TOKEN MAPPING below, not regex.
    # A generic "SOLID WASTE" -> "MUNICIPAL SOLID WASTE" rule fires on the
    # SOLID WASTE *inside* MUNICIPAL SOLID WASTE and yields the double-word
    # artifact that corrupted v1/v2. Exact mapping cannot bleed.
    # Particulates
    "FINE PARTICULATE MATTER( \\(PM ?2\\.5\\))?" = "PM2.5",
    "PARTICULATE MATTER 2\\.5" = "PM2.5", "PM ?2\\.5" = "PM2.5",
    "PARTICULATE MATTER 10" = "PM10", "PM ?10" = "PM10",
    "PARTICLES \\(PARTICULATE MATTER\\)" = "PARTICULATE MATTER",
    "\\b(SUSPENDED )?PARTICULATES\\b" = "PARTICULATE MATTER",
    # Chemistry
    "CO2 EMISSIONS?" = "CARBON EMISSIONS",
    "CARBON DIOXIDE( EQUIVALENT)? EMISSIONS?" = "CARBON EMISSIONS",
    "\\bO3\\b" = "OZONE", "\\bNOX\\b" = "NITROGEN OXIDES", "\\bNH3\\b" = "AMMONIA",
    # Misc
    "SUSTAINABLE DEVELOPMENT GOALS? \\(SDGS?\\)" = "SUSTAINABLE DEVELOPMENT GOALS",
    "ENVIRONMENTAL SUSTAINABILITY" = "SUSTAINABILITY",
    "WASTE ?WATER( TREATMENT)?" = "WASTEWATER",
    "SOLAR POWER( SYSTEM)?" = "SOLAR ENERGY",
    "SOLAR PHOTOVOLTAIC" = "PHOTOVOLTAIC", "PHOTOVOLTAIC \\(PV\\)" = "PHOTOVOLTAIC",
    "\\bPV\\b" = "PHOTOVOLTAIC",
    "\\bGIS\\b" = "GEOGRAPHIC INFORMATION SYSTEM", "\\bAI\\b" = "ARTIFICIAL INTELLIGENCE",
    "COVID ?19" = "COVID-19", "SARS-COV-2" = "COVID-19"
  )

  # ---- 3b. EXACT token map (safe: no partial-match bleed) -------------
  exact <- c(
    "SOLID WASTE"="MUNICIPAL SOLID WASTE", "SOLID WASTES"="MUNICIPAL SOLID WASTE",
    "SOLID WASTE MANAGEMENT"="MUNICIPAL SOLID WASTE", "MSW"="MUNICIPAL SOLID WASTE",
    "SOLID WASTE GENERATION"="MUNICIPAL SOLID WASTE",
    "SOLID WASTE MANAGEMENT SYSTEMS"="MUNICIPAL SOLID WASTE",
    "MUNICIPAL SOLID WASTES"="MUNICIPAL SOLID WASTE",
    "MUNICIPAL SOLID WASTE MANAGEMENT"="MUNICIPAL SOLID WASTE",
    "MUNICIPAL SOLID WASTE (MSW)"="MUNICIPAL SOLID WASTE",
    "MUNICIPAL SOLID WASTE GENERATION"="MUNICIPAL SOLID WASTE",
    "INTEGRATED MUNICIPAL SOLID WASTE MANAGEMENT"="MUNICIPAL SOLID WASTE",
    "MUNICIPAL MUNICIPAL SOLID WASTE"="MUNICIPAL SOLID WASTE",
    "MECCA"="MAKKAH", "MEDINA"="MADINAH", "PV"="PHOTOVOLTAIC", "MSW "="MUNICIPAL SOLID WASTE"
  )

  toks <- strsplit(x, ";")
  toks <- lapply(toks, function(v) {
    v <- trimws(gsub("\\s+", " ", v))
    v <- v[v != "" & v != "__DROP__"]
    v <- strip_geo(v)                                  # 2b
    hit <- v %in% names(exact); v[hit] <- exact[v[hit]] # 3b exact map
    for (pat in names(merges)) v <- gsub(pat, merges[[pat]], v, perl = TRUE)
    v <- gsub("\\b(MUNICIPAL ){2,}", "MUNICIPAL ", v)  # final safety heal
    v <- v[v != "" & v != "__DROP__"]
    unique(trimws(v))                 # de-dup tokens created BY merging
  })
  vapply(toks, paste, character(1), collapse = ";")
}

df$`Author Keywords` <- normalize_keywords(df$`Author Keywords`)
df$`Index Keywords`  <- normalize_keywords(df$`Index Keywords`)

# ---- 4. VERIFY (do not skip) ----------------------------------------
kw_all <- c(df$`Author Keywords`, df$`Index Keywords`)
stopifnot(!any(grepl("<SUB>|<SUP>", kw_all)))
stopifnot(!any(grepl("MUNICIPAL MUNICIPAL", kw_all)))
stopifnot(!any(grepl("\\s\\[[^]]+\\]$", kw_all)))  # no trailing geo brackets
# NOTE: do NOT assert absence of "[" — BENZO[A]PYRENE and other PAH names are
# legitimate keywords and must survive. Only trailing " [REGION]" is stripped.

# ---- 5. Catch-up pass from the tab-delimited thesaurus files -----------
load_map <- function(path) {
  ln <- readLines(path, encoding = "UTF-8")
  ln <- ln[!grepl("^\\s*#", ln) & nzchar(trimws(ln))]
  p  <- strsplit(ln, "\t"); p <- p[lengths(p) == 2]
  m  <- setNames(toupper(trimws(vapply(p, `[`, "", 2))),
                 toupper(trimws(vapply(p, `[`, "", 1))))
  m[names(m) != "VARIANT"]
}
normalize_field <- function(x, lookup, sep = ";") {
  vapply(x, function(cell) {
    if (is.na(cell) || !nzchar(cell)) return(cell)
    kw  <- trimws(strsplit(cell, sep, fixed = TRUE)[[1]])
    hit <- kw %in% names(lookup)
    kw[hit] <- lookup[kw[hit]]
    paste(unique(kw[nzchar(kw)]), collapse = ";")
  }, character(1), USE.NAMES = FALSE)
}
kw_map <- c(load_map("results/normalize_terms.txt"), load_map("results/normalize_geographic.txt"))
kw_map <- kw_map[!duplicated(names(kw_map))]

df$`Author Keywords` <- normalize_field(df$`Author Keywords`, kw_map)
df$`Index Keywords`  <- normalize_field(df$`Index Keywords`, kw_map)

# ---- 6. Idempotence check -----------------------------------------------
# After both passes, applying the tab-delimited lookup a further time must
# be a no-op -- if it isn't, the corpus still isn't fully normalized
# against the current thesaurus files.
recheck_de <- normalize_field(df$`Author Keywords`, kw_map)
recheck_id <- normalize_field(df$`Index Keywords`, kw_map)
if (!identical(recheck_de, df$`Author Keywords`) || !identical(recheck_id, df$`Index Keywords`)) {
  stop("Keyword normalization did not converge -- results/normalize_terms.txt / ",
       "results/normalize_geographic.txt still change the output on a repeat pass. ",
       "Check for a merge rule that maps to a value which is itself a variant ",
       "of something else in the thesaurus.")
}

# ---- 7. Sanity check + write ------------------------------------------
kw_freq <- sort(table(unlist(strsplit(df$`Author Keywords`, ";", fixed = TRUE))), decreasing = TRUE)
kw_freq <- kw_freq[nzchar(names(kw_freq))]
print(head(kw_freq, 30))

write_csv(df, OUT_CSV, na = "")
cat("[02_keyword_normalization_v3] wrote", OUT_CSV, "\n")

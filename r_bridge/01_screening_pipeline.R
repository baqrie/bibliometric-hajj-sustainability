# =====================================================================
# 01 — SCREENING PIPELINE  (Scopus query retrieved 2026-07-16, 1,691 records)
# Produces the PRISMA chain and the final analytic corpus (n = 428:
# 121 EVENT / 307 CITY).
#
# Run from the repo root:  Rscript r_bridge/01_screening_pipeline.R
# Reads:  db/<raw Scopus export, 1,691 records>   (NOT in the repo; see README)
#         results/triage_madinah_ruleM_decisions.csv  (frozen Rule-M decision log)
# Writes: db/final_corpus_v428.csv            (pre-normalization; input to 02)
#         db/triage_madinah_flagged.csv       (98 records flagged for Rule M; audit only)
#         results/screening_log_1691.csv      (identifiers + exclusion stage per record)
# The two db/ outputs contain Scopus abstracts and are not redistributed.
# =====================================================================
library(dplyr); library(stringr); library(readr); library(tidyr)

# The raw export name has been seen with a space and with an underscore
RAW_CANDIDATES <- c("db/scopus_export_Jul 16-2026_new_query_1691.csv",
                    "db/scopus_export_Jul_16-2026_new_query_1691.csv")
RAW <- RAW_CANDIDATES[file.exists(RAW_CANDIDATES)][1]
if (is.na(RAW)) stop("Raw Scopus export not found. Put it in db/ under one of: ",
                     paste(RAW_CANDIDATES, collapse = " | "), " (see README).")
RETRIEVAL_DATE <- "2026-07-16"   # state verbatim in Methods

df <- read_csv(RAW, show_col_types = FALSE, guess_max = Inf)

stopifnot(nrow(df) == 1691)

# ---------------------------------------------------------------------
# STAGE 1 — IDENTIFICATION
# ---------------------------------------------------------------------
n_identified <- nrow(df)   # 1691  (doc-type + English limits already applied in Scopus)

# ---------------------------------------------------------------------
# STAGE 2 — DUPLICATE DETECTION  (three-tier cascade)
#   Tier 1  EID          — Scopus's own record ID. Catches export errors only.
#                          It CANNOT catch a work indexed twice under two EIDs.
#   Tier 2  DOI          — case-normalized + whitespace-trimmed. Catches
#                          Scopus double-indexation (real case found: …90051-N vs -n).
#   Tier 3  TITLE        — lowercased, punctuation stripped, whitespace collapsed.
#                          Catches duplicate publication across journals (different DOIs)
#                          and proceedings front-matter. Requires MANUAL adjudication:
#                          identical titles are not always the same work.
# Scopus does NOT de-duplicate content across its own index; Tiers 2–3 are required.
# ---------------------------------------------------------------------
norm_doi   <- function(x) str_trim(str_to_lower(replace_na(x, "")))
norm_title <- function(x) x |> str_to_lower() |> str_replace_all("[^a-z0-9 ]", "") |>
                            str_squish()

df <- df |> mutate(.doi_n = norm_doi(DOI), .ti_n = norm_title(Title))

dup_eid   <- sum(duplicated(df$EID))                                   
dup_doi   <- sum(duplicated(df$.doi_n[df$.doi_n != ""]))               
dup_title <- sum(duplicated(df$.ti_n))                                 
stopifnot(dup_eid == 0, dup_doi == 1, dup_title == 5)

dup_remove_auto <- c(
  "2-s2.0-84996467047",   # proceedings front-matter (no author/DOI)
  "2-s2.0-84996449369",   # proceedings front-matter, 2nd indexation
  "2-s2.0-105006969836"   # Bluedorn 1993 double-indexation (DOI case)
)
dup_remove_manual <- c(
  "2-s2.0-84863818328",  # Seroji 2011 Arab Gulf
  "2-s2.0-0028252922",   # Abd El-Ghani 1994 Feddes
  "2-s2.0-105026754761"  # Mansir 2025 IJRER
)  

ex_dup  <- df$EID[df$EID %in% c(dup_remove_auto, dup_remove_manual)]
df2 <- df |> filter(!(EID %in% c(dup_remove_auto, dup_remove_manual)))
n_after_dedup <- nrow(df2)
stopifnot(n_identified - n_after_dedup == 6)

# ---------------------------------------------------------------------
# STAGE 3 — LANGUAGE REPAIR
# Scopus LIMIT-TO(LANGUAGE,"English")
# The English-only criterion is applied here rather than trusted to Scopus.
# ---------------------------------------------------------------------
ex_lang <- df2$EID[df2$`Language of Original Document` != "English"]
df3 <- df2 |> filter(`Language of Original Document` == "English")
n_lang_removed <- n_after_dedup - nrow(df3)   # expect 1
stopifnot(n_lang_removed == 1)


# ---------------------------------------------------------------------
# STAGE 3b — ENVIRONMENTAL-DOMAIN CRITERION  
# This is faithful to a Scopus TITLE-ABS-KEY search because Title + Abstract +
# Author Keywords + Index Keywords are exactly the fields TITLE-ABS-KEY covers.
# ---------------------------------------------------------------------
# 12 sustainability terms + 11 heat-exposure terms 
ENV12 <- paste(
  "sustainab", "carbon\\s*footprint", "greenhouse\\s*gas", "waste\\s*management",
  "solid\\s*waste", "water\\s*management", "renewable\\s*energy", "air\\s*quality",
  "life\\s*cycle\\s*assessment", "circular\\s*econom", "eco[\\s-]?friendly",
  "environmental\\s*impact",
  "heat\\s*stress", "urban\\s*heat", "heat[\\s-]?related", "heat\\s*wave", "heatwave",
  "heat\\s*illness", "heat\\s*exhaustion", "heat\\s*stroke", "thermal\\s*comfort",
  "thermal\\s*stress", "heat\\s*exposure", sep = "|")

# blob() is used from this stage on (env-domain screening here, scope
# screening and metaphor removal below) -- defined once, up front, rather
# than at its first call site further down.
blob <- function(d) str_to_lower(paste(
  replace_na(d$Title,""), replace_na(d$Abstract,""),
  replace_na(d$`Author Keywords`,""), replace_na(d$`Index Keywords`,"")))

env_ok <- str_detect(blob(df3), ENV12)
n_env_removed <- sum(!env_ok)     # verified: 859
stopifnot(n_env_removed == 859)
ex_env <- df3$EID[!env_ok]
df3 <- df3[env_ok, ]

# ---------------------------------------------------------------------
# STAGE 4 — SCOPE SCREENING  (tiered anchors)
# ---------------------------------------------------------------------
B <- blob(df3)

CORE   <- str_detect(B, "\\bhajj\\b|\\bumrah\\b|\\bumra\\b|makkah|mecca|muzdalifah|jamarat|masjid al-haram|al-madinah al-munawwarah|prophet's mosque|tawaf|ihram|kaaba|ka'bah")
MADINAH<- str_detect(B, "\\bmadinah\\b|\\bmedina\\b|\\bmadina\\b") &
          str_detect(B, "saudi|\\bhijaz\\b|\\bhejaz\\b|\\bhajj\\b|\\bumrah\\b|pilgrim")
GRAND  <- str_detect(B, "grand mosque|holy mosque|holy sites")
SAUDI  <- str_detect(B, "saudi|\\bhijaz\\b|\\bhejaz\\b")
MUSLIM <- str_detect(B, "muslim|islam")

df3 <- df3 |> mutate(
  TIER = case_when(
    CORE | MADINAH            ~ "A_CORE",      # unambiguous Islamic pilgrimage / holy cities
    GRAND                     ~ "B_MOSQUE",    # grand/holy mosque, no core term
    SAUDI                     ~ "C_SAUDI",     # Saudi context only, no holy-city term
    MUSLIM                    ~ "D_MUSLIM",    # muslim/islam only
    TRUE                      ~ "E_NONE"       # no Islamic anchor at all
  ))

# ---- SCOPE_MODE (decided 2026-07-16) ---------------------------------
# "A"  Islamic-only, strict     : keep A_CORE (+ B_MOSQUE)
# "A+" Islamic + Saudi host ctx : keep A_CORE + B_MOSQUE + C_SAUDI (needs a Tier-C rule)
# "B"  All pilgrimage           : keep everything except metaphor/namesake noise
# "C"  Islamic + comparison arm : keep all, tag PILGRIM_TRADITION for a comparison subgroup
SCOPE_MODE <- "A"    # Islamic pilgrimage review (Tier A only)
#  NOTE: Tier B (grand/holy mosque, no core term) was INSPECTED and DROPPED —
#  all 3 surviving records are non-Islamic ("holy sites" matched Polish rural
#  sanctuaries in Mazovia and generic religious tourism).

keep <- switch(SCOPE_MODE,
  "A"  = df3$TIER %in% c("A_CORE"),      # Tier B dropped after inspection
  "A+" = df3$TIER %in% c("A_CORE","B_MOSQUE","C_SAUDI"),
  "B"  = rep(TRUE, nrow(df3)),
  "C"  = rep(TRUE, nrow(df3)))
n_scope_removed <- sum(!keep)   # verified: 310
stopifnot(n_scope_removed == 310)
ex_scope <- df3$EID[!keep]
df4 <- df3[keep, ]


# ---------------------------------------------------------------------
# STAGE 4b — METAPHOR / NAMESAKE REMOVAL
# "mecca" is unguarded in block 1 of the query, so the metaphor class that
# contaminated the previous corpus returns. Verified: 6 records.
# ---------------------------------------------------------------------
meta <- str_detect(blob(df4),
  "mecca (of|for)\\b|\\ba mecca\\b|shopping mecca|tourist mecca|sporting mecca|mecca[- ]cola|black mecca|mecca, california")
n_meta_removed <- sum(meta)       # verified: 6
stopifnot(n_meta_removed == 6)
ex_meta <- df4$EID[meta]
df4 <- df4[!meta, ]

# ---------------------------------------------------------------------
# STAGE 4c — MADINAH LOCALITY SCREENING ("RULE M")
# 509 automated-screening records include some that only mention Madinah
# as a place with no pilgrimage/holy-city anchor elsewhere in the record
# (blob() already covers Title+Abstract+DE+ID). Those got manually
# triaged; decisions are frozen in triage_madinah_ruleM_decisions.csv
# (EID -> RULE_M INCLUDE/EXCLUDE). This stage applies that frozen
# decision log rather than re-deciding anything here.
# ---------------------------------------------------------------------
B4 <- blob(df4)
holy <- str_detect(B4, "\\bhajj\\b|\\bumrah\\b|\\bumra\\b|makkah|mecca|pilgrim|tawaf|ihram|kaaba|muzdalifah|jamarat|prophet's mosque|masjid al-haram|holy cit|holy site")
df4$TRIAGE_FLAG <- ifelse(str_detect(B4, "\\bmadinah\\b|\\bmedina\\b|\\bmadina\\b") & !holy,
                          "MADINAH_NO_PILGRIMAGE_SIGNAL", "")
write_csv(df4 |> filter(TRIAGE_FLAG != "") |> select(-.doi_n, -.ti_n), "db/triage_madinah_flagged.csv", na = "")
stopifnot(sum(df4$TRIAGE_FLAG != "") == 98)   

rule_m <- read_csv("results/triage_madinah_ruleM_decisions.csv", show_col_types = FALSE)
rule_m_exclude <- rule_m$EID[rule_m$RULE_M == "EXCLUDE"] 

ex_rulem <- df4$EID[df4$EID %in% rule_m_exclude]
df4 <- df4 |> filter(!(EID %in% rule_m_exclude))
n_rule_m_removed <- 509 - nrow(df4)
stopifnot(nrow(df4) == 434, n_rule_m_removed == 75)

# ---------------------------------------------------------------------
# STAGE 4d — CONFIRMED FALSE POSITIVES (post-hoc content verification)
# ---------------------------------------------------------------------
false_positives <- c(
  "2-s2.0-0020331338",  # Tucker & Kermond 1982
  "2-s2.0-0029472733",  # Henderson-Sellers et al. 1995
  "2-s2.0-0031393910",   # anonymous 1997, evolutionary economics

  # acronym UMRA/UMRAH = "Universal Matrix of Risk Analysis" (Slovak flood EIA)
  "2-s2.0-84964769115",  # Šiba, Slovakia
  "2-s2.0-85160622331",  # Snakov, Slovakia
  # scope: Iraqi religious tourism; Hajj/Umrah mentioned only in passing
  "2-s2.0-85078236844"
)
stopifnot(all(false_positives %in% df4$EID))
n_before_fp <- nrow(df4)
df4 <- df4 |> filter(!(EID %in% false_positives))
n_false_positives_removed <- n_before_fp - nrow(df4)
ex_fp <- false_positives
stopifnot(n_false_positives_removed == 6)
stopifnot(nrow(df4) == 428)

# ---------------------------------------------------------------------
# STAGE 5 — SUBGROUP TAGGING (RQ3)
# EVENT  = the pilgrimage event itself;  CITY = host-city environmental systems.
# Rule mirrors the previous project: event terms present vs place terms only.
#
# ---------------------------------------------------------------------
B5 <- blob(df4)
df4 <- df4 |> mutate(
  SUBGROUP = if_else(
    str_detect(B5, "\\bhajj\\b|\\bumrah\\b|\\bumra\\b|pilgrim|mass gathering|tawaf|muzdalifah|jamarat|ihram|\\barafat\\b|\\barafah\\b"),
    "EVENT", "CITY"))
# 121 EVENT: the three later false positives (two Slovak UMRA acronym
# records and one Iraqi religious-tourism record)
stopifnot(sum(df4$SUBGROUP == "EVENT") == 121, sum(df4$SUBGROUP == "CITY") == 307)

# ---------------------------------------------------------------------
# STAGE 6 — FREEZE + PRISMA COUNTS
# ---------------------------------------------------------------------
write_csv(df4 |> select(-.doi_n, -.ti_n, -TRIAGE_FLAG), "db/final_corpus_v428.csv", na = "")   # n = 428

# Per-record screening log: identifiers only (no abstracts), safe to share.
stage_of <- function(e) dplyr::case_when(
  e %in% ex_dup   ~ "2_duplicate",
  e %in% ex_lang  ~ "3_non_english",
  e %in% ex_env   ~ "3b_env_domain",
  e %in% ex_scope ~ "4_scope",
  e %in% ex_meta  ~ "4b_metaphor_namesake",
  e %in% ex_rulem ~ "4c_rule_m_madinah",
  e %in% ex_fp    ~ "4d_false_positive",
  e %in% df4$EID  ~ "INCLUDED",
  TRUE            ~ "UNCLASSIFIED")
screen_log <- df |> transmute(EID, DOI, Year, Title, STAGE = stage_of(EID)) |>
  left_join(df4 |> select(EID, SUBGROUP), by = "EID")
stopifnot(!any(screen_log$STAGE == "UNCLASSIFIED"), sum(screen_log$STAGE == "INCLUDED") == 428)
write_csv(screen_log, "results/screening_log_1691.csv", na = "")

cat("PRISMA (retrieval", RETRIEVAL_DATE, ")\n",
    " identified            :", n_identified, "\n",
    " duplicates removed    :", n_identified - n_after_dedup, "\n",
    " non-English removed   :", n_lang_removed, "\n",
    " env-domain excluded   :", n_env_removed, "\n",
    " non-Islamic excluded  :", n_scope_removed, "\n",
    " metaphor/namesake     :", n_meta_removed, "\n",
    " Rule M (Madinah)      :", n_rule_m_removed, "\n",
    " false positives (4d)  :", n_false_positives_removed, "\n",
    " FINAL                 :", nrow(df4), "\n",
    " EVENT / CITY          :", sum(df4$SUBGROUP=="EVENT"), "/", sum(df4$SUBGROUP=="CITY"), "\n",
    " Rule-M flagged, kept  :", sum(df4$TRIAGE_FLAG != ""), "\n")

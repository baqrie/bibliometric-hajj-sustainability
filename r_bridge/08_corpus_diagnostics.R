# =====================================================================
# 08 — CORPUS-WIDE DIAGNOSTICS
#
# Reads:  db/final_corpus_v428.csv (raw text fields; not redistributed)
# Writes (identifiers and flags only, safe to share):
#   results/revision/corpus_diagnostics_summary.csv
#   results/revision/corpus_diagnostics_flags_428.csv
# Needs: dplyr, readr, stringr, tidyr.
# =====================================================================
suppressPackageStartupMessages({ library(dplyr); library(readr); library(stringr); library(tidyr) })

OUT <- "results/revision"
raw <- read_csv("db/final_corpus_v428.csv", show_col_types = FALSE, guess_max = Inf)
stopifnot(nrow(raw) == 428)

lw  <- function(x) str_to_lower(replace_na(as.character(x), ""))
TAD <- paste(lw(raw$Title), lw(raw$Abstract), lw(raw$`Author Keywords`))  # authors' own text
ALL <- paste(TAD, lw(raw$`Index Keywords`))                               # + Scopus index terms
EVENT <- raw$SUBGROUP == "EVENT"

OPT_NARROW <- "optimi[sz]"
OPT_METHOD <- paste(
  "optimi[sz]", "linear programming", "integer programming", "mixed[- ]integer",
  "\\bmilp\\b", "goal programming", "dynamic programming",
  "metaheuristic", "genetic algorithm", "particle swarm", "simulated annealing",
  "ant colony", "nsga", "multi[- ]objective", "\\bhomer\\b", "operations research",
  sep = "|")
# Season / ritual-site framing (pilgrimage calendar or the named sites)
SEASON_SITE <- paste(
  "hajj season", "umrah season", "pilgrimage season", "during (the )?hajj",
  "during (the )?umrah", "hajj period", "\\bmina\\b", "\\barafat\\b", "\\barafah\\b",
  "muzdalifah", "jamarat", "grand mosque", "holy mosque", "masjid al-haram",
  "prophet's mosque", "ritual site", "holy site", "mashaer",
  sep = "|")
# Clinical and travel-medicine work
CLIN_NARROW <- paste(
  "travel medicine", "clinical", "patient", "hospital", "\\bicu\\b",
  "vaccin", "mening", "infection", "infectious", "epidemiolog", sep = "|")
CLIN_BROAD <- paste(CLIN_NARROW, "disease", "respiratory", "heat ?stroke",
                    "mortality", "morbidity", "diagnos", "symptom", sep = "|")

f <- function(text, pat) str_detect(text, pat)
flags <- tibble(
  EID = raw$EID, Year = raw$Year, SUBGROUP = raw$SUBGROUP,
  opt_narrow_author  = f(TAD, OPT_NARROW),
  opt_narrow_all     = f(ALL, OPT_NARROW),
  opt_method_author  = f(TAD, OPT_METHOD),
  opt_method_all     = f(ALL, OPT_METHOD),
  season_site_author = f(TAD, SEASON_SITE),
  clin_narrow_author = f(TAD, CLIN_NARROW),
  clin_broad_author  = f(TAD, CLIN_BROAD),
  clin_narrow_all    = f(ALL, CLIN_NARROW))
write_csv(flags, file.path(OUT, "corpus_diagnostics_flags_428.csv"))

row <- function(claim, variant, rule, n, denom, target_n) tibble(
  claim, variant, rule, n = n, denominator = denom, percent = round(100 * n / denom, 1),
  printed = target_n, matches_printed = n == target_n)

summ <- bind_rows(
  row("(1) optimization in corpus", "optimi* in title/abstract/author kw", "OPT_NARROW on TAD",
      sum(flags$opt_narrow_author), 428, 33),
  row("(1) optimization in corpus", "optimi* incl. index keywords", "OPT_NARROW on ALL",
      sum(flags$opt_narrow_all), 428, 33),
  row("(1) optimization in corpus", "optimization-method terms, author text", "OPT_METHOD on TAD",
      sum(flags$opt_method_author), 428, 33),
  row("(1) optimization in corpus", "optimization-method terms incl. index kw", "OPT_METHOD on ALL",
      sum(flags$opt_method_all), 428, 33),
  row("(2) optimization x season/ritual site", "optimi* (author) & season/site", "OPT_NARROW & SEASON_SITE on TAD",
      sum(flags$opt_narrow_author & flags$season_site_author), 428, 3),
  row("(2) optimization x season/ritual site", "method terms (author) & season/site", "OPT_METHOD & SEASON_SITE on TAD",
      sum(flags$opt_method_author & flags$season_site_author), 428, 3),
  row("(3) clinical/travel medicine in EVENT", "narrow clinical terms, author text", "CLIN_NARROW on TAD",
      sum(flags$clin_narrow_author & EVENT), sum(EVENT), 19),
  row("(3) clinical/travel medicine in EVENT", "broad clinical terms, author text", "CLIN_BROAD on TAD",
      sum(flags$clin_broad_author & EVENT), sum(EVENT), 19),
  row("(3) clinical/travel medicine in EVENT", "narrow clinical terms incl. index kw", "CLIN_NARROW on ALL",
      sum(flags$clin_narrow_all & EVENT), sum(EVENT), 19))
write_csv(summ, file.path(OUT, "corpus_diagnostics_summary.csv"))

cat("\n[08] Corpus diagnostics (printed value vs documented rule):\n")
print(as.data.frame(summ |> select(claim, variant, n, denominator, percent, printed, matches_printed)),
      row.names = FALSE)
cat("\nRecords behind claim (2), author-text method rule:\n")
print(as.data.frame(flags |> filter(opt_method_author & season_site_author) |> select(EID, Year, SUBGROUP)),
      row.names = FALSE)
cat("\nIf no variant matches a printed value, report the documented count instead,",
    "quote the rule in Methods,\nand treat the count as a keyword screen (an upper bound",
    "for claims 1 and 3), not a coded result.\n")

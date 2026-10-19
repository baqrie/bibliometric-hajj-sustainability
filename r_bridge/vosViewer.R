# =====================================================================
# vosViewer.R — exports the frozen analytic corpus into VOSviewer's
# expected column layout for the external (GUI) RQ2 keyword co-occurrence
# map. 
#
# Reads:  db/final_corpus_normalized_428.csv, results/author_thesaurus.txt
# Writes: db/vosviewer_source_428.csv (full corpus)
#         db/vosviewer_source_event_428.csv, db/vosviewer_source_city_428.csv
#         (RQ3 subgroup exports, for keyword-profile cross-checks only --
#         the EVENT subset (n=121) is too small for its own network map)
# Outputs go to db/ because they contain abstracts and reference lists
# (Scopus content) and must not be committed.
# =====================================================================
suppressPackageStartupMessages({
  library(bibliometrix)
  library(dplyr)
})

CORPUS_PATH <- "db/final_corpus_normalized_428.csv"
OUT_DIR     <- "db"

M <- convert2df(CORPUS_PATH, dbsource = "scopus", format = "csv")
stopifnot(nrow(M) == 428, all(c("DE", "ID", "AU", "CR") %in% names(M)))

# Author-name merge, same thesaurus and matcher run_analysis.R uses --
# keeps VOSviewer's author-level views (if any) consistent with the
# bibliometrix-side collaboration network.
load_map <- function(path, upper = TRUE) {
  ln <- readLines(path, encoding = "UTF-8")
  ln <- ln[!grepl("^\\s*#", ln) & nzchar(trimws(ln))]
  p  <- strsplit(ln, "\t"); p <- p[lengths(p) == 2]
  variant   <- trimws(vapply(p, `[`, "", 1))
  canonical <- trimws(vapply(p, `[`, "", 2))
  if (upper) { variant <- toupper(variant); canonical <- toupper(canonical) }
  m <- setNames(canonical, variant)
  m[tolower(names(m)) != "variant"]
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
au_map <- load_map("results/author_thesaurus.txt", upper = FALSE)
au_map <- au_map[!duplicated(names(au_map))]
M$AU <- normalize_field(M$AU, au_map)

# ";" -> "; " for every multi-value field VOSviewer parses.
sep_fix <- function(x) gsub(";\\s*", "; ", x)

vos <- M %>% transmute(
  `Authors`                       = sep_fix(AU),
  `Author full names`             = if ("AF" %in% names(M)) AF else sep_fix(AU),
  `Title`                         = TI,
  `Year`                          = PY,
  `Source title`                  = SO,
  `Cited by`                      = TC,
  `DOI`                           = DI,
  `Abstract`                      = AB,
  `Author Keywords`               = sep_fix(DE),   # already keyword-normalized (02_keyword_normalization_v3.R)
  `Index Keywords`                = sep_fix(ID),   # already keyword-normalized
  `Affiliations`                  = C1,
  `References`                    = sep_fix(CR),
  `Document Type`                 = DT,
  `Language of Original Document` = LA,
  `EID`                           = UT
)

write.csv(vos, file.path(OUT_DIR, "vosviewer_source_428.csv"), row.names = FALSE, na = "")
write.csv(vos[M$SUBGROUP == "EVENT", ], file.path(OUT_DIR, "vosviewer_source_event_428.csv"), row.names = FALSE, na = "")
write.csv(vos[M$SUBGROUP == "CITY",  ], file.path(OUT_DIR, "vosviewer_source_city_428.csv"),  row.names = FALSE, na = "")

cat("[vosViewer] wrote", file.path(OUT_DIR, "vosviewer_source_428.csv"),
    "(", nrow(vos), "rows;", sum(M$SUBGROUP == "EVENT"), "event /",
    sum(M$SUBGROUP == "CITY"), "city )\n")

# =====================================================================
# r_bridge/run_analysis.R
#
# Non-interactive bibliometrix analysis (never calls biblioshiny()).
# Recomputes every number from db/final_corpus_normalized_428.csv on each run.
# Reads:  db/final_corpus_normalized_428.csv, results/*.txt thesauri
# Writes: results/manuscript_tables_pyexport_428.xlsx,
#         results/verify_stats_428.json, results/img/pyexport_428/*.png
#
# Run from the repo root: Rscript r_bridge/run_analysis.R
# =====================================================================

suppressPackageStartupMessages({
  library(bibliometrix)
  library(dplyr)
  library(jsonlite)
  library(openxlsx)
})

set.seed(2026)  # conceptualStructure(clust="auto") and networkPlot(fruchterman)
                # are both stochastic; fix the seed so "recompute" gives the
                # same clusters/layouts every run, not just the same numbers.

CORPUS_PATH <- "db/final_corpus_normalized_428.csv"
OUT_XLSX    <- "results/manuscript_tables_pyexport_428.xlsx"
OUT_JSON    <- "results/verify_stats_428.json"
IMG_DIR     <- "results/img/pyexport_428"
dir.create(IMG_DIR, showWarnings = FALSE, recursive = TRUE)

# ---------------------------------------------------------------------
# 1. Load + known-bug patches (MSW double-word artifact, SO whitespace)
# ---------------------------------------------------------------------
M <- convert2df(CORPUS_PATH, dbsource = "scopus", format = "csv")
stopifnot(nrow(M) == 428)

# --- 1. Load the variant -> canonical map -------------------------------
lines <- readLines("results/normalize_terms.txt", encoding = "UTF-8")
lines <- lines[!grepl("^\\s*#", lines)]              # drop comment lines
parts <- strsplit(lines, "\t")
parts <- parts[lengths(parts) == 2]                  # keep well-formed rows only

lookup <- setNames(
  toupper(trimws(vapply(parts, `[`, "", 2))),        # canonical  (values)
  toupper(trimws(vapply(parts, `[`, "", 1)))         # variant    (names)
)
lookup <- lookup[!duplicated(names(lookup))]         # safety: one mapping per variant

# --- 2. Whole-keyword normalizer for a ';'-separated field --------------
normalize_field <- function(x, lookup, sep = ";") {
  x <- gsub("(MUNICIPAL )+MUNICIPAL SOLID WASTE", "MUNICIPAL SOLID WASTE", x)
  x <- gsub("MUNICIPAL SOLID WASTE( MANAGEMENT| \\(MSW\\))?", "MUNICIPAL SOLID WASTE", x)

  vapply(x, function(cell) {
    if (is.na(cell) || !nzchar(cell)) return(cell)
    kw  <- trimws(strsplit(cell, sep, fixed = TRUE)[[1]])
    hit <- kw %in% names(lookup)
    kw[hit] <- lookup[kw[hit]]
    paste(unique(kw[nzchar(kw)]), collapse = ";")     # unique() drops merge-created dupes
  }, character(1), USE.NAMES = FALSE)
}

#fix_msw <- function(x) {
#  x <- gsub("(MUNICIPAL )+MUNICIPAL SOLID WASTE", "MUNICIPAL SOLID WASTE", x)
#  x <- gsub("MUNICIPAL SOLID WASTE( MANAGEMENT| \\(MSW\\))?", "MUNICIPAL SOLID WASTE", x)
#  x
#}

# --- 3. Apply -----------------------------------------------------------
M$DE <- normalize_field(M$DE, lookup)
M$ID <- normalize_field(M$ID, lookup)
M$SO <- trimws(M$SO)

# extend load_map with a case switch — authors must NOT be uppercased
load_map <- function(path, upper = TRUE) {
  ln <- readLines(path, encoding = "UTF-8")
  ln <- ln[!grepl("^\\s*#", ln) & nzchar(trimws(ln))]
  p  <- strsplit(ln, "\t"); p <- p[lengths(p) == 2]
  variant   <- trimws(vapply(p, `[`, "", 1))
  canonical <- trimws(vapply(p, `[`, "", 2))
  if (upper) { variant <- toupper(variant); canonical <- toupper(canonical) }
  m <- setNames(canonical, variant)
  m[tolower(names(m)) != "variant"]          # drop the header row if the file has one
}

au_map <- load_map("results/author_thesaurus.txt", upper = FALSE)   # case-sensitive!
au_map <- au_map[!duplicated(names(au_map))]

M$AU <- normalize_field(M$AU, au_map)         # same ';'-split whole-token matcher

M <- metaTagExtraction(M, Field = "SR")                  # rebuilds first-author-based labels
M <- metaTagExtraction(M, Field = "AU_CO",  sep = ";")   # authors' countries (from C1)
M <- metaTagExtraction(M, Field = "AU1_CO", sep = ";")   # corresponding-author country (from RP)

stopifnot("SUBGROUP" %in% names(M),               # guard: all() on an absent column is TRUE
          all(M$SUBGROUP %in% c("EVENT", "CITY")),
          sum(M$SUBGROUP == "EVENT") == 121, sum(M$SUBGROUP == "CITY") == 307)

# ---------------------------------------------------------------------
# 2. Verify block -- descriptive stats, recomputed every run
# ---------------------------------------------------------------------
verify <- list(
  n_docs          = nrow(M),
  n_event         = sum(M$SUBGROUP == "EVENT"),
  n_city          = sum(M$SUBGROUP == "CITY"),
  timespan_start  = min(M$PY, na.rm = TRUE),
  timespan_end    = max(M$PY, na.rm = TRUE),
  mean_TC         = round(mean(M$TC, na.rm = TRUE), 2),
  median_TC       = median(M$TC, na.rm = TRUE),
  docs_ge10       = sum(M$TC >= 10, na.rm = TRUE),
  docs_ge50       = sum(M$TC >= 50, na.rm = TRUE),
  peak_year       = as.integer(M %>% count(PY) %>% slice_max(n, n = 1) %>% pull(PY) %>% first()),
  y2026_partial   = sum(M$PY == 2026, na.rm = TRUE),
  median_TC_event = median(M$TC[M$SUBGROUP == "EVENT"], na.rm = TRUE),
  median_TC_city  = median(M$TC[M$SUBGROUP == "CITY"],  na.rm = TRUE),
  n_rp_coverage   = sum(!is.na(M$RP) & trimws(M$RP) != "")
)
write_json(verify, OUT_JSON, auto_unbox = TRUE, pretty = TRUE)
cat("[run_analysis] wrote", OUT_JSON, "\n")

# ---------------------------------------------------------------------
# 3. RQ1 performance analysis (biblioAnalysis + summary, non-interactive
#    -- replaces the biblioshiny()-GUI-dependent country/source tables)
# ---------------------------------------------------------------------
res <- biblioAnalysis(M)
s   <- summary(res, k = 100)

# ---------------------------------------------------------------------
# 4. Affiliation merge (document-level)
# ---------------------------------------------------------------------
#####################
# --- 1. Load affiliation thesaurus (variant -> canonical) ----------------
lines <- readLines("results/affiliation_thesaurus.txt", encoding = "UTF-8")
lines <- lines[!grepl("^\\s*#", lines) & nzchar(trimws(lines))]   # drop comments + blanks
parts <- strsplit(lines, "\t")
parts <- parts[lengths(parts) == 2]                               # well-formed rows only

aff_lookup <- setNames(
  toupper(trimws(vapply(parts, `[`, "", 2))),   # canonical (value)
  toupper(trimws(vapply(parts, `[`, "", 1)))    # variant   (name)
)
aff_lookup <- aff_lookup[names(aff_lookup) != "VARIANT"]          # drop header row
aff_lookup <- aff_lookup[!duplicated(names(aff_lookup))]

# longest variant first, so a specific string is replaced before a shorter substring of it
aff_lookup <- aff_lookup[order(-nchar(names(aff_lookup)))]

# --- 2. Substring normalizer --------------------------------------------
norm_aff <- function(x, lookup = aff_lookup) {
  x <- toupper(x)
  x <- gsub("UMM AL[- ]?QURA UNIVERSITY|UNIVERSITY OF UMM AL[- ]?QURA|UMM ALQURA UNIVERSITY",
            "UMM AL-QURA UNIVERSITY", x)
  x <- gsub("KING ABDUL[- ]?AZIZ UNIVERSITY", "KING ABDULAZIZ UNIVERSITY", x)
  x <- gsub("KING FAHD UNIVERSITY OF PETROLEUM \\(AND|&\\) MINERALS", "KFUPM", x)
  x <- gsub("UNIVERSITY OF (HAIL|HA'IL|HA’IL)", "UNIVERSITY OF HAIL", x)

  for (v in names(lookup)) {
    x <- gsub(v, lookup[[v]], x, fixed = TRUE)   # fixed = TRUE -> no escaping ( ) &
  }
  x
}


targets <- union(
  c("KING ABDULAZIZ UNIVERSITY", "UMM AL-QURA UNIVERSITY",
    "KING SAUD UNIVERSITY",
    "TAIF UNIVERSITY", "UNIVERSITY OF JEDDAH", "KFUPM",
    "ALEXANDRIA UNIVERSITY", "CAIRO UNIVERSITY", "KAUST", "TAIBAH UNIVERSITY",
    "ISLAMIC UNIVERSITY OF MADINAH",
    "KING KHALID UNIVERSITY", "KING FAISAL UNIVERSITY",
    "KING SAUD BIN ABDULAZIZ UNIVERSITY FOR HEALTH SCIENCES",
    "KING ABDULAZIZ CITY FOR SCIENCE AND TECHNOLOGY"),
  unique(unname(aff_lookup))
)

# --- 3. Apply + count (document-level: each institution once per paper) ---
C1n <- norm_aff(M$C1)

tab_affiliations <- tibble(
  Affiliation = targets,
  Documents   = sapply(targets, function(t) sum(grepl(t, C1n, fixed = TRUE)))
) %>% arrange(desc(Documents))

# ---------------------------------------------------------------------
# 5. RQ3 subgroup descriptives
# ---------------------------------------------------------------------
M_event <- M[M$SUBGROUP == "EVENT", ]
M_city  <- M[M$SUBGROUP == "CITY", ]
rq3_summary <- tibble(
  Metric = c("Documents", "Share of corpus (%)", "Median citations", "Mean citations",
             "Docs 2021-2026", "Docs 2021-2026 (%)", "First year", "Peak year"),
  Event = c(nrow(M_event), round(100 * nrow(M_event) / nrow(M), 1),
            median(M_event$TC), round(mean(M_event$TC), 1),
            sum(M_event$PY >= 2021), round(100 * mean(M_event$PY >= 2021), 1),
            min(M_event$PY), M_event %>% count(PY) %>% slice_max(n, n = 1) %>% pull(PY) %>% first()),
  City  = c(nrow(M_city), round(100 * nrow(M_city) / nrow(M), 1),
            median(M_city$TC), round(mean(M_city$TC), 1),
            sum(M_city$PY >= 2021), round(100 * mean(M_city$PY >= 2021), 1),
            min(M_city$PY), M_city %>% count(PY) %>% slice_max(n, n = 1) %>% pull(PY) %>% first())
)
kw_event <- tableTag(M_event, "DE") %>% head(20) %>% as.data.frame()
kw_city  <- tableTag(M_city,  "DE") %>% head(20) %>% as.data.frame()


# The logo is the image that bibliometrix itself uses (its dataset "logo"); the plots are drawn once each,
# as before, so the later steps give the same results.

# ---- A) Paste once, just BEFORE section 5 (the thematic map) -------------------------------------------
KEEP_LOGO <- TRUE          # FALSE = leave the logo out, as in the run of 15 October
LOGO <- local({ e <- new.env(); utils::data("logo", package = "bibliometrix", envir = e); e$logo })
save_fig <- function(p, file, width_mm, height_mm, x, y, w = 0.06) {
  png(file, width = width_mm, height = height_mm, units = "mm", res = 400, bg = "white")
  print(p)
  if (KEEP_LOGO) grid::grid.raster(LOGO, x = grid::unit(x, "npc"), y = grid::unit(y, "npc"),
                                   width = grid::unit(w, "npc"), just = c("left", "top"),
                                   interpolate = TRUE)
  invisible(dev.off())
}

# ---------------------------------------------------------------------
# 6. Thematic map + conceptual structure (RQ2, recomputed fresh)
#
# NOTE: cluster NAMES in the manuscript were assigned by hand after
# inspecting cluster membership; this script reproduces the clusters,
# not the names. Cluster count/composition shifts with minfreq/n.
# ---------------------------------------------------------------------
geo_terms <- c("SAUDI ARABIA", "MAKKAH", "JEDDAH", "MADINAH", "RIYADH")
###############################
load_map <- function(path) {
  ln <- readLines(path, encoding = "UTF-8")
  ln <- ln[!grepl("^\\s*#", ln) & nzchar(trimws(ln))]
  p  <- strsplit(ln, "\t"); p <- p[lengths(p) == 2]
  m  <- setNames(toupper(trimws(vapply(p, `[`, "", 2))),   # canonical
                 toupper(trimws(vapply(p, `[`, "", 1))))    # variant
  m[names(m) != "VARIANT"]                                  # drop header row if present
}

kw_map <- c(load_map("results/normalize_terms.txt"), load_map("results/normalize_geographic.txt"))
kw_map <- kw_map[!duplicated(names(kw_map))]

# normalize FIRST — collapses MECCA / MAKKAH REGION / KSA / ... to canonicals
M$DE <- normalize_field(M$DE, kw_map)   # normalize_field() from the topic-term step
M$ID <- normalize_field(M$ID, kw_map)

geo_canon <- unique(unname(load_map("results/normalize_geographic.txt")))
# "MAKKAH"  "MADINAH"  "SAUDI ARABIA"  "VISION 2030"

geo_terms <- setdiff(geo_canon, "VISION 2030")           # keep the topic, drop the places
geo_terms <- union(geo_terms, c("JEDDAH", "RIYADH"))     # not in the file; bare-keyword removal

tm <- thematicMap(M, field = "DE", n = 250, minfreq = 4, stemming = FALSE,
                  size = 0.5, repel = TRUE, remove.terms = geo_terms)
ggplot2::ggsave(file.path(IMG_DIR, "thematic_map_DE.png"), tm$map,
                width = 200, height = 160, units = "mm", dpi = 400)
tm_terms <- tm$words %>% arrange(Cluster, desc(Occurrences))


p <- tm$map   # or CS$graph_terms for the MCA
p$layers <- p$layers[!vapply(p$layers, 
                      function(L) inherits(L$geom, "GeomCustomAnn"), logical(1))]
#ggplot2::ggsave(file.path(IMG_DIR, "thematic_map_DE_.png"), p, 
#                width = 200, height = 160, units = "mm", dpi = 400)

# ---- B) Figure 3: replace the ggsave(...) call that writes "thematic_map_DE_.png" with this line --------
# (top of the Niche Themes quadrant, right of its label: empty in the current map)
save_fig(p, file.path(IMG_DIR, "thematic_map_DE_.png"), 200, 160, x = 0.32, y = 0.95)

###############################

png(file.path(IMG_DIR, "conceptual_structure_%02d.png"),
    width = 10, height = 8, units = "in", res = 400, bg = "white")

cs_ok <- TRUE
cs <- tryCatch(
  conceptualStructure(M, field = "DE", method = "MCA", minDegree = 4,
                      clust = "auto", remove.terms = geo_terms,
                      k.max = 5,  stemming  = FALSE, labelsize = 14,
                      documents = 0),
  error = function(e) {
    cat("[run_analysis] conceptualStructure skipped:", conditionMessage(e), "\n")
    cs_ok <<- FALSE
    NULL
  }
)

dev.off()


# Written from the bibliometrix source: graph_terms is a ggplot with the logo added 
# by annotation_custom(),
# graph_dendogram is a list (class "bibliodendrogram") with $dend and $line.
# Check both PNGs by eye before copying them into the manuscript's Figures folder.

if (isTRUE(cs_ok) && !is.null(cs)) {

  # Figure 4: MCA map without the bibliometrix logo (same method as the thematic map)
  p_mca <- cs$graph_terms
  p_mca$layers <- p_mca$layers[!vapply(p_mca$layers,
                                       function(L) inherits(L$geom, "GeomCustomAnn"),
                                       logical(1))]
  #ggplot2::ggsave(file.path(IMG_DIR, "conceptual_structure_01_nologo.png"), p_mca,
  #                width = 254, height = 203, units = "mm", dpi = 400, bg = "white")
  # ---- C) Figure 4: in the block pasted from figfix_snippet_2026-10-15.R, replace the ggsave(...) call ------
  #         that writes "conceptual_structure_01_nologo.png" with this line
  # (top-left corner of the map: empty; the canvas is the original 254 x 203 mm)
  save_fig(p_mca, file.path(IMG_DIR, "conceptual_structure_01_fig4.png"), 254, 203, x = 0.10, y = 0.93)

  # Figure 5: dendrogram with a wider bottom margin, so the rotated labels are not cut off
  png(file.path(IMG_DIR, "conceptual_structure_02_margins.png"),
      width = 10, height = 8, units = "in", res = 400, bg = "white")
  op <- par(mar = c(13, 4, 1, 1), cex = 1.1)
  plot(cs$graph_dendogram$dend, ylab = "Height")
  abline(h = cs$graph_dendogram$line, lty = 2)
  par(op)
  dev.off()


}

# ---------------------------------------------------------------------
# 7. Historiograph (direct citation history network)
# ---------------------------------------------------------------------
hn <- histNetwork(M, sep = ";", verbose = FALSE)
hp <- histPlot(hn, n = 30, verbose = FALSE)
ggplot2::ggsave(file.path(IMG_DIR, "historiograph.png"), hp$g,
                 width = 260, height = 180, units = "mm", dpi = 400)
histdata_tab <- hn$histData

# ---------------------------------------------------------------------
# 8. Collaboration networks (author / country / institution)
# ---------------------------------------------------------------------
plot_network <- function(NetMatrix, file, title) {
  png(file, width = 10, height = 10, units = "in", res = 400, bg = "white")
  net <- networkPlot(NetMatrix, n = 30, Title = title, type = "fruchterman",
                      size = 4, size.cex = TRUE, remove.multiple = TRUE,
                      remove.isolates = TRUE, labelsize = 0.7, verbose = TRUE)
  dev.off()
  net$cluster_res
}

collab_nodes <- list()
for (unit in c("authors", "countries", "universities")) {
  nm <- biblioNetwork(M, analysis = "collaboration", network = unit, sep = ";")
  collab_nodes[[unit]] <- plot_network(
    nm, file.path(IMG_DIR, paste0("collab_", unit, ".png")),
    paste("Collaboration -", unit))
}

# ---------------------------------------------------------------------
# 9. Coupling networks (author / country / article)
# ---------------------------------------------------------------------
coupling_units <- c(authors = "authors", countries = "countries", articles = "references")
coupling_nodes <- list()
for (key in names(coupling_units)) {
  nm <- biblioNetwork(M, analysis = "coupling", network = coupling_units[[key]], sep = ";")
  coupling_nodes[[key]] <- plot_network(
    nm, file.path(IMG_DIR, paste0("coupling_", key, ".png")),
    paste("Coupling -", key))
}

# ---------------------------------------------------------------------
# 10. Co-citation networks (author / article -- bibliometrix has no
#     country-level co-citation network)
# ---------------------------------------------------------------------
M <- metaTagExtraction(M, Field = "CR_AU", sep = ";")  # required for co-citation-by-authors

cocitation_units <- c(authors = "authors", articles = "references")
cocitation_nodes <- list()
for (key in names(cocitation_units)) {
  nm <- biblioNetwork(M, analysis = "co-citation", network = cocitation_units[[key]], sep = ";")
  cocitation_nodes[[key]] <- plot_network(
    nm, file.path(IMG_DIR, paste0("cocitation_", key, ".png")),
    paste("Co-citation -", key))
}

# ---------------------------------------------------------------------
# 11. Export all tables to one workbook
# ---------------------------------------------------------------------
wb <- createWorkbook()
add_sheet <- function(name, df) {
  nm <- substr(name, 1, 31)  # Excel caps sheet names at 31 chars
  addWorksheet(wb, nm)
  writeData(wb, nm, df)
}

add_sheet("MainInformation",   s$MainInformationDF)
add_sheet("AnnualProduction",  s$AnnualProduction)
add_sheet("MostRelKeywords",   s$MostRelKeywords)
add_sheet("MostRelSources",    s$MostRelSources)
add_sheet("MostCitedPapers",   s$MostCitedPapers)
add_sheet("MostProdCountries", s$MostProdCountries)
add_sheet("TCperCountries",    s$TCperCountries)
add_sheet("MostProdAuthors",   s$MostProdAuthors)
add_sheet("Affiliations",      tab_affiliations)
add_sheet("RQ3_summary",       rq3_summary)
add_sheet("RQ3_kw_event",      kw_event)
add_sheet("RQ3_kw_city",       kw_city)
add_sheet("TM_terms",          tm_terms)
add_sheet("Historiograph",     histdata_tab)
for (unit in names(collab_nodes))     add_sheet(paste0("Collab_", unit),   collab_nodes[[unit]])
for (unit in names(coupling_nodes))   add_sheet(paste0("Coupling_", unit), coupling_nodes[[unit]])
for (unit in names(cocitation_nodes)) add_sheet(paste0("Cocit_", unit),    cocitation_nodes[[unit]])

saveWorkbook(wb, OUT_XLSX, overwrite = TRUE)
cat("[run_analysis] wrote", OUT_XLSX, "\n")
cat("[run_analysis] wrote figures to", IMG_DIR, "\n")


verify$conceptual_structure_ok <- cs_ok
write_json(verify, OUT_JSON, auto_unbox = TRUE, pretty = TRUE)

cat("[run_analysis] DONE\n")

# =====================================================================
# 06 — SCIENCE-MAP SENSITIVITY
# Re-runs the thematic map and the MCA under alternative settings and
# reports, for each one, how far the clusters move from the published
# baseline (minfreq 4, n 250, Louvain -- the thematicMap default -- ; MCA
# minDegree 4, k.max 5).

## Reads:  db/final_corpus_normalized_428.csv, results/normalize_*.txt
# Writes: results/revision/map_sensitivity.csv
#         results/revision/map_sensitivity_terms.csv (term -> cluster, per variant)
#         results/revision/mca_inertia.csv
# Needs: bibliometrix, dplyr, readr.
# =====================================================================
suppressPackageStartupMessages({ library(bibliometrix); library(dplyr); library(readr) })

OUT <- "results/revision"
dir.create(OUT, showWarnings = FALSE, recursive = TRUE)
SEED <- 2026

# ---------------------------------------------------------------------
# 1. Same load + normalization as run_analysis.R (sections 1 and 6)
# ---------------------------------------------------------------------
load_map <- function(path) {
  ln <- readLines(path, encoding = "UTF-8")
  ln <- ln[!grepl("^\\s*#", ln) & nzchar(trimws(ln))]
  p  <- strsplit(ln, "\t"); p <- p[lengths(p) == 2]
  m  <- setNames(toupper(trimws(vapply(p, `[`, "", 2))), toupper(trimws(vapply(p, `[`, "", 1))))
  m[names(m) != "VARIANT"]
}
normalize_field <- function(x, lookup, sep = ";") {
  x <- gsub("(MUNICIPAL )+MUNICIPAL SOLID WASTE", "MUNICIPAL SOLID WASTE", x)
  x <- gsub("MUNICIPAL SOLID WASTE( MANAGEMENT| \\(MSW\\))?", "MUNICIPAL SOLID WASTE", x)
  vapply(x, function(cell) {
    if (is.na(cell) || !nzchar(cell)) return(cell)
    kw <- trimws(strsplit(cell, sep, fixed = TRUE)[[1]])
    hit <- kw %in% names(lookup); kw[hit] <- lookup[kw[hit]]
    paste(unique(kw[nzchar(kw)]), collapse = ";")
  }, character(1), USE.NAMES = FALSE)
}

M0 <- convert2df("db/final_corpus_normalized_428.csv", dbsource = "scopus", format = "csv")
stopifnot(nrow(M0) == 428)
kw_map <- c(load_map("results/normalize_terms.txt"), load_map("results/normalize_geographic.txt"))
kw_map <- kw_map[!duplicated(names(kw_map))]
M0$DE <- normalize_field(M0$DE, kw_map)
M0$ID <- normalize_field(M0$ID, kw_map)

geo_canon  <- setdiff(unique(unname(load_map("results/normalize_geographic.txt"))), "VISION 2030")
GEO_PUBLISHED <- union(geo_canon, c("JEDDAH", "RIYADH"))   # what run_analysis.R removes
GEO_QUERY_ONLY <- geo_canon                                  # query place names only
EVENT_ANCHORS <- c("HAJJ", "UMRAH", "PILGRIMAGE", "PILGRIMS", "PILGRIM", "MASS GATHERINGS", "MASS GATHERING")

EVENT_MARKERS <- c("HAJJ", "MASS GATHERINGS", "PILGRIMAGE", "HEAT STRESS", "HEAT STROKE", "HEAT-RELATED ILLNESS")
HEAT_MARKERS  <- c("HEAT STRESS", "HEAT STROKE", "HEAT-RELATED ILLNESS", "HEAT EXPOSURE")
CITY_MARKERS  <- c("AIR QUALITY", "PM2.5", "PM10", "GROUNDWATER", "SOLAR ENERGY", "PHOTOVOLTAIC",
                   "LAND USE", "URBAN HEAT ISLAND", "WASTEWATER")

# Corpus variant F: records anchored in the authors' own text
flags_path <- file.path(OUT, "retrieval_flags_428.csv")
M_author <- if (file.exists(flags_path)) {
  fl <- read_csv(flags_path, show_col_types = FALSE)
  M0[toupper(M0$UT) %in% toupper(fl$EID[fl$ANCHOR_SOURCE == "AUTHOR_TEXT"]), ]   # convert2df upper-cases UT
} else NULL

# ---------------------------------------------------------------------
# 2. Helpers
# ---------------------------------------------------------------------
ari <- function(x, y) {
  t <- table(x, y); n <- sum(t)
  if (n < 2) return(NA_real_)
  s <- sum(choose(t, 2)); a <- sum(choose(rowSums(t), 2)); b <- sum(choose(colSums(t), 2))
  e <- a * b / choose(n, 2); d <- (a + b) / 2 - e
  if (d == 0) return(NA_real_)
  (s - e) / d
}
separation <- function(memb, ev, ct) {
  ev <- intersect(ev, names(memb)); ct <- intersect(ct, names(memb))
  if (!length(ev) || !length(ct)) return(c(sep = NA_real_, n_pairs = 0))
  pairs <- expand.grid(e = ev, c = ct, stringsAsFactors = FALSE)
  c(sep = mean(memb[pairs$e] != memb[pairs$c]), n_pairs = nrow(pairs))
}
# term -> cluster from a thematicMap result (column names differ by version)
tm_membership <- function(tm) {
  w <- as.data.frame(tm$words)
  wc <- intersect(c("Words", "words", "Word"), names(w))[1]
  cc <- intersect(c("Cluster", "Cluster_Label", "cluster", "groups"), names(w))[1]
  if (is.na(wc) || is.na(cc)) stop("Unexpected thematicMap$words columns: ", paste(names(w), collapse = ", "))
  setNames(as.character(w[[cc]]), toupper(trimws(w[[wc]])))
}
run_tm <- function(M, minfreq = 4, n = 250, remove = GEO_PUBLISHED, cluster = "louvain") {
  set.seed(SEED)
  args <- list(M, field = "DE", n = n, minfreq = minfreq, stemming = FALSE,
               size = 0.5, repel = TRUE, remove.terms = remove,
               cluster = cluster)   # [8 Oct 2026] always explicit; "louvain" is the default the paper used
  tm <- do.call(thematicMap, args)
  tm_membership(tm)
}
floor_of <- function(minfreq, N) max(2, floor(minfreq * N / 1000))   # thematicMap occurrence floor
run_mca <- function(M, minDegree = 4, remove = GEO_PUBLISHED) {
  set.seed(SEED)
  pdf(NULL)                                            # conceptualStructure always plots
  on.exit(dev.off())
  cs <- conceptualStructure(M, field = "DE", method = "MCA", minDegree = minDegree,
                            clust = "auto", k.max = 5, stemming = FALSE, labelsize = 10,
                            documents = 0, remove.terms = remove, graph = FALSE)
  memb <- cs$km.res$cluster                           # hclust + cutree in bibliometrix
  if (is.null(memb) && !is.null(cs$km.res$data.clust)) {
    dc <- cs$km.res$data.clust; memb <- setNames(dc[[ncol(dc)]], rownames(dc))
  }
  if (is.null(memb)) stop("could not find MCA cluster membership in conceptualStructure() output")
  names(memb) <- toupper(names(memb))
  eig <- tryCatch(as.numeric(cs$res$eig$perc[1:2]), error = function(e) c(NA, NA))
  list(memb = setNames(as.character(memb), names(memb)), inertia = eig)
}

# ---------------------------------------------------------------------
# 3. Thematic-map variants
# ---------------------------------------------------------------------
tm_specs <- list(
  list(id = "TM0 baseline (published: Louvain, minfreq 4 = floor 2)", M = M0, minfreq = 4, n = 250, remove = GEO_PUBLISHED, cluster = "louvain"),
  list(id = "TM-A floor 3 (minfreq 8)",          M = M0, minfreq = 8,  n = 250, remove = GEO_PUBLISHED, cluster = "louvain"),
  list(id = "TM-A floor 4 (minfreq 10)",         M = M0, minfreq = 10, n = 250, remove = GEO_PUBLISHED, cluster = "louvain"),
  list(id = "TM-A floor 5 (minfreq 12)",         M = M0, minfreq = 12, n = 250, remove = GEO_PUBLISHED, cluster = "louvain"),
  list(id = "TM-B n 200",                        M = M0, minfreq = 4, n = 200, remove = GEO_PUBLISHED, cluster = "louvain"),
  list(id = "TM-B n 300",                        M = M0, minfreq = 4, n = 300, remove = GEO_PUBLISHED, cluster = "louvain"),
  list(id = "TM-C walktrap",                     M = M0, minfreq = 4, n = 250, remove = GEO_PUBLISHED, cluster = "walktrap"),
  list(id = "TM-D event anchors removed",        M = M0, minfreq = 4, n = 250, remove = union(GEO_PUBLISHED, EVENT_ANCHORS), cluster = "louvain"),
  list(id = "TM-E query place names only removed", M = M0, minfreq = 4, n = 250, remove = GEO_QUERY_ONLY, cluster = "louvain"))
if (!is.null(M_author))
  tm_specs <- c(tm_specs, list(list(id = "TM-F author-anchored corpus", M = M_author, minfreq = 4,
                                    n = 250, remove = GEO_PUBLISHED, cluster = "louvain")))

mca_specs <- list(
  list(id = "MCA0 baseline (published)",  M = M0, minDegree = 4, remove = GEO_PUBLISHED),
  list(id = "MCA-A minDegree 3",          M = M0, minDegree = 3, remove = GEO_PUBLISHED),
  list(id = "MCA-A minDegree 5",          M = M0, minDegree = 5, remove = GEO_PUBLISHED),
  list(id = "MCA-D event anchors removed", M = M0, minDegree = 4, remove = union(GEO_PUBLISHED, EVENT_ANCHORS)))
if (!is.null(M_author))
  mca_specs <- c(mca_specs, list(list(id = "MCA-F author-anchored corpus", M = M_author, minDegree = 4,
                                      remove = GEO_PUBLISHED)))

safe <- function(expr, id) tryCatch(expr, error = function(e) {
  message("[06] ", id, " failed: ", conditionMessage(e)); NULL })

summ <- list(); terms <- list(); inertia <- list()
base_tm <- NULL; base_mca <- NULL
for (s in tm_specs) {
  memb <- safe(run_tm(s$M, s$minfreq, s$n, s$remove, s$cluster), s$id)
  if (is.null(memb)) next
  if (is.null(base_tm)) base_tm <- memb
  shared <- intersect(names(memb), names(base_tm))
  ev <- if (grepl("event anchors", s$id)) HEAT_MARKERS else EVENT_MARKERS
  sp <- separation(memb, ev, CITY_MARKERS)
  summ[[s$id]] <- tibble(variant = s$id, method = "thematic map", records = nrow(s$M),
                         minfreq = s$minfreq, occurrence_floor = floor_of(s$minfreq, nrow(s$M)), cluster_algorithm = s$cluster,
                         terms = length(memb), clusters = length(unique(memb)),
                         shared_terms_with_baseline = length(shared),
                         ARI_vs_baseline = round(ari(memb[shared], base_tm[shared]), 3),
                         event_city_separation = round(sp[["sep"]], 3), marker_pairs = sp[["n_pairs"]])
  terms[[s$id]] <- tibble(variant = s$id, term = names(memb), cluster = memb)
}
for (s in mca_specs) {
  r <- safe(run_mca(s$M, s$minDegree, s$remove), s$id)
  if (is.null(r)) next
  memb <- r$memb
  if (is.null(base_mca)) base_mca <- memb
  shared <- intersect(names(memb), names(base_mca))
  ev <- if (grepl("event anchors", s$id)) HEAT_MARKERS else EVENT_MARKERS
  sp <- separation(memb, ev, CITY_MARKERS)
  summ[[s$id]] <- tibble(variant = s$id, method = "MCA", records = nrow(s$M),
                         terms = length(memb), clusters = length(unique(memb)),
                         shared_terms_with_baseline = length(shared),
                         ARI_vs_baseline = round(ari(memb[shared], base_mca[shared]), 3),
                         event_city_separation = round(sp[["sep"]], 3), marker_pairs = sp[["n_pairs"]])
  terms[[s$id]] <- tibble(variant = s$id, term = names(memb), cluster = memb)
  inertia[[s$id]] <- tibble(variant = s$id, dim1_pct = round(r$inertia[1], 2), dim2_pct = round(r$inertia[2], 2),
                            dims12_pct = round(sum(r$inertia), 2))
}

summ <- bind_rows(summ)
write_csv(summ, file.path(OUT, "map_sensitivity.csv"))
write_csv(bind_rows(terms), file.path(OUT, "map_sensitivity_terms.csv"))
write_csv(bind_rows(inertia), file.path(OUT, "mca_inertia.csv"))

cat("\n[06] Map sensitivity (baseline should show 11 thematic clusters and MCA dims 1+2 = 45.29%):\n")
print(as.data.frame(summ), row.names = FALSE)
cat("\n[06] MCA inertia:\n"); print(as.data.frame(bind_rows(inertia)), row.names = FALSE)
cat("\nMarker terms found in the baseline map:",
    paste(intersect(c(EVENT_MARKERS, CITY_MARKERS), names(base_tm)), collapse = ", "), "\n")
cat("Read: ARI near 1 = same clusters; separation near 1 = event and host-city themes stay apart.\n")

# =====================================================================
# 04 — RQ1 PERFORMANCE ANALYSIS (reproducible, thesaurus-aware)
# Input : db/final_corpus_normalized_428.csv (output of 01 + 02)
# Output: results/RQ1_tables.xlsx, results/summary_428_corpus.xlsx,
#         results/Fig2_annual_production_stacked.(png|pdf)
# Requires: bibliometrix, dplyr, tidyr, stringr, ggplot2, openxlsx
# =====================================================================
library(bibliometrix); library(dplyr); library(tidyr); library(stringr)
library(ggplot2); library(openxlsx)


M <- convert2df("db/final_corpus_normalized_428.csv", dbsource = "scopus", format = "csv")
# convert2df may re-read raw keyword columns; re-assert the normalized ones:
norm <- read.csv("db/final_corpus_normalized_428.csv", check.names = FALSE)
M$DE <- toupper(norm$`Author Keywords`)[match(toupper(M$UT), toupper(norm$EID))]
M$ID <- toupper(norm$`Index Keywords`)[match(toupper(M$UT), toupper(norm$EID))]
M$SO <- trimws(M$SO)
stopifnot(nrow(M) == 428, nrow(norm) == 428)

results <- biblioAnalysis(M)
S <- summary(results, k = 60, pause = FALSE)
wb <- createWorkbook()

for (nm in names(S)) {
  addWorksheet(wb, substr(nm, 1, 31))          # Excel caps sheet names at 31 chars
  writeData(wb, substr(nm, 1, 31), S[[nm]])
}
saveWorkbook(wb, "results/summary_428_corpus.xlsx", overwrite = TRUE)


# ---- load the thesauri / lists (results/) ----------------------------
read_thes <- function(f) {                       # variant<TAB>canonical
  x <- readLines(f); x <- x[!grepl("^#|^$|^label", x)]
  m <- do.call(rbind, strsplit(x, "\t"))
  setNames(trimws(m[,2]), toupper(trimws(m[,1])))
}
excluded <- toupper(trimws(readLines("results/excluded_terms.txt")))
excluded <- excluded[!grepl("^#|^$|^##", excluded)]

# =====================================================================
# (1)+(5) KEYWORD TABLES with EMTREE/geography terms removed
# =====================================================================
kw_table <- function(field, n = 60) {
  tab <- tableTag(M, field)                       # named freq vector
  tab <- tab[!(names(tab) %in% excluded)]         # drop EMTREE + geo
  head(data.frame(term = names(tab), freq = as.integer(tab)), n)
}
T_DE <- kw_table("DE"); T_ID <- kw_table("ID")

# =====================================================================
# (2) INSTITUTIONS table — affiliation thesaurus, DOCUMENT-level count
# =====================================================================
aff_map <- read_thes("results/affiliation_thesaurus.txt")
norm_aff <- function(x){
  x <- toupper(x)
  for (v in names(aff_map)) x <- gsub(v, toupper(aff_map[[v]]), x, fixed = TRUE)
  x
}
C1n <- norm_aff(M$C1)
targets <- toupper(unique(aff_map))
inst <- data.frame(
  Institution = targets,
  Documents   = sapply(targets, function(t) sum(str_detect(replace_na(C1n,""), fixed(t))))
) %>% arrange(desc(Documents))
# NOTE footnote for the paper: "documents with >=1 author from the institution;
#   name variants merged; see affiliation_thesaurus.txt (supplementary)."

# =====================================================================
# (3) COUNTRIES with n beside the average; flag n < 3
# =====================================================================
prod <- as.data.frame(results$Countries) %>%           # corresponding-author counts
        rename(Country = 1, Articles = Freq)
tcc  <- as.data.frame(results$CountryCollaboration)     # has SCP/MCP
M <- metaTagExtraction(M, Field = "AU_CO", sep = ";")   # authors' countries (from C1), as in run_analysis.R
tc_by_country <- M %>%
  mutate(CO = toupper(trimws(sub(";.*","", replace_na(M$AU_CO, ""))))) %>%
  filter(CO != "") %>% group_by(Country = CO) %>%
  summarise(n_docs = n(), total_TC = sum(TC, na.rm = TRUE),
            avg_TC = round(mean(TC, na.rm = TRUE), 1), .groups = "drop") %>%
  mutate(flag = if_else(n_docs < 3, "n<3 — interpret with caution", "")) %>%
  arrange(desc(total_TC))

# =====================================================================
# (4) SOURCES (trim already applied) + PM label handled by normalization
# =====================================================================
T_sources <- M %>% count(SO, sort = TRUE, name = "Documents") %>% head(20)

# =====================================================================
# ANNUAL PRODUCTION, stacked by SUBGROUP (RQ1 headline + previews RQ3)
# =====================================================================
sg <- norm$SUBGROUP[match(toupper(M$UT), toupper(norm$EID))]
prod_year <- data.frame(PY = M$PY, SUBGROUP = sg) %>% count(PY, SUBGROUP)
p <- ggplot(prod_year, aes(PY, n,
       fill = factor(SUBGROUP, c("CITY","EVENT"),
                     c("Host-city studies","Event (Hajj/Umrah) studies")))) +
  geom_col(width = .85) +
  scale_fill_manual(values = c("#7f8fa6","#c0392b"), name = NULL) +
  scale_x_continuous(breaks = seq(1985, 2025, 5)) +
  labs(x = "Year", y = "Documents",
       caption = "2026 is a partial year (retrieval July 2026).") +
  theme_classic(base_size = 12) + theme(legend.position = c(.24,.85))
ggsave("results/Fig2_annual_production_stacked.png", p, width = 180, height = 110, units = "mm", dpi = 400)
ggsave("results/Fig2_annual_production_stacked.pdf", p, width = 180, height = 110, units = "mm")

# =====================================================================
# WRITE ALL TABLES
# =====================================================================
wb <- createWorkbook()
add <- function(name, df){ addWorksheet(wb, name); writeData(wb, name, df) }
add("T1_sources",       T_sources)
add("T2_institutions",  head(inst, 15))
add("T3_countries_n",   tc_by_country)
add("T4_prod_countries",prod)
add("T5_top_cited",     as.data.frame(results$MostCitedPapers)[1:20, ])
add("T6_authors",       data.frame(Author = names(results$Authors)[1:20],
                                    Articles = as.integer(results$Authors)[1:20]))
add("T7_keywords_DE",   T_DE)
add("T8_keywords_ID",   T_ID)
saveWorkbook(wb, "results/RQ1_tables.xlsx", overwrite = TRUE)

cat("results/RQ1_tables.xlsx + Fig2 written.\n")
cat("Institutions (merged, doc-level): \n"); print(head(inst, 8))
cat("\nVERIFY block (paste values into Methods; must match manuscript):\n")
print(list(n = nrow(M), event = sum(sg=="EVENT"), city = sum(sg=="CITY"),
           mean_TC = round(mean(M$TC),2), median_TC = median(M$TC),
           sources = n_distinct(M$SO), peak = names(sort(table(M$PY),decreasing=TRUE))[1]))

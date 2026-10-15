# =============================================================================
# make_lab_data.R
# Builds the fictional measles LAB dataset used in the proposed redesign
# (see proposal/README.md). Students never run this script: they get the
# CSV files it writes to raw_data/.
#
# Story: a subset of the measles cases had a blood sample taken at diagnosis.
# A 60-gene immune panel was measured on each sample (read counts).
#
# Files written:
#   raw_data/measles_lab_samples.csv    sample sheet: one row per sample (messy)
#   raw_data/measles_immune_panel.csv   count matrix: genes in rows, samples in columns
#   raw_data/gene_annotation.csv        gene ID -> symbol -> pathway (messy)
#
# Built-in signals (what students should be able to find):
#   - Vaccinated cases: LOWER interferon genes (less virus), HIGHER antibody genes
#   - Cases who died:   HIGHER inflammation genes (IL6, IL10, CXCL8, S100A8/9)
#   - Batch B2:         larger library sizes + 12 genes shifted (batch effect)
#
# Planted problems (to clean with skills from Sessions 2-4):
#   - Sample sheet: messy column names, lower-case/space-padded sample IDs,
#     patient IDs missing the hyphen, "b2" vs "B2", 2 duplicated rows,
#     3 samples from patients not in the case list
#   - Count matrix: 1 sample column with no row in the sample sheet
#   - Annotation: Excel turned MARCH1 into "1-Mar" and SEPT7 into "7-Sep";
#     1 gene in the matrix is missing from the annotation
# =============================================================================

library(tidyverse)
library(here)

set.seed(2027)

# ---- Genes ------------------------------------------------------------------
genes <- tribble(
  ~symbol,     ~pathway,
  "ISG15",     "Interferon response",
  "MX1",       "Interferon response",
  "OAS1",      "Interferon response",
  "OAS2",      "Interferon response",
  "IFIT1",     "Interferon response",
  "IFIT3",     "Interferon response",
  "IFI44L",    "Interferon response",
  "RSAD2",     "Interferon response",
  "IFI27",     "Interferon response",
  "CXCL10",    "Interferon response",
  "STAT1",     "Interferon response",
  "IRF7",      "Interferon response",
  "IFNG",      "Interferon response",
  "SIGLEC1",   "Interferon response",
  "IL6",       "Inflammation",
  "IL10",      "Inflammation",
  "CXCL8",     "Inflammation",
  "TNF",       "Inflammation",
  "IL1B",      "Inflammation",
  "CCL2",      "Inflammation",
  "S100A8",    "Inflammation",
  "S100A9",    "Inflammation",
  "NLRP3",     "Inflammation",
  "PTGS2",     "Inflammation",
  "CD3E",      "T cell",
  "CD4",       "T cell",
  "CD8A",      "T cell",
  "GZMB",      "T cell",
  "PRF1",      "T cell",
  "CD69",      "T cell",
  "IL2RA",     "T cell",
  "FOXP3",     "T cell",
  "PDCD1",     "T cell",
  "LAG3",      "T cell",
  "CD19",      "B cell / antibody",
  "MS4A1",     "B cell / antibody",
  "CD27",      "B cell / antibody",
  "JCHAIN",    "B cell / antibody",
  "MZB1",      "B cell / antibody",
  "IGHG1",     "B cell / antibody",
  "IGHM",      "B cell / antibody",
  "TNFRSF17",  "B cell / antibody",
  "XBP1",      "B cell / antibody",
  "PRDM1",     "B cell / antibody",
  "HLA-DRA",   "Antigen presentation",
  "CD74",      "Antigen presentation",
  "CIITA",     "Antigen presentation",
  "MARCHF1",   "Antigen presentation",
  "B2M",       "Antigen presentation",
  "TAP1",      "Antigen presentation",
  "SEPTIN7",   "Cytoskeleton",
  "MKI67",     "Proliferation",
  "ACTB",      "Housekeeping",
  "GAPDH",     "Housekeeping",
  "RPLP0",     "Housekeeping",
  "PPIA",      "Housekeeping",
  "TBP",       "Housekeeping",
  "HPRT1",     "Housekeeping",
  "GUSB",      "Housekeeping",
  "YWHAZ",     "Housekeeping"
) %>%
  mutate(gene_id = sprintf("GX%05d", seq(101, by = 37, length.out = n())))

n_genes <- nrow(genes)

# ---- Patients and samples ---------------------------------------------------
measles <- readRDS(here("processed_data", "measles_final.rds"))

patients <- measles %>%
  filter(vaccinated %in% c("Yes", "No"), outcome %in% c("Died", "Recovered")) %>%
  group_by(vaccinated, outcome) %>%
  # keep every death (rare) and a random set of the others
  slice_sample(prop = 1) %>%
  slice_head(n = 45) %>%
  ungroup() %>%
  slice_sample(prop = 1)   # shuffle

samples <- patients %>%
  transmute(
    patient_id,
    vaccinated,
    outcome,
    collection_date = report_date + sample(0:3, n(), replace = TRUE)
  ) %>%
  arrange(collection_date) %>%
  mutate(
    sample_id = sprintf("S%03d", row_number()),
    # first ~55% of samples in batch B1, the rest in B2
    batch = if_else(row_number() <= round(0.55 * n()), "B1", "B2")
  )

n_samples <- nrow(samples)

# ---- Counts -----------------------------------------------------------------
base_mean <- setNames(round(exp(runif(n_genes, log(40), log(4000)))), genes$symbol)
base_mean[genes$pathway == "Housekeeping"] <- round(exp(runif(8, log(3000), log(12000))))

effect <- matrix(1, n_genes, n_samples, dimnames = list(genes$symbol, samples$sample_id))

is_vax  <- samples$vaccinated == "Yes"
is_died <- samples$outcome == "Died"
is_b2   <- samples$batch == "B2"

ifn_genes  <- genes$symbol[genes$pathway == "Interferon response"][1:10]
ab_genes   <- c("JCHAIN", "MZB1", "IGHG1", "TNFRSF17", "PRDM1", "XBP1")
infl_genes <- c("IL6", "IL10", "CXCL8", "S100A8", "S100A9")
# batch-affected genes are kept separate from the biological signals
batch_genes <- sample(
  setdiff(genes$symbol[genes$pathway != "Housekeeping"], c(ifn_genes, ab_genes, infl_genes)),
  12
)

effect[ifn_genes, is_vax]    <- effect[ifn_genes, is_vax] * 0.35
effect[ab_genes, is_vax]     <- effect[ab_genes, is_vax] * 2.5
effect[infl_genes, is_died]  <- effect[infl_genes, is_died] * 3
effect[batch_genes, is_b2]   <- effect[batch_genes, is_b2] * 1.7

size_factor <- exp(rnorm(n_samples, 0, 0.3)) * if_else(is_b2, 1.6, 1)

mu <- base_mean * effect * rep(size_factor, each = n_genes)
counts <- matrix(
  rnbinom(length(mu), mu = mu, size = 8),
  nrow = n_genes,
  dimnames = dimnames(effect)
)

# One extra sample column with no metadata (sample failed QC, sheet row lost)
extra <- rnbinom(n_genes, mu = base_mean, size = 8)
counts <- cbind(counts, S999 = extra)

panel_out <- as_tibble(counts) %>%
  mutate(gene_id = genes$gene_id, .before = 1)

# ---- Messy sample sheet -----------------------------------------------------
sheet <- samples %>%
  transmute(
    `Sample ID`       = sample_id,
    `Patient ID`      = patient_id,
    `Collection.Date` = format(collection_date, "%Y-%m-%d"),
    BATCH             = batch
  )

idx <- sample(n_samples, 12)
sheet$`Sample ID`[idx[1:4]]  <- tolower(sheet$`Sample ID`[idx[1:4]])
sheet$`Sample ID`[idx[5:6]]  <- paste0(sheet$`Sample ID`[idx[5:6]], " ")
sheet$`Patient ID`[idx[7:12]] <- str_remove(sheet$`Patient ID`[idx[7:12]], "-")
b2_rows <- which(sheet$BATCH == "B2")
sheet$BATCH[sample(b2_rows, 5)] <- "b2"

# 3 samples from patients who are not in the case list
strangers <- tibble(
  `Sample ID`       = c("S901", "S902", "S903"),
  `Patient ID`      = c("MS-9001", "MS-9002", "MS-9003"),
  `Collection.Date` = c("2024-05-02", "2024-05-09", "2024-05-20"),
  BATCH             = c("B1", "B2", "B2")
)
strangers_counts <- matrix(
  rnbinom(n_genes * 3, mu = base_mean, size = 8),
  nrow = n_genes, dimnames = list(NULL, strangers$`Sample ID`)
)
panel_out <- bind_cols(panel_out, as_tibble(strangers_counts))

sheet <- bind_rows(sheet, strangers, sheet[sample(n_samples, 2), ]) %>%
  slice_sample(prop = 1)

# ---- Messy annotation -------------------------------------------------------
annotation <- genes %>%
  select(gene_id, symbol, pathway) %>%
  mutate(symbol = recode(symbol, "MARCHF1" = "1-Mar", "SEPTIN7" = "7-Sep"))
# MKI67 is in the matrix but missing from the annotation
annotation <- filter(annotation, symbol != "MKI67")

# ---- Write ------------------------------------------------------------------
write_csv(sheet,      here("raw_data", "measles_lab_samples.csv"))
write_csv(panel_out,  here("raw_data", "measles_immune_panel.csv"))
write_csv(annotation, here("raw_data", "gene_annotation.csv"))

message("Samples: ", n_samples, " (+3 unknown patients, +1 without metadata)")
message("Genes: ", n_genes)
message("Batch genes: ", paste(sort(batch_genes), collapse = ", "))

#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(vegan)
  library(permute)
  library(ggplot2)
  library(ggrepel)
  library(patchwork)
  library(knitr)
  library(kableExtra)
  library(scales)
  library(viridis)
})

set.seed(743)

root <- normalizePath(file.path("..", ".."), mustWork = TRUE)
out_dir <- normalizePath(".", mustWork = TRUE)
fig_dir <- file.path(out_dir, "figures")
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

env_path <- file.path(root, "data", "BCB743", "DoubsEnv.csv")
spe_path <- file.path(root, "data", "BCB743", "DoubsSpe.csv")
spa_path <- file.path(root, "data", "BCB743", "DoubsSpa.csv")

env_all <- read.csv(env_path, row.names = 1)
spe_all <- read.csv(spe_path, row.names = 1)
spa_all <- read.csv(spa_path, row.names = 1)

keep <- rowSums(spe_all) > 0
spe <- spe_all[keep, , drop = FALSE]
env <- env_all[keep, , drop = FALSE]
spa <- spa_all[keep, , drop = FALSE]
E <- as.data.frame(scale(env))

var_names <- c(
  dfs = "Distance from source",
  alt = "Altitude",
  slo = "Channel slope",
  flo = "Flow rate",
  pH = "pH",
  har = "Water hardness",
  pho = "Phosphate",
  nit = "Nitrate",
  amm = "Ammonium",
  oxy = "Dissolved oxygen",
  bod = "Biological oxygen demand"
)

species_names <- c(
  Cogo = "Bullhead",
  Satr = "Brown trout",
  Phph = "Minnow",
  Babl = "Stone loach",
  Thth = "Grayling",
  Teso = "Souffia / Western vairone",
  Chna = "Nase",
  Pato = "Stone moroko",
  Lele = "Sunbleak",
  Sqce = "Chub",
  Baba = "Barbel",
  Albi = "Spirlin",
  Gogo = "Gudgeon",
  Eslu = "Pike",
  Pefl = "Perch",
  Rham = "Bitterling",
  Legi = "Pumpkinseed",
  Scer = "Rudd",
  Cyca = "Crucian carp",
  Titi = "Tench",
  Abbr = "Common bream",
  Icme = "Ide",
  Gyce = "Ruffe",
  Ruru = "Roach",
  Blbj = "Silver bream",
  Alal = "Bleak",
  Anan = "Eel"
)

fmt <- function(x, digits = 2) {
  ifelse(is.na(x), "", formatC(x, digits = digits, format = "f"))
}

p_fmt <- function(x) {
  ifelse(is.na(x), "", ifelse(x < 0.001, "<0.001", fmt(x, 3)))
}

latex_escape <- function(x) {
  x <- as.character(x)
  x <- gsub("\\\\", "\\\\textbackslash{}", x)
  x <- gsub("([#$%&_{}])", "\\\\\\1", x, perl = TRUE)
  x <- gsub("~", "\\\\textasciitilde{}", x)
  x <- gsub("\\^", "\\\\textasciicircum{}", x)
  x
}

table_tex <- function(x, caption, label = NULL, font_size = 8, longtable = FALSE) {
  tab <- kable(
    x,
    format = "latex",
    booktabs = TRUE,
    longtable = longtable,
    escape = FALSE,
    caption = caption,
    label = label,
    row.names = FALSE,
    linesep = ""
  )
  tab <- kable_styling(
    tab,
    latex_options = c("hold_position", "scale_down"),
    font_size = font_size
  )
  paste(as.character(tab), collapse = "\n")
}

anova_table <- function(a, metric_name) {
  df <- as.data.frame(a)
  df$Term <- rownames(df)
  rownames(df) <- NULL
  stat_col <- intersect(c("F", "Pr(>F)"), names(df))
  p_col <- "Pr(>F)"
  value_col <- setdiff(names(df), c("Df", "F", p_col, "Term"))[1]
  out <- data.frame(
    Term = latex_escape(df$Term),
    Df = df$Df,
    Metric = metric_name,
    Value = fmt(df[[value_col]], 4),
    F = fmt(df$F, 2),
    p = p_fmt(df[[p_col]])
  )
  out
}

stepwise_table <- function(ord, model_name) {
  a <- as.data.frame(ord$anova)
  a$Step <- seq_len(nrow(a))
  a$Variable <- rownames(a)
  rownames(a) <- NULL
  a <- a[grepl("^\\+", a$Variable), , drop = FALSE]
  data.frame(
    Model = latex_escape(model_name),
    Step = a$Step,
    `Added variable` = paste0("\\texttt{", sub("^\\+\\s*", "", a$Variable), "}"),
    `Adjusted R2 after step` = fmt(a$R2.adj, 3),
    F = fmt(a$F, 2),
    p = p_fmt(a$`Pr(>F)`),
    check.names = FALSE
  )
}

vif_table <- function(v) {
  data.frame(
    Variable = paste0("\\texttt{", names(v), "}"),
    Description = latex_escape(unname(var_names[names(v)])),
    VIF = fmt(as.numeric(v), 2),
    check.names = FALSE
  )
}

eigen_table <- function(ord, axis_prefix, total = NULL, n = 6) {
  ev <- eigenvals(ord)
  if (is.null(total)) total <- sum(ev)
  data.frame(
    Axis = paste0(axis_prefix, seq_len(min(n, length(ev)))),
    Eigenvalue = fmt(ev[seq_len(min(n, length(ev)))], 4),
    Percent = fmt(100 * ev[seq_len(min(n, length(ev)))] / total, 2),
    Cumulative = fmt(cumsum(100 * ev / total)[seq_len(min(n, length(ev)))], 2)
  )
}

save_plot <- function(plot, filename, width = 7, height = 5) {
  ggsave(
    filename = file.path(fig_dir, filename),
    plot = plot,
    width = width,
    height = height,
    units = "in",
    device = grDevices::pdf,
    useDingbats = FALSE
  )
}

site_gradient <- as.numeric(rownames(env))

orient_to_dfs <- function(site_df, other_df = NULL, axis = "Axis1") {
  mult <- if (cor(site_df[[axis]], env$dfs, use = "complete.obs") < 0) -1 else 1
  site_df[[axis]] <- site_df[[axis]] * mult
  if (!is.null(other_df) && axis %in% names(other_df)) {
    other_df[[axis]] <- other_df[[axis]] * mult
  }
  list(site = site_df, other = other_df, mult = mult)
}

perm999 <- how(nperm = 999)

# Analyses --------------------------------------------------------------------

env_pca <- rda(env_all, scale = TRUE)
spe_ca <- cca(spe)
spe_dca <- decorana(spe)
spe_pcoa <- capscale(spe ~ 1, distance = "bray")
set.seed(743)
spe_nmds <- metaMDS(spe, distance = "bray", trace = 0, autotransform = FALSE, trymax = 100)

set.seed(743)
ca_envfit <- envfit(spe_ca, env, permutations = perm999)
set.seed(743)
nmds_envfit <- envfit(spe_nmds, env, permutations = perm999)

spe_hel <- decostand(spe, "hellinger")
rda_full <- rda(spe_hel ~ ., E)
rda0 <- rda(spe_hel ~ 1, E)
set.seed(743)
rda_step <- ordiR2step(rda0, scope = formula(rda_full), R2scope = TRUE, trace = FALSE, permutations = perm999)

cca_full <- cca(spe ~ ., E)
cca0 <- cca(spe ~ 1, E)
set.seed(743)
cca_step <- ordiR2step(cca0, scope = formula(cca_full), R2scope = TRUE, trace = FALSE, permutations = perm999)

dbrda_full <- capscale(spe ~ ., E, distance = "bray")
dbrda0 <- capscale(spe ~ 1, E, distance = "bray")
set.seed(743)
dbrda_step <- ordiR2step(dbrda0, scope = formula(dbrda_full), R2scope = TRUE, trace = FALSE, permutations = perm999)

set.seed(743)
rda_overall <- anova(rda_step, permutations = perm999)
set.seed(743)
rda_terms <- anova(rda_step, by = "term", permutations = perm999)
set.seed(743)
rda_axes <- anova(rda_step, by = "axis", permutations = perm999)
set.seed(743)
cca_overall <- anova(cca_step, permutations = perm999)
set.seed(743)
cca_terms <- anova(cca_step, by = "term", permutations = perm999)
set.seed(743)
cca_axes <- anova(cca_step, by = "axis", permutations = perm999)
set.seed(743)
dbrda_overall <- anova(dbrda_step, permutations = perm999)
set.seed(743)
dbrda_terms <- anova(dbrda_step, by = "term", permutations = perm999)
set.seed(743)
dbrda_axes <- anova(dbrda_step, by = "axis", permutations = perm999)

# Advanced analyses outside the core QE ordination sequence -------------------

E_dbrda <- E[, all.vars(formula(dbrda_step))[-1], drop = FALSE]
space_poly <- as.data.frame(scale(spa))
names(space_poly) <- c("X", "Y")
space_poly$X2 <- space_poly$X^2
space_poly$Y2 <- space_poly$Y^2
space_poly$XY <- space_poly$X * space_poly$Y

varpart_env_space <- varpart(spe_hel, E_dbrda, space_poly)
env_given_space <- rda(spe_hel, E_dbrda, space_poly)
space_given_env <- rda(spe_hel, space_poly, E_dbrda)
env_space_full <- rda(spe_hel, cbind(E_dbrda, space_poly))
set.seed(743)
env_given_space_test <- anova(env_given_space, permutations = perm999)
set.seed(743)
space_given_env_test <- anova(space_given_env, permutations = perm999)
set.seed(743)
env_space_full_test <- anova(env_space_full, permutations = perm999)

mem_available <- requireNamespace("adespatial", quietly = TRUE)
if (mem_available) {
  mem <- adespatial::dbmem(as.matrix(spa), silent = TRUE)
  mem_df <- as.data.frame(mem)
  mem_full <- rda(spe_hel ~ ., mem_df)
  mem0 <- rda(spe_hel ~ 1, mem_df)
  set.seed(743)
  mem_step <- ordiR2step(mem0, scope = formula(mem_full), R2scope = TRUE, trace = FALSE, permutations = perm999)
  set.seed(743)
  mem_overall <- anova(mem_step, permutations = perm999)
} else {
  mem <- NULL
  mem_df <- NULL
  mem_step <- NULL
  mem_overall <- NULL
}

env_pca_matched <- rda(env, scale = TRUE)
set.seed(743)
env_comm_protest <- protest(
  scores(env_pca_matched, display = "sites", choices = 1:2),
  scores(spe_nmds, display = "sites"),
  permutations = 999
)

mvabund_available <- requireNamespace("mvabund", quietly = TRUE)
if (mvabund_available) {
  Y_mv <- mvabund::mvabund(as.matrix(spe))
  manyglm_fit <- mvabund::manyglm(Y_mv ~ dfs + bod + oxy, data = E, family = "negative.binomial")
  set.seed(743)
  manyglm_anova <- anova(manyglm_fit, p.uni = "none", nBoot = 499, test = "LR")
} else {
  manyglm_fit <- NULL
  manyglm_anova <- NULL
}

# Further advanced analyses ---------------------------------------------------
# Theory-driven candidate models, matrix correlation, beta-diversity
# components, site/species contributions, and species-response shapes. All are
# placed after the seeded core analyses so they cannot alter the RNG stream that
# feeds the constrained-model permutation tests above.

# (a) Data-driven and theory-driven candidate constrained models (Hellinger RDA).
theory_models <- list(
  list(name = "Forward-selected", form = spe_hel ~ dfs + oxy + bod,
       rationale = "ordiR2step() data-driven selection"),
  list(name = "Position only", form = spe_hel ~ dfs,
       rationale = "Single longitudinal-position variable"),
  list(name = "Position + nitrate", form = spe_hel ~ dfs + nit,
       rationale = "Position plus the nutrient that tracks the continuum"),
  list(name = "Nutrient loading", form = spe_hel ~ alt + nit + amm,
       rationale = "Altitude plus dissolved inorganic nitrogen"),
  list(name = "Nutrient + phosphate", form = spe_hel ~ alt + nit + pho,
       rationale = "Altitude plus nitrogen and phosphorus loading")
)
theory_rows <- lapply(theory_models, function(m) {
  fit <- rda(m$form, data = E)
  vv <- tryCatch(vif.cca(fit), error = function(e) NA_real_)
  preds <- all.vars(m$form)[-1]
  data.frame(
    Model = m$name,
    Predictors = paste(sprintf("\\texttt{%s}", preds), collapse = ", "),
    Rationale = latex_escape(m$rationale),
    `Adjusted R2` = fmt(RsquareAdj(fit)$adj.r.squared, 3),
    `Max VIF` = fmt(max(vv, na.rm = TRUE), 2),
    check.names = FALSE
  )
})
theory_compare <- do.call(rbind, theory_rows)

# (b) Correlation among the enrichment / position variables.
enrich_vars <- c("dfs", "oxy", "bod", "nit", "amm", "pho")
enrich_cor_mat <- cor(env[, enrich_vars])
enrich_cor_chr <- matrix(
  fmt(as.numeric(enrich_cor_mat), 2),
  nrow = length(enrich_vars),
  dimnames = dimnames(enrich_cor_mat)
)
enrich_cor <- data.frame(
  Variable = paste0("\\texttt{", rownames(enrich_cor_mat), "}"),
  enrich_cor_chr,
  check.names = FALSE,
  row.names = NULL
)
names(enrich_cor) <- c("", paste0("\\texttt{", colnames(enrich_cor_mat), "}"))

# (c) Mantel and partial Mantel tests.
D_comm <- vegdist(spe, method = "bray")
D_env  <- dist(E)
D_geo  <- dist(spa)
set.seed(743); mantel_env   <- mantel(D_comm, D_env, permutations = 999)
set.seed(743); mantel_geo   <- mantel(D_comm, D_geo, permutations = 999)
set.seed(743); mantel_env_p <- mantel.partial(D_comm, D_env, D_geo, permutations = 999)
set.seed(743); mantel_geo_p <- mantel.partial(D_comm, D_geo, D_env, permutations = 999)
mantel_tab <- data.frame(
  Test = c(
    "Community vs environment",
    "Community vs geographic distance",
    "Community vs environment, geography held constant",
    "Community vs geography, environment held constant"
  ),
  `Mantel r` = fmt(c(mantel_env$statistic, mantel_geo$statistic,
                     mantel_env_p$statistic, mantel_geo_p$statistic), 3),
  p = p_fmt(c(mantel_env$signif, mantel_geo$signif,
              mantel_env_p$signif, mantel_geo_p$signif)),
  check.names = FALSE
)
mantel_tab$Test <- latex_escape(mantel_tab$Test)

# (d) Beta-diversity partitioning into turnover and abundance-gradient components.
betapart_available <- requireNamespace("betapart", quietly = TRUE)
if (betapart_available) {
  bpart <- betapart::beta.multi.abund(spe, index.family = "bray")
  betapart_tab <- data.frame(
    Component = c(
      "Total dissimilarity (beta.BRAY)",
      "Balanced turnover (species replacement)",
      "Abundance gradient (nestedness-like)"
    ),
    Value = fmt(c(bpart$beta.BRAY, bpart$beta.BRAY.BAL, bpart$beta.BRAY.GRA), 3),
    `Share of total` = fmt(c(1, bpart$beta.BRAY.BAL / bpart$beta.BRAY,
                             bpart$beta.BRAY.GRA / bpart$beta.BRAY), 3),
    check.names = FALSE
  )
  betapart_tab$Component <- latex_escape(betapart_tab$Component)
} else {
  bpart <- NULL
  betapart_tab <- data.frame(Component = "Not run: betapart unavailable",
                             Value = "", `Share of total` = "", check.names = FALSE)
}

# (e) Local and species contributions to beta diversity (Legendre & De Caceres).
set.seed(743)
bdiv <- adespatial::beta.div(spe, method = "hellinger", nperm = 999)
lcbd_df <- data.frame(
  site = as.integer(rownames(spe)),
  dfs = env$dfs,
  LCBD = as.numeric(bdiv$LCBD),
  p = as.numeric(bdiv$p.LCBD)
)
lcbd_df <- lcbd_df[order(lcbd_df$LCBD, decreasing = TRUE), ]
lcbd_tab <- data.frame(
  Site = lcbd_df$site[1:6],
  `Distance from source` = fmt(lcbd_df$dfs[1:6], 1),
  LCBD = fmt(lcbd_df$LCBD[1:6], 4),
  p = p_fmt(lcbd_df$p[1:6]),
  check.names = FALSE
)
scbd_sorted <- sort(bdiv$SCBD, decreasing = TRUE)
scbd_top <- head(scbd_sorted, 5)
scbd_top_str <- paste(
  sprintf("\\texttt{%s} (%s, %s)", names(scbd_top),
          latex_escape(unname(species_names[names(scbd_top)])), fmt(as.numeric(scbd_top), 3)),
  collapse = ", "
)

# (f) Species-response shapes along the gradient (negative-binomial GLM).
response_species <- c("Satr", "Cogo", "Thth", "Babl", "Baba", "Gogo", "Ruru", "Abbr")
dfs_raw <- env$dfs
dfs_lim <- range(dfs_raw)
fit_response <- function(s) {
  y <- spe[[s]]
  fit <- tryCatch(suppressWarnings(MASS::glm.nb(y ~ dfs_raw + I(dfs_raw^2))),
                  error = function(e) NULL)
  if (is.null(fit)) return(NULL)
  co <- coef(fit)
  b1 <- co[["dfs_raw"]]; b2 <- co[["I(dfs_raw^2)"]]
  p2 <- tryCatch(summary(fit)$coefficients["I(dfs_raw^2)", "Pr(>|z|)"],
                 error = function(e) NA_real_)
  peak <- if (!is.na(b2) && b2 < 0) -b1 / (2 * b2) else NA_real_
  in_range <- !is.na(peak) && peak >= dfs_lim[1] && peak <= dfs_lim[2]
  shape <- if (!is.na(b2) && b2 < 0 && !is.na(p2) && p2 < 0.05 && in_range) {
    "Unimodal, optimum within survey"
  } else if (!is.na(peak) && peak > dfs_lim[2]) {
    "Increasing, optimum below survey"
  } else if (!is.na(peak) && peak < dfs_lim[1]) {
    "Declining from the source"
  } else {
    "Monotonic within survey"
  }
  list(code = s, fit = fit, peak = peak, in_range = in_range, p2 = p2, shape = shape)
}
response_fits <- Filter(Negate(is.null), lapply(response_species, fit_response))
response_tab <- do.call(rbind, lapply(response_fits, function(r) {
  data.frame(
    Code = paste0("\\texttt{", r$code, "}"),
    Species = latex_escape(unname(species_names[r$code])),
    `Fitted optimum (km)` = if (r$in_range) fmt(r$peak, 0) else "beyond survey",
    `Quadratic p` = p_fmt(r$p2),
    Response = latex_escape(r$shape),
    check.names = FALSE
  )
}))

# Tables ----------------------------------------------------------------------

data_overview <- data.frame(
  Table = c("DoubsEnv.csv", "DoubsSpe.csv", "DoubsSpa.csv"),
  Role = c("Environmental predictors", "Fish abundance response", "Spatial coordinates"),
  Rows = c(nrow(env_all), nrow(spe_all), nrow(spa_all)),
  Columns = c(ncol(env_all), ncol(spe_all), ncol(spa_all)),
  Notes = c(
    "Used for PCA; matched subset used for direct gradient analyses",
    "Site 8 has zero total abundance and was removed for species ordinations",
    "Used for the site map"
  )
)
data_overview[] <- lapply(data_overview, latex_escape)

env_summary <- data.frame(
  Code = names(env_all),
  Variable = unname(var_names[names(env_all)]),
  Minimum = fmt(vapply(env_all, min, numeric(1))),
  Median = fmt(vapply(env_all, median, numeric(1))),
  Maximum = fmt(vapply(env_all, max, numeric(1))),
  `r with dfs` = fmt(vapply(env_all, function(z) cor(z, env_all$dfs), numeric(1)), 2),
  check.names = FALSE
)
env_summary$Code <- paste0("\\texttt{", env_summary$Code, "}")
env_summary$Variable <- latex_escape(env_summary$Variable)

pca_eig <- eigen_table(env_pca, "PC", n = 6)
pca_load <- as.data.frame(scores(env_pca, display = "species", choices = 1:2, scaling = 2))
pca_load$Code <- rownames(pca_load)
pca_load$Variable <- unname(var_names[pca_load$Code])
pca_load <- pca_load[order(abs(pca_load$PC1), decreasing = TRUE), c("Code", "Variable", "PC1", "PC2")]
pca_load$Code <- paste0("\\texttt{", pca_load$Code, "}")
pca_load$Variable <- latex_escape(pca_load$Variable)
pca_load$PC1 <- fmt(pca_load$PC1, 3)
pca_load$PC2 <- fmt(pca_load$PC2, 3)

ca_eig <- eigen_table(spe_ca, "CA", total = sum(spe_ca$CA$eig), n = 6)
pcoa_ev <- eigenvals(spe_pcoa)
pcoa_pos <- pcoa_ev[pcoa_ev > 0]
pcoa_eig <- data.frame(
  Axis = paste0("MDS", seq_len(6)),
  Eigenvalue = fmt(pcoa_pos[seq_len(6)], 4),
  Percent = fmt(100 * pcoa_pos[seq_len(6)] / sum(pcoa_pos), 2),
  Cumulative = fmt(cumsum(100 * pcoa_pos / sum(pcoa_pos))[seq_len(6)], 2)
)

method_summary <- data.frame(
  Method = c("PCA", "CA", "DCA", "PCoA", "nMDS", "RDA", "CCA", "db-RDA"),
  Response = c(
    "11 environmental variables",
    "27 fish abundance columns",
    "27 fish abundance columns",
    "Bray-Curtis dissimilarity of fish assemblages",
    "Ranks of Bray-Curtis dissimilarities",
    "Hellinger-transformed fish abundances",
    "Raw fish abundance table",
    "Bray-Curtis dissimilarity of fish assemblages"
  ),
  Key_output = c(
    paste0("PC1 = ", fmt(100 * eigenvals(env_pca)[1] / sum(eigenvals(env_pca)), 1), "\\%, PC2 = ", fmt(100 * eigenvals(env_pca)[2] / sum(eigenvals(env_pca)), 1), "\\%"),
    paste0("CA1 = ", fmt(100 * spe_ca$CA$eig[1] / sum(spe_ca$CA$eig), 1), "\\%, CA2 = ", fmt(100 * spe_ca$CA$eig[2] / sum(spe_ca$CA$eig), 1), "\\%"),
    paste0("DCA1 axis length = ", fmt(spe_dca$rproj[1], 2)),
    paste0("MDS1 = ", fmt(100 * pcoa_pos[1] / sum(pcoa_pos), 1), "\\% of positive inertia"),
    paste0("Stress = ", fmt(spe_nmds$stress, 3)),
    paste0("Selected: ", paste(all.vars(formula(rda_step))[-1], collapse = ", "), "; adj. $R^2$ = ", fmt(RsquareAdj(rda_step)$adj.r.squared, 2)),
    paste0("Selected: ", paste(all.vars(formula(cca_step))[-1], collapse = ", "), "; adj. $R^2$ = ", fmt(RsquareAdj(cca_step)$adj.r.squared, 2)),
    paste0("Selected: ", paste(all.vars(formula(dbrda_step))[-1], collapse = ", "), "; adj. $R^2$ = ", fmt(RsquareAdj(dbrda_step)$adj.r.squared, 2))
  ),
  Interpretation = c(
    "Environmental covariance forms a strong river-continuum axis.",
    "Species composition turns over mainly along the river continuum.",
    "Gradient length supports a long unimodal species gradient and explains the CA arch.",
    "Bray-Curtis distances recover the same faunal turnover.",
    "Rank-order composition has a faithful two-dimensional representation.",
    "Linear constrained model confirms environment-composition coupling.",
    "Unimodal constrained model confirms the same coupling.",
    "Distance-based constrained model gives the clearest community-distance test."
  )
)
method_summary[] <- lapply(method_summary, latex_escape)
method_summary$Key_output <- c(
  paste0("PC1 = ", fmt(100 * eigenvals(env_pca)[1] / sum(eigenvals(env_pca)), 1), "\\%, PC2 = ", fmt(100 * eigenvals(env_pca)[2] / sum(eigenvals(env_pca)), 1), "\\%"),
  paste0("CA1 = ", fmt(100 * spe_ca$CA$eig[1] / sum(spe_ca$CA$eig), 1), "\\%, CA2 = ", fmt(100 * spe_ca$CA$eig[2] / sum(spe_ca$CA$eig), 1), "\\%"),
  paste0("DCA1 axis length = ", fmt(spe_dca$rproj[1], 2)),
  paste0("MDS1 = ", fmt(100 * pcoa_pos[1] / sum(pcoa_pos), 1), "\\% of positive inertia"),
  paste0("Stress = ", fmt(spe_nmds$stress, 3)),
  paste0("Selected: ", paste(all.vars(formula(rda_step))[-1], collapse = ", "), "; adj. $R^2$ = ", fmt(RsquareAdj(rda_step)$adj.r.squared, 2)),
  paste0("Selected: ", paste(all.vars(formula(cca_step))[-1], collapse = ", "), "; adj. $R^2$ = ", fmt(RsquareAdj(cca_step)$adj.r.squared, 2)),
  paste0("Selected: ", paste(all.vars(formula(dbrda_step))[-1], collapse = ", "), "; adj. $R^2$ = ", fmt(RsquareAdj(dbrda_step)$adj.r.squared, 2))
)
names(method_summary) <- c("Method", "Response", "Key output", "Interpretation")

envfit_table <- function(fit) {
  arr <- as.data.frame(scores(fit, display = "vectors"))
  arr$Code <- rownames(arr)
  arr$r2 <- fit$vectors$r
  arr$p <- fit$vectors$pvals
  out <- data.frame(
    Code = paste0("\\texttt{", arr$Code, "}"),
    Variable = latex_escape(unname(var_names[arr$Code])),
    Axis1 = fmt(arr[, 1], 3),
    Axis2 = fmt(arr[, 2], 3),
    r2 = fmt(arr$r2, 3),
    p = p_fmt(arr$p)
  )
  out[order(as.numeric(out$p), -as.numeric(out$r2)), ]
}

ca_envfit_tab <- envfit_table(ca_envfit)
nmds_envfit_tab <- envfit_table(nmds_envfit)

model_summary <- data.frame(
  Model = c("RDA", "CCA", "db-RDA"),
  Selected_predictors = c(
    paste(all.vars(formula(rda_step))[-1], collapse = ", "),
    paste(all.vars(formula(cca_step))[-1], collapse = ", "),
    paste(all.vars(formula(dbrda_step))[-1], collapse = ", ")
  ),
  Adjusted_R2 = fmt(c(
    RsquareAdj(rda_step)$adj.r.squared,
    RsquareAdj(cca_step)$adj.r.squared,
    RsquareAdj(dbrda_step)$adj.r.squared
  ), 3),
  Overall_F = fmt(c(rda_overall$F[1], cca_overall$F[1], dbrda_overall$F[1]), 2),
  Overall_p = p_fmt(c(rda_overall$`Pr(>F)`[1], cca_overall$`Pr(>F)`[1], dbrda_overall$`Pr(>F)`[1]))
)
model_summary$Model <- latex_escape(model_summary$Model)
format_vars <- function(x) {
  paste(sprintf("\\texttt{%s}", strsplit(x, ", ", fixed = TRUE)[[1]]), collapse = ", ")
}
model_summary$Selected_predictors <- vapply(model_summary$Selected_predictors, format_vars, character(1))
names(model_summary) <- c("Model", "Selected predictors", "Adjusted $R^2$", "Overall F", "Overall p")

constrained_terms <- rbind(
  cbind(Model = "RDA", anova_table(rda_terms, "Variance")),
  cbind(Model = "CCA", anova_table(cca_terms, "Chi-square")),
  cbind(Model = "db-RDA", anova_table(dbrda_terms, "Sum of squares"))
)
constrained_terms$Model <- latex_escape(constrained_terms$Model)
constrained_terms <- constrained_terms[!constrained_terms$Term %in% c("Residual"), ]
constrained_terms[is.na(constrained_terms)] <- ""

axis_tests <- rbind(
  cbind(Model = "RDA", anova_table(rda_axes, "Variance")),
  cbind(Model = "CCA", anova_table(cca_axes, "Chi-square")),
  cbind(Model = "db-RDA", anova_table(dbrda_axes, "Sum of squares"))
)
axis_tests$Model <- latex_escape(axis_tests$Model)
axis_tests <- axis_tests[!axis_tests$Term %in% c("Residual"), ]
axis_tests[is.na(axis_tests)] <- ""

full_vif_tab <- rbind(
  cbind(Model = "RDA", vif_table(vif.cca(rda_full))),
  cbind(Model = "CCA", vif_table(vif.cca(cca_full))),
  cbind(Model = "db-RDA", vif_table(vif.cca(dbrda_full)))
)
full_vif_tab$Model <- latex_escape(full_vif_tab$Model)
full_vif_tab <- full_vif_tab[order(full_vif_tab$Model, -as.numeric(full_vif_tab$VIF)), ]

selected_vif_tab <- rbind(
  cbind(Model = "RDA", vif_table(vif.cca(rda_step))),
  cbind(Model = "CCA", vif_table(vif.cca(cca_step))),
  cbind(Model = "db-RDA", vif_table(vif.cca(dbrda_step)))
)
selected_vif_tab$Model <- latex_escape(selected_vif_tab$Model)
selected_vif_tab <- selected_vif_tab[order(selected_vif_tab$Model, -as.numeric(selected_vif_tab$VIF)), ]

stepwise_summary <- rbind(
  stepwise_table(rda_step, "RDA"),
  stepwise_table(cca_step, "CCA"),
  stepwise_table(dbrda_step, "db-RDA")
)

vp_frac <- varpart_env_space$part$indfract
variation_partition <- data.frame(
  Fraction = c(
    "Environment only",
    "Space only",
    "Shared environment-space",
    "Residual"
  ),
  Interpretation = c(
    "Selected environmental predictors after conditioning on polynomial space",
    "Spatial trend after conditioning on selected environmental predictors",
    "Variation jointly structured by measured environment and spatial position",
    "Unexplained variation"
  ),
  `Adjusted R2` = fmt(c(
    vp_frac["[a] = X1|X2", "Adj.R.squared"],
    vp_frac["[b] = X2|X1", "Adj.R.squared"],
    vp_frac["[c]", "Adj.R.squared"],
    vp_frac["[d] = Residuals", "Adj.R.squared"]
  ), 3),
  p = c(
    p_fmt(env_given_space_test$`Pr(>F)`[1]),
    p_fmt(space_given_env_test$`Pr(>F)`[1]),
    "",
    ""
  ),
  check.names = FALSE
)
variation_partition$Fraction <- latex_escape(variation_partition$Fraction)
variation_partition$Interpretation <- latex_escape(variation_partition$Interpretation)

advanced_summary <- data.frame(
  Analysis = c(
    "Variation partitioning",
    "MEM spatial eigenvectors",
    "Procrustes comparison",
    "Negative-binomial manyGLM"
  ),
  Purpose = c(
    "Separate pure environmental, pure spatial, and shared spatial-environmental fractions.",
    "Model spatially structured community variation using Moran eigenvector maps.",
    "Compare environmental PCA geometry with fish-community nMDS geometry.",
    "Test species abundances with a model-based multivariate count framework."
  ),
  Result = c(
    paste0(
      "Environment only adj. $R^2$ = ",
      fmt(vp_frac["[a] = X1|X2", "Adj.R.squared"], 3),
      "; space only adj. $R^2$ = ",
      fmt(vp_frac["[b] = X2|X1", "Adj.R.squared"], 3)
    ),
    if (mem_available) {
      paste0(
        paste(all.vars(formula(mem_step))[-1], collapse = ", "),
        "; adj. $R^2$ = ",
        fmt(RsquareAdj(mem_step)$adj.r.squared, 3),
        "; p = ",
        p_fmt(mem_overall$`Pr(>F)`[1])
      )
    } else {
      "Not run: adespatial unavailable"
    },
    paste0(
      "Procrustes r = ",
      fmt(env_comm_protest$t0, 3),
      "; p = ",
      p_fmt(env_comm_protest$signif)
    ),
    if (mvabund_available) {
      paste0(
        "dfs, bod, and oxy all p = ",
        paste(p_fmt(manyglm_anova$table$`Pr(>Dev)`[-1]), collapse = ", ")
      )
    } else {
      "Not run: mvabund unavailable"
    }
  ),
  check.names = FALSE
)
advanced_summary$Analysis <- latex_escape(advanced_summary$Analysis)
advanced_summary$Purpose <- latex_escape(advanced_summary$Purpose)
advanced_summary$Result <- c(
  paste0(
    "Environment only adj. $R^2$ = ",
    fmt(vp_frac["[a] = X1|X2", "Adj.R.squared"], 3),
    "; space only adj. $R^2$ = ",
    fmt(vp_frac["[b] = X2|X1", "Adj.R.squared"], 3)
  ),
  if (mem_available) {
    paste0(
      paste(all.vars(formula(mem_step))[-1], collapse = ", "),
      "; adj. $R^2$ = ",
      fmt(RsquareAdj(mem_step)$adj.r.squared, 3),
      "; p = ",
      p_fmt(mem_overall$`Pr(>F)`[1])
    )
  } else {
    "Not run: adespatial unavailable"
  },
  paste0(
    "Procrustes r = ",
    fmt(env_comm_protest$t0, 3),
    "; p = ",
    p_fmt(env_comm_protest$signif)
  ),
  if (mvabund_available) {
    paste0(
      "dfs, bod, and oxy p = ",
      paste(p_fmt(manyglm_anova$table$`Pr(>Dev)`[-1]), collapse = ", ")
    )
  } else {
    "Not run: mvabund unavailable"
  }
)

adv_extra <- data.frame(
  Analysis = latex_escape(c(
    "Mantel and partial Mantel",
    "Beta-diversity partitioning",
    "LCBD and SCBD",
    "Species-response GLMs"
  )),
  Purpose = latex_escape(c(
    "Correlate the fish, environmental, and geographic distance matrices directly.",
    "Split total dissimilarity into species replacement and abundance-gradient parts.",
    "Rank sites and species by their contribution to total beta diversity.",
    "Fit abundance responses to distance from source for individual species."
  )),
  Result = c(
    paste0("partial env $|$ space r = ", fmt(mantel_env_p$statistic, 3),
           "; partial space $|$ env r = ", fmt(mantel_geo_p$statistic, 3)),
    if (betapart_available) {
      paste0("turnover = ", fmt(bpart$beta.BRAY.BAL / bpart$beta.BRAY, 2),
             " of total; gradient = ", fmt(bpart$beta.BRAY.GRA / bpart$beta.BRAY, 2))
    } else {
      "Not run: betapart unavailable"
    },
    paste0(sum(bdiv$p.LCBD <= 0.05, na.rm = TRUE),
           " sites at p $\\leq$ 0.05, incl. headwater and pollution reach"),
    paste0(sum(vapply(response_fits, function(r) grepl("^Unimodal", r$shape), logical(1))),
           " of ", length(response_fits), " species unimodal within survey")
  ),
  check.names = FALSE
)
advanced_summary <- rbind(advanced_summary, adv_extra)

manyglm_tab <- if (mvabund_available) {
  mg <- manyglm_anova$table
  mg$Term <- rownames(mg)
  mg <- mg[mg$Term != "(Intercept)", , drop = FALSE]
  data.frame(
    Term = paste0("\\texttt{", mg$Term, "}"),
    `Residual df` = mg$Res.Df,
    `Df diff` = mg$Df.diff,
    Deviance = fmt(mg$Dev, 1),
    p = p_fmt(mg$`Pr(>Dev)`),
    check.names = FALSE
  )
} else {
  data.frame(
    Term = "Not run",
    `Residual df` = "",
    `Df diff` = "",
    Deviance = "",
    p = "",
    check.names = FALSE
  )
}

axis_scores <- function(ord, display, choices = 1:2, names_out = c("Axis1", "Axis2")) {
  out <- as.data.frame(scores(ord, display = display, choices = choices))
  names(out)[1:2] <- names_out
  out
}

pca_sites_grad <- as.data.frame(scores(env_pca, display = "sites", choices = 1:2, scaling = 2))
pca_sites_grad$dfs <- env_all$dfs
ca_sites_grad <- axis_scores(spe_ca, "sites")
ca_sites_grad$dfs <- env$dfs
pcoa_sites_grad <- as.data.frame(scores(spe_pcoa, display = "sites", choices = 1:2))
pcoa_sites_grad$dfs <- env$dfs
nmds_sites_grad <- as.data.frame(scores(spe_nmds, display = "sites"))
nmds_sites_grad$dfs <- env$dfs

gradient_alignment <- data.frame(
  Ordination = c("PCA PC1", "CA CA1", "PCoA MDS1", "nMDS NMDS1"),
  `Correlation with distance from source` = fmt(c(
    cor(pca_sites_grad$PC1, pca_sites_grad$dfs),
    abs(cor(ca_sites_grad$Axis1, ca_sites_grad$dfs)),
    abs(cor(pcoa_sites_grad$MDS1, pcoa_sites_grad$dfs)),
    abs(cor(nmds_sites_grad$NMDS1, nmds_sites_grad$dfs))
  ), 3),
  check.names = FALSE
)
gradient_alignment$Ordination <- latex_escape(gradient_alignment$Ordination)

# Figures ---------------------------------------------------------------------

theme_set(theme_bw(base_size = 9))

map_df <- data.frame(site = as.integer(rownames(spa_all)), spa_all, dfs = env_all$dfs)
fig_map <- ggplot(map_df, aes(X, Y)) +
  geom_path(colour = "grey75", linewidth = 0.4) +
  geom_point(aes(colour = dfs), size = 2.5) +
  geom_text_repel(aes(label = site), size = 2.4, max.overlaps = Inf, segment.colour = "grey80") +
  scale_colour_viridis_c(name = "Distance\nfrom source") +
  labs(x = "X coordinate", y = "Y coordinate") +
  coord_equal()
save_plot(fig_map, "fig01_doubs_sites.pdf", 6, 4.5)

pca_sites <- as.data.frame(scores(env_pca, display = "sites", choices = 1:2, scaling = 2))
pca_sites$site <- as.integer(rownames(pca_sites))
pca_sites$dfs <- env_all$dfs
pca_vars <- as.data.frame(scores(env_pca, display = "species", choices = 1:2, scaling = 2))
pca_vars$code <- rownames(pca_vars)
pca_vars$label <- var_names[pca_vars$code]
arrow_mult <- 0.85 * min(
  diff(range(pca_sites$PC1)) / diff(range(pca_vars$PC1)),
  diff(range(pca_sites$PC2)) / diff(range(pca_vars$PC2))
)
pca_vars$PC1 <- pca_vars$PC1 * arrow_mult
pca_vars$PC2 <- pca_vars$PC2 * arrow_mult
fig_pca <- ggplot(pca_sites, aes(PC1, PC2)) +
  geom_hline(yintercept = 0, colour = "grey85") +
  geom_vline(xintercept = 0, colour = "grey85") +
  geom_point(aes(colour = dfs), size = 2.2) +
  geom_text_repel(aes(label = site), size = 2.3, max.overlaps = Inf, segment.colour = "grey85") +
  geom_segment(data = pca_vars, aes(x = 0, y = 0, xend = PC1, yend = PC2),
               inherit.aes = FALSE, arrow = arrow(length = unit(2, "mm")),
               colour = "firebrick", linewidth = 0.45) +
  geom_text_repel(data = pca_vars, aes(PC1, PC2, label = code),
                  inherit.aes = FALSE, colour = "firebrick", size = 2.5,
                  max.overlaps = Inf, segment.colour = "grey80") +
  scale_colour_viridis_c(name = "Distance\nfrom source") +
  labs(
    x = paste0("PC1 (", fmt(100 * eigenvals(env_pca)[1] / sum(eigenvals(env_pca)), 1), "%)"),
    y = paste0("PC2 (", fmt(100 * eigenvals(env_pca)[2] / sum(eigenvals(env_pca)), 1), "%)")
  ) +
  coord_equal()
save_plot(fig_pca, "fig02_environmental_pca.pdf", 6.5, 5)

ca_sites <- axis_scores(spe_ca, "sites")
ca_sites$site <- as.integer(rownames(ca_sites))
ca_sites$dfs <- env$dfs
ca_spp <- axis_scores(spe_ca, "species")
ca_spp$code <- rownames(ca_spp)
ca_or <- orient_to_dfs(ca_sites, ca_spp, "Axis1")
ca_sites <- ca_or$site
ca_spp <- ca_or$other
ca_spp$dist <- sqrt(ca_spp$Axis1^2 + ca_spp$Axis2^2)
ca_lab <- ca_spp[ca_spp$dist > quantile(ca_spp$dist, 0.45), ]
ca_pct <- 100 * spe_ca$CA$eig / sum(spe_ca$CA$eig)
fig_ca <- ggplot(ca_sites, aes(Axis1, Axis2)) +
  geom_hline(yintercept = 0, colour = "grey85") +
  geom_vline(xintercept = 0, colour = "grey85") +
  geom_path(data = ca_sites[order(ca_sites$site), ], colour = "grey75", linewidth = 0.4) +
  geom_point(aes(colour = dfs), size = 2.2) +
  geom_point(data = ca_spp, aes(Axis1, Axis2), inherit.aes = FALSE,
             shape = 3, colour = "seagreen4", size = 1) +
  geom_text_repel(data = ca_lab, aes(Axis1, Axis2, label = code),
                  inherit.aes = FALSE, colour = "seagreen4", size = 2.4,
                  max.overlaps = Inf, segment.colour = "grey80") +
  scale_colour_viridis_c(name = "Distance\nfrom source") +
  labs(x = paste0("CA1 (", fmt(ca_pct[1], 1), "%)"), y = paste0("CA2 (", fmt(ca_pct[2], 1), "%)")) +
  coord_equal()
save_plot(fig_ca, "fig03_species_ca.pdf", 6.5, 5)

pcoa_sites <- as.data.frame(scores(spe_pcoa, display = "sites", choices = 1:2))
names(pcoa_sites)[1:2] <- c("Axis1", "Axis2")
pcoa_sites$site <- as.integer(rownames(pcoa_sites))
pcoa_sites$dfs <- env$dfs
pcoa_spp <- as.data.frame(scores(spe_pcoa, display = "species", choices = 1:2))
names(pcoa_spp)[1:2] <- c("Axis1", "Axis2")
pcoa_spp$code <- rownames(pcoa_spp)
pcoa_or <- orient_to_dfs(pcoa_sites, pcoa_spp, "Axis1")
pcoa_sites <- pcoa_or$site
pcoa_spp <- pcoa_or$other
pcoa_spp$dist <- sqrt(pcoa_spp$Axis1^2 + pcoa_spp$Axis2^2)
pcoa_lab <- pcoa_spp[pcoa_spp$dist > quantile(pcoa_spp$dist, 0.45), ]

nmds_sites <- as.data.frame(scores(spe_nmds, display = "sites"))
names(nmds_sites)[1:2] <- c("Axis1", "Axis2")
nmds_sites$site <- as.integer(rownames(nmds_sites))
nmds_sites$dfs <- env$dfs
nmds_spp <- as.data.frame(scores(spe_nmds, display = "species"))
names(nmds_spp)[1:2] <- c("Axis1", "Axis2")
nmds_spp$code <- rownames(nmds_spp)
nmds_or <- orient_to_dfs(nmds_sites, nmds_spp, "Axis1")
nmds_sites <- nmds_or$site
nmds_spp <- nmds_or$other
nmds_spp$dist <- sqrt(nmds_spp$Axis1^2 + nmds_spp$Axis2^2)
nmds_lab <- nmds_spp[nmds_spp$dist > quantile(nmds_spp$dist, 0.45), ]

fig_pcoa <- ggplot(pcoa_sites, aes(Axis1, Axis2)) +
  geom_hline(yintercept = 0, colour = "grey85") +
  geom_vline(xintercept = 0, colour = "grey85") +
  geom_path(data = pcoa_sites[order(pcoa_sites$site), ], colour = "grey75", linewidth = 0.4) +
  geom_point(aes(colour = dfs), size = 2) +
  geom_point(data = pcoa_spp, aes(Axis1, Axis2), inherit.aes = FALSE, shape = 3,
             colour = "seagreen4", size = 0.9) +
  geom_text_repel(data = pcoa_lab, aes(Axis1, Axis2, label = code),
                  inherit.aes = FALSE, colour = "seagreen4", size = 2.3,
                  max.overlaps = Inf, segment.colour = "grey80") +
  scale_colour_viridis_c(name = "Distance\nfrom source") +
  labs(
    title = "PCoA, Bray-Curtis",
    x = paste0("MDS1 (", fmt(100 * pcoa_pos[1] / sum(pcoa_pos), 1), "%)"),
    y = paste0("MDS2 (", fmt(100 * pcoa_pos[2] / sum(pcoa_pos), 1), "%)")
  ) +
  coord_equal()

fig_nmds <- ggplot(nmds_sites, aes(Axis1, Axis2)) +
  geom_hline(yintercept = 0, colour = "grey85") +
  geom_vline(xintercept = 0, colour = "grey85") +
  geom_path(data = nmds_sites[order(nmds_sites$site), ], colour = "grey75", linewidth = 0.4) +
  geom_point(aes(colour = dfs), size = 2) +
  geom_point(data = nmds_spp, aes(Axis1, Axis2), inherit.aes = FALSE, shape = 3,
             colour = "seagreen4", size = 0.9) +
  geom_text_repel(data = nmds_lab, aes(Axis1, Axis2, label = code),
                  inherit.aes = FALSE, colour = "seagreen4", size = 2.3,
                  max.overlaps = Inf, segment.colour = "grey80") +
  scale_colour_viridis_c(name = "Distance\nfrom source") +
  labs(title = paste0("nMDS, stress = ", fmt(spe_nmds$stress, 3)), x = "NMDS1", y = "NMDS2") +
  coord_equal()
save_plot(fig_pcoa + fig_nmds + plot_layout(guides = "collect"), "fig04_pcoa_nmds.pdf", 12, 5)

dca_sites <- as.data.frame(scores(spe_dca, display = "sites", choices = 1:2))
names(dca_sites)[1:2] <- c("Axis1", "Axis2")
dca_sites$site <- as.integer(rownames(dca_sites))
dca_sites$dfs <- env$dfs
dca_or <- orient_to_dfs(dca_sites, NULL, "Axis1")
dca_sites <- dca_or$site
fig_dca <- ggplot(dca_sites, aes(Axis1, Axis2)) +
  geom_hline(yintercept = 0, colour = "grey85") +
  geom_vline(xintercept = 0, colour = "grey85") +
  geom_path(data = dca_sites[order(dca_sites$site), ], colour = "grey70", linewidth = 0.4) +
  geom_point(aes(colour = dfs), size = 2.3) +
  geom_text_repel(aes(label = site), size = 2.4, max.overlaps = Inf, segment.colour = "grey80") +
  scale_colour_viridis_c(name = "Distance\nfrom source") +
  labs(x = "DCA1", y = "DCA2") +
  coord_equal()
save_plot(fig_dca, "fig05_dca_gradient.pdf", 6.5, 5)

envfit_vec <- function(fit, ord_sites, mult_axis1 = 1) {
  vec <- as.data.frame(scores(fit, display = "vectors"))
  names(vec)[1:2] <- c("Axis1", "Axis2")
  vec$Axis1 <- vec$Axis1 * mult_axis1
  vec$code <- rownames(vec)
  vec$p <- fit$vectors$pvals
  scale_mult <- 0.8 * min(
    diff(range(ord_sites$Axis1)) / diff(range(vec$Axis1)),
    diff(range(ord_sites$Axis2)) / diff(range(vec$Axis2))
  )
  vec$Axis1 <- vec$Axis1 * scale_mult
  vec$Axis2 <- vec$Axis2 * scale_mult
  vec
}
ca_vec <- envfit_vec(ca_envfit, ca_sites, ca_or$mult)
fig_envfit <- ggplot(ca_sites, aes(Axis1, Axis2)) +
  geom_hline(yintercept = 0, colour = "grey85") +
  geom_vline(xintercept = 0, colour = "grey85") +
  geom_point(colour = "grey35", size = 2) +
  geom_segment(data = ca_vec, aes(x = 0, y = 0, xend = Axis1, yend = Axis2, colour = p <= 0.05),
               inherit.aes = FALSE, arrow = arrow(length = unit(2.2, "mm")), linewidth = 0.5) +
  geom_text_repel(data = ca_vec, aes(Axis1, Axis2, label = code, colour = p <= 0.05),
                  inherit.aes = FALSE, size = 2.7, max.overlaps = Inf, segment.colour = "grey80") +
  scale_colour_manual(values = c("TRUE" = "firebrick", "FALSE" = "grey45"), name = "p <= 0.05") +
  labs(x = paste0("CA1 (", fmt(ca_pct[1], 1), "%)"), y = paste0("CA2 (", fmt(ca_pct[2], 1), "%)")) +
  coord_equal()
save_plot(fig_envfit, "fig06_ca_envfit.pdf", 6.5, 5)

constrained_plot <- function(ord, title, xlab = "Axis 1", ylab = "Axis 2") {
  sites <- as.data.frame(scores(ord, display = "sites", choices = 1:2))
  names(sites)[1:2] <- c("Axis1", "Axis2")
  sites$site <- as.integer(rownames(sites))
  sites$dfs <- env$dfs
  spp <- tryCatch(as.data.frame(scores(ord, display = "species", choices = 1:2)), error = function(e) NULL)
  if (!is.null(spp) && ncol(spp) >= 2) {
    names(spp)[1:2] <- c("Axis1", "Axis2")
    spp$code <- rownames(spp)
    or <- orient_to_dfs(sites, spp, "Axis1")
    sites <- or$site
    spp <- or$other
    spp$dist <- sqrt(spp$Axis1^2 + spp$Axis2^2)
    spp <- spp[spp$dist > quantile(spp$dist, 0.65), ]
  } else {
    or <- orient_to_dfs(sites, NULL, "Axis1")
    sites <- or$site
  }
  g <- ggplot(sites, aes(Axis1, Axis2)) +
    geom_hline(yintercept = 0, colour = "grey85") +
    geom_vline(xintercept = 0, colour = "grey85") +
    geom_path(data = sites[order(sites$site), ], colour = "grey75", linewidth = 0.35) +
    geom_point(aes(colour = dfs), size = 2) +
    scale_colour_viridis_c(name = "Distance\nfrom source") +
    labs(title = title, x = xlab, y = ylab) +
    coord_equal()
  if (!is.null(spp) && nrow(spp) > 0) {
    g <- g +
      geom_point(data = spp, aes(Axis1, Axis2), inherit.aes = FALSE, shape = 3,
                 colour = "seagreen4", size = 0.8) +
      geom_text_repel(data = spp, aes(Axis1, Axis2, label = code),
                      inherit.aes = FALSE, colour = "seagreen4", size = 2,
                      max.overlaps = Inf, segment.colour = "grey80")
  }
  g
}

fig_rda <- constrained_plot(rda_step, "RDA")
fig_cca <- constrained_plot(cca_step, "CCA")
fig_dbrda <- constrained_plot(dbrda_step, "db-RDA")
save_plot(fig_rda + fig_cca + fig_dbrda + plot_layout(guides = "collect"), "fig07_constrained_ordination.pdf", 13, 4.5)

focus_species <- c("Cogo", "Satr", "Phph", "Babl", "Thth", "Baba", "Abbr", "Blbj", "Gyce", "Ruru", "Alal", "Anan")
heat <- as.data.frame(spe[, focus_species])
heat$site <- as.integer(rownames(heat))
heat$dfs <- env$dfs
heat <- reshape(
  heat,
  varying = focus_species,
  v.names = "abundance",
  timevar = "code",
  times = focus_species,
  direction = "long"
)
heat$label <- paste0(heat$code, " (", species_names[heat$code], ")")
heat$label <- factor(heat$label, levels = rev(paste0(focus_species, " (", species_names[focus_species], ")")))
fig_heat <- ggplot(heat, aes(dfs, label, fill = abundance)) +
  geom_tile(colour = "white", linewidth = 0.2) +
  scale_fill_viridis_c(name = "Abundance", option = "magma") +
  labs(x = "Distance from source", y = NULL) +
  theme(panel.grid = element_blank())
save_plot(fig_heat, "fig08_species_gradient_heatmap.pdf", 7.5, 4.8)

# fig09: fitted negative-binomial species-response curves along the gradient.
resp_grid <- seq(dfs_lim[1], dfs_lim[2], length.out = 200)
resp_pred <- do.call(rbind, lapply(response_fits, function(r) {
  pr <- predict(r$fit, newdata = data.frame(dfs_raw = resp_grid), type = "response")
  data.frame(code = r$code, dfs = resp_grid, fit = as.numeric(pr))
}))
resp_obs <- do.call(rbind, lapply(response_fits, function(r) {
  data.frame(code = r$code, dfs = dfs_raw, abundance = spe[[r$code]])
}))
resp_levels <- response_species
resp_lab <- setNames(
  paste0(resp_levels, " (", species_names[resp_levels], ")"),
  resp_levels
)
resp_pred$label <- factor(resp_lab[resp_pred$code], levels = resp_lab[resp_levels])
resp_obs$label  <- factor(resp_lab[resp_obs$code], levels = resp_lab[resp_levels])
fig_response <- ggplot(resp_pred, aes(dfs, fit)) +
  geom_point(data = resp_obs, aes(dfs, abundance), colour = "grey55", size = 1) +
  geom_line(colour = "firebrick", linewidth = 0.6) +
  facet_wrap(~ label, scales = "free_y", ncol = 4) +
  labs(x = "Distance from source", y = "Fitted abundance") +
  theme(strip.text = element_text(size = 7))
save_plot(fig_response, "fig09_species_response.pdf", 9, 4.6)

# fig10: local contributions to beta diversity and richness along the gradient.
lcbd_plot <- data.frame(
  site = as.integer(rownames(spe)),
  dfs = env$dfs,
  LCBD = as.numeric(bdiv$LCBD),
  p = as.numeric(bdiv$p.LCBD),
  richness = rowSums(spe > 0)
)
lcbd_plot$signif <- lcbd_plot$p <= 0.05
fig_lcbd_a <- ggplot(lcbd_plot, aes(dfs, LCBD)) +
  geom_line(colour = "grey75", linewidth = 0.4) +
  geom_point(aes(colour = signif), size = 2) +
  geom_text_repel(data = subset(lcbd_plot, signif), aes(label = site),
                  size = 2.4, max.overlaps = Inf, segment.colour = "grey80") +
  scale_colour_manual(values = c("FALSE" = "grey55", "TRUE" = "firebrick"),
                      name = "p <= 0.05") +
  labs(title = "Local contribution to beta diversity", x = "Distance from source", y = "LCBD")
fig_lcbd_b <- ggplot(lcbd_plot, aes(dfs, richness)) +
  geom_line(colour = "grey75", linewidth = 0.4) +
  geom_point(colour = "steelblue4", size = 2) +
  geom_text_repel(data = subset(lcbd_plot, richness <= 4), aes(label = site),
                  size = 2.4, max.overlaps = Inf, segment.colour = "grey80") +
  labs(title = "Species richness", x = "Distance from source", y = "Number of species")
save_plot(fig_lcbd_a + fig_lcbd_b + plot_layout(guides = "collect"),
          "fig10_lcbd_richness.pdf", 10, 4)

# LaTeX report ----------------------------------------------------------------

tex <- c(
  "\\documentclass[11pt]{article}",
  "\\usepackage[margin=1in]{geometry}",
  "\\usepackage{graphicx}",
  "\\usepackage{booktabs}",
  "\\usepackage{longtable}",
  "\\usepackage{array}",
  "\\usepackage{float}",
  "\\usepackage{caption}",
  "\\usepackage{subcaption}",
  "\\usepackage{xcolor}",
  "\\usepackage{hyperref}",
  "\\usepackage{amsmath}",
  "\\usepackage{placeins}",
  "\\hypersetup{colorlinks=true,linkcolor=blue!50!black,urlcolor=blue!50!black,citecolor=blue!50!black}",
  "\\graphicspath{{figures/}}",
  "\\title{Multivariate Analysis of the Doubs River Fish-Environment System}",
  "\\author{Generated from the BCB743 Quantitative Ecology data}",
  paste0("\\date{", format(Sys.Date(), "%d %B %Y"), "}"),
  "\\begin{document}",
  "\\maketitle",
  "\\begin{abstract}",
  paste(
    "The Doubs River fish and environmental data were analysed with the main multivariate methods of the Quantitative Ecology chapters, namely PCA, CA, DCA, PCoA, nMDS, RDA, CCA, and db-RDA, and then extended with variable-selection, spatial, beta-diversity, and species-response analyses.",
    "The methods agree on one dominant structure, a longitudinal river-continuum gradient from cool, steep, oxygen-rich headwaters to lower, slower, nutrient- and organic-load-enriched reaches, and fish composition follows it as an ordered replacement of trout-zone species by lowland cyprinids.",
    "Two refinements emerge from the extended analyses.",
    "A short mid-river reach departs from the continuum as an organic-pollution oxygen sag that collapses local richness and keeps oxygen and biological oxygen demand in the selected models, and the contrast between data-driven and theory-driven variable selection shows that the retained oxygen variables are proximate correlates of an upstream nutrient load rather than independent causes.",
    "Constrained models explain about half of the community variation, and beta-diversity partitioning attributes most of the turnover to species replacement."
  ),
  "\\end{abstract}",
  "\\section{Data and Pre-processing}",
  "The source files are the local BCB743 Doubs River tables. Environmental PCA used all 30 environmental rows. Species ordinations and constrained analyses removed site 8 because it has zero total fish abundance, which leaves 29 matched fish, environmental, and spatial rows.",
  table_tex(data_overview, "Local Doubs River data files used in this report.", "tab:data-overview", font_size = 8),
  table_tex(env_summary, "Environmental variables and their longitudinal behaviour. The last column is the Pearson correlation with distance from source (`dfs`) across all 30 sites.", "tab:env-summary", font_size = 7),
  "\\begin{figure}[H]",
  "\\centering",
  "\\includegraphics[width=0.78\\linewidth]{fig01_doubs_sites.pdf}",
  "\\caption{Spatial configuration of the Doubs River sites. Colour gives distance from source.}",
  "\\label{fig:sites}",
  "\\end{figure}",
  "\\section{Methods}",
  "The methods follow the BCB743 ordination sequence and then extend it with a set of more advanced checks, each asking a sharper question of the same data. Unconstrained ordinations come first. They let the data reveal their dominant structure without forcing environmental predictors into the solution. PCA summarises covariance among the measured environmental variables, CA and DCA summarise unimodal fish turnover, and PCoA and nMDS summarise Bray-Curtis dissimilarities among sites. Constrained ordinations come second, and they ask a sharper question, namely how much of the fish-composition pattern the measured environmental variables can account for. RDA, CCA, and db-RDA answer that question under three different geometries. The advanced analyses come last. They ask whether the same conclusion survives when the problem is reframed in terms of collinearity, spatial structure, distributional assumptions, the philosophy of variable selection, and the shape of individual species responses.",
  table_tex(method_summary, "Summary of methods and headline outputs.", "tab:method-summary", font_size = 7),
  "\\section{Unconstrained Gradients}",
  "\\subsection{Environmental PCA}",
  paste0(
    "The standardised environmental PCA is strongly one-dimensional: PC1 explains ",
    fmt(100 * eigenvals(env_pca)[1] / sum(eigenvals(env_pca)), 1),
    "\\% of the environmental variance and PC2 explains ",
    fmt(100 * eigenvals(env_pca)[2] / sum(eigenvals(env_pca)), 1),
    "\\%. PC1 contrasts high-altitude, high-oxygen, steep upper sites against downstream sites with higher distance from source, discharge, nitrate, phosphate, ammonium, hardness, and biological oxygen demand."
  ),
  table_tex(pca_eig, "Environmental PCA eigenvalues.", "tab:pca-eig", font_size = 8),
  table_tex(pca_load, "Environmental PCA variable scores, sorted by absolute PC1 loading.", "tab:pca-load", font_size = 8),
  "\\begin{figure}[H]",
  "\\centering",
  "\\includegraphics[width=0.82\\linewidth]{fig02_environmental_pca.pdf}",
  "\\caption{PCA biplot of the Doubs environmental variables. Arrows indicate environmental loadings; sites are coloured by distance from source.}",
  "\\label{fig:pca}",
  "\\end{figure}",
  "\\subsection{Species CA and DCA}",
  paste0(
    "CA gives the same gradient through the fish assemblage. CA1 explains ",
    fmt(100 * spe_ca$CA$eig[1] / sum(spe_ca$CA$eig), 1),
    "\\% of total inertia and CA2 explains ",
    fmt(100 * spe_ca$CA$eig[2] / sum(spe_ca$CA$eig), 1),
    "\\%. DCA estimates the first gradient length as ",
    fmt(spe_dca$rproj[1], 2),
    " standard deviation units, which is long enough for clear species turnover and explains why a CA arch appears on the second axis."
  ),
  table_tex(ca_eig, "Correspondence analysis eigenvalues.", "tab:ca-eig", font_size = 8),
  "\\begin{figure}[H]",
  "\\centering",
  "\\includegraphics[width=0.82\\linewidth]{fig03_species_ca.pdf}",
  "\\caption{CA of Doubs fish abundances. Species labels are shown for the more peripheral taxa; points are sites coloured by distance from source.}",
  "\\label{fig:ca}",
  "\\end{figure}",
  "\\begin{figure}[H]",
  "\\centering",
  "\\includegraphics[width=0.78\\linewidth]{fig05_dca_gradient.pdf}",
  "\\caption{DCA of the Doubs fish data. The connected site sequence shows the long compositional gradient from source to lower reaches.}",
  "\\label{fig:dca}",
  "\\end{figure}",
  "\\subsection{PCoA and nMDS}",
  paste0(
    "The distance-based ordinations recover the same gradient under a geometry free of the CA arch. Bray-Curtis PCoA MDS1 explains ",
    fmt(100 * pcoa_pos[1] / sum(pcoa_pos), 1),
    "\\% of positive inertia, and the nMDS stress is ",
    fmt(spe_nmds$stress, 3),
    ", a good two-dimensional representation of the rank-order dissimilarities."
  ),
  table_tex(pcoa_eig, "PCoA positive eigenvalues for Bray-Curtis fish dissimilarities. Percentages are relative to the sum of positive eigenvalues.", "tab:pcoa-eig", font_size = 8),
  "Table~\\ref{tab:tab:gradient-align} links the ordination figures to the ecological interpretation. For each unconstrained ordination it takes the site scores on the first axis and correlates them with distance from source, the one variable that orders the sites unambiguously from headwater to lower reach. The correlations are alignment diagnostics rather than model tests, since distance from source is fitted in none of these ordinations. It is projected onto them afterwards. A high value therefore says that an axis extracted from the data alone, whether from environmental covariance (PCA), fish turnover (CA), or fish dissimilarity (PCoA and nMDS), reconstructs the physical river order without being told to do so. The four correlations lie between 0.81 and 0.85, so the same upstream-to-downstream sequence emerges from every geometry and from both the environmental and the faunal table. The dominant structure is a property of the system, not of any one method.",
  table_tex(gradient_alignment, "Alignment between first ordination axes and the longitudinal river gradient.", "tab:gradient-align", font_size = 8),
  "\\begin{figure}[H]",
  "\\centering",
  "\\includegraphics[width=\\linewidth]{fig04_pcoa_nmds.pdf}",
  "\\caption{Bray-Curtis PCoA and nMDS ordinations of fish composition. Both recover the upstream-to-downstream species gradient.}",
  "\\label{fig:pcoa-nmds}",
  "\\end{figure}",
  "\\subsection{Environmental Vector Fitting}",
  "Vector fitting projects each environmental variable a posteriori onto the CA of fish composition and identifies the variables most aligned with the assemblage pattern. The step is indirect-gradient in character. The CA is built from the fish matrix alone, and the environmental variables enter only afterwards, as arrows fitted to the finished configuration. Table~\\ref{tab:tab:ca-envfit} is therefore an interpretation aid and carries none of the inferential weight of a constrained model. Three quantities describe each fitted vector. Its direction gives the part of the ordination in which the variable increases, its $r^2$ gives the strength of association with the two-axis configuration, and its permutation p-value asks whether that association exceeds what random reassignment of the variable to sites would produce. Altitude ($r^2 = 0.81$), distance from source (0.69), and oxygen (0.63) give the longest vectors, followed by hardness, nitrate, and flow. These vectors share a common direction because they express one river-continuum gradient rather than several independent mechanisms. Ammonium, phosphate, and pH fit weakly ($r^2 \\leq 0.18$), and two of them fail the permutation test, the first sign that the enrichment variables carry structure the continuum axis does not fully absorb.",
  table_tex(ca_envfit_tab, "Environmental vectors fitted to the CA ordination. The $r^2$ column is the squared correlation between the fitted vector and the ordination configuration.", "tab:ca-envfit", font_size = 7),
  "\\begin{figure}[H]",
  "\\centering",
  "\\includegraphics[width=0.82\\linewidth]{fig06_ca_envfit.pdf}",
  "\\caption{CA ordination with fitted environmental vectors. Red vectors are significant at $p \\leq 0.05$.}",
  "\\label{fig:envfit}",
  "\\end{figure}",
  "\\section{Constrained Ordination}",
  "The constrained ordinations ask whether the environmental table explains the fish composition. They are direct-gradient analyses, since the environmental variables build the ordination axes and permutation tests then ask whether the explained component exceeds what chance would produce. This is a stronger statement than vector fitting, though still short of a proof of causation. In a river, many predictors are collinear because altitude, flow, distance from source, oxygen, nutrients, and organic load all change together along the channel. A constrained model can therefore show that the environmental matrix explains community structure while remaining unable to separate the causal contribution of each correlated variable.",
  "Variance inflation factors (VIFs) and forward selection are two diagnostics applied before any interpretation of the selected models. A high VIF marks a variable that is largely predictable from the others, so it cannot be interpreted as an independent effect. Forward selection with \\texttt{ordiR2step()} answers a different question. It starts from a null model, tries the candidate variables from the full model, adds the one that gives the strongest permitted gain in adjusted $R^2$, and stops when no remaining candidate passes both the permutation test and the adjusted-$R^2$ ceiling. VIF is a diagnostic of predictor structure and \\texttt{ordiR2step()} is response-informed selection, so the two are complementary. The selected variables describe the river-continuum gradient compactly rather than naming independent causal drivers.",
  "Table~\\ref{tab:tab:model-summary} summarises the three selected constrained models. Compare them on adjusted $R^2$, which corrects for the number of predictors and so does not reward a model for merely carrying more variables. The raw $R^2$ would. The whole-model permutation p-values, all 0.001, show that each selected environmental set explains a detectable component of fish-community structure. The three models were built under different geometries and on different response transformations, yet they converge. RDA and db-RDA both retain \\texttt{dfs}, \\texttt{oxy}, and \\texttt{bod}, differing only in the order of the last two, and CCA retains \\texttt{dfs}, \\texttt{oxy}, and \\texttt{alt}. The convergence carries more weight than the small differences between models, because every model keeps one longitudinal-position variable (distance from source or altitude) together with oxygen and a marker of organic loading. Adjusted $R^2$ ranges from 0.50 in CCA to 0.57 in db-RDA, so roughly half of the community variation is captured by three correlated descriptors of river position.",
  table_tex(model_summary, "Selected constrained ordination models.", "tab:model-summary", font_size = 8),
  "Table~\\ref{tab:tab:term-tests} decomposes each selected model term by term. The tests are sequential, applied in the order \\texttt{ordiR2step()} added the variables, so each term is assessed over and above those already in the model. The first term absorbs the broadest gradient and later terms explain only what remains. Distance from source therefore carries by far the largest F statistic in every model (23.5 in RDA, 19.6 in CCA, 25.1 in db-RDA), since it stands for the dominant longitudinal ordering of the river. Oxygen and biological oxygen demand enter next and stay individually significant ($p \\leq 0.003$), which means they capture chemical and organic-loading structure that distance from source alone leaves unexplained. That residual signal is the quantitative trace of the mid-river oxygen sag examined later in the report.",
  table_tex(constrained_terms, "Permutation tests for terms in selected constrained models.", "tab:term-tests", font_size = 7),
  "Table~\\ref{tab:tab:axis-tests} tests the constrained axes rather than the named predictors. A constrained axis is a composite gradient, a weighted combination of the selected predictors ranked by the explained community variation it carries. A significant first axis, with F between 22.9 and 28.5 across the three models, shows that the primary fitted environmental gradient is strong. Significant second and third axes show that the selected predictors retain lower-dimensional structure once that primary gradient is removed. These later axes reward caution, since each mixes several correlated variables and need not correspond to any single measured gradient.",
  table_tex(axis_tests, "Permutation tests for constrained axes.", "tab:axis-tests", font_size = 7),
  "\\subsection{VIF and Forward Selection Diagnostics}",
  "The full models deliberately begin with all measured environmental variables. This is useful because it shows the scale of the collinearity problem before any selection is done. The full-model VIFs are high for distance from source, altitude, flow, phosphate, ammonium, nitrate, oxygen, and biological oxygen demand, confirming that the river variables are not independent measurements. They are linked parts of the same physical and chemical continuum. This is why the report does not interpret the full-model coefficients or arrows as separate effects.",
  table_tex(full_vif_tab, "Variance inflation factors for the full constrained models before forward selection.", "tab:full-vif", font_size = 7),
  "After forward selection, the VIFs fall substantially. This does not make the selected variables causally independent, but it does mean the reduced models are less redundant and easier to interpret. The remaining VIFs are acceptable for a compact explanatory summary of the fish-community gradient.",
  table_tex(selected_vif_tab, "Variance inflation factors for the selected constrained models after forward selection.", "tab:selected-vif", font_size = 7),
  "The forward-selection table records the actual \\texttt{ordiR2step()} path. Each row is one accepted addition to the model. The adjusted $R^2$ column shows the cumulative explanatory power after that step, so it should increase from top to bottom within each model. The p-value tests the additional term at the point it enters the model, not in isolation from all other variables.",
  table_tex(stepwise_summary, "Forward-selection paths from \\texttt{ordiR2step()} for the constrained ordinations.", "tab:stepwise-summary", font_size = 7),
  "\\begin{figure}[H]",
  "\\centering",
  "\\includegraphics[width=\\linewidth]{fig07_constrained_ordination.pdf}",
  "\\caption{Selected RDA, CCA, and db-RDA models. Sites are coloured by distance from source.}",
  "\\label{fig:constrained}",
  "\\end{figure}",
  "\\subsection{Data-driven and theory-driven variable selection}",
  "Forward selection is a statistical procedure, not an ecological hypothesis. It keeps whichever variables most improve adjusted $R^2$, and in these data that is \\texttt{dfs} together with \\texttt{oxy} and \\texttt{bod}. A mechanistic reading of the same river suggests a different starting point. Position in the catchment sets the physical template, since altitude and distance from source govern temperature, slope, and discharge. Land use along the valley then loads the water with dissolved inorganic nitrogen (DIN, namely nitrate plus ammonium) and with phosphate. That nutrient load drives primary production and the supply of degradable organic matter, which raises biological oxygen demand, and the respiration that follows draws down dissolved oxygen. In this chain nutrients are the upstream lever and oxygen is a downstream consequence. A researcher who wants the model to name a cause rather than a correlate might therefore prefer a nutrient variable over oxygen, on the grounds that nutrient input is what a catchment manager can actually change.",
  "Table~\\ref{tab:tab:theory-compare} puts that intuition to a test. It fits several candidate Hellinger RDA models and compares their adjusted $R^2$ and their worst variance inflation factor. Distance from source alone already reaches adjusted $R^2 = 0.37$. Adding nitrate lifts it only to 0.39, whereas adding oxygen and biological oxygen demand lifts it to 0.54. The two nutrient-loading models, altitude with nitrate and either ammonium or phosphate, settle near 0.39 as well, and their variance inflation factors stay modest. Swapping the enrichment pair for a nutrient variable therefore costs about 0.15 of adjusted $R^2$, which is not a rounding difference.",
  table_tex(theory_compare, "Data-driven and theory-driven candidate constrained models, all fitted as Hellinger RDA. Adjusted $R^2$ measures explanatory power and the maximum VIF measures residual collinearity within each candidate set.", "tab:theory-compare", font_size = 8),
  "The reason for that cost is visible in the correlations of Table~\\ref{tab:tab:enrich-cor}. Along the clean part of the continuum, oxygen, biological oxygen demand, nitrate, ammonium, and phosphate move together, with many pairwise correlations above 0.7 in absolute value, so any one of them stands in for the others and the choice among them is largely interpretive. The exception is where the ecology becomes interesting. Nitrate rises almost monotonically with distance from source ($r = 0.75$) and tracks the continuum, whereas oxygen and biological oxygen demand decouple from it at the mid-river reach near sites 23 to 25, where oxygen falls to 4 to 6 mg\\,L$^{-1}$ and biological oxygen demand exceeds 12 mg\\,L$^{-1}$. Forward selection retains oxygen and biological oxygen demand precisely because they carry that local oxygen sag, which a position or nitrate model cannot reproduce.",
  table_tex(enrich_cor, "Pearson correlations among distance from source and the enrichment variables across the 29 matched sites. The tight cluster among oxygen, biological oxygen demand, nitrate, ammonium, and phosphate is the collinearity that makes them substitutable along the continuum.", "tab:enrich-cor", font_size = 8),
  "The honest reading is therefore layered. For a management question about nutrient control, DIN is the defensible predictor, and it forfeits little explanatory power over the continuum. For describing the community that is actually present, oxygen is more than a proxy; near the pollution reach it is a proximate stressor with a signal of its own, and dropping it would discard the one place where the river departs from a smooth gradient. Statistical selection cannot arbitrate between these readings, because the variables are collinear over most of the river and diverge only at a handful of sites. The choice is an ecological judgement informed by the correlation structure, not an output of the algorithm. This is the sense in which explanation is not causation: \\texttt{ordiR2step()} finds the most economical description of the pattern, but the causal ordering of nutrients, organic load, and oxygen must be supplied by the ecology.",
  "\\section{Additional Advanced Analyses}",
  "The analyses in this section sit outside the core Quantitative Ecology chapter sequence. Each re-examines the river-continuum interpretation from an angle the ordinations do not cover, namely the separation of environment from space, the correlation of whole dissimilarity matrices, the decomposition of beta diversity, the identification of ecologically distinctive sites and species, and the shape of individual species responses. They extend the ordinations rather than replace them, and several of them sharpen the interpretation by showing where the smooth-continuum picture breaks down. Table~\\ref{tab:tab:advanced-summary} maps the section.",
  table_tex(advanced_summary, "Advanced analyses added beyond the core Quantitative Ecology workflow, with their purpose and headline result.", "tab:advanced-summary", font_size = 7),
  "\\subsection{Environment-Space Variation Partitioning}",
  "Variation partitioning splits the Hellinger-transformed community variation into four fractions, namely the part explained uniquely by the selected environmental variables, the part explained uniquely by spatial position, the part they explain jointly, and the unexplained remainder. The exercise is informative here because river environments are spatially organised. Sites further downstream are also further along the channel, so environment and space are confounded by construction, and a large shared fraction is expected rather than a nuisance to be removed. Working through Table~\\ref{tab:tab:variation-partition}, the pure environmental fraction is adjusted $R^2 = 0.11$ and the pure spatial fraction is 0.12, while the shared fraction reaches 0.43. Most of the explained community variation is therefore jointly attributable to environment and position, which is the numerical statement of the fact that the environmental gradient is laid out along the length of the river. Only about a third of the variation (adjusted $R^2 = 0.34$) is left unexplained.",
  table_tex(variation_partition, "Variation partitioning between selected environmental predictors and polynomial spatial coordinates.", "tab:variation-partition", font_size = 7),
  "\\subsection{Spatial Eigenvectors, Procrustes Rotation, and manyGLM}",
  "Moran eigenvector maps (MEMs) are orthogonal spatial variables that describe pattern at a range of scales, from a broad upstream-to-downstream trend to finer alternations between neighbouring reaches. Forward selection over the MEM set retains four of them and reaches adjusted $R^2 = 0.55$ ($p = 0.001$), which confirms that the fish assemblage is spatially structured along the river network. The MEMs name no environmental cause. They quantify the scale at which spatial structure exists, and the broadest-scale MEMs carry the same longitudinal signal as distance from source.",
  "The Procrustes rotation asks a geometric question. It rotates and rescales the environmental PCA configuration onto the fish-community nMDS configuration and measures how closely the two point clouds coincide. The fit is strong (Procrustes $r = 0.75$, $p = 0.001$), so a site that is unusual in environmental space tends to be unusual in the same way in faunal space. The two ordinations describe one gradient in two currencies.",
  "The manyGLM analysis moves from distance-based to model-based inference. It fits a negative-binomial generalised linear model to each species, respecting the mean-variance relationship of counts that dissimilarity indices only approximate, and it tests the selected predictors jointly by resampling (Table~\\ref{tab:tab:manyglm}). Distance from source, biological oxygen demand, and oxygen are each significant ($p = 0.002$), with distance from source carrying much the largest deviance, 430 against 133 and 112 for the two chemistry terms. A method that makes the count distribution explicit therefore reaches the same ranking as the ordinations, which is reassurance that the pattern does not depend on the Bray-Curtis geometry. The agreement across MEM, Procrustes, and manyGLM supports the main conclusion and repeats the standing caution that environment and space are tightly coupled here.",
  table_tex(manyglm_tab, "Negative-binomial manyGLM multivariate tests for the db-RDA selected predictors.", "tab:manyglm", font_size = 7),
  "\\subsection{Matrix Correlation with Mantel and Partial Mantel Tests}",
  "The Mantel test correlates two distance matrices directly, without first reducing either to ordination axes. Here it compares the fish Bray-Curtis dissimilarity with a Euclidean distance on the standardised environmental variables, and with the geographic distance between sites (Table~\\ref{tab:tab:mantel}). Both simple correlations are significant, but the environmental matrix is much the stronger match (Mantel $r = 0.61$ against $r = 0.32$ for geography). The partial tests then hold one matrix constant while testing the other. Community structure remains strongly tied to environment once geography is held constant ($r = 0.56$, $p = 0.001$), whereas its tie to geography almost disappears once environment is held constant ($r = 0.18$, $p = 0.015$). The environment carries most of the signal, and the pure-space contribution is small. This agrees in direction with the variation partitioning, though the two methods weight the environment differently, since the Mantel test uses the full eleven-variable environmental distance while the partitioning uses only the three selected predictors. Mantel correlations are also known to be conservative and to depend on the distance measure, so they are read as corroboration rather than as a separate estimate of effect size.",
  table_tex(mantel_tab, "Mantel and partial Mantel correlations among the fish, environmental, and geographic distance matrices, with 999 permutations.", "tab:mantel", font_size = 8),
  "\\subsection{Beta-Diversity Components: Replacement and Abundance Gradient}",
  "The central claim of the report is that downstream change is a replacement of one set of species by another rather than a simple loss of species. Beta-diversity partitioning tests that claim quantitatively by splitting the total multi-site Bray-Curtis dissimilarity into a balanced-variation component, which is true turnover where a gain of one species is matched by a loss of another, and an abundance-gradient component, which is the nestedness-like part where assemblages differ because one is a subset or a diluted version of the other (Table~\\ref{tab:tab:betapart}). Turnover dominates. It accounts for 0.86 of the total dissimilarity, against 0.14 for the abundance-gradient component. The pattern is therefore replacement, which is consistent with the long DCA gradient and with the species arch in the CA. The abundance-gradient component is not zero, and the sites that inflate it are the depauperate ones at the two ends of the disturbance range, namely the headwater and the polluted mid-river reach, where the assemblage is a thinned subset of the richer lowland fauna rather than a distinct set of species. Beta-diversity partitioning thus recovers both processes and shows which one dominates.",
  table_tex(betapart_tab, "Multi-site beta-diversity partitioning of the fish assemblage into balanced turnover and abundance-gradient components (Baselga family, abundance-based).", "tab:betapart", font_size = 8),
  "\\subsection{Distinctive Sites and Species: LCBD and SCBD}",
  "Local contributions to beta diversity (LCBD) rank the sites by how much each one adds to the total variance of the community matrix, and species contributions (SCBD) do the same for the species. High-LCBD sites are ecologically distinctive, whether through rarity, unusual composition, or a departure from the general trend. Table~\\ref{tab:tab:lcbd} lists the six sites with the largest LCBD, and the permutation p-values flag those whose distinctiveness exceeds chance. The result is telling. The most distinctive sites are the extreme headwater (site 1, a near-monospecific brown-trout reach) and the organic-pollution zone (sites 23 to 25), each significant at $p \\leq 0.05$. These are exactly the two places where the smooth continuum is interrupted, the first by the severity of the upstream physical filter and the second by the mid-river oxygen sag. The heaviest SCBD contributions come from the gradient-defining species, led by " ,
  paste0(scbd_top_str, ". Figure~\\ref{fig:lcbd} places the LCBD values and the site-level species richness along distance from source, and the richness collapse at sites 23 to 25, from 22 species down to 3, coincides with the LCBD peak there."),
  table_tex(lcbd_tab, "The six sites with the largest local contribution to beta diversity (LCBD), with distance from source and the permutation p-value.", "tab:lcbd", font_size = 8),
  "\\begin{figure}[H]",
  "\\centering",
  "\\includegraphics[width=\\linewidth]{fig10_lcbd_richness.pdf}",
  "\\caption{Local contribution to beta diversity (left) and species richness (right) along distance from source. Firebrick points on the left mark sites significant at $p \\leq 0.05$; the richness minimum at sites 23 to 25 marks the organic-pollution reach.}",
  "\\label{fig:lcbd}",
  "\\end{figure}",
  "\\subsection{Species-Response Shapes Along the Gradient}",
  "The ordinations assume, through the chi-square and Bray-Curtis geometries, that species rise and fall along the gradient rather than increasing without limit. A direct way to check that assumption is to fit each species against distance from source with a negative-binomial generalised linear model that includes a linear and a quadratic term, \\texttt{glm.nb(y \\textasciitilde{} dfs + I(dfs\\textasciicircum{}2))}. A significant negative quadratic term indicates a unimodal response with an optimum inside the surveyed reach, and the optimum itself is the fitted peak position (Table~\\ref{tab:tab:response}, Figure~\\ref{fig:response}). Headwater and mid-river species show clear unimodal responses with optima ordered along the channel, from brown trout near 50 km through stone loach, bullhead, and grayling to barbel near 350 km. The lowland species behave differently. Roach, gudgeon, and common bream are still rising at the downstream end of the survey, so their fitted optima lie beyond the sampled reach and their responses are monotonic within it. This is the expected signature of a gradient sampled from its cold, species-poor source to a lower reach that has not yet reached the true lowland optima of the most downstream taxa, and it is the same truncation that gives the DCA its long first axis.",
  table_tex(response_tab, "Negative-binomial species-response models against distance from source. The fitted optimum is the peak of the quadratic response where it falls inside the surveyed reach; the quadratic p-value tests for curvature.", "tab:response", font_size = 8),
  "\\begin{figure}[H]",
  "\\centering",
  "\\includegraphics[width=\\linewidth]{fig09_species_response.pdf}",
  "\\caption{Fitted negative-binomial abundance responses (firebrick) with observed counts (grey) against distance from source, for species spanning the gradient. Vertical scales differ between panels.}",
  "\\label{fig:response}",
  "\\end{figure}",
  "\\section{Species Replacement Along the Gradient}",
  "The ordinations reduce the data to axes. The raw abundance pattern shows the ecological turnover directly (Figure~\\ref{fig:species-gradient}). The gradient has a physical origin. Near the source the channel is high, steep, and cold, the discharge is low but turbulent, and the water is close to oxygen saturation. Downstream the profile flattens, discharge and hardness rise, the water warms, and organic matter accumulates and decays over a longer residence time. This physical template sets up a chemical one, since warmer, slower, nutrient-richer water supports more production and a higher biological oxygen demand. The fish assemblage is layered onto both.",
  "Each species occupies a bounded stretch of the gradient, and the stretches are ordered, which is what produces replacement rather than simple enrichment or loss. The headwaters hold cool-water specialists, namely brown trout, bullhead, minnow, and grayling, whose fitted optima fall in the upper reaches (Table~\\ref{tab:tab:response}). Stone loach and barbel take over through the middle river. The lower reaches are held by cyprinids and generalists such as roach, common bream, bleak, and eel, whose abundances are still climbing at the downstream limit of the survey. The sequence recovers the classical river fish zonation, from a trout zone through grayling and barbel zones to a bream zone, expressed as a continuous replacement rather than a set of sharp boundaries.",
  "One reach departs from the smooth sequence. At sites 23 to 25 the water chemistry changes abruptly, with biological oxygen demand rising above 12 mg\\,L$^{-1}$, ammonium reaching 1.8 mg\\,L$^{-1}$, and dissolved oxygen falling to 4 to 6 mg\\,L$^{-1}$. This is the chemical signature of a point-source organic load. Species richness collapses from 22 at site 22 to 3 at site 23 before recovering downstream (Figure~\\ref{fig:lcbd}), and these sites carry the highest local contributions to beta diversity outside the headwater. The organic-pollution reach is a second, shorter gradient superimposed on the longitudinal one, an oxygen sag that thins the assemblage locally and then releases it as the river re-aerates. This reach is what keeps oxygen and biological oxygen demand in the selected constrained models and what lifts the abundance-gradient component of beta diversity above zero.",
  "\\begin{figure}[H]",
  "\\centering",
  "\\includegraphics[width=0.95\\linewidth]{fig08_species_gradient_heatmap.pdf}",
  "\\caption{Abundance of representative fish species along distance from source. Species are ordered to show the broad upstream-to-downstream replacement.}",
  "\\label{fig:species-gradient}",
  "\\end{figure}",
  "\\section{Discussion: Gradients that Structure the System}",
  "The Doubs assemblage is organised first and foremost by a longitudinal river-continuum gradient. The upper sites are high, steep, cool reaches with near-saturated oxygen and little nutrient or organic load. The lower sites are further from source and lower in altitude, with greater discharge, harder water, more nutrients, and higher biological oxygen demand. These variables correlate because they are different measurements of one downstream integration of catchment, geomorphological, and biogeochemical processes, and every analysis in this report, unconstrained or constrained, distance-based or model-based, recovers that single axis as the dominant structure.",
  "The fish assemblage is layered onto this template as an ordered replacement. The upstream end supports trout-zone species, and downstream those decline and give way to cyprinids and generalists of the warmer, slower, more productive lower river. Because each species occupies a bounded stretch of the gradient (Table~\\ref{tab:tab:response}), the turnover is genuine replacement rather than progressive enrichment, which the beta-diversity partitioning confirms by attributing 0.86 of total dissimilarity to balanced turnover. CA, PCoA, and nMDS recover the same sequence from composition alone, so the interpretation rests on the data rather than on one ordination geometry.",
  "The continuum is not perfectly smooth. A short reach at sites 23 to 25 carries a strong organic-load signal, with biological oxygen demand above 12 mg\\,L$^{-1}$ and oxygen down to 4 mg\\,L$^{-1}$, and the fauna there thins to a few tolerant species before recovering downstream. This reach is the most distinctive part of the river after the headwater on the LCBD ranking, it supplies most of the nestedness-like component of beta diversity, and it is the reason oxygen and biological oxygen demand survive forward selection where nitrate, which tracks the continuum more faithfully, does not. Read together, the analyses describe two overlaid gradients, a long physical-chemical continuum and a short, sharp oxygen sag, rather than a single monotonic trend.",
  "The second ordination axes reward caution. CA2 in particular is partly the arch effect of a long first gradient rather than a separate ecological process, and DCA supports that reading with a first-axis length of 3.9 standard-deviation units. Where second-axis structure is genuine, it is best interpreted as local departure from the main sequence, and the oxygen-sag reach is the clearest example.",
  "The constrained models show that the measured environment explains about half of the community variation, yet they cannot rank the predictors causally, because those predictors are correlated expressions of river position and organic load. Forward selection returns the most economical description, namely distance from source with oxygen and biological oxygen demand, while a mechanistic account would place dissolved inorganic nitrogen upstream of the oxygen response. The two views are compatible. Statistics identifies the compact set of correlates, and ecology supplies the causal ordering. Neither on its own licenses a claim that a single variable drives the fish community.",
  "The analyses are correlational, so association is not mechanism. The gradient is truncated at both ends, so the optima of the most downstream species lie beyond the survey and the headwater is sampled to near-zero richness. Environment and space are confounded along a single channel, so the pure environmental and pure spatial fractions are both small while the shared fraction is large. These are properties of a one-dimensional river surveyed once, and they would be relaxed only by sampling more rivers, more seasons, or by manipulative work.",
  "\\section{Reproducibility}",
  paste0(
    "This report was generated by \\texttt{BCB743/reports/make\\_doubs\\_multivariate\\_report.R}. ",
    "The run used \\textsf{vegan} ", as.character(packageVersion("vegan")), " under R ", getRversion(),
    ", with 999 permutations for the ordination and matrix tests. The advanced analyses used ",
    paste(c(
      if (mem_available) paste0("\\textsf{adespatial} ", as.character(packageVersion("adespatial"))),
      if (betapart_available) paste0("\\textsf{betapart} ", as.character(packageVersion("betapart"))),
      if (mvabund_available) paste0("\\textsf{mvabund} ", as.character(packageVersion("mvabund"))),
      paste0("\\textsf{MASS} ", as.character(packageVersion("MASS")))
    ), collapse = ", "),
    ". All stochastic steps were seeded with \\texttt{set.seed(743)}."
  ),
  "\\end{document}"
)

writeLines(tex, file.path(out_dir, "doubs_multivariate_report.tex"))

cat("Wrote ", file.path(out_dir, "doubs_multivariate_report.tex"), "\n", sep = "")
cat("Wrote figures in ", fig_dir, "\n", sep = "")

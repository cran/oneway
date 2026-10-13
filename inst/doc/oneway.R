## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval=FALSE---------------------------------------------------------------
# install.packages("oneway")

## ----eval=FALSE---------------------------------------------------------------
# if (!require("pak")) install.packages("pak")
# pak::pak("P10911004-NPUST/oneway")

## -----------------------------------------------------------------------------
library(oneway)

# For reproducibility
set.seed(123)

## -----------------------------------------------------------------------------
out <- pairwise_comparison(morphine, tolerance ~ grp)

## -----------------------------------------------------------------------------
anorexia[c(2, 19, 43, 47, 60, 71), ]

## -----------------------------------------------------------------------------
pairwise_comparison(anorexia, weight_gain ~ therapy)

## -----------------------------------------------------------------------------
group_data <- list(control = rnorm(20, 10, 2),
                   treatment_A = rnorm(20, 12, 2),
                   treatment_B = rnorm(20, 15, 2))

pairwise_comparison(group_data)

## -----------------------------------------------------------------------------
describe(anorexia, weight_gain ~ therapy, rounding = 2)

## ----pre-hoc------------------------------------------------------------------
anova_ooo <- oneway_anova(O_O_O, val ~ grp)
anova_ooo

## -----------------------------------------------------------------------------
fisher <- oneway_anova(O_O_O, val ~ grp, var_equal = TRUE)
welch <- oneway_anova(O_X_X, val ~ grp, var_equal = FALSE)

## -----------------------------------------------------------------------------
art <- oneway_art(X_O_O, val ~ grp)
art

## -----------------------------------------------------------------------------
attr(art, "data") |> head()

## -----------------------------------------------------------------------------
kw <- Kruskal_Wallis_test(X_X_O, val ~ grp)
kw

## -----------------------------------------------------------------------------
regwq <- REGWQ_test(O_O_O, val ~ grp)

## -----------------------------------------------------------------------------
tukey <- Tukey_HSD_test(O_O_O, val ~ grp)

## -----------------------------------------------------------------------------
tukey_kramer <- Tukey_Kramer_test(O_O_X, val ~ grp)

## -----------------------------------------------------------------------------
games_howell <- Games_Howell_test(O_X_X, val ~ grp)

## -----------------------------------------------------------------------------
dunn <- Dunn_test(X_X_O, val ~ grp, p_adjust_method = "holm")

## -----------------------------------------------------------------------------
dunn_bh <- Dunn_test(X_X_O, val ~ grp, p_adjust_method = "BH")

## -----------------------------------------------------------------------------
adaptive <- pairwise_comparison(O_O_O, val ~ grp)

## -----------------------------------------------------------------------------
dunn <- Dunn_test(X_X_O, val ~ grp, verbose = FALSE)

summary_tab <- row_arrange(dunn$summary, cols = "MED")

padj_name <- "Padj (holm)"

cld <- compact_letter_display(x1 = dunn$post_hoc$x1,
                              x2 = dunn$post_hoc$x2,
                              pvalues = dunn$post_hoc[[padj_name]],
                              grp_names = summary_tab$GROUP,
                              centers = summary_tab$MED)

cld

## -----------------------------------------------------------------------------
compact_letter_display(
  x1 = dunn$post_hoc$x1,
  x2 = dunn$post_hoc$x2,
  pvalues = dunn$post_hoc[[padj_name]],
  grp_names = summary_tab$GROUP,
  centers = summary_tab$MED,
  display_letters = as.character(1:6),
  display_null_letter = "_"
)

## -----------------------------------------------------------------------------
p <- c(0.20, 0.04, 0.008, 0.0005, 1e-6)

pval2asterisk(p)

## -----------------------------------------------------------------------------
pval2asterisk(p,
              break_points = c(0.05, 0.01, 0),
              symbols = c("NS", "S", "HS"))

## -----------------------------------------------------------------------------
g1 <- subset(O_O_O, grp == "G1")$val
g2 <- subset(O_O_O, grp == "G2")$val
Hedges_g_s(g1, g2)

## -----------------------------------------------------------------------------
fisher[, c("method", "Pvalue", "p_omega2")]


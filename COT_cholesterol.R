################################################################################
# COT-LP Bootstrap Bounds
# Cholesterol Reduction Dataset & Lalonde Dataset
#
# 論文の Corollary 1-5 に忠実な実装:
#
# [ICDE 偶数次モーメント, Corollary 1]
#   E^U[(Y1-Y0)^m] <= E[(Y1-Y0)^m] <= E^L[(Y1-Y0)^m]  (sharp)
#   -> Variance UB = E^L[(Y1-Y0)^2] - ACDE^2
#   -> Variance LB = max(E^U[(Y1-Y0)^2] - ACDE^2, 0)
#
# [ICDE 奇数次モーメント, Corollary 3]
#   sum_{l}(-1)^l C(m,l)[I((-1)^l=-1)E^U[Y1^{m-l}Y0^l] + I((-1)^l=1)E^L[...]]
#   <= E[(Y1-Y0)^m] <= ...  (not sharp)
#   ただし歪度・尖度は中心モーメントから計算するため
#   3次中心モーメントは直接 g=(y1-y0-acde)^3 でバウンド
#
# [ICRE, Corollary 4]
#   E^U[(Y1/Y0)^m] <= E[(Y1/Y0)^m] <= E^L[(Y1/Y0)^m]  (sharp)
#   -> Mean  UB = E^L[Y1/Y0],  LB = E^U[Y1/Y0]
#   -> Var   UB = E^L[(Y1/Y0)^2] - (E^U[Y1/Y0])^2
#   -> Var   LB = max(E^U[(Y1/Y0)^2] - (E^L[Y1/Y0])^2, 0)
#
# [歪度・尖度, Appendix B Eq.60-61]
#   skew UB = mu3_UB / mu2_LB^{3/2} (mu3_UB>=0), mu3_UB/mu2_UB^{3/2} (mu3_UB<0)
#   skew LB = mu3_LB / mu2_LB^{3/2} (mu3_LB<0),  mu3_LB/mu2_UB^{3/2} (mu3_LB>=0)
#   kurt UB = mu4_UB / mu2_LB^2 - 3
#   kurt LB = mu4_LB / mu2_UB^2 - 3
################################################################################

if (!requireNamespace("lpSolve",     quietly=TRUE))
  install.packages("lpSolve",     repos="https://cloud.r-project.org")
if (!requireNamespace("readr",       quietly=TRUE))
  install.packages("readr",       repos="https://cloud.r-project.org")
if (!requireNamespace("designmatch", quietly=TRUE))
  install.packages("designmatch", repos="https://cloud.r-project.org")

library(lpSolve)
library(readr)
library(designmatch)

set.seed(42)

# ── COT LP solver ─────────────────────────────────────────────────────────────
cot_lp <- function(y1, y0, g_mat, mode = c("max","min")) {
  mode <- match.arg(mode)
  NT <- length(y1); NC <- length(y0)
  n_vars <- NT * NC
  obj <- as.vector(g_mat)
  n_con <- NT + NC
  con_mat <- matrix(0, nrow = n_con, ncol = n_vars)
  for (i in seq_len(NT)) con_mat[i, ((i-1)*NC+1):(i*NC)] <- 1
  for (j in seq_len(NC)) con_mat[NT+j, seq(j, n_vars, by=NC)] <- 1
  rhs <- c(rep(1/NT, NT), rep(1/NC, NC))
  res <- lp(direction=mode, objective=obj,
            const.mat=con_mat, const.dir=rep("=",n_con), const.rhs=rhs)
  if (res$status != 0) return(NA_real_)
  res$objval
}

# ── バウンド計算 ──────────────────────────────────────────────────────────────
compute_bounds <- function(y1, y0) {
  acde <- mean(y1) - mean(y0)
  lp2  <- function(gf, mo) cot_lp(y1, y0, outer(y1, y0, Vectorize(gf)), mo)
  
  # ── ICDE ──────────────────────────────────────────────────────────────────
  
  # E^U[(Y1-Y0)^2] と E^L[(Y1-Y0)^2]  (Corollary 1, sharp)
  eu_icde2 <- lp2(function(a,b)(a-b)^2, "min")   # comonotonic   = E^U
  el_icde2 <- lp2(function(a,b)(a-b)^2, "max")   # countercomono = E^L
  
  # Variance (Corollary 1)
  var_icde_ub <- el_icde2 - acde^2                # sharp UB
  var_icde_lb <- max(eu_icde2 - acde^2, 0)        # sharp LB
  
  # 3次中心モーメント (Corollary 3 に対応, not sharp)
  mu3_ub <- lp2(function(a,b)(a-b-acde)^3, "max")
  mu3_lb <- lp2(function(a,b)(a-b-acde)^3, "min")
  
  # 4次中心モーメント (Corollary 2, sharp)
  mu4_ub <- lp2(function(a,b)(a-b-acde)^4, "max")   # E^L = UB
  mu4_lb <- lp2(function(a,b)(a-b-acde)^4, "min")   # E^U = LB
  
  # 歪度バウンド (Appendix B Eq.60, not sharp)
  skew_icde_ub <- ifelse(!is.na(mu3_ub) && mu3_ub >= 0, mu3_ub / (var_icde_lb^1.5 + 1e-12),
                         mu3_ub / (var_icde_ub^1.5 + 1e-12))
  skew_icde_lb <- ifelse(!is.na(mu3_lb) && mu3_lb <  0, mu3_lb / (var_icde_lb^1.5 + 1e-12),
                         mu3_lb / (var_icde_ub^1.5 + 1e-12))
  
  # 尖度バウンド (Appendix B Eq.61, not sharp)
  kurt_icde_ub <- mu4_ub / (var_icde_lb^2 + 1e-12)
  kurt_icde_lb <- mu4_lb / (var_icde_ub^2 + 1e-12)
  
  # ── ICRE ──────────────────────────────────────────────────────────────────
  
  # E^U[Y1/Y0] と E^L[Y1/Y0]  (Corollary 4, sharp)
  eu_icre1 <- lp2(function(a,b) a/b, "min")   # E^U = LB of Mean
  el_icre1 <- lp2(function(a,b) a/b, "max")   # E^L = UB of Mean
  
  mean_icre_ub <- el_icre1   # sharp UB
  mean_icre_lb <- eu_icre1   # sharp LB
  
  # E^U[(Y1/Y0)^2] と E^L[(Y1/Y0)^2]  (Corollary 4, sharp)
  eu_icre2 <- lp2(function(a,b)(a/b)^2, "min")
  el_icre2 <- lp2(function(a,b)(a/b)^2, "max")
  
  # Variance (Corollary 4)
  var_icre_ub <- el_icre2 - eu_icre1^2          # sharp UB
  var_icre_lb <- max(eu_icre2 - el_icre1^2, 0)  # sharp LB
  
  # 中心化の基準: E^U[Y1/Y0] = LB of mean を使用
  acre_ref <- eu_icre1
  
  # 3次中心モーメント of ICRE (not sharp)
  nu3_ub <- lp2(function(a,b)((a/b)-acre_ref)^3, "max")
  nu3_lb <- lp2(function(a,b)((a/b)-acre_ref)^3, "min")
  
  # 4次中心モーメント of ICRE (sharp)
  nu4_ub <- lp2(function(a,b)((a/b)-acre_ref)^4, "max")
  nu4_lb <- lp2(function(a,b)((a/b)-acre_ref)^4, "min")   # E^U = LB
  
  # 歪度バウンド (Appendix B Eq.60, not sharp)
  skew_icre_ub <- ifelse(!is.na(nu3_ub) && nu3_ub >= 0, nu3_ub / (var_icre_lb^1.5 + 1e-12),
                         nu3_ub / (var_icre_ub^1.5 + 1e-12))
  skew_icre_lb <- ifelse(!is.na(nu3_lb) && nu3_lb <  0, nu3_lb / (var_icre_lb^1.5 + 1e-12),
                         nu3_lb / (var_icre_ub^1.5 + 1e-12))
  
  # 尖度バウンド (Appendix B Eq.61, not sharp)
  kurt_icre_ub <- nu4_ub / (var_icre_lb^2 + 1e-12)
  kurt_icre_lb <- nu4_lb / (var_icre_ub^2 + 1e-12)
  
  c(acde         = acde,
    var_icde_ub  = var_icde_ub,  var_icde_lb  = var_icde_lb,
    skew_icde_ub = skew_icde_ub, skew_icde_lb = skew_icde_lb,
    kurt_icde_ub = kurt_icde_ub, kurt_icde_lb = kurt_icde_lb,
    mean_icre_ub = mean_icre_ub, mean_icre_lb = mean_icre_lb,
    var_icre_ub  = var_icre_ub,  var_icre_lb  = var_icre_lb,
    skew_icre_ub = skew_icre_ub, skew_icre_lb = skew_icre_lb,
    kurt_icre_ub = kurt_icre_ub, kurt_icre_lb = kurt_icre_lb)
}

# ── ブートストラップ ──────────────────────────────────────────────────────────
run_bootstrap <- function(y1, y0, B = 200, label = "",
                          boot_size1 = length(y1),
                          boot_size0 = length(y0)) {
  cat(sprintf("\n  %s  (NT=%d, NC=%d, B=%d)\n",
              label, length(y1), length(y0), B))
  cat(strrep("─", 72), "\n")
  
  boot_list <- lapply(seq_len(B), function(b) {
    y1b <- sample(y1, boot_size1, replace=TRUE)
    y0b <- sample(y0, boot_size0, replace=TRUE)
    res <- compute_bounds(y1b, y0b)
    if (anyNA(res)) return(NULL)
    res
  })
  valid_list <- Filter(Negate(is.null), boot_list)
  cat(sprintf("  (有効サンプル数: %d / %d)\n", length(valid_list), B))
  if (length(valid_list) == 0) {
    cat("  有効なブートストラップサンプルがありません\n")
    return(invisible(NULL))
  }
  boot_mat <- do.call(rbind, valid_list)
  
  fmt_val <- function(x) {
    if (is.na(x)) return("NA")
    formatC(x, format="e", digits=3)
  }
  
  fmt <- function(lbl, key) {
    vals <- tryCatch(as.numeric(boot_mat[, key]), error=function(e) NA_real_)
    m  <- tryCatch(mean(vals, na.rm=TRUE),     error=function(e) NA_real_)
    lo <- tryCatch(quantile(vals, 0.025, na.rm=TRUE), error=function(e) NA_real_)
    hi <- tryCatch(quantile(vals, 0.975, na.rm=TRUE), error=function(e) NA_real_)
    cat(sprintf("  %-42s  %s [%s, %s]\n",
                lbl, fmt_val(m), fmt_val(lo), fmt_val(hi)))
  }
  
  cat(sprintf("  %-42s  %s\n", "Statistics", "Mean [95% CI]"))
  cat(strrep("─", 72), "\n")
  
  cat("  --- ICDE ---\n")
  fmt("Mean of ICDE",                      "acde")
  fmt("UB of Variance of ICDE (COT) (✓)",  "var_icde_ub")
  fmt("LB of Variance of ICDE (COT) (✓)",  "var_icde_lb")
  fmt("UB of Skewness of ICDE (COT)",      "skew_icde_ub")
  fmt("LB of Skewness of ICDE (COT)",      "skew_icde_lb")
  fmt("UB of Kurtosis of ICDE (COT) (✓)",  "kurt_icde_ub")
  fmt("LB of Kurtosis of ICDE (COT)",    "kurt_icde_lb")
  
  cat("  --- ICRE ---\n")
  fmt("UB of Mean of ICRE (COT) (✓)",      "mean_icre_ub")
  fmt("LB of Mean of ICRE (COT) (✓)",      "mean_icre_lb")
  fmt("UB of Variance of ICRE (COT) (✓)",  "var_icre_ub")
  fmt("LB of Variance of ICRE (COT) (✓)",  "var_icre_lb")
  fmt("UB of Skewness of ICRE (COT)",      "skew_icre_ub")
  fmt("LB of Skewness of ICRE (COT)",      "skew_icre_lb")
  fmt("UB of Kurtosis of ICRE (COT)",      "kurt_icre_ub")
  fmt("LB of Kurtosis of ICRE (COT)",      "kurt_icre_lb")
  
  invisible(boot_mat)
}

# ══════════════════════════════════════════════════════════════════════════════
# 1. Cholesterol Reduction Dataset
# ══════════════════════════════════════════════════════════════════════════════
cat("\n", strrep("═", 72), "\n")
cat("  Cholesterol Reduction Dataset\n")
cat(strrep("═", 72), "\n")

cholesterol <- read_csv("cholesterol.csv", show_col_types=FALSE)
X <- cholesterol$trt
Y <- cholesterol$response

pairs_chol <- list(
  list(label="Y2 - Y1  /  Y2/Y1", x1="2times", x0="1time"),
  list(label="Y4 - Y2  /  Y4/Y2", x1="4times", x0="2times"),
  list(label="Y4 - Y1  /  Y4/Y1", x1="4times", x0="1time")
)

for (p in pairs_chol) {
  y1 <- Y[X == p$x1]
  y0 <- Y[X == p$x0]
  run_bootstrap(y1, y0, B=200, label=p$label)
}

# ══════════════════════════════════════════════════════════════════════════════
# 2. Lalonde Dataset
# ══════════════════════════════════════════════════════════════════════════════
cat("\n\n", strrep("═", 72), "\n")
cat("  Lalonde Dataset\n")
cat(strrep("═", 72), "\n")

data(lalonde)
D <- lalonde

y1_la   <- D$re78[D$treat == 1]
y0_la   <- D$re78[D$treat == 0]
y1_la_r <- y1_la + 1   # ICRE: +1 で 0 除算を回避（論文と同じ処理）
y0_la_r <- y0_la + 1

cat(sprintf("  N(treated)=%d, N(control)=%d\n", length(y1_la), length(y0_la)))

run_bootstrap(y1_la_r, y0_la_r, B=50, label="Y1 - Y0  /  Y1/Y0")

cat("\nDone.\n")
if (!requireNamespace("lpSolve", quietly=TRUE))
  install.packages("lpSolve", repos="https://cloud.r-project.org")
if (!requireNamespace("tictoc", quietly=TRUE))
  install.packages("tictoc", repos="https://cloud.r-project.org")
library(lpSolve)
library(tictoc)

set.seed(42)

# ── COT LP solver ─────────────────────────────────────────────────────────────
cot_lp <- function(y1, y0, g_mat, mode = c("max","min")) {
  mode <- match.arg(mode)
  NT <- length(y1); NC <- length(y0)
  n_vars <- NT * NC
  obj <- as.vector(g_mat)
  n_con <- NT + NC
  con_mat <- matrix(0, nrow = n_con, ncol = n_vars)
  for (i in seq_len(NT)) {
    idx <- ((i-1)*NC + 1):(i*NC)
    con_mat[i, idx] <- 1
  }
  for (j in seq_len(NC)) {
    idx <- seq(j, n_vars, by = NC)
    con_mat[NT + j, idx] <- 1
  }
  rhs <- c(rep(1/NT, NT), rep(1/NC, NC))
  res <- lp(direction=mode, objective=obj,
            const.mat=con_mat, const.dir=rep("=",n_con), const.rhs=rhs)
  if (res$status != 0) { warning("LP did not converge"); return(NA_real_) }
  res$objval
}

cot_ub_lb <- function(y1, y0, g_fn, stat_type) {
  g_mat   <- outer(y1, y0, Vectorize(g_fn))
  val_max <- cot_lp(y1, y0, g_mat, "max")
  val_min <- cot_lp(y1, y0, g_mat, "min")
  c(UB = val_max, LB = val_min)
}

# ── Data generators ───────────────────────────────────────────────────────────
draw_groups <- function(raw_fn, NC, NT, mult = 6L) {
  y1_pool <- y0_pool <- numeric(0)
  while (length(y1_pool) < NT || length(y0_pool) < NC) {
    d <- raw_fn((NC + NT) * mult)
    y1_pool <- c(y1_pool, d$y1)
    y0_pool <- c(y0_pool, d$y0)
    mult <- mult * 2L
  }
  list(y1 = y1_pool[seq_len(NT)], y0 = y0_pool[seq_len(NC)])
}

raw_scmB <- function(n) {
  UY <- runif(n,-1,1); X <- rbinom(n,1,.5)
  Y  <- ifelse(X==1, 4*UY^2, ifelse(UY>0, -UY^2, 0))
  list(y1=Y[X==1], y0=Y[X==0])
}
raw_scmD <- function(n) {
  UY <- runif(n,-1,1); X <- rbinom(n,1,.5)
  Y  <- ifelse(X==1, 6*UY^2+2, ifelse(UY>0.75, UY^2+2, 2))
  list(y1=Y[X==1], y0=Y[X==0])
}

gen_scmB <- function(NC,NT) draw_groups(raw_scmB, NC, NT)
gen_scmD <- function(NC,NT) draw_groups(raw_scmD, NC, NT)

# ── Timing: 1 simulation per N ────────────────────────────────────────────────
SIZES <- c(20, 100, 1000, 2000, 5000)

cat("\n", strrep("═", 60), "\n")
cat("  Timing: 1 simulation per N  (SCM B, 2nd M. ICDE)\n")
cat(strrep("═", 60), "\n")
cat(sprintf("  %-8s  %-10s  %-10s  %s\n", "N", "LP vars", "time (s)", "UB / LB"))
cat(strrep("─", 60), "\n")

for (N in SIZES) {
  NC <- NT <- N %/% 2
  d  <- gen_scmB(NC, NT)
  
  tic(paste0("N=", N))
  res <- cot_ub_lb(d$y1, d$y0, function(a,b)(a-b)^2, "icde_even")
  t   <- toc(quiet = TRUE)
  
  elapsed <- t$toc - t$tic
  cat(sprintf("  %-8d  %-10d  %-10.3f  UB=%.4f  LB=%.4f\n",
              N, NC*NT, elapsed, res["UB"], res["LB"]))
}

cat("\n", strrep("═", 60), "\n")
cat("  Timing: 1 simulation per N  (SCM D, 1st M. ICRE)\n")
cat(strrep("═", 60), "\n")
cat(sprintf("  %-8s  %-10s  %-10s  %s\n", "N", "LP vars", "time (s)", "UB / LB"))
cat(strrep("─", 60), "\n")

for (N in SIZES) {
  NC <- NT <- N %/% 2
  d  <- gen_scmD(NC, NT)
  
  tic(paste0("N=", N))
  res <- cot_ub_lb(d$y1, d$y0, function(a,b)(a/b)^1, "icre")
  t   <- toc(quiet = TRUE)
  
  elapsed <- t$toc - t$tic
  cat(sprintf("  %-8d  %-10d  %-10.3f  UB=%.4f  LB=%.4f\n",
              N, NC*NT, elapsed, res["UB"], res["LB"]))
}

cat(strrep("─", 60), "\n")
cat("  注: time (s) = 1回のシミュレーションの計算時間\n")
cat("      n_sim=100 の場合は time x 100 が目安\n")
cat(strrep("─", 60), "\n")
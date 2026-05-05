if (!requireNamespace("lpSolve", quietly=TRUE))
  install.packages("lpSolve", repos="https://cloud.r-project.org")
library(lpSolve)

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
  # submodular (icde_even, icre): max=E^L=UB, min=E^U=LB
  # supermodular (icde_odd)     : max=E^U=UB, min=E^L=LB
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

# ── Ground truths ─────────────────────────────────────────────────────────────
cat("Computing ground truths (N=2M)...\n")
N_pop <- 2e6

UY <- runif(N_pop,-1,1)
truth_B <- sapply(2:4, function(m) mean((4*UY^2 - ifelse(UY>0,-UY^2,0))^m))

UY <- runif(N_pop,-1,1)
y1D <- 6*UY^2+2; y0D <- ifelse(UY>0.75,UY^2+2,2)
truth_D <- sapply(1:4, function(m) mean((y1D/y0D)^m))

cat(sprintf("SCM B (2nd,3rd,4th M. ICDE): %.4f  %.4f  %.4f\n",  truth_B[1],truth_B[2],truth_B[3]))
cat(sprintf("SCM D (1st-4th M. ICRE):     %.4f  %.4f  %.4f  %.4f\n", truth_D[1],truth_D[2],truth_D[3],truth_D[4]))

# ── Simulation engine ─────────────────────────────────────────────────────────
SIZES <- c(20, 100, 1000)

run_cot_sim <- function(gen_fn, g_fn, stat_type, true_val, n_sim = 100) {
  lapply(SIZES, function(N) {
    NC <- NT <- N %/% 2
    cat(sprintf("  N=%d (n_sim=%d)...\n", N, n_sim))
    ub_v <- lb_v <- numeric(n_sim)
    for (s in seq_len(n_sim)) {
      d   <- gen_fn(NC, NT)
      res <- cot_ub_lb(d$y1, d$y0, g_fn, stat_type)
      ub_v[s] <- res["UB"]
      lb_v[s] <- res["LB"]
    }
    ci_ub <- quantile(ub_v, c(.025,.975), na.rm=TRUE)
    ci_lb <- quantile(lb_v, c(.025,.975), na.rm=TRUE)
    data.frame(
      N       = N,
      UB_mean = mean(ub_v, na.rm=TRUE),
      UB_lo   = ci_ub[1], UB_hi = ci_ub[2],
      LB_mean = mean(lb_v, na.rm=TRUE),
      LB_lo   = ci_lb[1], LB_hi = ci_lb[2],
      TrueVal = true_val,
      UB_ok   = mean(ub_v, na.rm=TRUE) >= true_val,
      LB_ok   = mean(lb_v, na.rm=TRUE) <= true_val
    )
  }) |> do.call(what=rbind)
}

# ── Pretty printer ────────────────────────────────────────────────────────────
fmt <- function(m,lo,hi) sprintf("%.3f [%.3f, %.3f]", m, lo, hi)

print_table <- function(res_list, stat_labels, true_vals, title, col_hdr) {
  w <- 26L
  total_w <- 40 + (w+2)*length(SIZES) + 10
  cat("\n", strrep("═", total_w), "\n")
  cat(sprintf("  %s\n", title))
  cat(strrep("═", total_w), "\n")
  hdr <- paste(sprintf("%-*s", w, paste0("N=",SIZES)), collapse="  ")
  cat(sprintf("  %-40s  %s  %s\n", "Statistics", hdr, col_hdr))
  cat(strrep("─", total_w), "\n")
  for (i in seq_along(res_list)) {
    res <- res_list[[i]]; lbl <- stat_labels[i]; tv <- true_vals[i]
    for (bnd in c("UB","LB")) {
      rows <- sapply(SIZES, function(n) {
        r <- res[res$N==n,]
        if (bnd=="UB") fmt(r$UB_mean, r$UB_lo, r$UB_hi)
        else           fmt(r$LB_mean, r$LB_lo, r$LB_hi)
      })
      ok   <- if (bnd=="UB") all(res$UB_ok) else all(res$LB_ok)
      tick <- if (ok) " (✓)" else " (!)"
      vals <- paste(sprintf("%-*s", w, rows), collapse="  ")
      cat(sprintf("  %-40s  %s  %.4f\n",
                  sprintf("%s of %s (COT-LP)%s", bnd, lbl, tick),
                  vals, tv))
    }
    cat(strrep("─", total_w), "\n")
  }
}

################################################################################
# TABLE 5  SCM(B) — ICDE bounds
################################################################################
cat("\nRunning Table 5 (SCM B)...\n")
res5 <- list(
  run_cot_sim(gen_scmB, function(a,b)(a-b)^2, "icde_even", truth_B[1]),
  run_cot_sim(gen_scmB, function(a,b)(a-b)^3, "icde_odd",  truth_B[2]),
  run_cot_sim(gen_scmB, function(a,b)(a-b)^4, "icde_even", truth_B[3])
)
print_table(res5, c("2nd M. ICDE","3rd M. ICDE","4th M. ICDE"), truth_B,
            "TABLE 5 COT-LP: SCM(B) — ICDE bounds", "True M. value")

################################################################################
# TABLE 7  SCM(D) — ICRE bounds
################################################################################
cat("\nRunning Table 7 (SCM D)...\n")
res7 <- lapply(1:4, function(m)
  run_cot_sim(gen_scmD, function(a,b)(a/b)^m, "icre", truth_D[m])
)
print_table(res7, paste0(c("1st","2nd","3rd","4th")," M. ICRE (bound)"), truth_D,
            "TABLE 7 COT-LP: SCM(D) — ICRE bounds", "True M. value")

cat("\nDone.\n")
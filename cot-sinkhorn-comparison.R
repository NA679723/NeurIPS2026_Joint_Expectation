################################################################################
# Sinkhorn (log-domain) COT Bounds — Tables 5 and 7
#
# 数値安定版: log ドメインで反復
#   exp(-C/eps) のオーバーフローを避けるため
#   u, v を log スケールで保持
################################################################################

set.seed(42)

# ── Log-domain Sinkhorn ───────────────────────────────────────────────────────
log_sum_exp <- function(x) {
  m <- max(x, na.rm=TRUE)
  if (!is.finite(m)) return(-Inf)
  m + log(sum(exp(x - m), na.rm=TRUE))
}

sinkhorn_log <- function(cost_mat, a, b,
                         eps = 0.5, max_iter = 500, tol = 1e-6) {
  NT <- length(a); NC <- length(b)
  log_a  <- log(a); log_b <- log(b)
  M      <- -cost_mat / eps          # NT x NC
  
  # log スケールのスケーリング変数
  log_u <- rep(0, NT)
  log_v <- rep(0, NC)
  
  for (iter in seq_len(max_iter)) {
    # log_v update: log_v[j] = log_b[j] - log_sum_exp(M[,j] + log_u)
    log_v_new <- log_b - apply(M + log_u, 2, log_sum_exp)
    # log_u update: log_u[i] = log_a[i] - log_sum_exp(M[i,] + log_v_new)
    log_u_new <- log_a - apply(M + rep(log_v_new, each=NT), 1, log_sum_exp)
    
    if (all(is.finite(log_u_new)) &&
        max(abs(log_u_new - log_u)) < tol) {
      log_u <- log_u_new; log_v <- log_v_new; break
    }
    log_u <- log_u_new; log_v <- log_v_new
  }
  
  # 輸送計画: log_pi[i,j] = M[i,j] + log_u[i] + log_v[j]
  log_pi <- M + log_u + rep(log_v, each=NT)
  pi_mat <- exp(log_pi)
  pi_mat[!is.finite(pi_mat)] <- 0
  
  sum(cost_mat * pi_mat)
}

cot_sinkhorn_ub_lb <- function(y1, y0, g_fn, eps = 0.5) {
  NT <- length(y1); NC <- length(y0)
  a  <- rep(1/NT, NT)
  b  <- rep(1/NC, NC)
  g_mat <- outer(y1, y0, Vectorize(g_fn))
  
  val_max <- -sinkhorn_log(-g_mat, a, b, eps)   # max
  val_min <-  sinkhorn_log( g_mat, a, b, eps)   # min
  
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

cat(sprintf("SCM B (2nd,3rd,4th M. ICDE): %.4f  %.4f  %.4f\n",
            truth_B[1],truth_B[2],truth_B[3]))
cat(sprintf("SCM D (1st-4th M. ICRE):     %.4f  %.4f  %.4f  %.4f\n",
            truth_D[1],truth_D[2],truth_D[3],truth_D[4]))

# ── Simulation engine ─────────────────────────────────────────────────────────
SIZES <- c(20, 100, 1000)
EPS   <- 100

run_sinkhorn_sim <- function(gen_fn, g_fn, stat_type, true_val,
                             n_sim = 100, eps = EPS) {
  lapply(SIZES, function(N) {
    NC <- NT <- N %/% 2
    cat(sprintf("  N=%d (n_sim=%d, eps=%.3f)...\n", N, n_sim, eps))
    ub_v <- lb_v <- numeric(n_sim)
    for (s in seq_len(n_sim)) {
      d        <- gen_fn(NC, NT)
      res      <- cot_sinkhorn_ub_lb(d$y1, d$y0, g_fn, eps=eps)
      ub_v[s]  <- res["UB"]
      lb_v[s]  <- res["LB"]
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
                  sprintf("%s of %s (Sinkhorn)%s", bnd, lbl, tick),
                  vals, tv))
    }
    cat(strrep("─", total_w), "\n")
  }
}

################################################################################
# TABLE 5  SCM(B)
################################################################################
cat("\nRunning Table 5 (SCM B)...\n")
res5 <- list(
  run_sinkhorn_sim(gen_scmB, function(a,b)(a-b)^2, "icde_even", truth_B[1]),
  run_sinkhorn_sim(gen_scmB, function(a,b)(a-b)^3, "icde_odd",  truth_B[2]),
  run_sinkhorn_sim(gen_scmB, function(a,b)(a-b)^4, "icde_even", truth_B[3])
)
print_table(res5, c("2nd M. ICDE","3rd M. ICDE","4th M. ICDE"), truth_B,
            "TABLE 5 Sinkhorn: SCM(B) — ICDE bounds", "True M. value")

################################################################################
# TABLE 7  SCM(D)
################################################################################
cat("\nRunning Table 7 (SCM D)...\n")
res7 <- lapply(1:4, function(m)
  run_sinkhorn_sim(gen_scmD, function(a,b)(a/b)^m, "icre", truth_D[m])
)
print_table(res7, paste0(c("1st","2nd","3rd","4th")," M. ICRE (bound)"), truth_D,
            "TABLE 7 Sinkhorn: SCM(D) — ICRE bounds", "True M. value")

cat(sprintf("\nDone.  (eps=%.3f)\n", EPS))
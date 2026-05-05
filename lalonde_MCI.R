# MCI estimator for LaLonde dataset
# Kawakami and Tian [2025]
# Table 12 に追加する数値を出力

library(designmatch)
data(lalonde)

df <- lalonde
Y  <- df$re78
X  <- df$treat

Sim <- 100
Ite <- 1000

R2  <- numeric(Sim)
R2L <- numeric(Sim)
R3  <- numeric(Sim)
R3L <- numeric(Sim)
R4  <- numeric(Sim)
R4L <- numeric(Sim)

Rskew_U <- numeric(Sim)
Rskew_L <- numeric(Sim)
Rkurt_U <- numeric(Sim)
Rkurt_L <- numeric(Sim)

for (sim in 1:Sim) {
  
  idx <- sample(length(Y), replace = TRUE)
  Yb  <- Y[idx]
  Xb  <- X[idx]
  
  a2  <- 0; a2L <- 0
  a3  <- 0; a3L <- 0
  a4  <- 0; a4L <- 0
  
  for (r in 1:Ite) {
    
    y_1 <- runif(1, min(Yb), max(Yb))
    y_2 <- runif(1, min(Yb), max(Yb))
    y_3 <- runif(1, min(Yb), max(Yb))
    y_4 <- runif(1, min(Yb), max(Yb))
    
    # second moment (upper)
    a2 <- a2 +
      (min(min(mean(subset(as.numeric(Yb < y_1), Xb == 0)),
               mean(subset(as.numeric(Yb < y_2), Xb == 0))),
           1 - max(mean(subset(as.numeric(Yb < y_1), Xb == 1)),
                   mean(subset(as.numeric(Yb < y_2), Xb == 1)))) +
         min(min(mean(subset(as.numeric(Yb < y_1), Xb == 1)),
                 mean(subset(as.numeric(Yb < y_2), Xb == 1))),
             1 - max(mean(subset(as.numeric(Yb < y_1), Xb == 0)),
                     mean(subset(as.numeric(Yb < y_2), Xb == 0)))))
    
    # second moment (lower)
    a2L <- a2L +
      max(sum(mean(subset(as.numeric(Yb < y_1), Xb == 0)),
              mean(subset(as.numeric(Yb < y_2), Xb == 0))) -
            sum(mean(subset(as.numeric(Yb < y_1), Xb == 1)),
                mean(subset(as.numeric(Yb < y_2), Xb == 1))) - 1, 0) +
      max(sum(mean(subset(as.numeric(Yb < y_1), Xb == 1)),
              mean(subset(as.numeric(Yb < y_2), Xb == 1))) -
            sum(mean(subset(as.numeric(Yb < y_1), Xb == 0)),
                mean(subset(as.numeric(Yb < y_2), Xb == 0))) - 1, 0)
    
    # third moment (upper)
    a3 <- a3 +
      (min(min(mean(subset(as.numeric(Yb < y_1), Xb == 0)),
               mean(subset(as.numeric(Yb < y_2), Xb == 0)),
               mean(subset(as.numeric(Yb < y_3), Xb == 0))),
           1 - max(mean(subset(as.numeric(Yb < y_1), Xb == 1)),
                   mean(subset(as.numeric(Yb < y_2), Xb == 1)),
                   mean(subset(as.numeric(Yb < y_3), Xb == 1)))) -
         max(sum(mean(subset(as.numeric(Yb < y_1), Xb == 1)),
                 mean(subset(as.numeric(Yb < y_2), Xb == 1)),
                 mean(subset(as.numeric(Yb < y_3), Xb == 1))) -
               sum(mean(subset(as.numeric(Yb < y_1), Xb == 0)),
                   mean(subset(as.numeric(Yb < y_2), Xb == 0)),
                   mean(subset(as.numeric(Yb < y_3), Xb == 0))) - 2, 0))
    
    # third moment (lower)
    a3L <- a3L +
      max(sum(mean(subset(as.numeric(Yb < y_1), Xb == 0)),
              mean(subset(as.numeric(Yb < y_2), Xb == 0)),
              mean(subset(as.numeric(Yb < y_3), Xb == 0))) -
            sum(mean(subset(as.numeric(Yb < y_1), Xb == 1)),
                mean(subset(as.numeric(Yb < y_2), Xb == 1)),
                mean(subset(as.numeric(Yb < y_3), Xb == 1))) - 2, 0) -
      min(min(mean(subset(as.numeric(Yb < y_1), Xb == 1)),
              mean(subset(as.numeric(Yb < y_2), Xb == 1)),
              mean(subset(as.numeric(Yb < y_3), Xb == 1))),
          1 - max(mean(subset(as.numeric(Yb < y_1), Xb == 0)),
                  mean(subset(as.numeric(Yb < y_2), Xb == 0)),
                  mean(subset(as.numeric(Yb < y_3), Xb == 0))))
    
    # fourth moment (upper)
    a4 <- a4 +
      (min(min(mean(subset(as.numeric(Yb < y_1), Xb == 0)),
               mean(subset(as.numeric(Yb < y_2), Xb == 0)),
               mean(subset(as.numeric(Yb < y_3), Xb == 0)),
               mean(subset(as.numeric(Yb < y_4), Xb == 0))),
           1 - max(mean(subset(as.numeric(Yb < y_1), Xb == 1)),
                   mean(subset(as.numeric(Yb < y_2), Xb == 1)),
                   mean(subset(as.numeric(Yb < y_3), Xb == 1)),
                   mean(subset(as.numeric(Yb < y_4), Xb == 1)))) +
         min(min(mean(subset(as.numeric(Yb < y_1), Xb == 1)),
                 mean(subset(as.numeric(Yb < y_2), Xb == 1)),
                 mean(subset(as.numeric(Yb < y_3), Xb == 1)),
                 mean(subset(as.numeric(Yb < y_4), Xb == 1))),
             1 - max(mean(subset(as.numeric(Yb < y_1), Xb == 0)),
                     mean(subset(as.numeric(Yb < y_2), Xb == 0)),
                     mean(subset(as.numeric(Yb < y_3), Xb == 0)),
                     mean(subset(as.numeric(Yb < y_4), Xb == 0)))))
    
    # fourth moment (lower)
    a4L <- a4L +
      max(sum(mean(subset(as.numeric(Yb < y_1), Xb == 0)),
              mean(subset(as.numeric(Yb < y_2), Xb == 0)),
              mean(subset(as.numeric(Yb < y_3), Xb == 0)),
              mean(subset(as.numeric(Yb < y_4), Xb == 0))) -
            sum(mean(subset(as.numeric(Yb < y_1), Xb == 1)),
                mean(subset(as.numeric(Yb < y_2), Xb == 1)),
                mean(subset(as.numeric(Yb < y_3), Xb == 1)),
                mean(subset(as.numeric(Yb < y_4), Xb == 1))) - 3, 0) +
      max(sum(mean(subset(as.numeric(Yb < y_1), Xb == 1)),
              mean(subset(as.numeric(Yb < y_2), Xb == 1)),
              mean(subset(as.numeric(Yb < y_3), Xb == 1)),
              mean(subset(as.numeric(Yb < y_4), Xb == 1))) -
            sum(mean(subset(as.numeric(Yb < y_1), Xb == 0)),
                mean(subset(as.numeric(Yb < y_2), Xb == 0)),
                mean(subset(as.numeric(Yb < y_3), Xb == 0)),
                mean(subset(as.numeric(Yb < y_4), Xb == 0))) - 3, 0)
  }
  
  span <- max(Yb) - min(Yb)
  
  mu2_U <- a2  / Ite * span^2
  mu2_L <- a2L / Ite * span^2
  mu3_U <- a3  / Ite * span^3
  mu3_L <- a3L / Ite * span^3
  mu4_U <- a4  / Ite * span^4
  mu4_L <- a4L / Ite * span^4
  
  R2[sim]  <- mu2_U
  R2L[sim] <- mu2_L
  R3[sim]  <- mu3_U
  R3L[sim] <- mu3_L
  R4[sim]  <- mu4_U
  R4L[sim] <- mu4_L
  
  # Skewness / Kurtosis per sim (Eqs. 57-58)
  Rskew_U[sim] <- if (mu3_U >= 0) mu3_U / (mu2_L + 0.001)^1.5 else mu3_U / (mu2_U + 0.001)^1.5
  Rskew_L[sim] <- if (mu3_L >= 0) mu3_L / (mu2_U + 0.001)^1.5 else mu3_L / (mu2_L + 0.001)^1.5
  Rkurt_U[sim] <- mu4_U / (mu2_L + 0.001)^2 - 3
  Rkurt_L[sim] <- mu4_L / (mu2_U + 0.001)^2 - 3
}

# ── Table 12 output ───────────────────────────────────────────────────────────

fmt <- function(R) {
  sprintf("%.3e [%.3e, %.3e]", mean(R), quantile(R, 0.025), quantile(R, 0.975))
}

cat("=== Table 12: MCI results (Y1 - Y0) ===\n\n")
cat("UB of Variance of ICDE (MCI):  ", fmt(R2),       "\n")
cat("LB of Variance of ICDE (MCI):  ", fmt(R2L),      "\n")
cat("UB of Skewness of ICDE (MCI):  ", fmt(Rskew_U),  "\n")
cat("LB of Skewness of ICDE (MCI):  ", fmt(Rskew_L),  "\n")
cat("UB of Kurtosis of ICDE (MCI):  ", fmt(Rkurt_U),  "\n")
cat("LB of Kurtosis of ICDE (MCI):  ", fmt(Rkurt_L),  "\n")
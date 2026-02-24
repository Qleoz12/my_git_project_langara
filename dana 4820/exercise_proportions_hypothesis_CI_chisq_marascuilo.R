############################################################
# exercise_proportions_hypothesis_CI_chisq_marascuilo.R
# One proportion test + Two proportions + Chi-square + Marascuilo
############################################################

options(digits = 6)

cat("=========================================================
")
cat("1) ONE-PROPORTION HYPOTHESIS TEST (Marijuana example)
")
cat("=========================================================
")

# Given: n = 200, sample proportion p_hat = 0.59, H0: p = 0.50, Ha: p > 0.50
n <- 200
p_hat <- 0.59
p0 <- 0.50
alpha <- 0.05

# z test statistic (large-sample z test)
se0 <- sqrt(p0 * (1 - p0) / n)
z <- (p_hat - p0) / se0
p_value_right <- 1 - pnorm(z)

cat("Inputs: n =", n, " p_hat =", p_hat, " p0 =", p0, " alpha =", alpha, "
")
cat("SE under H0 =", se0, "
")
cat("z =", z, "
")
cat("p-value (right tail) =", p_value_right, "
")
cat("Decision:", ifelse(p_value_right < alpha, "Reject H0", "Do NOT reject H0"), "

")

# Using built-in test (prop.test uses chi-square approximation; disable continuity correction)
x <- round(p_hat * n)  # 0.59*200=118 exactly, but rounding safe
test1 <- prop.test(x = x, n = n, p = p0, alternative = "greater", correct = FALSE)
print(test1)

cat("
=========================================================
")
cat("2) TWO-PROPORTION TEST + 90% CI (College A vs College B smoking)
")
cat("=========================================================
")

# Given: College A: n1=80, p1=0.20; College B: n2=120, p2=0.10
n1 <- 80;  p1 <- 0.20
n2 <- 120; p2 <- 0.10

x1 <- p1 * n1  # 16
x2 <- p2 * n2  # 12

# Combined (pooled) proportion for hypothesis test H0: pA = pB
p_pool <- (x1 + x2) / (n1 + n2)

# z test statistic for Ha: pA > pB
se_pool <- sqrt(p_pool * (1 - p_pool) * (1/n1 + 1/n2))
z2 <- (p1 - p2) / se_pool
p_value_right_2prop <- 1 - pnorm(z2)

cat("Counts: x1 =", x1, "of", n1, " x2 =", x2, "of", n2, "
")
cat("p_pool =", p_pool, "
")
cat("z =", z2, "
")
cat("p-value (right tail) =", p_value_right_2prop, "

")

# 90% CI for (p1 - p2) uses unpooled SE
conf_level <- 0.90
alpha_ci <- 1 - conf_level
zcrit_90 <- qnorm(1 - alpha_ci/2)  # 1.645...
se_unpooled <- sqrt(p1*(1-p1)/n1 + p2*(1-p2)/n2)
ME <- zcrit_90 * se_unpooled
ci_90 <- c((p1 - p2) - ME, (p1 - p2) + ME)

cat("90% CI for (p1 - p2):
")
cat("zcrit =", zcrit_90, "
")
cat("SE(unpooled) =", se_unpooled, "
")
cat("ME =", ME, "
")
cat("CI =", ci_90, "

")

# Built-in two-proportion test (prop.test)
test2 <- prop.test(x = c(x1, x2), n = c(n1, n2),
                   alternative = "greater", correct = FALSE)
print(test2)

cat("
=========================================================
")
cat("3) CHI-SQUARE TEST (3 countries iPhone usage) + p-value
")
cat("=========================================================
")

# Given in slides:
# Country A: 210/300 => 0.70
# Country B: 260/400 => 0.65
# Country C: 250/500 => 0.50

iphone <- c(A = 210, B = 260, C = 250)
n_cty <- c(A = 300, B = 400, C = 500)
not_iphone <- n_cty - iphone

two_way <- rbind(Using_iPhone = iphone,
                 Not_using_iPhone = not_iphone)

cat("Two-way table:
")
print(two_way)

chisq_res <- chisq.test(two_way, correct = FALSE)
cat("
Chi-square test output:
")
print(chisq_res)

# Manual p-value from chi-square statistic
chisq_stat <- unname(chisq_res$statistic)
df <- unname(chisq_res$parameter)
p_value_chisq <- pchisq(chisq_stat, df = df, lower.tail = FALSE)

cat("
Manual p-value check:
")
cat("chisq_stat =", chisq_stat, " df =", df, "
")
cat("p-value =", p_value_chisq, "

")

cat("=========================================================
")
cat("4) MARASCUILO PROCEDURE (pairwise comparisons after chi-square)
")
cat("=========================================================
")

# Goal: compare pairs (A vs B), (A vs C), (B vs C) while controlling overall error rate
# Slides use: overall alpha = 0.05 and df = k-1 where k=3 => df=2
overall_alpha <- 0.05
k <- 3
df_mar <- k - 1
chi_crit <- qchisq(1 - overall_alpha, df = df_mar)  # e.g., 5.991
mult <- sqrt(chi_crit)                              # sqrt(5.991)

cat("Overall alpha =", overall_alpha, "
")
cat("df (k-1) =", df_mar, "
")
cat("Chi-square critical =", chi_crit, "
")
cat("sqrt(chi_crit) =", mult, "

")

# sample proportions
p <- iphone / n_cty

pairs <- list(c("A","B"), c("A","C"), c("B","C"))

marascuilo_ci <- lapply(pairs, function(pair) {
  i <- pair[1]; j <- pair[2]
  diff <- p[i] - p[j]
  se_diff <- sqrt(p[i]*(1-p[i])/n_cty[i] + p[j]*(1-p[j])/n_cty[j])
  ME_m <- mult * se_diff
  c(pair = paste(i, j, sep=" vs "),
    diff = diff,
    ME = ME_m,
    L = diff - ME_m,
    U = diff + ME_m)
})

marascuilo_df <- do.call(rbind, marascuilo_ci)
marascuilo_df <- as.data.frame(marascuilo_df, stringsAsFactors = FALSE)

# Convert numeric columns
for (col in c("diff","ME","L","U")) marascuilo_df[[col]] <- as.numeric(marascuilo_df[[col]])

cat("Marascuilo intervals (like CI bands):
")
print(marascuilo_df)

cat("
Interpretation rule:
")
cat("- If interval includes 0 => not enough evidence that proportions differ (at overall alpha).
")
cat("- If interval is all positive => first country proportion > second.
")
cat("- If interval is all negative => first country proportion < second.
")

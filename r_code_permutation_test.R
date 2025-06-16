library(ggplot2)
library(MatchIt)
library(ggpubr)
library(survminer)
library(survival)
library(cobalt)
library(dplyr)

data_firstvisit_index <- readRDS("data_firstvisit_index_time_final.rds")
data_firstvisit_index <- data_firstvisit_index[data_firstvisit_index$gender != 2, ]
data_firstvisit_index_pfic1 <- data_firstvisit_index[data_firstvisit_index$type_pfic == 0 & !is.na(data_firstvisit_index$type_pfic), ]
data_firstvisit_index_pfic2 <- data_firstvisit_index[data_firstvisit_index$type_pfic == 1 & !is.na(data_firstvisit_index$type_pfic), ]

data_firstvisit_index_pfic2 %>%
  count(nw_or_sc)       # 69 from nw_or_sc = 0 and 64 from nw_or_sc = 1

data_firstvisit_index_pfic2$nw_or_sc <- as.factor(data_firstvisit_index_pfic2$nw_or_sc)

p.denom <- glm(nw_or_sc ~ age+as.factor(gender)+log10_tsb_conv+log10_sba_conv+log10_alt_conv
               , data = data_firstvisit_index_pfic2, family = binomial(link="logit"))

summary(p.denom)
data_firstvisit_index_pfic2$pd <- predict(p.denom, data_firstvisit_index_pfic2, type="response")

#estimation of numerator of ip weights
p.num <- glm(nw_or_sc ~ 1, data = data_firstvisit_index_pfic2, family = binomial(link="logit"))
data_firstvisit_index_pfic2$pn <- predict(p.num, data_firstvisit_index_pfic2, type="response")

#computation of estimated weights
weights_stab <- ifelse(data_firstvisit_index_pfic2$nw_or_sc == 1, data_firstvisit_index_pfic2$pn/data_firstvisit_index_pfic2$pd , (1-data_firstvisit_index_pfic2$pn)/(1-data_firstvisit_index_pfic2$pd))
summary(weights_stab)
data_firstvisit_index_pfic2$IPTW <- weights_stab

data_firstvisit_index_pfic2$nw_or_sc <- as.numeric(as.character(data_firstvisit_index_pfic2$nw_or_sc))
data_firstvisit_index_pfic2$gender <- as.numeric(as.character(data_firstvisit_index_pfic2$gender))

# PERMUTATION TEST WITH IPTW WEIGHTS (FIRST VISIT INDEX TIME)
# Fit the original weighted Cox model
original_model_pfic2_firstvisit <- coxph(Surv(event_time_years, status) ~ nw_or_sc, data = data_firstvisit_index_pfic2, weights = IPTW)
original_logrank_pfic2_firstvisit <- original_model_pfic2_firstvisit$score

n_permutations <- 1000
permuted_logranks <- numeric(n_permutations)

# Permutation loop
set.seed(42)
for (i in 1:n_permutations){
  # permute the region label: add a new column with random grouping
  random_grouping <- c(rep(0,69), rep(1, 64))
  random_grouping <- sample(random_grouping)
  data_firstvisit_index_pfic2$random_grouping <- random_grouping
  
  # recalculate the IPTW weight
  #data_firstvisit_index_pfic2$random_grouping <- as.factor(data_firstvisit_index_pfic2$random_grouping)
  p.denom_perm <- glm(random_grouping ~ age+as.factor(gender)+log10_tsb_conv+log10_sba_conv+log10_alt_conv, data = data_firstvisit_index_pfic2, family = binomial(link="logit"))
  pd_perm <- predict(p.denom_perm, data_firstvisit_index_pfic2, type="response")
  p.num_perm <- glm(random_grouping ~ 1, data = data_firstvisit_index_pfic2, family = binomial(link="logit"))
  pn_perm <- predict(p.num_perm, data_firstvisit_index_pfic2, type="response")
  weights_stab_perm <- ifelse(data_firstvisit_index_pfic2$random_grouping == 1, pn_perm/pd_perm , (1-pn_perm)/(1-pd_perm))
  
  #recalculate the log-rank statistic on permuted data
  permuted_fit <- coxph(Surv(event_time_years, status) ~ random_grouping, data = data_firstvisit_index_pfic2, weights = weights_stab_perm)
  permuted_logranks[i] <- permuted_fit$score
}
# plot null distribution
hist(permuted_logranks, breaks = 100, main = "Histogram of Log-Rank Statistics \n of weighted model (PFIC2 with first visit as index time)", xlab = "Log-Rank statistic", col="lightblue", border="black")
abline(v = original_logrank_pfic2_firstvisit, col = 'red', lwd = 2, lty = 2)

# calculate permutation p-value
p_value <- mean(permuted_logranks >= original_logrank_pfic2_firstvisit)
cat("Original Log-Rank statistic:", original_logrank_pfic2_firstvisit, "\n")
cat("Permutation p-value:", p_value, "\n")

# PERMUTATION TEST WITHOUT WEIGHT #################

# Fit the original Cox model
original_model_pfic2_firstvisit_noweight <- coxph(Surv(event_time_years, status) ~ nw_or_sc, data = data_firstvisit_index_pfic2)
original_logrank_pfic2_firstvisit_noweight <- original_model_pfic2_firstvisit_noweight$score
summary(original_model_pfic2_firstvisit_noweight)

n_permutations <- 1000
permuted_logranks <- numeric(n_permutations)

# Permutation loop
set.seed(42)
for (i in 1:n_permutations){
  # permute the region label: add a new column with random grouping
  random_grouping <- c(rep(0,69), rep(1, 64))
  random_grouping <- sample(random_grouping)
  data_firstvisit_index_pfic2$random_grouping <- random_grouping
  
  #recalculate the log-rank statistic on permuted data
  permuted_fit <- coxph(Surv(event_time_years, status) ~ random_grouping, data = data_firstvisit_index_pfic2)
  permuted_logranks[i] <- permuted_fit$score
}

# plot null distribution
hist(permuted_logranks, breaks = 100, main = "Histogram of Log-Rank Statistics \n of unweighted model (PFIC2 with first visit as index time)", xlab = "Log-Rank statistic", col="lightblue", border="black")
abline(v = original_logrank_pfic2_firstvisit_noweight, col = 'red', lwd = 2, lty = 2)

# calculate permutation p-value
p_value <- mean(permuted_logranks >= original_logrank_pfic2_firstvisit_noweight)
cat("Original Log-Rank statistic:", original_logrank_pfic2_firstvisit_noweight, "\n")
cat("Permutation p-value:", p_value, "\n")

########################### For random index time

# RANDOM INDEX TIME
data_random_index_1 <- readRDS("data_random_index_time_1_final.rds")
data_random_index_1 <- data_random_index_1[data_random_index_1$gender != 2, ]
data_random_index_1_pfic1 <- data_random_index_1[data_random_index_1$type_pfic == 0 & !is.na(data_random_index_1$type_pfic), ]
data_random_index_1_pfic2 <- data_random_index_1[data_random_index_1$type_pfic == 1 & !is.na(data_random_index_1$type_pfic), ]

# FOR PFIC2:
data_random_index_1_pfic2 %>%
  count(nw_or_sc)       # 69 from nw_or_sc = 0 and 64 from nw_or_sc = 1

data_random_index_1_pfic2$nw_or_sc <- as.factor(data_random_index_1_pfic2$nw_or_sc)

p.denom <- glm(nw_or_sc ~ age+as.factor(gender)+log10_tsb_conv+log10_sba_conv+log10_alt_conv, data = data_random_index_1_pfic2, family = binomial(link="logit"))

data_random_index_1_pfic2$pd <- predict(p.denom, data_random_index_1_pfic2, type="response")

#estimation of numerator of ip weights
p.num <- glm(nw_or_sc ~ 1, data = data_random_index_1_pfic2, family = binomial(link="logit"))
data_random_index_1_pfic2$pn <- predict(p.num, data_random_index_1_pfic2, type="response")

#computation of estimated weights
weights_stab <- ifelse(data_random_index_1_pfic2$nw_or_sc == 1, data_random_index_1_pfic2$pn/data_random_index_1_pfic2$pd , (1-data_random_index_1_pfic2$pn)/(1-data_random_index_1_pfic2$pd))
summary(weights_stab)
data_random_index_1_pfic2$IPTW <- weights_stab

data_random_index_1_pfic2$nw_or_sc <- as.numeric(as.character(data_random_index_1_pfic2$nw_or_sc))
data_random_index_1_pfic2$gender <- as.numeric(as.character(data_random_index_1_pfic2$gender))

# Fit the original weighted Cox model
original_model_pfic2_randomvisit <- coxph(Surv(event_time_years, status) ~ nw_or_sc, data = data_random_index_1_pfic2, weights = IPTW)
original_logrank_pfic2_randomvisit <- original_model_pfic2_randomvisit$score

n_permutations <- 1000
permuted_logranks <- numeric(n_permutations)

# Permutation loop
set.seed(43)
for (i in 1:n_permutations){
  # permute the region label: add a new column with random grouping
  random_grouping <- c(rep(0,69), rep(1, 64))
  random_grouping <- sample(random_grouping)
  data_random_index_1_pfic2$random_grouping <- random_grouping
  
  # recalculate the IPTW weight
  p.denom_perm <- glm(random_grouping ~ age+as.factor(gender)+log10_tsb_conv+log10_sba_conv+log10_alt_conv, data = data_random_index_1_pfic2, family = binomial(link="logit"))
  pd_perm <- predict(p.denom_perm, data_random_index_1_pfic2, type="response")
  p.num_perm <- glm(random_grouping ~ 1, data = data_random_index_1_pfic2, family = binomial(link="logit"))
  pn_perm <- predict(p.num_perm, data_random_index_1_pfic2, type="response")
  weights_stab_perm <- ifelse(data_random_index_1_pfic2$random_grouping == 1, pn_perm/pd_perm , (1-pn_perm)/(1-pd_perm))
  
  #recalculate the log-rank statistic on permuted data
  permuted_fit <- coxph(Surv(event_time_years, status) ~ random_grouping, data = data_random_index_1_pfic2, weights = weights_stab_perm)
  permuted_logranks[i] <- permuted_fit$score
}

# plot null distribution
hist(permuted_logranks, breaks = 100, main = "Histogram of Log-Rank Statistics \n of weighted model (PFIC2 with random visit as index time)", xlab = "Log-Rank statistic", col="lightblue", border="black")
abline(v = original_logrank_pfic2_randomvisit, col = 'red', lwd = 2, lty = 2)

# calculate permutation p-value
p_value <- mean(permuted_logranks >= original_logrank_pfic2_randomvisit)
cat("Original Log-Rank statistic:", original_logrank_pfic2_randomvisit, "\n")
cat("Permutation p-value:", p_value, "\n")

# PERMUTATION WITHOUT WEIGHT

# Fit the original Cox model
original_model_pfic2_randomvisit_noweight <- coxph(Surv(event_time_years, status) ~ nw_or_sc, data = data_random_index_1_pfic2)
original_logrank_pfic2_randomvisit_noweight <- original_model_pfic2_randomvisit_noweight$score

n_permutations <- 1000
permuted_logranks <- numeric(n_permutations)

# Permutation loop
set.seed(43)
for (i in 1:n_permutations){
  # permute the region label: add a new column with random grouping
  random_grouping <- c(rep(0,64), rep(1, 69))
  random_grouping <- sample(random_grouping)
  data_random_index_1_pfic2$random_grouping <- random_grouping
  
  #recalculate the log-rank statistic on permuted data
  permuted_fit <- coxph(Surv(event_time_years, status) ~ random_grouping, data = data_random_index_1_pfic2)
  permuted_logranks[i] <- permuted_fit$score
}

# plot null distribution
hist(permuted_logranks, breaks = 100, main = "Histogram of Log-Rank Statistics \n of unweighted model (PFIC2 with random visit as index time)", xlab = "Log-Rank statistic", col="lightblue", border="black")
abline(v = original_logrank_pfic2_randomvisit_noweight, col = 'red', lwd = 2, lty = 2)

# calculate permutation p-value
p_value <- mean(permuted_logranks >= original_logrank_pfic2_randomvisit_noweight)
cat("Original Log-Rank statistic:", original_logrank_pfic2_randomvisit_noweight, "\n")
cat("Permutation p-value:", p_value, "\n")


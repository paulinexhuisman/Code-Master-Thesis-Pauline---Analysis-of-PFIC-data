library(survival)
library(survminer)

data_firstvisit_index_pfic2 <- readRDS("data_firstvisit_index_pfic2_afterweighting_final.rds")
fit <- survfit(Surv(event_time_years, status) ~ nw_or_sc, data = data_firstvisit_index_pfic2,weights = IPTW)
ggsurvplot(fit, data = data_firstvisit_index_pfic2, risk.table = TRUE, surv.median.line = "hv", conf.int = TRUE, title = "Adjusted Kaplan-Meier for North-West EU vs South-Central EU (PFIC2)",
           xlab = "Survival time (years)", ylab="Event-free survival probability", legend.labs = c("EU SC", "EU NW"))

# Cox model for PFIC2
result_cox_pfic2_firstvisit_index_iptw <- summary(coxph(Surv(event_time_years, status) ~ nw_or_sc, data = data_firstvisit_index_pfic2, weights = IPTW,robust = TRUE))
result_cox_pfic2_firstvisit_index_unadj <- summary(coxph(Surv(event_time_years, status) ~ nw_or_sc, data = data_firstvisit_index_pfic2))
result_cox_pfic2_firstvisit_index_coxadj <- summary(coxph(Surv(event_time_years, status) ~ nw_or_sc + age + gender + log10_sba_conv + log10_alt_conv + log10_tsb_conv, data = data_firstvisit_index_pfic2))

data_random_index_1_pfic2 <- readRDS("data_random_1_index_pfic2_afterweighting_final.rds")
result_cox_pfic2_random_index_1_iptw <- summary(coxph(Surv(event_time_years, status) ~ nw_or_sc, data = data_random_index_1_pfic2, weights = IPTW))

data_lastvisit_index_pfic2 <- readRDS("data_lastvisit_index_pfic2_afterweighting_final.rds")
result_cox_pfic2_lastvisit_index_iptw <- summary(coxph(Surv(event_time_years, status) ~ nw_or_sc, data = data_lastvisit_index_pfic2, weights = IPTW))

# First visit as index time
HR_iptw_firstvisit <- result_cox_pfic2_firstvisit_index_iptw$conf.int[1]
CI_lower_iptw_firstvisit <- result_cox_pfic2_firstvisit_index_iptw$conf.int[3]
CI_upper_iptw_firstvisit <- result_cox_pfic2_firstvisit_index_iptw$conf.int[4]
p_iptw_firstvisit <- result_cox_pfic2_firstvisit_index_iptw$sctest[3]

HR_unadj_firstvisit <- result_cox_pfic2_firstvisit_index_unadj$conf.int[1]
CI_lower_unadj_firstvisit <- result_cox_pfic2_firstvisit_index_unadj$conf.int[3]
CI_upper_unadj_firstvisit <- result_cox_pfic2_firstvisit_index_unadj$conf.int[4]
p_unadj_firstvisit <- result_cox_pfic2_firstvisit_index_unadj$sctest[3]

HR_coxadj_firstvisit <- result_cox_pfic2_firstvisit_index_coxadj$conf.int[1,1]
CI_lower_coxadj_firstvisit <- result_cox_pfic2_firstvisit_index_coxadj$conf.int[1,3]
CI_upper_coxadj_firstvisit <- result_cox_pfic2_firstvisit_index_coxadj$conf.int[1,4]
p_coxadj_firstvisit <- result_cox_pfic2_firstvisit_index_coxadj$coefficients[1,5]

# Random eligible visit as index time
HR_iptw_random <- result_cox_pfic2_random_index_1_iptw$conf.int[1]
CI_lower_iptw_random <- result_cox_pfic2_random_index_1_iptw$conf.int[3]
CI_upper_iptw_random <- result_cox_pfic2_random_index_1_iptw$conf.int[4]
p_iptw_random <- result_cox_pfic2_random_index_1_iptw$sctest[3]

# Last eligible visit as index time
HR_iptw_lastvisit <- result_cox_pfic2_lastvisit_index_iptw$conf.int[1]
CI_lower_iptw_lastvisit <- result_cox_pfic2_lastvisit_index_iptw$conf.int[3]
CI_upper_iptw_lastvisit <- result_cox_pfic2_lastvisit_index_iptw$conf.int[4]
p_iptw_lastvisit <- result_cox_pfic2_lastvisit_index_iptw$sctest[3]

# Create forest plot
data_forestplot <- data.frame(
  Sensitivity_analysis = c("Unadjusted", "IPTW Adjusted", "Random eligible visit (IPTW)", "Last eligible visit (IPTW)", "Cox Adjusted"),
  HR = c(round(HR_unadj_firstvisit,3), round(HR_iptw_firstvisit,3),round(HR_iptw_random,3), round(HR_iptw_lastvisit,3), round(HR_coxadj_firstvisit,3)),
  CI_Lower = c(round(CI_lower_unadj_firstvisit,3), round(CI_lower_iptw_firstvisit,3), round(CI_lower_iptw_random,3), round(CI_lower_iptw_lastvisit,3), round(CI_lower_coxadj_firstvisit,3)),
  CI_Upper = c(round(CI_upper_unadj_firstvisit,3), round(CI_upper_iptw_firstvisit,3), round(CI_upper_iptw_random,3), round(CI_upper_iptw_lastvisit,3), round(CI_upper_coxadj_firstvisit,3)),
  p_value = c(round(p_unadj_firstvisit,3), round(p_iptw_firstvisit,3), round(p_iptw_random,3), round(p_iptw_lastvisit,3),round(p_coxadj_firstvisit,3))
)
data_forestplot$' ' <- paste(rep(" ", 60), collapse = " ")
data_forestplot$'95% CI' <- paste("(", data_forestplot$CI_Lower, "-", data_forestplot$CI_Upper, ")", sep = "")
data_forestplot <- data_forestplot %>%
  select(Sensitivity_analysis, CI_Lower, CI_Upper, ' ', HR, '95% CI', p_value)
tm <- forest_theme(base_size = 10,
                   # Graphical parameters of confidence intervals
                   ci_pch = 16,
                   ci_col = "#0e8abb",
                   ci_fill = "red",
                   ci_alpha = 1,
                   ci_lty = 1,
                   ci_lwd = 2,
                   ci_Theight = 0.2,
                   # Graphical parameters of reference line
                   refline_lwd = 1,
                   refline_lty = "dashed",
                   refline_col = "grey20",
                   # Graphical parameters of vertical line
                   vertline_lwd = 1,
                   vertline_lty = "dashed",
                   vertline_col = "grey20",
                   # Graphical parameters of diamond shaped summary CI
                   summary_fill = "#006400",
                   summary_col = "#006400")

# Final data manipulation part 
data_forestplot$CI_Upper <- as.numeric(data_forestplot$CI_Upper)
data_forestplot$CI_Lower <- as.numeric(data_forestplot$CI_Lower)
data_forestplot$HR <- as.numeric(data_forestplot$HR)

colnames(data_forestplot)[1] <- "Sensitivity Analysis"
colnames(data_forestplot)[7] <- "p-value"

# Forest plot
pt <- forest(data_forestplot[, c(1, 4:7)],
             est = data_forestplot$HR,
             lower = data_forestplot$CI_Lower, 
             upper = data_forestplot$CI_Upper,
             sizes = 0.5,
             ci_column = 2,
             ref_line = 1,
             arrow_lab = c("Favours South-Central Europe", "Favours North-West Europe"),
             xlim = c(0.5, 2),
             ticks_at = c(0.5, 1.0, 1.5, 2.0),
             xlab = "HR",
             theme = tm)

plot(pt)

##### CHECK PROP HAZARD ASSUMPTION 
cox_model_pfic2_firstvisit <- coxph(Surv(event_time_years, status) ~ nw_or_sc, data = data_firstvisit_index_pfic2, weights = IPTW,robust = TRUE)
ph_test <- cox.zph(cox_model_pfic2_firstvisit)
ggcoxzph(ph_test)

#### Weighted descriptives
sum_weights_nw <- sum(data_firstvisit_index_pfic2_nw$IPTW) #64.11
sum_weights_sc <- sum(data_firstvisit_index_pfic2_sc$IPTW) #68.90

sum_weights_nw_male <- sum(data_firstvisit_index_pfic2_nw[data_firstvisit_index_pfic2_nw$gender == 0,]$IPTW) #33.47
sum_weights_nw_female <- sum(data_firstvisit_index_pfic2_nw[data_firstvisit_index_pfic2_nw$gender == 1,]$IPTW) #30.64
sum_weights_sc_male <- sum(data_firstvisit_index_pfic2_sc[data_firstvisit_index_pfic2_sc$gender == 0,]$IPTW) #35.99
sum_weights_sc_female <- sum(data_firstvisit_index_pfic2_sc[data_firstvisit_index_pfic2_sc$gender == 1,]$IPTW) #32.91

sum_weights_nw_sbd <- sum(data_firstvisit_index_pfic2_nw[data_firstvisit_index_pfic2_nw$event_type == 3,]$IPTW) #18.82
sum_weights_nw_ltx <- sum(data_firstvisit_index_pfic2_nw[data_firstvisit_index_pfic2_nw$event_type == 2,]$IPTW) #16.94
sum_weights_nw_death <- sum(data_firstvisit_index_pfic2_nw[data_firstvisit_index_pfic2_nw$event_type == 1,]$IPTW) #1.94
sum_weights_sc_sbd <- sum(data_firstvisit_index_pfic2_sc[data_firstvisit_index_pfic2_sc$event_type == 3,]$IPTW) #18.66
sum_weights_sc_ltx <- sum(data_firstvisit_index_pfic2_sc[data_firstvisit_index_pfic2_sc$event_type == 2,]$IPTW) #21.31
sum_weights_sc_death <- sum(data_firstvisit_index_pfic2_sc[data_firstvisit_index_pfic2_sc$event_type == 1,]$IPTW) #0.67

#### Tests for p-values
# Gender
chisq.test(table(data_firstvisit_index_pfic2$nw_or_sc, data_firstvisit_index_pfic2$gender),correct=FALSE)
library(weights)
wtd.chi.sq(data_firstvisit_index_pfic2$gender, data_firstvisit_index_pfic2$nw_or_sc, weight=data_firstvisit_index_pfic2$IPTW)

# Age
wilcox.test(data_firstvisit_index_pfic2_nw$age, data_firstvisit_index_pfic2_sc$age)
library(sjstats)
library(coin)
mann_whitney_test(data_firstvisit_index_pfic2, "age", by = "nw_or_sc", weights = "IPTW")

# sba
t.test(
  x = data_firstvisit_index_pfic2_nw$log10_sba_conv,
  y = data_firstvisit_index_pfic2_sc$log10_sba_conv,
  var.equal = TRUE
)
wtd.t.test(
  x = data_firstvisit_index_pfic2_nw$log10_sba_conv,
  y = data_firstvisit_index_pfic2_sc$log10_sba_conv, 
  weight = data_firstvisit_index_pfic2_nw$IPTW, 
  weighty = data_firstvisit_index_pfic2_sc$IPTW)

# total bilirubin
t.test(
  x = data_firstvisit_index_pfic2_nw$log10_tsb_conv,
  y = data_firstvisit_index_pfic2_sc$log10_tsb_conv,
  var.equal = TRUE
)
wtd.t.test(
  x = data_firstvisit_index_pfic2_nw$log10_tsb_conv,
  y = data_firstvisit_index_pfic2_sc$log10_tsb_conv, 
  weight = data_firstvisit_index_pfic2_nw$IPTW, 
  weighty = data_firstvisit_index_pfic2_sc$IPTW)

# ALT
t.test(
  x = data_firstvisit_index_pfic2_nw$log10_alt_conv,
  y = data_firstvisit_index_pfic2_sc$log10_alt_conv,
  var.equal = TRUE
)
wtd.t.test(
  x = data_firstvisit_index_pfic2_nw$log10_alt_conv,
  y = data_firstvisit_index_pfic2_sc$log10_alt_conv, 
  weight = data_firstvisit_index_pfic2_nw$IPTW, 
  weighty = data_firstvisit_index_pfic2_sc$IPTW)
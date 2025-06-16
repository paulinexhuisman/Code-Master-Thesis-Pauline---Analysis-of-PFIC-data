### PS model
library(ggplot2)
library(MatchIt)
library(ggpubr)
library(survminer)
library(survival)
library(cobalt)

# Draw love plot
love.plot = function(cov, treat, ## cov is the matrix of covariates and treat is a vector of treatment assignment
                     weights = rep(1, length(treat)),
                     plot = F)
{
  ## mean with normalized weights \ sum w_i x_i / (\sum w_i)
  treat.means <- colSums(cov[treat == 1,] * weights[treat == 1])/sum(weights[treat==1])
  treat.var <- colSums(t(t(cov[treat == 1,])-treat.means)^2 *
                         weights[treat == 1])/sum(weights[treat==1])
  
  control.means <- colSums(cov[treat == 0,] * weights[treat == 0])/sum(weights[treat==0])
  control.var <- colSums(t(t(cov[treat == 0,])-control.means)^2 *
                           weights[treat == 0])/sum(weights[treat==0])
  
  ## the standardized mean differences for every covariance
  smd <- (treat.means - control.means)/sqrt((treat.var + control.var)/2)
  names(smd) <- colnames(cov)
  
  if (plot == T){
    plot.data <-data.frame(smd = smd, covariates =names(smd))
    range <- max(abs(smd))
    ggplot(plot.data) + geom_point(aes(x=as.numeric(smd), y=covariates))+
      geom_vline(xintercept = 0) + xlim(-range,range) + 
      labs(x = 'Standardized Difference in Means')
  }
  return(smd)
}

######### FIRST VISIT INDEX TIME
data_firstvisit_index <- readRDS("data_firstvisit_index_time_final.rds")
data_firstvisit_index <- data_firstvisit_index[data_firstvisit_index$gender != 2, ]
data_firstvisit_index_pfic1 <- data_firstvisit_index[data_firstvisit_index$type_pfic == 0 & !is.na(data_firstvisit_index$type_pfic), ]
data_firstvisit_index_pfic2 <- data_firstvisit_index[data_firstvisit_index$type_pfic == 1 & !is.na(data_firstvisit_index$type_pfic), ]

# FOR PFIC1:
data_firstvisit_index_pfic1$nw_or_sc <- as.factor(data_firstvisit_index_pfic1$nw_or_sc)

p.denom <- glm(nw_or_sc ~ age+as.factor(gender)+log10_tsb_conv+log10_sba_conv+log10_alt_conv, data = data_firstvisit_index_pfic1, family = binomial(link="logit"))
summary(p.denom)
data_firstvisit_index_pfic1$pd <- predict(p.denom, data_firstvisit_index_pfic1, type="response")


g <- ggplot(data_firstvisit_index_pfic1, aes(x = pd, color = nw_or_sc, fill = nw_or_sc))+
  geom_density(alpha=.47) +
  xlab("Estimated probability of being in North-West (1) or South-Central (0) EU") +
  ylab("Density")
ggpar(g,palette = "nejm")
summary(data_firstvisit_index_pfic1$pd)

#estimation of numerator of ip weights
p.num <- glm(nw_or_sc ~ 1, data = data_firstvisit_index_pfic1, family = binomial(link="logit"))
data_firstvisit_index_pfic1$pn <- predict(p.num, data_firstvisit_index_pfic1, type="response")

#computation of estimated weights
weights_stab <- ifelse(data_firstvisit_index_pfic1$nw_or_sc == 1, data_firstvisit_index_pfic1$pn/data_firstvisit_index_pfic1$pd , (1-data_firstvisit_index_pfic1$pn)/(1-data_firstvisit_index_pfic1$pd))
summary(weights_stab)
data_firstvisit_index_pfic1$IPTW <- weights_stab

data_firstvisit_index_pfic1$nw_or_sc <- as.numeric(as.character(data_firstvisit_index_pfic1$nw_or_sc))
data_firstvisit_index_pfic1$gender <- as.numeric(as.character(data_firstvisit_index_pfic1$gender))

colnames(data_firstvisit_index_pfic1)
raw.smd <- love.plot(data_firstvisit_index_pfic1[,c(6,8,81,82,83)], data_firstvisit_index_pfic1$nw_or_sc) 
weighted.smd <- love.plot(data_firstvisit_index_pfic1[,c(6,8,81,82,83)], data_firstvisit_index_pfic1$nw_or_sc, weights=weights_stab) 

plot.data <- data.frame(smd = c(raw.smd, weighted.smd),
                        covariates = c(names(raw.smd), names(weighted.smd)),
                        category = c(rep("Original", length(raw.smd)), rep("IPW",length(weighted.smd))))
range <- max(abs(plot.data$smd))
ggplot(plot.data)+geom_point(aes(x=as.numeric(smd),y=covariates, color=category))+
  geom_vline(xintercept = c(-0.1,-0.05,0,0.05,0.1),
             linetype = c("solid","dashed","solid","dashed","solid"))+
  xlim(-range,range)+
  labs(x='Standardized Difference in Means')

#Plot boxplot stabilized weights
ggplot(temp.data, aes(x=region, y=weights_stab, color = region))+
  xlab("Region group")+
  ylab("Stabilized weights")+
  labs(title = "Boxplot of the stabilized weights calculated for PFIC1 patients")+
  geom_boxplot()

saveRDS(data_firstvisit_index_pfic1, "data_firstvisit_index_pfic1_afterweighting_final.rds")

# FOR PFIC2:
data_firstvisit_index_pfic2$nw_or_sc <- as.factor(data_firstvisit_index_pfic2$nw_or_sc)

p.denom <- glm(nw_or_sc ~ age+as.factor(gender)+log10_tsb_conv+log10_sba_conv+log10_alt_conv
               , data = data_firstvisit_index_pfic2, family = binomial(link="logit"))
summary(p.denom)
data_firstvisit_index_pfic2$pd <- predict(p.denom, data_firstvisit_index_pfic2, type="response")

g <- ggplot(data_firstvisit_index_pfic2, aes(x = pd, color = nw_or_sc, fill = nw_or_sc))+
  geom_density(alpha=.47) +
  xlab("Estimated probability of being in North-West (1) or South-Central (0) EU") +
  ylab("Density")
ggpar(g,palette = "nejm")
summary(data_firstvisit_index_pfic2$pd)

#estimation of numerator of ip weights
p.num <- glm(nw_or_sc ~ 1, data = data_firstvisit_index_pfic2, family = binomial(link="logit"))
data_firstvisit_index_pfic2$pn <- predict(p.num, data_firstvisit_index_pfic2, type="response")

#computation of estimated weights
weights_stab <- ifelse(data_firstvisit_index_pfic2$nw_or_sc == 1, data_firstvisit_index_pfic2$pn/data_firstvisit_index_pfic2$pd , (1-data_firstvisit_index_pfic2$pn)/(1-data_firstvisit_index_pfic2$pd))
summary(weights_stab)
data_firstvisit_index_pfic2$IPTW <- weights_stab

data_firstvisit_index_pfic2$nw_or_sc <- as.numeric(as.character(data_firstvisit_index_pfic2$nw_or_sc))
data_firstvisit_index_pfic2$gender <- as.numeric(as.character(data_firstvisit_index_pfic2$gender))

colnames(data_firstvisit_index_pfic2)
raw.smd <- love.plot(data_firstvisit_index_pfic2[,c(6,8,81,82,83)], data_firstvisit_index_pfic2$nw_or_sc) 
weighted.smd <- love.plot(data_firstvisit_index_pfic2[,c(6,8,81,82,83)], data_firstvisit_index_pfic2$nw_or_sc, weights=weights_stab) 

plot.data <- data.frame(smd = c(raw.smd, weighted.smd),
                        covariates = c(names(raw.smd), names(weighted.smd)),
                        category = c(rep("Before weighting", length(raw.smd)), rep("After weighting (with IPTW)",length(weighted.smd))))
range <- max(abs(plot.data$smd))
names_plot_y <- c(age = 'Age', gender = "Sex", log10_alt_conv = "Log(ALT)", log10_sba_conv = "Log(sBA)", log10_tsb_conv = "Log(Total bilirubin)")
plot.data$covariates <- names_plot_y[as.character(plot.data$covariates)]
ggplot(plot.data)+geom_point(aes(x=as.numeric(smd),y=covariates, color=category))+
  geom_vline(xintercept = c(-0.1,-0.05,0,0.05,0.1),
             linetype = c("solid","dashed","solid","dashed","solid"))+
  xlim(-range,range)+
  labs(x='Standardized Difference in Means',
       title = 'Covariate balance before and after weighting') +
  theme(plot.title = element_text(hjust = 0.5))

# Plot boxplot stabilized weights
ggplot(temp.data, aes(x=region, y=weights_stab, color = region))+
  xlab("Region group")+
  ylab("Stabilized weights")+
  labs(title = "Boxplot of the stabilized weights calculated for PFIC2 patients")+
  geom_boxplot()

#Plot propensity scores distribution

data_firstvisit_index_pfic2$nw_or_sc <- factor(data_firstvisit_index_pfic2$nw_or_sc,
                                               levels = c(0,1),
                                               labels = c("South-Central EU", "North-West EU"))
g <- ggplot(data_firstvisit_index_pfic2, aes(x = pd, color = nw_or_sc, fill = nw_or_sc))+
  geom_density(alpha=.47) +
  xlab("Estimated probability of being in North-West EU or South-Central EU (for PFIC2)") +
  ylab("Density")+
  labs(fill = "Region group", color = "Region group")
ggpar(g,palette = "nejm")

saveRDS(data_firstvisit_index_pfic2, "data_firstvisit_index_pfic2_afterweighting_final.rds")

#Plot propensity scores distribution

data_firstvisit_index_pfic2$nw_or_sc <- factor(data_firstvisit_index_pfic2$nw_or_sc,
                                               levels = c(0,1),
                                               labels = c("South-Central EU", "North-West EU"))
g <- ggplot(data_firstvisit_index_pfic2, aes(x = pd, color = nw_or_sc, fill = nw_or_sc))+
  geom_density(alpha=.47) +
  xlab("Estimated probability of being in North-West EU or South-Central EU (for PFIC2)") +
  ylab("Density")+
  labs(fill = "Region group", color = "Region group")
ggpar(g,palette = "nejm")

# Make density plots
# The weightit function creates exactly the same weights as my code; so assign my weights to the weighit object 
# and use the bal.plot function to create the plots
library(WeightIt)
library(cobalt)

w.out <- weightit(
  nw_or_sc ~ age + gender + log10_tsb_conv + log10_sba_conv + log10_alt_conv,
  data = data_firstvisit_index_pfic2,
  method = "ps",        # Logistic regression propensity scores
  estimand = "ATE"      # For IPTW
)

w.out$weights <- weights_stab

bal.plot(w.out, var.name = "age", which = "both")
bal.plot(w.out, var.name = "gender", which = "both")
bal.plot(w.out, var.name = "log10_tsb_conv", which = "both")
bal.plot(w.out, var.name = "log10_sba_conv", which = "both")
bal.plot(w.out, var.name = "log10_alt_conv", which = "both")

bal.plot(w.out, var.name = "age", type = "ecdf", which = "both")
bal.plot(w.out, var.name = "log10_tsb_conv", type = "ecdf", which = "both")
bal.plot(w.out, var.name = "log10_sba_conv", type = "ecdf", which = "both")
bal.plot(w.out, var.name = "log10_alt_conv", type = "ecdf", which = "both")

######### RANDOM VISIT INDEX TIME
data_random_index_1 <- readRDS("data_random_index_time_1_final.rds")
data_random_index_1_pfic2 <- data_random_index_1[data_random_index_1$type_pfic == 1 & !is.na(data_random_index_1$type_pfic), ]

# FOR PFIC2:
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

saveRDS(data_random_index_1_pfic2, "data_random_1_index_pfic2_afterweighting_final.rds")

######### LAST VISIT INDEX TIME
data_lastvisit_index <- readRDS("data_lastvisit_index_time_final.rds")
data_lastvisit_index <- data_lastvisit_index[data_lastvisit_index$gender != 2, ]
data_lastvisit_index_pfic2 <- data_lastvisit_index[data_lastvisit_index$type_pfic == 1 & !is.na(data_lastvisit_index$type_pfic), ]

# FOR PFIC2:
data_lastvisit_index_pfic2$nw_or_sc <- as.factor(data_lastvisit_index_pfic2$nw_or_sc)

p.denom <- glm(nw_or_sc ~ age+as.factor(gender)+log10_tsb_conv+log10_sba_conv+log10_alt_conv, data = data_lastvisit_index_pfic2, family = binomial(link="logit"))
data_lastvisit_index_pfic2$pd <- predict(p.denom, data_lastvisit_index_pfic2, type="response")

#estimation of numerator of ip weights
p.num <- glm(nw_or_sc ~ 1, data = data_lastvisit_index_pfic2, family = binomial(link="logit"))
data_lastvisit_index_pfic2$pn <- predict(p.num, data_lastvisit_index_pfic2, type="response")

#computation of estimated weights
weights_stab <- ifelse(data_lastvisit_index_pfic2$nw_or_sc == 1, data_lastvisit_index_pfic2$pn/data_lastvisit_index_pfic2$pd , (1-data_lastvisit_index_pfic2$pn)/(1-data_lastvisit_index_pfic2$pd))
summary(weights_stab)
data_lastvisit_index_pfic2$IPTW <- weights_stab

data_lastvisit_index_pfic2$nw_or_sc <- as.numeric(as.character(data_lastvisit_index_pfic2$nw_or_sc))
data_lastvisit_index_pfic2$gender <- as.numeric(as.character(data_lastvisit_index_pfic2$gender))

saveRDS(data_lastvisit_index_pfic2, "data_lastvisit_index_pfic2_afterweighting_final.rds")
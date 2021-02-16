library(afex)
library(effects)
library(RePsychLing)
library(corrplot)
library(rcompanion)

setwd("~/GitHub/yorkshire-assimilation")
ya = na.omit(read.csv("./dataset_voicing2.csv"))
ya$participant = as.factor(ya$participant)

# check for correlations among predictors:
# continous
continuous = c("w1_freq", "w2_freq")
corrplot(cor(ya[, continuous]), method = "number")

# categorical
categorical = c("item_condition", "vowel_type", "C1_MoA", "C2_MoA")
cat_mat = matrix(data = c(cramerV(table(ya[,c("item_condition", "item_condition")]), bias.correct = TRUE),
                          cramerV(table(ya[,c("item_condition", "vowel_type")]), bias.correct = TRUE),
                          cramerV(table(ya[,c("item_condition", "C1_MoA")]), bias.correct = TRUE),
                          cramerV(table(ya[,c("item_condition", "C2_MoA")]), bias.correct = TRUE),
                          cramerV(table(ya[,c("vowel_type", "item_condition")]), bias.correct = TRUE),
                          cramerV(table(ya[,c("vowel_type", "vowel_type")]), bias.correct = TRUE),
                          cramerV(table(ya[,c("vowel_type", "C1_MoA")]), bias.correct = TRUE),
                          cramerV(table(ya[,c("vowel_type", "C2_MoA")]), bias.correct = TRUE),
                          cramerV(table(ya[,c("C1_MoA", "item_condition")]), bias.correct = TRUE),
                          cramerV(table(ya[,c("C1_MoA", "vowel_type")]), bias.correct = TRUE),
                          cramerV(table(ya[,c("C1_MoA", "C1_MoA")]), bias.correct = TRUE),
                          cramerV(table(ya[,c("C1_MoA", "C2_MoA")]), bias.correct = TRUE),
                          cramerV(table(ya[,c("C2_MoA", "item_condition")]), bias.correct = TRUE),
                          cramerV(table(ya[,c("C2_MoA", "vowel_type")]), bias.correct = TRUE),
                          cramerV(table(ya[,c("C2_MoA", "C1_MoA")]), bias.correct = TRUE),
                          cramerV(table(ya[,c("C2_MoA", "C2_MoA")]), bias.correct = TRUE)),
                 nrow = 4, ncol = 4, byrow = TRUE,
                 dimnames = list(categorical, categorical))

corrplot(cat_mat, method = "number")
table(ya[, c("C1_MoA","vowel_type")])
long = ya[ya$vowel_type == "long",]
categorical = c("item_condition", "C1_MoA", "C2_MoA")

# categorical - continuous
cat_cont = matrix(data = c(sqrt(summary(lm(w1_freq ~ item_condition, data = long))$r.squared),
                           sqrt(summary(lm(w1_freq ~ C1_MoA, data = long))$r.squared),
                           sqrt(summary(lm(w1_freq ~ C2_MoA, data = long))$r.squared),
                           sqrt(summary(lm(w2_freq ~ item_condition, data = long))$r.squared),
                           sqrt(summary(lm(w2_freq ~ C1_MoA, data = long))$r.squared),
                           sqrt(summary(lm(w2_freq ~ C2_MoA, data = long))$r.squared)),
                  nrow = 3, ncol = 2, byrow = TRUE,
                  dimnames = list(categorical, continuous))

corrplot(cat_cont, method = "number")

dummies = dummy(long$item_condition)
cond_sonorant = dummies[,1]
cond_voiced = dummies[,2]
cond_voiceless = dummies[,3]

max1 = lmer(ratio ~ w1_freq + w2_freq + C2_MoA
            + item_condition*C1_MoA 
            + (1 + cond_sonorant + cond_voiced + cond_voiceless | participant) + (1 | word), data = long)

max1b = lmer(ratio ~ w1_freq + w2_freq + C2_MoA
             + item_condition*C1_MoA 
             + (1 + cond_sonorant + cond_voiced + cond_voiceless || participant) + (1 | word), data = long)

# at this point some authors might keep this zcp random effects structure
# but others would not accept variance components that are estimated at 0

max2 = mixed(ratio ~ w1_freq + w2_freq + C2_MoA
             + item_condition*C1_MoA 
             + (1+item_condition|participant) + (1|word), data = long)

#ya$mean_freq = rowMeans(ya[c("w1_freq", "w2_freq")])
#ya$sum_freq = ya$w1_freq + ya$w2_freq
#ya$voicing_dur = ya$voicing_proportion * ya$cons_length
#ya$voicing_prop_trans = (ya$voicing_proportion*(ya$n_frames - 1) + 0.5) / ya$n_frames

d0 = lmer(ratio ~ 1 + (1|participant) + (1|word), data = ya)
d1 = lmer(ratio ~ 1 + vowel_type + (1|participant) + (1|word), data = ya)
anova(d0,d1)
d2 = lmer(ratio ~ 1 + vowel_type + sum_freq + (1|participant) + (1|word), data = ya)
d3 = lmer(ratio ~ 1 + vowel_type + w1_freq + w2_freq + (1|participant) + (1|word), data = ya)
d4 = lmer(ratio ~ 1 + vowel_type + mean_freq + (1|participant) + (1|word), data = ya)
anova(d1,d2)
anova(d1,d3)
anova(d1,d4)

d5 = lmer(ratio ~ 1 + vowel_type + item_condition + (1|participant) + (1|word), data = ya)
d6 = lmer(ratio ~ 1 + vowel_type*item_condition + (1|participant) + (1|word), data = ya)
anova(d0, d1, d5, d6)
d6b = lmer(ratio ~ 1 + vowel_type*item_condition + C1_MoA + (1|participant) + (1|word), data = ya)
d6c = lmer(ratio ~ 1 + vowel_type*item_condition + C2_MoA + (1|participant) + (1|word), data = ya)
anova(d6,d6b)
anova(d6,d6c)
d7 = lmer(ratio ~ 1 + vowel_type*item_condition + (1+vowel_type*item_condition|participant) + (1|word), data = ya)
anova(d6,d7)
# model failed to converge remove highest order interaction
d8 = lmer(ratio ~ 1 + vowel_type*item_condition + (1+vowel_type+item_condition|participant) + (1|word), data = ya)
anova(d6,d8)
# check if degenerate
summary(rePCA(d8))
# yes remove lowest varince component
summary(d8, corr=FALSE)
# item_condition
d9 = lmer(ratio ~ 1 + vowel_type*item_condition + (1+vowel_type|participant) + (1|word), data = ya)
summary(rePCA(d9))
# still degenerate
d10 = lmer(ratio ~ 1 + vowel_type*item_condition + (1|participant) + (1|word), data = ya)
summary(rePCA(d10))

# time to check the residuals
par(mfrow=c(2,3))
plot(fitted(d10), residuals(d10))
cor(fitted(d10), residuals(d10))
qqnorm(residuals(d10))
qqline(residuals(d10))
plot(density(residuals(d10)))

ya2 = ya[abs(scale(resid(d10))) < 2.5,]
d11 = lmer(ratio ~ 1 + vowel_type*item_condition + (1|participant) + (1|word), data = ya2)

plot(fitted(d11), residuals(d11))
qqnorm(residuals(d11))
qqline(residuals(d11))
plot(density(residuals(d11)))

# residuals are not not normal at all --> transform?
d0 = lmer(log(ratio) ~ 1 + (1|participant) + (1|word), data = ya)
d1 = lmer(log(ratio) ~ 1 + vowel_type + (1|participant) + (1|word), data = ya)
anova(d0,d1)
d2 = lmer(log(ratio) ~ 1 + vowel_type + sum_freq + (1|participant) + (1|word), data = ya)
d3 = lmer(log(ratio) ~ 1 + vowel_type + w1_freq + w2_freq + (1|participant) + (1|word), data = ya)
d4 = lmer(log(ratio) ~ 1 + vowel_type + mean_freq + (1|participant) + (1|word), data = ya)
anova(d1,d2)
anova(d1,d3)
anova(d1,d4)
d5 = lmer(log(ratio) ~ 1 + vowel_type + item_condition + (1|participant) + (1|word), data = ya)
d6 = lmer(log(ratio) ~ 1 + vowel_type*item_condition + (1|participant) + (1|word), data = ya)
anova(d0, d1, d5, d6)
d5b = lmer(log(ratio) ~ 1 + vowel_type + item_condition + C1_MoA + (1|participant) + (1|word), data = ya)
d5c = lmer(log(ratio) ~ 1 + vowel_type + item_condition + C2_MoA + (1|participant) + (1|word), data = ya)
anova(d5,d5b)
anova(d5,d5c)
d7 = lmer(log(ratio) ~ 1 + vowel_type + item_condition + (1+vowel_type+item_condition|participant) + (1|word), data = ya)
anova(d5,d7)
summary(rePCA(d7))
# degenerate
summary(d7, corr=FALSE)
d8 = lmer(log(ratio) ~ 1 + vowel_type + item_condition + (1+item_condition|participant) + (1|word), data = ya)
summary(rePCA(d8))
d9 = lmer(log(ratio) ~ 1 + vowel_type + item_condition + (1|participant) + (1|word), data = ya)
summary(rePCA(d9))

par(mfrow=c(2,3))
plot(fitted(d9), residuals(d9))
cor(fitted(d9), residuals(d9))
qqnorm(residuals(d9))
qqline(residuals(d9))
plot(density(residuals(d9)))

ya2 = ya[abs(scale(resid(d9))) < 2.5,]
d10 = lmer(log(ratio) ~ 1 + vowel_type + item_condition + (1|participant) + (1|word), data = ya2)

plot(fitted(d10), residuals(d10))
cor(fitted(d10), residuals(d10))
qqnorm(residuals(d10))
qqline(residuals(d10))
plot(density(residuals(d10)))

vif.mer(d10)

par(mfrow=c(1,2))
# levene tests are significant but plots show similar variances
boxplot(residuals(d10) ~ ya2$vowel_type)
boxplot(residuals(d10) ~ ya2$item_condition)

summary(d10, corr=FALSE)
plot(allEffects(d10, confidence.level=0.95))

############## Parsimonious Approach ###############

# upon second reading of Bates et al. going straight for the zcpLMM is 
# only advised when variance components and correlationparameters are 
# estimated at 0 and 1 respectively. 
# This is not the case for the current model. So method below should not be used.

library(afex)
maxLMM = lmer_alt(log(ratio) ~ 1 + vowel_type + item_condition + (1+vowel_type+item_condition|participant) + (1|word), data = ya)
summary(maxLMM, corr=FALSE)
summary(rePCA(maxLMM))

zcpLMM = lmer_alt(log(ratio) ~ 1 + vowel_type + item_condition + (1+vowel_type+item_condition||participant) + (1|word), data = ya)
summary(zcpLMM, corr=FALSE)
summary(rePCA(zcpLMM))

anova(zcpLMM, maxLMM)

zcpLMM2 = lmer_alt(log(ratio) ~ 1 + vowel_type + item_condition + (1+item_condition||participant) + (1|word), data = ya)
anova(zcpLMM2, zcpLMM)

####################################################

v0 = lmer(voicing_dur ~ 1 + (1|participant) + (1|word), data = ya)
v1 = lmer(voicing_dur ~ 1 + vowel_type + (1|participant) + (1|word), data = ya)
anova(v0, v1)
v2 = lmer(voicing_dur ~ 1 + item_condition + (1|participant) + (1|word), data = ya)
v3 = lmer(voicing_dur ~ 1 + item_condition + mean_freq + (1|participant) + (1|word), data = ya)
v4 = lmer(voicing_dur ~ 1 + item_condition + w1_freq + w2_freq + (1|participant) + (1|word), data = ya)
anova(v0, v2, v3)
anova(v0, v2, v4)
v5 = lmer(voicing_dur ~ 1 + item_condition + C1_MoA + (1|participant) + (1|word), data = ya)
v6 = lmer(voicing_dur ~ 1 + item_condition + C1_MoA + C2_MoA + (1|participant) + (1|word), data = ya)
anova(v0, v2, v5, v6)
summary(v6)
v7 = lmer(voicing_dur ~ 1 + item_condition*C1_MoA + C2_MoA + (1|participant) + (1|word), data = ya)
anova(v6, v7)
#plot(effect("item_condition:C1_MoA", v7, confidence.level=0.95))
v8 = lmer(voicing_dur ~ 1 + item_condition*C2_MoA + C1_MoA + (1|participant) + (1|word), data = ya)
anova(v6, v8)
#plot(effect("item_condition:C2_MoA", v8, confidence.level=0.95))
v9 = lmer(voicing_dur ~ 1 + item_condition*C2_MoA*C1_MoA + (1|participant) + (1|word), data = ya)
anova(v8, v9)
#v10 = lmer(voicing_proportion ~ 1 + item_condition*C1_MoA*C2_MoA + (1|participant) + (1|word), data = ya)
#anova(v9, v10)
v11 = lmer(voicing_dur ~ 1 + item_condition*C2_MoA + C1_MoA  + (1+item_condition*C2_MoA+C1_MoA|participant) + (1|word), data = ya)
anova(v8, v11)
# v11 fails to converge
v12 = lmer(voicing_dur ~ 1 + item_condition*C2_MoA + C1_MoA + (1+item_condition+C2_MoA+C1_MoA|participant) + (1|word), data = ya)
anova(v8, v12)

# check v12 for degenerate random effects structure

#vignette("KB", package = "RePsychLing")

summary(rePCA(v12))

# remove smallest variance component (C2_MoA)
summary(v12)

v13 = lmer(voicing_dur ~ 1 + item_condition*C2_MoA + C1_MoA + (1+item_condition+C1_MoA|participant) + (1|word), data = ya)

summary(rePCA(v13))

# remove smallest variance component 

v14 = lmer(voicing_dur ~ 1 + item_condition*C2_MoA + C1_MoA + (1+item_condition|participant) + (1|word), data = ya)
summary(rePCA(v14))

# does further reduction result in a significantly worse model?

v15 = lmer(voicing_dur ~ 1 + item_condition*C2_MoA + C1_MoA + (1|participant) + (1|word), data = ya)
anova(v14, v15)
# no, keep v15
# we know removing other factors would result in worse model

# check vif

vif.mer(v15)

# shit I should have used a model for sigmoid distribution
# or model voicing dur instead (should proabaly be fine; residuals look ok)
#v19 = glmmadmb(voicing_prop_trans ~ item_condition*C2_MoA + item_condition*C1_MoA + (1+C1_MoA|participant) + (1|word), data = ya, family = "beta")

# time to check the residuals
par(mfrow=c(2,3))
plot(fitted(v15), residuals(v15))
cor(fitted(v15), residuals(v15))
qqnorm(residuals(v15))
qqline(residuals(v15))
plot(density(residuals(v15)))

ya2 = ya[abs(scale(resid(v15))) < 2.5,]
v16 = lmer(voicing_dur ~ 1 + item_condition*C2_MoA + C1_MoA + (1|participant) + (1|word), data = ya2)

plot(fitted(v16), residuals(v16))
qqnorm(residuals(v16))
qqline(residuals(v16))
plot(density(residuals(v16)))

#shapiro.test(residuals(v15))

leveneTest(residuals(v15) ~ C1_MoA, data=ya)
leveneTest(residuals(v15) ~ item_condition*C2_MoA, data=ya)
bartlett.test(residuals(v15) ~ C1_MoA, data = ya)
bartlett.test(residuals(v15) ~ interaction(item_condition, C2_MoA), data = ya)
fligner.test(residuals(v15) ~ C1_MoA, data = ya)
fligner.test(residuals(v15) ~ interaction(item_condition, C2_MoA), data = ya)


par(mfrow=c(1,2))
# levene tests are significant but plots show similar variances
boxplot(residuals(v15) ~ ya$C1_MoA)
boxplot(residuals(v15) ~ interaction(ya$item_condition, ya$C2_MoA))

par(mfrow=c(1,1))
plot(effect("item_condition:C2_MoA", v15, confidence.level=0.95))


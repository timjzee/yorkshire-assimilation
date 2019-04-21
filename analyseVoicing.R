library(lme4)
library(effects)
library(RePsychLing)
library(car)
library(betareg)

setwd("~/GitHub/yorkshire-assimilation")
ya = na.omit(read.csv("./dataset_voicing2.csv"))
ya$participant = as.factor(ya$participant)
ya$mean_freq = rowMeans(ya[c("w1_freq", "w2_freq")])
ya$sum_freq = ya$w1_freq + ya$w2_freq
ya$voicing_dur = ya$voicing_proportion * ya$cons_length
ya$voicing_prop_trans = (ya$voicing_proportion*(ya$n_frames - 1) + 0.5) / ya$n_frames

d0 = lmer(ratio ~ 1 + (1|participant) + (1|word), data = ya)
d1 = lmer(ratio ~ 1 + vowel_type + (1|participant) + (1|word), data = ya)
d2 = lmer(ratio ~ 1 + vowel_type + sum_freq + (1|participant) + (1|word), data = ya)
d3 = lmer(ratio ~ 1 + vowel_type + w1_freq + w2_freq + (1|participant) + (1|word), data = ya)
d4 = lmer(ratio ~ 1 + vowel_type + mean_freq + (1|participant) + (1|word), data = ya)

d5 = lmer(ratio ~ 1 + vowel_type + item_condition + (1|participant) + (1|word), data = ya)
d6 = lmer(ratio ~ 1 + vowel_type*item_condition + (1|participant) + (1|word), data = ya)
anova(d0, d1, d5, d6)

plot(effect("vowel_type:item_condition", d6, confidence.level=0.95))


v0 = lmer(voicing_proportion ~ 1 + (1|participant) + (1|word), data = ya)
v1 = lmer(voicing_proportion ~ 1 + vowel_type + (1|participant) + (1|word), data = ya)
anova(v0, v1)
v2 = lmer(voicing_proportion ~ 1 + item_condition + (1|participant) + (1|word), data = ya)
v3 = lmer(voicing_proportion ~ 1 + item_condition + mean_freq + (1|participant) + (1|word), data = ya)
v4 = lmer(voicing_proportion ~ 1 + item_condition + w1_freq + w2_freq + (1|participant) + (1|word), data = ya)
anova(v0, v2, v3)
anova(v0, v2, v4)
v5 = lmer(voicing_proportion ~ 1 + item_condition + C1_MoA + (1|participant) + (1|word), data = ya)
v6 = lmer(voicing_proportion ~ 1 + item_condition + C1_MoA + C2_MoA + (1|participant) + (1|word), data = ya)
anova(v0, v2, v5, v6)
summary(v6)
v7 = lmer(voicing_proportion ~ 1 + item_condition*C1_MoA + C2_MoA + (1|participant) + (1|word), data = ya)
anova(v6, v7)
plot(effect("item_condition:C1_MoA", v7, confidence.level=0.95))
v8 = lmer(voicing_proportion ~ 1 + item_condition*C2_MoA + C1_MoA + (1|participant) + (1|word), data = ya)
anova(v6, v8)
plot(effect("item_condition:C2_MoA", v8, confidence.level=0.95))
v9 = lmer(voicing_proportion ~ 1 + item_condition*C2_MoA + item_condition*C1_MoA + (1|participant) + (1|word), data = ya)
anova(v7, v9)
#v10 = lmer(voicing_proportion ~ 1 + item_condition*C1_MoA*C2_MoA + (1|participant) + (1|word), data = ya)
#anova(v9, v10)
#v11 = lmer(voicing_proportion ~ 1 + item_condition*C2_MoA + item_condition*C1_MoA + (1+item_condition*C2_MoA|participant) + (1|word), data = ya)
#anova(v9, v11)
#v12 = lmer(voicing_proportion ~ 1 + item_condition*C2_MoA + item_condition*C1_MoA + (1+item_condition|participant) + (1|word), data = ya)
#anova(v9, v12)
#v13 = lmer(voicing_proportion ~ 1 + item_condition*C2_MoA + item_condition*C1_MoA + (1+C2_MoA|participant) + (1|word), data = ya)
#anova(v9, v13)
v14 = lmer(voicing_proportion ~ 1 + item_condition*C2_MoA + item_condition*C1_MoA + (1+item_condition*C1_MoA+item_condition*C2_MoA|participant) + (1|word), data = ya)
anova(v9, v14)
# check v14 for degenerate random effects structure

#vignette("KB", package = "RePsychLing")

summary(rePCA(v14))

# remove highest order interaction

v15 = lmer(voicing_proportion ~ 1 + item_condition*C2_MoA + item_condition*C1_MoA + (1+item_condition+C1_MoA+C2_MoA|participant) + (1|word), data = ya)

summary(rePCA(v15))

# remove smallest variance component (C2_MoA)
summary(v15)

v16 = lmer(voicing_proportion ~ 1 + item_condition*C2_MoA + item_condition*C1_MoA + (1+item_condition+C1_MoA|participant) + (1|word), data = ya)

summary(rePCA(v16))

# remove smallest variance component (item_condition)

v17 = lmer(voicing_proportion ~ 1 + item_condition*C2_MoA + item_condition*C1_MoA + (1+C1_MoA|participant) + (1|word), data = ya)

summary(rePCA(v17))
summary(v17)

# does further reduction result in a significantly worse model?

v18 = lmer(voicing_proportion ~ 1 + item_condition*C2_MoA + item_condition*C1_MoA + (1|participant) + (1|word), data = ya)
anova(v17, v18)
# yes
# check vif
vif.mer <- function (fit) {
  ## adapted from rms::vif
  v <- vcov(fit)
  nam <- names(fixef(fit))
  ## exclude intercepts
  ns <- sum(1 * (nam == "Intercept" | nam == "(Intercept)"))
  if (ns > 0) {
    v <- v[-(1:ns), -(1:ns), drop = FALSE]
    nam <- nam[-(1:ns)]
  }
  d <- diag(v)^0.5
  v <- diag(solve(v/(d %o% d)))
  names(v) <- nam
  v
}

vif.mer(v17)

par(mfrow=c(1,2))
plot(fitted(v17), residuals(v17))
qqnorm(residuals(v17))
qqline(residuals(v17))
# shit I should have used a model for sigmoid distribution
# or model voicing dur instead
v19 = glmmadmb(voicing_prop_trans ~ item_condition*C2_MoA + item_condition*C1_MoA + (1+C1_MoA|participant) + (1|word), data = ya, family = "beta")
v19 = lmer(voicing_dur ~ 1 + item_condition*C2_MoA + item_condition*C1_MoA + (1+C1_MoA|participant) + (1|word), data = ya)

par(mfrow=c(2,2))
plot(fitted(v19), residuals(v19))
qqnorm(residuals(v19))
qqline(residuals(v19))

v20 = lmer(voicing_dur ~ 1 + item_condition*C2_MoA + item_condition*C1_MoA + (1+C1_MoA|participant) + (1|word), data = ya, subset = abs(scale(resid(v19))) < 2.5)

plot(fitted(v20), residuals(v20))
qqnorm(residuals(v20))
qqline(residuals(v20))

# time to check the residuals

plot(effect("item_condition:C1_MoA", v18, confidence.level=0.95))
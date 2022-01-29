library(lattice)
library(ggpubr)
library(lme4)
install.packages("glmmADMB")
library(glmmADMB)
library(viridis)
library(betareg)
library(ggplot2)
library(gam)
library(mgcv)
library(gamlss) 
library(glmmTMB)

setwd("/Users/andresmontes/Documents/Andres_Academico/Articulo_agrupamiento/archivos_para_someter")

#######################  SUBGROUP SIZE ANALYSIS #########################

###### TIPUTINI MODEL #########
tbs <- read.csv("area_sg_sex_tbs.csv", h=T)

xyplot(mean ~ area_m2_ha | sex,
       xlab = "area", data = tbs, col = 1,
       ylab = "subgroup",
       layout = c(3,1), groups = year, type = "p",
       strip = strip.custom(bg = 'white',
                            par.strip.text = list(cex = 1.2)),
       scales = list(alternating = T,
                     x = list(relation = "same"),
                     y = list(relation = "same")))


m1 <- glmer(ceiling(mean) ~ area_m2_ha*sex+ (1|year), data = tbs, family = poisson )
summary(m1)
m <- glm(ceiling(mean) ~ area_m2_ha+sex, data = tbs_sin_NA, family = poisson )
summary(m)
tbs_sin_NA <- na.omit(tbs)

m2 <- glmer(ceiling(mean) ~ area_m2_ha*sex+ (1|year), data = tbs_sin_NA, family = poisson )
summary(m2)

cov.m <- vcovHC(m, type="HC0")
std.err <- sqrt(diag(cov.m))
r.est <- cbind(Estimate= coef(m), "Robust SE" = std.err,
               "Pr(>|z|)" = 2 * pnorm(abs(coef(m)/std.err), lower.tail=FALSE),
               LL = coef(m) - 1.96 * std.err,
               UL = coef(m) + 1.96 * std.err)

with(m, cbind(res.deviance = deviance, df = df.residual,
               p = pchisq(deviance, df.residual, lower.tail=FALSE)))

## update m1 model dropping prog
mu <- update(m, . ~ . - sex)
## test model differences with chi square test
anova(mu, m, test="Chisq")

install.packages("msm")
library(msm)
s <- deltamethod(list(~ exp(x1), ~ exp(x2), ~ exp(x3), ~ exp(x4)), 
                 coef(m), cov.m)

## exponentiate old estimates dropping the p values
rexp.est <- exp(r.est[, -3])
## replace SEs with estimates for exponentiated coefficients
rexp.est[, "Robust SE"] <- s

rexp.est

(s1 <- data.frame(area_m2_ha = mean(tbs_sin_NA$area_m2_ha),
                  sex = factor(1:3, levels = 1:3, labels = levels(tbs$sex))))



predict(m, s1, type="response", se.fit=TRUE)

## calculate and store predicted values
tbs_sin_NA$phat <- predict(m, type="response")

## order by program and then by math
tbs_sin_NA_o <- tbs_sin_NA[with(tbs_sin_NA, order(sex, area_m2_ha)), ]

## create the plot
ggplot(tbs_sin_NA_o, aes(x = area_m2_ha, y = phat, colour = sex)) +
  geom_point(aes(y = mean), alpha=.5, position=position_jitter(h=.2)) +
  geom_line(size = 1) +
  labs(x = "", y = "")+
  facet_wrap(~sex)



performance::r2(m2)

E1 <- resid(m2, type = "pearson")
N <- nrow(tbs_sin_NA)
p <- length(fixef(m2)) + 1
Overdispersion <- sum(E1^2) / (N - p)
Overdispersion


F1 <- fitted(m2, type = "response")
par(mfrow = c(2, 2), mar = c(5, 5, 2, 2))
plot(x = F1, y = E1, xlab = "Fitted values",ylab = "Pearson residuals")
abline(h = 0, lty = 2)
plot(x = tbs_sin_NA$area_m2_ha, y = E1, xlab = "area",ylab = "Pearson residuals")
abline(h = 0, lty = 2)
boxplot(E1 ~ sex, data = tbs_sin_NA,xlab = "sex",ylab = "Pearson residuals")
abline(h = 0, lty = 2)
boxplot(E1 ~ year, data = tbs_sin_NA, xlab = "year",ylab = "Pearson residuals")
abline(h = 0, lty = 2)

dev.off()


# pred.data = data.frame(area_m2_ha=tbs_sin_NA$area_m2_ha, year=tbs_sin_NA$year,sex=tbs_sin_NA$sex) 
# pred.data$mean = predict(m1, newdata=pred.data, type="response")




###### SAN JUAN #########
sj <- read.csv("area_sg_sex_sj.csv", h=T)

xyplot(mean ~ area_m2_ha | sex,
       xlab = "area", data = sj, col = 1,
       ylab = "subgroup",
       layout = c(3,1), groups = year, type = "p",
       strip = strip.custom(bg = 'white',
                            par.strip.text = list(cex = 1.2)),
       scales = list(alternating = T,
                     x = list(relation = "same"),
                     y = list(relation = "same")))


m3 <- glmer(ceiling(mean) ~ area_m2_ha*sex+ (1|year), data = sj, family = poisson )
summary(m3)





E1 <- resid(m3, type = "pearson")
N <- nrow(sj)
p <- length(fixef(m3)) + 1
Overdispersion <- sum(E1^2) / (N - p)
Overdispersion

sj_sin_NA <- na.omit(sj)

m4 <- glmer(ceiling(mean) ~ area_m2_ha*sex+ (1|year), data = sj_sin_NA, family = poisson )
summary(m4)

f <- subset(tbs_sin_NA, sex == "female")
plot(f$area_m2_ha, f$mean)
# new Y estimated


pred.data = data.frame(area_m2_ha=sj_sin_NA$area_m2_ha, year=sj_sin_NA$year,sex=sj_sin_NA$sex) 
pred.data$mean = predict(m3, newdata=pred.data, type="response")



model_colors <- RColorBrewer::brewer.pal(3, "Set1")
paleta <- c("#161616", "#161616", "#161616")
new_labels <- c("All"="Both sexes", "female"="Females", "male"="Males")

model_colors <- RColorBrewer::brewer.pal(3, "Set1")
paleta <- c("#161616", "#161616", "#161616")
n_labels <- c("all"="Both sexes", "female"="Females", "male"="Males")




tb <- ggplot(tbs_sin_NA, aes(area_m2_ha, ceiling(mean))) +
  labs(title = "Ateles belzebuth") +
  geom_point(aes(color=sex),size=3, alpha=0.4)  +
  geom_smooth(aes(color=sex, fill=sex),method="glm", method.args=list(family=poisson))+
  scale_color_manual(values = paleta)+
  scale_fill_manual(values = paleta) +
  scale_y_continuous(breaks = c(2,4,6,8,10))+
  labs(x=expression(Fruit~productivity~index~(m^2/ha))) +ylab("Subgroup size")+
  theme(plot.title = element_text(face="bold.italic"),axis.text.x = element_text(hjust = 0.5, vjust = 0.5),legend.position="none",panel.background = element_rect(fill = "white", colour = "grey40")) +
  theme(axis.text=element_text(size=11),axis.title.x = element_blank(), axis.title=element_text(size=12),strip.text = element_text(size=12))+
  facet_wrap(~sex,ncol = 3, labeller = labeller(sex=new_labels))


s <- ggplot(sj_sin_NA, aes(area_m2_ha, ceiling(mean))) +
  labs(title="Ateles hybridus") +
  geom_point(aes(color=sex),size=3, alpha=0.4)  +
  geom_smooth(aes(color=sex, fill=sex),method="glm", method.args=list(family=poisson))+
  scale_color_manual(values = paleta)+
  scale_fill_manual(values = paleta) +
  scale_y_continuous(breaks = c(2,4,6,8,10))+
  labs(x=expression(Fruit~productivity~index~(m^2/ha))) +ylab("Subgroup size")+
  theme(plot.title = element_text(face="bold.italic"),axis.text.x = element_text(hjust = 0.5, vjust = 0.5),legend.position="none",panel.background = element_rect(fill = "white", colour = "grey40")) +
  theme(axis.text=element_text(size=11), axis.title=element_text(size=12),strip.text = element_text(size=12))+
  facet_wrap(~sex,ncol = 3,  labeller = labeller(sex=n_labels))


png("mod_pois_tbs_sj.png", height = 18, width = 16, units = "cm", res = 150)
ggarrange(tb, s, labels = c("A","B"),
          ncol= 1,nrow = 2)
dev.off()



#######################  DIET ANALYSIS #########################

###### TIPUTINI #########
tbs_sin_NA <- na.omit(tbs)


N <- nrow(tbs_sin_NA)
tbs_sin_NA$PropBeta <- (tbs_sin_NA$fruits_comp_p * (N - 1) + 0.5)/N

m_b_tbs <- betareg(PropBeta ~ area_m2_ha*sex, data = tbs_sin_NA)
summary(m_b_tbs)
m5 <- glmmTMB(PropBeta ~ area_m2_ha*sex + (1|year), tbs_sin_NA, family=list(family="beta",link="logit"))
summary(m5)


pred.data = data.frame(area_m2_ha=tbs_sin_NA$area_m2_ha, year=tbs_sin_NA$year,sex=tbs_sin_NA$sex) 
pred.data$PropBeta = predict(m5, newdata=pred.data, type="response")
pred.data$var = predict(m5, newdata=pred.data, type="variance")
pred.data$q25=predict(m5, type = "quantile", at = c(0.25))
pred.data$q75=predict(m5, type = "quantile", at = c(0.75))

new_labels <- c("All"="Both sexes", "female"="Females", "male"="Males")

f_tbs <- ggplot(tbs_sin_NA, aes(x = area_m2_ha, y = PropBeta)) +
  labs(title="Ateles belzebuth")+
  geom_point(size = 3, aes(fill = sex), shape = 21) +
  scale_fill_grey() +
  geom_line(size=1,aes(y = predict(m_b_tbs, tbs_sin_NA), 
                colour = "logit", linetype = "logit")) +
  scale_colour_manual("", values = c("blue")) +
  scale_linetype_manual("", values = c("solid")) +
  labs(x=expression(Fruit~productivity~index~(m^2/ha))) +ylab("Proportion of fruit comsumption")+
  theme(plot.title = element_text(face="bold.italic"),axis.text.x = element_text(hjust = 0.5, vjust = 0.5),legend.position="none",panel.background = element_rect(fill = "white", colour = "grey40")) +
  theme(axis.text=element_text(size=11), axis.title.x = element_blank(),axis.title=element_text(size=12),strip.text = element_text(size=12))+
  facet_wrap(~sex,ncol = 3,labeller = labeller(sex=new_labels))







###### SAN JUAN #########
sj_sin_NA <- na.omit(sj)


Ns <- nrow(sj_sin_NA)
sj_sin_NA$PropBeta <- (sj_sin_NA$fruits_comp_p * (Ns - 1) + 0.5)/Ns

m_b_sj <- betareg(PropBeta ~ area_m2_ha*sex, data = sj_sin_NA)
summary(m_b_sj)

m6 <- glmmTMB(PropBeta ~ area_m2_ha*sex + (1|year), sj_sin_NA, family=list(family="beta",link="logit"))
summary(m6)

pred.data.sj = data.frame(area_m2_ha=sj_sin_NA$area_m2_ha, year=sj_sin_NA$year,sex=sj_sin_NA$sex) 
pred.data.sj$PropBeta = predict(m6, newdata=pred.data, type="response")
pred.data.sj$var = predict(m6, newdata=pred.data, type="variance")
pred.data.sj$q25=predict(m6, type = "quantile", at = c(0.25))
pred.data.sj$q75=predict(m6, type = "quantile", at = c(0.75))

new_labels_s <- c("all"="Both sexes", "female"="Females", "male"="Males")

f_sj <- ggplot(sj_sin_NA, aes(x = area_m2_ha, y = PropBeta)) +
  labs(title="Ateles hybridus")+
  geom_point(size = 3, aes(fill = sex), shape = 21) +
  scale_fill_grey() +
  geom_line(size=1,aes(y = predict(m_b_sj, sj_sin_NA), 
                       colour = "logit", linetype = "logit")) +
  scale_colour_manual("", values = c("blue")) +
  scale_linetype_manual("", values = c("solid")) +
  labs(x=expression(Fruit~productivity~index~(m^2/ha))) +ylab("Proportion of fruit comsumption")+
  theme(plot.title = element_text(face="bold.italic"),axis.text.x = element_text(hjust = 0.5, vjust = 0.5),legend.position="none",panel.background = element_rect(fill = "white", colour = "grey40")) +
  theme(axis.text=element_text(size=11),axis.title=element_text(size=12),strip.text = element_text(size=12))+
  facet_wrap(~sex,ncol = 3,labeller = labeller(sex=new_labels_s))

png("mod_beta_tbs_sj.png", height = 18, width = 16, units = "cm", res = 150)
ggarrange(f_tbs, f_sj, labels = c("A","B"),
          ncol= 1,nrow = 2)
dev.off()




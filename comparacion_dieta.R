## DIET COMPARISON A.BELZEBUTH VS A.HYBRIDUS ##

library(lattice)
library(lme4)
library(viridis)
library(betareg)
library(ggplot2)
library(gam)
library(mgcv)
library(gamlss) 
library(ggplot2)
library(devtools)
library(nlme)
library(sjstats)
library(piecewiseSEM)
install.packages("glmmTMB")
library(glmmTMB)
install.packages("ggsignif")
library(ggsignif)

setwd("/Users/andresmontes/Documents/Andres_Academico/Articulo_agrupamiento")

# diet data set 
diet <- read.csv("dieta_indivs_mes_sj_tbs.csv", h=T)
porcentaje <- unlist(lapply(diet$proporcion, function(x){x*100}))

diet$porcentaje <- porcentaje


#MODELS
#Differences in the diaet between A.belzebuth and A.hybridus
modmix1<- lme(porcentaje ~ species*item, random = ~1|individuo, data=diet, na.action = na.omit)
summary(modmix1)
sjstats::r2(modmix1)

beta_diet <- glmmTMB(PropBeta ~ species+item + (1|individuo), diet, family=list(family="beta",link="logit"))
summary(beta_diet)

fruit <- filter(diet, item == "Fruits")
leaves <- filter(diet, item == "Leaves")
mod_anidado_f <- glmmTMB(PropBeta ~ species + (1|species/individuo), fruit,  family=list(family="beta",link="logit"))
summary(mod_anidado_f)

mod_anidado_l <- glmmTMB(PropBeta ~ species + (1|species/individuo), leaves,  family=list(family="beta",link="logit"))
summary(mod_anidado_l)


n <- nrow(diet)
diet$PropBeta <- (diet$proporcion * (n - 1) + 0.5)/n

m_diet <- betareg(PropBeta ~ species+item, data = diet)
summary(m_diet)



performance::r2(beta_diet)


png("dieta.items.png", height = 10, width = 12, units = "cm", res = 150)
ggplot(dieta, aes(x=item, y=porcentaje, fill=species)) + 
  geom_boxplot() + ylim(0, 100)+
  labs( x="Item", y = "% Consumption time")+
  geom_signif(comparisons=list(c(2, 2)), annotations="***",
              y_position = 100, tip_length = 0, vjust=0.4, textsize = 7)+
  geom_signif(comparisons=list(c(3, 3)), annotations="***",
              y_position = 85, tip_length = 0, vjust=0.4, textsize = 7)+
  scale_fill_manual(name = "Species", labels = c("A. belzebuth", "A. hybridus"),values=c("#404040" , "#C8C8C8"))+
  theme_classic()+theme(legend.text = element_text(size= 9,face = "italic"), legend.position = c(0.85, 0.9))+
  theme(axis.text=element_text(size=10), axis.title=element_text(size=12),strip.text = element_text(size=10, face = "italic"))
dev.off()

nrow(subset(dieta, species == "A.hybridus"))

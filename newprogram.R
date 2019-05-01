


d=read.table("data.txt")


y=d$V1
busnumber=d$V2
metraj=d$V3
mantaghe=d$V4
taghatonumber=d$V5
station=d$V6
khattype=d$V7
khattype=factor(khattype)

######## packages #####


library(nloptr)
library(lme4)
library(nlme)
library(zoo)
library(lmtest)


############### I??E?? ?CE??? ??????? ??C?? ?CI? ##########

dwtest(y~busnumber+metraj+taghatonumber+station+khattype)



########   ?I? ????? ?C??C??   ########

mod1 <- glmer(y ~ 1+( 1|mantaghe),family="poisson", data=d)
summary(mod1)



################################################################################################
######### ?I? EC ??? C? ?EIC E?CI?? EC ?EU???C? ??E?? #########


mod2 <- glmer(y~ busnumber+metraj+station+I(khattype)+ taghatonumber+( 1 | mantaghe),family="poisson", data=d)
summary(mod2)




################################################################################################
########  ?I? ??? C? ?EIC E?CI?? ? O?E E?CI??  #########

#### E?ICI CE?E?? ?EU?? ??E??  #####

mod3 <- glmer(y~ busnumber+metraj+station+ taghatonumber+khattype
+( 1 | mantaghe)+(busnumber|mantaghe)
,family="poisson", data=d)
summary(mod3)


#### ?E?C? ?EU?? ??E??  #####

mod4 <- glmer(y~ busnumber+metraj+station+ taghatonumber+khattype
+( 1 | mantaghe)+(metraj|mantaghe)
,family="poisson", data=d)
summary(mod4)


#### C??E?C? ?EU?? ??E??  #####

mod5 <- glmer(y~ busnumber+metraj+station+ taghatonumber+khattype
+( 1 | mantaghe)+(station|mantaghe)
,family="poisson", data=d)
summary(mod5)


#### E?ICI E?C?? ?EU?? ??E??  #####
khattype=factor(khattype)
mod6 <- glmer(y~ busnumber+metraj+station+ taghatonumber+I(khattype)
+( 1 | mantaghe)+(taghatonumber|mantaghe)
,family="poisson", data=d)
summary(mod6)


#### ??? I??? ?EU?? ??E??  #####

mod7 <- glmer(y~ busnumber+metraj+station+ taghatonumber+khattype
+( 1 | mantaghe)+(I(khattype)|mantaghe)
,family="poisson", data=d)
summary(mod7)



######### C?EICE E?E??? ?I? ###########

anova(mod1,mod2,mod3,mod4,mod5,mod6,mod7)






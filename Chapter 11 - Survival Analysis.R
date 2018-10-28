#######################################
#### CHAPTER 1 - SURVIVAL ANALYSIS ####
#######################################

library(survival)
library(HSAUR3)
library(coin)

#### Glioma radioimmunotherapy ####

data("glioma", package="coin")

# Survival times comparing treated and control patients (Kaplan-Mayer estimates)
layout(matrix(1:2, ncol=2))
g3 <- subset(glioma, histology == "Grade3")
plot(survfit(Surv(time, event) ~ group, data=g3), lty=c(2,1),
     main="Grade III glioma", ylab="Probability", xlab="Survival time in months",
     legend.text=c("Control", "Treated"), legend.bty="n")
g4 <- subset(glioma, histology == "GBM")
plot(survfit(Surv(time, event) ~ group, data=g4), lty=c(2,1),
     main="Grade IV glioma", ylab="Probability", xlab="Survival time in months",
     xlim=c(0, max(glioma$time) * 1.05))
# Patients treated with the novel radioimmunotherapy appear to survive lonter. log-rank test is performed
# to assess this
survdiff(Surv(time, event) ~ group, data=g3)
# Survival times are indeed different in both groups

# Since groups are small relaying on asymptotic tests is ill advised. We use surv_test to compiute an 
# exact conditional test (using logrank_test since surv_test is defunct)
logrank_test(Surv(time, event) ~ group, data=g3)
logrank_test(Surv(time, event) ~ group, data=g4)

# To answer the question of whether the new therapy is superiorfor both groups we use statification
logrank_test(Surv(time, event) ~ group | histology, data=glioma)

# Results confirm the findings from the graphs




################################
#### FIN DE LA PROGRAMACI?N ####
################################
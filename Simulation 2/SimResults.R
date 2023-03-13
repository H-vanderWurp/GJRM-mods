
Erg1 <- data.frame()
setwd("Results/Results N0.1")
for(i in list.files()){
  load(i)
  erg <- do.call(rbind, erg)
  Erg1 <- rbind(Erg1, erg)
}
Erg1$tau <- 0.1

#ggplot(Erg1, aes(y = MSE, fill = Modell, x = as.factor(tau))) + geom_boxplot() + ylim(0, 1.5) #+ facet_wrap(~Cop)
#ggplot(Erg1, aes(y = nu, fill = Modell, x = as.factor(tau))) + geom_boxplot() + scale_y_continuous(trans = "log10")

Erg2 <- data.frame()
setwd("../Results N0.25")
for(i in 1:50){
  load(paste0("N0.25.", i, ".Rdata"))
  erg <- do.call(rbind, erg)
  Erg2 <- rbind(Erg2, erg)
}
Erg2$tau <- 0.25

Erg3 <- data.frame()
setwd("../Results N0.5")
for(i in 1:50){
  load(paste0("N0.5.", i, ".Rdata"))
  erg <- do.call(rbind, erg)
  Erg3 <- rbind(Erg3, erg)
}
Erg3$tau <- 0.5

Erg4 <- data.frame()
setwd("../Results N0.75")
for(i in 1:50){
  load(paste0("N0.75.", i, ".Rdata"))
  erg <- do.call(rbind, erg)
  Erg4 <- rbind(Erg4, erg)
}
Erg4$tau <- 0.75

Erg5 <- data.frame()
setwd("../Results N0.1m")
for(i in 1:50){
  load(paste0("N0.1m.", i, ".Rdata"))
  erg <- do.call(rbind, erg)
  Erg5 <- rbind(Erg5, erg)
}
Erg5$tau <- -0.1

Erg6 <- data.frame()
setwd("../Results N0.25m")
for(i in 1:50){
  load(paste0("N0.25m.", i, ".Rdata"))
  erg <- do.call(rbind, erg)
  Erg6 <- rbind(Erg6, erg)
}
Erg6$tau <- -0.25

Erg7 <- data.frame()
setwd("../Results N0.5m")
for(i in 1:50){
  load(paste0("N0.5m.", i, ".Rdata"))
  erg <- do.call(rbind, erg)
  Erg7 <- rbind(Erg7, erg)
}
Erg7$tau <- -0.5

Erg8 <- data.frame()
setwd("../Results N0.75m")
for(i in 1:50){
  load(paste0("N0.75m.", i, ".Rdata"))
  erg <- do.call(rbind, erg)
  Erg8 <- rbind(Erg8, erg)
}
Erg8$tau <- -0.75

ErgN <- rbind(Erg1, Erg2, Erg3, Erg4, Erg5, Erg6, Erg7, Erg8)
rm(Erg1, Erg2, Erg3, Erg4, Erg5, Erg6, Erg7, Erg8, erg, i)

library(ggplot2)


############################### C0 #####################################

Erg1 <- data.frame()
setwd("../Results C0.01")
for(i in list.files()){
  load(i)
  erg <- do.call(rbind, erg)
  Erg1 <- rbind(Erg1, erg)
}
Erg1$tau <- 0.1

Erg2 <- data.frame()
setwd("../Results C0.025")
for(i in list.files()){
  load(i)
  erg <- do.call(rbind, erg)
  Erg2 <- rbind(Erg2, erg)
}
Erg2$tau <- 0.25

Erg3 <- data.frame()
setwd("../Results C0.05")
for(i in list.files()){
  load(i)
  erg <- do.call(rbind, erg)
  Erg3 <- rbind(Erg3, erg)
}
Erg3$tau <- 0.5

Erg4 <- data.frame()
setwd("../Results C0.075")
for(i in list.files()){
  load(i)
  erg <- do.call(rbind, erg)
  Erg4 <- rbind(Erg4, erg)
}
Erg4$tau <- 0.75

ErgC0 <- rbind(Erg1, Erg2, Erg3, Erg4)
rm(Erg1, Erg2, Erg3, Erg4, i, erg)

#################### F ########################

Erg1 <- data.frame()
setwd("../Results F0.1")
for(i in list.files()){
  load(i)
  erg <- do.call(rbind, erg)
  Erg1 <- rbind(Erg1, erg)
}
Erg1$tau <- 0.1

Erg2 <- data.frame()
setwd("../Results F0.25")
for(i in list.files()){
  load(i)
  erg <- do.call(rbind, erg)
  Erg2 <- rbind(Erg2, erg)
}
Erg2$tau <- 0.25

Erg3 <- data.frame()
setwd("../Results F0.5")
for(i in list.files()){
  load(i)
  erg <- do.call(rbind, erg)
  Erg3 <- rbind(Erg3, erg)
}
Erg3$tau <- 0.5

Erg4 <- data.frame()
setwd("../Results F0.75")
for(i in list.files()){
  load(i)
  erg <- do.call(rbind, erg)
  Erg4 <- rbind(Erg4, erg)
}
Erg4$tau <- 0.75

Erg5 <- data.frame()
setwd("../Results F0.1m")
for(i in list.files()){
  load(i)
  erg <- do.call(rbind, erg)
  Erg5 <- rbind(Erg5, erg)
}
Erg5$tau <- -0.1

Erg6 <- data.frame()
setwd("../Results F0.25m")
for(i in list.files()){
  load(i)
  erg <- do.call(rbind, erg)
  Erg6 <- rbind(Erg6, erg)
}
Erg6$tau <- -0.25

Erg7 <- data.frame()
setwd("../Results F0.5m")
for(i in list.files()){
  load(i)
  erg <- do.call(rbind, erg)
  Erg7 <- rbind(Erg7, erg)
}
Erg7$tau <- -0.5

Erg8 <- data.frame()
setwd("../Results F0.75m")
for(i in list.files()){
  load(i)
  erg <- do.call(rbind, erg)
  Erg8 <- rbind(Erg8, erg)
}
Erg8$tau <- -0.75

ErgF <- rbind(Erg1, Erg2, Erg3, Erg4, Erg5, Erg6, Erg7, Erg8)
rm(Erg1, Erg2, Erg3, Erg4, Erg5, Erg6, Erg7, Erg8, erg, i)

################ G0 ######################


Erg1 <- data.frame()
setwd("../Results G0.01")
for(i in list.files()){
  load(i)
  erg <- do.call(rbind, erg)
  Erg1 <- rbind(Erg1, erg)
}
Erg1$tau <- 0.1

Erg2 <- data.frame()
setwd("../Results G0.025")
for(i in list.files()){
  load(i)
  erg <- do.call(rbind, erg)
  Erg2 <- rbind(Erg2, erg)
}
Erg2$tau <- 0.25

Erg3 <- data.frame()
setwd("../Results G0.05")
for(i in list.files()){
  load(i)
  erg <- do.call(rbind, erg)
  Erg3 <- rbind(Erg3, erg)
}
Erg3$tau <- 0.5

Erg4 <- data.frame()
setwd("../Results G0.075")
for(i in list.files()){
  load(i)
  erg <- do.call(rbind, erg)
  Erg4 <- rbind(Erg4, erg)
}
Erg4$tau <- 0.75

ErgG0 <- rbind(Erg1, Erg2, Erg3, Erg4)
rm(Erg1, Erg2, Erg3, Erg4, i, erg)

################# J0 #########################


Erg1 <- data.frame()
setwd("../Results J0.01")
for(i in list.files()){
  load(i)
  erg <- do.call(rbind, erg)
  Erg1 <- rbind(Erg1, erg)
}
Erg1$tau <- 0.1

Erg2 <- data.frame()
setwd("../Results J0.025")
for(i in list.files()){
  load(i)
  erg <- do.call(rbind, erg)
  Erg2 <- rbind(Erg2, erg)
}
Erg2$tau <- 0.25

Erg3 <- data.frame()
setwd("../Results J0.05")
for(i in list.files()){
  load(i)
  erg <- do.call(rbind, erg)
  Erg3 <- rbind(Erg3, erg)
}
Erg3$tau <- 0.5

Erg4 <- data.frame()
setwd("../Results J0.075")
for(i in list.files()){
  load(i)
  erg <- do.call(rbind, erg)
  Erg4 <- rbind(Erg4, erg)
}
Erg4$tau <- 0.75

ErgJ <- rbind(Erg1, Erg2, Erg3, Erg4)
rm(Erg1, Erg2, Erg3, Erg4, i, erg)


############################### C90 #####################################

Erg1 <- data.frame()
setwd("../Results C90.01")
for(i in list.files()){
  load(i)
  erg <- do.call(rbind, erg)
  Erg1 <- rbind(Erg1, erg)
}
Erg1$tau <- -0.1

Erg2 <- data.frame()
setwd("../Results C90.025")
for(i in list.files()){
  load(i)
  erg <- do.call(rbind, erg)
  Erg2 <- rbind(Erg2, erg)
}
Erg2$tau <- -0.25

Erg3 <- data.frame()
setwd("../Results C90.05")
for(i in list.files()){
  load(i)
  erg <- do.call(rbind, erg)
  Erg3 <- rbind(Erg3, erg)
}
Erg3$tau <- -0.5

Erg4 <- data.frame()
setwd("../Results C90.075")
for(i in list.files()){
  load(i)
  erg <- do.call(rbind, erg)
  Erg4 <- rbind(Erg4, erg)
}
Erg4$tau <- -0.75

ErgC90 <- rbind(Erg1, Erg2, Erg3, Erg4)
rm(Erg1, Erg2, Erg3, Erg4, i, erg)

########## Plots for all copula classes ##########

ErgN$Cop <- "N"
ErgF$Cop <- "F"
ErgC0$Cop <- "C0"
ErgC90$Cop <- "C90"
ErgJ$Cop <- "J"
ErgG0$Cop <- "G0"

ErgGesamt <- rbind(ErgN, ErgF, ErgC0, ErgC90, ErgJ, ErgG0)

levels(ErgGesamt$Modell) <- c(levels(ErgGesamt$Modell), "unpenalised")
ErgGesamt$Modell[ErgGesamt$Modell == "vanilla"] <- "unpenalised"
ErgGesamt$Model <- ErgGesamt$Modell


## Figure 3.1
#pdf("C:/Users/hendr/Dropbox/CGAMLSS/Paper2/SimResultsN_MSE.pdf", height = 3, width = 6)
ggplot(ErgGesamt[ErgGesamt$Cop=="N",], aes(y = MSE, fill = Model, x = as.factor(tau))) + geom_boxplot() + ylim(0, 3.25) +
  xlab(expression("Kendall's"~~tau)) + ylab("SSE") #+ facet_wrap(~Cop) 
#dev.off()

## Figure 3.4
#pdf("C:/Users/hendr/Dropbox/CGAMLSS/Paper2/SimResultsALL_MSE.pdf", height = 5, width = 8)
ggplot(ErgGesamt, aes(y = MSE, fill = Model, x = as.factor(tau))) + geom_boxplot() + ylim(0, 3.25) +
  xlab(expression("Kendall's"~~tau)) + ylab("SSE") + facet_wrap(~Cop, nrow=3) 
#ggplot(ErgC0, aes(y = MSE, fill = Modell, x = as.factor(tau))) + geom_boxplot() + ylim(0, 1.5)
#dev.off()

## True positive rate: 
## Figure 3.2
#pdf("C:/Users/hendr/Dropbox/CGAMLSS/Paper2/SimResultsN_truepos.pdf", height = 3, width = 6)
a <- aggregate(true.positive ~ Model + tau + Cop, ErgGesamt, mean)
#ggplot(a[a$Cop == "N",], aes(fill = Modell, y=true.positive, x = as.factor(tau))) + 
#  geom_bar(position="dodge", stat="identity", colour="black") +
#  xlab(expression("Kendalls's"~~tau)) + ylab("true positive rate") #+ facet_wrap(~Cop, nrow=3)
ggplot(a[a$Cop == "N",], aes(shape = Model, y=true.positive, x = as.factor(tau))) + 
  geom_point() + geom_line(aes(group = Model, linetype = Model)) +  
  xlab(expression("Kendalls's"~~tau)) + ylab("true positive rate") 
#dev.off()


## Figure 3.5
#pdf("C:/Users/hendr/Dropbox/CGAMLSS/Paper2/SimResultsALL_truepos.pdf", height = 5, width = 8)
a <- aggregate(true.positive ~ Model + tau + Cop, ErgGesamt, mean)
#ggplot(a, aes(fill = Modell, y=true.positive, x = as.factor(tau))) + 
#  geom_col(width = 0.65, position = position_dodge(0.75)) +
#  xlab(expression("Kendalls's"~~tau)) + ylab("true positive rate") + facet_wrap(~Cop, nrow=3)
ggplot(a, aes(shape = Model, y=true.positive, x = as.factor(tau))) + 
  geom_point() + geom_line(aes(group = Model, linetype = Model)) +  
  xlab(expression("Kendalls's"~~tau)) + ylab("true positive rate") + facet_wrap(~Cop, nrow=3) 
#dev.off()


## Figure 3.3
#pdf("C:/Users/hendr/Dropbox/CGAMLSS/Paper2/SimResultsN_trueneg.pdf", height = 3, width = 6)
a <- aggregate(true.negative ~ Model + tau + Cop, ErgGesamt, mean)
#ggplot(a[a$Cop == "N",], aes(fill = Modell, y=true.negative, x = as.factor(tau))) + 
#  geom_bar(position="dodge", stat="identity", colour="black") +
#  xlab(expression("Kendalls's"~~tau)) + ylab("true negative rate") #+ facet_wrap(~Cop, nrow=3)
ggplot(a[a$Cop == "N",], aes(shape = Model, y=true.negative, x = as.factor(tau))) + 
  geom_point() + geom_line(aes(group = Model, linetype = Model)) +  
  xlab(expression("Kendalls's"~~tau)) + ylab("true negative rate") 
#dev.off()


## Figure 3.6
#pdf("C:/Users/hendr/Dropbox/CGAMLSS/Paper2/SimResultsALL_trueneg.pdf", height = 5, width = 8)
a <- aggregate(true.negative ~ Model + tau + Cop, ErgGesamt, mean)
#ggplot(a, aes(fill = Modell, y=true.negative, x = as.factor(tau))) + 
#  geom_bar(position="dodge", stat="identity", colour="black") +
#  xlab(expression("Kendalls's"~~tau)) + ylab("true negative rate") + facet_wrap(~Cop, nrow=3)
ggplot(a, aes(shape = Model, y=true.negative, x = as.factor(tau))) + 
  geom_point() + geom_line(aes(group = Model, linetype = Model)) +  
  xlab(expression("Kendalls's"~~tau)) + ylab("true negative rate") + facet_wrap(~Cop, nrow=3)
#dev.off()

library(knitr)
library(tidyr)
library(ggplot2)

ratio <- factor(rep(c("1:1","1:3"),each=2))
block=factor(rep(c(1,2),times=2))
data =c(6.28,6.64,6.43,6.70,
        6.36,6.55,6.31,6.64,
        6.38,6.64,6.41,6.75,
        6.40,6.58,6.52,6.65,
        6.48,6.60,6.50,6.72,
        6.44,6.56,6.56,6.70,
        6.46,6.62,6.60,6.70,
        6.40,6.57,6.59,6.75)
t=as.data.frame(matrix(data,nrow=4))
t=cbind(ratio,block,t)
t

colnames(t)[3:10]=c("tap whole white","tap whole brown","tap skim white","tap skim brown","purif whole white","purif whole brown","purif skim white","purif skim brown")
kable(t)
d=pivot_longer(
  t,
  cols=-c("ratio","block"),
  values_to="PH"
)
d$water=factor(rep(rep(c("tap","purified"),each=4),times=4))
d$milk = factor(rep(rep(c("whole","skim"),each=2),times=8))
d$sugar=factor(rep(c("white","brown"),times=16))
d$name=NULL
fit <- aov(PH ~water+milk+ratio+sugar+water:milk+water:ratio+water:sugar+milk:ratio+milk:sugar+ratio:sugar+block,d)
anova(fit)

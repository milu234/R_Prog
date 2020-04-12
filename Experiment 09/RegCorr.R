mtcars
to_correlate <- mtcars %>% dplyr::select(qsec, cyl, disp, hp)
cor(to_correlate)

cor.test(mtcars$qsec, mtcars$cyl)

cor.test(mtcars$qsec, mtcars$cyl, conf.level = 0.9)


ggplot(to_correlate, aes(x=cyl, y=qsec)) + geom_jitter(width=0.1) + stat_smooth(method="lm", se=FALSE)

Hmisc::rcorr(to_correlate %>% as.matrix())
install.packages("stargazer")
library(stargazer)

stargazer(cor(to_correlate), type = "html")
install.packages("apaTables")
library(apaTables)
apaTables::apa.cor.table(to_correlate)

ctest <- cor.test(mtcars$qsec, mtcars$cyl)
str(ctest)

ctest$p.value

ctest$estimate

broom::glance(ctest)

cor.test(mtcars$qsec, mtcars$cyl, method = "spearman")
cor(to_correlate, method = "spearman")

to_correlate_miss <- to_correlate
to_correlate_miss$qsec[c(1, 5)] <- NA
cor(to_correlate_miss) #implicitly use="everything"

cor(to_correlate_miss, use="pairwise.complete.obs")

ggplot(cars, aes(x=speed, y=dist)) + 
  geom_point(color='darkblue', size = 3) + 
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, color='black', size=1.2) +
  labs(x="Speed (mph)", y="Stopping distance (ft)")


lm_cars <- lm(dist ~ speed, data=cars)
summary(lm_cars)
library(autoplot)

plot(lm_cars)
autoplot(lm_cars)

install.packages("Boot")
install.packages("boot",dep=TRUE)
library(boot)
.carEnv <- car:::.carEnv
system.time(lm_cars.boot <- Boot(lm_cars, R=2000))

hist(lm_cars, legend="separate")

summary(lm_cars.boot, high.moments=TRUE)

library(boot)
data(mtcars)

bs <-  function(formula,data,indices){
  d <- data[indices,]
  fit <- lm(formula,data=d)
  return(coef(fit))
}

lm_cars.boot <- boot(data=mtcars,statistic=bs,R=2000,formula=mpg~disp+wt)
summary(lm_cars.boot, high.moments=TRUE)

confint(lm_cars.boot, level=.95, type="bca")

hist(lm_cars.boot, legend="separate")
results

mpg_model <- lm(mpg ~ hp + am, mtcars)
summary(mpg_model)
remove.packages("ggplot2") # Unisntall ggplot
install.packages("ggplot2")
library(ggplot2)

ggplot2(mtcars, aes(x=hp, y=mpg, color=factor(am))) + geom_point() + stat_smooth(method=lm, se=FALSE)
ggplot(mtcars, aes(x=hp, y=mpg, color=factor(am))) + geom_point() + stat_smooth(method=lm, se=FALSE)

broom::glance(mpg_model)

broom::tidy(mpg_model)

int_model <- lm(mpg ~ hp*wt + am, mtcars)
summary(int_model)
install.packages("interactions")
library(interactions)
remove.packages("rlang")
library(Interact)
install.packages("jtools")
library(jtools)
devtools::install_github("jacob-long/jtools")2
#handy 2-way interation plotting function from jtools.
interact_plot(int_model, pred = "hp", modx = "wt")

install.packages("interactions")
library(interactions)
interact_plot(int_model, pred = "hp", modx = "wt")
install.packages("emmeans")
library(emmeans)
library(dplyr)
data(pigs, package="emmeans")
pigs <- pigs %>% mutate(log_conc=log(conc), percent_fac=factor(percent))
pigs.lm <- lm(log_conc ~ source + percent_fac, data = pigs)
summary(pigs.lm)

pigs.emm.s <- emmeans(pigs.lm, "source")
print(pigs.emm.s)

pigs.emm.p <- emmeans(pigs.lm, "percent_fac")
print(pigs.emm.p)

plot(pigs.emm.s, comparisons = TRUE)

print(emmeans(pigs.lm, ~source*percent_fac))
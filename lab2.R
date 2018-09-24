library(dplyr)
library(tidyr)
fev <- read.csv("fev.csv", header=T)

fev %>%
  group_by(id)%>%
  mutate(base_age = min(age),time=age-base_age)%>%
  ggplot(aes(time, logfev1, col=as.factor(id)))+
  geom_line()

fev %>%
  group_by(id)%>%
  mutate(baseage = min(age), baseht = min(ht), time=age-baseage)%>%
  select(-age,-ht)%>%
  filter(baseage<7 | baseht<1.2)

data.wide <- data.frame(name=c("Wilbur", "Petunia", "Gregory"), a=c(67,80,64), b=c(56,90,50))

## need to include which variables you want to make long form, or else it will also separate for  
##all variables

data.wide %>%
  gather(key=drug, value=heart_rate, a:b)

set.seed(10)
data.wide  <- data.frame(
  id = 1:4,
  trt = sample(rep(c('control', 'treatment'), each = 2)),
  work.T1 = runif(4),
  home.T1 = runif(4),
  work.T2 = runif(4),
  home.T2 = runif(4)
)

### These both return the same frames
data.wide %>%
  gather(key=info, value=time, work.T1:home.T2)

data.long <- data.wide %>%
  gather(key=info, value=time, -id, -trt)

data.long %>%
  spread(key = variable, value=c(id, age))


#### Data Visualization ####

data.long <- fev %>%
  gather(key=variable, value=value, ht, age, logfev1)

ggplot(data.long, aes(x=age, y=value)) +
  geom_line(aes(color=id))+
  geom_point(aes(color=id))+
  geom_smooth(method=lm, linetype="dashed",
              color="darkred", fill="blue")



x.norm = seq(-2,2, by=0.05)
y.norm=seq(-2,2, by=0.05)
m <- matrix(dnorm(x.norm) %*%t(dnorm(y.norm)), length(x.norm), length(y.norm))
p5 <- plot_ly(x=x.norm, y=y.norm, z=m) %>% add_surface()


library(plotly)
library(shiny)

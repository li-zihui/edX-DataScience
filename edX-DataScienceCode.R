
##Data Science: Visualization
titanic %>% filter(Age != "NA") %>% 
  group_by(Sex) %>%
  ggplot(aes(x = Age, fill = Sex)) +
    geom_density() +
    facet_grid(.~Sex)


params <- titanic %>%
  filter(!is.na(Age)) %>%
  summarize(mean = mean(Age), sd = sd(Age))

titanic %>% ggplot(aes(sample = Age)) +
  geom_qq(dparams = params) +
  geom_abline()


titanic %>% group_by(Sex) %>%
  ggplot(aes(Survived, fill = Sex)) +
           geom_bar(position = position_dodge())

titanic %>% 
  ggplot(aes(x = Age, y = ..count.., fill = Survived)) +
  geom_density(alpha = 0.2)


titanic %>% filter(Fare != 0) %>% 
  ggplot(aes(x = Survived, y = Fare)) +
    geom_boxplot(alpha = 0.2) +
    scale_y_continuous(trans = "log2") +
    geom_point() +
    geom_jitter()

titanic %>% 
  ggplot(aes(x = Survived, fill = Pclass)) +
  geom_bar(position = position_fill())

titanic %>%
  ggplot(aes(x = Age, y = ..count.., fill = Survived)) +
  geom_density(alpha = 0.2) +
  facet_grid(Sex ~ Pclass)

##Data Science: Visualization
##Comprehensive Assessment 1
library(tidyverse)
library(dslabs)
data(stars)
options(digits = 3)

ggplot(data=stars, aes(x=temp, y=magnitude, label=star)) +
  geom_point() +
  scale_y_reverse() +
  scale_x_continuous(trans="log10") +
  geom_text()

ggplot(data=stars, aes(x=temp, y=magnitude, color=type)) +
  geom_point()

##Data Science: Visualization
##Comprehensive Assessment 2
library(tidyverse)
library(dslabs)
data(temp_carbon)
data(greenhouse_gases)
data(historic_co2)

#temperature changes
temp_carbon %>%
  filter(!is.na(carbon_emissions)) %>%
  .$year %>%
  min()

p <- temp_carbon %>%
  filter(!is.na(temp_anomaly)) %>%
  ggplot(aes(x=year, y=temp_anomaly)) +
  geom_line()

p <- p + geom_hline(aes(yintercept = 0), col = "blue")

p + ylab("Temperature anomaly (degrees C)") +
  ggtitle("Temperature anomaly relative to 20th century mean, 1880-2018") +
  geom_text(aes(x = 2000, y = 0.05, label = "20th century mean"), col = "blue")

p + geom_line(data=temp_carbon, aes(x=year, y=ocean_anomaly), col="red") +
  geom_line(data=temp_carbon, aes(x=year, y=land_anomaly), col="green") +
  scale_x_continuous(limits=c(1880, 2020))

temp_carbon %>%
  ggplot(aes(x=year, y=carbon_emissions)) +
  geom_line()

#greenhouse gas
greenhouse_gases %>%
  ggplot(aes(x=year, y=concentration, color=gas)) +
  geom_line() +
  facet_grid(gas~., scales="free") +
  geom_vline(xintercept = 1850) +
  ylab("Concentration (ch4/n2o ppb, co2 ppm)") +
  ggtitle("Atmospheric greenhouse gas concentration by year, 0-2000")


#historic co2
co2_time <- historic_co2 %>%
  ggplot(aes(x=year, y=co2, col=source)) +
  geom_line()

co2_time + scale_x_continuous(limits=c(-3000, 2018))


##Data Science: Probability

#Olympic running
library(gtools)
library(tidyverse)
permutations(8,3)
combinations(8,3)

runners <- c("Jamaica", "Jamaica", "Jamaica", "USA", "Ecuador", "Netherlands", "France", "South Africa")
set.seed(1)
B <- 10000
results <- replicate(B, {
  winners <- sample(runners, 3)
  (winners[1] == "Jamaica" & winners[2] == "Jamaica" & winners[3] == "Jamaica")
})
mean(results)

#Restaurant management
side <- 2:12
f <- function(options){
  print(3*6*nrow(combinations(options,2)))
}
sapply(side, f)

#Esophageal cancer and alcohol/tobacco use
head(esoph)

high_alcgp <- filter(esoph, alcgp == "120+")
sum(high_alcgp$ncases) / (sum(high_alcgp$ncases) + sum(high_alcgp$ncontrols))

low_alcgp <- filter(esoph, alcgp == "0-39g/day")
sum(low_alcgp$ncases) / (sum(low_alcgp$ncases) + sum(low_alcgp$ncontrols))

esoph %>% filter(ncases !=0) %>%
  summarize(total_case = sum(ncases))
esoph %>% filter(ncases !=0 & (tobgp =="30+" | alcgp == "120+")) %>%
  summarize(case = sum(ncases))

esoph %>% summarize(total_control = sum(ncontrols))
esoph %>% filter(alcgp == "120+" | tobgp == "30+") %>%
  summarize(control = sum(ncontrols))


#ACT scores
set.seed(16, sample.kind = "Rounding")
act_scores <- rnorm(10000, 20.9, 5.7)

x <- seq(1,36,1)
f_x <- dnorm(x, 20.9, 5.7)
plot(x, f_x)

z_score <- (act_scores - 20.9)/5.7
1 - pnorm(2, 0, 1)
qnorm(0.975, 20.9, 5.7)

p <- seq(0.01, 0.99, 0.01)
sample_quantiles <- quantile(act_scores, p)
theoretical_quantiles <- qnorm(p, 20.9, 5.7)
plot(theoretical_quantiles, sample_quantiles)


#SAT testing
set.seed(21, sample.kind = "Rounding")
n_q <- 44
p_correct <- 0.2
p_incorrect <- 0.8
B <- 10000
S <- replicate(B, {
  X <- sample(c(1, -0.25), n_q, replace=TRUE, prob=c(p_correct, p_incorrect))
  sum(X)
})
mean(S>=8)

p <- seq(0.25, 0.95, 0.05)
score <- sapply(p, function(v){
  avg <- n_q*1*v
  se <- sqrt(n_q)*1*sqrt(v*(1-v))
  1 - pnorm(35, avg, se)
})
p[score > 0.8]


#Insurance rates
library(tidyverse)
library(dslabs)

death_prob %>% 
  filter(sex=="Male" & age==50)

n <- 1000
a <- -150000
b <- 1150
p <- seq(.01, .03, .0025)
profit_prob <- sapply(p, function(v){
  avg <- n*(a*v + b*(1-v))
  se <- sqrt(n)*abs(b-a)*sqrt(v*(1-v))
  pnorm(-1000000, avg, se)
})
p[profit_prob>0.9]


set.seed(25, sample.kind = "Rounding")
n <- 1000
p_loss = 0.015
a <- -150000
b <- 1150
profit <- sample(c(a,b), n, prob=c(p_loss,(1-p_loss)), replace=TRUE)
sum(profit)


set.seed(27, sample.kind = "Rounding")
n <- 1000
p_loss = 0.015
a <- -150000
b <- 1150
B <- 10000
S <- replicate(B, {
  X <- sample(c(a,b), n, prob=c(p_loss,(1-p_loss)), replace=TRUE)
  sum(X)
})
mean(S<=-1000000)


z <- qnorm(0.05)
l <- -150000
n <- 1000
p <- 0.015
x <- -l*(n*p-z*sqrt(n*p*(1-p)))/(n*(1-p)+z*sqrt(n*p*(1-p)))
set.seed(28, sample.kind = "Rounding")
B <- 10000
S <- replicate(B, {
  X <- sample(c(l,x), n, prob=c(p,1-p), replace=TRUE)
  sum(X)
})
mean(S<0)

set.seed(29, sample.kind = "Rounding")
B <- 10000
S <- replicate(B, {
  p_rd <- p + sample(seq(-0.01, 0.01, length = 100), 1)
  X <- sample(c(l,x), n, prob=c(p_rd,1-p_rd), replace=TRUE)
  sum(X)
})


##Data Science: Inference and Modeling
#Brexit poll analysis
library(tidyverse)
options(digits = 3)
library(dslabs)
data(brexit_polls)

p <- 0.481    # official proportion voting "Remain"
d <- 2*p-1    # official spread

N <- 1500
brexit_polls <- brexit_polls %>% 
  mutate(x_hat = (spread + 1)/2)

brexit_polls[1,]
x_hat <- 0.52
samplesize <- 4772
se_hat <- sqrt(x_hat*(1-x_hat)/samplesize)
qnorm(0.975, x_hat, se_hat)
qnorm(0.025, x_hat, se_hat)

#Confidence intervals for polls in June
p <- 0.481    # official proportion voting "Remain"
d <- 2*p-1    # official spread

june_polls <- brexit_polls %>%
  filter(enddate >= "2016-06-01") %>%
  mutate(se_x_hat = sqrt(x_hat*(1-x_hat)/samplesize),
         se_spread = 2*se_x_hat,
         upper = spread + qnorm(.975)*se_spread,
         lower = spread - qnorm(.975)*se_spread,
         hit = (d<=upper & d>=lower))

june_polls %>% summarize(mean(hit))

june_polls %>% group_by(pollster) %>%
  summarize(n=n(), hit_rate = mean(hit)) %>%
  arrange(hit_rate)

june_polls %>% group_by(poll_type) %>%
  ggplot(aes(x=poll_type, y=spread)) +
  geom_boxplot()

#Combined spread across poll type
combined_by_type <- june_polls %>%
  group_by(poll_type) %>%
  summarize(N = sum(samplesize),
            spread = sum(spread*samplesize)/N,
            p_hat = (spread + 1)/2) %>%
  mutate(se_spread = 2*sqrt(p_hat*(1-p_hat)/N),
         upper = qnorm(0.975, spread, se_spread),
         lower = qnorm(0.025, spread, se_spread))

#Chi-squared p-value
brexit_hit <- brexit_polls %>%
  mutate(p_hat = (spread + 1)/2,
         se_spread = 2*sqrt(p_hat*(1-p_hat)/samplesize),
         spread_lower = spread - qnorm(.975)*se_spread,
         spread_upper = spread + qnorm(.975)*se_spread,
         hit = spread_lower < -0.038 & spread_upper > -0.038) %>%
  select(poll_type, hit)

online_no <- brexit_hit%>%filter(poll_type=="Online"& hit=="FALSE") %>% nrow() 
online_yes <- brexit_hit%>%filter(poll_type=="Online"& hit=="TRUE") %>% nrow()

phone_no <- brexit_hit%>%filter(poll_type=="Telephone"& hit=="FALSE")%>% nrow() 
phone_yes <- brexit_hit%>%filter(poll_type=="Telephone"& hit=="TRUE")%>% nrow()

two_by_two <- tibble(Hit=c("yes","no"), Online = c(online_yes,online_no), Telephone= c(phone_yes,phone_no))

two_by_two %>% 
  select(-Hit) %>% chisq.test()

odds_online <- (two_by_two$Online[1]/sum(two_by_two$Online)) / (two_by_two$Online[2]/sum(two_by_two$Online))
odds_phone <- (two_by_two$Telephone[1]/sum(two_by_two$Telephone)) / (two_by_two$Telephone[2]/sum(two_by_two$Telephone))
odds_online/odds_phone

#Plotting spread over time
brexit_polls %>% 
  ggplot(aes(x = enddate, y = spread, color = poll_type)) +
  geom_smooth(method = "loess", span=0.4) +
  geom_point() +
  geom_hline(yintercept = -0.038)

#Plotting raw percentages over time
brexit_long <- brexit_polls %>%
  gather(vote, proportion, "remain":"undecided") %>%
  mutate(vote = factor(vote))

brexit_long %>% 
  ggplot(aes(x=enddate, y=proportion, color=vote)) +
  geom_smooth(method = "loess", span = 0.3)

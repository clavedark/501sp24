#basic simulations
#dc spring 2024


library(tidyverse)
library(ggplot2)
library(faux)
library(patchwork)


####
# n simulation ----
# simulate data with different sample sizes and estimate the coefficients, stack output in one coef and one se column
#### 

results <- data.frame ( var = character (0), coef = numeric(0), se = numeric (0), n = numeric (0))

for(i in seq(10,1000,1)) { 
  set.seed(12345)
  data <- tibble(
    X <- rnorm_multi(i, 3, 
                     mu=c(0, 0, 0), 
                     sd=1,
                     r = c(0.0, 0.0, 0.0),
                     varnames=c("x1", "x2", "e"))
  ) %>%
    mutate(y = .5 + 1*x1 + 2*x2 + e)
  
  mod <- (lm(y ~ x1 + x2, data=data))
  
  results[((i-9)*3-2):((i-9)*3),1:4] <- data.frame ( var = c("x0","x1","x2"), coef(summary(mod))[,1:2], i)
  
}

results <-data.frame(results, t=results$coef/results$se)


p1 <- ggplot(results, aes(x=n, y=coef, color=var)) +
  geom_line() +
  labs ( colour = NULL, x = "Sample Size", y =  "Estimated Coefficient" ) +
  theme ( legend.position = "bottom",
          legend.key = element_blank() )
p1

p2 <- ggplot(results, aes(x=n, y=se, color=var)) +
  geom_line() +
  labs ( colour = NULL, x = "Sample Size", y =  "Estimated Standard Error" ) +
  theme ( legend.position = "bottom",
          legend.key = element_blank() )
p2

p1/p2


####
#n simulation, another way ----
#simulate data with different sample sizes and estimate the coefficients, output columns for each coefficient and se
#### 


b_0 <- c()
se_0 <- c()
b_1 <- c()
se_1 <- c()
b_2 <- c()
se_2 <- c()
n <-c()

for(i in seq(10,100,1)) { 
  set.seed(12345)
  dat <- tibble(
    X <- rnorm_multi(i, 3, 
                     mu=c(2, -1, 0), 
                     sd=1,
                     r = c(0.0, 0.0, 0.0),
                     varnames=c("x1", "x2", "e"))
  ) %>%
    mutate(y = .1 + 1*x1 + 2*x2 + e)
  
  mod <- summary(lm(y ~ x1 + x2, data=dat))
  
  n[i-9] <- i
  b_0[i-9] <- coef(mod)[1:1,1:1]
  se_0[i-9] <- coef(mod)[1:1,2:2]
  b_1[i-9] <- coef(mod)[2:2,1:1]
  se_1[i-9] <- coef(mod)[2:2,2:2]
  b_2[i-9] <- coef(mod)[3:3,1:1]
  se_2[i-9] <- coef(mod)[3,2]
  
  
}

df <- data.frame(b_0, se_0, b_1, se_1, b_2,se_2, n)

ggplot() + 
  geom_line(data=df, aes(x=n, y=b_1))+
  geom_line(data=df, aes(x=n, y=b_2))



# n sim function - no explicit loop ----
# simulate data with different sample sizes and estimate the coefficients, output columns for each coefficient and se
# write function, use map to execute
#rm(list=ls())

nsim <- function(i){
  set.seed(12345)
  X <- rnorm_multi(i, 3, 
                   mu=c(0, 0, 0), 
                   sd=1,
                   r = c(0.0, 0.0, 0.0),
                   varnames=c("x1", "x2", "e")) %>%
    mutate(y = .5 + 1*x1 + 2*x2 + e)
  
  mod <- (lm(y ~ x1 + x2, data=X))
  # print(summary(mod))
  coef <- numeric(0)
  coef<-coef(summary(mod))[1:3,1]
  se <- numeric(0)
  se <-coef(summary(mod))[1:3,2]
  n <- numeric(0)
  n <- i
  var = c("x0","x1","x2")
  out <- data.frame(var, coef, se, n)
}

i <- seq(10,1000,1) #vector of i for "nsim" function
dfout <- map(i,nsim) |> list_rbind()

#plot

b <- ggplot(dfout, aes(x=n, y=coef, color=var)) +
  geom_line() +
  labs(x="Sample Size", y="Estimated Coefficient")+
  theme ( legend.position = "bottom",
          legend.key = element_blank() )


se <- ggplot(dfout, aes(x=n, y=se, color=var)) +
  geom_line() +
  labs(x="Sample Size", y="Estimated Standard Error")+
  theme ( legend.position = "bottom",
          legend.key = element_blank() )

b+se

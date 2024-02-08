################
#shiny app to simulate changes cov(x,e), cov(x1,x2)
#dc - spring 2024
#to run from rstudio, shiny::runGitHub('ML22', 'clavedark', subdir = "docs/simulation")
################
library(shiny)
library(ggplot2)
library(mvtnorm)
library(patchwork)
library(faux)


#unit circle inscribed in a square. The area of the circle is pi*r^2, the area of the square is 4*r^2. The ratio of the area of the circle to the area of the square is pi/4. Therefore, if you randomly select points in the square, the probability that the point will fall within the circle is pi/4.



# function to estimate pi
rm(list=ls())
pi_est <- data.frame(replicate(10, {
  x<-runif(1000,-1,1)
  y<-runif(1000,-1,1)
  incircle <- data.frame(ifelse(x^2+y^2 <= 1, 1, 0))
  4*(sum(incircle)/nrow(incircle))
}))


# create 3 random normal variables with a given correlation matrix and replicate 1000 times

piVal <- function(nPoints = 100){
  x <- runif(nPoints,-1,1)
  y <- runif(nPoints,-1,1)
  result <- ifelse(x^2+y^2 <= 1, TRUE, FALSE)
  4*sum(result)/nPoints
}


#############
# n simulation ----
# this produces separate columns for each b and se; more difficult to plot

nsim <- function(i){
  X <- rnorm_multi(i, 3, 
                   mu=c(0, 0, 0), 
                   sd=1,
                   r = c(0.0, 0.0, 0.0),
                   varnames=c("x1", "x2", "e")) %>%
    mutate(y = .5 + 1*x1 + 2*x2 + e)
  
  mod <- (lm(y ~ x1 + x2, data=X))
  b0 <- numeric(0)
  b0<-coef(summary(mod))[1,1]
  b1 <- numeric(0)
  b1 <-coef(summary(mod))[2,1]
  b2 <- numeric(0)
  b2 <-coef(summary(mod))[3,1]
  n <- numeric(0)
  n <- i
  out <- data.frame(b0, b1, b2, n)
}

i <- seq(10,1000,1) #vector of i for "nsim" function
dfout <- map(i,nsim) |> list_rbind() #run function over i and convert list to df

#dfout <- do.call(rbind.data.frame, out) #another wayward  to convert list to df; see https://stackoverflow.com/questions/4227223/convert-a-list-to-a-data-frame

#plot

ggplot(dfout, aes(x=n, y=b1)) +
  geom_line() +
  geom_line(aes(x=n, y=b2), color="red") +
  geom_line(aes(x=n, y=b0), color="blue") +
  labs(x="Sample Size", y="Estimated Coefficient")



#############
# n simulation ----
# this version puts b in one column, 3 obs for each n, and se in another column, 3 obs for each n; easier to plot.
#rm(list=ls())

nsim <- function(i){
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

installed <- as.data.frame(installed.packages()) 
write.csv(installed, "installed_previously.csv")






#############
# seat analysis

f23 <- read.csv("/users/dave/downloads/dc23.csv")
f24 <- read.csv("/users/dave/downloads/mp24.csv") 

f23 <- strsplit(f23$COURSE_ID, "(?<=[A-Za-z])(?=[0-9])|(?<=[0-9])(?=[A-Za-z])", perl = TRUE) 
 



all <- left_join(f23, f24 , by="seat") #merge the two files by seat


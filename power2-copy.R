library(tidyverse)
library(dplyr)
library(magrittr)
library(lme4)
alpha_j <- 3
beta_c <- 0.08
gamma_t <- 0.2

table <- function(n){

sigma_i <- rnorm(n,0,1)
mu_i <- rnorm(n, 0, 0.05)
error <- rnorm(n, 0, 0.2)
fake_data_control <- as.data.frame(matrix(NA, n,10))
for (t in 1:10) {
  y_control <- alpha_j+sigma_i+ error + (beta_c+mu_i)*t
  fake_data_control[,t] <- y_control
}
fake_data_treatment <- as.data.frame(matrix(NA, n,10))

for (t in 1:10) {
  y_treatment <- alpha_j+sigma_i+ error + (beta_c+mu_i+gamma_t)*t
  fake_data_treatment[,t] <- y_treatment
}
fake_data_control$patient_ID <- c(1:n)
colnames(fake_data_treatment)[1:10] <- c(1:10)
fake_data_treatment$patient_ID <- c((n+1):(2*n))
colnames(fake_data_control)[1:10] <- c(1:10)
fake_data_control$group <- "c"
fake_data_treatment$group <- "t"
fake_data <- rbind(fake_data_control, fake_data_treatment)
fake_data <- fake_data %>% pivot_longer(1:10, names_to = "week", values_to = "score")
fake_data_control%>% pivot_longer(1:10, names_to = "week", values_to = "score")
fake_data$week <- as.numeric(fake_data$week)
fake_data$patient_ID <- as.factor(fake_data$patient_ID)
fake_data$group <- as.factor(fake_data$group)
return(fake_data)
}

Power3 <- function(n,si,ns=100){
for (i in 1:ns) {
    a <- table(n)
    fit2 <- lmerTest::lmer(data = a, score ~ (1|patient_ID)+ week + group:week+(week|patient_ID))
    p[i] <- summary(fit2)$coef[4,5]

}}
 

for (i in 1:100) {
  

   a <- table(50)
   p[i] <- summary(lmerTest::lmer(data = a, score ~ (1|patient_ID) + week + group:week+(week|patient_ID)))$coef[3,5]
}
  
p <- summary(lmerTest::lmer(data = table(50), score ~ (1|patient_ID) + week + group:week+(week|patient_ID)))$coef[4,5]
fit3 <- lmerTest::lmer(data = table(50), score ~ (1|patient_ID)+ week + group:week+(week|patient_ID))
summary(fit3)
 a <- table(50)
 for (i in 1:100) {
   
   skip_to_next <- FALSE
   
   tryCatch({s<- lmerTest::lmer(data = table(30), score ~ (1|patient_ID) + week + group:week+(week|patient_ID))
            }, 
   error = function(e) { skip_to_next <<- TRUE})
   
   if(skip_to_next) { next }   
   
   p[i] <- summary(s)$coef[3,5]
 }
 
 
power2 <- function(n, si, ns=100){
  for (i in 1:100) {
    
    skip_to_next <- FALSE
    
    tryCatch({p[i] <- summary(lmerTest::lmer(data = table(n), score ~ (1|patient_ID) + week + group:week+(0+week|patient_ID)))$coef[3,5]
    }, 
    error = function(e) { skip_to_next <<- TRUE})
    
    if(skip_to_next) { next }     
  }
  return(c(mean(p < si), sd(p < si) / sqrt(ns)) )  
  
}

power2 <- function(n, si, ns=100){
  for (i in 1:100) {
    
    skip_to_next <- FALSE
    
    tryCatch({s<- lmerTest::lmer(data = table(30), score ~ (1|patient_ID) + week + group:week+(0+week|patient_ID))
    }, 
    error = function(e) { skip_to_next <<- TRUE})
    
    if(skip_to_next) { next }   
    
    p[i] <- summary(s)$coef[3,5]
  }
  
  return(c(mean(p < si), sd(p < si) / sqrt(ns)) )  
  
}
n <- c(100,200,300,500)
p1 <- sapply(n, power2, .05)

plot(n, p1[1,], pch = 19, log = "x", ylim = c(0, 1),
     xlab = "sample size", ylab = "power")
for (i in seq_along(n))
  lines(c(n[i], n[i]), c(pmax(0, p1[1, i] - 2 * p1[2, i]),
                         pmin(1, p1[1, i] + 2 * p1[2, i])))

Building Model
We structured a multilevel model for this pilot trail study, the variables I use for multilevel are week (week1 - week 10), 
group, patient_id and score. For the random effect is (1|patient_ID) and (0+week|patient_ID).  


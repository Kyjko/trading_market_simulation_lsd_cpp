library(tidyverse)
library(ggplot2)
library(patchwork)
library(e1071)

p <- read.table("C:/LSD/Work/agens_beadando_2/agens_beadando_3/agens_beadando_3/price.csv", 
                sep=",", header=F, check.names=F) %>% as.numeric %>% na.omit
v <- read.table("C:/LSD/Work/agens_beadando_2/agens_beadando_3/agens_beadando_3/volume.csv", 
                sep=",", header=F, check.names=F) %>% as.numeric %>% na.omit
n <- read.table("C:/LSD/Work/agens_beadando_2/agens_beadando_3/agens_beadando_3/number.csv", 
                sep=",", header=F, check.names=F) %>% as.numeric %>% na.omit
r <- read.table("C:/LSD/Work/agens_beadando_2/agens_beadando_3/agens_beadando_3/return.csv", 
                sep=",", header=F, check.names=F) %>% as.numeric %>% na.omit
w <- read.table("C:/LSD/Work/agens_beadando_2/agens_beadando_3/agens_beadando_3/wealth.csv", 
                sep=",", header=F, check.names=F) %>% as.numeric %>% na.omit
s <- read.table("C:/LSD/Work/agens_beadando_2/agens_beadando_3/agens_beadando_3/trader_strat.csv", 
                sep=",", header=F, check.names=F) %>% as.numeric %>% na.omit
f <- read.table("C:/LSD/Work/agens_beadando_2/agens_beadando_3/agens_beadando_3/trader_strat_fund.csv", 
                sep=",", header=F, check.names=F) %>% as.numeric %>% na.omit
fund <- read.table("C:/LSD/Work/agens_beadando_2/agens_beadando_3/agens_beadando_3/fundament.csv", 
                   sep=",", header=F, check.names=F) %>% as.numeric %>% na.omit

t <- 1:2000

df <- data.frame(t, p, v, n, r, s, f, fund)

p1 <- ggplot(df) +
  geom_line(aes(x=t,y=p), alpha=0.8, color="black") +
  labs(
    title = "Piaci ár (P)",
    x = "t",
    y = "P"
  )

p2 <- ggplot(df, aes(x=t,y=v)) +
  geom_bar(alpha=0.8, stat="identity", color="blue") +
  labs(
    title = "Kereskedési volumen (V)",
    x = "t",
    y = "V"
  )

p3 <- ggplot(df, aes(x=t,y=r)) +
  geom_line(alpha=0.8) +
  labs(
    title = "1-időszakos hozamok (R)",
    x = "t",
    y = "R"
  )

p4 <- ggplot(df, aes(x=r,y=r)) +
  geom_histogram(aes(y= ..density..), bins=30, fill="skyblue", alpha=0.7) +
  stat_function(fun=dnorm, args = list(mean=mean(df$r), sd=sd(df$r)), color="red",size=1) +
  annotate("text", x=0.1, y=18, label= "kurt = 7.94", color="blue") +
  labs(
    title = "1-időszakos hozamok eloszlása"
  )

p5 <- ggplot(df) +
  geom_line(aes(x=t,y=s,color="mean(rho_ma)"), alpha=1) +
  geom_line(aes(x=t,y=f,color="mean(rho_f)"), alpha=1) +
  labs(
    title = "Átlagos stratégia-paraméterek",
    x = "t",
    y = "mean(rho_ma), mean(rho_f)"
  )

 p6 <- ggplot(df, aes(x=t,y=p-fund)) +
   geom_line(alpha=0.8) +
   geom_hline(yintercept=0, color="red", size=2) +
   labs(
     title = "Túlértékeltség",
     x = "t",
     y = "P - F"
   ) 


cp <- (p1 | p2 | p6) / (p3 | p4 | p5)

cp

kurt_r <- kurtosis(r)
ref_r <- rnorm(200000, mean(r), sd(r))
kurt_ref <- kurtosis(ref_r)

### correl

par(mfrow = c(1, 2))
pacf(p, main="Ár PACF")
acf(abs(r), main="Abszolút hozam (|r|) ACF")


########################## robustness ###################

# összesen N darab szimuláció, mind eltérő seedekkel
comb_data <- data.frame(1:2000)

for(i in 1:10) {
  tp <- sprintf("C:/LSD/Work/agens_beadando_2/agens_beadando_3/agens_beadando_3/Sim1_%d.csv", i)
  td <- read.table(tp, sep=",", header=F, check.names=F) %>% as.numeric %>% na.omit
  comb_data <- cbind(comb_data, td)
}

colnames(comb_data) <- c("t", "run_1", "run_2", "run_3", "run_4", "run_5", "run_6", "run_7", "run_8", "run_9", "run_10")

# kurtosis

r_plot1 <- ggplot(comb_data, aes(x=run_1,y=run_1)) +
  geom_histogram(aes(y= ..density..), bins=30, fill="skyblue", alpha=0.7) +
  stat_function(fun=dnorm, args = list(mean=mean(comb_data$run_1), sd=sd(comb_data$run_1)), color="red",size=1) +
  annotate("text", x=0.1, y=18, label= kurtosis(comb_data$run_1), color="blue") +
  labs(
    title = "1-időszakos hozamok eloszlása"
  )

r_plot2 <- ggplot(comb_data, aes(x=run_2,y=run_2)) +
  geom_histogram(aes(y= ..density..), bins=30, fill="skyblue", alpha=0.7) +
  stat_function(fun=dnorm, args = list(mean=mean(comb_data$run_2), sd=sd(comb_data$run_2)), color="red",size=1) +
  annotate("text", x=0.1, y=18, label= kurtosis(comb_data$run_2), color="blue") +
  labs(
    title = "1-időszakos hozamok eloszlása"
  )

r_plot3 <- ggplot(comb_data, aes(x=run_3,y=run_3)) +
  geom_histogram(aes(y= ..density..), bins=30, fill="skyblue", alpha=0.7) +
  stat_function(fun=dnorm, args = list(mean=mean(comb_data$run_3), sd=sd(comb_data$run_3)), color="red",size=1) +
  annotate("text", x=0.1, y=18, label= kurtosis(comb_data$run_3), color="blue") +
  labs(
    title = "1-időszakos hozamok eloszlása"
  )

r_plot4 <- ggplot(comb_data, aes(x=run_4,y=run_4)) +
  geom_histogram(aes(y= ..density..), bins=30, fill="skyblue", alpha=0.7) +
  stat_function(fun=dnorm, args = list(mean=mean(comb_data$run_4), sd=sd(comb_data$run_4)), color="red",size=1) +
  annotate("text", x=0.1, y=18, label= kurtosis(comb_data$run_4), color="blue") +
  labs(
    title = "1-időszakos hozamok eloszlása"
  )

r_plot5 <- ggplot(comb_data, aes(x=run_5,y=run_5)) +
  geom_histogram(aes(y= ..density..), bins=30, fill="skyblue", alpha=0.7) +
  stat_function(fun=dnorm, args = list(mean=mean(comb_data$run_5), sd=sd(comb_data$run_5)), color="red",size=1) +
  annotate("text", x=0.1, y=18, label= kurtosis(comb_data$run_5), color="blue") +
  labs(
    title = "1-időszakos hozamok eloszlása"
  )

r_plot6 <- ggplot(comb_data, aes(x=run_6,y=run_6)) +
  geom_histogram(aes(y= ..density..), bins=30, fill="skyblue", alpha=0.7) +
  stat_function(fun=dnorm, args = list(mean=mean(comb_data$run_6), sd=sd(comb_data$run_6)), color="red",size=1) +
  annotate("text", x=0.1, y=18, label= kurtosis(comb_data$run_6), color="blue") +
  labs(
    title = "1-időszakos hozamok eloszlása"
  )

r_plot7 <- ggplot(comb_data, aes(x=run_10,y=run_10)) +
  geom_histogram(aes(y= ..density..), bins=30, fill="skyblue", alpha=0.7) +
  stat_function(fun=dnorm, args = list(mean=mean(comb_data$run_10), sd=sd(comb_data$run_10)), color="red",size=1) +
  annotate("text", x=0.1, y=18, label= kurtosis(comb_data$run_10), color="blue") +
  labs(
    title = "1-időszakos hozamok eloszlása"
  )

r_plot8 <- ggplot(comb_data, aes(x=run_8,y=run_8)) +
  geom_histogram(aes(y= ..density..), bins=30, fill="skyblue", alpha=0.7) +
  stat_function(fun=dnorm, args = list(mean=mean(comb_data$run_8), sd=sd(comb_data$run_8)), color="red",size=1) +
  annotate("text", x=0.1, y=18, label= kurtosis(comb_data$run_8), color="blue") +
  labs(
    title = "1-időszakos hozamok eloszlása"
  )

r_plot9 <- ggplot(comb_data, aes(x=run_9,y=run_9)) +
  geom_histogram(aes(y= ..density..), bins=30, fill="skyblue", alpha=0.7) +
  stat_function(fun=dnorm, args = list(mean=mean(comb_data$run_9), sd=sd(comb_data$run_9)), color="red",size=1) +
  annotate("text", x=0.1, y=18, label= kurtosis(comb_data$run_9), color="blue") +
  labs(
    title = "1-időszakos hozamok eloszlása"
  )

cp2 <- (r_plot1 | r_plot2 | r_plot3) / (r_plot4 | r_plot5 | r_plot6) / (r_plot7 | r_plot8 | r_plot9)
cp2

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
  annotate("text", x=0.1, y=18, label= kurtosis(r), color="blue") +
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
 
 p_price_and_fund <- ggplot(df) +
   geom_line(alpha=0.8, aes(x=t,y=p,color="Ár")) +
   geom_line(alpha=0.8,aes(x=t,y=fund,color="Fundamentum")) +
   labs(
     title = "Ár és fundamentum",
     x = "t",
     y = "P, F"
   )
 
 p_hist_without_kurt <- ggplot(df, aes(x=r,y=r)) +
   geom_histogram(aes(y= ..density..), bins=30, fill="skyblue", alpha=0.7) +
   stat_function(fun=dnorm, args = list(mean=mean(df$r), sd=sd(df$r)), color="red",size=1) +
   labs(
     title = "1-időszakos hozamok eloszlása"
   )
 
 p_special_strat <- ggplot(df) +
   geom_line(aes(x=t,y=s,color="mean(rho_ma)"), alpha=1) +
   geom_line(aes(x=t,y=f,color="mean(rho_f)"), alpha=1) +
   geom_vline(xintercept=400, color="black", size=1) +
   annotate("text", x=450, y=0.4, label= "Válság", color="red") +
   labs(
     title = "Átlagos stratégia-paraméterek",
     x = "t",
     y = "mean(rho_ma), mean(rho_f)"
   )
 
 p_num_without <- ggplot(df, aes(x=t,y=n)) +
   geom_line(alpha=0.8) +
   labs(
     title = "Kereskedők száma",
     x = "t",
     y = "N"
   ) 

 p_num <- ggplot(df, aes(x=t,y=n)) +
   geom_line(alpha=0.8) +
   geom_vline(xintercept=400, color="red", size=1) +
   labs(
     title = "Kereskedők száma",
     x = "t",
     y = "N"
   ) 
 
cp <- (p1 | p2 | p6) / (p3 | p4 | p5)

cp

cp_special <- (p_price_and_fund | p3) / (p_special_strat | p2)
cp_special

cp_without_mut <-  (p_price_and_fund | p2 | p_num_without) / (p5 | p3 | p4)
cp_without_mut


cor(diff(p), diff(s))
cor(diff(p), diff(f))

cor(diff(s), diff(v))
cor(diff(f), diff(v))

sd(s)
sd(f)

cor(p-fund, s)
cor(p-fund, f)

### correl

par(mfrow = c(1, 2))
pacf(r, main="Hozam PACF")
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

r_plot7 <- ggplot(comb_data, aes(x=run_7,y=run_7)) +
  geom_histogram(aes(y= ..density..), bins=30, fill="skyblue", alpha=0.7) +
  stat_function(fun=dnorm, args = list(mean=mean(comb_data$run_7), sd=sd(comb_data$run_7)), color="red",size=1) +
  annotate("text", x=0.1, y=18, label= kurtosis(comb_data$run_7), color="blue") +
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


###########################  sensitivity  #################################################

df <- data.frame()
df_p <- data.frame(1:2000)

for(i in 1:11) {
  p <- read.table(sprintf("C:/LSD/Work/agens_beadando_2/agens_beadando_3/agens_beadando_3/price_%d.csv", i), 
                          sep=",", header=F, check.names=F) %>% as.numeric %>% na.omit
  f <- read.table(sprintf("C:/LSD/Work/agens_beadando_2/agens_beadando_3/agens_beadando_3/fundament_%d.csv", i), 
                  sep=",", header=F, check.names=F) %>% as.numeric %>% na.omit 
  rho_ma <- read.table(sprintf("C:/LSD/Work/agens_beadando_2/agens_beadando_3/agens_beadando_3/trader_strat_%d.csv", i), 
                  sep=",", header=F, check.names=F) %>% as.numeric %>% na.omit 
  rho_f <- read.table(sprintf("C:/LSD/Work/agens_beadando_2/agens_beadando_3/agens_beadando_3/trader_strat_fund_%d.csv", i), 
                  sep=",", header=F, check.names=F) %>% as.numeric %>% na.omit 

  range_strat_diff <- sd(rho_ma-rho_f)
  mean_strat_diff <- mean(rho_ma-rho_f)
  
  sigma_p <- 1-(i-1)*0.1
  
  df <- rbind(df, c(sigma_p, mean_strat_diff, range_strat_diff))
  
  df_p <- cbind(df_p, p)
  df_p <- cbind(df_p, f)
  
}

colnames(df) <- c("sigma_p", "mean_strat_diff", "range_strat_diff")
colnames(df_p) <- c("t", "p1", "f1","p2", "f2","p3", "f3","p4", "f4","p5", "f5",
                  "p6", "f6","p7", "f7","p8", "f8","p9", "f9","p10", "f10", "p11", "f11")

df <- df %>%
  mutate(
    ymin = mean_strat_diff - range_strat_diff,
    ymax = mean_strat_diff + range_strat_diff
  )


ggplot(df, aes(x = sigma_p, y = mean_strat_diff)) +
  geom_point(size = 5, color="red") +                             # means as points
  geom_line(aes(y = mean_strat_diff),color="red", size=3) +                         # line connecting means
  geom_errorbar(aes(ymin = ymin, ymax = ymax),       # ranges (SD, SE, CI)
                width = 0.05, linewidth = 0.8) +
  
  labs(
    x = "sigma_p",
    y = "mean(rho_ma - rho_f)",
    title = "Átlagos stratégiaparaméter-különbségek és szórásuk sigma_p függvényében"
  )

cor(df$sigma_p, df$mean_strat_diff)
cor(df$sigma_p, df$range_strat_diff)

# m_data <- df_p[,-1] %>% as.matrix

long_df_p <- df_p %>%
  pivot_longer(cols = -t,
               names_to = c(".value", "sigma_p"),
               names_pattern = "(.)(\\d+)") %>%
  mutate(sigma_p = factor(sigma_p))

long_df_p %>% mutate(sigma_p_val = 1 - (sigma_p %>% as.numeric -1)/10)

ggplot(long_df_p, aes(x = t)) +
  geom_line(aes(y = p, color = "Ár")) +
  geom_line(aes(y = f, color = "Fundamentum")) +
  facet_wrap(~ sigma_p, scales = "free_y", ncol = 3) +
  scale_color_manual(values = c("blue", "red")) +
  theme_minimal() +
  labs(color = "", y = "P, F", x = "t",
       title = "Ár és fundamentum sigma_p különböző értékei mellett")


# image(df_p[,-1] %>% as.matrix)


#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#cleanrmd::use_cleanrmd("sakura-vader")
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
library(tidyverse)
library(hrbrthemes)
set.seed(12)
df <- rbinom(3000, 12, 0.5)

df %>% 
  data.frame() %>% 
  ggplot(aes(.)) + 
  geom_histogram(aes(., stat(density)), binwidth = 1, color="white") +
  stat_function(fun=dnorm, color="black", args=list(mean=mean(df), sd=sd(df))) +
  scale_y_continuous(limits=c(0, 0.25), breaks = seq(0, 0.25, 0.05)) +
  labs(title="Poor Man's Galton Board") +
  theme_ipsum_rc()

#
#
#
#
#
#
#
#
#
library(patchwork)
set.seed(5)
df9 <- rbinom(3000, 12, 0.1)

p9 <- df9 %>% 
  data.frame() %>% 
  ggplot(aes(.)) + 
  geom_histogram(aes(., stat(density)), binwidth = 1, color="white") +
  stat_function(fun=dnorm, color="black", args=list(mean=mean(df9), sd=sd(df9))) +
  scale_y_continuous(limits=c(0, 0.40), breaks = seq(0, 0.40, 0.05)) +
  theme_ipsum_rc() +
  labs(title="Tilt left",y=" ", x=" ") 

set.seed(6)
df10 <- rbinom(3000, 12, 0.9)

p10 <- df10 %>% 
  data.frame() %>% 
  ggplot(aes(.)) + 
  geom_histogram(aes(., stat(density)), binwidth = 1, color="white") +
  stat_function(fun=dnorm, color="black", args=list(mean=mean(df10), sd=sd(df10))) +
  scale_y_continuous(limits=c(0, 0.40), breaks = seq(0, 0.40, 0.05)) +
  theme_ipsum_rc() +
  theme(axis.text.y = element_blank()) +
  labs(title="Tilt right",y=" ", x=" ") 

p9 + p10
#
#
#
#
#
#
#
set.seed(1)
df1 <- rbinom(3000, 12, 0.5)

p1 <- df1 %>% 
  data.frame() %>% 
  ggplot(aes(.)) + 
  geom_histogram(aes(., stat(density)), binwidth = 1, color="white") +
  stat_function(fun=dnorm, color="black", args=list(mean=mean(df1), sd=sd(df1))) +
  scale_y_continuous(limits=c(0, 0.30), breaks = seq(0, 0.30, 0.05)) +
  theme_ipsum_rc() +
  labs(title="n=3000",y=" ", x=" ") 

set.seed(2)
df2 <- rbinom(1000, 12, 0.5)

p2 <- df2 %>% 
  data.frame() %>% 
  ggplot(aes(.)) + 
  geom_histogram(aes(., stat(density)), binwidth = 1, color="white") +
  stat_function(fun=dnorm, color="black", args=list(mean=mean(df2), sd=sd(df2))) +
#  scale_x_continuous(limits=c(120, 220), breaks=seq(120, 220, 10)) +
  scale_y_continuous(limits=c(0, 0.30), breaks = seq(0, 0.30, 0.05)) +
  theme_ipsum_rc() +
#  theme(axis.text.y = element_blank()) +
  labs(title="n=1000",y=" ", x=" ") 



set.seed(3)
df3 <- rbinom(500, 12, 0.5)

p3 <- df3 %>% 
  data.frame() %>% 
  ggplot(aes(.)) + 
  geom_histogram(aes(., stat(density)), binwidth = 1, color="white") +
  stat_function(fun=dnorm, color="black", args=list(mean=mean(df3), sd=sd(df3))) +
  #  scale_x_continuous(limits=c(120, 220), breaks=seq(120, 220, 10)) +
  scale_y_continuous(limits=c(0, 0.30), breaks = seq(0, 0.30, 0.05)) +
  theme_ipsum_rc() +
  #  theme(axis.text.y = element_blank()) +
  labs(title="n=500",y=" ", x=" ") 


set.seed(4)
df4 <- rbinom(100, 12, 0.5)

p4 <- df4 %>% 
  data.frame() %>% 
  ggplot(aes(.)) + 
  geom_histogram(aes(., stat(density)), binwidth = 1, color="white") +
  stat_function(fun=dnorm, color="black", args=list(mean=mean(df4), sd=sd(df4))) +
  #  scale_x_continuous(limits=c(120, 220), breaks=seq(120, 220, 10)) +
  scale_y_continuous(limits=c(0, 0.30), breaks = seq(0, 0.30, 0.05)) +
  theme_ipsum_rc() +
  #  theme(axis.text.y = element_blank()) +
  labs(title="n=100",y=" ", x=" ") 


(p1 | p2) / (p3 | p4)

#
#
#
#
#
#
#
set.seed(7)
df5 <- rbinom(3000, 12, 0.5)

p5 <- df5 %>% 
  data.frame() %>% 
  ggplot(aes(.)) + 
  geom_histogram(aes(., stat(density)), binwidth = 1, color="white") +
  stat_function(fun=dnorm, color="black", args=list(mean=mean(df5), sd=sd(df5))) +
  #  scale_x_continuous(limits=c(120, 220), breaks=seq(120, 220, 10)) +
  scale_y_continuous(limits=c(0, 0.30), breaks = seq(0, 0.30, 0.05)) +
  theme_ipsum_rc() +
  #  theme(axis.text.y = element_blank()) +
  labs(title="pegs=12",y=" ", x=" ") 


set.seed(8)
df6 <- rbinom(3000, 50, 0.5)

p6 <- df6 %>% 
  data.frame() %>% 
  ggplot(aes(.)) + 
  geom_histogram(aes(., stat(density)), binwidth = 1, color="white") +
  stat_function(fun=dnorm, color="black", args=list(mean=mean(df6), sd=sd(df6))) +
  #  scale_x_continuous(limits=c(120, 220), breaks=seq(120, 220, 10)) +
  scale_y_continuous(limits=c(0, 0.30), breaks = seq(0, 0.30, 0.05)) +
  theme_ipsum_rc() +
  #  theme(axis.text.y = element_blank()) +
  labs(title="pegs=50",y=" ", x=" ") 


set.seed(9)
df7 <- rbinom(3000, 100, 0.5)

p7 <- df7 %>% 
  data.frame() %>% 
  ggplot(aes(.)) + 
  geom_histogram(aes(., stat(density)), binwidth = 1, color="white") +
  stat_function(fun=dnorm, color="black", args=list(mean=mean(df7), sd=sd(df7))) +
  #  scale_x_continuous(limits=c(120, 220), breaks=seq(120, 220, 10)) +
  scale_y_continuous(limits=c(0, 0.30), breaks = seq(0, 0.30, 0.05)) +
  theme_ipsum_rc() +
  #  theme(axis.text.y = element_blank()) +
  labs(title="pegs=100",y=" ", x=" ") 

set.seed(10)
df8 <- rbinom(3000, 250, 0.5)

p8 <- df8 %>% 
  data.frame() %>% 
  ggplot(aes(.)) + 
  geom_histogram(aes(., stat(density)), binwidth = 1, color="white") +
  stat_function(fun=dnorm, color="black", args=list(mean=mean(df8), sd=sd(df8))) +
  #  scale_x_continuous(limits=c(120, 220), breaks=seq(120, 220, 10)) +
  scale_y_continuous(limits=c(0, 0.30), breaks = seq(0, 0.30, 0.05)) +
  theme_ipsum_rc() +
  #  theme(axis.text.y = element_blank()) +
  labs(title="pegs=250",y=" ", x=" ") 


(p5 + p6)/(p7 + p8)
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
library(twitterwidget)
twitterwidget('1296417067814522881')
#
#
#
#

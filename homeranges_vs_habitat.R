########################################################
library(ggplot2)
library(tidyverse)

#########################################################

# Home ranges vs habitat

males <- readRDS("akde_final_df_males.rds")
females <- readRDS("akde_final_df_females.rds")
quantile(males$rsf_sum, probs= seq(0,1,0.05))
quantile(females$rsf_sum, probs= seq(0,1,0.05))


males$sex <- "m"
females$sex <- "f"

full <- rbind(males, females)

full$km_area <- full$areas / 1000000
full$log_area <- log(full$km_area)
outliers <- boxplot(full$area, plot=FALSE)$out
boxplot(full$area)
x<-full
x<- x[-which(x$area %in% outliers),]

x$km_area <- x$areas / 1000000
nrow(x)


#-----------------------------------------------------

x$log_area <- log(x$km_area)

# change this so theres only one linear line 
x %>% ggplot(aes(adjusted_value, log_area)) +
  geom_point(aes()) +
  geom_smooth(method = lm) +
  labs(x='Habitat quality index', y='Home range size (log(km^2))',
       title='Habitat quality vs home range size') +
  theme_bw()+
  theme(plot.title = element_text(hjust=0.5)) +
  geom_text(x=0.0012, y=2.9, label="p = 2.81e-15")+
  geom_text(x=0.00119, y=2.6, label="r2 = 0.1816")

modx <- lm(km_area~adjusted_value, data=x)
modx <- lm(log_area~adjusted_value, data=x)

summary(modx)
full %>% ggplot(aes(adjusted_value, log_area)) +
  geom_point(aes()) +
  geom_smooth(method = lm) +
  labs(x='Habitat quality index', y='Home range size (log(km^2))',
       title='Habitat quality vs home range size') +
  theme_bw()+
  theme(plot.title = element_text(hjust=0.5)) +
  geom_text(x=0.0012, y=2.9, label="p = 2e-16")+
  geom_text(x=0.00121, y=2.6, label="r2 = 0.2642")

modx <- lm(log_area~adjusted_value, data=full)

summary(modx)
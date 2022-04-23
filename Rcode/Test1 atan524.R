
library(tidyverse)
library(nortest)
library(car)
library(RColorBrewer)
library(biotools)
library(PMCMRplus)

# Q1 ----
df <- read.csv("test1-2022.csv", header = T) %>% 
  mutate(female = factor(female)
         , prog = factor(prog))
dim(df)
str(df)
head(df)

boxplot(read~female+prog, data = df)

# Normality Assumption
outlier1 <- boxplot(read~female+prog, data = df)$out
outlier1

out.df <- df[df$female == 1 & df$prog == 3 & df$read == 68,]
out.df

df.Noout <- df[-which(df$id %in% out.df$id), ]
nrow(df) - nrow(df.Noout)

shapiro.test(df.Noout$read)

# Homogeneity assumption
leveneTest(read ~ female * prog, data = df.Noout)

# Two-way ANOVA
fit.read <- aov(read ~ female * prog, data = df.Noout)
summary(fit.read)

# Tamhane test
summary(T2 <- tamhaneT2Test(df.Noout$read, df.Noout$female : df.Noout$prog))

# Q2 ----

al_df <- read.csv("alumnigiving.csv", header = T) %>% 
  mutate(State = as.factor(State))
dim(al_df)
head(al_df)
str(al_df)

reg.fit <- lm(Alumni.Giving.Rate ~ Graduation.Rate, data = al_df)
summary(reg.fit)


reg2.fit <- lm(Alumni.Giving.Rate ~ Graduation.Rate + Percentage.of.Classes.U20 + Student.Faculty.Ratio, data = al_df)
summary(reg2.fit)

reg3.fit <-lm(Alumni.Giving.Rate ~ Graduation.Rate + Student.Faculty.Ratio, data = al_df)
summary(reg3.fit)

imcdiag(reg3.fit)














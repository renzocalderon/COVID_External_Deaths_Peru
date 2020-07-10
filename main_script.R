# This is code to replicate the analyses and figures from the paper:
# "Impact of COVID-19 Lockdown Policy on Homicide, Suicide, and Motor Vehicle Deaths in Peru"
# Code developed by Renzo JC Calderon-Anyosa, Supervised by Jay S Kaufman
# Last Revised: July 10, 2020 / version 4.0

# Required packages

library(tidyverse)
library(nlme)
library(ggpubr)

# Main Data #### 
# from "Datos Abiertos Peru" web (this takes a while)
# [170 seconds by July 10, 2020 with 25 Mbps download speed and 6 Mbps upload speed])

db <- read.csv(url("https://www.datosabiertos.gob.pe/node/6450/download"),
              strip.white = TRUE,
              sep = ';', 
              header = FALSE,
              stringsAsFactors = FALSE)


# Data starts in column 4, with names in column 3
# Renaming and arrenging columns 

colnames(db) <- tolower(gsub("[[:space:]]", ".", as.vector(t(db)[,3]), perl=T))
db <- db[-c(1:3), ]

# Droping last 4 empty columns 
db <- db[-c(32:35)]


# Geneate age ####
# Missings
db$edad[db$edad=="SIN REGISTRO"] <- NA
# age from chr to numb
db$edad <- as.numeric(db$edad)
# Common age in days
db$aget <- ifelse(db$tiempo.edad=="DIAS",1,NA)
db$aget <- ifelse(db$tiempo.edad=="MESES",30.5,db$aget)
db$aget <- ifelse(db$tiempo.edad=="AÑOS",365.5,db$aget)
db$aget <- ifelse(db$tiempo.edad=="HORAS",1/24,db$aget)
db$aget <- ifelse(db$tiempo.edad=="MINUTOS",1/(24*60),db$aget)
db$aget <- ifelse(db$tiempo.edad=="SEGUNDOS",1/(24*60*60),db$aget)
db$aget <- ifelse(db$tiempo.edad=="SIN REGISTRO",NA,db$aget)
# gen age
db$age <- db$edad*db$aget

# Generate 15 days bins (bi.week) from lockdown date (March 16 2020) ####
db$date <-as.Date(db$fecha)

db$bi.week <- NA

for (i in 1:100) {
    db$bi.week <- ifelse(db$date>=as.Date("2020-03-16")-(15*i) & db$date<as.Date("2020-03-16")-(15*(i-1)), i*-1 ,db$bi.week)
}

for (i in 1:30) {
  db$bi.week <- ifelse(db$date>=as.Date("2020-03-16")+(15*(i-1)) & db$date<as.Date("2020-03-16")+(15*i), i*1 ,db$bi.week)
}

# Start from 0
db$bi.week <- ifelse(db$bi.week>= 0, db$bi.week-1, db$bi.week )

# Keep until June 28
db <- db %>% filter(bi.week<=6) 


# Adult DB ####
db$adult <- ifelse(db$age>=365.5*18,1,0)

db.adult <- db %>% filter(adult==1)

# Defining types of deaths ####
# base:Adult DB Recoding Female homicide
db.adult$female.homicide <- ifelse(db.adult$muerte.violenta=="HOMICIDIO" & db.adult$sexo=="FEMENINO",1,NA)

# base:Adult DB Recoding Male homicide
db.adult$male.homicide <- ifelse(db.adult$muerte.violenta=="HOMICIDIO" & db.adult$sexo=="MASCULINO",1,NA)

# base:Adult DB Recoding Female suicide
db.adult$female.suicide <- ifelse(db.adult$muerte.violenta=="SUICIDIO" & db.adult$sexo=="FEMENINO",1,NA)

# base:Adult DB Recoding Male suicide
db.adult$male.suicide <- ifelse(db.adult$muerte.violenta=="SUICIDIO" & db.adult$sexo=="MASCULINO",1,NA)

# base:Adult DB Recoding Female trafic accident
db.adult$female.ta<- ifelse(db.adult$muerte.violenta=="ACCIDENTE DE TRANSITO" & db.adult$sexo=="FEMENINO",1,NA)

# base:Adult DB Recoding Male trafic accident
db.adult$male.ta <- ifelse(db.adult$muerte.violenta=="ACCIDENTE DE TRANSITO" & db.adult$sexo=="MASCULINO",1,NA)

# base:Adult DB Recoding Female other accident
db.adult$female.oa <- ifelse(db.adult$muerte.violenta=="ACCIDENTE DE TRABAJO" | db.adult$muerte.violenta=="OTRO ACCIDENTE"  & db.adult$sexo=="FEMENINO",1,NA)

# base:Adult DB Recoding Male other accident
db.adult$male.oa <- ifelse(db.adult$muerte.violenta=="ACCIDENTE DE TRABAJO" | db.adult$muerte.violenta=="OTRO ACCIDENTE" & db.adult$sexo=="MASCULINO",1,NA)

# Sum number of deaths by 15 days bin ####
# Sum Female homicide by biweek
db.female.homicide <- db.adult %>% group_by(bi.week) %>%  summarise(su.fh = sum(female.homicide, na.rm = T))

# Sum Male homicide by biweek
db.male.homicide <- db.adult %>% group_by(bi.week) %>%  summarise(su.mh = sum(male.homicide, na.rm = T))

# Sum Female suiicide by biweek
db.female.suicide <- db.adult %>% group_by(bi.week) %>%  summarise(su.fs = sum(female.suicide, na.rm = T))

# Sum Male suicide by biweek
db.male.suicide <- db.adult %>% group_by(bi.week) %>%  summarise(su.ms = sum(male.suicide, na.rm = T))

# Sum Female trafic accident by biweek
db.female.ta <- db.adult %>% group_by(bi.week) %>%  summarise(su.fta = sum(female.ta, na.rm = T))

# Sum Male trafic accident by biweek
db.male.ta <- db.adult %>% group_by(bi.week) %>%  summarise(su.mta = sum(male.ta, na.rm = T))

# Sum Female other accident by biweek
db.female.oa <- db.adult %>% group_by(bi.week) %>%  summarise(su.foa = sum(female.oa, na.rm = T))

# Sum Male other accident by biweek
db.male.oa <- db.adult %>% group_by(bi.week) %>%  summarise(su.moa = sum(male.oa, na.rm = T))



# ITS Analysis ####

# ITSA female homicide

# Define postlockdown period
db.female.homicide$postlock <- ifelse(db.female.homicide$bi.week >= 0,1,0)
# Gen death by million
db.female.homicide$per1m <- db.female.homicide$su.fh/10397401*1000000

# Linear model
glsfit.fh <- gls(per1m~bi.week + postlock + bi.week:postlock,
                 data = db.female.homicide)

# Check for autocorr
acf(resid(glsfit.fh))

# Model estimates
coef(glsfit.fh)
confint(glsfit.fh)

# 1 year slope 95% CI
round(coef(glsfit.fh)[2]*24,2)
round(confint(glsfit.fh)[2]*24,2)
round(confint(glsfit.fh)[2,2]*24,2)

# Post-Lockdown difference per month 95% CI
round(coef(glsfit.fh)[3]*2,2)
round(confint(glsfit.fh)[3]*2,2)
round(confint(glsfit.fh)[3,2]*2,2)

# Post-Lockdown x 1 year Interaction 95% CI
round(coef(glsfit.fh)[4]*24,2)
round(confint(glsfit.fh)[4]*24,2)
round(confint(glsfit.fh)[4,2]*24,2)

# Predict data for graph
data.pred.fh <- db.female.homicide
data.pred.fh$per1m <- predict(glsfit.fh, newdata = data.pred.fh)

# Graph for Fig 1
fhg <- ggplot(data = db.female.homicide, mapping = aes(x = bi.week, y = per1m)) +
  geom_rect(aes( ymin=-Inf, ymax=Inf, xmin=0, xmax=8, fill="#E9E9E9"), col = "#E9E9E9", alpha=0.25) +
  geom_point() + 
  scale_fill_identity(name = "", guide = "none") +
  scale_x_continuous(name="", breaks=c(-78,-54,-29,-5,4), labels=c("Jan 2017","Jan 2018","Jan 2019","Jan 2020","\nLockdown"),
                     limits = c(-78.00005,8)) +
  scale_y_continuous(name="Deaths / 1,000,000 Women", limits = c(0,5))+
  geom_vline(xintercept = 0, linetype="longdash") +
  theme(
    plot.background = element_rect(fill = "white"),
    axis.line.x = element_line(color = "black"),
    axis.line.y = element_line(color = "black"),
    panel.grid.major = element_line(color = "grey"),
    panel.ontop = T,
    panel.background = element_rect(fill = NA),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.title = element_text(hjust = 0.5)) +
  ggtitle("Homicides") +
  geom_vline(xintercept=c(-78,-54,-29,-5), colour="grey") +
  stat_smooth(aes(group = postlock), method = "loess", formula = y~x, span = 0.6, color = "#287AB2", fill = "#287AB2", alpha = 0.3 ) +
  geom_line(data = subset(data.pred.fh, bi.week < 0), colour = "#E65757", size = 1.5, alpha = 0.7) + 
  geom_line(data = subset(data.pred.fh, bi.week > 0), colour = "#E65757", size = 1.5, alpha = 0.7) +
  theme(text=element_text(size=12,  family="sans")) 


# ITSA male homicide

# Define postlockdown period
db.male.homicide$postlock <- ifelse(db.male.homicide$bi.week >= 0,1,0)

# Gen death by million
db.male.homicide$per1m <- db.male.homicide$su.mh/9780154*1000000

# Linear model
glsfit.mh <- gls(per1m~bi.week + postlock + bi.week:postlock,
                 data = db.male.homicide)

# Check for autocorr
acf(resid(glsfit.mh))

# Model estimates
coef(glsfit.mh)
confint(glsfit.mh)

# 1 year slope 95% CI
round(coef(glsfit.mh)[2]*24,2)
round(confint(glsfit.mh)[2]*24,2)
round(confint(glsfit.mh)[2,2]*24,2)

# Post-Lockdown difference per month 95% CI
round(coef(glsfit.mh)[3]*2,2)
round(confint(glsfit.mh)[3]*2,2)
round(confint(glsfit.mh)[3,2]*2,2)

# Post-Lockdown x 1 year Interaction 95% CI
round(coef(glsfit.mh)[4]*24,2)
round(confint(glsfit.mh)[4]*24,2)
round(confint(glsfit.mh)[4,2]*24,2)

# Predict data for graph
data.pred.mh <- db.male.homicide
data.pred.mh$per1m <- predict(glsfit.mh, newdata = data.pred.mh)

# Graph for Fig 2
mhg <- ggplot(data = db.male.homicide, mapping = aes(x = bi.week, y = per1m)) +
  geom_rect(aes( ymin=-Inf, ymax=Inf, xmin=0, xmax=8, fill="#E9E9E9"), col = "#E9E9E9", alpha=0.25) +
  geom_point() + 
  scale_fill_identity(name = "", guide = "none") +
  scale_x_continuous(name="", breaks=c(-78,-54,-29,-5,4), labels=c("Jan 2017","Jan 2018","Jan 2019","Jan 2020","\nLockdown")) +
  scale_y_continuous(name="Deaths / 1,000,000 Men", limits = c(0,10))+
  geom_vline(xintercept = 0, linetype="longdash") +
  theme(
    plot.background = element_rect(fill = "white"),
    axis.line.x = element_line(color = "black"),
    axis.line.y = element_line(color = "black"),
    panel.grid.major = element_line(color = "grey"),
    panel.ontop = T,
    panel.background = element_rect(fill = NA),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.title = element_text(hjust = 0.5)) +
  ggtitle("Homicides") +
  geom_vline(xintercept=c(-78,-54,-29,-5), colour="grey") +
  stat_smooth(aes(group = postlock), method = "loess", formula = y~x, span = 0.5, color = "#287AB2", fill = "#287AB2", alpha = 0.3 ) +
  geom_line(data = subset(data.pred.mh, bi.week < 0), colour = "#E65757", size = 1.5, alpha = 0.7) + 
  geom_line(data = subset(data.pred.mh, bi.week > 0), colour = "#E65757", size = 1.5, alpha = 0.7) +
  theme(text=element_text(size=12,  family="sans"))

##ITSA female suicide

# Define postlockdown period
db.female.suicide$postlock <- ifelse(db.female.suicide$bi.week >= 0,1,0)
# Gen death by million
db.female.suicide$per1m <- db.female.suicide$su.fs/10397401*1000000

# Linear model
glsfit.fs <- gls(per1m~bi.week + postlock + bi.week:postlock,
                 data = db.female.suicide)

# Check for autocorr
acf(resid(glsfit.fs))

# Model estimates
coef(glsfit.fs)
confint(glsfit.fs)

# 1 year slope 95% CI
round(coef(glsfit.fs)[2]*24,2)
round(confint(glsfit.fs)[2]*24,2)
round(confint(glsfit.fs)[2,2]*24,2)

# Post-Lockdown difference per month 95% CI
round(coef(glsfit.fs)[3]*2,2)
round(confint(glsfit.fs)[3]*2,2)
round(confint(glsfit.fs)[3,2]*2,2)

# Post-Lockdown x 1 year Interaction 95% CI
round(coef(glsfit.fs)[4]*24,2)
round(confint(glsfit.fs)[4]*24,2)
round(confint(glsfit.fs)[4,2]*24,2)


# Predict data for graph
data.pred.fs <- db.female.suicide
data.pred.fs$per1m <- predict(glsfit.fs, newdata = data.pred.fs)

# Graph for Fig 1
fsg <- ggplot(data = db.female.suicide, mapping = aes(x = bi.week, y = per1m)) +
  geom_rect(aes( ymin=-Inf, ymax=Inf, xmin=0, xmax=8, fill="#E9E9E9"), col = "#E9E9E9", alpha=0.25) +
  geom_point() + 
  scale_fill_identity(name = "", guide = "none") +
  scale_x_continuous(name="", breaks=c(-78,-54,-29,-5,4), labels=c("Jan 2017","Jan 2018","Jan 2019","Jan 2020","\nLockdown")) +
  scale_y_continuous(name="Deaths / 1,000,000 Women", limits = c(0,5))+
  geom_vline(xintercept = 0, linetype="longdash") +
  theme(
    plot.background = element_rect(fill = "white"),
    axis.line.x = element_line(color = "black"),
    axis.line.y = element_line(color = "black"),
    panel.grid.major = element_line(color = "grey"),
    panel.ontop = T,
    panel.background = element_rect(fill = NA),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.title = element_text(hjust = 0.5)) +
  ggtitle("Suicides") +
  geom_vline(xintercept=c(-78,-54,-29,-5), colour="grey") +
  stat_smooth(aes(group = postlock), method = "loess", formula = y~x, span = 0.6, color = "#287AB2", fill = "#287AB2", alpha = 0.3 ) +
  geom_line(data = subset(data.pred.fs, bi.week < 0), colour = "#E65757", size = 1.5, alpha = 0.7) + 
  geom_line(data = subset(data.pred.fs, bi.week > 0), colour = "#E65757", size = 1.5, alpha = 0.7) +
  theme(text=element_text(size=12,  family="sans"))

##ITSA male suicide

# Define postlockdown period
db.male.suicide$postlock <- ifelse(db.male.suicide$bi.week >= 0,1,0)
# Gen death by million
db.male.suicide$per1m <- db.male.suicide$su.ms/9780154*1000000

# Linear model
glsfit.ms <- gls(per1m~bi.week + postlock + bi.week:postlock,
                 data = db.male.suicide)

# Check for autocorr
acf(resid(glsfit.ms))

# Model estimates
coef(glsfit.ms)
confint(glsfit.ms)

# 1 year slope 95% CI
round(coef(glsfit.ms)[2]*24,2)
round(confint(glsfit.ms)[2]*24,2)
round(confint(glsfit.ms)[2,2]*24,2)

# Post-Lockdown difference per month 95% CI
round(coef(glsfit.ms)[3]*2,2)
round(confint(glsfit.ms)[3]*2,2)
round(confint(glsfit.ms)[3,2]*2,2)

# Post-Lockdown x 1 year Interaction 95% CI
round(coef(glsfit.ms)[4]*24,2)
round(confint(glsfit.ms)[4]*24,2)
round(confint(glsfit.ms)[4,2]*24,2)

# Predict data for graph
data.pred.ms <- db.male.suicide
data.pred.ms$per1m <- predict(glsfit.ms, newdata = data.pred.ms)

# Graph for Fig 2
msg <- ggplot(data = db.male.suicide, mapping = aes(x = bi.week, y = per1m)) +
  geom_rect(aes( ymin=-Inf, ymax=Inf, xmin=0, xmax=8, fill="#E9E9E9"), col = "#E9E9E9", alpha=0.25) +
  geom_point() + 
  scale_fill_identity(name = "", guide = "none") +
  scale_x_continuous(name="", breaks=c(-78,-54,-29,-5,4), labels=c("Jan 2017","Jan 2018","Jan 2019","Jan 2020","\nLockdown")) +
  scale_y_continuous(name="Deaths / 1,000,000 Men", limits = c(0,10))+
  geom_vline(xintercept = 0, linetype="longdash") +
  theme(
    plot.background = element_rect(fill = "white"),
    axis.line.x = element_line(color = "black"),
    axis.line.y = element_line(color = "black"),
    panel.grid.major = element_line(color = "grey"),
    panel.ontop = T,
    panel.background = element_rect(fill = NA),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.title = element_text(hjust = 0.5)) +
  ggtitle("Suicides") +
  geom_vline(xintercept=c(-78,-54,-29,-5), colour="grey") +
  stat_smooth(aes(group = postlock), method = "loess", formula = y~x, span = 0.6, color = "#287AB2", fill = "#287AB2", alpha = 0.3 ) +
  geom_line(data = subset(data.pred.ms, bi.week < 0), colour = "#E65757", size = 1.5, alpha = 0.7) + 
  geom_line(data = subset(data.pred.ms, bi.week >= 0), colour = "#E65757", size = 1.5, alpha = 0.7) +
  theme(text=element_text(size=12,  family="sans"))


##ITSA female TA

# Define postlockdown period
db.female.ta$postlock <- ifelse(db.female.ta$bi.week >= 0,1,0)
# Gen death by million
db.female.ta$per1m <- db.female.ta$su.fta/10397401*1000000

# Linear model
glsfit.fta <- gls(per1m~bi.week + postlock + bi.week:postlock,
                  data = db.female.ta)

# Check for autocorr
acf(resid(glsfit.fta))

# Model estimates
coef(glsfit.fta)
confint(glsfit.fta)

# 1 year slope 95% CI
round(coef(glsfit.fta)[2]*24,2)
round(confint(glsfit.fta)[2]*24,2)
round(confint(glsfit.fta)[2,2]*24,2)

# Post-Lockdown difference per month 95% CI
round(coef(glsfit.fta)[3]*2,2)
round(confint(glsfit.fta)[3]*2,2)
round(confint(glsfit.fta)[3,2]*2,2)

# Post-Lockdown x 1 year Interaction 95% CI
round(coef(glsfit.fta)[4]*24,2)
round(confint(glsfit.fta)[4]*24,2)
round(confint(glsfit.fta)[4,2]*24,2)

# Predict data for graph
data.pred.fta <- db.female.ta
data.pred.fta$per1m <- predict(glsfit.fta, newdata = data.pred.fta)

# Graph for Fig 1
ftag <- ggplot(data = db.female.ta, mapping = aes(x = bi.week, y = per1m)) +
  geom_rect(aes( ymin=-Inf, ymax=Inf, xmin=0, xmax=8, fill="#E9E9E9"), col = "#E9E9E9", alpha=0.25) +
  geom_point() + 
  scale_fill_identity(name = "", guide = "none") +
  scale_x_continuous(name="Every 15 days", breaks=c(-78,-54,-29,-5,4), labels=c("Jan 2017","Jan 2018","Jan 2019","Jan 2020","\nLockdown")) +
  scale_y_continuous(name="Deaths / 1,000,000 Women", limits = c(0,5))+
  geom_vline(xintercept = 0, linetype="longdash") +
  theme(
    plot.background = element_rect(fill = "white"),
    axis.line.x = element_line(color = "black"),
    axis.line.y = element_line(color = "black"),
    panel.grid.major = element_line(color = "grey"),
    panel.ontop = T,
    panel.background = element_rect(fill = NA),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.title = element_text(hjust = 0.5)) +
  ggtitle("Traffic Accidents") +
  geom_vline(xintercept=c(-78,-54,-29,-5), colour="grey") +
  stat_smooth(aes(group = postlock), method = "loess", formula = y~x, span = 0.6, color = "#287AB2", fill = "#287AB2", alpha = 0.3 ) +
  geom_line(data = subset(data.pred.fta, bi.week < 0), colour = "#E65757", size = 1.5, alpha = 0.7) + 
  geom_line(data = subset(data.pred.fta, bi.week >= 0), colour = "#E65757", size = 1.5, alpha = 0.7) +
  theme(text=element_text(size=12,  family="sans"))

##ITSA male ta

# Define postlockdown period
db.male.ta$postlock <- ifelse(db.male.ta$bi.week >= 0,1,0)
# Gen death by million
db.male.ta$per1m <- db.male.ta$su.mta/9780154*1000000

# Linear model
glsfit.mta <- gls(per1m~bi.week + postlock + bi.week:postlock,
                  data = db.male.ta)

# Check for autocorr
acf(resid(glsfit.mta))

# Model estimates
coef(glsfit.mta)
confint(glsfit.mta)

# 1 year slope 95% CI
round(coef(glsfit.mta)[2]*24,2)
round(confint(glsfit.mta)[2]*24,2)
round(confint(glsfit.mta)[2,2]*24,2)

# Post-Lockdown difference per month 95% CI
round(coef(glsfit.mta)[3]*2,2)
round(confint(glsfit.mta)[3]*2,2)
round(confint(glsfit.mta)[3,2]*2,2)

# Post-Lockdown x 1 year Interaction 95% CI
round(coef(glsfit.mta)[4]*24,2)
round(confint(glsfit.mta)[4]*24,2)
round(confint(glsfit.mta)[4,2]*24,2)

# Predict data for graph
data.pred.mta <- db.male.ta
data.pred.mta$per1m <- predict(glsfit.mta, newdata = data.pred.mta)

# Graph for Fig 2
mtag <- ggplot(data = db.male.ta, mapping = aes(x = bi.week, y = per1m)) +
  geom_rect(aes( ymin=-Inf, ymax=Inf, xmin=0, xmax=8, fill="#E9E9E9"), col = "#E9E9E9", alpha=0.25) +
  geom_point() + 
  scale_fill_identity(name = "", guide = "none") +
  scale_x_continuous(name="Every 15 days", breaks=c(-78,-54,-29,-5,4), labels=c("Jan 2017","Jan 2018","Jan 2019","Jan 2020","\nLockdown")) +
  scale_y_continuous(name="Deaths / 1,000,000 Men", limits = c(0,10))+
  geom_vline(xintercept = 0, linetype="longdash") +
  theme(
    plot.background = element_rect(fill = "white"),
    axis.line.x = element_line(color = "black"),
    axis.line.y = element_line(color = "black"),
    panel.grid.major = element_line(color = "grey"),
    panel.ontop = T,
    panel.background = element_rect(fill = NA),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.title = element_text(hjust = 0.5)) +
  ggtitle("Traffic Accidents") +
  geom_vline(xintercept=c(-78,-54,-29,-5), colour="grey") +
  stat_smooth(aes(group = postlock), method = "loess", formula = y~x, span = 0.6, color = "#287AB2", fill = "#287AB2", alpha = 0.3 ) +
  geom_line(data = subset(data.pred.mta, bi.week < 0), colour = "#E65757", size = 1.5, alpha = 0.7) + 
  geom_line(data = subset(data.pred.mta, bi.week > 0), colour = "#E65757", size = 1.5, alpha = 0.7) +
  theme(text=element_text(size=12,  family="sans"))

##ITSA female OA

# Define postlockdown period
db.female.oa$postlock <- ifelse(db.female.oa$bi.week >= 0,1,0)
# Gen death by million
db.female.oa$per1m <- db.female.oa$su.foa/10397401*1000000

# Linear model
glsfit.foa <- gls(per1m~bi.week + postlock + bi.week:postlock,
                  data = db.female.oa)

# Check for autocorr
acf(resid(glsfit.foa))

# Model estimates
coef(glsfit.foa)
confint(glsfit.foa)

# 1 year slope 95% CI
round(coef(glsfit.foa)[2]*24,2)
round(confint(glsfit.foa)[2]*24,2)
round(confint(glsfit.foa)[2,2]*24,2)

# Post-Lockdown difference per month 95% CI
round(coef(glsfit.foa)[3]*2,2)
round(confint(glsfit.foa)[3]*2,2)
round(confint(glsfit.foa)[3,2]*2,2)

# Post-Lockdown x 1 year Interaction 95% CI
round(coef(glsfit.foa)[4]*24,2)
round(confint(glsfit.foa)[4]*24,2)
round(confint(glsfit.foa)[4,2]*24,2)


# Predict data for graph
data.pred.foa <- db.female.oa
data.pred.foa$per1m <- predict(glsfit.foa, newdata = data.pred.foa)

# Graph for Fig 1
foag <- ggplot(data = db.female.oa, mapping = aes(x = bi.week, y = per1m)) +
  geom_rect(aes( ymin=-Inf, ymax=Inf, xmin=0, xmax=8, fill="#E9E9E9"), col = "#E9E9E9", alpha=0.25) +
  geom_point() + 
  scale_fill_identity(name = "", guide = "none") +
  scale_x_continuous(name="Every 15 days", breaks=c(-78,-54,-29,-5,4), labels=c("Jan 2017","Jan 2018","Jan 2019","Jan 2020","\nLockdown")) +
  scale_y_continuous(name="Deaths / 1,000,000 Women", limits = c(0,5))+
  geom_vline(xintercept = 0, linetype="longdash") +
  theme(
    plot.background = element_rect(fill = "white"),
    axis.line.x = element_line(color = "black"),
    axis.line.y = element_line(color = "black"),
    panel.grid.major = element_line(color = "grey"),
    panel.ontop = T,
    panel.background = element_rect(fill = NA),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.title = element_text(hjust = 0.5)) +
  ggtitle("Other Accidental Deaths") +
  geom_vline(xintercept=c(-78,-54,-29,-5), colour="grey") +
  stat_smooth(aes(group = postlock), method = "loess", formula = y~x, span = 0.6, color = "#287AB2", fill = "#287AB2", alpha = 0.3 ) +
  geom_line(data = subset(data.pred.foa, bi.week < 0), colour = "#E65757", size = 1.5, alpha = 0.7) + 
  geom_line(data = subset(data.pred.foa, bi.week > 0), colour = "#E65757", size = 1.5, alpha = 0.7) +
  theme(text=element_text(size=12,  family="sans"))


##ITSA male oa

# Define postlockdown period
db.male.oa$postlock <- ifelse(db.male.oa$bi.week >= 0,1,0)
# Gen death by million
db.male.oa$per1m <- db.male.oa$su.moa/9780154*1000000

# Linear model
glsfit.moa <- gls(per1m~bi.week + postlock + bi.week:postlock,
                  data = db.male.oa)

# Check for autocorr
acf(resid(glsfit.moa))

# Model estimates
coef(glsfit.moa)
confint(glsfit.moa)

# 1 year slope 95% CI
round(coef(glsfit.moa)[2]*24,2)
round(confint(glsfit.moa)[2]*24,2)
round(confint(glsfit.moa)[2,2]*24,2)

# Post-Lockdown difference per month 95% CI
round(coef(glsfit.moa)[3]*2,2)
round(confint(glsfit.moa)[3]*2,2)
round(confint(glsfit.moa)[3,2]*2,2)

# Post-Lockdown x 1 year Interaction 95% CI
round(coef(glsfit.moa)[4]*24,2)
round(confint(glsfit.moa)[4]*24,2)
round(confint(glsfit.moa)[4,2]*24,2)

# Predict data for graph
data.pred.moa <- db.male.oa
data.pred.moa$per1m <- predict(glsfit.moa, newdata = data.pred.moa)

# Graph for Fig 2
moag <- ggplot(data = db.male.oa, mapping = aes(x = bi.week, y = per1m)) +
  geom_rect(aes( ymin=-Inf, ymax=Inf, xmin=0, xmax=8, fill="#E9E9E9"), col = "#E9E9E9", alpha=0.25) +
  geom_point() + 
  scale_fill_identity(name = "", guide = "none") +
  scale_x_continuous(name="Every 15 days", breaks=c(-78,-54,-29,-5,4), labels=c("Jan 2017","Jan 2018","Jan 2019","Jan 2020","\nLockdown")) +
  scale_y_continuous(name="Deaths / 1,000,000 Men", limits = c(0,10))+
  geom_vline(xintercept = 0, linetype="longdash") +
  theme(
    plot.background = element_rect(fill = "white"),
    axis.line.x = element_line(color = "black"),
    axis.line.y = element_line(color = "black"),
    panel.grid.major = element_line(color = "grey"),
    panel.ontop = T,
    panel.background = element_rect(fill = NA),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.title = element_text(hjust = 0.5)) +
  ggtitle("Other Accidental Deaths") +
  geom_vline(xintercept=c(-78,-54,-29,-5), colour="grey") +
  stat_smooth(aes(group = postlock), method = "loess", formula = y~x, span = 0.6, color = "#287AB2", fill = "#287AB2", alpha = 0.3 ) +
  geom_line(data = subset(data.pred.moa, bi.week < 0), colour = "#E65757", size = 1.5, alpha = 0.7) + 
  geom_line(data = subset(data.pred.moa, bi.week > 0), colour = "#E65757", size = 1.5, alpha = 0.7) +
  theme(text=element_text(size=12,  family="sans"))


# Rate of unknow deaths #### 

db.adult$nsc <- ifelse(db.adult$muerte.violenta=="NO SE CONOCE",1,NA)
db.adult$mv <- ifelse(db.adult$muerte.violenta!="SIN REGISTRO",1,NA)

# Sum Male other accident by biweek
nsc <- db.adult %>% group_by(bi.week) %>%  summarise(su.nsc = sum(nsc, na.rm = T))
mv <- db.adult %>% group_by(bi.week) %>%  summarise(su.mv = sum(mv, na.rm = T))

rate <- merge(nsc, mv, by ="bi.week")
# Rate of Unespecified violent deaths/Total violent deaths
rate$aa <- rate$su.nsc/rate$su.mv

# Define postlockdown period
rate$postlock <- ifelse(rate$bi.week>= 0,1,0)

# Linear model
glsfit.aa <- gls(aa~bi.week + postlock + bi.week:postlock,
                 data = rate)

# Predict data for graph
data.pred.aa <- rate
data.pred.aa$aa <- predict(glsfit.aa, newdata = data.pred.aa)

# Graph for Fig 3
uvdpg <-  ggplot(data = rate, mapping = aes(x = bi.week, y = aa)) +
  geom_rect(aes( ymin=-Inf, ymax=Inf, xmin=0, xmax=8, fill="#E9E9E9"), col = "#E9E9E9", alpha=0.25) +
  geom_point() + 
  scale_fill_identity(name = "", guide = "none") +
  scale_x_continuous(name="Every 15 days", breaks=c(-78,-54,-29,-5,4), labels=c("Jan 2017","Jan 2018","Jan 2019","Jan 2020","\nLockdown")) +
  scale_y_continuous(name="Unespecified Violent Deaths / Total Violent Deaths", limits = c(0.1,0.4))+
  geom_vline(xintercept = 0, linetype="longdash") +
  theme(
    plot.background = element_rect(fill = "white"),
    axis.line.x = element_line(color = "black"),
    axis.line.y = element_line(color = "black"),
    panel.grid.major = element_line(color = "grey"),
    panel.ontop = T,
    panel.background = element_rect(fill = NA),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.title = element_text(hjust = 0.5)) +
  ggtitle("") +
  geom_vline(xintercept=c(-78,-54,-29,-5), colour="grey") +
  stat_smooth(aes(group = postlock), method = "loess", formula = y~x, span = 0.6, color = "#287AB2", fill = "#287AB2", alpha = 0.3 ) +
  geom_line(data = subset(data.pred.aa, bi.week < 0), colour = "#E65757", size = 1) + 
  geom_line(data = subset(data.pred.aa, bi.week > 0), colour = "#E65757", size = 1) +
  theme(text=element_text(size=12,  family="sans"))

# Google Mobility trends ####

# Data from google
dbgm <- read.csv(url("https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv?cachebust=7d0cb7d254d29111"),
                 strip.white = TRUE,
                 sep = ',', 
                 header = TRUE,
                 stringsAsFactors = FALSE)

# Peru data until June 28

dbperu <- dbgm %>% filter(country_region=="Peru" & sub_region_1=="" & date<=as.Date("2020-06-28"))

dbperu$dn <- NA

for (i in 1:100) {
  dbperu$dn <- ifelse(dbperu$date>=as.Date("2020-03-16")-(i) & dbperu$date<as.Date("2020-03-16")-(i-1), i*-1 ,dbperu$dn)
}

for (i in 1:180) {
  dbperu$dn <- ifelse(dbperu$date>=as.Date("2020-03-16")+(i-1) & dbperu$date<as.Date("2020-03-16")+(i), i*1 ,dbperu$dn)
}

# Start from 0
dbperu$dn <- ifelse(dbperu$dn>= 0, dbperu$dn-1, dbperu$dn )

# Define postlockdown period
dbperu$postlock <- ifelse(dbperu$dn>= 0,1,0)

# Graph for Fig 3
gmd <- ggplot(data = dbperu, mapping = aes(x = dn, y = transit_stations_percent_change_from_baseline)) +
  geom_rect(aes( ymin=-Inf, ymax=Inf, xmin=0, xmax=104, fill="#E9E9E9"), col = "#E9E9E9", alpha=0.25) +
  geom_point() + 
  scale_fill_identity(name = "", guide = "none") +
  scale_x_continuous(name="Days", breaks=c(-30,-10,40,61,92), labels=c("Feb 15","March 6\nFirst Case","40 days\nLockdown","May 15","Jun 15")
                     ,  limits = c(-30,104)) +
  scale_y_continuous(name="% Change of Mobility to Transit Station")+
  geom_vline(xintercept = -10, linetype="dotted") +
  geom_vline(xintercept = 0, linetype="longdash") +
  geom_vline(xintercept = 40, linetype="dotted") +
  stat_smooth(aes(group = postlock), method = "loess", formula = y~x, span = 0.9, color = "#287AB2", fill = "#287AB2", alpha = 0.3 )+ 
  theme(
    plot.background = element_rect(fill = "white"),
    panel.background = element_rect(fill = NA),
    axis.line.x = element_line(color = "black"),
    axis.line.y = element_line(color = "black"),
    panel.grid.major = element_line(color = "#766E6E"),
    panel.ontop = T,
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank())+
  geom_vline(xintercept=c(-30,61,92), colour="#766E6E")+
  theme(text=element_text(size=12,  family="sans"))



#### Plot Figures ####

fig1 <- ggarrange(fhg,fsg,ftag, foag,
                  ncol = 2, nrow = 2)

annotate_figure(fig1,
                top = text_grob("Violent and accidental deaths in Women", size = 14))

fig2 <-ggarrange(mhg,msg,mtag, moag,
          ncol = 2, nrow = 2)

annotate_figure(fig2,
                top = text_grob("Violent and accidental deaths in Men", size = 14))

ggarrange(uvdpg,gmd,
          labels = c("A","B"),
          align = c("hv"),
          ncol = 2, nrow = 1)




# Final-Project-Biostat


## How to install 
R package with utility functions to help with data analysis
you can install this through use devtools:
```bash
devtools::install_github("NathalyDM/Final-Project-Biostat", upgrade_dependencies = FALSE)
```

### Packages required
```bash
library(ggplot2)
library(tidyverse)
library(rstatix)
library(ggpubr)
library(scales)
library(ggsci)
library(flextable)
library(BiostatSupBiotech)
```

### Boxline plot Function 
```bash
#Change the path of the file
data <- read.csv("data.csv", header=TRUE, sep=";") # Load the data, modify function if required
colnames(data)[1]<-c('ID')
data
data$Rotor.length_c<- factor(data$Rotor.length,labels=c(-1,0,1))
data$Base.length_c<- factor(data$Base.length,labels=c(-1,0,1))
data$Base.width_c<- factor(data$Base.width,labels=c(-1,0,1))
data
summary(data$Time[data$ID==1])

data$ID_c<-as.factor(rep(1:10,3))

boxline.plot(data,data$ID_c,data$Time,'Helicopter Number','Time of Flight (seconds)',"Box and Mean plot of the different in function of the Helicopter Number", "Graphical Representation of Data")
```
![time_fligth](https://user-images.githubusercontent.com/40121093/138725392-85311c4e-6339-4318-8da9-aff3855bdb9a.png)

### One factor Plot Function 
```bash
Mean.Rotor.length<-by(data$Time,data$Rotor.length_c,mean)
Mean.Base_Length<-by(data$Time,data$Base.length_c,mean)
Mean.Base_Width<-by(data$Time,data$Base.width_c,mean)

data$Rep<-as.factor(as.integer(data$Repetition))
data$Rotor.length_c2<- factor(data$Rotor.length_c,labels=c(-1,1), exclude = 0)
data$Base.length_c2<- factor(data$Base.length_c,labels=c(-1,1), exclude = 0)
data$Base.width_c2<- factor(data$Base.width_c,labels=c(-1,1), exclude = 0)

data2<-na.omit(data)
data2$Repetition<-as.factor(data2$Repetition)
onefactor.plot(data2,data2$Rotor.length_c2,data2$Time,data2$Repetition,"Effect of Rotor Length","Rotor Length (cm)","Flight Time (seconds)")
onefactor.plot(data2,data2$Base.length_c2,data2$Time,data2$Rep,"Effect of Base Length","Base Length (cm)","Flight Time (seconds)")
onefactor.plot(data2,data2$Base.width_c2,data2$Time,data2$Rep,"Effect of Base Width","Base Width (cm)","Flight Time (seconds)")
```
![gitimage2](https://user-images.githubusercontent.com/40121093/138725451-23622fb1-2780-4c11-a7f1-fad808dd6bc2.png)

### Two factor Plot Function 
```bash
twofactor.plot(data2,data2$Rotor.length_c2,data2$Time, data2$Base.width_c2,'Effect of Rotor Length and Base Width in y Flight','Rotor Length (cm)',"Base Width (cm)")
twofactor.plot(data2,data2$Base.length_c2,data2$Time, data2$Base.width_c2,'Effect of Base Length and Base Width in y Flight','Base Length (cm)',"Base Width (cm)")
twofactor.plot(data2,data2$Rotor.length_c2,data2$Time, data2$Base.width_c2,'Effect of Rotor Length and Base Length in y Flight','Rotor Length (cm)',"Base Length (cm)")
```
![gitimage1](https://user-images.githubusercontent.com/40121093/138725498-8f7d203e-4810-4d62-b6cb-da14b9aa2bed.png)


### Paretto plot 
Regression Analysis 
```bash
res <-lm(data = data,Time~Rotor.length+Base.length+Base.width+
           Rotor.length*Base.length+
           Base.length*Base.width+
           Rotor.length*Base.width)
res.sum<-summary(res)
res.sum
```
Plot
```bash
res <-lm(data = data,Time~Rotor.length+Base.length+Base.width+
           Rotor.length*Base.length+
           Base.length*Base.width+
           Rotor.length*Base.width)
res.sum<-summary(res)

names.coef<-c('Intercept', 'Rotor Length', 'Base Length', 
                           'Base width','Rotor Length: Base Length',
                           'Base length: Base Width','Rotor length: Base width')
paretto.r<-paretto(res.sum,names.coef)
paretto.r$plot
```

![gitparetto](https://user-images.githubusercontent.com/40121093/138725523-c2bd8007-f5b5-4669-bc22-8acea78b1236.png)


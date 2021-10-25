# Final-Project-Biostat

This package content the next functions with serve to plot the next graphs 

### Boxline plot Function 
```bash
boxline.plot<-function(data,x,y,xlab.caption,ylab.caption,subtitle.caption,title.caption){
  
  ##### Input parameters
  # data: data frame 
  # x   : x axis 
  # y   : y axis 
  # xlab.caption: x label
  # ylab.caption: y label 
  # subtitle.caption : Caption for the subtitle
  # title.caption : Caption for the title
  
  
  ggplot(data,aes(x=x,y=y)) +
  geom_boxplot()+
  geom_jitter(shape=16, position=position_jitter(0.2),alpha=0.7)+
  theme(panel.background = element_rect(fill = "#ffffff"),
          panel.grid.major = element_line(colour="#f0f0f0"),
          panel.grid.minor =element_line(colour="#f0f0f0"),
          panel.border = element_rect(fill = "transparent", 
                                      color = "#f0f0f0",         
                                      size = 1))+
  stat_summary(fun = mean, na.rm = TRUE, aes(group = 1),
               geom = "line", color = "blue", 
               size = 0.5) +
  stat_summary(fun = mean, na.rm = TRUE, 
               geom = "point", shape = "diamond",
               size = 2, color = "blue", 
               position = position_dodge(width = .3))+
  xlab(xlab.caption)+
  ylab(ylab.caption)+
    labs(title = title.caption,
              subtitle = subtitle.caption)
}
```
![time_fligth](https://user-images.githubusercontent.com/40121093/138725392-85311c4e-6339-4318-8da9-aff3855bdb9a.png)

### One factor Plot Function 
```bash
onefactor.plot<-function(data,x,y,color.var,title.var,x.label,y.label){

  ##### Input parameters
  # data: data frame 
  # x   : x axis 
  # y   : y axis 
  # color.var   : variable to classify
  # x.label: x label
  # y.label: y label

  lm<-lm(y~x)
  print('summary')
  print(summary(lm))
  print('R-squared')
  print(summary(lm)$r.squared)
    
ggplot(data, aes(x, y,group = x, color = color.var)) +
  geom_point(position = position_jitterdodge(jitter.width = .2, 
                                             dodge.width = .7), 
             alpha = .9) +
  stat_summary(fun = mean, na.rm = TRUE, aes(group = 1),
               geom = "line", color = "black", 
               size = .3) +
  stat_summary(fun = mean, na.rm = TRUE, 
               geom = "point", shape = "diamond",
               size = 2, color = "black", 
               position = position_dodge(width = .3)) +
  stat_summary(fun.data = mean_cl_normal, na.rm = TRUE, 
               geom = "errorbar", width = .2, color = "black",
               position = position_dodge(width = .3)) +
  theme(panel.background = element_rect(fill = "#ffffff"),
        panel.grid.major = element_line(colour="#f0f0f0"),
        panel.grid.minor =element_line(colour="#f0f0f0"),
        panel.border = element_rect(fill = "transparent", 
                                    color = "#f0f0f0",            
                                    size = 1))+
  labs(title = title.var,
              subtitle = "Plot of two-factor interactions",
              x = x.label, y = y.label, color="Repetition")+
  scale_color_brewer(palette = "Set1")  
}
```
![gitimage2](https://user-images.githubusercontent.com/40121093/138725451-23622fb1-2780-4c11-a7f1-fad808dd6bc2.png)

### Two factor Plot Function 
```bash
twofactor.plot<-function(data,x,y,color.var,title.var,x.label,lengend.name){


  ##### Input parameters
  # data: data frame 
  # x   : x axis 
  # y   : y axis 
  # title.var   : Title
  # legend.name: name of the legend
  # x.label: x label


  minmaxvals <- range(as.integer(as.character(x)))
                      
  print(levels(color.var)[1])
  print(summary(lm(y[color.var==levels(color.var)[1]]~x[color.var==levels(color.var)[1]])))
  
  print(levels(color.var)[2])
  print(summary(lm(y[color.var==levels(color.var)[2]]~x[color.var==levels(color.var)[2]])))
  

  ggplot(data2,aes(x,y,linetype=color.var, 
                 color = color.var))+
  stat_summary(fun = mean, na.rm = TRUE, aes(group = color.var),
               geom = "line", color = "black", 
               size = .3)+
  geom_point(data=subset(data2,x %in% minmaxvals),
               aes(shape=color.var))+ 
  stat_summary(fun = mean, na.rm = TRUE, 
               geom = "point",  shape="diamond",
               size = 2, color = "black",
               position = position_dodge(width = 0)) +
  stat_summary(fun.data = mean_cl_normal, na.rm = TRUE, 
               geom = "errorbar", width = .2, color = "black")+
  theme(panel.background = element_rect(fill = "#ffffff"),
        panel.grid.major = element_line(colour="#f0f0f0"),
        panel.grid.minor =element_line(colour="#f0f0f0"),
        panel.border = element_rect(fill = "transparent", 
                                    color = "#f0f0f0", 
                                    size = 1))+
  labs(title = title.var,
       subtitle = "Plot of two-factor interactions",
       x = x.label, y = "Flight Time (seconds)",
       linetype=lengend.name, color=lengend.name)+
  theme_bw()
}
```
![gitimage1](https://user-images.githubusercontent.com/40121093/138725498-8f7d203e-4810-4d62-b6cb-da14b9aa2bed.png)


### Paretto plot 
```bash
paretto<-function(res.sum,names.coef){

  ##### Input parameters
  # res.sum: summary of linear regression 
     # names.coef: names of coefficients

  paretto<-list()
  res.df<-data.frame(Coeficients=abs(res.sum$coefficients[,'Estimate']),
                   p.value=res.sum$coefficients[,'Pr(>|t|)'], 
                   names=as.factor(names.coef))
  res.df.filtered<-res.df[-1,]
  res.df.filtered$names<-factor(res.df.filtered$names,
                                levels= res.df.filtered$names[order(res.df.filtered$Coeficients)])
  paretto$plot<-ggplot(res.df.filtered,aes(x=Coeficients,y=names,fill=p.value))+
    geom_col() + 
    scale_fill_gradient(low="blue", high="red")+
    labs(Title='Pareto of the effects',
         x='Coeficients',y='Variable',fill="p-value")+
      theme(panel.background = element_rect(fill = "#ffffff"),
          panel.grid.major = element_line(colour="#f0f0f0"),
          panel.grid.minor =element_line(colour="#f0f0f0"),
          panel.border = element_rect(fill = "transparent", 
                                      color = "#f0f0f0", 
                                      size = 1))
  return(paretto)
}
```

![gitparetto](https://user-images.githubusercontent.com/40121093/138725523-c2bd8007-f5b5-4669-bc22-8acea78b1236.png)


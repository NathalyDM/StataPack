#' @title Makes a box line plot
#' @description This function helps to graph a preview of data
#' @param data data
#' @param x x axis
#' @param y y axis
#' @param color.var this is the factor to classify your data in your plot
#' @param x.label caption for x
#' @param lengend.name name of legend
#' @return twofactor.plot.r
#' @examples
#' twofactor.plot(data2,data2$Rotor.length_c2,data2$Time, data2$Base.width_c2,'Effect of Rotor Length and Base Width in y Flight','Rotor Length (cm)',"Base Width (cm)")

twofactor.plot<-function(data,x,y,color.var,title.var,x.label,lengend.name){
  minmaxvals <- range(as.integer(as.character(x)))

  print(levels(color.var)[1])
  print(summary(lm(y[color.var==levels(color.var)[1]]~x[color.var==levels(color.var)[1]])))

  print(levels(color.var)[2])
  print(summary(lm(y[color.var==levels(color.var)[2]]~x[color.var==levels(color.var)[2]])))


  twofactor.plot.r<-ggplot(data2,aes(x,y,linetype=color.var,
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

  return(twofactor.plot.r)
}

#' @title Makes a box line plot
#' @description This function helps to graph a preview of data
#' @param data data
#' @param x x axis
#' @param y y axis
#' @param color.var this is the factor to classify your data in your plot
#' @param x.label caption for x
#' @param y.label caption for y
#' @return onefactor.plot.r
#' @examples
#' onefactor.plot(data2,data2$Rotor.length_c2,data2$Time,data2$Repetition,"Effect of Rotor Length","Rotor Length (cm)","Flight Time (seconds)")

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

  onefactor.plot.r<-ggplot(data, aes(x, y,group = x, color = color.var)) +
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

  return(onefactor.plot.r)
}

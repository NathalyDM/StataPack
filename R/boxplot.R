#' @title Makes a box line plot
#' @description This function helps to graph a preview of data
#' @param data data
#' @param x x axis
#' @param y y axis
#' @param xlab.caption caption for x
#' @param ylab.caption caption for y
#' @param subtitle.caption caption for the subtitle
#' @param title.caption caption for the title
#' @export boxline.plot
#' @return boxline.plot.r
#' @examples
#' boxline.plot(data,data$ID_c,data$Time,'Helicopter Number','Time of Flight (seconds)', "Box and Mean plot of the different in function of the Helicopter Number", "Graphical Representation of Data")


boxline.plot<-function(data,x,y,xlab.caption,ylab.caption,subtitle.caption,title.caption){

  ##### Input parameters
  # data: data frame
  # x   : x axis
  # y   : y axis
  # xlab.caption: x label
  # ylab.caption: y label
  # subtitle.caption : Caption for the subtitle
  # title.caption : Caption for the title


  boxline.plot.r<-ggplot(data,aes(x=x,y=y)) +
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

  return(boxline.plot.r)
}

#' @title Makes a pareto plot
#' @description This function helps to graph a preview of data
#' @param res.sum summary of a linnear regression
#' @param names.coef names of coeficients for your y label
#' @return paretto
#' @examples
#' paretto.r<-paretto(res.sum,names.coef)
#' paretto.r$plot

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

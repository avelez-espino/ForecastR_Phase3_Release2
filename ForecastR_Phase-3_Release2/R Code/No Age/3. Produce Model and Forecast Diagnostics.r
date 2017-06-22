
## Extract ARIMA Modeling Results

extract.arima <- function(model){

   extract.arima.fit <- model

   sink("arimafit.txt")
   print(extract.arima.fit)
   sink()
   out <- readLines("arimafit.txt")
   usePackage("stringr")
   out.pattern <- str_detect(string=out, pattern="ARIMA")
   modelarima <- out[out.pattern==TRUE]
   require(stringr)
   modelarima <- str_trim(modelarima)

   extract.arima.fit.coef.names <- attr(extract.arima.fit$coef,"names")
   extract.arima.fit.coef <- round(extract.arima.fit$coef,4)
   extract.arima.fit.se <- round(sqrt(diag(extract.arima.fit$var.coef)),4)

   ## http://stats.stackexchange.com/questions/8868/how-to-calculate-the-p-value-of-parameters-for-arima-model-in-r

   if (length(extract.arima.fit$coef)==1){
   extract.arima.fit.z.statistic <- extract.arima.fit$coef/sqrt(extract.arima.fit$var.coef)
   } else if (length(extract.arima.fit$coef)>1) {
   extract.arima.fit.z.statistic <- extract.arima.fit$coef/sqrt(diag(extract.arima.fit$var.coef))
   }

   extract.arima.fit.p.value <- pnorm(abs(extract.arima.fit.z.statistic), lower.tail=FALSE)*2
   extract.arima.fit.p.value <- round(extract.arima.fit.p.value, 4)

   if (length(extract.arima.fit$coef)==1){
   extract.arima.fit.table <- data.frame(extract.arima.fit.coef.names,
                                            extract.arima.fit.coef,
                                            extract.arima.fit.se,
                                            round(extract.arima.fit.z.statistic,4),
                                            extract.arima.fit.p.value)
   } else if (length(extract.arima.fit$coef)>1) {
   extract.arima.fit.table <- cbind.data.frame(extract.arima.fit.coef.names,
                                            extract.arima.fit.coef,
                                            extract.arima.fit.se,
                                            round(extract.arima.fit.z.statistic,4),
                                            extract.arima.fit.p.value)
   }

   rownames(extract.arima.fit.table) <- extract.arima.fit.coef.names
   colnames(extract.arima.fit.table) <- c("Coefficient","Estimate","Standard Error","Z-statistic","P-value")

   extract.arima.fit.aic <- extract.arima.fit$aic
   extract.arima.fit.bic <-  extract.arima.fit$bic
   extract.arima.fit.aicc <-  extract.arima.fit$aicc

   extract.arima.fit.sigma2 <- extract.arima.fit$sigma2
   extract.arima.fit.loglik <- extract.arima.fit$loglik

   return(list(extract.arima.fit.model=modelarima,
               extract.arima.fit.table=extract.arima.fit.table,
               extract.arima.fit.aic=extract.arima.fit.aic,
               extract.arima.fit.bic=extract.arima.fit.bic,
               extract.arima.fit.aicc=extract.arima.fit.aicc,
               extract.arima.fit.sigma2=extract.arima.fit.sigma2,
               extract.arima.fit.loglik=extract.arima.fit.loglik))

}





#*******************************************************************************
#  barplot forecasted values: naiveone
#*******************************************************************************

barplot.forecasted.values.individual.stock.naiveone.no.age <- function(pred.int.individual.stock.naiveone.no.age,
                                                                 forecastingyear){

        .e <- environment()

        usePackage("scales")

        myfit <- pred.int.individual.stock.naiveone.no.age


        ### starts here

         years <- subset(myfit$model.data, select=eval(parse(text=paste0("Run_Year"))))

         ## years <- na.omit(years)


         ## retropointforecasts <-  myfits[[i]]$Data[,3]  # historic data ?

         retropointforecasts <-  subset(myfit$model.data, select=eval(parse(text=paste0("-","Run_Year"))))
         ## retropointforecasts <- na.omit(retropointforecasts)


         p <- myfit$PI.ctr

         p <- as.numeric(p)

         p <- ifelse(p>0,p,0)

         p <- round(p)

        ## p <- round(p)

         pointforecast <- p

         forecastingyear <- forecastingyear


        dfretro <- data.frame(years,retropointforecasts)
        names(dfretro) <- c("years","retropointforecasts")

        dffor <- data.frame(forecastingyear,pointforecast)

        usePackage("ggplot2")

        gp1 <- ggplot(data=dfretro, aes(x=years, y=retropointforecasts), environment=.e) +
             geom_rect(aes(xmax=years-1/3,
                xmin=years+1/3,
                ymax=retropointforecasts,
                ymin=0),fill="dodgerblue3") +
        geom_text(data=dfretro,aes(x=years,
               y=retropointforecasts,
               label=paste(comma(round(dfretro$retropointforecasts)))),
               colour="dodgerblue3",angle=90,size=2.5,hjust=0,vjust=0.2) +
        # scale_y_continuous(expand=c(0.01, 0.5)) +
        geom_rect(data=dffor, aes(x=NULL, y=NULL, xmax=forecastingyear-1/3,
                xmin=forecastingyear+1/3,
                ymax=pointforecast,
                ymin=0),fill="violetred2") +
        geom_text(data=dffor,aes(x=forecastingyear,
               y=pointforecast,
               label=paste(comma(round(dffor$pointforecast)))),
               colour="violetred2",angle=90,size=2.5,hjust=0,vjust=0.2) +
          geom_segment(data=dfretro,aes(x = years[1]-0.5,
                     y = mean(dfretro$retropointforecasts),
                     xend = forecastingyear+0.5,
                     yend = mean(dfretro$retropointforecasts)),
                     colour="grey15",linetype=1) +
        annotate("text",x=dfretro$years[1],y=1.3*max(max(dfretro$retropointforecasts,dffor$pointforecast)),
               label=paste("Historical Average =",comma(round(mean(dfretro$retropointforecasts)))), 
               size=2.5, colour="grey15",hjust=0,vjust=0) +
        scale_x_continuous("Return Year", breaks=c(dfretro$years,dffor$forecastingyear)) +
        ## scale_y_continuous("Terminal Run",label=comma, 
        ## breaks=pretty(c(0,1.4*max(max(dfretro$retropointforecasts,dffor$pointforecast))))) +
        scale_y_continuous(paste(stockabundance),label=comma, 
          breaks=pretty(c(0,1.4*max(max(dfretro$retropointforecasts,dffor$pointforecast))))) +
        labs(title=paste("Naive Model (Previous Year)")) +
        theme(plot.title=element_text(size=12, hjust=0.5),
              axis.text.x=element_text(size=8,angle=90,hjust=1,vjust=0.5),
              axis.text.y=element_text(size=8),
              axis.title.x=element_text(size=10,vjust=-0.5),
              axis.title.y=element_text(size=10,vjust=1.5),
              panel.grid = element_blank(),
              panel.border=element_blank(),
            panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),
            axis.text=element_text(colour="black"),
            panel.background = element_rect(fill='white',colour="grey50"))  +
             expand_limits(y=1.4*max(max(dfretro$retropointforecasts,dffor$pointforecast)))


      gp2 <- ggplot(data=dfretro, aes(years,retropointforecasts),environment=.e) +
      geom_line(data=dfretro, aes(years,retropointforecasts),col="dodgerblue3") +
      geom_segment(data=dfretro, aes(x = years[length(years)],
                     y = retropointforecasts[length(retropointforecasts)],
                     xend = dffor$forecastingyear,
                     yend = dffor$pointforecast),
                     colour="violetred2",linetype=2,size=0.7) +
      geom_point(data=dfretro, aes(years,y=retropointforecasts),colour="dodgerblue3",size=2) +
      geom_point(data=dffor, aes(x=forecastingyear,y=pointforecast),colour="violetred2",size=2) +
      geom_text(data=dffor, aes(x=forecastingyear,
                   y=pointforecast,
                   label=paste(comma(round(pointforecast)))),
                   # colour="red",angle=90,vjust=2,size=3) +
                   colour="violetred2",angle=90,hjust=-0.5,vjust=0,size=2.5) +
      geom_segment(data=dfretro,aes(x=years[1],
                       y=mean(retropointforecasts),
                       xend=forecastingyear,
                       yend=mean(retropointforecasts)), colour="grey15") +
      ## scale_y_continuous("Terminal Run",labels=comma, 
      ## breaks=pretty(c(0,1.4*max(max(dfretro$retropointforecasts,dffor$pointforecast))))) +
      scale_y_continuous(paste(stockabundance),labels=comma, 
        breaks=pretty(c(0,1.4*max(max(dfretro$retropointforecasts,dffor$pointforecast))))) +
         scale_x_continuous("Return Year", breaks=c(dfretro$years,dffor$forecastingyear)) +
         ## geom_text(data=dfretro,aes(x=years[1],
         ##              y=max(dfretro$retropointforecasts),
         ##              label=paste("Historical Average = ",comma(round(mean(dfretro$retropointforecasts),2)))),
         ##          colour="burlywood4",size=3,hjust=-1.2,vjust=1) +
     annotate("text",x=dfretro$years[1],y=1.3*max(max(dfretro$retropointforecasts,dffor$pointforecast)),
               label=paste("Historical Average =",comma(round(mean(dfretro$retropointforecasts)))), 
               size=2.5,colour="grey15",hjust=0,vjust=0) +
     labs(title="Naive Model (Previous Year)") +
     theme(plot.title=element_text(size=12, hjust=0.5),
           axis.text.x=element_text(size=8,angle=90,hjust=1,vjust=0.5),
           axis.text.y=element_text(size=8),
           axis.title.x=element_text(size=10,vjust=-0.5),
           axis.title.y=element_text(size=10,vjust=1.5),
              panel.grid = element_blank(),
              panel.border=element_blank(),
            panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),
            axis.text=element_text(colour="black"),
            panel.background = element_rect(fill='white',colour="grey50")) +
     expand_limits(y=1.4*max(max(dfretro$retropointforecasts,dffor$pointforecast)))

     ## print(gp2)

     usePackage("grid")
     usePackage("gridExtra")
     usePackage("ggplot2")

     ## plotlist <- gList(ggplotGrob(gp1),ggplotGrob(gp2))
     ## do.call("grid.arrange", c(plotlist,ncol=1))

     ## p <- arrangeGrob(gp1,gp2)

     p <- grid.arrange(gp1,gp2)

     p

}

if (noagemodelnaiveone) {

    barplot.forecasted.values.individual.stock.naiveone.no.age(pred.int.individual.stock.naiveone.no.age,
                                                                  forecastingyear)
}



#*******************************************************************************
#  barplot forecasted values: avgthree
#*******************************************************************************

barplot.forecasted.values.individual.stock.avgthree.no.age <- function(pred.int.individual.stock.avgthree.no.age,
                                                                 forecastingyear){

        .e <- environment()

        myfit <- pred.int.individual.stock.avgthree.no.age


        ### starts here

         years <- subset(myfit$model.data, select=eval(parse(text=paste0("Run_Year"))))

         ## years <- na.omit(years)


         ## retropointforecasts <-  myfits[[i]]$Data[,3]  # historic data ?

         retropointforecasts <-  subset(myfit$model.data, select=eval(parse(text=paste0("-","Run_Year"))))
         ## retropointforecasts <- na.omit(retropointforecasts)


         p <- myfit$PI.ctr

         p <- as.numeric(p)

         p <- ifelse(p>0,p,0)

         p <- round(p)

        ## p <- round(p)

         pointforecast <- p

         forecastingyear <- forecastingyear


        dfretro <- data.frame(years,retropointforecasts)
        names(dfretro) <- c("years","retropointforecasts")

        dffor <- data.frame(forecastingyear,pointforecast)

        usePackage("ggplot2")

        gp1 <- ggplot(data=dfretro, aes(x=years, y=retropointforecasts), environment=.e) +
             geom_rect(aes(xmax=years-1/3,
                xmin=years+1/3,
                ymax=retropointforecasts,
                ymin=0),fill="dodgerblue3") +
        geom_text(data=dfretro,aes(x=years,
               y=retropointforecasts,
               label=paste(comma(round(dfretro$retropointforecasts)))),
               colour="dodgerblue3",angle=90,size=2.5,hjust=0,vjust=0.2) +
        # scale_y_continuous(expand=c(0.01, 0.5)) +
        geom_rect(data=dffor, aes(x=NULL, y=NULL, xmax=forecastingyear-1/3,
                xmin=forecastingyear+1/3,
                ymax=pointforecast,
                ymin=0),fill="violetred2") +
        geom_text(data=dffor,aes(x=forecastingyear,
               y=pointforecast,
               label=paste(comma(round(dffor$pointforecast)))),
               colour="violetred2",angle=90,size=2.5,hjust=0,vjust=0.2) +
          geom_segment(data=dfretro,aes(x = years[1]-0.5,
                     y = mean(dfretro$retropointforecasts),
                     xend = forecastingyear+0.5,
                     yend = mean(dfretro$retropointforecasts)),
                     colour="grey15",linetype=1) +
        annotate("text",x=dfretro$years[1],y=1.3*max(max(dfretro$retropointforecasts,dffor$pointforecast)),
               label=paste("Historical Average =",comma(round(mean(dfretro$retropointforecasts)))), 
               size=2.5, colour="grey15",hjust=0,vjust=0) +
        scale_x_continuous("Return Year", breaks=c(dfretro$years,dffor$forecastingyear)) +
        ## scale_y_continuous("Terminal Run",label=comma, 
        ## breaks=pretty(c(0,1.4*max(max(dfretro$retropointforecasts,dffor$pointforecast))))) +
        scale_y_continuous(paste(stockabundance),label=comma, 
           breaks=pretty(c(0,1.4*max(max(dfretro$retropointforecasts,dffor$pointforecast))))) +
        labs(title=paste("Naive Model (Average of Previous 3 Years)")) +
        theme(plot.title=element_text(size=12, hjust=0.5),
              axis.text.x=element_text(size=8,angle=90,hjust=1,vjust=0.5),
              axis.text.y=element_text(size=8),
              axis.title.x=element_text(size=10,vjust=-0.5),
              axis.title.y=element_text(size=10,vjust=1.5),
              panel.grid = element_blank(),
              panel.border=element_blank(),
            panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),
            axis.text=element_text(colour="black"),
            panel.background = element_rect(fill='white',colour="grey50"))  +
             expand_limits(y=1.4*max(max(dfretro$retropointforecasts,dffor$pointforecast)))


      gp2 <- ggplot(data=dfretro, aes(years,retropointforecasts),environment=.e) +
      geom_line(data=dfretro, aes(years,retropointforecasts),col="dodgerblue3") +
      geom_segment(data=dfretro, aes(x = years[length(years)],
                     y = retropointforecasts[length(retropointforecasts)],
                     xend = dffor$forecastingyear,
                     yend = dffor$pointforecast),
                     colour="violetred2",linetype=2,size=0.7) +
      geom_point(data=dfretro, aes(years,y=retropointforecasts),colour="dodgerblue3",size=2) +
      geom_point(data=dffor, aes(x=forecastingyear,y=pointforecast),colour="violetred2",size=2) +
      geom_text(data=dffor, aes(x=forecastingyear,
                   y=pointforecast,
                   label=paste(comma(round(pointforecast)))),
                   # colour="red",angle=90,vjust=2,size=3) +
                   colour="violetred2",angle=90,hjust=-0.5,vjust=0,size=2.5) +
      geom_segment(data=dfretro,aes(x=years[1],
                       y=mean(retropointforecasts),
                       xend=forecastingyear,
                       yend=mean(retropointforecasts)), colour="grey15") +
      ## scale_y_continuous("Terminal Run",labels=comma, 
      ## breaks=pretty(c(0,1.4*max(max(dfretro$retropointforecasts,dffor$pointforecast))))) +
      scale_y_continuous(paste(stockabundance),labels=comma, 
        breaks=pretty(c(0,1.4*max(max(dfretro$retropointforecasts,dffor$pointforecast))))) +
         scale_x_continuous("Return Year", breaks=c(dfretro$years,dffor$forecastingyear)) +
         ## geom_text(data=dfretro,aes(x=years[1],
         ##              y=max(dfretro$retropointforecasts),
         ##              label=paste("Historical Average = ",comma(round(mean(dfretro$retropointforecasts),2)))),
         ##          colour="burlywood4",size=3,hjust=-1.2,vjust=1) +
     annotate("text",x=dfretro$years[1],y=1.3*max(max(dfretro$retropointforecasts,dffor$pointforecast)),
               label=paste("Historical Average =",comma(round(mean(dfretro$retropointforecasts)))), 
               size=2.5,colour="grey15",hjust=0,vjust=0) +
     labs(title="Naive Model (Average of Previous 3 Years)") +
     theme(plot.title=element_text(size=12, hjust=0.5),
           axis.text.x=element_text(size=8,angle=90,hjust=1,vjust=0.5),
           axis.text.y=element_text(size=8),
           axis.title.x=element_text(size=10,vjust=-0.5),
           axis.title.y=element_text(size=10,vjust=1.5),
              panel.grid = element_blank(),
              panel.border=element_blank(),
            panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),
            axis.text=element_text(colour="black"),
            panel.background = element_rect(fill='white',colour="grey50")) +
     expand_limits(y=1.4*max(max(dfretro$retropointforecasts,dffor$pointforecast)))

     ## print(gp2)

     usePackage("grid")
     usePackage("gridExtra")
     usePackage("ggplot2")

     ## plotlist <- gList(ggplotGrob(gp1),ggplotGrob(gp2))
     ## do.call("grid.arrange", c(plotlist,ncol=1))

     ## p <- arrangeGrob(gp1,gp2)
     p <- grid.arrange(gp1,gp2)

     p

}


if (noagemodelavgthree) {

barplot.forecasted.values.individual.stock.avgthree.no.age(
    pred.int.individual.stock.avgthree.no.age,
      forecastingyear)

}




#*******************************************************************************
#  barplot forecasted values: avgfive
#*******************************************************************************

barplot.forecasted.values.individual.stock.avgfive.no.age <- function(pred.int.individual.stock.avgfive.no.age,
                                                                 forecastingyear){

        .e <- environment()

        myfit <- pred.int.individual.stock.avgfive.no.age


        ### starts here

         years <- subset(myfit$model.data, select=eval(parse(text=paste0("Run_Year"))))

         ## years <- na.omit(years)


         ## retropointforecasts <-  myfits[[i]]$Data[,3]  # historic data ?

         retropointforecasts <-  subset(myfit$model.data, select=eval(parse(text=paste0("-","Run_Year"))))
         ## retropointforecasts <- na.omit(retropointforecasts)


         p <- myfit$PI.ctr

         p <- as.numeric(p)

         p <- ifelse(p>0,p,0)

         p <- round(p)

        ## p <- round(p)

         pointforecast <- p

         forecastingyear <- forecastingyear


        dfretro <- data.frame(years,retropointforecasts)
        names(dfretro) <- c("years","retropointforecasts")

        dffor <- data.frame(forecastingyear,pointforecast)

        usePackage("ggplot2")

        gp1 <- ggplot(data=dfretro, aes(x=years, y=retropointforecasts), environment=.e) +
             geom_rect(aes(xmax=years-1/3,
                xmin=years+1/3,
                ymax=retropointforecasts,
                ymin=0),fill="dodgerblue3") +
        geom_text(data=dfretro,aes(x=years,
               y=retropointforecasts,
               label=paste(comma(round(dfretro$retropointforecasts)))),
               colour="dodgerblue3",angle=90,size=2.5,hjust=0,vjust=0.2) +
        # scale_y_continuous(expand=c(0.01, 0.5)) +
        geom_rect(data=dffor, aes(x=NULL, y=NULL, xmax=forecastingyear-1/3,
                xmin=forecastingyear+1/3,
                ymax=pointforecast,
                ymin=0),fill="violetred2") +
        geom_text(data=dffor,aes(x=forecastingyear,
               y=pointforecast,
               label=paste(comma(round(dffor$pointforecast)))),
               colour="violetred2",angle=90,size=2.5,hjust=0,vjust=0.2) +
          geom_segment(data=dfretro,aes(x = years[1]-0.5,
                     y = mean(dfretro$retropointforecasts),
                     xend = forecastingyear+0.5,
                     yend = mean(dfretro$retropointforecasts)),
                     colour="grey15",linetype=1) +
        annotate("text",x=dfretro$years[1],y=1.3*max(max(dfretro$retropointforecasts,dffor$pointforecast)),
               label=paste("Historical Average =",comma(round(mean(dfretro$retropointforecasts)))), 
               size=2.5, colour="grey15",hjust=0,vjust=0) +
        scale_x_continuous("Return Year", breaks=c(dfretro$years,dffor$forecastingyear)) +
        ## scale_y_continuous("Terminal Run",label=comma, 
        ## breaks=pretty(c(0,1.4*max(max(dfretro$retropointforecasts,dffor$pointforecast))))) +
        scale_y_continuous(paste(stockabundance),label=comma, 
          breaks=pretty(c(0,1.4*max(max(dfretro$retropointforecasts,dffor$pointforecast))))) +
        labs(title=paste("Naive Model (Average of Previous 5 Years)")) +
        theme(plot.title=element_text(size=12, hjust=0.5),
              axis.text.x=element_text(size=8,angle=90,hjust=1,vjust=0.5),
              axis.text.y=element_text(size=8),
              axis.title.x=element_text(size=10,vjust=-0.5),
              axis.title.y=element_text(size=10,vjust=1.5),
              panel.grid = element_blank(),
              panel.border=element_blank(),
            panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),
            axis.text=element_text(colour="black"),
            panel.background = element_rect(fill='white',colour="grey50"))  +
             expand_limits(y=1.4*max(max(dfretro$retropointforecasts,dffor$pointforecast)))


      gp2 <- ggplot(data=dfretro, aes(years,retropointforecasts),environment=.e) +
      geom_line(data=dfretro, aes(years,retropointforecasts),col="dodgerblue3") +
      geom_segment(data=dfretro, aes(x = years[length(years)],
                     y = retropointforecasts[length(retropointforecasts)],
                     xend = dffor$forecastingyear,
                     yend = dffor$pointforecast),
                     colour="violetred2",linetype=2,size=0.7) +
      geom_point(data=dfretro, aes(years,y=retropointforecasts),colour="dodgerblue3",size=2) +
      geom_point(data=dffor, aes(x=forecastingyear,y=pointforecast),colour="violetred2",size=2) +
      geom_text(data=dffor, aes(x=forecastingyear,
                   y=pointforecast,
                   label=paste(comma(round(pointforecast)))),
                   # colour="red",angle=90,vjust=2,size=3) +
                   colour="violetred2",angle=90,hjust=-0.5,vjust=0,size=2.5) +
      geom_segment(data=dfretro,aes(x=years[1],
                       y=mean(retropointforecasts),
                       xend=forecastingyear,
                       yend=mean(retropointforecasts)), colour="grey15") +
      ## scale_y_continuous("Terminal Run",labels=comma, 
      ## breaks=pretty(c(0,1.4*max(max(dfretro$retropointforecasts,dffor$pointforecast))))) +
      scale_y_continuous(paste(stockabundance),labels=comma, 
         breaks=pretty(c(0,1.4*max(max(dfretro$retropointforecasts,dffor$pointforecast))))) +
         scale_x_continuous("Return Year", breaks=c(dfretro$years,dffor$forecastingyear)) +
         ## geom_text(data=dfretro,aes(x=years[1],
         ##              y=max(dfretro$retropointforecasts),
         ##              label=paste("Historical Average = ",comma(round(mean(dfretro$retropointforecasts),2)))),
         ##          colour="burlywood4",size=3,hjust=-1.2,vjust=1) +
     annotate("text",x=dfretro$years[1],y=1.3*max(max(dfretro$retropointforecasts,dffor$pointforecast)),
               label=paste("Historical Average =",comma(round(mean(dfretro$retropointforecasts)))), 
               size=2.5,colour="grey15",hjust=0,vjust=0) +
     labs(title="Naive Model (Average of Previous 5 Years)") +
     theme(plot.title=element_text(size=12, hjust=0.5),
           axis.text.x=element_text(size=8,angle=90,hjust=1,vjust=0.5),
           axis.text.y=element_text(size=8),
           axis.title.x=element_text(size=10,vjust=-0.5),
           axis.title.y=element_text(size=10,vjust=1.5),
              panel.grid = element_blank(),
              panel.border=element_blank(),
            panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),
            axis.text=element_text(colour="black"),
            panel.background = element_rect(fill='white',colour="grey50")) +
     expand_limits(y=1.4*max(max(dfretro$retropointforecasts,dffor$pointforecast)))

     ## print(gp2)

     usePackage("grid")
     usePackage("gridExtra")
     usePackage("ggplot2")

     ## plotlist <- gList(ggplotGrob(gp1),ggplotGrob(gp2))
     ## do.call("grid.arrange", c(plotlist,ncol=1))

     ## p <- arrangeGrob(gp1,gp2)
     
     p <- grid.arrange(gp1,gp2)

     p

}


if (noagemodelavgfive){

  barplot.forecasted.values.individual.stock.avgfive.no.age(
   pred.int.individual.stock.avgfive.no.age,
    forecastingyear)

}

#*******************************************************************************
#  barplot forecasted values: arima
#*******************************************************************************

barplot.forecasted.values.individual.stock.arima.no.age <- function(pred.int.individual.stock.arima.no.age,
                                                                 forecastingyear){

        .e <- environment()

        myfit <- pred.int.individual.stock.arima.no.age


        ### starts here

         years <- subset(myfit$model.data, select=eval(parse(text=paste0("Run_Year"))))

         ## years <- na.omit(years)


         ## retropointforecasts <-  myfits[[i]]$Data[,3]  # historic data ?

         retropointforecasts <-  subset(myfit$model.data, select=eval(parse(text=paste0("-","Run_Year"))))
         ## retropointforecasts <- na.omit(retropointforecasts)


         p <- myfit$PI.ctr

         p <- as.numeric(p)

         p <- ifelse(p>0,p,0)

         p <- round(p)

        ## p <- round(p)

         pointforecast <- p

         forecastingyear <- forecastingyear


        dfretro <- data.frame(years,retropointforecasts)
        names(dfretro) <- c("years","retropointforecasts")

        dffor <- data.frame(forecastingyear,pointforecast)

        usePackage("ggplot2")

        gp1 <- ggplot(data=dfretro, aes(x=years, y=retropointforecasts), environment=.e) +
             geom_rect(aes(xmax=years-1/3,
                xmin=years+1/3,
                ymax=retropointforecasts,
                ymin=0),fill="dodgerblue3") +
        geom_text(data=dfretro,aes(x=years,
               y=retropointforecasts,
               label=paste(comma(round(dfretro$retropointforecasts)))),
               colour="dodgerblue3",angle=90,size=2.5,hjust=0,vjust=0.2) +
        # scale_y_continuous(expand=c(0.01, 0.5)) +
        geom_rect(data=dffor, aes(x=NULL, y=NULL, xmax=forecastingyear-1/3,
                xmin=forecastingyear+1/3,
                ymax=pointforecast,
                ymin=0),fill="violetred2") +
        geom_text(data=dffor,aes(x=forecastingyear,
               y=pointforecast,
               label=paste(comma(round(dffor$pointforecast)))),
               colour="violetred2",angle=90,size=2.5,hjust=0,vjust=0.2) +
          geom_segment(data=dfretro,aes(x = years[1]-0.5,
                     y = mean(dfretro$retropointforecasts),
                     xend = forecastingyear+0.5,
                     yend = mean(dfretro$retropointforecasts)),
                     colour="grey15",linetype=1) +
        annotate("text",x=dfretro$years[1],y=1.3*max(max(dfretro$retropointforecasts,dffor$pointforecast)),
               label=paste("Historical Average =",comma(round(mean(dfretro$retropointforecasts)))), 
               size=2.5, colour="grey15",hjust=0,vjust=0) +
        scale_x_continuous("Return Year", breaks=c(dfretro$years,dffor$forecastingyear)) +
        ## scale_y_continuous("Terminal Run",label=comma, 
        ## breaks=pretty(c(0,1.4*max(max(dfretro$retropointforecasts,dffor$pointforecast))))) +
        scale_y_continuous(paste(stockabundance),label=comma, 
          breaks=pretty(c(0,1.4*max(max(dfretro$retropointforecasts,dffor$pointforecast))))) +
        labs(title=paste("ARIMA Model")) +
        theme(plot.title=element_text(size=12, hjust=0.5),
              axis.text.x=element_text(size=8,angle=90,hjust=1,vjust=0.5),
              axis.text.y=element_text(size=8),
              axis.title.x=element_text(size=10,vjust=-0.5),
              axis.title.y=element_text(size=10,vjust=1.5),
              panel.grid = element_blank(),
              panel.border=element_blank(),
            panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),
            axis.text=element_text(colour="black"),
            panel.background = element_rect(fill='white',colour="grey50"))  +
             expand_limits(y=1.4*max(max(dfretro$retropointforecasts,dffor$pointforecast)))


      gp2 <- ggplot(data=dfretro, aes(years,retropointforecasts),environment=.e) +
      geom_line(data=dfretro, aes(years,retropointforecasts),col="dodgerblue3") +
      geom_segment(data=dfretro, aes(x = years[length(years)],
                     y = retropointforecasts[length(retropointforecasts)],
                     xend = dffor$forecastingyear,
                     yend = dffor$pointforecast),
                     colour="violetred2",linetype=2,size=0.7) +
      geom_point(data=dfretro, aes(years,y=retropointforecasts),colour="dodgerblue3",size=2) +
      geom_point(data=dffor, aes(x=forecastingyear,y=pointforecast),colour="violetred2",size=2) +
      geom_text(data=dffor, aes(x=forecastingyear,
                   y=pointforecast,
                   label=paste(comma(round(pointforecast)))),
                   # colour="red",angle=90,vjust=2,size=3) +
                   colour="violetred2",angle=90,hjust=-0.5,vjust=0,size=2.5) +
      geom_segment(data=dfretro,aes(x=years[1],
                       y=mean(retropointforecasts),
                       xend=forecastingyear,
                       yend=mean(retropointforecasts)), colour="grey15") +
      ## scale_y_continuous("Terminal Run",labels=comma, 
      ## breaks=pretty(c(0,1.4*max(max(dfretro$retropointforecasts,dffor$pointforecast))))) +
      scale_y_continuous(paste(stockabundance),labels=comma, 
       breaks=pretty(c(0,1.4*max(max(dfretro$retropointforecasts,dffor$pointforecast))))) +
         scale_x_continuous("Return Year", breaks=c(dfretro$years,dffor$forecastingyear)) +
         ## geom_text(data=dfretro,aes(x=years[1],
         ##              y=max(dfretro$retropointforecasts),
         ##              label=paste("Historical Average = ",comma(round(mean(dfretro$retropointforecasts),2)))),
         ##          colour="burlywood4",size=3,hjust=-1.2,vjust=1) +
     annotate("text",x=dfretro$years[1],y=1.3*max(max(dfretro$retropointforecasts,dffor$pointforecast)),
               label=paste("Historical Average =",comma(round(mean(dfretro$retropointforecasts)))), 
               size=2.5,colour="grey15",hjust=0,vjust=0) +
     labs(title="ARIMA Model") +
     theme(plot.title=element_text(size=12, hjust=0.5),
           axis.text.x=element_text(size=8,angle=90,hjust=1,vjust=0.5),
           axis.text.y=element_text(size=8),
           axis.title.x=element_text(size=10,vjust=-0.5),
           axis.title.y=element_text(size=10,vjust=1.5),
           panel.grid = element_blank(),
           panel.border=element_blank(),
           panel.grid.major=element_blank(),
           panel.grid.minor=element_blank(),
           axis.text=element_text(colour="black"),
           panel.background = element_rect(fill='white',colour="grey50")) +
     expand_limits(y=1.4*max(max(dfretro$retropointforecasts,dffor$pointforecast)))

     ## print(gp2)

     usePackage("grid")
     usePackage("gridExtra")
     usePackage("ggplot2")

     ## plotlist <- gList(ggplotGrob(gp1),ggplotGrob(gp2))
     ## do.call("grid.arrange", c(plotlist,ncol=1))

     ## p <- arrangeGrob(gp1,gp2)
     p <- grid.arrange(gp1,gp2)

     p

}


if (noagemodelarima) {

  barplot.forecasted.values.individual.stock.arima.no.age(pred.int.individual.stock.arima.no.age,
                                                                  forecastingyear)

}


#*******************************************************************************
#  barplot forecasted values: expsmooth
#*******************************************************************************

barplot.forecasted.values.individual.stock.expsmooth.no.age <- function(pred.int.individual.stock.expsmooth.no.age,
                                                                 forecastingyear){

        .e <- environment()

        myfit <- pred.int.individual.stock.expsmooth.no.age


        ### starts here

         years <- subset(myfit$model.data, select=eval(parse(text=paste0("Run_Year"))))

         ## years <- na.omit(years)


         ## retropointforecasts <-  myfits[[i]]$Data[,3]  # historic data ?

         retropointforecasts <-  subset(myfit$model.data, select=eval(parse(text=paste0("-","Run_Year"))))
         ## retropointforecasts <- na.omit(retropointforecasts)


         p <- myfit$PI.ctr

         p <- as.numeric(p)

         p <- ifelse(p>0,p,0)

         p <- round(p)

        ## p <- round(p)

         pointforecast <- p

         forecastingyear <- forecastingyear


        dfretro <- data.frame(years,retropointforecasts)
        names(dfretro) <- c("years","retropointforecasts")

        dffor <- data.frame(forecastingyear,pointforecast)

        usePackage("ggplot2")

        gp1 <- ggplot(data=dfretro, aes(x=years, y=retropointforecasts), environment=.e) +
             geom_rect(aes(xmax=years-1/3,
                xmin=years+1/3,
                ymax=retropointforecasts,
                ymin=0),fill="dodgerblue3") +
        geom_text(data=dfretro,aes(x=years,
               y=retropointforecasts,
               label=paste(comma(round(dfretro$retropointforecasts)))),
               colour="dodgerblue3",angle=90,size=2.5,hjust=0,vjust=0.2) +
        # scale_y_continuous(expand=c(0.01, 0.5)) +
        geom_rect(data=dffor, aes(x=NULL, y=NULL, xmax=forecastingyear-1/3,
                xmin=forecastingyear+1/3,
                ymax=pointforecast,
                ymin=0),fill="violetred2") +
        geom_text(data=dffor,aes(x=forecastingyear,
               y=pointforecast,
               label=paste(comma(round(dffor$pointforecast)))),
               colour="violetred2",angle=90,size=2.5,hjust=0,vjust=0.2) +
          geom_segment(data=dfretro,aes(x = years[1]-0.5,
                     y = mean(dfretro$retropointforecasts),
                     xend = forecastingyear+0.5,
                     yend = mean(dfretro$retropointforecasts)),
                     colour="grey15",linetype=1) +
        annotate("text",x=dfretro$years[1],y=1.3*max(max(dfretro$retropointforecasts,dffor$pointforecast)),
                 label=paste("Historical Average =",comma(round(mean(dfretro$retropointforecasts)))), 
                 size=2.5, colour="grey15",hjust=0,vjust=0) +
        scale_x_continuous("Return Year", breaks=c(dfretro$years,dffor$forecastingyear)) +
        ## scale_y_continuous("Terminal Run",label=comma, 
        ## breaks=pretty(c(0,1.4*max(max(dfretro$retropointforecasts,dffor$pointforecast))))) +
        scale_y_continuous(paste(stockabundance),label=comma, 
          breaks=pretty(c(0,1.4*max(max(dfretro$retropointforecasts,dffor$pointforecast))))) +
        labs(title=paste("Exponential Smoothing Model")) +
        theme(plot.title=element_text(size=12, hjust=0.5),
              axis.text.x=element_text(size=8,angle=90,hjust=1,vjust=0.5),
              axis.text.y=element_text(size=8),
              axis.title.x=element_text(size=10,vjust=-0.5),
              axis.title.y=element_text(size=10,vjust=1.5),
              panel.grid = element_blank(),
              panel.border=element_blank(),
              panel.grid.major=element_blank(),
              panel.grid.minor=element_blank(),
              axis.text=element_text(colour="black"),
              panel.background = element_rect(fill='white',colour="grey50"))  +
              expand_limits(y=1.4*max(max(dfretro$retropointforecasts,dffor$pointforecast)))


      gp2 <- ggplot(data=dfretro, aes(years,retropointforecasts),environment=.e) +
      geom_line(data=dfretro, aes(years,retropointforecasts),col="dodgerblue3") +
      geom_segment(data=dfretro, aes(x = years[length(years)],
                     y = retropointforecasts[length(retropointforecasts)],
                     xend = dffor$forecastingyear,
                     yend = dffor$pointforecast),
                     colour="violetred2",linetype=2,size=0.7) +
      geom_point(data=dfretro, aes(years,y=retropointforecasts),colour="dodgerblue3",size=2) +
      geom_point(data=dffor, aes(x=forecastingyear,y=pointforecast),colour="violetred2",size=2) +
      geom_text(data=dffor, aes(x=forecastingyear,
                   y=pointforecast,
                   label=paste(comma(round(pointforecast)))),
                   # colour="red",angle=90,vjust=2,size=3) +
                   colour="violetred2",angle=90,hjust=-0.5,vjust=0,size=2.5) +
      geom_segment(data=dfretro,aes(x=years[1],
                       y=mean(retropointforecasts),
                       xend=forecastingyear,
                       yend=mean(retropointforecasts)), colour="grey15") +
      ## scale_y_continuous("Terminal Run",labels=comma, 
      ## breaks=pretty(c(0,1.4*max(max(dfretro$retropointforecasts,dffor$pointforecast))))) +
      scale_y_continuous(paste(stockabundance),labels=comma, 
        breaks=pretty(c(0,1.4*max(max(dfretro$retropointforecasts,dffor$pointforecast))))) +
         scale_x_continuous("Return Year", breaks=c(dfretro$years,dffor$forecastingyear)) +
         ## geom_text(data=dfretro,aes(x=years[1],
         ##              y=max(dfretro$retropointforecasts),
         ##              label=paste("Historical Average = ",comma(round(mean(dfretro$retropointforecasts),2)))),
         ##          colour="burlywood4",size=3,hjust=-1.2,vjust=1) +
      annotate("text",x=dfretro$years[1],y=1.3*max(max(dfretro$retropointforecasts,dffor$pointforecast)),
               label=paste("Historical Average =", comma(round(mean(dfretro$retropointforecasts)))), 
               size=2.5,colour="grey15",hjust=0,vjust=0) +
      labs(title="Exponential Smoothing Model") +
      theme(plot.title=element_text(size=12, hjust=0.5),
           axis.text.x=element_text(size=8,angle=90,hjust=1,vjust=0.5),
           axis.text.y=element_text(size=8),
           axis.title.x=element_text(size=10,vjust=-0.5),
           axis.title.y=element_text(size=10,vjust=1.5),
           panel.grid = element_blank(),
           panel.border=element_blank(),
           panel.grid.major=element_blank(),
           panel.grid.minor=element_blank(),
           axis.text=element_text(colour="black"),
           panel.background = element_rect(fill='white',colour="grey50")) +
     expand_limits(y=1.4*max(max(dfretro$retropointforecasts,dffor$pointforecast)))

     ## print(gp2)

     usePackage("grid")
     usePackage("gridExtra")
     usePackage("ggplot2")

     ## plotlist <- gList(ggplotGrob(gp1),ggplotGrob(gp2))
     ## do.call("grid.arrange", c(plotlist,ncol=1))

     ## p <- arrangeGrob(gp1,gp2)
     
     p <- grid.arrange(gp1,gp2)

     p

}

if (noagemodelexpsmooth) {

  barplot.forecasted.values.individual.stock.expsmooth.no.age(
  pred.int.individual.stock.expsmooth.no.age,
  forecastingyear)

}



###################################################################################################################################
#**********************************************************************************************************************************
#
#---- plot forecasted values & forecast intervals:  scatterplot (avgthree) -------------------------------------------------
#
#**********************************************************************************************************************************
###################################################################################################################################



scatterplot.forecasted.values.and.forecast.intervals.individual.stock.naiveone.no.age <-
          function(pred.int.individual.stock.naiveone.no.age, forecastingyear){

       .e <- environment()

        myfit <- pred.int.individual.stock.naiveone.no.age


        years <-  subset(myfit$model.data, select=eval(parse(text=paste0("Run_Year"))))
        years <- years[[1]]

        retropointforecasts <-  subset(myfit$model.data, select=eval(parse(text=paste0("-","Run_Year"))))

         retropointforecasts <- retropointforecasts[[1]]

         p <- myfit$PI.ctr

         p <- as.numeric(p)

         p <- ifelse(p>0,p,0)

         p <- round(p)

        pointforecast <- p

        forecastingyear <- forecastingyear


        dfretro <- data.frame(years=years,retropointforecasts=retropointforecasts)

        upper <- round(as.numeric(myfit$PI.upr))
        lower <- round(as.numeric(myfit$PI.lwr))

        lower <- ifelse(lower>0,lower,0)

        dffor <- data.frame(forecastingyear,pointforecast,upper,lower)


    gp <- ggplot(data=dfretro, aes(years,retropointforecasts), environment=.e) +
          ## ggplot(data=dfretro, aes(years,retropointforecasts)) +
      geom_line(aes(years,retropointforecasts), stat="identity", colour="dodgerblue3") +
      geom_rect(data=dffor, aes(x=NULL, y=NULL, xmax=forecastingyear+1/3,
                xmin=forecastingyear-1/3,
                ymax=upper,
                ymin=lower),fill="lightgrey") +
      geom_segment(data=dffor, aes(x = forecastingyear,
                     y = lower,
                     xend = forecastingyear,
                     yend = upper),
                     colour="violetred2",linetype=1,size=0.7) +
      geom_segment(data=dffor, aes(x = forecastingyear-1/3,
                     y = lower,
                     xend = forecastingyear+1/3,
                     yend = lower),
                     colour="violetred2",linetype=1,size=0.7) +
      geom_segment(data=dffor, aes(x = forecastingyear-1/3,
                     y = upper,
                     xend = forecastingyear+1/3,
                     yend = upper),
                     colour="violetred2",linetype=1,size=0.7) +
      geom_segment(data=dfretro, aes(x = dfretro$years[length(dfretro$years)],
                     y = dfretro$retropointforecasts[length(dfretro$retropointforecasts)],
                     xend = dffor$forecastingyear,
                     yend = dffor$pointforecast),
                     colour="violetred2",linetype=3,size=0.7) +
      geom_point(data=dfretro,aes(x=years,y=retropointforecasts),colour="dodgerblue3",size=2) +
      geom_point(data=dffor, aes(x=forecastingyear,y=pointforecast),colour="red",size=2) +
      geom_text(data=dffor, aes(x=forecastingyear+1/2,
                   y=pointforecast,
                   ## label=paste(comma(sprintf("%.2f",pointforecast)))),
                   label=paste(comma(round(pointforecast)))),
                   colour="violetred2",angle=0,hjust=0,size=2.5) +
      geom_text(data=dffor, aes(x=forecastingyear+1/2,     # lower end annotation
                   y=lower,
                   ## label=paste(comma(sprintf("%.2f",lower)))),
                   label=paste(comma(round(lower)))),
                   colour="violetred2",angle=0,hjust=0,size=2.5)+
      geom_text(data=dffor, aes(x=forecastingyear+1/2,     # upper end annotation
                   y=upper,
                   ## label=paste(comma(sprintf("%.2f",upper)))),
                   label=paste(comma(round(upper)))),
                   colour="violetred2",angle=0,hjust=0,size=2.5) +
      geom_segment(data=dfretro, aes(x=years[1],
                       y=mean(retropointforecasts),
                       xend=dffor$forecastingyear,
                       yend=mean(retropointforecasts)), colour="grey15") +
      # scale_y_continuous("Terminal Run",labels=comma) +
      scale_y_continuous(paste(stockabundance),labels=comma) +
        scale_x_continuous("Return Year",breaks=c(dfretro$years,dffor$forecastingyear),
                         limits=c(min(dfretro$years)-1, max(dffor$forecastingyear)+4), expand=c(0,0)) +
         geom_text(data=dfretro, aes(x=years[1]+1,
                       y=1.1*max(dfretro$retropointforecasts,dffor$upper),
                       label=paste("Historical Average = ",comma(round(mean(dfretro$retropointforecasts))))),
                   colour="grey15",size=2.5,hjust=0.1,vjust=0.1) +
      labs(title="Naive Model (Previous Year)") +
      theme( plot.title=element_text(size=12, hjust=0.5),
             axis.text.x=element_text(size=8,angle=90,hjust=1,vjust=0.5),
             axis.text.y=element_text(size=8),
             axis.title.x=element_text(size=10,vjust=-0.5),
             axis.title.y=element_text(size=10,vjust=1.5),
              panel.grid = element_blank(),
              panel.border=element_blank(),
            panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),
            axis.text=element_text(colour="black"),
            panel.background = element_rect(fill='white',colour="grey50"))

     ## print(gp)

     return(gp)

}

## scatterplot.forecasted.values.and.forecast.intervals.individual.stock.naiveone.no.age(pred.int.individual.stock.naiveone.no.age, forecastingyear)



###################################################################################################################################
#**********************************************************************************************************************************
#
#---- plot forecasted values & forecast intervals:  scatterplot (avgthree) -------------------------------------------------
#
#**********************************************************************************************************************************
###################################################################################################################################



scatterplot.forecasted.values.and.forecast.intervals.individual.stock.avgthree.no.age <-
          function(pred.int.individual.stock.avgthree.no.age, forecastingyear){

       .e <- environment()

        myfit <- pred.int.individual.stock.avgthree.no.age


        years <-  subset(myfit$model.data, select=eval(parse(text=paste0("Run_Year"))))
        years <- years[[1]]

        retropointforecasts <-  subset(myfit$model.data, select=eval(parse(text=paste0("-","Run_Year"))))

         retropointforecasts <- retropointforecasts[[1]]

         p <- myfit$PI.ctr

         p <- as.numeric(p)

         p <- ifelse(p>0,p,0)

         p <- round(p)

        pointforecast <- p

        forecastingyear <- forecastingyear


        dfretro <- data.frame(years=years,retropointforecasts=retropointforecasts)

        upper <- round(as.numeric(myfit$PI.upr))
        lower <- round(as.numeric(myfit$PI.lwr))

        lower <- ifelse(lower>0,lower,0)

        dffor <- data.frame(forecastingyear,pointforecast,upper,lower)


    gp <- ggplot(data=dfretro, aes(years,retropointforecasts), environment=.e) +
          ## ggplot(data=dfretro, aes(years,retropointforecasts)) +
      geom_line(aes(years,retropointforecasts), stat="identity", colour="dodgerblue3") +
      geom_rect(data=dffor, aes(x=NULL, y=NULL, xmax=forecastingyear+1/3,
                xmin=forecastingyear-1/3,
                ymax=upper,
                ymin=lower),fill="lightgrey") +
      geom_segment(data=dffor, aes(x = forecastingyear,
                     y = lower,
                     xend = forecastingyear,
                     yend = upper),
                     colour="violetred2",linetype=1,size=0.7) +
      geom_segment(data=dffor, aes(x = forecastingyear-1/3,
                     y = lower,
                     xend = forecastingyear+1/3,
                     yend = lower),
                     colour="violetred2",linetype=1,size=0.7) +
      geom_segment(data=dffor, aes(x = forecastingyear-1/3,
                     y = upper,
                     xend = forecastingyear+1/3,
                     yend = upper),
                     colour="violetred2",linetype=1,size=0.7) +
      geom_segment(data=dfretro, aes(x = dfretro$years[length(dfretro$years)],
                     y = dfretro$retropointforecasts[length(dfretro$retropointforecasts)],
                     xend = dffor$forecastingyear,
                     yend = dffor$pointforecast),
                     colour="violetred2",linetype=3,size=0.7) +
      geom_point(data=dfretro,aes(x=years,y=retropointforecasts),colour="dodgerblue3",size=2) +
      geom_point(data=dffor, aes(x=forecastingyear,y=pointforecast),colour="red",size=2) +
      geom_text(data=dffor, aes(x=forecastingyear+1/2,
                   y=pointforecast,
                   ## label=paste(comma(sprintf("%.2f",pointforecast)))),
                   label=paste(comma(round(pointforecast)))),
                   colour="violetred2",angle=0,hjust=0,size=2.5) +
      geom_text(data=dffor, aes(x=forecastingyear+1/2,     # lower end annotation
                   y=lower,
                   ## label=paste(comma(sprintf("%.2f",lower)))),
                   label=paste(comma(round(lower)))),
                   colour="violetred2",angle=0,hjust=0,size=2.5)+
      geom_text(data=dffor, aes(x=forecastingyear+1/2,     # upper end annotation
                   y=upper,
                   ## label=paste(comma(sprintf("%.2f",upper)))),
                   label=paste(comma(round(upper)))),
                   colour="violetred2",angle=0,hjust=0,size=2.5) +
      geom_segment(data=dfretro, aes(x=years[1],
                       y=mean(retropointforecasts),
                       xend=dffor$forecastingyear,
                       yend=mean(retropointforecasts)), colour="grey15") +
      # scale_y_continuous("Terminal Run",labels=comma) +
      scale_y_continuous(paste(stockabundance),labels=comma) +
        scale_x_continuous("Return Year",breaks=c(dfretro$years,dffor$forecastingyear),
                         limits=c(min(dfretro$years)-1, max(dffor$forecastingyear)+4), expand=c(0,0)) +
         geom_text(data=dfretro, aes(x=years[1]+1,
                       y=1.1*max(dfretro$retropointforecasts,dffor$upper),
                       label=paste("Historical Average = ",comma(round(mean(dfretro$retropointforecasts))))),
                   colour="grey15",size=2.5,hjust=0.1,vjust=0.1) +
      labs(title="Naive Model (Average of Previous 3 Years)") +
      theme( plot.title=element_text(size=12, hjust=0.5),
             axis.text.x=element_text(size=8,angle=90,hjust=1,vjust=0.5),
             axis.text.y=element_text(size=8),
             axis.title.x=element_text(size=10,vjust=-0.5),
             axis.title.y=element_text(size=10,vjust=1.5),
              panel.grid = element_blank(),
              panel.border=element_blank(),
            panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),
            axis.text=element_text(colour="black"),
            panel.background = element_rect(fill='white',colour="grey50"))

     ## print(gp)

     return(gp)

}

## scatterplot.forecasted.values.and.forecast.intervals.individual.stock.avgthree.no.age(pred.int.individual.stock.avgthree.no.age, forecastingyear)



###################################################################################################################################
#**********************************************************************************************************************************
#
#---- plot forecasted values & forecast intervals:  scatterplot (avgfive) -------------------------------------------------
#
#**********************************************************************************************************************************
###################################################################################################################################



scatterplot.forecasted.values.and.forecast.intervals.individual.stock.avgfive.no.age <-
          function(pred.int.individual.stock.avgfive.no.age, forecastingyear){

       .e <- environment()

        myfit <- pred.int.individual.stock.avgfive.no.age


        years <-  subset(myfit$model.data, select=eval(parse(text=paste0("Run_Year"))))
        years <- years[[1]]

        retropointforecasts <-  subset(myfit$model.data, select=eval(parse(text=paste0("-","Run_Year"))))

         retropointforecasts <- retropointforecasts[[1]]

         p <- myfit$PI.ctr

         p <- as.numeric(p)

         p <- ifelse(p>0,p,0)

         p <- round(p)

        pointforecast <- p

        forecastingyear <- forecastingyear


        dfretro <- data.frame(years=years,retropointforecasts=retropointforecasts)

        upper <- round(as.numeric(myfit$PI.upr))
        lower <- round(as.numeric(myfit$PI.lwr))

        lower <- ifelse(lower>0,lower,0)

        dffor <- data.frame(forecastingyear,pointforecast,upper,lower)


    gp <- ggplot(data=dfretro, aes(years,retropointforecasts), environment=.e) +
          ## ggplot(data=dfretro, aes(years,retropointforecasts)) +
      geom_line(aes(years,retropointforecasts), stat="identity", colour="dodgerblue3") +
      geom_rect(data=dffor, aes(x=NULL, y=NULL, xmax=forecastingyear+1/3,
                xmin=forecastingyear-1/3,
                ymax=upper,
                ymin=lower),fill="lightgrey") +
      geom_segment(data=dffor, aes(x = forecastingyear,
                     y = lower,
                     xend = forecastingyear,
                     yend = upper),
                     colour="violetred2",linetype=1,size=0.7) +
      geom_segment(data=dffor, aes(x = forecastingyear-1/3,
                     y = lower,
                     xend = forecastingyear+1/3,
                     yend = lower),
                     colour="violetred2",linetype=1,size=0.7) +
      geom_segment(data=dffor, aes(x = forecastingyear-1/3,
                     y = upper,
                     xend = forecastingyear+1/3,
                     yend = upper),
                     colour="violetred2",linetype=1,size=0.7) +
      geom_segment(data=dfretro, aes(x = dfretro$years[length(dfretro$years)],
                     y = dfretro$retropointforecasts[length(dfretro$retropointforecasts)],
                     xend = dffor$forecastingyear,
                     yend = dffor$pointforecast),
                     colour="violetred2",linetype=3,size=0.7) +
      geom_point(data=dfretro,aes(x=years,y=retropointforecasts),colour="dodgerblue3",size=2) +
      geom_point(data=dffor, aes(x=forecastingyear,y=pointforecast),colour="red",size=2) +
      geom_text(data=dffor, aes(x=forecastingyear+1/2,
                   y=pointforecast,
                   ## label=paste(comma(sprintf("%.2f",pointforecast)))),
                   label=paste(comma(round(pointforecast)))),
                   colour="violetred2",angle=0,hjust=0,size=2.5) +
      geom_text(data=dffor, aes(x=forecastingyear+1/2,     # lower end annotation
                   y=lower,
                   ## label=paste(comma(sprintf("%.2f",lower)))),
                   label=paste(comma(round(lower)))),
                   colour="violetred2",angle=0,hjust=0,size=2.5)+
      geom_text(data=dffor, aes(x=forecastingyear+1/2,     # upper end annotation
                   y=upper,
                   ## label=paste(comma(sprintf("%.2f",upper)))),
                   label=paste(comma(round(upper)))),
                   colour="violetred2",angle=0,hjust=0,size=2.5) +
      geom_segment(data=dfretro, aes(x=years[1],
                       y=mean(retropointforecasts),
                       xend=dffor$forecastingyear,
                       yend=mean(retropointforecasts)), colour="grey15") +
      # scale_y_continuous("Terminal Run",labels=comma) +
      scale_y_continuous(paste(stockabundance),labels=comma) +
        scale_x_continuous("Return Year",breaks=c(dfretro$years,dffor$forecastingyear),
                         limits=c(min(dfretro$years)-1, max(dffor$forecastingyear)+4), expand=c(0,0)) +
         geom_text(data=dfretro, aes(x=years[1]+1,
                       y=1.1*max(dfretro$retropointforecasts,dffor$upper),
                       label=paste("Historical Average = ",comma(round(mean(dfretro$retropointforecasts))))),
                   colour="grey15",size=2.5,hjust=0.1,vjust=0.1) +
      labs(title="Naive Model (Average of Previous 5 Years)") +
      theme( plot.title=element_text(size=12, hjust=0.5),
             axis.text.x=element_text(size=8,angle=90,hjust=1,vjust=0.5),
             axis.text.y=element_text(size=8),
             axis.title.x=element_text(size=10,vjust=-0.5),
             axis.title.y=element_text(size=10,vjust=1.5),
              panel.grid = element_blank(),
              panel.border=element_blank(),
            panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),
            axis.text=element_text(colour="black"),
            panel.background = element_rect(fill='white',colour="grey50"))

     ## print(gp)

     return(gp)

}

## scatterplot.forecasted.values.and.forecast.intervals.individual.stock.avgfive.no.age(pred.int.individual.stock.avgfive.no.age, forecastingyear)




###################################################################################################################################
#**********************************************************************************************************************************
#
#---- plot forecasted values & forecast intervals:  scatterplot (arima) -------------------------------------------------
#
#**********************************************************************************************************************************
###################################################################################################################################



scatterplot.forecasted.values.and.forecast.intervals.individual.stock.arima.no.age <-
          function(pred.int.individual.stock.arima.no.age, forecastingyear){

       .e <- environment()

        myfit <- pred.int.individual.stock.arima.no.age


        years <-  subset(myfit$model.data, select=eval(parse(text=paste0("Run_Year"))))
        years <- years[[1]]

        retropointforecasts <-  subset(myfit$model.data, select=eval(parse(text=paste0("-","Run_Year"))))

         retropointforecasts <- retropointforecasts[[1]]

         p <- myfit$PI.ctr

         p <- as.numeric(p)

         p <- ifelse(p>0,p,0)

         p <- round(p)

        pointforecast <- p

        forecastingyear <- forecastingyear


        dfretro <- data.frame(years=years,retropointforecasts=retropointforecasts)

        upper <- round(as.numeric(myfit$PI.upr))
        lower <- round(as.numeric(myfit$PI.lwr))

        lower <- ifelse(lower>0,lower,0)

        dffor <- data.frame(forecastingyear,pointforecast,upper,lower)


    gp <- ggplot(data=dfretro, aes(years,retropointforecasts), environment=.e) +
          ## ggplot(data=dfretro, aes(years,retropointforecasts)) +
      geom_line(aes(years,retropointforecasts), stat="identity", colour="dodgerblue3") +
      geom_rect(data=dffor, aes(x=NULL, y=NULL, xmax=forecastingyear+1/3,
                xmin=forecastingyear-1/3,
                ymax=upper,
                ymin=lower),fill="lightgrey") +
      geom_segment(data=dffor, aes(x = forecastingyear,
                     y = lower,
                     xend = forecastingyear,
                     yend = upper),
                     colour="violetred2",linetype=1,size=0.7) +
      geom_segment(data=dffor, aes(x = forecastingyear-1/3,
                     y = lower,
                     xend = forecastingyear+1/3,
                     yend = lower),
                     colour="violetred2",linetype=1,size=0.7) +
      geom_segment(data=dffor, aes(x = forecastingyear-1/3,
                     y = upper,
                     xend = forecastingyear+1/3,
                     yend = upper),
                     colour="violetred2",linetype=1,size=0.7) +
      geom_segment(data=dfretro, aes(x = dfretro$years[length(dfretro$years)],
                     y = dfretro$retropointforecasts[length(dfretro$retropointforecasts)],
                     xend = dffor$forecastingyear,
                     yend = dffor$pointforecast),
                     colour="violetred2",linetype=3,size=0.7) +
      geom_point(data=dfretro,aes(x=years,y=retropointforecasts),colour="dodgerblue3",size=2) +
      geom_point(data=dffor, aes(x=forecastingyear,y=pointforecast),colour="red",size=2) +
      geom_text(data=dffor, aes(x=forecastingyear+1/2,
                   y=pointforecast,
                   ## label=paste(comma(sprintf("%.2f",pointforecast)))),
                   label=paste(comma(round(pointforecast)))),
                   colour="violetred2",angle=0,hjust=0,size=2.5) +
      geom_text(data=dffor, aes(x=forecastingyear+1/2,     # lower end annotation
                   y=lower,
                   ## label=paste(comma(sprintf("%.2f",lower)))),
                   label=paste(comma(round(lower)))),
                   colour="violetred2",angle=0,hjust=0,size=2.5)+
      geom_text(data=dffor, aes(x=forecastingyear+1/2,     # upper end annotation
                   y=upper,
                   ## label=paste(comma(sprintf("%.2f",upper)))),
                   label=paste(comma(round(upper)))),
                   colour="violetred2",angle=0,hjust=0,size=2.5) +
      geom_segment(data=dfretro, aes(x=years[1],
                       y=mean(retropointforecasts),
                       xend=dffor$forecastingyear,
                       yend=mean(retropointforecasts)), colour="grey15") +
      # scale_y_continuous("Terminal Run",labels=comma) +
      scale_y_continuous(paste(stockabundance),labels=comma) +
        scale_x_continuous("Return Year",breaks=c(dfretro$years,dffor$forecastingyear),
                         limits=c(min(dfretro$years)-1, max(dffor$forecastingyear)+4), expand=c(0,0)) +
         geom_text(data=dfretro, aes(x=years[1]+1,
                       y=1.1*max(dfretro$retropointforecasts,dffor$upper),
                       label=paste("Historical Average = ",comma(round(mean(dfretro$retropointforecasts))))),
                   colour="grey15",size=2.5,hjust=0.1,vjust=0.1) +
      labs(title="ARIMA Model") +
      theme( plot.title=element_text(size=12, hjust=0.5),
             axis.text.x=element_text(size=8,angle=90,hjust=1,vjust=0.5),
             axis.text.y=element_text(size=8),
             axis.title.x=element_text(size=10,vjust=-0.5),
             axis.title.y=element_text(size=10,vjust=1.5),
              panel.grid = element_blank(),
              panel.border=element_blank(),
            panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),
            axis.text=element_text(colour="black"),
            panel.background = element_rect(fill='white',colour="grey50"))

     ## print(gp)

     return(gp)

}

## scatterplot.forecasted.values.and.forecast.intervals.individual.stock.arima.no.age(pred.int.individual.stock.arima.no.age, forecastingyear)





###################################################################################################################################
#**********************************************************************************************************************************
#
#---- plot forecasted values & forecast intervals:  scatterplot (expsmooth) -------------------------------------------------
#
#**********************************************************************************************************************************
###################################################################################################################################


scatterplot.forecasted.values.and.forecast.intervals.individual.stock.expsmooth.no.age <-
          function(pred.int.individual.stock.expsmooth.no.age, forecastingyear){

       .e <- environment()

        myfit <- pred.int.individual.stock.expsmooth.no.age


        years <-  subset(myfit$model.data, select=eval(parse(text=paste0("Run_Year"))))
        years <- years[[1]]

        retropointforecasts <-  subset(myfit$model.data, select=eval(parse(text=paste0("-","Run_Year"))))

         retropointforecasts <- retropointforecasts[[1]]

         p <- myfit$PI.ctr

         p <- as.numeric(p)

         p <- ifelse(p>0,p,0)

         p <- round(p)

        pointforecast <- p

        forecastingyear <- forecastingyear


        dfretro <- data.frame(years=years,retropointforecasts=retropointforecasts)

        upper <- round(as.numeric(myfit$PI.upr))
        lower <- round(as.numeric(myfit$PI.lwr))

        lower <- ifelse(lower>0,lower,0)

        dffor <- data.frame(forecastingyear,pointforecast,upper,lower)


    gp <- ggplot(data=dfretro, aes(years,retropointforecasts), environment=.e) +
          ## ggplot(data=dfretro, aes(years,retropointforecasts)) +
      geom_line(aes(years,retropointforecasts), stat="identity", colour="dodgerblue3") +
      geom_rect(data=dffor, aes(x=NULL, y=NULL, xmax=forecastingyear+1/3,
                xmin=forecastingyear-1/3,
                ymax=upper,
                ymin=lower),fill="lightgrey") +
      geom_segment(data=dffor, aes(x = forecastingyear,
                     y = lower,
                     xend = forecastingyear,
                     yend = upper),
                     colour="violetred2",linetype=1,size=0.7) +
      geom_segment(data=dffor, aes(x = forecastingyear-1/3,
                     y = lower,
                     xend = forecastingyear+1/3,
                     yend = lower),
                     colour="violetred2",linetype=1,size=0.7) +
      geom_segment(data=dffor, aes(x = forecastingyear-1/3,
                     y = upper,
                     xend = forecastingyear+1/3,
                     yend = upper),
                     colour="violetred2",linetype=1,size=0.7) +
      geom_segment(data=dfretro, aes(x = dfretro$years[length(dfretro$years)],
                     y = dfretro$retropointforecasts[length(dfretro$retropointforecasts)],
                     xend = dffor$forecastingyear,
                     yend = dffor$pointforecast),
                     colour="violetred2",linetype=3,size=0.7) +
      geom_point(data=dfretro,aes(x=years,y=retropointforecasts),colour="dodgerblue3",size=2) +
      geom_point(data=dffor, aes(x=forecastingyear,y=pointforecast),colour="red",size=2) +
      geom_text(data=dffor, aes(x=forecastingyear+1/2,
                   y=pointforecast,
                   ## label=paste(comma(sprintf("%.2f",pointforecast)))),
                   label=paste(comma(round(pointforecast)))),
                   colour="violetred2",angle=0,hjust=0,size=2.5) +
      geom_text(data=dffor, aes(x=forecastingyear+1/2,     # lower end annotation
                   y=lower,
                   ## label=paste(comma(sprintf("%.2f",lower)))),
                   label=paste(comma(round(lower)))),
                   colour="violetred2",angle=0,hjust=0,size=2.5)+
      geom_text(data=dffor, aes(x=forecastingyear+1/2,     # upper end annotation
                   y=upper,
                   ## label=paste(comma(sprintf("%.2f",upper)))),
                   label=paste(comma(round(upper)))),
                   colour="violetred2",angle=0,hjust=0,size=2.5) +
      geom_segment(data=dfretro, aes(x=years[1],
                       y=mean(retropointforecasts),
                       xend=dffor$forecastingyear,
                       yend=mean(retropointforecasts)), colour="grey15") +
      # scale_y_continuous("Terminal Run",labels=comma) +
      scale_y_continuous(paste(stockabundance),labels=comma) +
        scale_x_continuous("Return Year",breaks=c(dfretro$years,dffor$forecastingyear),
                         limits=c(min(dfretro$years)-1, max(dffor$forecastingyear)+4), expand=c(0,0)) +
         geom_text(data=dfretro, aes(x=years[1]+1,
                       y=1.1*max(dfretro$retropointforecasts,dffor$upper),
                       label=paste("Historical Average = ",comma(round(mean(dfretro$retropointforecasts))))),
                   colour="grey15",size=2.5,hjust=0.1,vjust=0.1) +
      labs(title="Exponential Smoothing Model") +
      theme( plot.title=element_text(size=12, hjust=0.5),
             axis.text.x=element_text(size=8,angle=90,hjust=1,vjust=0.5),
             axis.text.y=element_text(size=8),
             axis.title.x=element_text(size=10,vjust=-0.5),
             axis.title.y=element_text(size=10,vjust=1.5),
              panel.grid = element_blank(),
              panel.border=element_blank(),
            panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),
            axis.text=element_text(colour="black"),
            panel.background = element_rect(fill='white',colour="grey50"))

     ## print(gp)

     return(gp)

}

## scatterplot.forecasted.values.and.forecast.intervals.individual.stock.expsmooth.no.age(pred.int.individual.stock.expsmooth.no.age, forecastingyear)


###
### Density Plots of Retrospective Forecast Errors: All models selected by the user  
###

# rmse.results.no.age.expsmooth$resjoin                                               

# rmse.results.no.age.naiveone
# rmse.results.no.age.avgthree
# rmse.results.no.age.avgfive
# rmse.results.no.age.arima
# rmse.results.no.age.expsmooth

plot.dens.retrospective.forecast.errors.individual.stock.no.age <- function(rmse.results.no.age.args,  retro.args){

    .e = environment()

    usePackage("stringr")
    
    names.retro.args <-  names(retro.args) 
    names.retro.args <- str_replace_all(names.retro.args,"results.retro.","")
    names.retro.args <- str_replace_all(names.retro.args,".no.age","")
    names.retro.args
 
    errors.stacked <- NULL 
    labels.stacked <- NULL
    for (k in 1:length(rmse.results.no.age.args)) {
         
          tmperrors <- lapply(rmse.results.no.age.args,'[[',"e")[[k]]
          errors.stacked <- c(errors.stacked, tmperrors)
          
          tmplabel <-  names.retro.args[k] 
          if (tmplabel=="naiveone") {
              labels.stacked <- c(labels.stacked, rep("Naive Model (Previous Year)",length(tmperrors)))
          } else if (tmplabel=="avgthree") {
              labels.stacked <- c(labels.stacked, rep("Naive Model (Average of Previous Three Years)",length(tmperrors)))
          } else if (tmplabel=="avgfive") {
              labels.stacked <- c(labels.stacked, rep("Naive Model (Average of Previous Five Years)",length(tmperrors)))
          } else if (tmplabel=="arima") {
              labels.stacked <- c(labels.stacked, rep("ARIMA Model",length(tmperrors)))
          } else {
              labels.stacked <- c(labels.stacked, rep("Exponential Smoothing Model",length(tmperrors)))
          }
    
    }


    data.stacked <- data.frame(errors=errors.stacked,
                               labels=labels.stacked, stringsAsFactors=FALSE)
    data.stacked$labels <- factor(data.stacked$labels, levels=unique(data.stacked$labels))

    ## usePackage("plyr")

    ## data.stacked <- ddply(data.stacked, c('labels'))
    ## data.stacked$labels <- factor(data.stacked$labels, levels=unique(data.stacked$labels))

    usePackage("ggplot2")
    usePackage("scales")


    d <- data.stacked

    l <- levels(d$labels)
    ## l <- c(l[5], l[3], l[4], l[1], l[2])

    d$labels <- factor(d$labels, levels=l)

    breaks <- NULL
    for (j in 1:length(retro.args)){
        h <- hist(data.stacked$errors[data.stacked$labels==l[j]],plot=FALSE)
        breaks[[j]] <- h$breaks
    }

    #  environment=.e
    g <- ggplot(d, aes(x=errors),environment=.e) +
           ## mapply(function(d, b) {geom_histogram(data=d, aes(y=..count..), breaks=b, right=TRUE, fill="lightblue",colour="black")},
            ## split(d, d$labels), breaks) +
             geom_density(fill="lightblue",colour="black") + 
             facet_wrap(~ labels,  scales="fixed", ncol=1) +
               expand_limits(x=0, y=0) +
                  scale_y_continuous(paste("Frequency"),labels=comma) +
                   scale_x_continuous(paste("Retrospective Forecast Errors"),labels=comma)

      g = g + theme_bw() +
          theme(plot.title=element_text(size=12, hjust=0.5),
                axis.title.x=element_text(size=10,vjust=-0.5),
                axis.title.y=element_text(size=10,vjust=1.5),
                axis.text.x=element_text(size=8),
                axis.text.y=element_text(size=8),
                strip.text.x = element_text(size = 8, colour = "black", angle = 0))

     clim <- NULL
     for (j in 1:length(retro.args)){

        clim <- c(clim, 0)

     }

     dummy2 <- data.frame(z = clim, labels = levels(data.stacked$labels))
     g = g + geom_vline(data=dummy2,aes(xintercept = z), linetype="dashed",col="red", size=0.8)


     return(g)
}


plot.dens.retrospective.forecast.errors.individual.stock.no.age(
  rmse.results.no.age.args,  
  retro.args)




plot.superimposed.dens.retrospective.forecast.errors.individual.stock.no.age <- function(rmse.results.no.age.args,  retro.args){

    .e = environment()

    usePackage("stringr")
    
    names.retro.args <-  names(retro.args) 
    names.retro.args <- str_replace_all(names.retro.args,"results.retro.","")
    names.retro.args <- str_replace_all(names.retro.args,".no.age","")
    names.retro.args
 
    errors.stacked <- NULL 
    labels.stacked <- NULL
    for (k in 1:length(rmse.results.no.age.args)) {
         
          tmperrors <- lapply(rmse.results.no.age.args,'[[',"e")[[k]]
          errors.stacked <- c(errors.stacked, tmperrors)
          
          tmplabel <-  names.retro.args[k] 
          if (tmplabel=="naiveone") {
              labels.stacked <- c(labels.stacked, rep("Naive Model (Previous Year)",length(tmperrors)))
          } else if (tmplabel=="avgthree") {
              labels.stacked <- c(labels.stacked, rep("Naive Model (Average of Previous Three Years)",length(tmperrors)))
          } else if (tmplabel=="avgfive") {
              labels.stacked <- c(labels.stacked, rep("Naive Model (Average of Previous Five Years)",length(tmperrors)))
          } else if (tmplabel=="arima") {
              labels.stacked <- c(labels.stacked, rep("ARIMA Model",length(tmperrors)))
          } else {
              labels.stacked <- c(labels.stacked, rep("Exponential Smoothing Model",length(tmperrors)))
          }
    
    }


    data.stacked <- data.frame(errors=errors.stacked,
                               labels=labels.stacked, stringsAsFactors=FALSE)
    data.stacked$labels <- factor(data.stacked$labels, levels=unique(data.stacked$labels))

    ## usePackage("plyr")

    ## data.stacked <- ddply(data.stacked, c('labels'))
    ## data.stacked$labels <- factor(data.stacked$labels, levels=unique(data.stacked$labels))

    usePackage("ggplot2")
    usePackage("scales")
    usePackage("ggthemes")


    d <- data.stacked

    l <- levels(d$labels)
    ## l <- c(l[5], l[3], l[4], l[1], l[2])

    d$labels <- factor(d$labels, levels=l)

    breaks <- NULL
    for (j in 1:length(retro.args)){
        h <- hist(data.stacked$errors[data.stacked$labels==l[j]],plot=FALSE)
        breaks[[j]] <- h$breaks
    }

    #  environment=.e
    g <- ggplot(d, aes(x=errors, group=labels),environment=.e) +
           ## mapply(function(d, b) {geom_histogram(data=d, aes(y=..count..), breaks=b, right=TRUE, fill="lightblue",colour="black")},
            ## split(d, d$labels), breaks) +
             geom_density(aes(fill=labels, linetype=labels),colour="black", alpha=0.8) + 
             ## facet_wrap(~ labels,  scales="fixed", ncol=1) +
               expand_limits(x=0, y=0) +
                  scale_y_continuous(paste("Frequency"),labels=comma) +
                   scale_x_continuous(paste("Retrospective Forecast Errors"),labels=comma)   + 
                   scale_fill_discrete()

      g = g + theme_bw() +
          theme(plot.title=element_text(size=12, hjust=0.5),
                axis.title.x=element_text(size=10,vjust=-0.5),
                axis.title.y=element_text(size=10,vjust=1.5),
                axis.text.x=element_text(size=8),
                axis.text.y=element_text(size=8),
                strip.text.x = element_text(size = 8, colour = "black", angle = 0), 
                legend.title=element_blank(), 
                legend.position="top")

     clim <- NULL
     for (j in 1:length(retro.args)){

        clim <- c(clim, 0)

     }

     dummy2 <- data.frame(z = clim, labels = levels(data.stacked$labels))
     g = g + geom_vline(data=dummy2,aes(xintercept = z), linetype="dashed",col="red", size=0.8)

     g = g + guides(fill=guide_legend(ncol=1))

     return(g)
}


plot.superimposed.dens.retrospective.forecast.errors.individual.stock.no.age(
  rmse.results.no.age.args,  
  retro.args)

##===============================================================================================
## Bias Coefficient Plots 
##===============================================================================================


plot.bias.coefficients.retrospective.forecast.errors.individual.stock.no.age <- function(rmse.results.no.age.args,  retro.args){

    
    names.retro.args <-  names(retro.args) 
    names.retro.args <- str_replace_all(names.retro.args,"results.retro.","")
    names.retro.args <- str_replace_all(names.retro.args,".no.age","")
    names.retro.args
 
    errors.stacked <- NULL 
    labels.stacked <- NULL
    for (k in 1:length(rmse.results.no.age.args)) {
         
          tmperrors <- lapply(rmse.results.no.age.args,'[[',"e")[[k]]
          errors.stacked <- c(errors.stacked, tmperrors)
   
          tmplabel <-  names.retro.args[k] 
          if (tmplabel=="naiveone") {
              labels.stacked <- c(labels.stacked, rep("Naive Model (Previous Year)",length(tmperrors)))
          } else if (tmplabel=="avgthree") {
              labels.stacked <- c(labels.stacked, rep("Naive Model (Average of Previous Three Years)",length(tmperrors)))
          } else if (tmplabel=="avgfive") {
              labels.stacked <- c(labels.stacked, rep("Naive Model (Average of Previous Five Years)",length(tmperrors)))
          } else if (tmplabel=="arima") {
              labels.stacked <- c(labels.stacked, rep("ARIMA Model",length(tmperrors)))
          } else {
              labels.stacked <- c(labels.stacked, rep("Exponential Smoothing Model",length(tmperrors)))
          }
    
    }


    data.stacked <- data.frame(errors=errors.stacked,
                               labels=labels.stacked, stringsAsFactors=FALSE)
    data.stacked$labels <- factor(data.stacked$labels, levels=unique(data.stacked$labels))

    
    d <- data.stacked
    l <- levels(d$labels)
    d$labels <- factor(d$labels, levels=l)

    par(mfrow=c(length(l),1), mar=c(2,2,2,2), cex.main=0.9)

    bias.coeff.afe.individual.stock.retro.no.age <<- NULL 
 
    for (i in 1:length(l)) {

        data <- subset(d, labels==l[i])

        names(data)[names(data)=="errors"] <- "Error"

        error <- data$Error

        mre.error <- mre(error)

        ## plot.mre(mre.error)
        
        ## gamma <- Arg(mre.error)
        
        ## bias <- 1 - 4*gamma/pi
        
        ## k <- length(bias)


        bias.coeff.updated(mre.error, outplot=2)

        bias.coeff.afe.individual.stock.retro.no.age <<- c(bias.coeff.afe.individual.stock.retro.no.age, bias.coeff.updated(mre.error, outplot=0))


        tmp <- paste(stockabundance, ": ", l[i], sep="")
        
        title(main=paste0(tmp))


     }


     names(bias.coeff.afe.individual.stock.retro.no.age) <<- l 
     
     usePackage("gridGraphics")

     grid.echo()       
                                
     grid.grab() -> mapgrob

     return(mapgrob)

 

    
}


windows()
plot.bias.coefficients.retrospective.forecast.errors.individual.stock.no.age(
  rmse.results.no.age.args,  
  retro.args)





##===============================================================================================
## APPENDIX:  Model Diagnostics for All Considered Models
##===============================================================================================


## fits <-  list(naiveone = fit.naiveone.model.no.age,
##              avgthree = fit.avgthree.model.no.age,
##              avgfive = fit.avgfive.model.no.age,
##              arima = fit.arima.model.no.age,
##              expsmooth = fit.expsmooth.model.no.age)


user.fits <- mget( ls( pattern = "^fit.", env = .GlobalEnv) , env = .GlobalEnv )


all.fits <- c("fit.naiveone.model.no.age",
                "fit.avgthree.model.no.age",
                  "fit.avgfive.model.no.age",
                    "fit.arima.model.no.age", 
                      "fit.expsmooth.model.no.age")

user.fits <-  user.fits[all.fits]

## Source: https://stat.ethz.ch/pipermail/r-help/2006-August/111896.html
delete.NULLs  <-  function(x.list){   # delele null/empty entries in a list
    x.list[unlist(lapply(x.list, length) != 0)]
}

user.fits <- delete.NULLs(user.fits)

## names(user.fits)

## str(user.fits)


##--- Index Plot of Residuals -------------------------------------------------------------------

index.plot.residuals.individual.stock.all.models.no.age <- function(user.fits, retro.args){

    .e = environment()

    usePackage("stringr")
    
    names.retro.args <-  names(retro.args) 
    names.retro.args <- str_replace_all(names.retro.args,"results.retro.","")
    names.retro.args <- str_replace_all(names.retro.args,".no.age","")
    names.retro.args

    residuals.stacked <- NULL
    index.stacked <- NULL
    labels.stacked <- NULL

    ## for (i in 1:length(fits)){  # gives you ages
      for (k in 1:length(user.fits)){

           tmpresiduals <- lapply(user.fits,'[[',"model")[[k]]$residuals
           ## tmpfitted <- lapply(user.fits,'[[',"model")[[k]]$fitted
    
           residuals.stacked <- c( residuals.stacked, tmpresiduals)
           index.stacked <- c(index.stacked, 1:length(tmpresiduals))
           
           tmplabel <-  names.retro.args[k] 
           if (tmplabel=="naiveone") {
              labels.stacked <- c(labels.stacked, rep("Naive Model (Previous Year)",length(tmpresiduals)))
           } else if (tmplabel=="avgthree") {
              labels.stacked <- c(labels.stacked, rep("Naive Model (Average of Previous Three Years)",length(tmpresiduals)))
           } else if (tmplabel=="avgfive") {
              labels.stacked <- c(labels.stacked, rep("Naive Model (Average of Previous Five Years)",length(tmpresiduals)))
           } else if (tmplabel=="arima") {
              labels.stacked <- c(labels.stacked, rep("ARIMA Model",length(tmpresiduals)))
           } else {
              labels.stacked <- c(labels.stacked, rep("Exponential Smoothing Model",length(tmpresiduals)))
           }
           

          }
    ## }

    data.stacked <- data.frame(residuals=residuals.stacked,
                               index=index.stacked,
                               labels=labels.stacked, stringsAsFactors=FALSE)

    data.stacked$labels  <- factor(data.stacked$labels, levels=unique(data.stacked$labels))
    
    ## str(data.stacked$labels)
    
    ##require(plyr)

    ## data.stacked <- ddply(data.stacked, c('labels'))
    ## data.stacked$labels <- factor(data.stacked$labels, levels=c("Naive Model (Previous Year)",
    ##                                                             "Naive Model (Average of Previous 3 Years)",
    ##                                                             "Naive Model (Average of Previous 5 Years)",
    ##                                                             "ARIMA Model",
    ##                                                             "Exponential Smoothing Model"))

    usePackage("ggplot2")
    usePackage("scales")

    px <- pretty(data.stacked$index)
    py <- pretty(data.stacked$residuals)

    ## N <- ceiling(length(fits$model_fits[[i]])/2)

    ## g <- ggplot(data.stacked, aes(fitted,residuals),environment=.e)  +
      g <- ggplot(data.stacked, aes(index,residuals),environment=.e) +
           geom_abline(intercept=0,slope=0,colour="red",size=0.8) +
           geom_point(colour="blue",alpha=0.5) +
          # geom_text(aes(label=labels),col="blue",size=3) +
            # coord_fixed(ratio=1) +
              facet_wrap(~labels, scales="free", ncol = 1) +
                expand_limits(x=0, y=0) +
                  scale_y_continuous(paste("Residuals"),labels=comma) + # , limits=range(py)) +
                   scale_x_continuous(paste("Observation Index"),labels=comma, limits=range(px))
                   ## coord_fixed(ratio=1)

      ## print(g)

      g = g + theme_bw() +
          theme(plot.title=element_text(size=12, hjust=0.5),
                axis.title.x=element_text(size=10,vjust=-0.5),
                axis.title.y=element_text(size=10,vjust=1.5),
                axis.text.x=element_text(size=8),
                axis.text.y=element_text(size=8),
                strip.text.x = element_text(size = 8, colour = "black", angle = 0))

      ## Source: http://trinkerrstuff.wordpress.com/2012/09/01/add-text-annotations-to-ggplot2-faceted-plot/

     return(g)
}



## index.plot.residuals.individual.stock.all.models.no.age(user.fits, retro.args)


##--- Time Series Plot of Residuals -------------------------------------------------------------------

timeseries.plot.residuals.individual.stock.all.models.no.age <- function(user.fits, retro.args){

    .e = environment()

    usePackage("stringr")
    
    names.retro.args <-  names(retro.args) 
    names.retro.args <- str_replace_all(names.retro.args,"results.retro.","")
    names.retro.args <- str_replace_all(names.retro.args,".no.age","")
    names.retro.args

     
    residuals.stacked <- NULL
    index.stacked <- NULL
    labels.stacked <- NULL

    ## for (i in 1:length(user.fits)){  # gives you ages
      for (k in 1:length(user.fits)){
      
           tmpresiduals <- lapply(user.fits,'[[',"model")[[k]]$residuals
           ## tmpfitted <- lapply(user.fits,'[[',"model")[[k]]$fitted

           residuals.stacked <- c( residuals.stacked, tmpresiduals)
           index.stacked <- c(index.stacked, 1:length(tmpresiduals))
           
           tmplabel <-  names.retro.args[k] 
           if (tmplabel=="naiveone") {
              labels.stacked <- c(labels.stacked, rep("Naive Model (Previous Year)",length(tmpresiduals)))
           } else if (tmplabel=="avgthree") {
              labels.stacked <- c(labels.stacked, rep("Naive Model (Average of Previous Three Years)",length(tmpresiduals)))
           } else if (tmplabel=="avgfive") {
              labels.stacked <- c(labels.stacked, rep("Naive Model (Average of Previous Five Years)",length(tmpresiduals)))
           } else if (tmplabel=="arima") {
              labels.stacked <- c(labels.stacked, rep("ARIMA Model",length(tmpresiduals)))
           } else {
              labels.stacked <- c(labels.stacked, rep("Exponential Smoothing Model",length(tmpresiduals)))
           }
           

          }
    ## }

    data.stacked <- data.frame(residuals=residuals.stacked,
                               index=index.stacked,
                               labels=labels.stacked, stringsAsFactors=FALSE)

    data.stacked$labels <- factor(data.stacked$labels, levels=unique(data.stacked$labels))
    
    levels(data.stacked$labels)
    
    ## require(plyr)

    ## data.stacked <- ddply(data.stacked, c('labels'))
    ## data.stacked$labels <- factor(data.stacked$labels, levels=c("Naive Model (Previous Year)",
    ##                                                             "Naive Model (Average of Previous 3 Years)",
    ##                                                             "Naive Model (Average of Previous 5 Years)",
    ##                                                             "ARIMA Model",
    ##                                                             "Exponential Smoothing Model"))

    usePackage("ggplot2")
    usePackage("scales")

    px <- pretty(data.stacked$index)
    py <- pretty(data.stacked$residuals)

    ## N <- ceiling(length(fits$model_fits[[i]])/2)

    ## g <- ggplot(data.stacked, aes(fitted,residuals),environment=.e)  +
      g <- ggplot(data.stacked, aes(index,residuals),environment=.e) +
           geom_abline(intercept=0,slope=0,colour="red",size=0.8) +
           geom_point(colour="blue",alpha=0.5) +
           geom_line(linetype="dashed",colour="blue",alpha=0.5) +
          # geom_text(aes(label=labels),col="blue",size=3) +
            # coord_fixed(ratio=1) +
              facet_wrap(~labels, scales="free", ncol = 1) +
                expand_limits(x=0, y=0) +
                  scale_y_continuous(paste("Residuals"),labels=comma) + # , limits=range(py)) +
                   scale_x_continuous(paste("Observation Index"),labels=comma, limits=range(px))
                   ## coord_fixed(ratio=1)

      ## print(g)

      g = g + theme_bw() +
          theme(plot.title=element_text(size=12, hjust=0.5),
                axis.title.x=element_text(size=10,vjust=-0.5),
                axis.title.y=element_text(size=10,vjust=1.5),
                axis.text.x=element_text(size=8),
                axis.text.y=element_text(size=8),
                strip.text.x = element_text(size = 8, colour = "black", angle = 0))

      ## Source: http://trinkerrstuff.wordpress.com/2012/09/01/add-text-annotations-to-ggplot2-faceted-plot/

     return(g)
     
}

## timeseries.plot.residuals.individual.stock.all.models.no.age(user.fits, retro.args)

##--- ACF Plot of Model Residuals for All Models

acf.plot.residuals.individual.stock.all.models.no.age <- function(user.fits, retro.args){

    ## g2: ACF plot

    usePackage("forecast")
    
    usePackage("stringr")
    
    names.retro.args <-  names(retro.args) 
    names.retro.args <- str_replace_all(names.retro.args,"results.retro.","")
    names.retro.args <- str_replace_all(names.retro.args,".no.age","")
    names.retro.args


    .e <- environment()

    lag.stacked <- NULL
    acf.stacked <- NULL
    labels.stacked <- NULL
    for (j in 1:length(user.fits)){

         tmpresiduals <- lapply(user.fits,'[[',"model")[[j]]$residuals
       
         ## residuals <- residuals(fits[[j]]$model)

         residuals <- tmpresiduals 

         tmp <- Acf(residuals,plot=FALSE)$lag

         lagmax <- max(tmp)+1
         lag <- 1:lagmax

         acf <-  Acf(residuals,lag.max=lagmax, plot=FALSE)$acf[-1]

         lag.stacked <- c(lag.stacked, lag)
         acf.stacked <- c(acf.stacked, acf)

         
         tmplabel <-  names.retro.args[j] 
         if (tmplabel=="naiveone") {
              labels.stacked <- c(labels.stacked, rep("Naive Model (Previous Year)",length(lag)))
         } else if (tmplabel=="avgthree") {
              labels.stacked <- c(labels.stacked, rep("Naive Model (Average of Previous Three Years)",length(lag)))
         } else if (tmplabel=="avgfive") {
              labels.stacked <- c(labels.stacked, rep("Naive Model (Average of Previous Five Years)",length(lag)))
         } else if (tmplabel=="arima") {
              labels.stacked <- c(labels.stacked, rep("ARIMA Model",length(lag)))
         } else {
              labels.stacked <- c(labels.stacked, rep("Exponential Smoothing Model",length(lag)))
         }

        

    }


    data.stacked <- data.frame(lag=lag.stacked, acf=acf.stacked, labels=labels.stacked, stringsAsFactors=FALSE)

    data.stacked$labels <- factor(data.stacked$labels, levels=unique(data.stacked$labels))

    # environment=.e
    g2 <- ggplot(data.stacked, aes(lag,acf), environment=.e) +
           geom_linerange(aes(x=lag, ymin=0, ymax=acf), colour="red", size=0.6) +
           ## geom_hline(yintercept=2*mean(data.stacked$leverage)) +   ## doesn't work
          # geom_text(aes(label=labels),col="blue",size=3) +
            # coord_fixed(ratio=1) +
              # facet_wrap(~labels, scales="free", ncol = 1) +
                facet_wrap(~labels, scales="fixed", ncol = 1) +
                expand_limits(x=1, y=0) +
                    scale_x_continuous(paste("Lag"),breaks=0:length(unique(data.stacked$lag)))
                   ## coord_fixed(ratio=1)

      ## print(g)

      g2 = g2 + theme_bw() +
          theme(plot.title=element_text(size=12, hjust=0.5),
                axis.title.x=element_text(size=10,vjust=-0.5),
                axis.title.y=element_text(size=10,vjust=1.5),
                axis.text.x=element_text(size=8),
                axis.text.y=element_text(size=8),
                strip.text.x = element_text(size = 8, colour = "black", angle = 0))


     clim <- NULL
     for (j in 1:length(user.fits)){
     
        tmpresiduals <- lapply(user.fits,'[[',"model")[[j]]$residuals

        ## residuals <- residuals(fits[[j]]$model)
        
        residuals <- tmpresiduals
        
        acftmp <- Acf(residuals, plot=FALSE)

        ci <- 0.95 # Indicates 95% confidence level
        clim0 <- qnorm((1 + ci)/2)/sqrt(acftmp$n.used)
        clim <- c(clim, clim0)

     }

     dummy2 <- data.frame(z = clim, labels = levels(data.stacked$labels))

     g2 = g2 + geom_hline(aes(yintercept = z), data=dummy2, linetype="dashed",col="blue", size=0.6)

     dummy3 <- data.frame(z = -clim, labels = levels(data.stacked$labels))

     g2 = g2 + geom_hline(aes(yintercept = z), data=dummy3, linetype="dashed",col="blue", size=0.6)


     clim_zero <- NULL
     for (j in 1:length(user.fits)){
        clim_zero <- c(clim_zero, 0)
     }

     dummy0 <- data.frame(z = clim_zero, labels = levels(data.stacked$labels))

     g2 = g2 + geom_hline(aes(yintercept = z), data=dummy0, linetype="solid",col="black", size=0.6)

     Min <- min(by(data.stacked$acf, data.stacked$labels, min))
     Max <- max(by(data.stacked$acf, data.stacked$labels, max))

     M <- max(abs(Min),abs(Max), clim)

     g2 <- g2 +  scale_y_continuous(paste("ACF of Model Residuals"),labels=comma, limits=c(-M,M))

     ## ylim(-M,M)


     return(g2)


}

## acf.plot.residuals.individual.stock.all.models.no.age(user.fits, retro.args)



##--- PACF Plot of Model Residuals for All Models

pacf.plot.residuals.individual.stock.all.models.no.age <- function(user.fits, retro.args){

    ## g3: PACF plot


    .e <- environment()

    usePackage("forecast")

    usePackage("stringr")
    
    names.retro.args <-  names(retro.args) 
    names.retro.args <- str_replace_all(names.retro.args,"results.retro.","")
    names.retro.args <- str_replace_all(names.retro.args,".no.age","")
    names.retro.args

    lag.stacked <- NULL
    pacf.stacked <- NULL
    labels.stacked <- NULL
    for (j in 1:length(user.fits)){

         ## residuals <- residuals(fits[[j]]$model)

         tmpresiduals <- lapply(user.fits,'[[',"model")[[j]]$residuals

         residuals <- tmpresiduals

         tmp <- Acf(residuals,plot=FALSE)$lag

         lagmax <- max(tmp)+1
         lag <- 1:lagmax

         # acf <-  Acf(residuals,lag.max=lagmax, plot=FALSE)$acf[-1]
         pacf <- Pacf(residuals,lag.max=lagmax, plot=FALSE)$acf

         lag.stacked <- c(lag.stacked, lag)
         pacf.stacked <- c(pacf.stacked, pacf)

         tmplabel <-  names.retro.args[j] 
         if (tmplabel=="naiveone") {
              labels.stacked <- c(labels.stacked, rep("Naive Model (Previous Year)",length(lag)))
         } else if (tmplabel=="avgthree") {
              labels.stacked <- c(labels.stacked, rep("Naive Model (Average of Previous Three Years)",length(lag)))
         } else if (tmplabel=="avgfive") {
              labels.stacked <- c(labels.stacked, rep("Naive Model (Average of Previous Five Years)",length(lag)))
         } else if (tmplabel=="arima") {
              labels.stacked <- c(labels.stacked, rep("ARIMA Model",length(lag)))
         } else {
              labels.stacked <- c(labels.stacked, rep("Exponential Smoothing Model",length(lag)))
         }


    }


    data.stacked <- data.frame(lag=lag.stacked, pacf=pacf.stacked, labels=labels.stacked, stringsAsFactors=FALSE)
    data.stacked$labels <- factor(data.stacked$labels, levels=unique(data.stacked$labels))
    

    # environment=.e
    g3 <- ggplot(data.stacked, aes(lag,pacf), environment=.e) +
           geom_linerange(aes(x=lag, ymin=0, ymax=pacf), colour="red", size=0.6) +
           ## geom_hline(yintercept=2*mean(data.stacked$leverage)) +   ## doesn't work
          # geom_text(aes(label=labels),col="blue",size=3) +
            # coord_fixed(ratio=1) +
              # facet_wrap(~labels, scales="free", ncol = 1) +
                facet_wrap(~labels, scales="fixed", ncol = 1) +
                expand_limits(x=1, y=0) +
                    scale_x_continuous(paste("Lag"),breaks=0:length(unique(data.stacked$lag)))
                   ## coord_fixed(ratio=1)

      ## print(g)

      g3 = g3 + theme_bw() +
          theme(plot.title=element_text(size=12, hjust=0.5),
                axis.title.x=element_text(size=10,vjust=-0.5),
                axis.title.y=element_text(size=10,vjust=1.5),
                axis.text.x=element_text(size=8),
                axis.text.y=element_text(size=8),
                strip.text.x = element_text(size = 8, colour = "black", angle = 0))


     clim <- NULL
     for (j in 1:length(user.fits)){

        ## residuals <- residuals(fits[[j]]$model)
        
        tmpresiduals <- lapply(user.fits,'[[',"model")[[j]]$residuals
        residuals <- tmpresiduals 
        
        acftmp <- Acf(residuals, plot=FALSE)

        ci <- 0.95 # Indicates 95% confidence level
        clim0 <- qnorm((1 + ci)/2)/sqrt(acftmp$n.used)
        clim <- c(clim, clim0)

     }

     dummy2 <- data.frame(z = clim, labels = levels(data.stacked$labels))

     g3 = g3 + geom_hline(aes(yintercept = z), data=dummy2, linetype="dashed",col="blue", size=0.6)

     dummy3 <- data.frame(z = -clim, labels = levels(data.stacked$labels))

     g3 = g3 + geom_hline(aes(yintercept = z), data=dummy3, linetype="dashed",col="blue", size=0.6)


     clim_zero <- NULL
     for (j in 1:length(user.fits)){
        clim_zero <- c(clim_zero, 0)
     }

     dummy0 <- data.frame(z = clim_zero, labels = levels(data.stacked$labels))

     g3 = g3 + geom_hline(aes(yintercept = z), data=dummy0, linetype="solid",col="black", size=0.6)

     Min <- min(by(data.stacked$pacf, data.stacked$labels, min))
     Max <- max(by(data.stacked$pacf, data.stacked$labels, max))

     M <- max(abs(Min),abs(Max), clim)

     g3 <- g3 +  scale_y_continuous(paste("PACF of Model Residuals"),labels=comma, limits=c(-M,M))

     ## ylim(-M,M)


     return(g3)


}

## pacf.plot.residuals.individual.stock.all.models.no.age(user.fits, retro.args)


##--- P-Values of Ljung-Box Test for All Models

ljung.box.plot.residuals.individual.stock.all.models.no.age <- function(datalist, user.fits, retro.args, boxcoxtransform){

    ## g3: Plot of p-values from Ljung-Box test

     .e <- environment()

    usePackage("forecast")
    usePackage("portes")
    
    usePackage("stringr")
    
    names.retro.args <-  names(retro.args) 
    names.retro.args <- str_replace_all(names.retro.args,"results.retro.","")
    names.retro.args <- str_replace_all(names.retro.args,".no.age","")
    names.retro.args 

    ## residuals <- residuals(fits[[4]]$model)  ## pull out residuals from ARIMA model
    arima.model.fit.no.age  <- arima.model.no.age(datalist, boxcoxtransform)
    fit.arima.model.no.age <- arima.model.fit.no.age
    residuals <- residuals(fit.arima.model.no.age$model)
    
    tmp <- Acf(residuals,plot=FALSE)$lag     ## so you can compute complete lags
    lagmax <- max(tmp)+1
    lag <- 1:lagmax


    lag.stacked <- NULL
    pvalue.stacked <- NULL
    labels.stacked <- NULL
    for (j in 1:length(user.fits)){

         #  residuals <- residuals(fits[[j]]$model) 
         tmpresiduals <- lapply(user.fits,'[[',"model")[[j]]$residuals
         residuals <- tmpresiduals 
         residuals <- residuals[!is.na(residuals)]

         #### pvalue <-  LjungBox(residuals, lags=lag)[,"pvalue"]
         pvalue <- LjungBox(residuals, lags=lag)[,ncol(LjungBox(residuals, lags=lag))]

         ## lb <- LjungBox(residuals, lags=lag)

         ## pval.name <- dimnames(lb)[[2]][4]

         ## pvalue <- lb[,ncol(lb)]

         ## print(pval.name)
         ## print(pvalue)

         lag.stacked <- c(lag.stacked, lag)
         pvalue.stacked <- c(pvalue.stacked, pvalue)

         tmplabel <-  names.retro.args[j] 
         if (tmplabel=="naiveone") {
              labels.stacked <- c(labels.stacked, rep("Naive Model (Previous Year)",length(lag)))
         } else if (tmplabel=="avgthree") {
              labels.stacked <- c(labels.stacked, rep("Naive Model (Average of Previous Three Years)",length(lag)))
         } else if (tmplabel=="avgfive") {
              labels.stacked <- c(labels.stacked, rep("Naive Model (Average of Previous Five Years)",length(lag)))
         } else if (tmplabel=="arima") {
              labels.stacked <- c(labels.stacked, rep("ARIMA Model",length(lag)))
         } else {
              labels.stacked <- c(labels.stacked, rep("Exponential Smoothing Model",length(lag)))
         }

      

    }


    data.stacked <- data.frame(lag=lag.stacked, pvalue=pvalue.stacked, labels=labels.stacked, stringsAsFactors=FALSE)

    data.stacked$labels <- factor(data.stacked$labels, levels=unique(data.stacked$labels))


    # environment=.e
    g3 <- ggplot(data.stacked, aes(lag,pvalue), environment=.e) +
           geom_line(colour="black", size=0.5) +
            geom_point(colour="black", size=2) +
           ## geom_hline(yintercept=2*mean(data.stacked$leverage)) +   ## doesn't work
          # geom_text(aes(label=labels),col="blue",size=3) +
            # coord_fixed(ratio=1) +
              # facet_wrap(~labels, scales="free", ncol = 1) +
                facet_wrap(~labels, scales="fixed", ncol = 1) +
                ## expand_limits(x=1, y=0) +
                    scale_x_continuous(paste("Lag"),breaks=0:length(unique(data.stacked$lag)))
                   ## coord_fixed(ratio=1)

      ## print(g)

      g3 = g3 + theme_bw() +
          theme(plot.title=element_text(size=12, hjust=0.5),
                axis.title.x=element_text(size=10,vjust=-0.5),
                axis.title.y=element_text(size=10,vjust=1.5),
                axis.text.x=element_text(size=8),
                axis.text.y=element_text(size=8),
                strip.text.x = element_text(size = 8, colour = "black", angle = 0))



     clim_zero <- NULL
     for (j in 1:length(user.fits)){
        clim_zero <- c(clim_zero, 0.05)
     }

     dummy0 <- data.frame(z = clim_zero, labels = levels(data.stacked$labels))

     g3 = g3 + geom_hline(aes(yintercept = z), data=dummy0, linetype="dashed",col="red", size=0.6)

     g3 <- g3 +  scale_y_continuous(paste("P-values of Ljung-Box Test"),labels=comma)

     return(g3)


}

## ljung.box.plot.residuals.individual.stock.all.models.no.age(datalist, user.fits, retro.args, boxcoxtransform)





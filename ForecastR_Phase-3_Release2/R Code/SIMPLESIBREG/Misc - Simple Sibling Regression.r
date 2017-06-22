cat("Misc - Sibling Regression.R", "\n\n")

#*******************************************************************************
#  barplot forecasted values (youngest age)
#
#*******************************************************************************

## total.index

## pred.int.individual.ages.avgfive.youngest
## pred.int.individual.ages.arima.youngest
## pred.int.individual.ages.expsmooth.youngest

SIMPLESIBREG$barplot.forecasted.values.youngest.age.simplesib <- function(pred.int.individual.ages.avgfive.youngest,
                                                              pred.int.individual.ages.arima.youngest,
                                                              pred.int.individual.ages.expsmooth.youngest,
                                                              result.avgfive.youngest, result.arima.youngest, result.expsmooth.youngest, 
                                                              forecastingyear, total.index, stockabundance){

        .e <- environment()

        if (total.index==1) {   # naive model for youngest age
            myfit <- pred.int.individual.ages.avgfive.youngest # single model fit
            age <- myfit$age
            
            myretro <- result.avgfive.youngest[[1]]
            
        }

        if (total.index==2) {   # arima model for youngest age
            myfit <- pred.int.individual.ages.arima.youngest # single model fit
            age <- myfit$age
            
            myretro <- result.arima.youngest[[1]]
        }

        if (total.index==3) {   # exponential smoothing model for youngest age
            myfit <- pred.int.individual.ages.expsmooth.youngest # single model fit
            age <- myfit$age
            
            myretro <- result.expsmooth.youngest[[1]]
        }


        ### starts here


        years <- subset(myretro, select="cy")
         
        ## retropointforecasts <-  myretro$p    
         
        retroactualvalues <-  myretro$a  

         p <- myfit$PI.ctr

         p <- as.numeric(p)

         p <- ifelse(p>0,p,0)

         p <- round(p)

         ## p <- round(p)

         pointforecast <- p

         forecastingyear <- forecastingyear


         dfretro <- data.frame(years,retroactualvalues)
         names(dfretro) <- c("years","retroactualvalues")

         dffor <- data.frame(forecastingyear,pointforecast)

         usePackage("ggplot2")

         gp1 <- ggplot(data=dfretro, aes(x=years, y=retroactualvalues), environment=.e) +
             geom_rect(aes(xmax=years-1/3,
                xmin=years+1/3,
                ymax=retroactualvalues,
                ymin=0),fill="dodgerblue3") +
         geom_text(data=dfretro,aes(x=years,
               y=retroactualvalues,
               label=paste(comma(round(dfretro$retroactualvalues)))),
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
                     y = mean(dfretro$retroactualvalues),
                     xend = forecastingyear+0.5,
                     yend = mean(dfretro$retroactualvalues)),
                     colour="grey15",linetype=1) +
         annotate("text",x=dfretro$years[1],y=1.3*max(max(dfretro$retroactualvalues,dffor$pointforecast)),
               label=paste("Historical Average =",comma(round(mean(dfretro$retroactualvalues)))), size=2.5, colour="grey15",hjust=0,vjust=0) +
         scale_x_continuous("Return Year", breaks=c(dfretro$years,dffor$forecastingyear)) +
         ## scale_y_continuous("Terminal Run",label=comma, breaks=pretty(c(0,1.4*max(max(dfretro$retroactualvalues,dffor$pointforecast))))) +
         scale_y_continuous(paste(stockabundance),label=comma, breaks=pretty(c(0,1.4*max(max(dfretro$retroactualvalues,dffor$pointforecast))))) +
         ## labs(title=paste(age)) +
         ggtitle(label=paste(age)) + 
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
             expand_limits(y=1.4*max(max(dfretro$retroactualvalues,dffor$pointforecast)))


      gp2 <- ggplot(data=dfretro, aes(years,retroactualvalues),environment=.e) +
      geom_line(data=dfretro, aes(years,retroactualvalues),col="dodgerblue3") +
      geom_segment(data=dfretro, aes(x = years[length(years)],
                     y = retroactualvalues[length(retroactualvalues)],
                     xend = dffor$forecastingyear,
                     yend = dffor$pointforecast),
                     colour="violetred2",linetype=2,size=0.7) +
      geom_point(data=dfretro, aes(years,y=retroactualvalues),colour="dodgerblue3",size=2) +
      geom_point(data=dffor, aes(x=forecastingyear,y=pointforecast),colour="violetred2",size=2) +
      geom_text(data=dffor, aes(x=forecastingyear,
                   y=pointforecast,
                   label=paste(comma(round(pointforecast)))),
                   # colour="red",angle=90,vjust=2,size=3) +
                   colour="violetred2",angle=90,hjust=-0.5,vjust=0,size=2.5) +
      geom_segment(data=dfretro,aes(x=years[1],
                       y=mean(retroactualvalues),
                       xend=forecastingyear,
                       yend=mean(retroactualvalues)), colour="grey15") +
      ## scale_y_continuous("Terminal Run",labels=comma, breaks=pretty(c(0,1.4*max(max(dfretro$retroactualvalues,dffor$pointforecast))))) +
      scale_y_continuous(paste(stockabundance),labels=comma, breaks=pretty(c(0,1.4*max(max(dfretro$retroactualvalues,dffor$pointforecast))))) +
         scale_x_continuous("Return Year", breaks=c(dfretro$years,dffor$forecastingyear)) +
         ## geom_text(data=dfretro,aes(x=years[1],
         ##              y=max(dfretro$retroactualvalues),
         ##              label=paste("Historical Average = ",comma(round(mean(dfretro$retroactualvalues),2)))),
         ##          colour="burlywood4",size=3,hjust=-1.2,vjust=1) +
     annotate("text",x=dfretro$years[1],y=1.3*max(max(dfretro$retroactualvalues,dffor$pointforecast)),
               label=paste("Historical Average =",comma(round(mean(dfretro$retroactualvalues)))), size=2.5,colour="grey15",hjust=0,vjust=0) +
     ## labs(title=paste(age)) +
     ggtitle(label=paste(age)) + 
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
     expand_limits(y=1.4*max(max(dfretro$retroactualvalues,dffor$pointforecast)))

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


SIMPLESIBREG$barplot.forecasted.values.youngest.age.simplesib(SIMPLESIBREG$pred.int.individual.ages.avgfive.youngest,
                                                              SIMPLESIBREG$pred.int.individual.ages.arima.youngest,
                                                              SIMPLESIBREG$pred.int.individual.ages.expsmooth.youngest,
                                                              SIMPLESIBREG$result.avgfive.youngest, 
                                                              SIMPLESIBREG$result.arima.youngest, 
                                                              SIMPLESIBREG$result.expsmooth.youngest, 
                                                              SIMPLESIBREG$forecastingyear, 
                                                              SIMPLESIBREG$total.index, 
                                                              SIMPLESIBREG$stockabundance)


#*******************************************************************************
#  scatterplot of forecasted value with superimposed forecast interval (youngest age)
#
#*******************************************************************************


SIMPLESIBREG$scatterplot.forecasted.values.and.forecast.intervals.youngest.age.simple.sibling.regression <- function(pred.int.individual.ages.avgfive.youngest,
                                                              pred.int.individual.ages.arima.youngest,
                                                              pred.int.individual.ages.expsmooth.youngest,
                                                              result.avgfive.youngest, result.arima.youngest, result.expsmooth.youngest, 
                                                              forecastingyear, total.index, stockabundance){

     .e <- environment()

     if (total.index==1) {   # naive model for youngest age
            myfit <- pred.int.individual.ages.avgfive.youngest # single model fit
            age <- myfit$age
            
            myretro <- result.avgfive.youngest[[1]]
        }

     if (total.index==2) {   # arima model for youngest age
            myfit <- pred.int.individual.ages.arima.youngest # single model fit
            age <- myfit$age
            
            myretro <- result.arima.youngest[[1]]
     }

     if (total.index==3) {   # exponential smoothing model for youngest age
            myfit <- pred.int.individual.ages.expsmooth.youngest # single model fit
            age <- myfit$age
            
            myretro <- result.expsmooth.youngest[[1]]
     }

     usePackage("stringr")

     years <- myretro$cy 

     ## retropointforecasts <- myretro$p

     retroactualvalues <- myretro$a 

     p <-  as.numeric(myfit$PI.ctr)

     pointforecast <- round(p)

     dfretro <- data.frame(years=years,retroactualvalues=retroactualvalues)

     upper <- as.numeric(myfit$PI.upr)
     lower <- as.numeric(myfit$PI.lwr)

     dffor <- data.frame(forecastingyear,pointforecast,upper,lower)


     gp <- ggplot(data=dfretro, aes(years,retroactualvalues), environment=.e) +
          ## ggplot(data=dfretro, aes(years,retroactualvalues)) +
      geom_line(aes(years,retroactualvalues), stat="identity", colour="dodgerblue3") +
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
                     y = dfretro$retroactualvalues[length(dfretro$retroactualvalues)],
                     xend = dffor$forecastingyear,
                     yend = dffor$pointforecast),
                     colour="violetred2",linetype=3,size=0.7) +
      geom_point(data=dfretro,aes(x=years,y=retroactualvalues),colour="dodgerblue3",size=2) +
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
                       y=mean(retroactualvalues),
                       xend=dffor$forecastingyear,
                       yend=mean(retroactualvalues)), colour="grey15") +
      # scale_y_continuous("Terminal Run",labels=comma) +
      scale_y_continuous(paste(stockabundance),labels=comma) +
        scale_x_continuous("Return Year",breaks=c(dfretro$years,dffor$forecastingyear),
                         limits=c(min(dfretro$years)-1, max(dffor$forecastingyear)+4), expand=c(0,0)) +
         geom_text(data=dfretro, aes(x=years[1]+1,
                       y=1.1*max(dfretro$retroactualvalues,dffor$upper),
                       label=paste("Historical Average = ",comma(round(mean(dfretro$retroactualvalues))))),
                   colour="grey15",size=2.5,hjust=0.1,vjust=0.1) +
      ## labs(title=paste(age)) +
      ggtitle(label=paste(age)) + 
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



SIMPLESIBREG$scatterplot.forecasted.values.and.forecast.intervals.youngest.age.simple.sibling.regression(
                       SIMPLESIBREG$pred.int.individual.ages.avgfive.youngest,
                       SIMPLESIBREG$pred.int.individual.ages.arima.youngest,
                       SIMPLESIBREG$pred.int.individual.ages.expsmooth.youngest,
                       SIMPLESIBREG$result.avgfive.youngest, 
                       SIMPLESIBREG$result.arima.youngest, 
                       SIMPLESIBREG$result.expsmooth.youngest, 
                       SIMPLESIBREG$forecastingyear, 
                       SIMPLESIBREG$total.index, 
                       SIMPLESIBREG$stockabundance)



#*******************************************************************************
#  barplot forecasted values (total age)
#
#*******************************************************************************


SIMPLESIBREG$barplot.forecasted.values.total.age.simplesib <- function(pred.int.individual.ages.avgfive.youngest,
                                                              pred.int.individual.ages.arima.youngest,
                                                              pred.int.individual.ages.expsmooth.youngest,
                                                              pred.int.total.age.simple.sibling.regression.all.models,
                                                              best.fits,
                                                              result.avgfive.youngest, result.arima.youngest, result.expsmooth.youngest, 
                                                              forecastingyear, total.index, stockabundance){

         .e <- environment()

         if (total.index==1) {   # naive model for youngest age
            myfit <- pred.int.individual.ages.avgfive.youngest # single model fit
            
            myretro <- result.avgfive.youngest[[1]]
         }

         if (total.index==2) {   # arima model for youngest age
            myfit <- pred.int.individual.ages.arima.youngest # single model fit
            
            myretro <- result.arima.youngest[[1]]
            
         }

         if (total.index==3) {   # exponential smoothing model for youngest age
            myfit <- pred.int.individual.ages.expsmooth.youngest # single model fit
            
            myretro <- result.expsmooth.youngest[[1]]
         }


        ### calendar years and abundance for youngest age

        ## years <- subset(myfit$model.data, select="CY")

        ## years <- na.omit(years)

        years <- myretro$cy 

        ## retropointforecasts <- myretro$p
        
        retroactualvalues <- myretro$a 
        
        ####
        #### calendar years and abundance for older ages
        ####
        
        ages <- names(best.fits)
        years.sib <- vector("list",length(ages))
        retroactualvalues.sib <- vector("list",length(ages))
        for (k in 1:length(ages)){

             age <- ages[k]

             usePackage("stringr")

             ## age <- str_replace(age, "_"," ")

             agetmp <- str_replace(age, " ","_")

             numeric_age <- as.numeric(gsub("[^\\d]+", "", age, perl=TRUE))

             cat("k=",k,"\n")

             years.sib[[k]] <- best.fits[[k]]$data$Brood_Year  + numeric_age

             ## retroactualvalues <-  fits[[i]]$model.data[,3]  # historic data ?
             retroactualvalues.sib[[k]] <- as.numeric(unlist(subset(best.fits[[k]]$data, select=agetmp)))
        }  # Come back here!!!!
        
        
        retroactualvalues.list <- c(list(retroactualvalues), retroactualvalues.sib)
        years.list <- c(list(years), years.sib)
        
        ## min.years <- lapply(years.list, FUN=min)
        ## min.year <- min(unlist(min.years))

        years.retroactualvalues.df <- vector("list",length(years.list))
        for (k in 1:length(years.list)){
             usePackage("plyr")
             mytmp <- t(plyr::ldply(list(years.list[[k]],retroactualvalues.list[[k]]), rbind))
             colnames(mytmp) <- c("Calendar_Year","Actual_Abundance")
             years.retroactualvalues.df[[k]] <- mytmp
        }
        
        ## lapply(years.retroactualvalues.df, FUN=merge)

        ## plyr::ldply(years.list, cbind)
        
        ## Reduce(function(x, y) merge(x, y, all=TRUE), years.retroactualvalues.df)

        cbindPad <- function(args){
          ## args <- list(...)
          n <- sapply(args,nrow)
          mx <- max(n)
          pad <- function(x, mx){
          if (nrow(x) < mx){
            nms <- colnames(x)
            padTemp <- matrix(NA,mx - nrow(x), ncol(x))
            colnames(padTemp) <- nms
            return(rbind(padTemp,x))
          } else{
            return(x)
          }
          }
          rs <- lapply(args,pad,mx)
          return(do.call(cbind,rs))
        }

        pad.years.retroactualvalues.df <- cbindPad(args=years.retroactualvalues.df)

        ## pad.years.retroactualvalues.df <- as.data.frame(pad.years.retroactualvalues.df)

        pad.retroactualvalues <- pad.years.retroactualvalues.df[,dimnames(pad.years.retroactualvalues.df)[[2]] %in% "Actual_Abundance"]
        pad.years <- pad.years.retroactualvalues.df[,dimnames(pad.years.retroactualvalues.df)[[2]] %in% "Calendar_Year"]

        sum.pad.retroactualvalues <- apply(pad.retroactualvalues,1,sum)
        sum.pad.years <- apply(pad.years,1,min,na.rm=TRUE)


        total.age.years <- sum.pad.years[!is.na(sum.pad.retroactualvalues)]
        total.age.retroactualvalues <- sum.pad.retroactualvalues[!is.na(sum.pad.retroactualvalues)]

        p <- pred.int.total.age.simple.sibling.regression.all.models$p[[total.index]]   # point forecast for total age

        p <- as.numeric(p)

        p <- ifelse(p>0,p,0)

        p <- round(p)

        pointforecast <- p

        forecastingyear <- forecastingyear


        dfretro <- data.frame(years=total.age.years,retroactualvalues=total.age.retroactualvalues )
        names(dfretro) <- c("years","retroactualvalues")

        dffor <- data.frame(forecastingyear,pointforecast)

        usePackage("ggplot2")

        gp1 <- ggplot(data=dfretro, aes(x=years, y=retroactualvalues), environment=.e) +   # environment=.e
             geom_rect(aes(xmax=years-1/3,
                xmin=years+1/3,
                ymax=retroactualvalues,
                ymin=0),fill="dodgerblue3") +
        geom_text(data=dfretro,aes(x=years,
               y=retroactualvalues,
               label=paste(comma(round(dfretro$retroactualvalues)))),
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
                     y = mean(dfretro$retroactualvalues),
                     xend = forecastingyear+0.5,
                     yend = mean(dfretro$retroactualvalues)),
                     colour="grey15",linetype=1) +
        annotate("text",x=dfretro$years[1],y=1.3*max(max(dfretro$retroactualvalues,dffor$pointforecast)),
               label=paste("Historical Average =",comma(round(mean(dfretro$retroactualvalues)))), 
               size=2.5, colour="grey15",hjust=0,vjust=0) +
        scale_x_continuous("Return Year", breaks=c(dfretro$years,dffor$forecastingyear)) +
        ## scale_y_continuous("Terminal Run",label=comma, breaks=pretty(c(0,1.4*max(max(dfretro$retroactualvalues,dffor$pointforecast))))) +
        scale_y_continuous(paste("Total",stockabundance),label=comma, 
        breaks=pretty(c(0,1.4*max(max(dfretro$retroactualvalues,dffor$pointforecast))))) +
        ## labs(title=paste("Total",stockabundance)) +
        ggtitle(paste("Total",stockabundance)) + 
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
             expand_limits(y=1.4*max(max(dfretro$retroactualvalues,dffor$pointforecast)))


      gp2 <- ggplot(data=dfretro, aes(years,retroactualvalues), environment=.e) +   # environment=.e
      geom_line(data=dfretro, aes(years,retroactualvalues),col="dodgerblue3") +
      geom_segment(data=dfretro, aes(x = years[length(years)],
                     y = retroactualvalues[length(retroactualvalues)],
                     xend = dffor$forecastingyear,
                     yend = dffor$pointforecast),
                     colour="violetred2",linetype=2,size=0.7) +
      geom_point(data=dfretro, aes(years,y=retroactualvalues),colour="dodgerblue3",size=2) +
      geom_point(data=dffor, aes(x=forecastingyear,y=pointforecast),colour="violetred2",size=2) +
      geom_text(data=dffor, aes(x=forecastingyear,
                   y=pointforecast,
                   label=paste(comma(round(pointforecast)))),
                   # colour="red",angle=90,vjust=2,size=3) +
                   colour="violetred2",angle=90,hjust=-0.5,vjust=0,size=2.5) +
      geom_segment(data=dfretro,aes(x=years[1],
                       y=mean(retroactualvalues),
                       xend=forecastingyear,
                       yend=mean(retroactualvalues)), colour="grey15") +
      ## scale_y_continuous("Terminal Run",labels=comma, breaks=pretty(c(0,1.4*max(max(dfretro$retroactualvalues,dffor$pointforecast))))) +
      scale_y_continuous(paste("Total",stockabundance),labels=comma, 
      breaks=pretty(c(0,1.4*max(max(dfretro$retroactualvalues,dffor$pointforecast))))) +
         scale_x_continuous("Return Year", breaks=c(dfretro$years,dffor$forecastingyear)) +
         ## geom_text(data=dfretro,aes(x=years[1],
         ##              y=max(dfretro$retroactualvalues),
         ##              label=paste("Historical Average = ",comma(round(mean(dfretro$retroactualvalues),2)))),
         ##          colour="burlywood4",size=3,hjust=-1.2,vjust=1) +
     annotate("text",x=dfretro$years[1],y=1.3*max(max(dfretro$retroactualvalues,dffor$pointforecast)),
               label=paste("Historical Average =",comma(round(mean(dfretro$retroactualvalues)))), 
               size=2.5,colour="grey15",hjust=0,vjust=0) +
     ## labs(title=paste("Total", stockabundance)) +
     ggtitle(label=paste("Total", stockabundance)) + 
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
     expand_limits(y=1.4*max(max(dfretro$retroactualvalues,dffor$pointforecast)))

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


SIMPLESIBREG$barplot.forecasted.values.total.age.simplesib(SIMPLESIBREG$pred.int.individual.ages.avgfive.youngest,
                                                           SIMPLESIBREG$pred.int.individual.ages.arima.youngest,
                                                           SIMPLESIBREG$pred.int.individual.ages.expsmooth.youngest,
                                                           SIMPLESIBREG$pred.int.total.age.simple.sibling.regression.all.models,
                                                           SIMPLESIBREG$best.fits,
                                                           SIMPLESIBREG$result.avgfive.youngest, 
                                                           SIMPLESIBREG$result.arima.youngest, 
                                                           SIMPLESIBREG$result.expsmooth.youngest, 
                                                           SIMPLESIBREG$forecastingyear, 
                                                           SIMPLESIBREG$total.index, 
                                                           SIMPLESIBREG$stockabundance)








#*******************************************************************************
#  scatterplot of forecasted value with superimposed forecast interval (total age)
#
#*******************************************************************************

SIMPLESIBREG$scatterplot.forecasted.values.and.forecast.intervals.total.age.simple.sibling.regression <- function(pred.int.individual.ages.avgfive.youngest,
                                                              pred.int.individual.ages.arima.youngest,
                                                              pred.int.individual.ages.expsmooth.youngest,
                                                              pred.int.total.age.simple.sibling.regression.all.models,
                                                              best.fits,
                                                              result.avgfive.youngest, result.arima.youngest, result.expsmooth.youngest, 
                                                              forecastingyear, total.index, stockabundance){

        .e <- environment()

         if (total.index==1) {   # naive model for youngest age
            myfit <- pred.int.individual.ages.avgfive.youngest # single model fit
            
            myretro <- result.avgfive.youngest[[1]]
         }

         if (total.index==2) {   # arima model for youngest age
            myfit <- pred.int.individual.ages.arima.youngest # single model fit
            
            myretro <- result.arima.youngest[[1]]
         }

         if (total.index==3) {   # exponential smoothing model for youngest age
            myfit <- pred.int.individual.ages.expsmooth.youngest # single model fit
            
            myretro <- result.expsmooth.youngest[[1]] 
         }


        ### calendar years and abundance for youngest age

        ## years <- subset(myfit$model.data, select="CY")
        
        years <- myretro$cy
        
        ## years <- na.omit(years)

        ## retroactualvalues <-  subset(myfit$model.data, select=-c(CY, BY))  # historical abundance for youngest age
        ## retroactualvalues <- na.omit(retroactualvalues)

        retroactualvalues <- myretro$p  

        ####
        #### calendar years and abundance for older ages
        ####

        ages <- names(best.fits)
        years.sib <- vector("list",length(ages))
        retroactualvalues.sib <- vector("list",length(ages))
        for (k in 1:length(ages)){

             age <- ages[k]

             usePackage("stringr")

             ## age <- str_replace(age, "_"," ")

             agetmp <- str_replace(age, " ","_")

             numeric_age <- as.numeric(gsub("[^\\d]+", "", age, perl=TRUE))

             cat("k=",k,"\n")

             years.sib[[k]] <- best.fits[[k]]$data$Brood_Year  + numeric_age

             ## retroactualvalues <-  fits[[i]]$model.data[,3]  # historic data ?
             retroactualvalues.sib[[k]] <- as.numeric(unlist(subset(best.fits[[k]]$data, select=agetmp)))
        }  # Come back here!!!!

        retroactualvalues.list <- c(list(retroactualvalues), retroactualvalues.sib)
        years.list <- c(list(years), years.sib)

        ## min.years <- lapply(years.list, FUN=min)
        ## min.year <- min(unlist(min.years))

        years.retroactualvalues.df <- vector("list",length(years.list))
        for (k in 1:length(years.list)){
             usePackage("plyr")
             mytmp <- t(plyr::ldply(list(years.list[[k]],retroactualvalues.list[[k]]), rbind))
             colnames(mytmp) <- c("Calendar_Year","Actual_Abundance")
             years.retroactualvalues.df[[k]] <- mytmp
        }

        ## lapply(years.retroactualvalues.df, FUN=merge)

        ## plyr::ldply(years.list, cbind)

        ## Reduce(function(x, y) merge(x, y, all=TRUE), years.retroactualvalues.df)

        cbindPad <- function(args){
          ## args <- list(...)
          n <- sapply(args,nrow)
          mx <- max(n)
          pad <- function(x, mx){
          if (nrow(x) < mx){
            nms <- colnames(x)
            padTemp <- matrix(NA,mx - nrow(x), ncol(x))
            colnames(padTemp) <- nms
            return(rbind(padTemp,x))
          } else{
            return(x)
          }
          }
          rs <- lapply(args,pad,mx)
          return(do.call(cbind,rs))
        }

        pad.years.retroactualvalues.df <- cbindPad(args=years.retroactualvalues.df)

        ## pad.years.retroactualvalues.df <- as.data.frame(pad.years.retroactualvalues.df)

        pad.retroactualvalues <- pad.years.retroactualvalues.df[,dimnames(pad.years.retroactualvalues.df)[[2]] %in% "Actual_Abundance"]
        pad.years <- pad.years.retroactualvalues.df[,dimnames(pad.years.retroactualvalues.df)[[2]] %in% "Calendar_Year"]

        sum.pad.retroactualvalues <- apply(pad.retroactualvalues,1,sum)
        sum.pad.years <- apply(pad.years,1,min,na.rm=TRUE)


        total.age.years <- sum.pad.years[!is.na(sum.pad.retroactualvalues)]
        total.age.retroactualvalues <- sum.pad.retroactualvalues[!is.na(sum.pad.retroactualvalues)]

        p <- pred.int.total.age.simple.sibling.regression.all.models$p[[total.index]]   # point forecast for total age

        p <- as.numeric(p)

        p <- ifelse(p>0,p,0)

        p <- round(p)

        pointforecast <- p

        forecastingyear <- forecastingyear


        dfretro <- data.frame(years=total.age.years,retroactualvalues=total.age.retroactualvalues )
        names(dfretro) <- c("years","retroactualvalues")

       ### IGNORE

       upper <- as.numeric(pred.int.total.age.simple.sibling.regression.all.models$p.upr[[total.index]])
       lower <- as.numeric(pred.int.total.age.simple.sibling.regression.all.models$p.lwr[[total.index]])

       dffor <- data.frame(forecastingyear,pointforecast,upper,lower)


       usePackage("ggplot2")

       gp <- ggplot(data=dfretro, aes(years,retroactualvalues), environment=.e) +       ## environment=.e
              ## ggplot(data=dfretro, aes(years,retroactualvalues)) +
              geom_line(data=dfretro, aes(years,retroactualvalues), stat="identity", colour="dodgerblue3") +
              geom_rect(data=dffor, aes(x=NULL, y = NULL, xmax=forecastingyear+1/3,
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
                     y = dfretro$retroactualvalues[length(dfretro$retroactualvalues)],
                     xend = dffor$forecastingyear,
                     yend = dffor$pointforecast),
                     colour="violetred2",linetype=3,size=0.7) +
              geom_point(data=dfretro,aes(x=years,y=retroactualvalues),colour="dodgerblue3",size=2) +
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
                       y=mean(retroactualvalues),
                       xend=dffor$forecastingyear,
                       yend=mean(retroactualvalues)), colour="grey15") +
              # scale_y_continuous("Terminal Run",labels=comma) +
              scale_y_continuous(paste("Total",stockabundance),labels=comma) +
              scale_x_continuous("Return Year",breaks=c(dfretro$years,dffor$forecastingyear),
                         limits=c(min(dfretro$years)-1, max(dffor$forecastingyear)+4), expand=c(0,0)) +
              geom_text(data=dfretro, aes(x=years[1]+1,
                       y=1.1*max(dfretro$retroactualvalues,dffor$upper),
                       label=paste("Historical Average = ",comma(round(mean(dfretro$retroactualvalues))))),
                   colour="grey15",size=2.5,hjust=0.1,vjust=0.1) +
              ## labs(title=paste("Total",stockabundance)) +
              ggtitle(label=paste("Total",stockabundance)) + 
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




SIMPLESIBREG$scatterplot.forecasted.values.and.forecast.intervals.total.age.simple.sibling.regression(
                     SIMPLESIBREG$pred.int.individual.ages.avgfive.youngest,
                     SIMPLESIBREG$pred.int.individual.ages.arima.youngest,
                     SIMPLESIBREG$pred.int.individual.ages.expsmooth.youngest,
                     SIMPLESIBREG$pred.int.total.age.simple.sibling.regression.all.models,
                     SIMPLESIBREG$best.fits,
                     SIMPLESIBREG$result.avgfive.youngest, 
                     SIMPLESIBREG$result.arima.youngest, 
                     SIMPLESIBREG$result.expsmooth.youngest, 
                     SIMPLESIBREG$forecastingyear, 
                     SIMPLESIBREG$total.index, 
                     SIMPLESIBREG$stockabundance)


##
## Retro Measures for Youngest Age and Older Ages
##

## need to use this!
## best.rmse.youngest.age



SIMPLESIBREG$retro.measures.all.ages.simple.sibling <- function(best.rmse.youngest.age){

	retro <- best.rmse.youngest.age$retro$resjoin

  retro.names <- names(retro)


  MRE <- NULL
  MAE <- NULL
  MPE <- NULL
  MAPE <- NULL
  MASE <- NULL
  RMSE <- NULL
  for (i in 1:length(retro.names)) {

	     retro.individual.age <- retro[[i]]

       result <- retro.individual.age

	     res <- list()

	     
       if ( sum(grepl("a|p|e",names(result)[3:5]))) {
	        a <- as.numeric(result$a)
	        p <- as.numeric(result$p)
	        e <- as.numeric(result$e)
       }

       if ( sum(grepl("Actual|Forecast|Error",names(result)[3:5]))) {
	         a <- as.numeric(result$Actual)
	         p <- as.numeric(result$Forecast)
	         e <- as.numeric(result$Error)
       }


	     p.bench <- as.numeric(result$p.bench)
	     e.bench <- as.numeric(result$e.bench)

	     mre <- mean(e)
       MRE <- c(MRE, mre)

	     mae <- mean(abs(e))
	     MAE <- c(MAE, mae)

	     mpe <- mean(e/a)
	     MPE <- c(MPE, mpe)

	     mape <- mean(abs(e)/a)
       MAPE <- c(MAPE, mape)

	     num_mase <- mean(abs(e))
	     denom_mase <- mean(abs(e.bench))    # e.bench contains retrospective forecast errors
                                              # from naive forecast method (previous year)
	     mase <- num_mase/denom_mase
	     
	     MASE <- c(MASE, mase)
	     
	     rmse <- sqrt(sum(e^2)/length(e))
       RMSE <- c(RMSE, rmse)
   }

   MEASURE.INDIVIDUAL.AGES <- rbind(MRE,
                    MAE,
                    MPE,
                    MAPE,
                    MASE,
                    RMSE)

   colnames(MEASURE.INDIVIDUAL.AGES) <- retro.names
  	
   ## Total Age - Retrospective Measures

   mre.total <- best.rmse.youngest.age$retro$mre.total

   mae.total <- best.rmse.youngest.age$retro$mae.total

   mpe.total <- best.rmse.youngest.age$retro$mpe.total

   mape.total <- best.rmse.youngest.age$retro$mape.total

   mase.total <- best.rmse.youngest.age$retro$mase.total

   rmse.total <- best.rmse.youngest.age$retro$rmse.total

   MEASURE.TOTAL <- c(mre.total, mae.total, mpe.total, mape.total, mase.total, rmse.total)


   MEASURE <- cbind(MEASURE.INDIVIDUAL.AGES, MEASURE.TOTAL)
   
   
   colnames(MEASURE)[length(colnames(MEASURE))] <- "Total"
   
   MEASURE <- round(MEASURE, 2)
   
   M <- cbind.data.frame(rownames(MEASURE),MEASURE)
   
   colnames(M)[1] <- "Measure"
   
   M

}

SIMPLESIBREG$retro.measures.all.ages.simple.sibling(SIMPLESIBREG$best.rmse.youngest.age)




##
## Model Diagnostics for Naive Model (Average of Previous Five Years) for Youngest Age
## 


SIMPLESIBREG$diagnostics.avgfive.model.fit.youngest.age  <- function(avgfive.model.fit.youngest){

    .e <- environment()

    usePackage("forecast")
    usePackage("portes")

    avgfivefit <- avgfive.model.fit.youngest

    avgfivemodel <- avgfivefit$model
    avgfiveresiduals <- residuals(avgfivemodel)

    CY <- avgfivefit$model.data[,"CY"]

    modelavgfive <- "Naive Model (Average of Previous Five Years)"

    Age <- avgfivefit$age

    avgfiveresiduals <- avgfiveresiduals[-c(1,2,3,4,5)]  # first five residuals are in fact missing values
    CY <- CY[-c(1,2,3,4,5)]  # retain only calendar years for which the residuals are NOT missing values


    ## environment=.e

    g1 <- ggplot(data=data.frame(avgfiveresiduals=as.numeric(avgfiveresiduals),CY=CY),
           aes(CY, avgfiveresiduals), environment=.e) +
    		geom_point() +
    		 geom_line() +
    		  geom_hline(aes(yintercept=0), colour="red", linetype="dashed") +
    		   labs(x="Return Year") +
    		    labs(y="Residuals")+
    		     ggtitle(paste("Terminal Run at ", Age, ": ", modelavgfive, sep="")) +
    		     scale_y_continuous(labels=scales::comma) + 
    			theme_bw() +
          		 theme(plot.title=element_text(size=12, hjust=0.5), axis.title.x=element_text(size=10,vjust=-0.5),
                	  axis.title.y=element_text(size=10,vjust=1.5),
                	   axis.text.x=element_text(size=8),
                	    axis.text.y=element_text(size=8),
                	     strip.text.x = element_text(size = 8, colour = "black", angle = 0))

    ## g2: ACF plot
    
    tmp <- Acf(avgfiveresiduals,plot=FALSE)$lag
    lagmax <- max(tmp)+1

    lag <- 1:lagmax
    
    acf <-  Acf(avgfiveresiduals,lag.max=lagmax, plot=FALSE)$acf[-1]
    data.stacked <- data.frame(lag=lag, acf=acf)

    g2 <- ggplot(data.stacked, aes(lag,acf), environment=.e) +
           geom_linerange(aes(x=lag, ymin=0, ymax=acf), colour="red", size=0.3) +
           ## geom_hline(yintercept=2*mean(data.stacked$leverage)) +   ## doesn't work
          # geom_text(aes(label=labels),col="blue",size=3) +
            # coord_fixed(ratio=1) +
              # facet_wrap(~labels, scales="free", ncol = 1) +
                expand_limits(x=1, y=0) +
                  scale_y_continuous(paste("ACF of Residuals"),labels=comma) +
                    scale_x_continuous(paste("Lag"),breaks=0:length(unique(data.stacked$lag)))
                   ## coord_fixed(ratio=1)

      ## print(g)

      g2 = g2 + theme_bw() +
          theme(axis.title.x=element_text(size=10,vjust=-0.5),
                axis.title.y=element_text(size=10,vjust=1.5),
                axis.text.x=element_text(size=8),
                axis.text.y=element_text(size=8),
                strip.text.x = element_text(size = 8, colour = "black", angle = 0))


        acftmp <- Acf(avgfiveresiduals, plot=FALSE)

        ci <- 0.95 # Indicates 95% confidence level
        clim0 <- qnorm((1 + ci)/2)/sqrt(acftmp$n.used)
        clim <- clim0

      g2 = g2 + geom_hline(aes(yintercept=0))
      g2 = g2 + geom_hline(aes(yintercept=clim),linetype="dashed",colour="blue")
      g2 = g2 + geom_hline(aes(yintercept=-clim),linetype="dashed",colour="blue")


    ## g3: PACF plot

    # lag <- acf(avgfiveresiduals,type="partial",plot=FALSE)$lag
    # pacf <-  acf(avgfiveresiduals,type="partial",plot=FALSE)$acf
    # data.stacked <- data.frame(lag=lag, pacf=pacf)
    
    
    tmp <- Acf(avgfiveresiduals,plot=FALSE)$lag
    lagmax <- max(tmp)+1
    lag <- 1:lagmax

    pacf <-  Pacf(avgfiveresiduals,lag.max=lagmax, plot=FALSE)$acf
    

    g3 <- ggplot(data.stacked, aes(lag,pacf), environment=.e) +
           geom_linerange(aes(x=lag, ymin=0, ymax=pacf), colour="red", size=0.3) +
           ## geom_hline(yintercept=2*mean(data.stacked$leverage)) +   ## doesn't work
          # geom_text(aes(label=labels),col="blue",size=3) +
            # coord_fixed(ratio=1) +
              # facet_wrap(~labels, scales="free", ncol = 1) +
                expand_limits(x=1, y=0) +
                  scale_y_continuous(paste("PACF of Residuals"),labels=comma) +
                    scale_x_continuous(paste("Lag"),breaks=0:length(unique(data.stacked$lag)))
                   ## coord_fixed(ratio=1)

      ## print(g)

      g3 = g3 + theme_bw() +
          theme(axis.title.x=element_text(size=10,vjust=-0.5),
                axis.title.y=element_text(size=10,vjust=1.5),
                axis.text.x=element_text(size=8),
                axis.text.y=element_text(size=8),
                strip.text.x = element_text(size = 8, colour = "black", angle = 0))


       acftmp <- Acf(avgfiveresiduals, plot=FALSE)

       ci <- 0.95 # Indicates 95% confidence level
       clim0 <- qnorm((1 + ci)/2)/sqrt(acftmp$n.used)
       clim <- clim0

      g3 = g3 + geom_hline(aes(yintercept=0))
      g3 = g3 + geom_hline(aes(yintercept=clim),linetype="dashed",colour="blue")
      g3 = g3 + geom_hline(aes(yintercept=-clim),linetype="dashed",colour="blue")


    ## g4: Plot of p-values from Ljung-Box test

    ## lags <- acf(avgfiveresiduals,plot=FALSE)$lag[-1]

    tmp <- Acf(avgfiveresiduals,plot=FALSE)$lag
    lagmax <- max(tmp)+1
    lags <- 1:lagmax


    Ljung.Box.Test <- LjungBox(avgfiveresiduals, lags=lags)
    Ljung.Box.Test <- data.frame(lags= Ljung.Box.Test[,"Lags"], pvalue=Ljung.Box.Test[,ncol(Ljung.Box.Test)])


    g4 <- ggplot(data= Ljung.Box.Test, aes(lags, pvalue), environment=.e) +
           geom_point() +
            geom_line()
    g4 <- g4 + geom_hline(aes(yintercept=0.05), linetype="dashed", colour="steelblue")
    g4 <- g4 + labs(x="Lag") + labs(y="P-value for Ljung-Box Test")
    g4 <- g4 + theme_bw() +
          theme(axis.title.x=element_text(size=10,vjust=-0.5),
                axis.title.y=element_text(size=10,vjust=1.5),
                axis.text.x=element_text(size=8),
                axis.text.y=element_text(size=8),
                strip.text.x = element_text(size = 8, colour = "black", angle = 0))
    g4 <- g4 + geom_text(data = NULL, x=Ljung.Box.Test$lags[length(Ljung.Box.Test$lags)-1], y=0.10, label=paste("alpha","= 0.05"),size=2.5)

    ## g <- arrangeGrob(g1, g2, g3, g4, ncol=1)

    g <- grid.arrange(g1, g2, g3, g4, ncol=1)

    g
}


SIMPLESIBREG$diagnostics.avgfive.model.fit.youngest.age(SIMPLESIBREG$avgfive.model.fit.youngest)


##
## Model Diagnostics for ARIMA Model for Youngest Age
## 


SIMPLESIBREG$diagnostics.arima.model.fit.youngest.age  <- function(arima.model.fit.youngest){
    
    .e <- environment()
    
    usePackage("forecast")
    usePackage("portes")
    
    arimafit <- arima.model.fit.youngest
    
    arimamodel <- arimafit$model
    arimaresiduals <- residuals(arimamodel)
    
    CY <- arimafit$model.data[,"CY"]

    sink("arimafit.txt")
    print(arimafit)
    sink()

    out <- readLines("arimafit.txt")
    usePackage("stringr")
    out.pattern <- str_detect(string=out, pattern="ARIMA")

    modelarima <- out[out.pattern==TRUE]
    usePackage("stringr")
    modelarima <- str_trim(modelarima)

    if (boxcoxtransform==TRUE) {
          
          out.lambda <- str_detect(string=out, pattern="lambda")
          
          lambda <- out[out.lambda==TRUE]
          lambda 
          
          modellambda <- str_trim(lambda, side="left")
          modellambda <- str_trim(lambda, side="right")
          modellambda <- str_trim(lambda, side="left")
       
          modelarima <- paste0(modelarima, "; \n ", modellambda)
       
       } 



    Age <- arimafit$age

    g1 <- ggplot(data=data.frame(arimaresiduals=as.numeric(arimaresiduals),CY=CY), 
           aes(CY, arimaresiduals), environment=.e) + 
    		geom_point() + 
    		 geom_line() + 
    		  geom_hline(aes(yintercept=0), colour="red", linetype="dashed") + 
    		   labs(x="Return Year") +
    		    labs(y="Residuals")+
    		     ggtitle(paste("Terminal Run at ", Age, ": ", modelarima, sep="")) + 
    		     scale_y_continuous(labels=scales::comma) + 
    			theme_bw() +
          		 theme(plot.title=element_text(size=12, hjust=0.5), axis.title.x=element_text(size=10,vjust=-0.5),
                	  axis.title.y=element_text(size=10,vjust=1.5),
                	   axis.text.x=element_text(size=8),
                	    axis.text.y=element_text(size=8),
                	     strip.text.x = element_text(size = 8, colour = "black", angle = 0))

    ## g2: ACF plot

    lag <- acf(arimaresiduals,plot=FALSE)$lag[-1]
    acf <-  acf(arimaresiduals,plot=FALSE)$acf[-1]
    data.stacked <- data.frame(lag=lag, acf=acf)

    g2 <- ggplot(data.stacked, aes(lag,acf), environment=.e) +
           geom_linerange(aes(x=lag, ymin=0, ymax=acf), colour="red", size=0.3) +
           ## geom_hline(yintercept=2*mean(data.stacked$leverage)) +   ## doesn't work
          # geom_text(aes(label=labels),col="blue",size=3) +
            # coord_fixed(ratio=1) +
              # facet_wrap(~labels, scales="free", ncol = 1) +
                expand_limits(x=1, y=0) +
                  scale_y_continuous(paste("ACF of Residuals"),labels=comma) +
                    scale_x_continuous(paste("Lag"),breaks=0:length(unique(data.stacked$lag)))
                   ## coord_fixed(ratio=1)

      ## print(g)

      g2 = g2 + theme_bw() +
          theme(axis.title.x=element_text(size=10,vjust=-0.5),
                axis.title.y=element_text(size=10,vjust=1.5),
                axis.text.x=element_text(size=8),
                axis.text.y=element_text(size=8),
                strip.text.x = element_text(size = 8, colour = "black", angle = 0))

      
        acftmp <- acf(arimaresiduals, plot=FALSE)

        ci <- 0.95 # Indicates 95% confidence level
        clim0 <- qnorm((1 + ci)/2)/sqrt(acftmp$n.used)
        clim <- clim0

      g2 = g2 + geom_hline(aes(yintercept=0))
      g2 = g2 + geom_hline(aes(yintercept=clim),linetype="dashed",colour="blue")
      g2 = g2 + geom_hline(aes(yintercept=-clim),linetype="dashed",colour="blue")


    ## g3: PACF plot

    lag <- acf(arimaresiduals,type="partial",plot=FALSE)$lag
    pacf <-  acf(arimaresiduals,type="partial",plot=FALSE)$acf
    data.stacked <- data.frame(lag=lag, pacf=pacf)

    g3 <- ggplot(data.stacked, aes(lag,pacf), environment=.e) +
           geom_linerange(aes(x=lag, ymin=0, ymax=pacf), colour="red", size=0.3) +
           ## geom_hline(yintercept=2*mean(data.stacked$leverage)) +   ## doesn't work
          # geom_text(aes(label=labels),col="blue",size=3) +
            # coord_fixed(ratio=1) +
              # facet_wrap(~labels, scales="free", ncol = 1) +
                expand_limits(x=1, y=0) +
                  scale_y_continuous(paste("PACF of Residuals"),labels=comma) +
                    scale_x_continuous(paste("Lag"),breaks=0:length(unique(data.stacked$lag)))
                   ## coord_fixed(ratio=1)

      ## print(g)

      g3 = g3 + theme_bw() +
          theme(plot.title=element_text(size=12), axis.title.x=element_text(size=10,vjust=-0.5),
                axis.title.y=element_text(size=10,vjust=1.5),
                axis.text.x=element_text(size=8),
                axis.text.y=element_text(size=8),
                strip.text.x = element_text(size = 8, colour = "black", angle = 0))

      
        acftmp <- acf(arimaresiduals, plot=FALSE)

        ci <- 0.95 # Indicates 95% confidence level
        clim0 <- qnorm((1 + ci)/2)/sqrt(acftmp$n.used)
        clim <- clim0

      g3 = g3 + geom_hline(aes(yintercept=0))
      g3 = g3 + geom_hline(aes(yintercept=clim),linetype="dashed",colour="blue")
      g3 = g3 + geom_hline(aes(yintercept=-clim),linetype="dashed",colour="blue")

      
    ## g4: Plot of p-values from Ljung-Box test

    lags <- acf(arimaresiduals,plot=FALSE)$lag[-1]
 
    Ljung.Box.Test <- LjungBox(arimaresiduals, lags=lags)
    Ljung.Box.Test <- data.frame(lags= Ljung.Box.Test[,"Lags"], pvalue=Ljung.Box.Test[,ncol(Ljung.Box.Test)])
   

    g4 <- ggplot(data= Ljung.Box.Test, aes(lags, pvalue), environment=.e) + 
           geom_point() +
            geom_line()
    g4 <- g4 + geom_hline(aes(yintercept=0.05), linetype="dashed", colour="steelblue") 
    g4 <- g4 + labs(x="Lag") + labs(y="P-value for Ljung-Box Test")
    g4 <- g4 + theme_bw() +
          theme(plot.title=element_text(size=12), axis.title.x=element_text(size=10,vjust=-0.5),
                axis.title.y=element_text(size=10,vjust=1.5),
                axis.text.x=element_text(size=8),
                axis.text.y=element_text(size=8),
                strip.text.x = element_text(size = 8, colour = "black", angle = 0))
    g4 <- g4 + geom_text(data = NULL, x=Ljung.Box.Test$lags[length(Ljung.Box.Test$lags)-1], y=0.10, label=paste("alpha","= 0.05"),size=2.5)

    ## g <- arrangeGrob(g1, g2, g3, g4, ncol=1)

    g <- grid.arrange(g1, g2, g3, g4, ncol=1)

    g 
}



SIMPLESIBREG$diagnostics.arima.model.fit.youngest.age(SIMPLESIBREG$arima.model.fit.youngest)



##
## Model Diagnostics for Exponential Smoothing Model for Youngest Age
## 


SIMPLESIBREG$diagnostics.expsmooth.model.fit.youngest.age  <- function(expsmooth.model.fit.youngest){

    .e <- environment()
    
    usePackage("forecast")
    usePackage("portes")
    
    expsmoothfit <- expsmooth.model.fit.youngest
    expsmoothmodel <- expsmoothfit$model
    expsmoothresiduals <- residuals(expsmoothmodel)
    CY <- expsmoothfit$model.data[,"CY"]

    sink("expsmoothfit.txt")
    print(expsmoothfit)
    sink()

    out <- readLines("expsmoothfit.txt")
    usePackage("stringr")
    out.pattern <- str_detect(string=out, pattern="ETS")

    modelexpsmooth <- out[out.pattern==TRUE]
    usePackage("stringr")
    modelexpsmooth <- str_trim(modelexpsmooth)

    if (boxcoxtransform==TRUE) {
          
          out.lambda <- str_detect(string=out, pattern="lambda")
          
          lambda <- out[out.lambda==TRUE]
          lambda <- lambda[2]
          
          modellambda <- str_trim(lambda, side="left")
          modellambda <- str_trim(lambda, side="right")
          modellambda <- str_trim(lambda, side="left")
       
          modelexpsmooth <- paste0(modelexpsmooth, "; \n", modellambda)
       
    } 


    Age <- expsmoothfit$age
    
    CY <- expsmoothfit$model.data[,"CY"]

    ## environment=.e

    g1 <- ggplot(data=data.frame(expsmoothresiduals=as.numeric(expsmoothresiduals),CY=CY),
           aes(CY, expsmoothresiduals),environment=.e) +
    		geom_point() +
    		 geom_line() +
    		  geom_hline(aes(yintercept=0), colour="red", linetype="dashed") +
    		   labs(x="Return Year") +
    		    labs(y="Residuals")+
    		     ggtitle(paste("Terminal Run at ", Age, ": ", modelexpsmooth, sep="")) +
    		     scale_y_continuous(labels=scales::comma) + 
    			theme_bw() +
          		 theme(plot.title=element_text(hjust=0.5),
                    axis.title.x=element_text(size=10,vjust=-0.5),
                	  axis.title.y=element_text(size=10,vjust=1.5),
                	   axis.text.x=element_text(size=8),
                	    axis.text.y=element_text(size=8),
                	     strip.text.x = element_text(size = 8, colour = "black", angle = 0))

    ## g2: ACF plot

    tmp <- Acf(expsmoothresiduals,plot=FALSE)$lag
    lagmax <- max(tmp)+1

    lag <- 1:lagmax

    acf <-  Acf(expsmoothresiduals,lag.max=lagmax, plot=FALSE)$acf[-1]
    data.stacked <- data.frame(lag=lag, acf=acf)

    g2 <- ggplot(data.stacked, aes(lag,acf),environment=.e) +
           geom_linerange(aes(x=lag, ymin=0, ymax=acf), colour="red", size=0.3) +
           ## geom_hline(yintercept=2*mean(data.stacked$leverage)) +   ## doesn't work
          # geom_text(aes(label=labels),col="blue",size=3) +
            # coord_fixed(ratio=1) +
              # facet_wrap(~labels, scales="free", ncol = 1) +
                expand_limits(x=1, y=0) +
                  scale_y_continuous(paste("ACF of Residuals"),labels=comma) +
                    scale_x_continuous(paste("Lag"),breaks=0:length(unique(data.stacked$lag)))
                   ## coord_fixed(ratio=1)

      ## print(g)

      g2 = g2 + theme_bw() +
          theme(axis.title.x=element_text(size=10,vjust=-0.5),
                axis.title.y=element_text(size=10,vjust=1.5),
                axis.text.x=element_text(size=8),
                axis.text.y=element_text(size=8),
                strip.text.x = element_text(size = 8, colour = "black", angle = 0))


        acftmp <- Acf(expsmoothresiduals, plot=FALSE)

        ci <- 0.95 # Indicates 95% confidence level
        clim0 <- qnorm((1 + ci)/2)/sqrt(acftmp$n.used)
        clim <- clim0

      g2 = g2 + geom_hline(aes(yintercept=0))
      g2 = g2 + geom_hline(aes(yintercept=clim),linetype="dashed",colour="blue")
      g2 = g2 + geom_hline(aes(yintercept=-clim),linetype="dashed",colour="blue")


    ## g3: PACF plot

    # lag <- acf(expsmoothresiduals,type="partial",plot=FALSE)$lag
    # pacf <-  acf(expsmoothresiduals,type="partial",plot=FALSE)$acf
    # data.stacked <- data.frame(lag=lag, pacf=pacf)


    tmp <- Acf(expsmoothresiduals,plot=FALSE)$lag
    lagmax <- max(tmp)+1
    lag <- 1:lagmax

    pacf <-  Pacf(expsmoothresiduals,lag.max=lagmax, plot=FALSE)$acf


    g3 <- ggplot(data.stacked, aes(lag,pacf),environment=.e) +
           geom_linerange(aes(x=lag, ymin=0, ymax=pacf), colour="red", size=0.3) +
           ## geom_hline(yintercept=2*mean(data.stacked$leverage)) +   ## doesn't work
          # geom_text(aes(label=labels),col="blue",size=3) +
            # coord_fixed(ratio=1) +
              # facet_wrap(~labels, scales="free", ncol = 1) +
                expand_limits(x=1, y=0) +
                  scale_y_continuous(paste("PACF of Residuals"),labels=comma) +
                    scale_x_continuous(paste("Lag"),breaks=0:length(unique(data.stacked$lag)))
                   ## coord_fixed(ratio=1)

      ## print(g)

      g3 = g3 + theme_bw() +
          theme(axis.title.x=element_text(size=10,vjust=-0.5),
                axis.title.y=element_text(size=10,vjust=1.5),
                axis.text.x=element_text(size=8),
                axis.text.y=element_text(size=8),
                strip.text.x = element_text(size = 8, colour = "black", angle = 0))


       acftmp <- Acf(expsmoothresiduals, plot=FALSE)

       ci <- 0.95 # Indicates 95% confidence level
       clim0 <- qnorm((1 + ci)/2)/sqrt(acftmp$n.used)
       clim <- clim0

      g3 = g3 + geom_hline(aes(yintercept=0))
      g3 = g3 + geom_hline(aes(yintercept=clim),linetype="dashed",colour="blue")
      g3 = g3 + geom_hline(aes(yintercept=-clim),linetype="dashed",colour="blue")


    ## g4: Plot of p-values from Ljung-Box test

    ## lags <- acf(expsmoothresiduals,plot=FALSE)$lag[-1]

    tmp <- Acf(expsmoothresiduals,plot=FALSE)$lag
    lagmax <- max(tmp)+1
    lags <- 1:lagmax


    Ljung.Box.Test <- LjungBox(expsmoothresiduals, lags=lags)
    Ljung.Box.Test <- data.frame(lags= Ljung.Box.Test[,"Lags"], pvalue=Ljung.Box.Test[,ncol(Ljung.Box.Test)])


    g4 <- ggplot(data= Ljung.Box.Test, aes(lags, pvalue), environment=.e) +
           geom_point() +
            geom_line()
    g4 <- g4 + geom_hline(aes(yintercept=0.05), linetype="dashed", colour="steelblue")
    g4 <- g4 + labs(x="Lag") + labs(y="P-value for Ljung-Box Test")
    g4 <- g4 + theme_bw() +
          theme(axis.title.x=element_text(size=10,vjust=-0.5),
                axis.title.y=element_text(size=10,vjust=1.5),
                axis.text.x=element_text(size=8),
                axis.text.y=element_text(size=8),
                strip.text.x = element_text(size = 8, colour = "black", angle = 0))
    g4 <- g4 + geom_text(data = NULL, x=Ljung.Box.Test$lags[length(Ljung.Box.Test$lags)-1], y=0.10, label=paste("alpha","= 0.05"),size=2.5)

    ## g <- arrangeGrob(g1, g2, g3, g4, ncol=1)

    g <- grid.arrange(g1, g2, g3, g4, ncol=1)

    g
}

SIMPLESIBREG$diagnostics.expsmooth.model.fit.youngest.age(SIMPLESIBREG$expsmooth.model.fit.youngest)



## pred.int.individual.ages.avgfive.youngest
## pred.int.individual.ages.arima.youngest
## pred.int.individual.ages.expsmooth.youngest


###
### Histogram of Bootstrap Predictions: Best Model for Youngest Age
###

SIMPLESIBREG$plot.yboot.simple.sibling.regression.youngest.age <- function(pred.int.individual.ages.avgfive.youngest,
                                                               pred.int.individual.ages.arima.youngest,
                                                               pred.int.individual.ages.expsmooth.youngest,
                                                               total.index, stockabundance
                                                               ){

    .e = environment()
    
    
    if (total.index==1) {   # naive model for youngest age (average of previous 5 years)
    
        y.star.boot.stacked <- pred.int.individual.ages.avgfive.youngest$sim
        mylabel <- paste(stockabundance,"at",pred.int.individual.ages.avgfive.youngest$age)
        labels.stacked <-  rep(mylabel, length(y.star.boot.stacked))

        data.stacked <- data.frame(y.star.boot=y.star.boot.stacked, labels=labels.stacked)
                               
    }
    
    if (total.index==2) {   # arima model for youngest age

        y.star.boot.stacked <- pred.int.individual.ages.arima.youngest$sim
        mylabel <- paste(stockabundance,"at",pred.int.individual.ages.arima.youngest$age)
        labels.stacked <-  rep(mylabel, length(y.star.boot.stacked))

        data.stacked <- data.frame(y.star.boot=y.star.boot.stacked, labels=labels.stacked)

    }

    if (total.index==3) {   # exponential smoothing model for youngest age

        y.star.boot.stacked <- pred.int.individual.ages.expsmooth.youngest$sim
        mylabel <- paste(stockabundance,"at",pred.int.individual.ages.expsmooth.youngest$age)
        labels.stacked <-  rep(mylabel, length(y.star.boot.stacked))

        data.stacked <- data.frame(y.star.boot=y.star.boot.stacked, labels=labels.stacked)

    }


    usePackage("ggplot2")
    usePackage("scales")


    d <- data.stacked

    l <- levels(d$labels)

    h <- hist(data.stacked$y.star.boot,plot=FALSE, breaks = "Freedman-Diaconis") 
    h.tmp <- seq( from=min(h$breaks), to=max(h$breaks), by = unique(diff(h$breaks))[1] ) 
    breaks <- h.tmp 
    ## breaks <- h$breaks


    ## g <- ggplot(d, aes(x=residuals), environment=.e) +
     g <- ggplot(d, aes(x=y.star.boot),  environment=.e) +
           geom_histogram(data=d, breaks=breaks, fill="wheat",colour="black") +
             facet_wrap(~ labels,  scales="free", ncol=1) +
               ## expand_limits(x=0, y=0) +
                  scale_y_continuous(paste("Frequency"),labels=comma) +
                   scale_x_continuous(paste("Bootstrapped Point Forecasts"),labels=comma)

      g = g + theme_bw() +
          theme(axis.title.x=element_text(size=10,vjust=-0.5),
                axis.title.y=element_text(size=10,vjust=1.5),
                axis.text.x=element_text(size=8),
                axis.text.y=element_text(size=8),
                strip.text.x = element_text(size = 8, colour = "black", angle = 0))


      if (total.index==1) {   # naive model for youngest age
       
         tmp <- pred.int.individual.ages.avgfive.youngest$PI.ctr
         clim <- round(tmp)
      }

      if (total.index==1) {   # naive model for youngest age  (average of previous five years)

         tmp <- pred.int.individual.ages.avgfive.youngest$PI.ctr
         clim <- round(tmp)
      }

      if (total.index==2) {   # arima model for youngest age

         tmp <- pred.int.individual.ages.arima.youngest$PI.ctr
         clim <- round(tmp)
      }
      
      if (total.index==3) {   # exponential smoothing model for youngest age

         tmp <- pred.int.individual.ages.expsmooth.youngest$PI.ctr
         clim <- round(tmp)
      }


     dummy2 <- data.frame(z = clim, labels = levels(data.stacked$labels))

     g = g + geom_vline(aes(xintercept = z), data=dummy2, linetype="dashed",col="red", size=1)


     if (total.index==1) {   # naive model for youngest age  (average of previous five years)

        x <- max(0,pred.int.individual.ages.avgfive.youngest$PI.lwr)
        xend <-  round(pred.int.individual.ages.avgfive.youngest$PI.upr)
        y <- 0
        yend <- 0

     }

     if (total.index==2) {   # arima model for youngest age

        x <- max(0,pred.int.individual.ages.arima.youngest$PI.lwr)
        xend <-  round(pred.int.individual.ages.arima.youngest$PI.upr)
        y <- 0
        yend <- 0

     }
     
     if (total.index==3) {   # exponential smoothing model for youngest age

        x <- max(0,pred.int.individual.ages.expsmooth.youngest$PI.lwr)
        xend <-  round(pred.int.individual.ages.expsmooth.youngest$PI.upr)
        y <- 0
        yend <- 0

     }


     dummy3 <- data.frame(x=x,xend=xend,y=y,yend=yend, labels = levels(data.stacked$labels))
     g = g + geom_segment(aes(x=x,y=y,xend=xend,yend=yend), data=dummy3, linetype="solid",col="blue", size=1)



     return(g)
}


SIMPLESIBREG$plot.yboot.simple.sibling.regression.youngest.age(SIMPLESIBREG$pred.int.individual.ages.avgfive.youngest,
                                                   SIMPLESIBREG$pred.int.individual.ages.arima.youngest,
                                                   SIMPLESIBREG$pred.int.individual.ages.expsmooth.youngest,
                                                   SIMPLESIBREG$total.index, 
                                                   SIMPLESIBREG$stockabundance)


###
### Histogram of Bootstrap Predictions: Best Model for Total Age
###

SIMPLESIBREG$plot.yboot.simple.sibling.regression.total.age <- function(pred.int.total.age.simple.sibling.regression.all.models,
                                                               total.index, stockabundance){

    .e = environment()


    if (total.index==1) {   # naive model for youngest age (average of previous 5 years)

        y.star.boot.stacked <- pred.int.total.age.simple.sibling.regression.all.models$sim[[total.index]]
        mylabel <- paste("Total", stockabundance)
        labels.stacked <-  rep(mylabel, length(y.star.boot.stacked))

        data.stacked <- data.frame(y.star.boot=y.star.boot.stacked, labels=labels.stacked)

    }

    if (total.index==2) {   # arima model for youngest age

        y.star.boot.stacked <- pred.int.total.age.simple.sibling.regression.all.models$sim[[total.index]]
        mylabel <- paste("Total", stockabundance)
        labels.stacked <-  rep(mylabel, length(y.star.boot.stacked))

        data.stacked <- data.frame(y.star.boot=y.star.boot.stacked, labels=labels.stacked)

    }

    if (total.index==3) {   # exponential smoothing model for youngest age

        y.star.boot.stacked <- pred.int.total.age.simple.sibling.regression.all.models$sim[[total.index]]
        mylabel <- paste("Total", stockabundance)
        labels.stacked <-  rep(mylabel, length(y.star.boot.stacked))

        data.stacked <- data.frame(y.star.boot=y.star.boot.stacked, labels=labels.stacked)

    }


    usePackage("ggplot2")
    usePackage("scales")


    d <- data.stacked

    l <- levels(d$labels)

    h <- hist(data.stacked$y.star.boot, plot=FALSE, breaks = "Freedman-Diaconis")
    h.tmp <- seq(from=min(h$breaks), to=max(h$breaks), by = unique(diff(h$breaks))) 
    ## breaks <- h$breaks
    breaks <- h.tmp

    ## g <- ggplot(d, aes(x=residuals), environment=.e) +
     g <- ggplot(d, aes(x=y.star.boot), environment=.e) +
           geom_histogram(data=d, breaks=breaks, fill="wheat",colour="black") +
             facet_wrap(~ labels,  scales="free", ncol=1) +
               ## expand_limits(x=0, y=0) +
                  scale_y_continuous(paste("Frequency"),labels=comma) +
                   scale_x_continuous(paste("Bootstrapped Point Forecasts"),labels=comma)

      g = g + theme_bw() +
          theme(axis.title.x=element_text(size=10,vjust=-0.5),
                axis.title.y=element_text(size=10,vjust=1.5),
                axis.text.x=element_text(size=8),
                axis.text.y=element_text(size=8),
                strip.text.x = element_text(size = 8, colour = "black", angle = 0))




      if (total.index==1) {   # naive model for youngest age  (average of previous five years)

         tmp <- pred.int.total.age.simple.sibling.regression.all.models$p[[total.index]]
         clim <- round(tmp)
      }

      if (total.index==2) {   # arima model for youngest age

         tmp <- pred.int.total.age.simple.sibling.regression.all.models$p[[total.index]]
         clim <- round(tmp)
      }

      if (total.index==3) {   # exponential smoothing model for youngest age

         tmp <- pred.int.total.age.simple.sibling.regression.all.models$p[[total.index]]
         clim <- round(tmp)
      }


     dummy2 <- data.frame(z = clim, labels = levels(data.stacked$labels))

     g = g + geom_vline(aes(xintercept = z), data=dummy2, linetype="dashed",col="red", size=1)


     if (total.index==1) {   # naive model for youngest age  (average of previous five years)

        x <- max(0,pred.int.total.age.simple.sibling.regression.all.models$p.lwr[[total.index]])
        xend <-  round(pred.int.total.age.simple.sibling.regression.all.models$p.upr[[total.index]])
        y <- 0
        yend <- 0

     }

     if (total.index==2) {   # arima model for youngest age

        x <- max(0,pred.int.total.age.simple.sibling.regression.all.models$p.lwr[[total.index]])
        xend <-  round(pred.int.total.age.simple.sibling.regression.all.models$p.upr[[total.index]])
        y <- 0
        yend <- 0

     }

     if (total.index==3) {   # exponential smoothing model for youngest age

        x <- max(0,pred.int.total.age.simple.sibling.regression.all.models$p.lwr[[total.index]])
        xend <-  round(pred.int.total.age.simple.sibling.regression.all.models$p.upr[[total.index]])
        y <- 0
        yend <- 0

     }


     dummy3 <- data.frame(x=x,xend=xend,y=y,yend=yend, labels = levels(data.stacked$labels))
     g = g + geom_segment(aes(x=x,y=y,xend=xend,yend=yend), data=dummy3, linetype="solid",col="blue", size=1)



     return(g)
}


SIMPLESIBREG$plot.yboot.simple.sibling.regression.total.age(
                         SIMPLESIBREG$pred.int.total.age.simple.sibling.regression.all.models,
                         SIMPLESIBREG$total.index, 
                         SIMPLESIBREG$stockabundance)
                                                
                                                
                                                

# This file is a generated template, your changes will not be overwritten
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import iarm
#' @import ggplot2
#' @importFrom iarm clr_tests
#' @importFrom eRm RM
#' @importFrom eRm PCM
#' @importFrom iarm item_obsexp
#' @importFrom iarm partgam_DIF
#' @importFrom iarm ICCplot
#' @export

clrClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "clrClass",
    inherit = clrBase,
    private = list(

        .init = function() {
            if (is.null(self$data) | is.null(self$options$vars)) {
                self$results$instructions$setVisible(visible = TRUE)
                
            }
            
            self$results$instructions$setContent(
                "<html>
            <head>
            </head>
            <body>
            <div class='instructions'>
            <p>____________________________________________________________________________________</p>
            <p>1. Conditional likelihood ratio tests are estimated by <b>'iarm'</b> R package.</p>
            <p>2. Model='RM' for binary items, or model='PCM' for polytomous items, is used. </p>
            <p>3. Feature requests and bug reports can be made on my <a href='https://github.com/hyunsooseol/snowIRT/issues'  target = '_blank'>GitHub.</a></p>
            <p>____________________________________________________________________________________</p>
            </div>
            </body>
            </html>"
            )
            
            if (self$options$clr)
              self$results$clr$setNote(
                "Note",
                "'Overall' indicates test of homogeneity."
                
              )
            
            
            
            if(isTRUE(self$options$plot)){
              width <- self$options$width
              height <- self$options$height
              self$results$plot$setSize(width, height)
            }
            
            if(isTRUE(self$options$plot1)){
              width <- self$options$width1
              height <- self$options$height1
              self$results$plot1$setSize(width, height)
            }  
            
            if(isTRUE(self$options$plot2)){
              width <- self$options$width2
              height <- self$options$height2
              self$results$plot2$setSize(width, height)
            }  
            
            
            if (length(self$options$vars) <= 1)
                self$setStatus('complete')
            
        },
     #########################################################   
        
                .run = function() {

                  
                    data <- self$data
                    
                    groupVarName <- self$options$group
                    
                    vars <- self$options$vars
                    
                    varNames <- c(groupVarName, vars)
                    
                    model <- self$options$model
                    
                    if (is.null(groupVarName))
                        
                        return()
                    
                    data <- select(self$data, varNames)
                    
                    for (var in vars)
                        
                        data[[var]] <- jmvcore::toNumeric(data[[var]])
                    
                    # exclude rows with missings in the grouping variable
                    
                    data <- data[!is.na(data[[groupVarName]]), ]
                   
                    #############################################################
                    
                    dif <- iarm::clr_tests(dat.items=data[, -1],
                                           dat.exo=data[[groupVarName]],
                                           model=self$options$model)
                    #########################################################
      
                   
                     names <- c("Overall", groupVarName)

                     clr <- as.numeric(dif[,1])
                     df <- as.numeric(dif[,2])
                     pvalue <- as.numeric(dif[,3])

                     res <- data.frame(names,clr,df,pvalue)

                 #    self$results$text$setContent(res)


                    # Creating table-------------

                     table <- self$results$clr


                      for(i in seq_along(1:2)){

                         row <- list()
                         
                         row[["name"]] <- res[i,1]

                         row[["clr"]] <- res[i,2]

                         row[["df"]] <- res[i,3]

                         row[["p"]] <- res[i,4]


                         table$addRow(rowKey = i, values = row)

                     }

                      
                     # Standardized residuals----------------

                     items <- self$options$vars   
                     
                     model <- self$options$model

                     score<- self$options$score


                       if(model=="RM"){


                       rm.mod <- eRm::RM(X=data[, -1])

                       rm<- iarm::item_obsexp(rm.mod)
                       
                    #   self$results$text$setContent(rm)

                     
                         if(score=="low"){

                         sc<- rm[[1]]
                         
                         } 
                       
                       if(score=="high"){
                           
                           sc<- rm[[2]]
                           
                         }

                         obs <-as.numeric(sc[,1])
                         exp <- as.numeric(sc[,2])
                         std <- as.numeric(sc[,3])
                         sig <- as.character(sc[,4])
                         
                         low <- data.frame(obs,exp,std,sig)

                         # Creating low score table-------------

                         table <- self$results$resi


                         for(i in seq_along(items)){

                           row <- list()

                           row[["obs"]] <- low[i,1]

                           row[["exp"]] <- low[i,2]

                           row[["std"]] <- low[i,3]
                           
                           row[["sig"]] <- low[i,4]

                           table$setRow(rowKey = items[i], values = row)

                         }

                         }

     if(model=="PCM"){
       
       
       pc.mod <- PCM(X=data[, -1])
       
       pc<- item_obsexp(pc.mod)
       
     
    #   self$results$text$setContent(pc)
       
       
       if(score=="low"){
         
         sc<- pc[[1]]
         
       } 
       
       if(score=="high"){
         
         sc<- pc[[2]]
         
       }
       
       obs <-as.numeric(sc[,1])
       exp <- as.numeric(sc[,2])
       std <- as.numeric(sc[,3])
       sig <- as.character(sc[,4])
       
       pc <- data.frame(obs,exp,std,sig)
       
       # Creating low score table-------------
       
       table <- self$results$resi
       
       
       for(i in seq_along(items)){
         
         row <- list()
         
         row[["obs"]] <- pc[i,1]
         
         row[["exp"]] <- pc[i,2]
         
         row[["std"]] <- pc[i,3]
         
         row[["sig"]] <- pc[i,4]
         
         table$setRow(rowKey = items[i], values = row)
         
       }
       
       
     }
                     
                     # Partial Gamma to detect Differential Item Functioning (DIF)------
                     
                     gam <- iarm::partgam_DIF(dat.items=data[, -1],dat.exo=data[[groupVarName]])
                     ################################################################
                     
                     gamma <- gam$gamma
                     se <- gam$se
                     p <- gam$pvalue
                     lower <- gam$lower
                     upper <- gam$upper
                     
                     #  gf <- data.frame(gamma,se,p,lower, upper)
                     
                     ################## 
                     
                     items <- self$options$vars
                     
                     table <- self$results$dif
                     
                     for(i in seq_along(items)){
                       
                       row <- list()
                       
                       row[["gamma"]] <- gamma[i]
                       
                       row[["se"]] <- se[i]
                       
                       row[["p"]] <- p[i]
                       
                       row[["lower"]] <- lower[i]
                       
                       row[["upper"]] <- upper[i]
                       
                       table$setRow(rowKey = items[i], values = row)
                       
                     }
                     
                     
                      #  plot----------
                     
                     
                     image <- self$results$plot
                     
                      # image$setState(data[, -1])
                      
                       image$setState(data[, -1])
                     
                     # DIF using total scores---------------
                     
                     image1 <- self$results$plot1
                     
                     state <- list(data[, -1], data[[groupVarName]])
                     
                     image1$setState(state)
                     
                     # DIF using class intervals---------------
                     
                     image2 <- self$results$plot2
                     
                     state <- list(data[, -1], data[[groupVarName]])
                     
                     image2$setState(state)
                     
                     
                     
                },
                     
                     .plot = function(image,...) {     
                     
                       if (is.null(image$state))
                         return(FALSE)
                       
                      data <- image$state
                      num <- self$options$num
                      
                  
                       plot<-  iarm::ICCplot(data=data, 
                                             itemnumber=num, 
                                             method="score"
                                                       )
                      
                       
                       #plot <- plot+ggtheme
                       
                       print(plot)
                       TRUE
                       
                     },
     
     .plot1 = function(image1,ggtheme, theme, ...) {     
       
       
       if (is.null(image1$state))
         return(FALSE)
       
       num <- self$options$num
       
       data <- image1$state[[1]]
       group <- image1$state[[2]] 
       
       
       
       plot1<-  iarm::ICCplot(data= data, 
                             itemnumber= num, 
                             method= "score",
                             icclabel = "yes",
                              dif="yes",
                              difvar=group,
                             difstats = "no")
                             
  
       #  plot1 <- plot1+ggtheme
            
       print(plot1)
       TRUE
       
     },
    
    
    .plot2 = function(image2,ggtheme, theme, ...) {     
      
      
      if (is.null(image2$state))
        return(FALSE)
      
      
      num <- self$options$num
      ci <- self$options$ci
      
      data <- image2$state[[1]]
      group <- image2$state[[2]] 
      
      
      
      plot2<-  iarm::ICCplot(data= data, 
                             itemnumber= num, 
                             method= "cut",
                             cinumber=ci,
                             icclabel = "yes",
                             dif="yes",
                             difvar=group,
                             difstats = "no")
                             
      
    
     # plot2 <- plot2+ggtheme
      
      print(plot2)
      TRUE
      
    }
    

     )
)

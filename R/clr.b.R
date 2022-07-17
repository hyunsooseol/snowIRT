
# This file is a generated template, your changes will not be overwritten
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import iarm
#' @import ggplot2
#' @importFrom iarm clr_tests
#' @importFrom eRm RM
#' @importFrom eRm PCM
#' @importFrom iarm item_obsexp
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
            <p><b>Instructions</b></p>
            <p>____________________________________________________________________________________</p>
            <p>1. Feature requests and bug reports can be made on my <a href='https://github.com/hyunsooseol/snowIRT/issues'  target = '_blank'>GitHub.</a></p>
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
      
                    # table <- self$results$clr
                    # 
                    # table$addRow(rowKey="overall", 
                    #              values=list(clr=clr[1,1], df=df[1,2], p=pvalue[1,3]))
                    # table$addRow(rowKey=groupVarName, 
                    #              values=list(clr=clr[2,1], df=df[2,2], p=pvalue[2,3]))   

                  
                   
                     names <- c("Overall", "Group")

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

                      #  for(name in names){
                      # 
                      #     row <- list()
                      # 
                      #     row[["clr"]] <- res[name,1]
                      # 
                      #     row[["df"]] <- res[name,2]
                      # 
                      #     row[["p"]] <- res[name,3]
                      # 
                      # 
                      #     table$addRow(rowKey = name, values = row)
                      # 
                      # }

                     # Standardized residuals----------------

                     items <- self$options$vars   
                     
                     model <- self$options$model

                     score<- self$options$score


                       if(model=="RM"){


                       rm.mod <- eRm::RM(X=data[, -1])

                       rm<- iarm::item_obsexp(rm.mod)
                       
                       self$results$text$setContent(rm)

                     
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
       
     
       self$results$text$setContent(pc)
       
       
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

                }               
       
     )
)

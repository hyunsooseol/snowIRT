
# This file is a generated template, your changes will not be overwritten
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import iarm
#' @import ggplot2
#' @importFrom iarm clr_tests
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
                    # Example
                    
                    # library(iarm)
                    # 
                    # data("amts")
                    # 
                    # #CLR overall test and test of no DIF for agegrp and sex
                    # dif<- clr_tests(amts[,4:13], amts[,2:3])
                    # 
                    
                    
                   #######################################################
            
                    dif <- iarm:: clr_tests(dat.items = data, 
                                            dat.exo = groupVarName, 
                                            model=self$options$model)
                    ##########################################################
                    
                    
                    # self$results$text$setContent(dif)
                    
                    
                    clr <- as.numeric(dif[,1])
                    df <- as.numeric(dif[,2])
                    pvalue <- as.numeric(dif[,3])
                    
                    res <- data.frame(clr,df,pvalue)
                    
                   # Creating table-------------
                    
                    table <- self$results$clr
                    
                    names <- dimnames(dif)[1]
                    
                     for(name in names){
                        
                        row <- list()
                        
                        row[["clr"]] <- res[name,1]
                        
                        row[["df"]] <- res[name,2]
                        
                        row[["pvalue"]] <- res[name,3]
                        
                       
                        table$addRow(rowKey = name, values = row)
                        
                    }
  
                
                    
        }
     )
)

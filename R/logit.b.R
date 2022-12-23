
# This file is a generated template, your changes will not be overwritten

#' @importFrom R6 R6Class
#' @import jmvcore
#' @import difNLR
#' @import ShinyItemAnalysis
#' @importFrom difNLR difORD
#' @export


logitClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "logitClass",
    inherit = logitBase,
    private = list(

        #=============================================================
        
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
            <p>1. Performs DIF detection procedure for ordinal data based either on adjacent category logit model or on cumulative logit model.</p>
            <P>2. The focal group should be coded as 1.</P>
            <p>3. DIF likelihood ratio statistics are estimated by using <b>difNLR::difORD</b> function.</p>
            <p>4. Feature requests and bug reports can be made on my <a href='https://github.com/hyunsooseol/snowIRT/issues'  target = '_blank'>GitHub.</a></p>
            <p>____________________________________________________________________________________</p>
            </div>
            </body>
            </html>"
            )
            
            if (self$options$method)
                self$results$method$setNote(
                    "Note",
                    "Adj.p = The adjusted p-values by likelihood ratio test using multiple comparison."

                )


            if (length(self$options$vars) <= 1)
                self$setStatus('complete')

        },
        
        #=================================================
        
 .run = function() {

            if (length(self$options$vars)<1) return()
            
            # get variables-------
            
            data <- self$data
            
            groupVarName <- self$options$group
            
            vars <- self$options$vars
            
            varNames <- c(groupVarName, vars)
            
            
            if (is.null(groupVarName))
                
                return()
            
            data <- select(self$data, varNames)
            
            for (var in vars)
                
                data[[var]] <- jmvcore::toNumeric(data[[var]])
            
            # exclude rows with missings in the grouping variable
            
            data <- data[!is.na(data[[groupVarName]]), ]
            
            
            # analysis--------
            
            fit <- difNLR::difORD(
                Data = data, group = groupVarName, focal.name = 1,
                model = self$options$model,
                type = self$options$type, 
                match = self$options$match,
             #   purify = self$options$puri,
                p.adjust.method = self$options$padjust, 
                )
            
        
            if(self$options$puri==TRUE){
                
                fit <- difNLR::difORD(
                    Data = data, group = groupVarName, focal.name = 1,
                    model = self$options$model,
                    type = self$options$type, 
                    match = self$options$match,
                    purify = TRUE,
                    p.adjust.method = self$options$padjust, 
                )
                
                
            }
            
            
                
        #    self$results$text$setContent(fit)
            
            
                chi<- fit$Sval
                p<- fit$pval
                padj <- fit$adj.pval
                
                
             #   res<- data.frame(chi, p)
            #    names <-  dimnames(res)[[1]]
           #     dims <- dimnames(res)[[2]]
               
            
                    table <- self$results$method
                    
                    
                    for (i in seq_along(self$options$vars)) {
                        
                        row <- list()
                        
                        row[["chi"]] <- chi[i]
                        row[["p"]] <- p[i]
                        row[["padj"]] <- padj[i]
                        
                        table$setRow(rowKey = vars[i], values = row)
                
                }
                
            
                    # Normal dif plot--------
                    
                    image <- self$results$plot
                    image$setState(fit)
 },   
 
 
 .plot = function(image, ggtheme, theme,...) {
     
     if (is.null(self$options$vars) | is.null(self$options$group))
         return()
     
   # if (is.null(self$options$group))
   #   
   #   return()
   
   
     num <- self$options$num
     model <- self$options$model
     
     fit <- image$state
     
    # plot <- plot(fit, item=num)
     
     if(self$options$model=="cumulative"){
      plot <- plot(fit, item=num, plot.type="cumulative")
     } else{
       
       plot <- plot(fit, item=num, plot.type="category")
       
     }
      
     print(plot)
     TRUE
 }

  )
)

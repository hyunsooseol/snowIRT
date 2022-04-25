
# This file is a generated template, your changes will not be overwritten
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import difR
#' @import ShinyItemAnalysis
#' @importFrom deltaPlotR deltaPlot
#' @importFrom deltaPlotR diagPlot
#' @export

deltamClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "deltamClass",
    inherit = deltamBase,
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
            <p>1. Each variable should be coded as 0 or 1 with the 'Grouping variable'in jamovi.</p>
            <P>2. The focal group should be coded as 1.</P>
            <p>3. The Raju's Z statistics are estimated by Marginal Maximum Likelihood Estimation(MMLE),and area method is obtained by using the unsigned areas between the ICCs.</p>
            <p>4. Feature requests and bug reports can be made on my <a href='https://github.com/hyunsooseol/snowIRT/issues'  target = '_blank'>GitHub.</a></p>
            <p>____________________________________________________________________________________</p>
            </div>
            </body>
            </html>"
            )
            
            
            
        },
        
        #----------------------------------
        
        
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
                   
                   groupLevels <- base::levels(data[[groupVarName]])
                   
                   if (length(groupLevels) != 2)
                       jmvcore::reject("Grouping variable '{a}' must have exactly 2 levels",
                                       code = "grouping_var_must_have_2_levels",
                                       a = groupVarName)
                   
                   if(self$options$fixed==TRUE){
                   
                   #--------Delta method-------------------------------------------
                   
                   # delta scores with fixed threshold---------
                   fixed <- deltaPlotR::deltaPlot(
                       data = data, group = groupVarName, focal.name = 1,
                       thr = 1.5, purify = FALSE)
                   
                   #---------------------------------------
                   
                #   self$results$text$setContent(fixed)
                   
                   
                   
                   df<- data.frame(fixed$Props,fixed$Deltas, fixed$Dist)


                   table <- self$results$fixed

                   items <- self$options$vars

                   pr<- df$X1
                   pf<-df$X2
                   deltar<-df$X1.1
                   deltaf<-df$X2.1
                   dist<-df$fixed.Dist



                   for (i in seq_along(items)) {

                       row <- list()

                       row[["pr"]] <- pr[i]

                       row[["pf"]] <- pf[i]

                       row[["dr"]] <- deltar[i]

                       row[["df"]] <- deltaf[i]

                       row[["dist"]] <- dist[i]

                       table$setRow(rowKey = items[i], values = row)
                   }
                   
                   # DIF ITEMS---------
                   
                   fixed.dif<- fixed$DIFitems
                   
                   self$results$text$setContent(fixed.dif)
                   
                   # Fixed dif plot--------
                   
                   image <- self$results$plot
                   image$setState(fixed)
                   
                   
                   
                   }
                   
                   
                   if(self$options$normal==TRUE){
                   
                   # delta scores with normal threshold
                   
                   puri <- self$options$puri
                   
                   #----------------------
                   normal <- deltaPlotR::deltaPlot(
                       data = data, group = groupVarName, focal.name = 1,
                        thr = "norm",  purify = TRUE, purType = puri)
                   
                #--------------------------------
                   
                   dist<- normal$Dist
                   dist <- as.data.frame(dist)
                   
                   names<- dimnames(dist)[[1]] #variables
                   dims <- dimnames(dist)[[2]] # add column
                  
                   table1 <- self$results$normal
                   
                   
                   for (dim in dims) {
                       
                       table1$addColumn(name = paste0(dim),
                                       type = 'number')
                   }
                   
                   
                   for (name in names) {
                       
                       row <- list()
                       
                       for(j in seq_along(dims)){
                           
                           row[[dims[j]]] <- dist[name,j]
                           
                       }
                       
                       table1$addRow(rowKey=name, values=row)
                       
                   }
                   
                   
                   }
                   
               },
                   .plot = function(image, ...) {
                       
                       # if (is.null(self$options$facs))
                       #     return()
                       
                       
                       fixed <- image$state
                       
                       plot <- deltaPlotR::diagPlot(fixed, thr.draw = TRUE)
                       
                       print(plot)
                       TRUE
                   }
                   
                   
                   
                   
                   
)
)

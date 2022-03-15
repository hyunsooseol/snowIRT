
# This file is a generated template, your changes will not be overwritten

# Polytomous Rasch model
#' @importFrom R6 R6Class
#' @import jmvcore
#' @importFrom ShinyItemAnalysis DistractorAnalysis
#' @importFrom ShinyItemAnalysis plotDistractorAnalysis
#' @importFrom CTT distractor.analysis
#' @export


itemClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "itemClass",
    inherit = itemBase,
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
            <p> </p>
            <p> Feature requests and bug reports can be made on the <a href='https://github.com/hyunsooseol/seolmatrix/issues'  target = '_blank'>GitHub.</a></p>
            <p>____________________________________________________________________________________</p>
            </div>
            </body>
            </html>"
            )
            
        },
        
        
         .run = function() {
             
             if (length(self$options$vars)<1) return()
             
             
             #get the data--------
             
             data <- self$data
             
             
           #  data[[vars]] <- jmvcore::toNumeric(data[[vars]])
             
             data <- na.omit(data)
 
             # vars <- self$options$vars
             
             
             key <- self$options$key
             key1 <- strsplit(self$options$key, ',')[[1]]
             
             
             # Counts of respondents-----------
              
              counts<- CTT::distractor.analysis(data,key1)
                                                         
              
              self$results$count$setContent(counts)
              
             # Proportions of respondents--------
              
              prop<- CTT::distractor.analysis(data,key1, p.table=TRUE)
              
              
              self$results$prop$setContent(prop)
             

  # #  # plot----------
  # # 
  #  image <- self$results$plot
  #  image$setState(prop)

          },


 .plot = function(image, ...) {


     data <- self$data
     data <- na.omit(data)

    
     key <- self$options$key
     key1 <- strsplit(self$options$key, ',')[[1]]
     
     nums<- self$options$num
     
     plot <- ShinyItemAnalysis::plotDistractorAnalysis(data, key1, 
                                                       item = nums)


 print(plot)
 TRUE
}

       )
)

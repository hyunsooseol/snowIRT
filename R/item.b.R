
# This file is a generated template, your changes will not be overwritten

# Polytomous Rasch model
#' @importFrom R6 R6Class
#' @import jmvcore
#' @importFrom ShinyItemAnalysis DistractorAnalysis
#' @importFrom ShinyItemAnalysis plotDistractorAnalysis
#' @importFrom CTT distractor.analysis
#' @importFrom CTT distractorAnalysis
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
            <P>1. Enter the correct answer separated by commas, but there must be no spaces between commas.
            <p>2. Feature requests and bug reports can be made on the <a href='https://github.com/hyunsooseol/snowIRT/issues'  target = '_blank'>GitHub.</a></p>
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
 
             
             key <- self$options$key
             key1 <- strsplit(self$options$key, ',')[[1]]
             
             
             # Counts of respondents-----------
              
              counts<- CTT::distractor.analysis(data,key1)
                                                         
              
              # self$results$count$setContent(counts)
              
             vars <- self$options$vars
             
             table <- self$results$count
              
             tab <- NULL
             
             for(i in seq_along(vars)){
               
               tab[[i]]<- as.data.frame.matrix(counts[[i]]) 
              
               
             }
             
             tab <- tab
              
           
             for(i in seq_along(vars)){
               
               table <- self$results$count[[i]]
               names <- dimnames(tab[[i]])[[1]]
               dims <- dimnames(tab[[i]])[[2]]
               
               for (name in names) {
                 row <- list()
                 
                 for(j in seq_along(dims)){       
                   row[[dims[j]]] <- tab[[i]][name,j]
                 }
                 
                 table$addRow(rowKey=name, values=row)
               }
             }
             
             
               
          
              
              # Proportions of respondents--------
              
            prop<- CTT::distractor.analysis(data,key1, p.table=TRUE)
              
             
            
             # self$results$prop$setContent(prop)
             
             table <- self$results$prop
             
             tab <- NULL
             
             for(i in seq_along(vars)){
               
               tab[[i]]<- as.data.frame.matrix(prop[[i]]) 
               
               
             }
             
             tab <- tab
             
             
             for(i in seq_along(vars)){
               
               table <- self$results$prop[[i]]
               names <- dimnames(tab[[i]])[[1]]
               dims <- dimnames(tab[[i]])[[2]]
               
               for (name in names) {
                 row <- list()
                 
                 for(j in seq_along(dims)){       
                   row[[dims[j]]] <- tab[[i]][name,j]
                 }
                 
                 table$addRow(rowKey=name, values=row)
               }
             }
             
      
             
             #summary-------------
             
             res<- CTT::distractorAnalysis(data,key1) 
             
             
              table <- self$results$sum
              
              tab <- NULL
              
              for(i in seq_along(vars)){
                
                tab[[i]]<- as.data.frame.matrix(res[[i]][1:4]) 
                
                
              }
              
              tab <- tab
              
              
              for(i in seq_along(vars)){
                
                table <- self$results$sum[[i]]
                names <- dimnames(tab[[i]])[[1]]
                dims <- dimnames(tab[[i]])[[2]]
                
                for (name in names) {
                  row <- list()
                  
                  for(j in seq_along(dims)){       
                    row[[dims[j]]] <- tab[[i]][name,j]
                  }
                  
                  table$addRow(rowKey=name, values=row)
                }
              }
              
              
        
             # plot----------
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

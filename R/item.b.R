
# This file is a generated template, your changes will not be overwritten

# Polytomous Rasch model
#' @importFrom R6 R6Class
#' @import jmvcore
#' @importFrom ShinyItemAnalysis DistractorAnalysis
#' @importFrom ShinyItemAnalysis plotDistractorAnalysis
#' @importFrom CTT distractor.analysis
#' @importFrom CTT distractorAnalysis
#' @importFrom CTT score
#' @importFrom ShinyItemAnalysis ItemAnalysis
#' @importFrom ShinyItemAnalysis DDplot
#' @importFrom ShinyItemAnalysis theme_app
#' @import ggplot2
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
            <P>2. By a rule of thumb, all items with a discrimination lower than 0.2 (threshold in the plot), should be checked for content.
            <p>3. Feature requests and bug reports can be made on the <a href='https://github.com/hyunsooseol/snowIRT/issues'  target = '_blank'>GitHub.</a></p>
            <p>____________________________________________________________________________________</p>
            </div>
            </body>
            </html>"
            )
           
            if (self$options$disc)
              self$results$disc$setNote(
                "Note",
                "ULI:Upper-Lower Index, RIT:Item-Total correlation, RIR: Item-Rest correlation."
                
                
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
              
          
              # Scores, scored file-----------
              
              dicho<- CTT::score(data,key1,output.scored=TRUE)
              dicho<- dicho$scored
              dicho <- as.data.frame(dicho) # dichotomous matrix
              
              
             if(self$options$total==TRUE){
             
               binary<- CTT::score(data,key1,output.scored=TRUE)
             
                # Save total score---------
                score<- as.vector(binary$score)
              
              self$results$total$setRowNums(rownames(data))     
               self$results$total$setValues(score)
               
             }
                
            
              # Save scoring-----------------
              
              if(self$options$scoring==TRUE){
                
                binary<- CTT::score(data,key1,output.scored=TRUE)
                
               
                keys <- 1:length(self$options$vars)
                titles <- paste("Item", 1:length(self$options$vars))
                descriptions <- paste("Item", 1:length(self$options$vars))
                measureTypes <- rep("continuous", length(self$options$vars))
                
                self$results$scoring$set(
                  keys=keys,
                  titles=titles,
                  descriptions=descriptions,
                  measureTypes=measureTypes
                )
                
                self$results$scoring$setRowNums(rownames(data))
                
                
                scoring<- as.data.frame(binary$scored)
                
                for (i in 1:length(self$options$vars)) {
                  scores <- as.numeric(scoring[, i])
                  self$results$scoring$setValues(index=i, scores)
                }
                
              } 
               
              
              # traditional item analysis table--------------------
              
              it<- ShinyItemAnalysis::ItemAnalysis(dicho)
              
              dif <- it[1]
              ULI <- it[10]
              RIT <- it[11]
              RIR <- it[12]
              
              discri <- data.frame(dif, ULI, RIT, RIR)
              
              
              dif <- discri$Difficulty
              ULI<- discri$ULI
              RIT<- discri$RIT
              RIR<- discri$RIR
              
              
              # self$results$text$setContent(dis)
              
              disc <- self$options$disc
              
              table <- self$results$disc
              
              vars <- self$options$vars
              
            
              
              for (i in seq_along(vars)) {
                
                row <- list()
                
                row[["dif"]] <-  dif[i]
                row[["ULI"]] <-  ULI[i]
                row[["RIT"]] <-  RIT[i]
                row[["RIR"]] <-  RIR[i]
                
                table$setRow(rowKey=vars[i], values=row)
                
              }
              
              
            #  plot1----------
              
              disi<- self$options$disi
             
             image <- self$results$plot1
     
             image$setState(dicho)
             
             # Histogram of total score------------
            
            if(self$options$plot2==TRUE){ 
             
              dicho<- CTT::score(data,key1,output.scored=TRUE)
              
              binary<- dicho$scored
              
             score <- rowSums(binary)
             df <- data.frame(score)
             
             state <- list(score, df)
             
             image2 <- self$results$plot2
             image2$setState(state)
            }
           
          },
#################################################################

        .plot2 = function(image2, ggtheme, theme,...) {
          
          if (is.null(image2$state))
            return(FALSE)
          
          score <- image2$state[[1]]
          df <- image2$state[[2]]
          
          # plot2<- ggplot(df, aes(score)) +
          #   geom_histogram(binwidth = 1) +
          #   xlab("Total score") +
          #   ylab("Number of respondents")
        
          # histogram
          ggplot(df, aes(score)) +
            geom_histogram(binwidth = 1, col = "black") +
            xlab("Total score") +
            ylab("Number of respondents") +
            ShinyItemAnalysis::theme_app()
          
          # colors by cut-score
          cut <- median(score) # cut-score
          color <- c(rep("red", cut - min(score)), "gray", rep("blue", max(score) - cut))
          df <- data.frame(score)
          
          # histogram
          plot2<- ggplot(df, aes(score)) +
            geom_histogram(binwidth = 1, fill = color, col = "black") +
            xlab("Total score") +
            ylab("Number of respondents") +
            ShinyItemAnalysis::theme_app()
          
          
          plot2+ggtheme
          
          print(plot2)
          TRUE
        },
        
        
 .plot = function(image, ...) {


     data <- self$data
     data <- na.omit(data)

    
     key <- self$options$key
     key1 <- strsplit(self$options$key, ',')[[1]]
     
     nums<- self$options$num
     group <- self$options$group
     
     plot <- ShinyItemAnalysis::plotDistractorAnalysis(data, key1, 
                                                       num.groups = group,
                                                       item = nums)


 print(plot)
 TRUE
},

.plot1 = function(image, ggtheme, theme,...) {
  
  if (is.null(image$state))
    return(FALSE)
  
  dicho <- image$state
  
  disi<- self$options$disi
  
  plot1 <- ShinyItemAnalysis::DDplot(dicho, discrim = disi, k = 3, l = 1, u = 3)
   
  
  if (self$options$angle > 0) {
    plot1 <- plot1 + ggplot2::theme(
      axis.text.x = ggplot2::element_text(
        angle = self$options$angle, hjust = 1
      )
    )
  }
   
  
  print(plot1)
  TRUE
}


       )
)

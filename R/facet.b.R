
# This file is a generated template, your changes will not be overwritten

#' @importFrom R6 R6Class
#' @import jmvcore
#' @importFrom jmvcore toB64
#' @importFrom TAM tam.mml.mfr
#' @importFrom TAM tam.personfit
#' @importFrom TAM tam.wle
#' @importFrom TAM tam.threshold
#' @importFrom TAM msq.itemfit
#' @importFrom TAM tam.wle
#' @import ggplot2
#' @export


facetClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "facetClass",
    inherit = facetBase,
    private = list(

      .init = function() {
        if (is.null(self$data) | is.null(self$options$facet)) {
          self$results$instructions$setVisible(visible = TRUE)
          
        }
        
        self$results$instructions$setContent(
          "<html>
            <head>
            </head>
            <body>
            <div class='instructions'>
            <p>____________________________________________________________________________________</p>
            <p>1. Note that Polytomous model needs <b>the bottom category to be coded as 0.</b>
            <p>2. <b>Person Analysis</b> will be displayed in the datasheet.</p>
            <p>3. The result tables are estimated by Marginal Maximum likelihood Estimation(MMLE).</p>
            <p>4. The <b>eRm</b> R package was used for the person-item map for PCM.</p>
            <p>5. The rationale of snowIRT module is described in the <a href='https://bookdown.org/dkatz/Rasch_Biome/' target = '_blank'>documentation</a>.</p>
            <p>6. Feature requests and bug reports can be made on my <a href='https://github.com/hyunsooseol/snowIRT/issues'  target = '_blank'>GitHub</a>.</p>
            <p>____________________________________________________________________________________</p>
            </div>
            </body>
            </html>"
        )
        
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
        
      },
      
      
      .run = function() {

        # example with dataset (facet1.csv) ----------
        # facet1<- read.csv("facet1.csv")
        # attach(facet1)
        # facets = dplyr::select(facet1, raters:trait)
        # formula <- ~ trait + raters +step
        # res <- TAM::tam.mml.mfr(score,
        #                         facets =  facets,
        #                         formulaA = formula,
        #                         pid=subjects)
        
        #https://rpubs.com/isbell_daniel/735520
        
        if (is.null(self$options$dep) ||
            is.null(self$options$id) ||
            is.null(self$options$facet)) return()
        
        
        
        dep <- self$options$dep
        id <- self$options$id
        facets <- self$options$facet
        
        data <- self$data
        data <- na.omit(data)
        data <- as.data.frame(data)
        
        # Formula---------------
        
        facets <- vapply(facets, function(x) jmvcore::composeTerm(x), '')
        facets <- paste0(facets, collapse='*')
        formula <- as.formula(paste0('~ step+', facets))
        
        
        facets = dplyr::select(data, self$options$facet)
        
        #self$results$text$setContent(formula)
        
        
        res <- TAM::tam.mml.mfr(resp = data[[self$options$dep]],
                                facets = facets, 
                                pid = data[[self$options$id]],
                                formulaA = formula)
        
        
      #  self$results$text$setContent(res$xsi.facets)
        
         # Facet estimates----------
         
         facet.estimates <- res$xsi.facets # Whole estimates
         
         im <- subset(facet.estimates, facet.estimates$facet == "task")
         
       #  self$results$text$setContent(im)
      
         # rater measure----------   
       rm <- subset(facet.estimates, facet.estimates$facet == "rater")
      
       # interaction-------
       inter <- subset(facet.estimates, facet.estimates$facet == "rater:task")
       
         inter<- inter |> tidyr::separate(parameter, c("rater", "task"), ":")
         inter$task <-  gsub("task", "", inter$task) 
         inter <- data.frame(inter$rater, inter$task, inter$xsi, inter$se.xsi)
         colnames(inter) <- c("Rater", "Task","Measure","SE")
         
         
       # step measure-----------
         
        sm <- subset(facet.estimates, facet.estimates$facet == "step")
         
         # Task measure table----------------
           
           table<- self$results$im
          
           im<- as.data.frame(im)
           dif<- as.vector(im[[3]])
           se<- as.vector(im[[4]])
           
           items <- as.vector(im[[1]])
          
           for (i in seq_along(items)) {
             
             row <- list()
             
             row[["measure"]] <-dif[i]
             
             row[["se"]] <- se[i]
             
             table$addRow(rowKey = items[i], values = row)
           }
           
           # Item bar plot----------
           
           if(isTRUE(self$options$plot2)){
             
             im <- as.data.frame(im)
             colnames(im) <- c("Task", "facet", "Value", "SE")
             
             # Rater bar plot--------
             image <- self$results$plot2
             image$setState(im)
             
           }
           
           # Rater measure table----------------
           
           table<- self$results$rm
           
           rm<- as.data.frame(rm)
           
           dif<- as.vector(rm[[3]])
           se<- as.vector(rm[[4]])
           
           items <- as.vector(rm[[1]])
           
           for (i in seq_along(items)) {
             
             row <- list()
             
             row[["measure"]] <-dif[i]
             
             row[["se"]] <- se[i]
             
             table$addRow(rowKey = items[i], values = row)
           }
           
           
           # Rater bar plot----------
           
           if(isTRUE(self$options$plot1)){
           
             rm<- as.data.frame(rm)
             colnames(rm) <- c("Rater", "facet", "Value", "SE")
             
            # Rater bar plot--------
           image <- self$results$plot1
           image$setState(rm)
           
           }
           
          
           # Interaction measure table----------------
           
           table<- self$results$inter
           
           inter<- as.data.frame(inter)
           
           names <- dimnames(inter)[[1]]
           
           rater <- as.vector(inter[[1]])
           task <- as.vector(inter[[2]])
           dif<- as.vector(inter[[3]])
           se<- as.vector(inter[[4]])
           
           items <- as.vector(inter[[1]])
           
           # for (i in seq_along(items)) {
           #   
           #   row <- list()
           #   
           #   row[["task"]] <- task[i]
           #   row[["measure"]] <-dif[i]
           #   row[["se"]] <- se[i]
           #   
           #   table$addRow(rowKey = items[i], values = row)
           # }
           
           for (name in names) {
             
             row <- list()
             
             row[["rater"]]   <-  inter[name, 1]
             row[["task"]]   <-  inter[name, 2]
             row[["measure"]] <-  inter[name, 3]
             row[["se"]] <-  inter[name, 4]
             
             table$addRow(rowKey=name, values=row)
             
           }
           
           
           
           
           
           
           
           
           # Step measure table----------------
           
           table<- self$results$sm
           
           sm<- as.data.frame(sm)
           
           dif<- as.vector(sm[[3]])
           se<- as.vector(sm[[4]])
           
           items <- as.vector(sm[[1]])
           
           for (i in seq_along(items)) {
             
             row <- list()
             
             row[["measure"]] <-dif[i]
             
             row[["se"]] <- se[i]
             
             table$addRow(rowKey = items[i], values = row)
           }
           
           
            # item fit statistics------------
            ## fit is shown for the rater*item combinations

              ifit <- TAM::msq.itemfit(res)

           # self$results$text$setContent(ifit)

            # Item fit table------------

            table <- self$results$ifit

            ifit <- as.data.frame(ifit$itemfit)

            outfit.t<- as.vector(ifit[4])
            outfit<- outfit.t$Outfit
            p <- as.vector(ifit[5])
            p<- p$Outfit_p

            items<- as.vector(ifit[[1]])

            for (i in seq_along(items)) {

              row <- list()

              row[["outfit"]] <-outfit[i]

              row[["p"]] <- p[i]

              table$addRow(rowKey = items[i], values = row)
            }

           # Person ability----------
            persons <- TAM::tam.wle(res)
            
            per <-data.frame(persons$pid, persons$PersonScores,
                             persons$theta, persons$error,
                             persons$WLE.rel) 
            
            
            # WLE Reliability-------
            
            pw<- as.vector(per[[5]])[1]
            self$results$text$setContent(pw)
            
            
            # Person measure table-------------
            
            table <- self$results$pm
            
            # ps<- as.vector(per[[2]])
            # pt<- as.vector(per[[3]])
            # pe<- as.vector(per[[4]])
            # pw<- as.vector(per[[5]])
            # 
            # self$results$text$setContent(pw)
            # 
            # items <- as.vector(per[[1]])
            # 
            # for (i in seq_along(items)) {
            #   
            #   row <- list()
            #   
            #   row[["ps"]] <- ps[i]
            #   row[["pt"]] <- pt[i]
            #   row[["pe"]] <- pe[i]
            #   row[["pw"]] <- pw[i]
            #   
            #   table$addRow(rowKey = items[i], values = row)
            # }
            # 
            
            names<- dimnames(per)[[1]]
            
            for (name in names) {
              
              row <- list()
              
              row[["ps"]]   <-  per[name, 2]
              row[["pt"]] <-  per[name, 3]
              row[["pe"]] <-  per[name, 4]
           
              table$addRow(rowKey=name, values=row)
              
            }
            
           # Person fit table-----------
            
            pfit <- TAM::tam.personfit(res)
            
            pfit <- data.frame(pfit$outfitPerson,
                               pfit$infitPerson)
            
            table <- self$results$pfit
            
            names<- dimnames(pfit)[[1]]
            
            for (name in names) {
              
              row <- list()
              
              row[["outfit"]]   <-  pfit[name, 1]
              row[["infit"]] <-  pfit[name, 2]
             
              
              table$addRow(rowKey=name, values=row)
              
            }
      },
      
      .plot1 = function(image, ggtheme, theme,...) {
        
        if (is.null(image$state))
          return(FALSE)
        
        rm <- image$state
        
         fill <- theme$fill[2]
         color <- theme$color[1]
        
        plot1 <- ggplot(data=rm, aes(x=Rater, y=Value)) +
        
          geom_bar(
            stat="identity",
           # position="dodge",
            width = 0.7,
             fill=fill,
             color=color
          ) +  theme_bw() + coord_flip()
    
        # 
        # if (self$options$angle > 0) {
        #   plot1 <- plot1 + ggplot2::theme(
        #     axis.text.x = ggplot2::element_text(
        #       angle = self$options$angle, hjust = 1
        #     )
        #   )
        # }
       
        plot1+ggtheme 
        
        print(plot1)
        TRUE
        
      },
      
      .plot2 = function(image, ggtheme, theme,...) {
        
        if (is.null(image$state))
          return(FALSE)
        
        im <- image$state
        
        fill <- theme$fill[2]
        color <- theme$color[1]
        
        plot2 <- ggplot(data=im, aes(x=Task, y=Value)) +
          
          geom_bar(
            stat="identity",
            #position="dodge",
            width = 0.7,
            fill=fill,
            color=color
          ) +  theme_bw()+ coord_flip()
        
        
        # if (self$options$angle > 0) {
        #   plot2 <- plot2 + ggplot2::theme(
        #     axis.text.x = ggplot2::element_text(
        #       angle = self$options$angle, hjust = 1
        #     )
        #   )
        # }
        
        plot2+ggtheme 
        
        print(plot2)
        TRUE
        
      }
      
      
      
             
        )
)

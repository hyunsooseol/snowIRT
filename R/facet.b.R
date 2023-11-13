
# This file is a generated template, your changes will not be overwritten

#' @importFrom R6 R6Class
#' @import jmvcore
#' @importFrom jmvcore toB64
#' @importFrom TAM tam.mml.mfr
#' @importFrom TAM tam.personfit
#' @importFrom TAM tam.wle
#' @importFrom TAM tam.threshold
#' @importFrom TAM msq.itemfit
#' @import ggplot2
#' @export


facetClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "facetClass",
    inherit = facetBase,
    private = list(

      .init = function() {
        if (is.null(self$data) | is.null(self$options$dep)) {
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
        
        
           #self$results$text$setContent(res$xsi.facets)
          
           # Facet table----------------
           
           table<- self$results$fm
          
           im<- res$xsi.facets
           im<- as.data.frame(im)
           
           items <- as.vector(im[[1]])
          
         
            dif<- as.vector(im[[3]])
            se<- as.vector(im[[4]])
         
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
            outfit.t<- outfit.t$Outfit
            p <- as.vector(ifit[5])
            p<- p$Outfit_p
            
            items<- as.vector(ifit[[1]])
            
            for (i in seq_along(items)) {
              
              row <- list()
              
              row[["outfit.t"]] <-outfit.t[i]
              
              row[["p"]] <- p[i]
              
              table$addRow(rowKey = items[i], values = row)
            }
            
            
            
            
        })
)

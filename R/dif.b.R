

difClass <- if (requireNamespace('jmvcore'))
  R6::R6Class(
    "difClass",
    inherit = difBase,
    private = list(
      .htmlwidget = NULL,
      #=============================================================
      
      .init = function() {
        private$.htmlwidget <- HTMLWidget$new()
        
        if (is.null(self$data) | is.null(self$options$vars)) {
          self$results$instructions$setVisible(visible = TRUE)
          
        }
        
        self$results$instructions$setContent(private$.htmlwidget$generate_accordion(
          title = "Instructions",
          content = paste(
            '<div style="border: 2px solid #e6f4fe; border-radius: 15px; padding: 15px; background-color: #e6f4fe; margin-top: 10px;">',
            '<div style="text-align:justify;">',
            '<ul>',
            '<li>Performs DIF detection using <b>difR</b> R package.</li>',
            '<li>For Raju and MH method, the focal group should be coded as <b>1</b>.</li>',
            '<li>Feature requests and bug reports can be made on my <a href="https://github.com/hyunsooseol/snowIRT/issues" target="_blank">GitHub</a>.</li>',
            '</ul></div></div>'
            
          )
          
        ))
        
        if (self$options$raju)
          self$results$raju$setNote(
            "Note",
            "1. Effect size(ETS Delta scale) for absolute values of 'deltaRaju' = A: negligible effect(<1.0), B: moderate effect(>1.0),C: large effect(>1.5)."
            
          )
        
        if (isTRUE(self$options$zplot)) {
          width <- self$options$width1
          height <- self$options$height1
          self$results$zplot$setSize(width, height)
        }
        if (isTRUE(self$options$plot3)) {
          width <- self$options$width2
          height <- self$options$height2
          self$results$plot3$setSize(width, height)
        }
        
        if (isTRUE(self$options$plot1)) {
          width <- self$options$width3
          height <- self$options$height3
          self$results$plot1$setSize(width, height)
        }
        
        if (isTRUE(self$options$plot2)) {
          width <- self$options$width4
          height <- self$options$height4
          self$results$plot2$setSize(width, height)
        }
        if (length(self$options$vars) <= 1)
          self$setStatus('complete')
      },
      
      #=================================================
      .run = function() {
        data <- self$data
        groupVarName <- self$options$group
        vars <- self$options$vars
        varNames <- c(groupVarName, vars)
        padjust <- self$options$padjust
        padjust1 <- self$options$padjust1
        padjust2 <- self$options$padjust2
        
        #--------------------------------------------
        
        if (is.null(groupVarName))
          return()
        data <- dplyr::select(self$data, varNames)
        
        for (var in vars)
          
          data[[var]] <- jmvcore::toNumeric(data[[var]])
        
        # exclude rows with missings in the grouping variable
        data <- data[!is.na(data[[groupVarName]]), ]
        groupLevels <- base::levels(data[[groupVarName]])
        
        # Generalized MH method--------------
        if (length(groupLevels) > 2) {
          if (isTRUE(self$options$gmh | self$options$plot2)) {
            fn <- as.numeric(strsplit(self$options$fn, ',')[[1]])
            
            gmh <- difR::difGMH(data,
                                groupVarName,
                                focal.names = fn,
                                p.adjust.method = padjust2)
            if (is.null(self$options$group))
              return()
            table <- self$results$gmh
            items <- self$options$vars
            
            # get result---
            
            gmhstat <- as.vector(gmh$GMH)
            p <- as.vector(gmh$p.value)
            padjust <- as.vector(gmh$adjusted.p)
            
            
            for (i in seq_along(items)) {
              row <- list()
              row[["gmhstat"]] <- gmhstat[i]
              row[["p"]] <- p[i]
              row[["padjust"]] <- padjust[i]
              table$setRow(rowKey = items[i], values = row)
            }
            
            # GMH Plot -------
            image2 <- self$results$plot2
            image2$setState(gmh)
          }
        }
        
        else{
          if (length(groupLevels) > 2)
            return()
          
          # if (length(groupLevels) != 2)
          #   jmvcore::reject("Grouping variable '{a}' must have exactly 2 levels",
          #                   code = "grouping_var_must_have_2_levels",
          #                   a = groupVarName)
          #
          
          ref = dplyr::filter(data, data[[groupVarName]] == 0)
          ref.data = dplyr::select(ref, -groupVarName)
          tam.ref <-  TAM::tam.mml(resp = ref.data)
          ref1 = tam.ref$xsi
          
          
          focal = dplyr::filter(data, data[[groupVarName]] == 1)
          focal.data = dplyr::select(focal, -groupVarName)
          tam.focal <- TAM::tam.mml(resp = focal.data)
          focal1 = tam.focal$xsi
          
          
          # calculating item parameter for each group----------
          item.1PL <- rbind(ref1, focal1)
          
          res1 <- difR::difRaju(
            irtParam = item.1PL,
            focal.name = 1,
            p.adjust.method = padjust,
            same.scale = FALSE
          )
          
          # Calculating Mantel-Haenszel using difR::difMH()-------
          
          if (isTRUE(self$options$mh | self$options$plot1)) {
            # example:
            # difR::difMH(verbal, group = "Gender", focal.name = 1)
            mh <- difR::difMH(data,
                              groupVarName,
                              # data[[groupVarName]],
                              focal.name = 1,
                              p.adjust.method = padjust)
            
            
            if (is.null(self$options$group))
              return()
            
            table <- self$results$mh
            items <- self$options$vars
            
            # get result---
            mhstat <- as.vector(mh$MH)
            p <- as.vector(mh$p.value)
            padjust <- as.vector(mh$adjusted.p)
            
            
            for (i in seq_along(items)) {
              row <- list()
              row[["mhstat"]] <- mhstat[i]
              row[["p"]] <- p[i]
              row[["padjust"]] <- padjust[i]
              table$setRow(rowKey = items[i], values = row)
            }
            
            # MH Plot -------
            image1 <- self$results$plot1
            image1$setState(mh)
          }
          # ICC PLOT RESULT----------
          
          # Coefficients for all items
          itempar <- res1$itemParInit
          #dif result---------
          
          zstat <- as.vector(res1$RajuZ)
          p <- as.vector(res1$p.value)
          padjust <- as.vector(res1$adjusted.p)
          
          
          # get ETS delta scale--------
          
          itk <- 1:length(res1$RajuZ)
          pars <- res1$itemParInit
          J <- nrow(pars) / 2
          mR <- pars[1:J, 1]
          mF <- difR::itemRescale(pars[1:J, ], pars[(J + 1):(2 * J), ])[, 1]
          
          
          rr1 <- mF - mR
          rr2 <- -2.35 * rr1
          
          symb1 <- symnum(abs(rr2), c(0, 1, 1.5, Inf), symbols = c("A", "B", "C"))
          
          #get ETS delta result------
          
          delta <- as.vector(rr2)
          es <- as.vector(symb1)
          
          results <-
            list(
              'zstat' = zstat,
              'p' = p,
              'padjust' = padjust,
              'delta' = delta,
              'es' = es,
              'itempar' = itempar
            )
          
          
          if (is.null(self$options$group))
            return()
          
          table <- self$results$raju
          items <- self$options$vars
          
          # get result---
          
          zstat <- results$zstat
          p <- results$p
          padjust <- results$padjust
          delta <- results$delta
          es <- results$es
          
          
          for (i in seq_along(items)) {
            row <- list()
            row[["zstat"]] <- zstat[i]
            row[["p"]] <- p[i]
            row[["padjust"]] <- padjust[i]
            row[["delta"]] <- delta[i]
            row[["es"]] <- es[i]
            table$setRow(rowKey = items[i], values = row)
          }
          
          # Prepare Data For Plot -------
          image <- self$results$zplot
          image$setState(res1)
          
          # prepare dif icc plot--------
          
          image <- self$results$plot3
          image$setState(itempar)
          
        }
        
      },
      
      
      .plot = function(image, ...) {
        if (is.null(image$state))
          return(FALSE)
        
        plotData <- image$state
        plot <- plot(plotData)
        print(plot)
        TRUE
      },
      
      .plot3 = function(image, ...) {
        #  itempar <- image$parent$state
        
        if (is.null(image$state))
          return()
        
        itempar <- image$state
        
        #images <- self$results$plot3
        
        # index <- 1
        #
        # for (item in images$items) {
        #   if (identical(image, item))
        #     break()
        #
        #   index <- index + 1
        # }
        num <- self$options$num
        plot3 <- ShinyItemAnalysis::plotDIFirt(parameters = itempar,
                                               item = num,
                                               test = "Raju")
        print(plot3)
        TRUE
      },
      
      .plot1 = function(image1, ...) {
        if (is.null(image1$state))
          return(FALSE)
        
        plotData <- image1$state
        plot1 <- plot(plotData)
        print(plot1)
        TRUE
      },
      
      .plot2 = function(image2, ...) {
        if (is.null(image2$state))
          return(FALSE)
        
        plotData <- image2$state
        plot2 <- plot(plotData)
        print(plot2)
        TRUE
      }
    )
  )

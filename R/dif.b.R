
difClass <- if (requireNamespace('jmvcore'))
  R6::R6Class(
    "difClass",
    inherit = difBase,
    private = list(
      .htmlwidget = NULL,
      
      .init = function() {
        private$.htmlwidget <- HTMLWidget$new()
        
        # Check data and variable availability
        if (is.null(self$data) | is.null(self$options$vars)) {
          self$results$instructions$setVisible(visible = TRUE)
        }
        
        # Set instruction content
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
        
        # Set table notes
        if (self$options$raju) {
          self$results$raju$setNote(
            "Note",
            "1. Effect size(ETS Delta scale) for absolute values of 'deltaRaju' = A: negligible effect(<1.0), B: moderate effect(>1.0),C: large effect(>1.5)."
          )
        }
        
        if (length(self$options$vars) <= 1)
          self$setStatus('complete')
      },
      
      .run = function() {
        # Prepare basic data
        groupVarName <- self$options$group
        vars <- self$options$vars
        
        if (is.null(groupVarName))
          return()
        
        # Select only required variables
        data <- self$data[, c(groupVarName, vars), drop = FALSE]
        
        # Convert item variables to numeric
        data[vars] <- lapply(data[vars], jmvcore::toNumeric)
        
        # Remove missing values in the grouping variable
        data <- data[!is.na(data[[groupVarName]]), , drop = FALSE]
        groupLevels <- base::levels(data[[groupVarName]])
        
        # Run GMH for more than two groups
        if (length(groupLevels) > 2) {
          private$.runGMH(data, groupVarName)
        } else {
          # Run binary group DIF analyses
          private$.runBinaryGroup(data, groupVarName)
        }
        
        # Clean up memory
        gc(verbose = FALSE)
      },
      
      .runGMH = function(data, groupVarName) {
        if (isTRUE(self$options$gmh | self$options$plot2)) {
          fn <- as.numeric(strsplit(self$options$fn, ',')[[1]])
          
          # Run GMH DIF analysis
          gmh <- difR::difGMH(data,
                              groupVarName,
                              focal.names = fn,
                              p.adjust.method = self$options$padjust2)
          
          # Update GMH result table
          table <- self$results$gmh
          items <- self$options$vars
          
          gmhstat <- as.vector(gmh$GMH)
          p <- as.vector(gmh$p.value)
          padjust <- as.vector(gmh$adjusted.p)
          
          for (i in seq_along(items)) {
            table$setRow(rowKey = items[i], values = list(
              gmhstat = gmhstat[i],
              p = p[i],
              padjust = padjust[i]
            ))
          }
          
          # Set GMH plot state
          self$results$plot2$setState(gmh)
        }
      },
      
      .runBinaryGroup = function(data, groupVarName) {
        # Split data by group
        groupData <- split(data, data[[groupVarName]])
        
        # Prepare reference and focal group data
        ref.data <- groupData[["0"]][, -1, drop = FALSE]
        focal.data <- groupData[["1"]][, -1, drop = FALSE]
        
        # Fit TAM models
        tam.ref <- TAM::tam.mml(resp = ref.data)
        tam.focal <- TAM::tam.mml(resp = focal.data)
        
        ref1 <- tam.ref$xsi
        focal1 <- tam.focal$xsi
        
        # Combine item parameters from both groups
        item.1PL <- rbind(ref1, focal1)
        
        # Run Raju DIF method
        res1 <- difR::difRaju(
          irtParam = item.1PL,
          focal.name = 1,
          p.adjust.method = self$options$padjust,
          same.scale = FALSE
        )
        
        # Calculate ETS delta scale
        pars <- res1$itemParInit
        J <- nrow(pars) / 2
        mR <- pars[1:J, 1]
        mF <- difR::itemRescale(pars[1:J, ], pars[(J + 1):(2 * J), ])[, 1]
        
        rr1 <- mF - mR
        rr2 <- -2.35 * rr1
        
        symb1 <- symnum(abs(rr2), c(0, 1, 1.5, Inf), symbols = c("A", "B", "C"))
        
        # Store current Raju results for table update
        rajuResults <- list(
          zstat = as.vector(res1$RajuZ),
          p = as.vector(res1$p.value),
          padjust = as.vector(res1$adjusted.p),
          delta = as.vector(rr2),
          es = as.vector(symb1),
          itempar = res1$itemParInit,
          rajuData = res1
        )
        
        # Run MH method if requested
        if (isTRUE(self$options$mh | self$options$plot1)) {
          private$.runMH(data, groupVarName)
        }
        
        # Update Raju result table
        private$.updateRajuTable(rajuResults)
        
        # Set plot states
        self$results$zplot$setState(rajuResults$rajuData)
        self$results$plot3$setState(rajuResults$itempar)
        
        # Clean up large objects
        rm(tam.ref, tam.focal, res1, groupData)
        gc(verbose = FALSE)
      },
      
      .runMH = function(data, groupVarName) {
        # Run MH DIF method
        mh <- difR::difMH(data,
                          groupVarName,
                          focal.name = 1,
                          p.adjust.method = self$options$padjust1)
        
        # Update MH result table
        table <- self$results$mh
        items <- self$options$vars
        
        mhstat <- as.vector(mh$MH)
        p <- as.vector(mh$p.value)
        padjust <- as.vector(mh$adjusted.p)
        
        for (i in seq_along(items)) {
          table$setRow(rowKey = items[i], values = list(
            mhstat = mhstat[i],
            p = p[i],
            padjust = padjust[i]
          ))
        }
        
        # Set MH plot state
        self$results$plot1$setState(mh)
      },
      
      .updateRajuTable = function(results) {
        table <- self$results$raju
        items <- self$options$vars
        
        # Update each row individually
        for (i in seq_along(items)) {
          table$setRow(rowKey = items[i], values = list(
            zstat = results$zstat[i],
            p = results$p[i],
            padjust = results$padjust[i],
            delta = results$delta[i],
            es = results$es[i]
          ))
        }
      },
      
      .plot = function(image, ...) {
        if (is.null(image$state))
          return(FALSE)
        
        # Generate plot only when displayed
        plot <- plot(image$state)
        print(plot)
        TRUE
      },
      
      .plot3 = function(image, ...) {
        if (is.null(image$state))
          return()
        
        # Generate plot only when displayed
        num <- self$options$num
        plot3 <- ShinyItemAnalysis::plotDIFirt(parameters = image$state,
                                               item = num,
                                               test = "Raju")
        print(plot3)
        TRUE
      },
      
      .plot1 = function(image1, ...) {
        if (is.null(image1$state))
          return(FALSE)
        
        # Generate plot only when displayed
        plot1 <- plot(image1$state)
        print(plot1)
        TRUE
      },
      
      .plot2 = function(image2, ...) {
        if (is.null(image2$state))
          return(FALSE)
        
        # Generate plot only when displayed
        plot2 <- plot(image2$state)
        print(plot2)
        TRUE
      }
    )
  )
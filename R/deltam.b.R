deltamClass <- if (requireNamespace('jmvcore', quietly = TRUE))
  R6::R6Class(
    "deltamClass",
    inherit = deltamBase,
    private = list(
      .htmlwidget = NULL,
      .cachedData = NULL,       # Cache for processed data
      .cachedGroupLevels = NULL, # Cache for group levels
      
      .init = function() {
        # Create widget once during initialization
        private$.htmlwidget <- HTMLWidget$new()
        
        # Show instructions if data or variables are not available
        if (is.null(self$data) | is.null(self$options$vars)) {
          self$results$instructions$setVisible(visible = TRUE)
        }
        
        # Generate instruction content only once
        instructionContent <- paste(
          '<div style="border: 2px solid #e6f4fe; border-radius: 15px; padding: 15px; background-color: #e6f4fe; margin-top: 10px;">',
          '<div style="text-align:justify;">',
          '<ul>',
          '<li>Each variable should be coded as 0 or 1 with the <b>Grouping variable</b>in jamovi.</li>',
          '<li>The focal group should be coded as 1.</li>',
          '<li>The Angoff delta method is described in the <a href="https://ppw.kuleuven.be/okp/_pdf/Magis2011ADMRI.pdf"  target = "_blank">paper</a>.</li>',
          '<li>Feature requests and bug reports can be made on my <a href="https://github.com/hyunsooseol/snowIRT/issues" target="_blank">GitHub</a>.</li>',
          '</ul></div></div>'
        )
        
        self$results$instructions$setContent(private$.htmlwidget$generate_accordion(
          title = "Instructions",
          content = instructionContent
        ))
        
        # Set notes and plot sizes only once during initialization
        if (self$options$fixed)
          self$results$fixed$setNote("Note", "Detection threshold: 1.5")
        
        # Configure plot sizes conditionally
        if (isTRUE(self$options$plot)) {
          width <- self$options$width
          height <- self$options$height
          self$results$plot$setSize(width, height)
        }
        
        if (isTRUE(self$options$plot1)) {
          width <- self$options$width1
          height <- self$options$height1
          self$results$plot1$setSize(width, height)
        }
        
        # Early completion if insufficient variables
        if (length(self$options$vars) <= 1)
          self$setStatus('complete')
      },
      
      # Process data only once and store for reuse
      .prepareData = function() {
        # Return cached data if available
        if (!is.null(private$.cachedData))
          return(list(data = private$.cachedData, groupLevels = private$.cachedGroupLevels))
        
        # Check if we have sufficient variables
        if (length(self$options$vars) < 1)
          return(NULL)
        
        # Get variables
        groupVarName <- self$options$group
        vars <- self$options$vars
        
        # Early return if group is missing
        if (is.null(groupVarName))
          return(NULL)
        
        # Extract only needed variables
        varNames <- c(groupVarName, vars)
        data <- jmvcore::select(self$data, varNames)
        
        # Convert to numeric only where necessary
        for (var in vars)
          data[[var]] <- jmvcore::toNumeric(data[[var]])
        
        # Remove NAs in grouping variable
        data <- data[!is.na(data[[groupVarName]]), ]
        groupLevels <- base::levels(data[[groupVarName]])
        
        # Validate group levels
        if (length(groupLevels) != 2)
          jmvcore::reject("Grouping variable '{a}' must have exactly 2 levels",
                          code = "grouping_var_must_have_2_levels",
                          a = groupVarName)
        
        # Cache the processed data
        private$.cachedData <- data
        private$.cachedGroupLevels <- groupLevels
        
        return(list(data = data, groupLevels = groupLevels))
      },
      
      .run = function() {
        # Get prepared data or exit early if not available
        prepData <- private$.prepareData()
        if (is.null(prepData))
          return()
        
        data <- prepData$data
        groupVarName <- self$options$group
        
        # Only run fixed threshold analysis if selected
        if (self$options$fixed == TRUE) {
          # Calculate delta scores with fixed threshold (compute only when needed)
          fixed <- deltaPlotR::deltaPlot(
            data = data,
            group = groupVarName,
            focal.name = 1,
            thr = 1.5,
            purify = FALSE
          )
          
          # Process results efficiently
          df <- data.frame(fixed$Props, fixed$Deltas, fixed$Dist)
          table <- self$results$fixed
          items <- self$options$vars
          
          # Extract vectors once before the loop
          pr <- df$X1
          pf <- df$X2
          deltar <- df$X1.1
          deltaf <- df$X2.1
          dist <- df$fixed.Dist
          
          # Populate table with precomputed values
          for (i in seq_along(items)) {
            row <- list()
            row[["pr"]] <- pr[i]
            row[["pf"]] <- pf[i]
            row[["dr"]] <- deltar[i]
            row[["df"]] <- deltaf[i]
            row[["dist"]] <- dist[i]
            
            table$setRow(rowKey = items[i], values = row)
          }
          
          # Set DIF items content
          self$results$text$setContent(fixed$DIFitems)
          
          # Store fixed results for plotting (lazy evaluation)
          self$results$plot$setState(fixed)
        }
        
        # Only run normal threshold analysis if selected
        if (self$options$normal == TRUE) {
          puri <- self$options$puri
          
          # Calculate delta scores with normal threshold
          normal <- deltaPlotR::deltaPlot(
            data = data,
            group = groupVarName,
            focal.name = 1,
            thr = "norm",
            purify = TRUE,
            purType = puri
          )
          
          # Process distance matrix efficiently
          dist <- as.data.frame(normal$Dist)
          
          # Use existing variable names and dimensions
          names <- dimnames(dist)[[1]] # variables
          dims <- dimnames(dist)[[2]]  # columns
          table1 <- self$results$normal
          
          # Add columns only once
          for (dim in dims) {
            table1$addColumn(name = paste0(dim), type = 'number')
          }
          
          # Populate table efficiently
          for (name in names) {
            row <- list()
            for (j in seq_along(dims)) {
              row[[dims[j]]] <- dist[name, j]
            }
            table1$addRow(rowKey = name, values = row)
          }
          
          # Set content for DIF items and thresholds
          self$results$text1$setContent(normal$DIFitems)
          self$results$text2$setContent(normal$thr)
          
          # Store normal results for plotting (lazy evaluation)
          self$results$plot1$setState(normal)
        }
      },
      
      # Deferred plotting functions - only executed when needed
      .plot = function(image, ...) {
        if (is.null(image$state))
          return(FALSE)
        
        # Generate plot only when displayed
        plot <- deltaPlotR::diagPlot(image$state, thr.draw = TRUE)
        print(plot)
        TRUE
      },
      
      .plot1 = function(image, ...) {
        if (is.null(image$state))
          return(FALSE)
        
        # Generate plot only when displayed
        plot1 <- deltaPlotR::diagPlot(image$state, thr.draw = TRUE)
        print(plot1)
        TRUE
      }
    )
  )
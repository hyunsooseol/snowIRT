
deltamClass <- if (requireNamespace('jmvcore', quietly = TRUE))
  R6::R6Class(
    "deltamClass",
    inherit = deltamBase,
    private = list(
      .htmlwidget = NULL,
      
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
          '<li>Each variable should be coded as 0 or 1 with the <b>Grouping variable</b> in jamovi.</li>',
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
        
        
        # Early completion if insufficient variables
        if (length(self$options$vars) <= 1)
          self$setStatus('complete')
      },
      
      # Process data
      .prepareData = function() {
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
          
          # Set detection threshold below the plot
          self$results$plotNote$setContent(
            "Note. Detection threshold: 1.5"
          )
          
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
          
          # Set detection threshold below the plot
          normalThr <- suppressWarnings(
            as.numeric(normal$thr)
          )
          
          normalThr <- normalThr[
            is.finite(normalThr)
          ]
          
          if (length(normalThr) > 0) {
            normalThr <- utils::tail(normalThr, 1)
            
            self$results$plot1Note$setContent(
              paste0(
                "Note. Detection threshold: ",
                round(normalThr, 3)
              )
            )
          } else {
            self$results$plot1Note$setContent(
              "Note. Detection threshold could not be calculated."
            )
          }
          
          # Store normal results for plotting (lazy evaluation)
          self$results$plot1$setState(normal)
        }
      },
      
      # Create ggplot2 delta plot
      .makeDeltaPlot = function(result) {
        
        if (is.null(result$Deltas))
          return(NULL)
        
        deltaData <- as.data.frame(result$Deltas)
        
        if (ncol(deltaData) < 2)
          return(NULL)
        
        plotData <- data.frame(
          item = rownames(deltaData),
          reference = as.numeric(deltaData[[1]]),
          focal = as.numeric(deltaData[[2]]),
          originalRow = seq_len(nrow(deltaData)),
          stringsAsFactors = FALSE
        )
        
        # Use selected variable names if row names are unavailable
        defaultRowNames <- as.character(
          seq_len(nrow(plotData))
        )
        
        if (
          is.null(plotData$item) ||
          any(plotData$item == "") ||
          identical(plotData$item, defaultRowNames)
        ) {
          if (
            !is.null(self$options$vars) &&
            length(self$options$vars) == nrow(plotData)
          ) {
            plotData$item <- self$options$vars
          } else {
            plotData$item <- paste0(
              "Item ",
              seq_len(nrow(plotData))
            )
          }
        }
        
        # Extract the final distance
        distanceData <- as.data.frame(result$Dist)
        
        plotData$distance <- NA_real_
        
        if (nrow(distanceData) == nrow(plotData)) {
          plotData$distance <- as.numeric(
            distanceData[[ncol(distanceData)]]
          )
        }
        
        # Remove invalid delta values
        plotData <- plotData[
          is.finite(plotData$reference) &
            is.finite(plotData$focal),
          ,
          drop = FALSE
        ]
        
        if (nrow(plotData) < 2)
          return(NULL)
        
        # Extract detection threshold
        threshold <- suppressWarnings(
          as.numeric(result$thr)
        )
        
        threshold <- threshold[
          is.finite(threshold)
        ]
        
        if (length(threshold) > 0) {
          threshold <- utils::tail(
            threshold,
            1
          )
        } else {
          threshold <- 1.5
        }
        
        # Identify DIF items
        plotData$isDIF <- FALSE
        
        if (any(is.finite(plotData$distance))) {
          plotData$isDIF <-
            abs(plotData$distance) > threshold
        }
        
        difItems <- result$DIFitems
        
        if (!is.null(difItems)) {
          difItems <- as.character(
            unlist(difItems)
          )
          
          detectedByName <- vapply(
            plotData$item,
            function(itemName) {
              any(
                grepl(
                  pattern = itemName,
                  x = difItems,
                  fixed = TRUE
                )
              )
            },
            logical(1)
          )
          
          plotData$isDIF <-
            plotData$isDIF | detectedByName
        }
        
        # Fit central reference line
        fit <- stats::lm(
          focal ~ reference,
          data = plotData
        )
        
        coefficients <- stats::coef(fit)
        
        intercept <- unname(coefficients[1])
        slope <- unname(coefficients[2])
        
        if (
          !is.finite(intercept) ||
          !is.finite(slope)
        )
          return(NULL)
        
        # Convert perpendicular threshold to vertical offset
        interceptDifference <-
          threshold * sqrt(1 + slope^2)
        
        upperIntercept <-
          intercept + interceptDifference
        
        lowerIntercept <-
          intercept - interceptDifference
        
        # Determine axis range
        allValues <- c(
          plotData$reference,
          plotData$focal
        )
        
        axisRange <- range(
          allValues,
          finite = TRUE
        )
        
        axisPadding <- diff(axisRange) * 0.07
        
        if (
          !is.finite(axisPadding) ||
          axisPadding == 0
        ) {
          axisPadding <- 0.5
        }
        
        # Add extra space for DIF item labels
        axisLimits <- c(
          axisRange[1] - axisPadding,
          axisRange[2] + axisPadding * 1.7
        )
        
        nonDIFData <- plotData[
          !plotData$isDIF,
          ,
          drop = FALSE
        ]
        
        difData <- plotData[
          plotData$isDIF,
          ,
          drop = FALSE
        ]
        
        # Draw plot
        plot <- ggplot2::ggplot(
          plotData,
          ggplot2::aes(
            x = reference,
            y = focal
          )
        ) +
          ggplot2::geom_abline(
            intercept = intercept,
            slope = slope,
            linewidth = 0.7,
            colour = "grey35"
          ) +
          ggplot2::geom_abline(
            intercept = lowerIntercept,
            slope = slope,
            linewidth = 0.6,
            linetype = "dashed",
            colour = "grey55"
          ) +
          ggplot2::geom_abline(
            intercept = upperIntercept,
            slope = slope,
            linewidth = 0.6,
            linetype = "dashed",
            colour = "grey55"
          )
        
        # Non-DIF items
        if (nrow(nonDIFData) > 0) {
          plot <- plot +
            ggplot2::geom_point(
              data = nonDIFData,
              shape = 24,
              size = 2.7,
              stroke = 0.8,
              colour = "grey45",
              fill = "white"
            )
        }
        
        # DIF items and labels
        if (nrow(difData) > 0) {
          
          difData$label_x <-
            difData$reference + axisPadding * 0.12
          
          difData$label_y <-
            difData$focal + axisPadding * 0.35
          
          plot <- plot +
            ggplot2::geom_point(
              data = difData,
              shape = 24,
              size = 4,
              stroke = 1,
              colour = "#C45100",
              fill = "#F28E2B"
            ) +
            ggplot2::geom_text(
              data = difData,
              ggplot2::aes(
                x = label_x,
                y = label_y,
                label = item
              ),
              colour = "#C45100",
              fontface = "bold",
              size = 3.5,
              hjust = 0,
              vjust = 0,
              check_overlap = TRUE,
              inherit.aes = FALSE,
              show.legend = FALSE
            )
        }
        
        plot <- plot +
          ggplot2::coord_cartesian(
            xlim = axisLimits,
            ylim = axisLimits,
            expand = FALSE,
            clip = "on"
          ) +
          ggplot2::labs(
            title = NULL,
            subtitle = NULL,
            x = "Reference group",
            y = "Focal group"
          ) +
          ggplot2::theme_bw(
            base_size = 12
          ) +
          ggplot2::theme(
            panel.grid.major =
              ggplot2::element_blank(),
            panel.grid.minor =
              ggplot2::element_blank(),
            axis.title = ggplot2::element_text(
              size = 11
            ),
            axis.text = ggplot2::element_text(
              size = 10
            ),
            plot.margin = ggplot2::margin(
              t = 10,
              r = 20,
              b = 10,
              l = 10
            )
          )
        
        plot
      },
      
      # Deferred plotting functions - only executed when needed
      .plot = function(image, ...) {
        if (is.null(image$state))
          return(FALSE)
        
        plot <- private$.makeDeltaPlot(
          result = image$state
        )
        
        if (is.null(plot))
          return(FALSE)
        
        print(plot)
        TRUE
      },
      
      .plot1 = function(image, ...) {
        if (is.null(image$state))
          return(FALSE)
        
        plot1 <- private$.makeDeltaPlot(
          result = image$state
        )
        
        if (is.null(plot1))
          return(FALSE)
        
        print(plot1)
        TRUE
      }
    )
  )
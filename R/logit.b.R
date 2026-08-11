
# diford

logitClass <- if (requireNamespace('jmvcore', quietly = TRUE))
  R6::R6Class(
    "logitClass",
    inherit = logitBase,
    private = list(
      
      .htmlwidget = NULL,
      
      # Cache for difORD fit
      .fitCache = NULL,
      .fitCacheKey = NULL,
      .fitCacheData = NULL,
      
      #=============================================================
      
      .init = function() {
        
        private$.htmlwidget <- HTMLWidget$new()
        
        if (is.null(self$data) | is.null(self$options$vars)) {
          self$results$instructions$setVisible(visible = TRUE)
        }
        
        self$results$instructions$setContent(
          private$.htmlwidget$generate_accordion(
            title = "Instructions",
            content = paste(
              '<div style="border: 2px solid #e6f4fe; border-radius: 15px; padding: 15px; background-color: #e6f4fe; margin-top: 10px;">',
              '<div style="text-align:justify;">',
              '<ul>',
              '<li>Performs DIF detection procedure for ordinal data based either on adjacent category logit model or on cumulative logit model.</li>',
              '<li>The focal group should be coded as 1.</li>',
              '<li>DIF likelihood ratio statistics are estimated by using <b>difNLR::difORD</b> function.</li>',
              '<li><b>Matching reference</b> determines how the matching criterion is constructed: All items, Item purification, or user-defined Anchor items.</li>',
              '<li><b>All items:</b> all selected DIF items are used to compute the matching criterion.</li>',
              '<li><b>Item purification:</b> the matching set is iteratively refined by excluding items identified as DIF.</li>',
              '<li><b>Anchor items:</b> select one or more items assumed to be DIF-free. These items are used to compute the matching criterion.</li>',
              '<li>Item purification and user-defined Anchor items are alternative procedures and should not be used simultaneously.</li>',
              '<li>DIF plots display model-based predicted category probabilities for the reference and focal groups.</li>',
              '<li><b>Caution for interpretation:</b> statistical significance should be interpreted together with the size and shape of the probability-curve differences.</li>',
              '<li><b>Caution for interpretation:</b> the DIF plot shows predicted probabilities, not raw observed response proportions.</li>',
              '<li><b>Caution for interpretation:</b> if the reference and focal curves largely overlap, the practical magnitude of DIF may be small even when numerical differences are present.</li>',
              '<li>Feature requests and bug reports can be made on my <a href="https://github.com/hyunsooseol/snowIRT/issues" target="_blank">GitHub</a>.</li>',
              '</ul></div></div>'
            )
          )
        )
        
        if (self$options$method)
          self$results$method$setNote(
            "Note",
            "Adj.p = The adjusted p-values by likelihood ratio test using multiple comparison."
          )
      },
      
      #=============================================================
      
      .run = function() {
        
        if (length(self$options$vars) < 1)
          return()
        
        #===========================================================
        # Get variables
        #===========================================================
        
        groupVarName <- self$options$group
        vars <- self$options$vars
        anchor <- self$options$anchor
        anchorMethod <- self$options$anchorMethod
        
        if (is.null(groupVarName))
          return()
        
        if (is.null(anchor))
          anchor <- character(0)
        
        anchor <- unique(anchor)
        
        #===========================================================
        # Validate matching reference
        #===========================================================
        
        if (
          is.null(anchorMethod) ||
          length(anchorMethod) != 1 ||
          !anchorMethod %in% c(
            "all",
            "purify",
            "anchor"
          )
        ) {
          jmvcore::reject(
            paste(
              "The Matching reference setting is not valid.",
              "Please select All items, Item purification, or Anchor items."
            )
          )
        }
        
        #-----------------------------------------------------------
        # Anchor method selected but no anchor item supplied
        #-----------------------------------------------------------
        
        if (
          identical(anchorMethod, "anchor") &&
          length(anchor) < 1
        ) {
          jmvcore::reject(
            paste(
              "Matching reference is set to 'Anchor items',",
              "but no anchor item has been selected.",
              "Please select one or more items in the Anchor Items box,",
              "or choose another Matching reference method."
            )
          )
        }
        
        #-----------------------------------------------------------
        # All items selected while anchor items remain in the UI
        #-----------------------------------------------------------
        
        if (
          identical(anchorMethod, "all") &&
          length(anchor) > 0
        ) {
          
          anchorNames <- paste(
            anchor,
            collapse = ", "
          )
          
          jmvcore::reject(
            paste0(
              "Anchor item(s) are currently selected: ",
              anchorNames,
              ". Matching reference is set to 'All items'. ",
              "Please remove the selected Anchor Items, ",
              "or change Matching reference to 'Anchor items'."
            )
          )
        }
        
        #-----------------------------------------------------------
        # Purification selected while anchor items remain in the UI
        #-----------------------------------------------------------
        
        if (
          identical(anchorMethod, "purify") &&
          length(anchor) > 0
        ) {
          
          anchorNames <- paste(
            anchor,
            collapse = ", "
          )
          
          jmvcore::reject(
            paste0(
              "Anchor item(s) are currently selected: ",
              anchorNames,
              ". Item purification and user-defined Anchor items ",
              "cannot be used together. ",
              "Please remove the selected Anchor Items, ",
              "or change Matching reference to 'Anchor items'."
            )
          )
        }
        
        #===========================================================
        # Prepare item set
        #
        # Variables moved to Anchor Items are removed from the DIF
        # Items list by the jamovi VariableSupplier. They must still
        # be included in Data because difORD uses them to calculate
        # the matching criterion.
        #===========================================================
        
        if (identical(anchorMethod, "anchor")) {
          
          allItems <- unique(
            c(
              anchor,
              vars
            )
          )
          
        } else {
          
          allItems <- vars
        }
        
        if (length(allItems) < 1)
          return()
        
        varNames <- c(
          groupVarName,
          allItems
        )
        
        data <- self$data[
          ,
          varNames,
          drop = FALSE
        ]
        
        # Convert ordinal items to numeric
        for (var in allItems)
          data[[var]] <- jmvcore::toNumeric(
            data[[var]]
          )
        
        # Exclude observations with missing grouping variable
        data <- data[
          !is.na(data[[groupVarName]]),
          ,
          drop = FALSE
        ]
        
        #===========================================================
        # Validate grouping variable
        #===========================================================
        
        groupLevels <- unique(
          data[[groupVarName]]
        )
        
        groupLevels <- groupLevels[
          !is.na(groupLevels)
        ]
        
        if (length(groupLevels) != 2) {
          jmvcore::reject(
            paste(
              "The grouping variable must have exactly 2 levels",
              "for logistic DIF analysis."
            )
          )
        }
        
        #===========================================================
        # Build cache key
        #
        # plot and plotItem are intentionally excluded.
        # Therefore, changing only the displayed DIF item does not
        # refit the difORD model.
        #===========================================================
        
        cacheKey <- list(
          group = groupVarName,
          vars = vars,
          anchor = anchor,
          anchorMethod = anchorMethod,
          model = self$options$model,
          type = self$options$type,
          match = self$options$match,
          padjust = self$options$padjust
        )
        
        useCachedFit <- (
          !is.null(private$.fitCache) &&
            !is.null(private$.fitCacheKey) &&
            !is.null(private$.fitCacheData) &&
            identical(
              private$.fitCacheKey,
              cacheKey
            ) &&
            identical(
              private$.fitCacheData,
              data
            )
        )
        
        #===========================================================
        # DIF analysis
        #===========================================================
        
        if (useCachedFit) {
          
          fit <- private$.fitCache
          
        } else {
          
          fit <- tryCatch(
            
            {
              
              #=====================================================
              # 1. User-defined Anchor items
              #=====================================================
              
              if (identical(anchorMethod, "anchor")) {
                
                difNLR::difORD(
                  Data = data,
                  group = groupVarName,
                  focal.name = 1,
                  model = self$options$model,
                  type = self$options$type,
                  match = self$options$match,
                  anchor = anchor,
                  p.adjust.method = self$options$padjust
                )
                
                #=====================================================
                # 2. Item purification
                #=====================================================
                
              } else if (identical(anchorMethod, "purify")) {
                
                difNLR::difORD(
                  Data = data,
                  group = groupVarName,
                  focal.name = 1,
                  model = self$options$model,
                  type = self$options$type,
                  match = self$options$match,
                  purify = TRUE,
                  p.adjust.method = self$options$padjust
                )
                
                #=====================================================
                # 3. All items
                #=====================================================
                
              } else {
                
                difNLR::difORD(
                  Data = data,
                  group = groupVarName,
                  focal.name = 1,
                  model = self$options$model,
                  type = self$options$type,
                  match = self$options$match,
                  p.adjust.method = self$options$padjust
                )
              }
            },
            
            error = function(e) {
              
              jmvcore::reject(
                paste0(
                  "The DIF analysis could not be estimated. ",
                  conditionMessage(e),
                  " Please check the selected items, grouping variable, ",
                  "matching reference, and model settings."
                )
              )
            }
          )
          
          # Save cache only after successful estimation
          private$.fitCache <- fit
          private$.fitCacheKey <- cacheKey
          private$.fitCacheData <- data
        }
        
        #===========================================================
        # DIF table
        #===========================================================
        
        chi <- fit$Sval
        p <- fit$pval
        padj <- fit$adj.pval
        
        #-----------------------------------------------------------
        # Identify item names in difORD result
        #-----------------------------------------------------------
        
        fitItemNames <- NULL
        
        if (!is.null(fit$Data)) {
          fitItemNames <- colnames(
            fit$Data
          )
        }
        
        # Fallback if item names cannot be recovered from fit$Data
        if (
          is.null(fitItemNames) ||
          length(fitItemNames) != length(chi)
        ) {
          
          if (length(allItems) == length(chi)) {
            
            fitItemNames <- allItems
            
          } else {
            
            fitItemNames <- NULL
          }
        }
        
        table <- self$results$method
        
        for (i in seq_along(vars)) {
          
          itemName <- vars[i]
          
          #---------------------------------------------------------
          # Match table row to the correct difORD item
          #
          # This is important when Anchor Items are placed before
          # DIF Items in the Data object.
          #---------------------------------------------------------
          
          if (!is.null(fitItemNames)) {
            
            itemIndex <- match(
              itemName,
              fitItemNames
            )
            
          } else {
            
            itemIndex <- i
          }
          
          if (
            is.na(itemIndex) ||
            itemIndex < 1 ||
            itemIndex > length(chi)
          ) {
            next
          }
          
          row <- list()
          
          row[["chi"]] <- chi[itemIndex]
          row[["p"]] <- p[itemIndex]
          row[["padj"]] <- padj[itemIndex]
          
          table$setRow(
            rowKey = itemName,
            values = row
          )
        }
        
        #===========================================================
        # DIF custom plot state
        #===========================================================
        
        if (isTRUE(self$options$plot)) {
          self$results$plot$setState(fit)
        }
      },
      
      #=============================================================
      # Custom DIF plot
      #
      # Uses predict.difORD() rather than plot.difORD().
      #
      # Design:
      #   - Category probabilities only
      #   - No empirical observations
      #   - No count legend
      #   - No score legend
      #   - One response category per panel
      #   - Reference/Focal curves only
      #=============================================================
      
      .plot = function(image, ...) {
        
        if (is.null(image$state))
          return(FALSE)
        
        fit <- image$state
        
        #===========================================================
        # Selected DIF item
        #===========================================================
        
        item <- suppressWarnings(
          as.integer(
            self$options$plotItem
          )
        )
        
        vars <- self$options$vars
        
        nItems <- length(vars)
        
        if (
          length(item) != 1 ||
          is.na(item) ||
          item < 1 ||
          item > nItems
        ) {
          return(FALSE)
        }
        
        # plotItem represents the position in the DIF Items list
        itemName <- vars[item]
        
        #===========================================================
        # Find actual item position in difORD fit
        #
        # Anchor items may occur before DIF items in fit$Data.
        # Therefore, the numeric plotItem value must not be passed
        # directly to predict.difORD().
        #===========================================================
        
        fitItemNames <- NULL
        
        if (!is.null(fit$Data)) {
          fitItemNames <- colnames(
            fit$Data
          )
        }
        
        if (
          is.null(fitItemNames) ||
          length(fitItemNames) < 1
        ) {
          return(FALSE)
        }
        
        fitItem <- match(
          itemName,
          fitItemNames
        )
        
        if (
          is.na(fitItem) ||
          fitItem < 1
        ) {
          return(FALSE)
        }
        
        #===========================================================
        # Matching criterion
        #===========================================================
        
        matching <- fit$match
        
        if (is.null(matching))
          return(FALSE)
        
        if (
          is.matrix(matching) ||
          is.data.frame(matching)
        ) {
          
          if (ncol(matching) < fitItem)
            return(FALSE)
          
          itemMatch <- matching[
            ,
            fitItem
          ]
          
        } else {
          
          itemMatch <- matching
        }
        
        itemMatch <- suppressWarnings(
          as.numeric(
            itemMatch
          )
        )
        
        itemMatch <- itemMatch[
          is.finite(itemMatch)
        ]
        
        if (length(itemMatch) < 2)
          return(FALSE)
        
        xMin <- min(
          itemMatch,
          na.rm = TRUE
        )
        
        xMax <- max(
          itemMatch,
          na.rm = TRUE
        )
        
        if (
          !is.finite(xMin) ||
          !is.finite(xMax) ||
          xMin == xMax
        ) {
          return(FALSE)
        }
        
        # Smooth prediction grid
        matchGrid <- seq(
          from = xMin,
          to = xMax,
          length.out = 150
        )
        
        #===========================================================
        # Predicted category probabilities
        #===========================================================
        
        predReference <- tryCatch(
          
          stats::predict(
            fit,
            item = fitItem,
            match = matchGrid,
            group = 0,
            type = "category"
          ),
          
          error = function(e)
            NULL
        )
        
        predFocal <- tryCatch(
          
          stats::predict(
            fit,
            item = fitItem,
            match = matchGrid,
            group = 1,
            type = "category"
          ),
          
          error = function(e)
            NULL
        )
        
        if (
          is.null(predReference) ||
          is.null(predFocal)
        ) {
          return(FALSE)
        }
        
        #===========================================================
        # Normalize predict.difORD output
        #===========================================================
        
        if (
          is.list(predReference) &&
          !is.data.frame(predReference) &&
          length(predReference) == 1 &&
          (
            is.matrix(predReference[[1]]) ||
            is.data.frame(predReference[[1]])
          )
        ) {
          predReference <- predReference[[1]]
        }
        
        if (
          is.list(predFocal) &&
          !is.data.frame(predFocal) &&
          length(predFocal) == 1 &&
          (
            is.matrix(predFocal[[1]]) ||
            is.data.frame(predFocal[[1]])
          )
        ) {
          predFocal <- predFocal[[1]]
        }
        
        predReference <- tryCatch(
          as.data.frame(
            predReference
          ),
          error = function(e)
            NULL
        )
        
        predFocal <- tryCatch(
          as.data.frame(
            predFocal
          ),
          error = function(e)
            NULL
        )
        
        if (
          is.null(predReference) ||
          is.null(predFocal)
        ) {
          return(FALSE)
        }
        
        if (
          nrow(predReference) != length(matchGrid) ||
          nrow(predFocal) != length(matchGrid)
        ) {
          return(FALSE)
        }
        
        if (
          ncol(predReference) < 1 ||
          ncol(predFocal) < 1
        ) {
          return(FALSE)
        }
        
        #===========================================================
        # Find common category-probability columns
        #===========================================================
        
        commonCategories <- intersect(
          names(predReference),
          names(predFocal)
        )
        
        if (length(commonCategories) < 1)
          return(FALSE)
        
        predReference <- predReference[
          ,
          commonCategories,
          drop = FALSE
        ]
        
        predFocal <- predFocal[
          ,
          commonCategories,
          drop = FALSE
        ]
        
        #===========================================================
        # Validate probability columns
        #===========================================================
        
        validColumns <- vapply(
          commonCategories,
          function(columnName) {
            
            refValues <- suppressWarnings(
              as.numeric(
                predReference[[columnName]]
              )
            )
            
            focalValues <- suppressWarnings(
              as.numeric(
                predFocal[[columnName]]
              )
            )
            
            all(
              is.na(refValues) |
                (
                  is.finite(refValues) &
                    refValues >= 0 &
                    refValues <= 1
                )
            ) &&
              all(
                is.na(focalValues) |
                  (
                    is.finite(focalValues) &
                      focalValues >= 0 &
                      focalValues <= 1
                  )
              )
          },
          logical(1)
        )
        
        commonCategories <- commonCategories[
          validColumns
        ]
        
        if (length(commonCategories) < 1)
          return(FALSE)
        
        predReference <- predReference[
          ,
          commonCategories,
          drop = FALSE
        ]
        
        predFocal <- predFocal[
          ,
          commonCategories,
          drop = FALSE
        ]
        
        #===========================================================
        # Convert predictions to long format
        #===========================================================
        
        referenceValues <- unlist(
          lapply(
            predReference,
            function(x)
              suppressWarnings(
                as.numeric(x)
              )
          ),
          use.names = FALSE
        )
        
        focalValues <- unlist(
          lapply(
            predFocal,
            function(x)
              suppressWarnings(
                as.numeric(x)
              )
          ),
          use.names = FALSE
        )
        
        referenceLong <- data.frame(
          Match = rep(
            matchGrid,
            times = length(commonCategories)
          ),
          Probability = referenceValues,
          Category = rep(
            commonCategories,
            each = length(matchGrid)
          ),
          Group = "Reference",
          stringsAsFactors = FALSE
        )
        
        focalLong <- data.frame(
          Match = rep(
            matchGrid,
            times = length(commonCategories)
          ),
          Probability = focalValues,
          Category = rep(
            commonCategories,
            each = length(matchGrid)
          ),
          Group = "Focal",
          stringsAsFactors = FALSE
        )
        
        plotData <- rbind(
          referenceLong,
          focalLong
        )
        
        plotData <- plotData[
          is.finite(plotData$Match) &
            is.finite(plotData$Probability),
          ,
          drop = FALSE
        ]
        
        if (nrow(plotData) < 1)
          return(FALSE)
        
        # Preserve response category order
        plotData$Category <- factor(
          plotData$Category,
          levels = commonCategories
        )
        
        plotData$Group <- factor(
          plotData$Group,
          levels = c(
            "Reference",
            "Focal"
          )
        )
        
        #===========================================================
        # Axis label
        #===========================================================
        
        xLabel <- fit$match.name
        
        if (
          is.null(xLabel) ||
          length(xLabel) < 1 ||
          is.na(xLabel[1]) ||
          !nzchar(
            as.character(
              xLabel[1]
            )
          )
        ) {
          
          xLabel <- "Matching criterion"
          
        } else {
          
          xLabel <- as.character(
            xLabel[1]
          )
        }
        
        #===========================================================
        # Custom snowIRT DIF plot
        #===========================================================
        
        p <- ggplot2::ggplot(
          plotData,
          ggplot2::aes(
            x = Match,
            y = Probability,
            colour = Group,
            linetype = Group,
            group = Group
          )
        ) +
          
          ggplot2::geom_line(
            linewidth = 0.9,
            na.rm = TRUE
          ) +
          
          ggplot2::facet_wrap(
            ~ Category,
            scales = "fixed"
          ) +
          
          ggplot2::scale_y_continuous(
            limits = c(0, 1),
            breaks = seq(
              0,
              1,
              by = 0.25
            ),
            expand = ggplot2::expansion(
              mult = c(
                0.01,
                0.03
              )
            )
          ) +
          
          ggplot2::labs(
            title = paste0(
              "Item: ",
              itemName
            ),
            x = xLabel,
            y = "Category probability",
            colour = "Group",
            linetype = "Group"
          ) +
          
          ggplot2::theme_minimal(
            base_size = 11
          ) +
          
          ggplot2::theme(
            plot.title =
              ggplot2::element_text(
                face = "bold",
                hjust = 0
              ),
            panel.grid.minor =
              ggplot2::element_blank(),
            panel.grid.major.x =
              ggplot2::element_blank(),
            strip.text =
              ggplot2::element_text(
                face = "bold"
              ),
            legend.position = "top",
            legend.title =
              ggplot2::element_blank(),
            axis.title =
              ggplot2::element_text(
                face = "plain"
              )
          )
        
        print(p)
        
        TRUE
      }
      
    )
  )
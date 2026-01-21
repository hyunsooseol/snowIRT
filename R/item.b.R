
# ITEM ANALYSIS
#' @import ggplot2

itemClass <- if (requireNamespace('jmvcore', quietly = TRUE))
  R6::R6Class(
    "itemClass",
    inherit = itemBase,
    private = list(
      .cache = list(),
      .htmlwidget = NULL,
      
      .init = function() {
        private$.htmlwidget <- HTMLWidget$new()
        if (is.null(self$data) | is.null(self$options$vars)) {
          # self$results$instructions$setVisible(visible = TRUE)
        }
        self$results$instructions$setContent(private$.htmlwidget$generate_accordion(
          title = "Instructions",
          content = paste(
            '<div style="border: 2px solid #e6f4fe; border-radius: 15px; padding: 15px; background-color: #e6f4fe; margin-top: 10px;">',
            '<div style="text-align:justify;">',
            '<ul>',
            '<li>Enter the correct answer separated by commas, but there must be no spaces between commas.</li>',
            '<li>The distractor analysis uses a fixed 3-group structure(33%-34%-33%) to ensure stable and reliable results.</li>',
            '<li>By a rule of thumb, all items with a discrimination lower than 0.2 (threshold in the plot), should be checked for content.</li>',
            '<li>Feature requests and bug reports can be made on my <a href="https://github.com/hyunsooseol/snowIRT/issues" target="_blank">GitHub</a>.</li>',
            '</ul></div></div>'
          )
        ))
        if (self$options$disc)
          self$results$disc$setNote(
            "Note",
            "ULI:Upper-Lower Index based on 3 groups, RIT:Item-Total correlation, RIR: Item-Rest correlation. Missing values should be recoded as 0 or incorrect answer in jamovi."
          )
      },
      
      .run = function() {
        if (length(self$options$vars) < 2) return()
        
        # missing values warning---
        data <- self$data
        has_missing <- any(is.na(data))
        
        if (has_missing) {
          
          self$results$instructions$setContent(
            private$.htmlwidget$generate_accordion(
              title = "⚠️ Missing Values Detected",
              content = paste(
                '<div style="border: 2px solid #ff9800; border-radius: 10px; padding: 12px; background-color: #fff3e0;">',
                '<p><strong>🔍 Missing responses found - Please verify:</strong></p>',
                '<ol style="text-align: left; margin: 10px 0; padding-left: 20px;">',
                '<li><strong>Check original answer sheets first</strong><br>',
                '<span style="font-size: 0.9em; color: #666;">Enter the actual response if student answered</span></li>',
                '<li><strong>If truly blank or invalid response</strong><br>',
                '<span style="font-size: 0.9em; color: #666;">Enter <b>0</b> for incorrect scoring</span></li>',
                '</ol>',
                '<p style="margin-top: 10px; font-size: 0.9em; color: #d84315;">',
                '<strong>⚠️ Analysis cannot proceed until all cells are filled</strong></p>',
                '</div>'
              )
            )
          )
          
          return()
        }
        
        
        
        key1 <- strsplit(self$options$key, ',')[[1]]
        
        if (!identical(private$.cache$options, self$options) ||
            !identical(private$.cache$data, self$data)) {
          private$.cache <- list()
          private$.computeRES(data, key1)
          private$.cache$is_ready <- TRUE
          
          private$.cache$options <- self$options
          private$.cache$data <- self$data
        }
        
        counts   <- private$.cache$counts
        ts       <- private$.cache$ts
        prop     <- private$.cache$prop
        res      <- private$.cache$res
        dicho    <- private$.cache$dicho
        group_ok <- isTRUE(private$.cache$group_ok)
        group_err <- private$.cache$group_err
        
        # If grouping failed, show a simple notice (but DO NOT stop other outputs)
        if (!group_ok) {
          
          self$results$instructions$setVisible(TRUE)
          self$results$instructions$setContent(
            private$.htmlwidget$generate_accordion(
              title = "⚠️ Group-based outputs skipped",
              content = paste(
                '<div style="border: 2px solid #ff9800; border-radius: 10px; padding: 12px; background-color: #fff3e0;">',
                
                '<p><strong>Group-based outputs were skipped.</strong></p>',
                
                '<p>',
                'Too many respondents had identical total scores, so the data could not be divided ',
                'into three distinct groups (lower / middle / upper).',
                '</p>',
                
                '<ul>',
                '<li>Group-based distractor tables and plots were skipped.</li>',
                '<li>Counts and proportions are shown using <b>TOTAL</b> values only.</li>',
                '<li>This does not affect total scores or item-level statistics.</li>',
                '</ul>',
                
                if (!is.null(group_err))
                  paste0(
                    '<p style="margin-top:6px; font-size:0.9em; color:#6d4c41;">',
                    group_err,
                    '</p>'
                  )
                else '',
                
                '</div>'
              )
            )
          )
        }
        
        
        # -------------------------------
        # Counts of respondents
        # - Normal: lower/middle/upper (from cached counts)
        # - If group failed: TOTAL only computed directly from data
        # -------------------------------
        if (isTRUE(self$options$count)) {
          
          vars <- self$options$vars
          
          if (group_ok) {
            
            if (is.null(counts)) {
              self$results$count$setVisible(FALSE)
            } else {
              
              tab <- NULL
              for (i in seq_along(vars)) {
                tab[[i]] <- as.data.frame.matrix(counts[[i]])
              }
              
              for (i in seq_along(vars)) {
                table <- self$results$count[[i]]
                names <- dimnames(tab[[i]])[[1]]
                dims  <- dimnames(tab[[i]])[[2]]
                
                for (name in names) {
                  row <- list()
                  for (j in seq_along(dims)) {
                    row[[dims[j]]] <- tab[[i]][name, j]
                  }
                  table$addRow(rowKey = name, values = row)
                }
              }
            }
            
          } else {

            # TOTAL-only
            for (i in seq_along(vars)) {
              
              table <- self$results$count[[i]]
              
              # make sure Total column exists (ignore if already exists)
              tryCatch({
                table$addColumn(name = "Total", type = "integer")
              }, error = function(e) { })
              
              x <- data[[vars[i]]]
              x <- x[!is.na(x)]
              
              if (length(x) == 0) {
                self$results$count$setVisible(FALSE)
              } else {
                
                tt <- table(x, useNA = "no")
                opts <- names(tt)
                
                # ✅ correct option for this item
                correct <- key1[i]
                
                for (k in seq_along(opts)) {
                  rk <- if (!is.null(correct) && !is.na(correct) && opts[k] == correct)
                    paste0("*", opts[k]) else opts[k]
                  
                  table$addRow(
                    rowKey = rk,
                    values = list(Total = as.integer(tt[[k]]))
                  )
                }
              }
            }
      
          }
        }
            
            
        # -------------------------------
        # Proportions of respondents
        # - Normal: lower/middle/upper (from cached prop)
        # - If group failed: TOTAL only computed directly from data
        # -------------------------------
        if (isTRUE(self$options$prop)) {
          
          vars <- self$options$vars
          
          if (group_ok) {
            
            if (is.null(prop)) {
              self$results$prop$setVisible(FALSE)
            } else {
              
              tab <- NULL
              for (i in seq_along(vars)) {
                tab[[i]] <- as.data.frame.matrix(prop[[i]])
              }
              
              for (i in seq_along(vars)) {
                table <- self$results$prop[[i]]
                names <- dimnames(tab[[i]])[[1]]
                dims  <- dimnames(tab[[i]])[[2]]
                
                for (name in names) {
                  row <- list()
                  for (j in seq_along(dims)) {
                    row[[dims[j]]] <- tab[[i]][name, j]
                  }
                  table$addRow(rowKey = name, values = row)
                }
              }
            }
            
          } else {

            # TOTAL-only proportions
            for (i in seq_along(vars)) {
              
              table <- self$results$prop[[i]]
              
              # make sure Total column exists
              tryCatch({
                table$addColumn(name = "Total", type = "number")
              }, error = function(e) { })
              
              x <- data[[vars[i]]]
              x <- x[!is.na(x)]
              
              if (length(x) == 0) {
                self$results$prop$setVisible(FALSE)
              } else {
                
                tt <- table(x, useNA = "no")
                p  <- tt / sum(tt)
                opts <- names(tt)
                
                # ✅ correct option for this item
                correct <- key1[i]
                
                for (k in seq_along(opts)) {
                  rk <- if (!is.null(correct) && !is.na(correct) && opts[k] == correct)
                    paste0("*", opts[k]) else opts[k]
                  
                  table$addRow(
                    rowKey = rk,
                    values = list(Total = as.numeric(p[[k]]))
                  )
                }
              }
            }
            
          }
        }
        # -------------------------------
        # Total summary (group-based) -> only when group_ok
        # -------------------------------
        if (isTRUE(self$options$sum1)) {
          if (!group_ok || is.null(ts)) {
            self$results$sum1$setVisible(FALSE)
          } else {
            vars <- self$options$vars
            tab <- NULL
            for (i in seq_along(vars)) {
              tab[[i]] <- as.data.frame.matrix(ts[[i]])
            }
            for (i in seq_along(vars)) {
              table <- self$results$sum1[[i]]
              names <- dimnames(tab[[i]])[[1]]
              dims  <- dimnames(tab[[i]])[[2]]
              for (dim in dims) {
                table$addColumn(name = paste0(dim), type = 'text')
              }
              for (name in names) {
                row <- list()
                for (j in seq_along(dims)) {
                  row[[dims[j]]] <- tab[[i]][name, j]
                }
                table$addRow(rowKey = name, values = row)
              }
            }
          }
        }
        
        # -------------------------------
        # Item summary (group-based) -> only when group_ok
        # -------------------------------
        if (isTRUE(self$options$sum)) {
          if (!group_ok || is.null(res)) {
            self$results$sum$setVisible(FALSE)
          } else {
            vars <- self$options$vars
            tab <- NULL
            for (i in seq_along(vars)) {
              tab[[i]] <- as.data.frame.matrix(res[[i]][1:4])
            }
            for (i in seq_along(vars)) {
              table <- self$results$sum[[i]]
              names <- dimnames(tab[[i]])[[1]]
              dims  <- dimnames(tab[[i]])[[2]]
              for (name in names) {
                row <- list()
                for (j in seq_along(dims)) {
                  row[[dims[j]]] <- tab[[i]][name, j]
                }
                table$addRow(rowKey = name, values = row)
              }
            }
          }
        }
        
        # Scores, scored file
        dicho_scored <- as.data.frame(dicho$scored) # dichotomous matrix
        
        # Total score
        if (isTRUE(self$options$total)) {
          score <- as.vector(dicho$score)
          self$results$total$setRowNums(rownames(data))
          self$results$total$setValues(score)
        }
        
        # Save scoring
        if (isTRUE(self$options$scoring)) {
          keys <- 1:length(self$options$vars)
          titles <- paste("Item", 1:length(self$options$vars))
          descriptions <- paste("Item", 1:length(self$options$vars))
          measureTypes <- rep("continuous", length(self$options$vars))
          
          self$results$scoring$set(
            keys = keys,
            titles = titles,
            descriptions = descriptions,
            measureTypes = measureTypes
          )
          
          self$results$scoring$setRowNums(rownames(data))
          
          scoring <- as.data.frame(dicho$scored)
          
          for (i in 1:length(self$options$vars)) {
            scores <- as.numeric(scoring[, i])
            self$results$scoring$setValues(index = i, scores)
          }
        }
        
        # Empirical ICC
        if (isTRUE(self$options$plot3)) {
          self$results$plot3$setState(dicho)
        }
        
        # Traditional item analysis table
        if (self$options$disc) {
          
          it <- ShinyItemAnalysis::ItemAnalysis(dicho_scored)
          
          dif <- it[1]
          RIT <- it[11]
          RIR <- it[10]
          
          # ULI is group-based: if grouping failed, set NA (keep others)
          if (group_ok) {
            ULI <- it[13]
          } else {
            ULI <- rep(NA_real_, length(dif))
          }
          
          discri <- data.frame(dif, ULI, RIT, RIR)
          
          dif <- discri$Difficulty
          ULI <- discri$ULI
          RIT <- discri$RIT
          RIR <- discri$RIR
          
          table <- self$results$disc
          vars <- self$options$vars
          for (i in seq_along(vars)) {
            row <- list()
            row[["dif"]] <-  dif[i]
            row[["ULI"]] <-  ULI[i]
            row[["RIT"]] <-  RIT[i]
            row[["RIR"]] <-  RIR[i]
            table$setRow(rowKey = vars[i], values = row)
          }
        }
        
        # Plot1
        if (isTRUE(self$options$plot1)) {
          self$results$plot1$setState(dicho_scored)
        }
        
        # Histogram of total score
        if (isTRUE(self$options$plot2)) {
          score <- rowSums(dicho$scored)
          df <- data.frame(score)
          state <- list(score, df)
          self$results$plot2$setState(state)
        }
      },
      
      .plot2 = function(image2, ggtheme, theme, ...) {
        if (is.null(image2$state))
          return(FALSE)
        
        score <- image2$state[[1]]
        df <- image2$state[[2]]
        
        cut <- median(score) # cut-score
        color <- c(rep("red", cut - min(score)), "gray", rep("blue", max(score) - cut))
        
        plot2 <- ggplot(df, aes(score)) +
          geom_histogram(binwidth = 1,
                         fill = color,
                         col = "black") +
          xlab("Total score") +
          ylab("Number of respondents") +
          ShinyItemAnalysis::theme_app()
        
        plot2 <- plot2 + ggtheme
        print(plot2)
        TRUE
      },
      
      .plot = function(image, ...) {
        
        # If 3-group failed, skip distractor plot cleanly
        if (isFALSE(isTRUE(private$.cache$group_ok)))
          return(FALSE)
        
        data <- self$data
        key1 <- strsplit(self$options$key, ',')[[1]]
        nums <- self$options$num
        group <- self$options$group
        
        plot <- tryCatch({
          ShinyItemAnalysis::plotDistractorAnalysis(data, key1, num.groups = group, item = nums)
        }, error = function(e) {
          
          msg <- paste0(
            "Plot could not be generated: ", e$message, "<br><br>",
            "This often happens when total scores have too few distinct values (many ties), ",
            "so group cut-points are not unique.<br>",
            "Group-based outputs were skipped."
          )
          
          self$results$instructions$setVisible(TRUE)
          self$results$instructions$setContent(private$.htmlwidget$generate_accordion(
            title = "⚠️ Plot skipped",
            content = paste(
              '<div style="border: 2px solid #ff9800; border-radius: 10px; padding: 12px; background-color: #fff3e0;">',
              '<p style="margin:0;">', msg, '</p>',
              '</div>'
            )
          ))
          
          return(NULL)
        })
        
        if (is.null(plot))
          return(FALSE)
        
        print(plot)
        TRUE
      },
      
      .plot1 = function(image, ggtheme, theme, ...) {
        if (is.null(image$state))
          return(FALSE)
        
        dicho <- image$state
        disi <- self$options$disi
        
        plot1 <- ShinyItemAnalysis::DDplot(
          dicho,
          discrim = disi,
          k = 3,
          l = 1,
          u = 3
        )
        
        if (self$options$angle > 0) {
          plot1 <- plot1 + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = self$options$angle, hjust = 1))
        }
        
        print(plot1)
        TRUE
      },
      
      .plot3 = function(image3, ...) {
        if (is.null(image3$state))
          return(FALSE)
        
        num1 <- self$options$num1
        myScores <- image3$state
        
        plot3 <- CTT::cttICC(
          myScores$score,
          myScores$scored[, num1],
          plotTitle = '',
          colTheme = "spartans",
          cex = 1.5
        )
        
        print(plot3)
        TRUE
      },
      
      .computeRES = function(data, key1) {
        
        # ---- scoring always first
        dicho <- CTT::score(data, key1, output.scored = TRUE)
        private$.cache$dicho <- dicho
        
        # ---- counts/prop are NOT group-based in our fallback plan
        # Always compute them from raw responses (so they never crash the whole analysis)
        private$.cache$counts <- tryCatch(
          CTT::distractor.analysis(data, key1),
          error = function(e) NULL
        )
        
        private$.cache$prop <- tryCatch(
          CTT::distractor.analysis(data, key1, p.table = TRUE),
          error = function(e) NULL
        )
        
        # ---- group status for group-based outputs only
        private$.cache$group_ok  <- TRUE
        private$.cache$group_err <- NULL
        
        safe_group <- function(expr) {
          tryCatch(expr, error = function(e) {
            if (grepl("breaks.*not unique", e$message, ignore.case = TRUE) ||
                grepl("not unique.*breaks", e$message, ignore.case = TRUE)) {
              private$.cache$group_ok  <<- FALSE
              private$.cache$group_err <<- e$message
              return(NULL)
            }
            stop(e)
          })
        }
        
        # group-based outputs (may fail; if so, we keep running)
        private$.cache$res <- safe_group(CTT::distractorAnalysis(data, key1))
        
        private$.cache$ts  <- safe_group(
          CTT::distractorAnalysis(data, key1, defineGroups = c(0.33, 0.34, 0.33))
        )
      }
    )
  )

# This file is a generated template, your changes will not be overwritten
#FACET ANALYSIS
#' @import ggplot2

facetClass <- if (requireNamespace('jmvcore', quietly = TRUE))
  R6::R6Class(
    "facetClass",
    inherit = facetBase,
    private = list(
      .allCache = NULL,
      .residualCache = NULL, 
      .htmlwidget = NULL,
      
      .init = function() {
        private$.htmlwidget <- HTMLWidget$new()
        
        if (is.null(self$data) | is.null(self$options$facet)) {
          self$results$instructions$setVisible(visible = TRUE)
          
        }
        self$results$instructions$setContent(private$.htmlwidget$generate_accordion(
          title = "Instructions",
          content = paste(
            '<div style="border: 2px solid #e6f4fe; border-radius: 15px; padding: 15px; background-color: #e6f4fe; margin-top: 10px;">',
            '<div style="text-align:justify;">',
            '<ul>',
            '<li>If your data format is in wide, you need to convert it to <b>long format</b> in order to run analysis.</li>',
            '<li>The variables should be named <b>subject</b>,<b>rater</b> and <b>task</b> respectively. Any other variable name will result in an error message.</b></li>',
            '<li>In the Facet variable box, you must put the variable <b>rater</b> first.</li>',
            '<li>You can currently only put <b>two variables</b> in the Facet variable box.</li>',
            '<li>Empty PCA or Local Dependency tables indicate insufficient variance in residuals or high missing values in the data.</li>',
            '<li>We recommend using <a href="https://www.winsteps.com" target = "_blank">Facet software</a> for analyzing various experimental designs.</li>',
            '<li>Feature requests and bug reports can be made on my <a href="https://github.com/hyunsooseol/snowIRT/issues" target="_blank">GitHub</a>.</li>',
            '</ul></div></div>'
          )
        ))
        
        if (self$options$ifit)
          self$results$ifit$setNote("Note",
                                    "Display 'X' when both Infit and Outfit values exceed 1.5.")
        if (self$options$pfit)
          self$results$pfit$setNote("Note",
                                    "Display 'X' when both Infit and Outfit values exceed 1.5.")
        
        if (self$options$local)
          self$results$local$setNote("Note",
                                      "|Loading| > 0.3 indicates potential local dependency; > 0.5 indicates strong dependency requiring attention.")
        
      },
      .run = function() {
        
        #---------------------------------------------
        if (is.null(self$options$dep) ||
            is.null(self$options$id) ||
            is.null(self$options$facet))
          return()
        
        if (is.null(private$.allCache)) {
          private$.allCache <- private$.computeRES()
        }
        res <- private$.allCache
        
        # Facet estimates--------------------------
        
        res1 <- res$xsi.facets # Whole estimates
        
        # Task measure----------------------------
        im <- subset(res1, res1$facet == "task")
        im$parameter <-  gsub("task", "", im$parameter)
        
        # rater measure----------
        rm <- subset(res1, res1$facet == "rater")
        
        #interaction(Raw score)-----------------
        
        if (isTRUE(self$options$raw)) {
          para <- res$item$item
          score <- res$item$M
          raw <- data.frame(para, score)
          
          raw$para <-  gsub("task", "", raw$para)
          raw$para <-  gsub("-rater", "rater", raw$para)
          raw <- raw |> tidyr::separate(para, c("rater", "item"), "-")
          
          table <- self$results$raw
          names <- dimnames(raw)[[1]]
          for (name in names) {
            row <- list()
            row[["rater"]]   <-  raw[name, 1]
            row[["task"]]   <-  raw[name, 2]
            row[["score"]] <-  raw[name, 3]
            table$addRow(rowKey = name, values = row)
          }
        }
        # interaction measure-------
        inter <- subset(res1, res1$facet == "rater:task")
        
        inter <- inter |> tidyr::separate(parameter, c("rater", "task"), ":")
        inter$task <-  gsub("task", "", inter$task)
        inter <- data.frame(inter$rater, inter$task, inter$xsi, inter$se.xsi)
        colnames(inter) <- c("Rater", "Task", "Measure", "SE")
        # step measure-----------
        sm <- subset(res1, res1$facet == "step")
        
        # Person ability----------
        persons <- TAM::tam.wle(res)
        
        per <- data.frame(persons$pid,
                          persons$PersonScores,
                          persons$theta,
                          persons$error,
                          persons$WLE.rel)
        
        # WLE Reliability-------
        pw <- as.vector(per[[5]])[1]
        self$results$text$setContent(pw)
        
        # Wrightmap plot---------
        
        if (isTRUE(self$options$plot4)) {
          itemm <- data.frame(im$parameter, im$xsi)
          
          # added rater measure into item-------
          rmm <- subset(res1, res1$facet == "rater")
          rmm <- data.frame(rmm$parameter, rmm$xsi)
          colnames(rmm) <- c("im.parameter", "im.xsi")
          itemm <- rbind(itemm, rmm)
          #---------------------------------
          colnames(itemm) <- c("vars", "measure")
          itemm$vars <-  gsub("task", "", itemm$vars)
          #self$results$text1$setContent(itemm)
          vars <- as.vector(itemm[[1]])
          ime <- as.vector(itemm[[2]])
          pme <- as.vector(per[[3]])
          
          image <- self$results$plot4
          state <- list(pme, ime, vars)
          image$setState(state)
        }
        
        # Task measure table----------------
        if (isTRUE(self$options$im)) {
          table <- self$results$im
          im <- as.data.frame(im)
          items <- as.vector(im[[1]])
          stat_indices <- c(measure = 3, se = 4)
          for (i in seq_along(items)) {
            row <- setNames(lapply(stat_indices, function(idx)
              im[[idx]][i]),
              names(stat_indices))
            table$addRow(rowKey = items[i], values = row)
          }
          
          # table <- self$results$im
          #
          # im <- as.data.frame(im)
          # dif <- as.vector(im[[3]])
          # se <- as.vector(im[[4]])
          #
          # items <- as.vector(im[[1]])
          # for (i in seq_along(items)) {
          #   row <- list()
          #   row[["measure"]] <- dif[i]
          #   row[["se"]] <- se[i]
          #   table$addRow(rowKey = items[i], values = row)
          # }
        }
        # Item bar plot----------
        if (isTRUE(self$options$plot2)) {
          im <- as.data.frame(im)
          colnames(im) <- c("Task", "facet", "Value", "SE")
          # Rater bar plot--------
          image <- self$results$plot2
          image$setState(im)
        }
        
        # Rater measure table----------------
        if (isTRUE(self$options$rm)) {
          table <- self$results$rm
          rm <- as.data.frame(rm)
          items <- as.vector(rm[[1]])
          stat_indices <- c(measure = 3, se = 4)
          for (i in seq_along(items)) {
            row <- setNames(lapply(stat_indices, function(idx)
              rm[[idx]][i]),
              names(stat_indices))
            table$addRow(rowKey = items[i], values = row)
          }
          
          # table <- self$results$rm
          #
          # rm <- as.data.frame(rm)
          # dif <- as.vector(rm[[3]])
          # se <- as.vector(rm[[4]])
          # items <- as.vector(rm[[1]])
          # for (i in seq_along(items)) {
          #   row <- list()
          #   row[["measure"]] <- dif[i]
          #   row[["se"]] <- se[i]
          #   table$addRow(rowKey = items[i], values = row)
          # }
        }
        
        # Rater bar plot----------
        
        if (isTRUE(self$options$plot1)) {
          rm <- as.data.frame(rm)
          colnames(rm) <- c("Rater", "facet", "Value", "SE")
          
          # Rater bar plot--------
          image <- self$results$plot1
          image$setState(rm)
        }
        # Interaction measure table----------------
        if (isTRUE(self$options$inter)) {
          table <- self$results$inter
          inter <- as.data.frame(inter)
          row_names <- rownames(inter)
          stat_indices <- c(
            rater = 1,
            task = 2,
            measure = 3,
            se = 4
          )
          for (name in row_names) {
            row <- setNames(lapply(stat_indices, function(idx)
              inter[name, idx]),
              names(stat_indices))
            table$addRow(rowKey = name, values = row)
          }
        }
        
        # Interaction plot--------------
        image <- self$results$plot3
        image$setState(inter)
        
        # Step measure table----------------
        if (isTRUE(self$options$sm)) {
          table <- self$results$sm
          sm <- as.data.frame(sm)
          items <- as.vector(sm[[1]])
          stat_indices <- c(measure = 3, se = 4)
          for (i in seq_along(items)) {
            row <- setNames(lapply(stat_indices, function(idx)
              sm[[idx]][i]),
              names(stat_indices))
            table$addRow(rowKey = items[i], values = row)
          }
        }
        # Interaction fit table------------
        # fit is shown for the rater*item combinations
        
        ifit <- TAM::msq.itemfit(res)
        ifit <- as.data.frame(ifit$itemfit)
        ifit <- dplyr::select(ifit, c("item", "Outfit", "Infit"))
        
        # THe order !!!(rater * item), otherwise table will be empty!!!
        ifit$item <-  gsub("-rater", "rater", ifit$item)
        ifit$item <-  gsub("task", "", ifit$item)
        ifit <- ifit |> tidyr::separate(item, c("rater", "task"), "-")
        
        ifit <- data.frame(ifit)
        
        # Display '*' when both infit and outfit values exceed 1.5
        ifit$marker <- ifelse(ifit$Outfit > 1.5 &
                                ifit$Infit > 1.5, 'X', '')
        
        # Item fit table------------
        if (isTRUE(self$options$ifit)) {
          table <- self$results$ifit
          row_names <- rownames(ifit)
          stat_indices <- c(
            rater = 1,
            task = 2,
            outfit = 3,
            infit = 4,
            marker = 5
          )
          for (name in row_names) {
            row <- setNames(lapply(stat_indices, function(idx)
              ifit[name, idx]),
              names(stat_indices))
            table$addRow(rowKey = name, values = row)
          }
          # table <- self$results$ifit
          #
          # names <- dimnames(ifit)[[1]]
          # for (name in names) {
          #   row <- list()
          #   row[["rater"]]   <-  ifit[name, 1]
          #   row[["task"]]   <-  ifit[name, 2]
          #   row[["outfit"]] <-  ifit[name, 3]
          #   row[["infit"]] <-  ifit[name, 4]
          #   row[["marker"]] <-  ifit[name, 5]
          #   table$addRow(rowKey = name, values = row)
          # }
        }
        
        if (isTRUE(self$options$plot7)) {
          ifit <- TAM::msq.itemfit(res)
          ifit <- as.data.frame(ifit$itemfit)
          ifit <- dplyr::select(ifit, c("item", "Outfit", "Infit"))
          
          Index <- dimnames(ifit)[[1]]
          ifit$Index <- Index
          
          ifit <- dplyr::select(ifit, c("Outfit", "Infit", "Index"))
          
          ifit.plot <- reshape2::melt(
            ifit,
            id.vars = 'Index',
            variable.name = "Fit",
            value.name = 'Value'
          )
          image <- self$results$plot7
          image$setState(ifit.plot)
        }
        
        # Person measure table-------------
        if (isTRUE(self$options$pm)) {
          table <- self$results$pm
          row_names <- rownames(per)
          stat_indices <- c(ps = 2, pt = 3, pe = 4)
          for (name in row_names) {
            row <- setNames(lapply(stat_indices, function(idx)
              per[name, idx]),
              names(stat_indices))
            table$addRow(rowKey = name, values = row)
          }
        }
        
        # Person fit table-----------
        
        pfit <- TAM::tam.personfit(res)
        pfit <- data.frame(pfit$outfitPerson, pfit$infitPerson)
        
        names(pfit) <- c("outfit", "infit")
        
        # Display '*' when both infit and outfit values exceed 1.5
        pfit$marker <- ifelse(pfit$outfit > 1.5 &
                                pfit$infit > 1.5, 'X', '')
        
        if (isTRUE(self$options$pfit)) {
          table <- self$results$pfit
          
          row_names <- rownames(pfit)
          stat_indices <- c(outfit = 1,
                            infit = 2,
                            marker = 3)
          
          for (name in row_names) {
            row <- setNames(lapply(stat_indices, function(idx)
              pfit[name, idx]),
              names(stat_indices))
            table$addRow(rowKey = name, values = row)
          }
        }
        # Person fit plot------------------
        # Person ability----------
        # persons <- TAM::tam.wle(res)
        #
        # per <-data.frame(persons$pid, persons$PersonScores,
        #                  persons$theta, persons$error,
        #                  persons$WLE.rel)
        
    # Residual analysis (show all cases with |residual| > 2.0)
        if (isTRUE(self$options$resid)) {
          
          residuals_result <- private$.computeResiduals()
          stand_resid <- residuals_result$stand_residuals
          
          # Find all large residuals (absolute value > 2.0)
          large_residuals <- which(abs(stand_resid) > 2, arr.ind = TRUE)
          
          if (nrow(large_residuals) > 0) {
            table <- self$results$resid
            
            # Sort by absolute residual value (largest first)
            residual_values <- abs(stand_resid[large_residuals])
            sorted_indices <- order(residual_values, decreasing = TRUE)
            
            # Show ALL cases (no limit)
            n_show <- nrow(large_residuals)
            
            for (i in 1:n_show) {
              idx <- sorted_indices[i]
              row_idx <- large_residuals[idx, 1]
              col_idx <- large_residuals[idx, 2]
              residual_val <- stand_resid[row_idx, col_idx]
              
              # Simple case numbering
              case_name <- paste0("Case ", i)
              
              item_name <- if (!is.null(colnames(stand_resid))) {
                colnames(stand_resid)[col_idx]
              } else {
                paste0("Item_", col_idx)
              }
              
              interpretation <- if (residual_val > 0) "Overfit" else "Underfit"
              
              # Add row to table
              table$addRow(rowKey = i, values = list(
                case = case_name,
                item = item_name,
                residual = residual_val,
                interpretation = interpretation
              ))
            }
          }
        }
        
        # Principal Component Analysis of Residuals (무한값 처리 추가)
        if (isTRUE(self$options$pca)) {
          residuals_result <- private$.computeResiduals()
          stand_resid <- residuals_result$stand_residuals
          
          # Remove rows/columns with NA or infinite values
          valid_rows <- apply(stand_resid, 1, function(x) all(is.finite(x)))
          valid_cols <- apply(stand_resid, 2, function(x) all(is.finite(x)))
          clean_resid <- stand_resid[valid_rows, valid_cols, drop = FALSE]
          
          # Remove columns with zero variance
          if (ncol(clean_resid) > 0 && nrow(clean_resid) > 0) {
            col_vars <- apply(clean_resid, 2, var, na.rm = TRUE)
            valid_var_cols <- !is.na(col_vars) & col_vars > 1e-10
            clean_resid <- clean_resid[, valid_var_cols, drop = FALSE]
          }
          
          if (ncol(clean_resid) > 1 && nrow(clean_resid) > 1) {
            # Principal component analysis
            pca_result <- prcomp(clean_resid, scale. = FALSE)
            eigenvalues <- pca_result$sdev^2
            
            # Calculate variance explained
            total_variance <- sum(eigenvalues)
            variance_explained <- (eigenvalues / total_variance) * 100
            cumulative_variance <- cumsum(variance_explained)
            
            # Show first 5 components
            n_components <- min(5, length(eigenvalues))
            
            table <- self$results$pca
            
            for (i in 1:n_components) {
              table$addRow(rowKey = i, values = list(
                component = paste0("PC", i),
                eigenvalue = eigenvalues[i],
                variance_explained = variance_explained[i],
                cumulative_variance = cumulative_variance[i]
              ))
            }
          }
        }
        
        # Local Dependency Detection (무한값 처리 추가)
        if (isTRUE(self$options$local)) {
          residuals_result <- private$.computeResiduals()
          stand_resid <- residuals_result$stand_residuals
          
          # Remove rows/columns with NA or infinite values
          valid_rows <- apply(stand_resid, 1, function(x) all(is.finite(x)))
          valid_cols <- apply(stand_resid, 2, function(x) all(is.finite(x)))
          clean_resid <- stand_resid[valid_rows, valid_cols, drop = FALSE]
          
          # Remove columns with zero variance
          if (ncol(clean_resid) > 0 && nrow(clean_resid) > 0) {
            col_vars <- apply(clean_resid, 2, var, na.rm = TRUE)
            valid_var_cols <- !is.na(col_vars) & col_vars > 1e-10
            clean_resid <- clean_resid[, valid_var_cols, drop = FALSE]
          }
          
          if (ncol(clean_resid) > 1 && nrow(clean_resid) > 1) {
            # Principal component analysis for dependency detection
            pca_result <- prcomp(clean_resid, scale. = FALSE)
            
            # Get loadings on first principal component
            loadings <- pca_result$rotation[, 1]
            
            # Find items with high loadings (> 0.3)
            high_loading_items <- which(abs(loadings) > 0.3)
            
            if (length(high_loading_items) > 1) {
              table <- self$results$local
              
              # Create pairs of potentially dependent items
              item_pairs <- combn(high_loading_items, 2)
              
              for (i in 1:ncol(item_pairs)) {
                idx1 <- item_pairs[1, i]
                idx2 <- item_pairs[2, i]
                
                item1_name <- names(loadings)[idx1]
                item2_name <- names(loadings)[idx2]
                loading1 <- loadings[idx1]
                loading2 <- loadings[idx2]
                
                # Determine dependency strength
                avg_loading <- mean(abs(c(loading1, loading2)))
                dependency_strength <- if (avg_loading > 0.5) {
                  "Strong"
                } else if (avg_loading > 0.3) {
                  "Moderate"
                } else {
                  "Weak"
                }
                
                table$addRow(rowKey = i, values = list(
                  item1 = item1_name,
                  item2 = item2_name,
                  loading1 = loading1,
                  loading2 = loading2,
                  dependency_strength = dependency_strength
                ))
              }
            }
          }
        }
        
        
        
        if (isTRUE(self$options$plot8)) {
          pfit <- TAM::tam.personfit(res)
          pfit <- data.frame(pfit$outfitPerson, pfit$infitPerson)
          names(pfit) <- c("outfit", "infit")
          pfit$Measure <- per$persons.theta
          pf <- reshape2::melt(
            pfit,
            id.vars = 'Measure',
            variable.name = "Fit",
            value.name = 'Value'
          )
          image <- self$results$plot8
          image$setState(pf)
        }
      },
      #----------------------------------------------------
      .plot1 = function(image, ggtheme, theme, ...) {
        if (is.null(image$state))
          return(FALSE)
        
        rm <- image$state
        
        fill <- theme$fill[2]
        color <- theme$color[1]
        
        plot1 <- ggplot(data = rm, aes(x = Rater, y = Value)) +
          
          geom_bar(
            stat = "identity",
            # position="dodge",
            width = 0.7,
            fill = fill,
            color = color
          ) +  theme_bw() + coord_flip()
        plot1 + ggtheme
        print(plot1)
        TRUE
      },
      
      .plot2 = function(image, ggtheme, theme, ...) {
        if (is.null(image$state))
          return(FALSE)
        
        im <- image$state
        
        fill <- theme$fill[2]
        color <- theme$color[1]
        
        plot2 <- ggplot(data = im, aes(x = Task, y = Value)) +
          geom_bar(
            stat = "identity",
            #position="dodge",
            width = 0.7,
            fill = fill,
            color = color
          ) +  theme_bw() + coord_flip()
        plot2 + ggtheme
        print(plot2)
        TRUE
      },
      
      .plot3 = function(image, ggtheme, theme, ...) {
        if (is.null(image$state))
          return(FALSE)
        
        inter <- image$state
        
        plot3 <- ggplot(inter, aes(x = Task, y = Measure, group = Rater)) +
          geom_line(size = 1.2, aes(color = Rater)) +
          geom_point(size = 3, aes(color = Rater)) +  theme_bw()
        if (self$options$angle > 0) {
          plot3 <- plot3 + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = self$options$angle, hjust = 1))
        }
        plot3 + ggtheme
        print(plot3)
        TRUE
      },
      
      .plot4 = function(image, ...) {
        if (is.null(image$state))
          return(FALSE)
        
        personmeasure <- image$state[[1]]
        imeasure <- image$state[[2]]
        vars <- image$state[[3]]
        
        plot4 <- ShinyItemAnalysis::ggWrightMap(
          personmeasure,
          imeasure,
          item.names = vars,
          binwidth = 0.5,
          # size=18,
          ylab.b = "Facet measure",
          rel_widths = c(1, 1.5),
          color = "deepskyblue"
        )
        
        print(plot4)
        TRUE
      },
      # Expected score curves-------------------
      
      .plot5 = function(image, ...) {
        num <- self$options$num
        if (!self$options$plot5)
          return(FALSE)
        res <- private$.allCache
        
        plot5 <- plot(res,
                      items = num,
                      type = "expected",
                      export = FALSE)
        print(plot5)
        TRUE
      },
      
      # Item response curve-------------------
      
      .plot6 = function(image, ...) {
        num1 <- self$options$num1
        if (!self$options$plot6)
          return(FALSE)
        res <- private$.allCache
        plot6 <- plot(res,
                      items = num1,
                      type = "items",
                      export = FALSE)
        print(plot6)
        TRUE
      },
      
      
      # interaction fit plot--------------
    .plot7 = function(image, ggtheme, theme, ...) {
      if (is.null(image$state))
        return(FALSE)
      
      ifit <- image$state
      
      plot7 <- ggplot2::ggplot(ifit, aes(x = Index, y = Value, shape = Fit, color = Fit)) +
        # Modern point styling with refined shapes
        geom_point(
          size = 2.5,
          stroke = 1,
          alpha = 0.75
        ) +
        # Elegant shape and color mapping
        ggplot2::scale_shape_manual(
          values = c("Infit" = 16, "Outfit" = 17),  # Circle and triangle
          name = "Fit"
        ) +
        ggplot2::scale_color_manual(
          values = c("Infit" = '#2980b9', "Outfit" = '#e74c3c'),
          name = "Fit"
        ) +
        # Clean boundary lines
        ggplot2::geom_hline(
          yintercept = 1.5,
          linetype = "solid",
          color = '#95a5a6',
          linewidth = 0.8,
          alpha = 0.8
        ) +
        ggplot2::geom_hline(
          yintercept = 0.5,
          linetype = "solid",
          color = '#95a5a6',
          linewidth = 0.8,
          alpha = 0.8
        ) +
        labs(
          title = "",
          x = "Rater × Task",
          y = "Values"
        ) +
        theme_minimal() +
        theme(
          # Background settings
          panel.background = element_rect(fill = "#fafafa", color = NA),
          plot.background = element_rect(fill = "white", color = NA),
          
          # Grid line settings
          panel.grid.major.y = element_line(color = "#e8e8e8", linewidth = 0.4),
          panel.grid.major.x = element_blank(),
          panel.grid.minor = element_blank(),
          
          # Title and axis label styling
          plot.title = element_text(
            hjust = 0.5,
            size = 15,
            face = "bold",
            color = "#2c3e50",
            margin = margin(b = 15)
          ),
          axis.title = element_text(size = 11, color = "#34495e"),
          axis.text = element_text(size = 9.5, color = "#7f8c8d"),
          axis.text.x = element_text(margin = margin(t = 6)),
          axis.text.y = element_text(margin = margin(r = 6)),
          
          # Legend styling
          legend.position = "right",
          legend.title = element_text(size = 11, color = "#34495e", face = "bold"),
          legend.text = element_text(size = 10, color = "#7f8c8d"),
          legend.background = element_rect(fill = "white", color = "#ecf0f1", linewidth = 0.3),
          legend.key = element_rect(fill = "transparent"),
          legend.margin = margin(10, 10, 10, 10),
          
          # Border settings
          panel.border = element_rect(color = "#bdc3c7", fill = NA, linewidth = 0.4),
          
          # Margin adjustments
          plot.margin = margin(15, 15, 15, 15)
        )
      
      plot7 <- plot7 + ggtheme
      
      if (self$options$angle1 > 0) {
        plot7 <- plot7 + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = self$options$angle1, hjust = 1))
      }
      print(plot7)
      TRUE
    },
    
    .plot8 = function(image, ggtheme, theme, ...) {
      if (is.null(image$state))
        return(FALSE)
      
      pf <- image$state
      
      plot8 <- ggplot2::ggplot(pf, aes(x = Measure, y = Value, shape = Fit, color = Fit)) +
        # Modern point styling with refined shapes
        geom_point(
          size = 2.8,
          stroke = 1.2,
          alpha = 0.8
        ) +
        # Elegant shape and color mapping
        ggplot2::scale_shape_manual(
          values = c(16, 17),  # Circle and triangle instead of + and X
          name = "Fit"
        ) +
        ggplot2::scale_color_manual(
          values = c('#2980b9', '#e74c3c'),  # Blue and red
          name = "Fit"
        ) +
        # Clean boundary lines
        ggplot2::geom_hline(
          yintercept = 1.5,
          linetype = "solid",
          color = '#95a5a6',
          linewidth = 0.8,
          alpha = 0.8
        ) +
        ggplot2::geom_hline(
          yintercept = 0.5,
          linetype = "solid",
          color = '#95a5a6',
          linewidth = 0.8,
          alpha = 0.8
        ) +
        ggplot2::coord_cartesian(xlim = c(-4, 4), ylim = c(0, 3)) +
        ggtitle("") +
        labs(
          x = "Measure",
          y = "Value"
        ) +
        theme_minimal() +
        theme(
          # Background settings
          panel.background = element_rect(fill = "#fafafa", color = NA),
          plot.background = element_rect(fill = "white", color = NA),
          
          # Grid line settings
          panel.grid.major = element_line(color = "#f0f0f0", linewidth = 0.3),
          panel.grid.minor = element_blank(),
          
          # Title and axis label styling
          plot.title = element_text(
            hjust = 0.5,
            size = 15,
            face = "bold",
            color = "#2c3e50",
            margin = margin(b = 15)
          ),
          axis.title = element_text(size = 11, color = "#34495e"),
          axis.text = element_text(size = 9.5, color = "#7f8c8d"),
          
          # Legend styling
          legend.position = "right",
          legend.title = element_text(size = 11, color = "#34495e", face = "bold"),
          legend.text = element_text(size = 10, color = "#7f8c8d"),
          legend.background = element_rect(fill = "white", color = "#ecf0f1", linewidth = 0.3),
          legend.key = element_rect(fill = "transparent"),
          legend.margin = margin(10, 10, 10, 10),
          
          # Border settings
          panel.border = element_rect(color = "#bdc3c7", fill = NA, linewidth = 0.4),
          
          # Margin adjustments
          plot.margin = margin(15, 15, 15, 15)
        )
      
      plot8 <- plot8 + ggtheme
      print(plot8)
      TRUE
    },
    

      # Optimized computation function
      .computeRES = function() {
        dep <- self$options$dep
        id <- self$options$id
        facets <- self$options$facet
        
        # Extract only needed columns to reduce memory usage
        needed_cols <- unique(c(dep, id, facets))
        data <- stats::na.omit(self$data[, needed_cols, drop = FALSE])
        
        # Create formula more efficiently
        facets_formula <- paste0(facets, collapse = '*')
        formula <- stats::as.formula(paste0('~ step+', facets_formula))
        
        # Extract facet data once
        facet_data <- data[, facets, drop = FALSE]
        
        # Run main computation
        res <- TAM::tam.mml.mfr(
          resp = data[[dep]],
          facets = facet_data,
          pid = data[[id]],
          formulaA = formula
        )
        # Clean up to free memory
        rm(data, facet_data)
        gc(verbose = FALSE)
        return(res)
      },
      
    .computeResiduals = function() {
      if (is.null(private$.allCache)) {
        stop("Model result not available. Run analysis first.")
      }
      
      if (is.null(private$.residualCache)) {
        res <- private$.allCache
        private$.residualCache <- TAM::IRT.residuals(res)
      }
      
      return(private$.residualCache)
    }
  
     )
  )

# Example------------------------------
# Wide to long for dataset using reshape packate
# data <- read.csv("guilford.csv")
# attach(data)
# long<- reshape::melt(data, id.vars =c("subject","rater"),
#                      variable_name = "task")


#-----------------------------------------
# facet<- read.csv("long.csv")
# attach(facet)
# formula <- ~ rater*task+step
# facets = dplyr::select(facet, rater:task)
#
# res <- TAM::tam.mml.mfr(value,
#                         facets =  facets,
#                         formulaA = formula,
#                         pid=subject)
# res1 <- res$xsi.facets

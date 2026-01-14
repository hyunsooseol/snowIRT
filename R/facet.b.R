# This file is a generated template, your changes will not be overwritten
# FACET ANALYSIS
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
            '<li><b>Do not put the Time variable in the Facet box.</b> Specify Time only in the <b>Time</b> option to estimate rater × time drift.</li>',
            '<li>Empty PCA or Local Dependency tables may occur when the residual matrix is sparse (e.g., unbalanced designs where not all students are rated by all rater × task combinations), when residual variance is near-zero, or when non-finite residuals are produced.</li>',
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
        
      
        if (self$options$tes)
          self$results$driftsum$setNote(
            "Note",
            "⚠ indicates a practically meaningful rater drift (|Δ| ≥ 0.5 logits)."
          )
        
        
        },
      .run = function() {
        
        #---------------------------------------------
        if (is.null(self$options$dep) ||
            is.null(self$options$id) ||
            is.null(self$options$facet))
          return()
        
        # Always recompute to ensure changes in Time/drift options are reflected
        private$.allCache <- private$.computeRES()
        res <- private$.allCache
        
        # Facet estimates--------------------------
        
        res1 <- res$xsi.facets # Whole estimates
        
        # Task measure----------------------------
        im <- subset(res1, res1$facet == "task")
        im$parameter <-  gsub("task", "", im$parameter)
        
        # rater measure----------
        rm <- subset(res1, res1$facet == "rater")
        
        # interaction(Raw score)-----------------
        
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
        

        # =========================================================
        # Rater × Time (Drift)
        # =========================================================
        if (!is.null(self$options$time)) {
          
          # (A) Drift - Measure (rater:time)
          if (isTRUE(self$options$driftm)) {
            
            drift <- subset(res1, res1$facet %in% c("rater:time", "time:rater"))
            
            if (nrow(drift) > 0) {
              
              drift <- drift |> tidyr::separate(parameter, c("rater", "time"),
                                                sep = ":", remove = FALSE)
              
              drift$time <- gsub("^time", "", drift$time)
              
              drift_out <- data.frame(
                rater = drift$rater,
                time  = drift$time,
                est   = drift$xsi,
                se    = drift$se.xsi
              )
              
              table <- self$results$driftm
              for (i in seq_len(nrow(drift_out))) {
                table$addRow(
                  rowKey = i,
                  values = list(
                    rater = drift_out$rater[i],
                    time  = drift_out$time[i],
                    est   = drift_out$est[i],
                    se    = drift_out$se[i]
                  )
                )
              }
            }
          }
        
          # (B) Drift - Fit (rater × time)
          if (isTRUE(self$options$driftfit)) {
            
            ifit_rt <- tryCatch({
              
              # ---- 1. item-level fit from TAM
              ff <- TAM::msq.itemfit(res)
              df <- as.data.frame(ff$itemfit)
              
              if (!all(c("item", "Outfit", "Infit") %in% names(df)))
                return(NULL)
              
              # ---- 2. parse rater and time from item labels
              # item labels look like: rater1-time t1-task k3 (or similar)
              extract <- function(x, key) {
                m <- regexpr(paste0(key, "[^:\\-\\._\\s]+"), x, perl = TRUE)
                ifelse(m > 0, regmatches(x, m), NA_character_)
              }
              
              df$rater <- extract(df$item, "rater")
              df$time  <- extract(df$item, "time")
              df$time  <- gsub("^time", "", df$time)   # -> t1, t2, ...
              
              # ---- 3. Aggregate to rater × time
              agg_fit <- df |>
                dplyr::filter(!is.na(rater), !is.na(time)) |>
                dplyr::group_by(rater, time) |>
                dplyr::summarise(
                  infit  = mean(Infit,  na.rm = TRUE),
                  outfit = mean(Outfit, na.rm = TRUE),
                  .groups = "drop"
                )
              
              # ---- 4. True N from raw data
              dep    <- self$options$dep
              id     <- self$options$id
              facets <- self$options$facet
              timev  <- self$options$time
              
              dat0 <- stats::na.omit(self$data[, c(dep, id, facets, timev), drop = FALSE])
              
              rater_var <- facets[[1]]
              
              # rater 라벨을 itemfit과 같은 형태로 강제: "rater1", "rater2", ...
              dat0$rater_norm <- as.character(dat0[[rater_var]])
              dat0$rater_norm <- ifelse(grepl("^rater", dat0$rater_norm),
                                        dat0$rater_norm,
                                        paste0("rater", dat0$rater_norm))
              
              # time 라벨: 이미 t로 시작하면 유지, 아니면 t를 붙임  (tt1 방지)
              tt <- as.character(dat0[[timev]])
              tt <- ifelse(grepl("^t", tt), tt, paste0("t", tt))
              dat0$time_norm <- tt
              
              n_by_rt <- dat0 |>
                dplyr::group_by(rater = rater_norm, time = time_norm) |>
                dplyr::summarise(n = dplyr::n(), .groups = "drop")
              
              out <- dplyr::left_join(agg_fit, n_by_rt, by = c("rater", "time"))              
              
              out
              
            }, error = function(e) NULL)
            
            if (!is.null(ifit_rt) && nrow(ifit_rt) > 0) {
              table <- self$results$driftfit
              for (i in seq_len(nrow(ifit_rt))) {
                table$addRow(
                  rowKey = i,
                  values = list(
                    rater  = ifit_rt$rater[i],
                    time   = ifit_rt$time[i],
                    infit  = ifit_rt$infit[i],
                    outfit = ifit_rt$outfit[i],
                    n      = ifit_rt$n[i]
                  )
                )
              }
            }
          }
          
          
          # (C) Drift Summary (대안 1)  : tes 체크박스 재활용
          if (isTRUE(self$options$tes)) {
            
            drift <- subset(res1, res1$facet %in% c("rater:time", "time:rater"))
            
            if (nrow(drift) > 0) {
              
              drift <- drift |> tidyr::separate(parameter, c("rater", "time"),
                                                sep = ":", remove = FALSE)
              
              drift$time_raw <- gsub("^time", "", drift$time)
              
              # ---- 시간 정렬: 숫자로 변환 가능하면 numeric, 아니면 factor 순서
              suppressWarnings(tnum <- as.numeric(as.character(drift$time_raw)))
              if (all(is.finite(tnum))) {
                drift$time_ord <- tnum
              } else {
                drift$time_ord <- as.numeric(factor(drift$time_raw, levels = unique(drift$time_raw)))
              }
              
              # rater별로 요약 계산
              by_rater <- split(drift, drift$rater)
              
              out <- lapply(names(by_rater), function(rt) {
                d <- by_rater[[rt]]
                d <- d[order(d$time_ord), , drop = FALSE]
                
                sev <- d$xsi
                times <- as.character(d$time_raw)
                
                rng <- if (length(sev) >= 2) max(sev, na.rm = TRUE) - min(sev, na.rm = TRUE) else NA_real_
                
                # 인접 시점 Δ
                max_abs_delta <- if (length(sev) >= 2) {
                  max(abs(diff(sev)), na.rm = TRUE)
                } else {
                  NA_real_
                }
                
                # 실무적 플래그 기준 (원하면 0.3/0.5를 옵션화 가능)
                flag <- if (!is.na(max_abs_delta) && max_abs_delta >= 0.5) "⚠" else ""
                
                data.frame(
                  rater    = rt,
                  tmin     = times[1],
                  tmax     = times[length(times)],
                  range    = rng,
                  maxdelta = max_abs_delta,
                  flag     = flag,
                  stringsAsFactors = FALSE
                )
              })
              
              out <- do.call(rbind, out)
              
              table <- self$results$driftsum
              for (i in seq_len(nrow(out))) {
                table$addRow(
                  rowKey = i,
                  values = list(
                    rater    = out$rater[i],
                    tmin     = out$tmin[i],
                    tmax     = out$tmax[i],
                    range    = out$range[i],
                    maxdelta = out$maxdelta[i],
                    flag     = out$flag[i]
                  )
                )
              }
            }
          }
        
          # (d) Drift plot (Rater × Time severity over time)
          if (isTRUE(self$options$plot9)) {
            
            drift <- subset(res1, res1$facet %in% c("rater:time", "time:rater"))
            
            if (nrow(drift) > 0) {
              
              drift <- drift |>
                tidyr::separate(parameter, c("rater", "time"), sep = ":", remove = FALSE)
              
              drift$time <- gsub("^time", "", drift$time)
              
              drift_plot <- data.frame(
                Rater    = as.character(drift$rater),
                Time     = as.character(drift$time),
                Severity = drift$xsi,
                SE       = drift$se.xsi,
                stringsAsFactors = FALSE
              )
              
              # --- Force x-axis to integer scale (preferred for drift interpretation)
              suppressWarnings(tnum <- as.numeric(drift_plot$Time))
              if (all(!is.na(tnum))) {
                drift_plot$TimeNum <- as.integer(tnum)
              } else {
                # fallback: keep original appearance order but map to integers
                drift_plot$TimeNum <- as.integer(factor(drift_plot$Time, levels = unique(drift_plot$Time)))
              }
              
              # sort for stable line drawing
              drift_plot <- drift_plot[order(drift_plot$Rater, drift_plot$TimeNum), , drop = FALSE]
              
              image <- self$results$plot9
              image$setState(drift_plot)
            }
          }
          
          
          }


        # Interaction fit table------------
        # fit is shown for the rater*item combinations
        
        ifit <- TAM::msq.itemfit(res)
        ifit <- as.data.frame(ifit$itemfit)
        ifit <- dplyr::select(ifit, c("item", "Outfit", "Infit"))
        
        # The order (rater * item) is important, otherwise the table will be empty
        ifit$item <-  gsub("-rater", "rater", ifit$item)
        ifit$item <-  gsub("task", "", ifit$item)
        ifit <- ifit |> tidyr::separate(item, c("rater", "task"), "-")
        
        ifit <- data.frame(ifit)
        
        # Display 'X' when both infit and outfit values exceed 1.5
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
        
        # Display 'X' when both infit and outfit values exceed 1.5
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
            
            # Show all cases (no limit)
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
        
        # Principal Component Analysis of Residuals (handling non-finite values)
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
        
        # Local Dependency Detection (handling non-finite values)
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
        
        if (!self$options$plot5)
          return(FALSE)
        
        res <- private$.allCache
        if (is.null(res)) {
          graphics::plot.new()
          graphics::text(0.5, 0.5, "Model is not available.", cex = 1.0)
          return(TRUE)
        }
        
        num <- suppressWarnings(as.integer(self$options$num))
        if (is.na(num) || num < 1) {
          graphics::plot.new()
          graphics::text(0.5, 0.5, "Select a valid item index.", cex = 1.0)
          return(TRUE)
        }
        
        tryCatch({
          p <- plot(res, items = num, type = "expected", export = FALSE)
          if (!is.null(p))
            print(p)
          TRUE
        }, error = function(e) {
          graphics::plot.new()
          graphics::text(0.5, 0.5, paste("Plot error:\n", e$message), cex = 0.9)
          TRUE
        })
      },
      
      
      
     
      # Item response curve-------------------
      .plot6 = function(image, ...) {
        
        if (!self$options$plot6)
          return(FALSE)
        
        res <- private$.allCache
        if (is.null(res)) {
          graphics::plot.new()
          graphics::text(0.5, 0.5, "Model is not available.", cex = 1.0)
          return(TRUE)
        }
        
        num1 <- suppressWarnings(as.integer(self$options$num1))
        if (is.na(num1) || num1 < 1) {
          graphics::plot.new()
          graphics::text(0.5, 0.5, "Select a valid item index.", cex = 1.0)
          return(TRUE)
        }
        
        tryCatch({
          p <- plot(res, items = num1, type = "items", export = FALSE)
          
          # plot() may return an object (lattice). If so, print it.
          if (!is.null(p))
            print(p)
          
          TRUE
        }, error = function(e) {
          graphics::plot.new()
          graphics::text(0.5, 0.5, paste("Plot error:\n", e$message), cex = 0.9)
          TRUE
        })
      },
      

      # interaction fit plot--------------
      .plot7 = function(image, ggtheme, theme, ...) {
        if (is.null(image$state))
          return(FALSE)
        
        ifit <- image$state
        
        plot7 <- ggplot2::ggplot(ifit, aes(x = Index, y = Value, shape = Fit, color = Fit)) +
          geom_point(
            size = 2.5,
            stroke = 1,
            alpha = 0.75
          ) +
          ggplot2::scale_shape_manual(
            values = c("Infit" = 16, "Outfit" = 17),
            name = "Fit"
          ) +
          ggplot2::scale_color_manual(
            values = c("Infit" = '#2980b9', "Outfit" = '#e74c3c'),
            name = "Fit"
          ) +
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
            panel.background = element_rect(fill = "#fafafa", color = NA),
            plot.background = element_rect(fill = "white", color = NA),
            panel.grid.major.y = element_line(color = "#e8e8e8", linewidth = 0.4),
            panel.grid.major.x = element_blank(),
            panel.grid.minor = element_blank(),
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
            legend.position = "right",
            legend.title = element_text(size = 11, color = "#34495e", face = "bold"),
            legend.text = element_text(size = 10, color = "#7f8c8d"),
            legend.background = element_rect(fill = "white", color = "#ecf0f1", linewidth = 0.3),
            legend.key = element_rect(fill = "transparent"),
            legend.margin = margin(10, 10, 10, 10),
            panel.border = element_rect(color = "#bdc3c7", fill = NA, linewidth = 0.4),
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
          geom_point(
            size = 2.8,
            stroke = 1.2,
            alpha = 0.8
          ) +
          ggplot2::scale_shape_manual(
            values = c(16, 17),
            name = "Fit"
          ) +
          ggplot2::scale_color_manual(
            values = c('#2980b9', '#e74c3c'),
            name = "Fit"
          ) +
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
            panel.background = element_rect(fill = "#fafafa", color = NA),
            plot.background = element_rect(fill = "white", color = NA),
            panel.grid.major = element_line(color = "#f0f0f0", linewidth = 0.3),
            panel.grid.minor = element_blank(),
            plot.title = element_text(
              hjust = 0.5,
              size = 15,
              face = "bold",
              color = "#2c3e50",
              margin = margin(b = 15)
            ),
            axis.title = element_text(size = 11, color = "#34495e"),
            axis.text = element_text(size = 9.5, color = "#7f8c8d"),
            legend.position = "right",
            legend.title = element_text(size = 11, color = "#34495e", face = "bold"),
            legend.text = element_text(size = 10, color = "#7f8c8d"),
            legend.background = element_rect(fill = "white", color = "#ecf0f1", linewidth = 0.3),
            legend.key = element_rect(fill = "transparent"),
            legend.margin = margin(10, 10, 10, 10),
            panel.border = element_rect(color = "#bdc3c7", fill = NA, linewidth = 0.4),
            plot.margin = margin(15, 15, 15, 15)
          )
        
        plot8 <- plot8 + ggtheme
        print(plot8)
        TRUE
      },
      
      
      .plot9 = function(image, ggtheme, theme, ...) {
        if (is.null(image$state))
          return(FALSE)
        
        df <- image$state
        
        # ---- Defensive checks
        if (!all(c("Rater", "TimeNum", "Severity") %in% names(df)))
          return(FALSE)
        if (nrow(df) == 0)
          return(FALSE)
        
        # Ensure correct types
        df$Rater   <- as.factor(df$Rater)
        df$TimeNum <- suppressWarnings(as.integer(df$TimeNum))
        if (all(is.na(df$TimeNum)))
          return(FALSE)
        
        # ---- Minimal drift plot (no error bars)
        p <- ggplot2::ggplot(
          df,
          ggplot2::aes(x = TimeNum, y = Severity, group = Rater, color = Rater)
        ) +
          ggplot2::geom_line(linewidth = 1.1) +
          ggplot2::geom_point(size = 2.6) +
          ggplot2::theme_bw() +
          ggplot2::labs(
            x = "Time",
            y = "Severity (logit)",
            title = ""
          )
        
        # Integer ticks on x-axis
        rng <- range(df$TimeNum, na.rm = TRUE)
        p <- p + ggplot2::scale_x_continuous(
          breaks = seq.int(from = rng[1], to = rng[2], by = 1)
        )
        
        p <- p + ggtheme
        print(p)
        TRUE
      },
      
      
      # Optimized computation function
      .computeRES = function() {
        dep    <- self$options$dep
        id     <- self$options$id
        facets <- self$options$facet
        timev  <- self$options$time
        
        if (is.null(dep) || is.null(id) || is.null(facets))
          return(NULL)
        
        # jamovi options → character vector
        facets_vec <- unlist(facets)
        facets_vec <- as.character(facets_vec)
        facets_vec <- facets_vec[!is.na(facets_vec) & nzchar(facets_vec)]
        
        # ---- Pull only needed columns (original time column included if selected)
        needed_cols <- unique(c(dep, id, facets_vec, timev))
        dat <- stats::na.omit(self$data[, needed_cols, drop = FALSE])
        
        # ---- Ensure facets are factors
        for (v in unique(c(facets_vec, timev))) {
          if (!is.null(v) && nzchar(v))
            dat[[v]] <- as.factor(dat[[v]])
        }
        
        # ---------------------------------------------------------
        # IMPORTANT: keep variable name 'timev' (do NOT create .time_fac),
        # but recode its LEVELS safely as "t1", "t2", ...
        # This prevents TAM duplicate row.names while preserving facet labels rater:time
        # ---------------------------------------------------------
        if (!is.null(timev) && nzchar(timev)) {
          dat[[timev]] <- as.factor(paste0("t", as.character(dat[[timev]])))
        }
        
        # ---- Identify rater/task
        rater_var <- facets_vec[1]
        task_var  <- if (length(facets_vec) >= 2) facets_vec[2] else NULL
        
        # ---- Build facet_data (use original facets + optional timev)
        facet_vars <- facets_vec
        if (!is.null(timev) && nzchar(timev))
          facet_vars <- unique(c(facet_vars, timev))
        facet_data <- dat[, facet_vars, drop = FALSE]
        
        # ---- Build formulaA terms (avoid duplicates)
        terms <- c("step")
        
        # rater * task (only if task exists and task is NOT timev)
        if (!is.null(task_var) && nzchar(task_var)) {
          if (is.null(timev) || !identical(task_var, timev)) {
            terms <- c(terms, paste0(rater_var, "*", task_var))
          } else {
            # task_var == timev 인 경우: facet 두 번째에 time을 넣은 상황이므로 task 항은 생략
            terms <- c(terms, rater_var)
          }
        } else {
          terms <- c(terms, rater_var)
        }
        
        # rater * time (use original timev name)
        if (!is.null(timev) && nzchar(timev)) {
          terms <- c(terms, paste0(rater_var, "*", timev))
        }
        
        terms <- unique(terms)
        formula <- stats::as.formula(paste0("~ ", paste(terms, collapse = " + ")))
        
        # ---- Fit model
        res <- TAM::tam.mml.mfr(
          resp     = dat[[dep]],
          facets   = facet_data,
          pid      = dat[[id]],
          formulaA = formula
        )
        
        rm(dat, facet_data)
        gc(verbose = FALSE)
        res
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

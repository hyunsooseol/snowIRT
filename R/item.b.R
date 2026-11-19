# ITEM ANALYSIS

itemClass <- if (requireNamespace("jmvcore", quietly = TRUE))
  R6::R6Class(
    "itemClass",
    inherit = itemBase,
    private = list(
      .init = function() {
        private$.setInstructions()
      },

      .setInstructions = function(result = NULL) {
        details <- ""
        if (!is.null(result)) {
          items <- result$items
          itemNames <- .html_escape(rownames(result$indices))
          nRespondents <- length(result$total)
          rows <- vapply(seq_along(items), function(i) {
            item <- items[[i]]
            sizes <- if (is.null(item$counts)) {
              "Too few distinct scores"
            } else {
              paste(paste0(colnames(item$counts), " = ", colSums(item$counts)), collapse = ", ")
            }
            sprintf('<tr><th scope="row">%s</th><td>%d</td><td>%d</td><td>%s</td></tr>',
                    itemNames[i], item$nValid, nRespondents - item$nValid, sizes)
          }, character(1L))
          keyWarnings <- vapply(items, function(item) item$keyUnlisted, logical(1L))
          keyMessage <- if (any(keyWarnings)) {
            paste0("<p>Check the answer keys and retained response levels for ",
                   paste(itemNames[keyWarnings], collapse = ", "), ".</p>")
          } else {
            ""
          }
          details <- paste0(
            '<details style="margin-top:12px">',
            '<summary>Sample sizes by item</summary>',
            sprintf("<p>Respondents with complete scores: %d of %d.</p>",
                    result$nComplete, nRespondents),
            keyMessage,
            '<table style="border-spacing:12px 5px;text-align:left">',
            '<thead><tr><th scope="col">Item</th><th scope="col">Valid</th>',
            '<th scope="col">Missing</th><th scope="col">Score group sizes</th></tr></thead>',
            '<tbody>', paste0(rows, collapse = ""), '</tbody></table></details>'
          )
        }
        widget <- HTMLWidget$new()
        self$results$instructions$setContent(widget$generate_accordion(
          title = "Instructions",
          content = paste0(
            '<div style="border:2px solid #e6f4fe;border-radius:15px;padding:15px;background:#e6f4fe">',
            '<ul>',
            '<li>Enter one correct answer per selected item, in order, separated by commas. Use the response labels displayed in jamovi.</li>',
            '<li>Define non-responses as missing in jamovi. To show options with zero selections, define their levels and enable <b>Retain unused levels in analyses</b> in the variable settings.</li>',
            '<li>Overall proportions use valid responses to each item. Point-biserial r correlates option selection (0 or 1) with the total score excluding the current item. This is a corrected point-biserial correlation. Correlations, score groups and the empirical ICC use respondents who answered all selected items.</li>',
            '<li>Upper - lower is the difference between the highest and lowest groups in the proportion selecting an option. The three-group setting targets 33%, 34% and 33%. Group sizes can differ because equal scores stay together.</li>',
            '<li>Difficulty is the proportion correct among valid responses. ULI uses the upper-minus-lower value for the correct option. RIT and RIR correlate the item score with the total and remaining-item scores, respectively.</li>',
            '<li>Saved item scores keep missing responses blank. A total score is saved when all selected items have a response.</li>',
            '<li>Report issues or request features on <a href="https://github.com/hyunsooseol/snowIRT/issues" target="_blank">GitHub</a>.</li>',
            '</ul></div>',
            details
          )
        ))
      },

      .run = function() {
        vars <- self$options$vars
        if (!isTRUE(self$options$run) || length(vars) == 0L)
          return()

        self$results$text$setContent("")
        private$.setInstructions()
        for (image in list(self$results$plot, self$results$plot1,
                           self$results$plot2, self$results$plot3)) {
          image$setState(NULL)
          image$.setPath(NULL)
        }
        data <- self$data
        if (is.null(data) || nrow(data) == 0L) {
          self$results$text$setContent("No respondents are available after applying the data filters.")
          return()
        }
        data <- data[, vars, drop = FALSE]

        key <- trimws(strsplit(paste0(self$options$key, ","), ",", fixed = TRUE)[[1L]])
        if (length(key) != length(vars) || any(!nzchar(key))) {
          self$results$text$setContent("Enter one correct answer for every selected item, separated by commas.")
          return()
        }

        result <- private$.compute(data, key, self$options$group)
        private$.setInstructions(result)
        for (i in seq_along(vars)) {
          item <- result$items[[i]]

          if (isTRUE(self$options$count)) {
            rows <- data.frame(response = item$displayLabels, total = item$summary$n)
            private$.writeTable(self$results$count[[i]], rows,
                                item$counts, "integer")
          }
          if (isTRUE(self$options$prop)) {
            rows <- data.frame(response = item$displayLabels, total = item$summary$rspP)
            private$.writeTable(self$results$prop[[i]], rows, item$proportions)
          }
          if (isTRUE(self$options$sum))
            private$.writeTable(self$results$sum[[i]], item$summary)
          if (isTRUE(self$options$sum1))
            private$.writeTable(self$results$sum1[[i]], item$summary, item$proportions)
        }

        if (isTRUE(self$options$disc)) {
          self$results$disc$setNote("indices", NULL)
          for (i in seq_along(vars)) {
            row <- as.list(result$indices[i, , drop = FALSE])
            self$results$disc$setRow(rowKey = vars[i], values = row)
          }
        }

        private$.saveScores(result, rownames(data))
        private$.preparePlots(result)
      },

      .compute = function(data, key, nGroups) {
        responses <- lapply(data, as.character)
        declared <- lapply(seq_along(responses), function(i) {
          observed <- responses[[i]]
          union(levels(data[[i]]), unique(observed[!is.na(observed)]))
        })
        # jamovi sends factors with labels and, for integer items, the original codes.
        for (i in seq_along(key)) {
          if (!key[i] %in% declared[[i]]) {
            column <- data[[i]]
            codeIndex <- match(key[i], attr(column, "values", exact = TRUE))
            if (!is.na(codeIndex) && codeIndex <= nlevels(column))
              key[i] <- levels(column)[codeIndex]
          }
        }

        scored <- do.call(cbind, lapply(seq_along(responses), function(i) {
          as.integer(responses[[i]] == key[i])
        }))
        dimnames(scored) <- dimnames(data)
        total <- rowSums(scored)
        complete <- !is.na(total)
        completeTotal <- total[complete]
        completeScored <- scored[complete, , drop = FALSE]

        items <- lapply(seq_along(responses), function(i) {
          levels <- union(declared[[i]], key[i])
          levels <- levels[!is.na(levels)]
          labels <- match(responses[[i]], levels)
          correct <- levels == key[i]
          frequency <- tabulate(labels, nbins = length(levels))
          nValid <- sum(frequency)
          rspP <- if (nValid > 0L) frequency / nValid else rep(NA_real_, length(levels))

          rest <- completeTotal - completeScored[, i]
          used <- labels[complete]
          pBis <- vapply(seq_along(levels), function(option) {
            private$.correlation(used == option, rest)
          }, numeric(1L))
          groups <- private$.formGroups(rest, nGroups)
          counts <- NULL
          proportions <- NULL
          discrim <- rep(NA_real_, length(levels))
          if (!is.null(groups)) {
            nOptions <- length(levels)
            groupLabels <- switch(as.character(nGroups),
              "2" = c("Lower", "Upper"),
              "3" = c("Lower", "Middle", "Upper"),
              c("Lower", paste("Group", seq.int(2L, nGroups - 1L)), "Upper")
            )
            cells <- used + nOptions * (groups - 1L)
            counts <- matrix(tabulate(cells, nbins = nOptions * nGroups),
                             nrow = nOptions, dimnames = list(levels, groupLabels))
            proportions <- sweep(counts, 2L, colSums(counts), "/")
            discrim <- proportions[, nGroups] - proportions[, 1L]
          }

          summary <- data.frame(
            correct = ifelse(correct, "*", ""),
            key = levels,
            n = frequency,
            rspP = rspP,
            pBis = pBis,
            discrim = unname(discrim),
            stringsAsFactors = FALSE
          )
          indices <- c(
            dif = rspP[correct],
            ULI = unname(discrim[correct]),
            RIT = if (length(responses) > 1L)
              private$.correlation(completeScored[, i], completeTotal) else NA_real_,
            RIR = pBis[correct]
          )
          list(summary = summary, nValid = nValid, indices = indices,
               keyUnlisted = !key[i] %in% declared[[i]],
               displayLabels = ifelse(correct, paste0("*", levels), levels),
               counts = counts, proportions = proportions)
        })

        indices <- as.data.frame(do.call(rbind, lapply(items, function(item) item$indices)))
        rownames(indices) <- names(data)
        list(items = items, indices = indices, scored = scored, total = total,
             complete = complete, nComplete = sum(complete))
      },

      .correlation = function(x, y) {
        if (length(x) < 2L || all(x == x[1L]) || all(y == y[1L]))
          return(NA_real_)
        stats::cor(x, y)
      },

      .formGroups = function(scores, nGroups) {
        distinct <- sort(unique(scores))
        if (length(nGroups) != 1L || !is.finite(nGroups) || nGroups < 2L ||
            nGroups != floor(nGroups) || length(distinct) < nGroups)
          return(NULL)

        targets <- if (nGroups == 3L) c(0.33, 0.67) else seq_len(nGroups - 1L) / nGroups
        scoreRanks <- match(scores, distinct)
        cumulativeN <- cumsum(tabulate(scoreRanks, nbins = length(distinct)))
        ends <- integer(nGroups - 1L)
        previous <- 0L
        # Choose the nearest eligible break, leaving at least one distinct score per group.
        for (j in seq_along(targets)) {
          possible <- seq.int(previous + 1L, length(distinct) - nGroups + j)
          previous <- possible[which.min(abs(cumulativeN[possible] - length(scores) * targets[j]))]
          ends[j] <- previous
        }
        findInterval(scoreRanks - 1L, ends) + 1L
      },

      .writeTable = function(table, rows, groupValues = NULL, groupType = "number") {
        table$deleteRows()
        for (column in table$columns) {
          if (startsWith(column$name, "scoreGroup"))
            column$setVisible(FALSE)
        }
        if (!is.null(groupValues)) {
          for (j in seq_len(ncol(groupValues))) {
            name <- paste0("scoreGroup", j)
            table$addColumn(name = name, title = colnames(groupValues)[j], type = groupType)
            rows[[name]] <- groupValues[, j]
          }
        }
        for (i in seq_len(nrow(rows)))
          table$addRow(rowKey = i, values = as.list(rows[i, , drop = FALSE]))
        # Remove notes that jamovi restored from previously saved analyses.
        table$setNote("sample", NULL)
      },

      .saveScores = function(result, rowNumbers) {
        if (isTRUE(self$options$total)) {
          self$results$total$setRowNums(rowNumbers)
          self$results$total$setValues(result$total)
        }

        if (isTRUE(self$options$scoring)) {
          items <- seq_len(ncol(result$scored))
          titles <- paste("Item", items)
          output <- self$results$scoring
          output$set(keys = items, titles = titles, descriptions = titles,
                     measureTypes = rep("continuous", length(items)))
          output$setRowNums(rowNumbers)
          for (i in items)
            output$setValues(index = i, values = as.numeric(result$scored[, i]))
        }
      },

      .preparePlots = function(result) {
        notices <- character()
        indices <- result$indices
        nItems <- length(result$items)

        if (isTRUE(self$options$plot)) {
          i <- self$options$num
          image <- self$results$plot
          if (length(i) != 1L || is.na(i) || i < 1L || i > nItems) {
            notices <- c(notices, "Distractor plot: select a valid item number.")
          } else {
            item <- result$items[[i]]
            if (is.null(item$proportions)) {
              notices <- c(notices, "Distractor plot: too few distinct remaining-item scores to form the requested groups.")
            } else {
              groupNames <- colnames(item$proportions)
              state <- data.frame(
                scoreGroup = factor(rep(groupNames, each = nrow(item$proportions)), levels = groupNames),
                response = factor(rep(item$summary$key, times = length(groupNames)), levels = item$summary$key),
                optionLabel = rep(item$displayLabels, times = length(groupNames)),
                proportion = as.vector(item$proportions)
              )
              attr(state, "itemNumber") <- i
              image$setState(state)
            }
          }
        }

        if (isTRUE(self$options$plot1)) {
          statistic <- self$options$disi
          if (is.null(statistic))
            statistic <- "ULI"
          image <- self$results$plot1
          if (all(is.na(indices$dif))) {
            notices <- c(notices, "Discrimination plot: no valid item responses are available.")
          } else {
            itemNames <- rownames(indices)
            itemOrder <- itemNames[order(indices$dif, na.last = TRUE)]
            values <- data.frame(
              item = factor(rep(itemNames, 2L), levels = itemOrder),
              measure = factor(rep(c("Difficulty", statistic), each = nItems), levels = c("Difficulty", statistic)),
              value = c(indices$dif, indices[[statistic]])
            )
            if (all(is.na(indices[[statistic]])))
              notices <- c(notices, paste("Discrimination plot:", statistic, "is unavailable. Difficulty is shown."))
            image$setState(values[!is.na(values$value), , drop = FALSE])
          }
        }

        if (isTRUE(self$options$plot2)) {
          score <- result$total[result$complete]
          image <- self$results$plot2
          if (length(score) == 0L) {
            notices <- c(notices, "Total score histogram: no respondents answered all selected items.")
          } else {
            middle <- stats::median(score)
            band <- ifelse(score < middle, "Below median", ifelse(score > middle, "Above median", "Median"))
            image$setState(data.frame(score = score, band = band))
          }
        }

        if (isTRUE(self$options$plot3)) {
          i <- self$options$num1
          image <- self$results$plot3
          if (length(i) != 1L || is.na(i) || i < 1L || i > nItems) {
            notices <- c(notices, "Empirical ICC: select a valid item number.")
          } else {
            itemScore <- result$scored[result$complete, i]
            rest <- result$total[result$complete] - itemScore
            if (length(unique(rest)) < 2L) {
              notices <- c(notices, "Empirical ICC: at least two distinct remaining-item scores are needed.")
            } else {
              state <- stats::aggregate(list(proportion = itemScore), list(rest = rest), mean)
              attr(state, "itemNumber") <- i
              image$setState(state)
            }
          }
        }

        self$results$text$setContent(notices)
      },

      .plotTheme = function(ggtheme) {
        # Keep jamovi's theme without its palette scales replacing plot-specific legends.
        if (inherits(ggtheme, "theme"))
          return(ggtheme)
        Filter(function(layer) inherits(layer, "theme"), ggtheme)
      },

      .plot = function(image, ggtheme = NULL, ...) {
        data <- image$state
        if (!is.data.frame(data) || !all(c("scoreGroup", "response", "optionLabel", "proportion") %in% names(data)))
          return(FALSE)

        itemNumber <- attr(data, "itemNumber", exact = TRUE)
        if (is.null(itemNumber))
          itemNumber <- self$options$num

        responses <- as.character(data$response)
        optionLabels <- ifelse(data$optionLabel == paste0("*", responses),
                               paste0(responses, "*"), data$optionLabel)
        labels <- stats::setNames(optionLabels, responses)
        plot <- ggplot2::ggplot(data, ggplot2::aes(x = scoreGroup, y = proportion, colour = response, group = response)) +
          ggplot2::geom_line(linewidth = 0.8) +
          ggplot2::geom_point(size = 2.6) +
          ggplot2::scale_colour_discrete(labels = function(keys) unname(labels[keys])) +
          ggplot2::scale_y_continuous(limits = c(0, 1)) +
          ggplot2::labs(title = paste("Item", itemNumber), x = "Remaining-item score group",
                        y = "Proportion selecting option", colour = "Response")
        print(plot + private$.plotTheme(ggtheme))
        TRUE
      },

      .plot1 = function(image, ggtheme = NULL, ...) {
        data <- image$state
        if (!is.data.frame(data) || !all(c("item", "measure", "value") %in% names(data)))
          return(FALSE)

        plot <- ggplot2::ggplot(data, ggplot2::aes(x = item, y = value, fill = measure)) +
          ggplot2::geom_col(position = ggplot2::position_dodge(preserve = "single"), width = 0.8) +
          ggplot2::geom_hline(yintercept = 0.2, linetype = "dashed", colour = "#666666") +
          ggplot2::scale_fill_manual(values = c("#4477aa", "#ce6248"), drop = FALSE) +
          ggplot2::scale_x_discrete(drop = FALSE) +
          ggplot2::labs(x = "Item (ordered by difficulty)", y = "Difficulty / discrimination", fill = NULL) +
          private$.plotTheme(ggtheme) +
          ggplot2::theme(axis.text.x = ggplot2::element_text(angle = self$options$angle, hjust = 1))
        print(plot)
        TRUE
      },

      .plot2 = function(image, ggtheme = NULL, ...) {
        data <- image$state
        if (!is.data.frame(data) || !all(c("score", "band") %in% names(data)))
          return(FALSE)

        plot <- ggplot2::ggplot(data, ggplot2::aes(x = score, fill = band)) +
          ggplot2::geom_histogram(binwidth = 1, boundary = -0.5, colour = "black") +
          ggplot2::scale_fill_manual(values = c("Below median" = "#ce6248", "Median" = "#999999",
                                               "Above median" = "#4477aa"), guide = "none") +
          ggplot2::labs(x = "Total score", y = "Number of respondents")
        print(plot + private$.plotTheme(ggtheme))
        TRUE
      },

      .plot3 = function(image, ggtheme = NULL, ...) {
        data <- image$state
        if (!is.data.frame(data) || !all(c("rest", "proportion") %in% names(data)))
          return(FALSE)

        itemNumber <- attr(data, "itemNumber", exact = TRUE)
        if (is.null(itemNumber))
          itemNumber <- self$options$num1

        plot <- ggplot2::ggplot(data, ggplot2::aes(x = rest, y = proportion)) +
          ggplot2::geom_line(colour = "#4477aa", linewidth = 0.8) +
          ggplot2::geom_point(colour = "#4477aa", size = 2.6) +
          ggplot2::scale_y_continuous(limits = c(0, 1)) +
          ggplot2::labs(title = paste("Item", itemNumber),
                        x = "Remaining-item score", y = "Proportion correct")
        print(plot + private$.plotTheme(ggtheme))
        TRUE
      }
    )
  )

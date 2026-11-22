# TAM's unidimensional curve grid and WLE grouping match plot.tam().
.prepareTamCurves <- function(model, items, type, observed, imageForItem) {
  prepared <- tryCatch({
    if (!identical(as.integer(model$ndim), 1L))
      stop("These curves require a unidimensional model.", call. = FALSE)

    observedGroups <- NULL
    if (observed) {
      ability <- TAM::tam.wle(model)$theta
      cuts <- unique(stats::quantile(ability, probs = (1:5) / 6, na.rm = TRUE))
      group <- cut(ability, breaks = c(-Inf, cuts, Inf), labels = FALSE)
      groups <- split(which(!is.na(group)), group[!is.na(group)])
      observedGroups <- list(rows = groups,
                             theta = vapply(groups, function(rows) mean(ability[rows]), numeric(1L)))
    }

    list(theta = seq(-3, 3, length.out = 100), observed = observedGroups)
  }, error = identity)

  for (item in items) {
    state <- if (inherits(prepared, "error")) {
      list(message = conditionMessage(prepared))
    } else {
      tryCatch(
        .tamCurveState(model, item, type, prepared),
        error = function(error) list(message = conditionMessage(error))
      )
    }
    imageForItem(item)$setState(state)
  }
}

.tamCurveState <- function(model, item, type, prepared) {
  if (length(item) != 1L || is.na(item) || item < 1L || item > model$nitems)
    stop(sprintf("Select an item index between 1 and %d.", model$nitems), call. = FALSE)

  # Same intercept and ability contribution as plot.tam(), on its plotting grid.
  design <- matrix(model$A[item, , ], nrow = model$maxK)
  intercept <- drop(design %*% model$xsi$xsi)
  slopes <- model$B[item, , 1L]
  categories <- which(is.finite(intercept) & is.finite(slopes))
  if (length(categories) == 0L)
    stop("There are no estimated response categories for this item.", call. = FALSE)

  theta <- prepared$theta
  logits <- sweep(outer(theta, slopes[categories]), 2L, intercept[categories], "+")
  weights <- exp(logits - apply(logits, 1L, max))
  probabilities <- weights / rowSums(weights)
  scores <- categories - 1L
  groupData <- prepared$observed
  response <- model$resp[, item]
  response[model$resp.ind[, item] == 0] <- NA

  observed <- NULL
  if (length(groupData$rows) > 0L) {
    observed <- list(theta = unname(groupData$theta))
    if (type == "expected") {
      observed$values <- vapply(groupData$rows, function(rows) mean(response[rows], na.rm = TRUE), numeric(1L))
    } else {
      observed$values <- vapply(scores, function(score) {
        vapply(groupData$rows, function(rows) mean(response[rows] == score, na.rm = TRUE), numeric(1L))
      }, numeric(length(groupData$rows)))
      dim(observed$values) <- c(length(groupData$rows), length(scores))
    }
  }

  name <- colnames(model$resp)[item]
  if (length(name) == 0L)
    name <- as.character(item)

  if (type == "expected") {
    return(list(type = type, theta = theta, values = drop(probabilities %*% scores),
                item = name, upper = max(response, na.rm = TRUE), observed = observed))
  }

  list(type = type, theta = theta, values = probabilities,
       scores = scores, item = name, observed = observed)
}

.drawTamCurve <- function(state, legendOutside = TRUE) {
  if (is.null(state))
    return(FALSE)

  if (!is.null(state$message)) {
    graphics::plot.new()
    graphics::text(0.5, 0.5, paste("Plot error:\n", state$message), cex = 0.9)
    return(TRUE)
  }

  observed <- state$observed
  if (state$type == "expected") {
    graphics::plot(state$theta, state$values, type = "l", col = 12, lwd = 3, las = 1,
                   ylab = "Score", xlab = "Ability", ylim = c(0, state$upper),
                   main = paste("Expected Scores Curve - Item", state$item))
    if (!is.null(observed))
      graphics::lines(observed$theta, observed$values, type = "o", lwd = 2, pch = 1)
    return(TRUE)
  }

  colors <- seq_along(state$scores) + 1L
  if (legendOutside) {
    margins <- graphics::par("mar")
    on.exit(graphics::par(mar = margins), add = TRUE)
    graphics::par(mar = c(margins[1:3], max(margins[4], 6.1)))
  }

  graphics::matplot(state$theta, state$values, type = "l", col = colors, lty = 1,
                    ylim = c(-0.1, 1.1), xlab = expression(theta),
                    ylab = expression(P(theta)), main = paste("Item", state$item))
  labels <- paste0("Cat", state$scores)
  if (is.null(observed)) {
    legendArgs <- list(legend = labels, col = colors, lty = 1)
  } else {
    graphics::matlines(observed$theta, observed$values, col = colors, lty = 2)
    graphics::matpoints(observed$theta, observed$values, col = colors, pch = 1)
    legendArgs <- list(legend = as.vector(rbind(paste0(labels, "(exp.)"), paste0(labels, "(obs.)"))),
                       col = rep(colors, each = 2L), lty = c(1, 2), pch = c(NA_integer_, 1))
  }
  position <- if (legendOutside) list(x = 3.3, y = 1.1) else list(x = "topright")
  do.call(graphics::legend, c(position, legendArgs, list(cex = 0.7, bty = "n", xpd = legendOutside)))
  TRUE
}

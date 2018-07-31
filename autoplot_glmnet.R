function (object, xvar = c("norm", "lambda", "dev"), label.n = 7, 
          label = TRUE, label.label = "Df", label.colour = NULL, label.alpha = NULL, 
          label.size = NULL, label.angle = NULL, label.family = NULL, 
          label.fontface = NULL, label.lineheight = NULL, label.hjust = NULL, 
          label.vjust = NULL, xlim = c(NA, NA), ylim = c(NA, NA), log = "", 
          main = NULL, xlab = NULL, ylab = "Coefficients", asp = NULL, 
          ...) 
{
  beta <- as.matrix(object$beta)
  xvar <- match.arg(xvar)
  switch(xvar, norm = {
    index <- apply(abs(beta), 2, sum)
    iname <- "L1 Norm"
  }, lambda = {
    index <- log(object$lambda)
    iname <- "Log Lambda"
  }, dev = {
    index <- object$dev.ratio
    iname <- "Fraction Deviance Explained"
  })
  if (is.null(xlab)) {
    xlab <- iname
  }
  plot.data <- ggplot2::fortify(object)
  cols <- rownames(beta)
  plot.data$index <- index
  indexer <- seq(0, max(plot.data$index), length = label.n)
  indexer <- sapply(indexer, function(x) which.min(abs(plot.data$index - 
                                                         x)))
  label.data <- plot.data[indexer, ]
  plot.data <- tidyr::gather_(plot.data, "variable", "value", 
                              cols)
  label.data$label_y <- rep(max(plot.data$value), nrow(label.data))
  p <- ggplot2::ggplot(data = plot.data) + ggplot2::geom_line(aes_string(x = "index", 
                                                                         y = "value", colour = "variable"), ...)
  p <- plot_label(p = p, data = label.data, x = "index", y = "label_y", 
                  label = label, label.label = label.label, label.colour = label.colour, 
                  label.alpha = label.alpha, label.size = label.size, label.angle = label.angle, 
                  label.family = label.family, label.fontface = label.fontface, 
                  label.lineheight = label.lineheight, label.hjust = label.hjust, 
                  label.vjust = label.vjust)
  p <- post_autoplot(p = p, xlim = xlim, ylim = ylim, log = log, 
                     main = main, xlab = xlab, ylab = ylab, asp = asp)
  p
}
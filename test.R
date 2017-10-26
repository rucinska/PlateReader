library(outliers)

# Function to detect outliers with Grubbs test in a vector
grubbs.flag <- function(vector) {
  outliers <- NULL
  test <- vector
  grubbs.result <- grubbs.test(test)
  pv <- grubbs.result$p.value
  # throw an error if there are too few values for the Grubb's test
  if (length(test) < 3 ) stop("Grubb's test requires > 2 input values")
  na.vect <- test
  while(pv < 0.05) {
    outliers <- c(outliers,as.numeric(strsplit(grubbs.result$alternative," ")[[1]][3]))
    test <- vector[!vector %in% outliers]
    # stop if all but two values are flagged as outliers
    if (length(test) < 3 ) {
      warning("All but two values flagged as outliers")
      break
    }
    grubbs.result <- grubbs.test(test)
    pv <- grubbs.result$p.value
    idx.outlier <- which(vector %in% outliers)
    na.vect <- replace(vector, idx.outlier, NA)
    
  }
  return(na.vect)
}




test <- data.frame(data_diff[,3:6])
data_diff2 <- apply(frag,2,grubbs.flag)



library(ggplot2)

df <- data.frame(x = c(1:100))

df$y <- 2 + 3 * df$x + rnorm(100, sd = 40)

m <- lm(y ~ x, data = df)

p <- ggplot(data = df, aes(x = x, y = y)) +
  
  geom_smooth(method = "lm", formula = y ~ x) +
  
  geom_point()

p



eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,
                 
                 list(        a = format(coef(m)[1], digits = 4),
                              
                              b = format(coef(m)[2], digits = 4),
                              
                              r2 = format(summary(m)$r.squared, digits = 3)))



dftext <- data.frame(x = 70, y = 50, eq = as.character(as.expression(eq)))

p + geom_text(aes(label = eq), data = dftext, parse = TRUE)


df <- data.frame(x = c(1:100))

df$y <- 2 + 3 * df$x + rnorm(100, sd = 40)

m <- lm(y ~ x, data = df)

p <- ggplot(data = df, aes(x = x, y = y)) +
  
  geom_smooth(method = "lm", formula = y ~ x) +
  
  geom_point()

p



eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,
                 
                 list(        a = format(coef(m)[1], digits = 4),
                              
                              b = format(coef(m)[2], digits = 4),
                              
                              r2 = format(summary(m)$r.squared, digits = 3)))



dftext <- data.frame(x = 70, y = 50, eq = as.character(as.expression(eq)))

p + geom_text(aes(label = eq), data = dftext, parse = TRUE)

t <- function(data, params){
                 
                 setup_params = function(data, params) {
                   # Figure out what type of smoothing to do: loess for small datasets,
                   # gam with a cubic regression basis for large data
                   # This is based on the size of the _largest_ group.
                   if (identical(params$method, "auto")) {
                     max_group <- max(table(data$group))
                     
                     if (max_group < 1000) {
                       params$method <- "loess"
                     } else {
                       params$method <- "gam"
                       params$formula <- y ~ s(x, bs = "cs")
                     }
                   }
                   if (identical(params$method, "gam")) {
                     params$method <- mgcv::gam
                   }
                   
                   params
                 }
                 
                 compute_group = function(data, scales, method = "auto", formula = y~x,
                                          se = TRUE, n = 80, span = 0.75, fullrange = FALSE,
                                          xseq = NULL, level = 0.95, method.args = list(),
                                          na.rm = FALSE, xpos=NULL, ypos=NULL) {
                   if (length(unique(data$x)) < 2) {
                     # Not enough data to perform fit
                     return(data.frame())
                   }
                   
                   if (is.null(data$weight)) data$weight <- 1
                   
                   if (is.null(xseq)) {
                     if (is.integer(data$x)) {
                       if (fullrange) {
                         xseq <- scales$x$dimension()
                       } else {
                         xseq <- sort(unique(data$x))
                       }
                     } else {
                       if (fullrange) {
                         range <- scales$x$dimension()
                       } else {
                         range <- range(data$x, na.rm = TRUE)
                       }
                       xseq <- seq(range[1], range[2], length.out = n)
                     }
                   }
                   # Special case span because it's the most commonly used model argument
                   if (identical(method, "loess")) {
                     method.args$span <- span
                   }
                   
                   if (is.character(method)) method <- match.fun(method)
                   
                   base.args <- list(quote(formula), data = quote(data), weights = quote(weight))
                   model <- do.call(method, c(base.args, method.args))
                   
                   m = model
                   eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                                    list(a = format(coef(m)[1], digits = 3), 
                                         b = format(coef(m)[2], digits = 3), 
                                         r2 = format(summary(m)$r.squared, digits = 3)))
                   func_string = as.character(as.expression(eq))
                   
                   if(is.null(xpos)) xpos = min(data$x)*0.9
                   if(is.null(ypos)) ypos = max(data$y)*0.9
                   data.frame(x=xpos, y=ypos, label=func_string)
                   
                 }
                 
                 required_aes = c("x", "y")
  
}
t(ea_cal, Layout)

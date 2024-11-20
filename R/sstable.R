# extract x variables from formula for sstable ----------------------------

getvar <- function(formula) {
  if (!is.call(formula)) {
    deparse(formula)
  } else {
    if (identical(formula[[1]], quote(`~`)) | identical(formula[[1]], quote(`+`)) | identical(formula[[1]], quote(`|`))) {
      unlist(lapply(c(formula[[2]], formula[[3]]), getvar))
    } else {
      deparse(formula)
    }
  }
}

# extract the sign from quo/formula for sstable - trinhdhk ----------------

getsign <- function(fml){
  fml <- unclass(fml)[[2]]
  .getsign = function(x){
    c(x[[1]],
      unlist(lapply(x[-1],
                    function(.x) if(is.call(.x)) .getsign(.x))))
  }

  out <- .getsign(fml)
  out[length(out):1]
}

# extract information from formula for sstable ----------------------------

sstable.formula <- function(formula) {
  if (length(formula) < 3) {
    stop("Missing row-wise variable(s) !!!")
  }

  ## get formula to extract all required variables (formula0)
  formula0 <- if (identical(formula[[3]], 1)) {
    as.formula(paste("~", deparse(formula[[2]])))
  } else {
    as.formula(paste("~", paste(getvar(formula), collapse = "+")))
  }


  ## get index of each element in formula
  xlen <- length(all.vars(formula[[2]]))
  if (xlen == 0) {stop("Missing row-wise variable(s) !!!")}

  if (length(formula[[3]]) > 1) {
    if (deparse(formula[[3]][[1]]) == "|") {
      ylen <- ifelse(identical(formula[[3]][[2]], 1), 0, length(all.vars(formula[[3]][[2]])))
      zlen <- length(all.vars(formula[[3]][[3]]))
    } else {
      ylen <- ifelse(identical(formula[[3]], 1), 0, length(all.vars(formula[[3]])))
      zlen <- 0
    }
  } else {
    ylen <- ifelse(identical(formula[[3]], 1), 0, length(all.vars(formula[[3]])))
    zlen <- 0
  }

  if (ylen > 1) {stop("Only 1 column-wise variable is allowed !!!")}
  if (zlen > 1) {stop("Only 1 third dimension variable is allowed !!!")}

  ## get all formulas required for tabulation
  xs <- getvar(formula[[2]])
  formula1 <- sapply(xs, function(x) update.formula(old = formula, new = paste(x, " ~ .")))

  ## output
  return(list(formula0 = formula0,
              index = list(x = 1:xlen,
                           y = if (ylen == 0) 0 else (xlen + 1),
                           z = if (zlen == 0) 0 else (xlen + 2)),
              formula1 = formula1))
}


# calculate summary statistics for continuous variable --------------------

#' Calculate summary statistics for continuous variable
#'
#' @description A function to calculate summary statistics for continuous variable.
#'
#' @param x a numeric vector to be summarised.
#' @param statistics a character specifies summary statistics for continuous row variables.
#' @param digits a number specifies number of significant digits for numeric statistics.
#' @param n a logical value specifies whether output will contain the number of non-missing values.
#'
#' @return a string displays formatted summary statistics.
#' @export

contSummary <- function(x, statistics = NULL, digits = 1, n = TRUE) {
  if (is.null(statistics)) {
    stop("Please provide a string for 'statistics'")
  }

  # Convert the 'statistics' string to lowercase
  statistics <- tolower(statistics)

  # Define a list of supported statistics and their corresponding functions
  stat_list <- list(
    mean = function(x) round(mean(x, na.rm = TRUE), digits),
    median = function(x) round(median(x, na.rm = TRUE), digits),
    sd = function(x) round(sd(x, na.rm = TRUE), digits),
    q1 = function(x) round(quantile(x, 0.25, na.rm = TRUE), digits),
    q3 = function(x) round(quantile(x, 0.75, na.rm = TRUE), digits),
    min = function(x) round(min(x, na.rm = TRUE), digits),
    max = function(x) round(max(x, na.rm = TRUE), digits)
  )

  # Replace statistical names with their calculated values
  for (stat_name in names(stat_list)) {
    stat_value <- stat_list[[stat_name]](x)
    stat_value <- format(stat_value, nsmall = digits)
    statistics <- gsub(stat_name, stat_value, statistics)
  }

  # Include the count if requested
  if (n) {
    statistics <- paste0(statistics)
  }

  return(statistics)
}

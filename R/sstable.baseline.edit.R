# create baseline table ---------------------------------------------------

#' Create baseline table
#'
#' @description A function to create a simple summary baseline table.
#'
#' @param formula a formula specifies variables for rows, variable for column and third-dimension variable.
#'
#' If formula is in the form of x+y ~ a, the function will summary x and y with respects to each value of a
#'
#' If formula is in the form of x+y ~ a|z, the function will summary z with respects to each value of {a,x} and {a, y}
#' @param data a data frame to derive baseline table from.
#' @param bycol a logical value specifies whether the summary will be by column or by row.
#' @param pooledGroup a logical value specifies whether to pool all subgroups of column variable.
#' @param keepEmptyGroup a logical value specifying whether should keep empty groups
#' @param statistics a character specifies summary statistics for continuous row variables.
#' @param cont a vector specifies whether each row variables is continuous.
#' @param cate a vector specifies whether each row variables is categorical.
#' @param digits a number specifies number of significant digits for numeric statistics.
#' @param test a logical value specifies whether a statistical test will be performed to compare between treatment arms.
#' @param pdigits a number specifies number of significant digits for p value.
#' @param pcutoff a number specifies threshold value of p value to be displayed as "< pcutoff".
#' @param chisq.test a logical value specifies whether Chi-squared test or Fisher's exact test will be used to compare between treatment arms.
#' @param correct a parameter for chisq.test().
#' @param simulate.p.value a parameter for chisq.test() and fisher.test().
#' @param B a parameter for chisq.test() and fisher.test().
#' @param workspace a parameter for fisher.test().
#' @param hybrid a parameter for fisher.test().
#' @param footer a vector of strings to be used as footnote of table.
#' @param flextable a logical value specifies whether output will be a flextable-type table.
#' @param bg a character specifies color of the odd rows in the body of flextable-type table.
#' @param df a logical values specifies whether output will be a draw dataframe.
#'
#' @return a flextable-type table or a list with values/headers/footers
#' @export
# function to format data frame to baseline table -hungtt
sstable.baseline.edit <- function(value, formula, data, bycol = TRUE, pooledGroup = FALSE, keepEmptyGroup = FALSE,
                                  statistics = "mean, median (Q1-Q3)", cont = NULL, cate = NULL, fullfreq = TRUE, digits = 1,
                                  test = FALSE, pdigits = 3, pcutoff = 0.0001,
                                  chisq.test = FALSE, correct = FALSE, simulate.p.value = FALSE, B = 2000,
                                  workspace = 1000000, hybrid = FALSE, footer = NULL, flextable = FALSE, bg = "#F2EFEE") {
  ## get information from formula
  info <- sstable.formula(formula)

  ## strip the tibble class which causes issue - trinhdhk
  data <- as.data.frame(data)

  ## get data
  dat <- model.frame(info$formula0, data = data, na.action = NULL)
  x <- xlabel <- dat[, info$index$x, drop = FALSE]
  y <- if (info$index$y > 0) dat[, info$index$y] else NULL
  z <- if (info$index$z > 0) dat[, info$index$z] else NULL

  ## y must be categorical variable
  if (!is.null(y)) {
    if (all(!c("character", "factor", "logical") %in% class(y)) |
        (any(c("numeric", "integer") %in% class(y)) & length(unique(na.omit(y))) > 5)) {
      stop("Column-wise variable must be categorical !!!")
    }
    if (is.factor(y)){
      ### [trinhdhk] 2024-04: reverse level of y for better summary
      y <- ._lv_rev_(y)
      if (!keepEmptyGroup) y <- droplevels(y)
      y <- addNA(y, ifany = TRUE)
    } else {
      y <- factor(as.character(y), levels = sort(unique(as.character(y), decreasing = TRUE), na.last = TRUE), exclude = NULL)
    }
  } else {
    y <- factor(rep("Total", nrow(dat)))
  }

  #browser()

  ## determine type of x (argument: cont and cate)
  varlist <- getvar(formula) #hungtt
  varlist <- varlist[-length(varlist)] #remove the y name

  # Convert varlist, cont, and cate to lowercase for case-insensitive comparison
  varlist_lower <- tolower(varlist)
  cont_lower <- tolower(cont)
  cate_lower <- tolower(cate)

  continuous <- ifelse(varlist_lower %in% cont_lower, TRUE, ifelse(varlist_lower %in% cate_lower, FALSE, NA)) #assign var type

  continuous <- sapply(1:ncol(x), function(i) {
    out <- ifelse(is.na(continuous[i]),
                  ifelse(any(c("factor", "character", "logical") %in% class(x[, i])) |
                           (any(c("numeric", "integer") %in% class(x[, i])) & length(unique(na.omit(x[, i]))) <= 5), FALSE, TRUE),
                  continuous[i])
    return(out)
  })


  for (i in (1:ncol(x))) {
    if (continuous[i] == FALSE & !is.factor(x[, i])) x[, i] <- factor(x[, i], levels = sort(unique(na.omit(x[, i]))))
  }

  ## if z exists, x must be categorical
  if (!is.null(z) & any(continuous == TRUE)) stop("Row-wise variable must be categorical when third dimension variable exists !!!")

  ## if use by-row layout, x must be categorical
  #browser()
  if (bycol == FALSE & any(sapply(1:ncol(x), function(i) is.factor(x[,i])) == FALSE)) stop("Row-wise variable must be categorical in by-row layout !!!")

  ## determine type of z
  if (!is.null(z)) {
    zdiscrete <- any(c("factor", "character", "logical") %in% class(unclass(z))) |
      (any(c("numeric", "integer") %in% class(unclass(z))) & length(unique(na.omit(z))) <= 5)
    # if (zcontinuous == FALSE & !is.factor(z)) z <- factor(z, levels = unique(na.omit(z)))
    if (zdiscrete) z <- factor(z, levels = unique(na.omit(z)))
  }

  # browser()
  ## digits
  if (length(digits) == 1) {
    digits <- rep(digits, ncol(x))
  } else {
    if (length(digits) != ncol(x)) stop("digits argument must have length 1 or similar length as number of row-wise variables !!!")
  }

  ## if pooledGroup
  if (pooledGroup) {
    x <- rbind(x, x)
    ypool <- ifelse("total" %in% tolower(levels(y)), "pooledGroup", "Total")
    y <- factor(c(as.character(y), rep(ypool, nrow(dat))), levels = c(levels(y), ypool), exclude = NULL)
    z <- if (!is.null(z)) factor(c(z, z), levels = unique(na.omit(z))) else NULL
  }
  # else{
  #   z <- if (!is.null(z)) factor(z, levels = unique(na.omit(z))) else NULL
  # }

  ## get variable name
  varname <- if (ncol(xlabel) == 1) getlabel(xlabel[, 1]) else getlabel(xlabel)
  varname <- unique(varname)

  # update order of variable customize for sstable.baseline.edit - hungtt
  value <- as.data.frame(value)
  dummy_df <- data.frame(V1= varname, order =seq(1:length(varname))*1000)
  value <- full_join(value, dummy_df)

  for (i in 1:nrow(value))
    {if (is.na(value[i,ncol(value)])) {value[i,ncol(value)] <- (value[i-1,ncol(value)]) + 1}}

  # Identify duplicated values in column V1 that are also in varname
  duplicates_to_keep <- value %>%
    filter(V1 %in% varname) %>%
    distinct(V1, .keep_all = TRUE)

  # Remove duplicates and keep only the first occurrence
  value <- value %>%
    filter(!(V1 %in% varname)) %>%
    filter(V1 != "- ") %>% #remove empty string -hungtt
    bind_rows(duplicates_to_keep)

  # arrange in order of appearing in formula argument and remove dummy column
  value <- value %>% arrange(value[[ncol(value)]])
  value <- value[, -ncol(value)]
  value

  ## output
  ### header
  gr.lev <- levels(y)
  header1 <- c("", c(rbind(rep("", length(gr.lev)), paste(gr.lev, " (N=", table(y), ")", sep = ""))))
  header2 <- c("Characteristic", rep(c("n", "Summary statistic"), length(gr.lev)))
  if (test) {
    header1 <- c(header1, "p value")
    header2 <- c(header2, "")
  }

  ### footer
  footer2 <- footer1 <- footer1.after <- footer.con <- footer.cat <- NULL

  #### summary statistics
  if ((is.null(z) & any(continuous)) | (!is.null(z) & !is.factor(z))) {
    footer.con <- paste0(
      paste(statistics, collapse = ", "),
      " for numeric variable(s)."
    )
  }

  if ((is.null(z) & any(continuous == FALSE)) | (!is.null(z) & is.factor(z))) {
    footer.cat <- "absolute count (%) for categorical variable(s)"
  }

  footer1.after <- if (is.null(footer.con)) {
    paste0(footer.cat, ".")
  } else {
    if (is.null(footer.cat)) {
      footer.con
    } else {
      paste0(footer.cat, " and ", footer.con)
    }
  }
  footer1 <- paste("Summary statistic is", footer1.after)

  #### test
  if (test) {
    footer2.cat <- paste(
      ifelse(chisq.test == FALSE, "Fisher's exact test", "Chi-squared test"),
      "for categorical variable(s)"
    )
    footer2.con <- "Kruskal-Wallis/Mann-Whitney U-test for continuous variable(s)."
    footer2.after <- if (is.null(footer.con)) {
      paste0(footer2.cat, ".")
    } else {
      if (is.null(footer.cat)) {
        footer2.con
      } else {
        paste0(footer2.cat, " and ", footer2.con)
      }
    }
    footer2 <- paste("p-values were based on", footer2.after)
  }

  footer <- c(
    "N is number of all patients, n is number of patients with non-missing value.",
    footer1,
    if (any(value == "-")) {"- : value cannot be estimated."} else NULL,
    footer2,
    footer
  )

  ### flextable
  if (flextable) {
    requireNamespace("flextable")
    requireNamespace("officer")

    ## main table
    tab <- flextable::flextable(as.data.frame(value))

    ## header
    header1[1] <- header2[1]
    header1[seq(from = 2, to = 2 * length(gr.lev), by = 2)] <- header1[seq(from = 2, to = 2 * length(gr.lev), by = 2) + 1]
    if (test) header2[length(header1)] <- header1[length(header1)]
    assign("tab",
           eval(parse(text = paste0("flextable::set_header_labels(tab,",
                                    paste(paste0("V", 1:length(header1)), paste0("'", header1, "'"), sep = "=", collapse = ","),
                                    ")"))))
    assign("tab",
           eval(parse(text = paste0("flextable::add_header(tab,",
                                    paste(paste0("V", 1:length(header1)), paste0("'", header2, "'"), sep = "=", collapse = ","),
                                    ", top = FALSE)"))))

    tab <- flextable::merge_h(tab, part = "header")
    tab <- flextable::merge_v(tab, part = "header")

    ## footer
    for (k in (1:length(footer))) {
      tab <- flextable::add_footer(tab, V1 = footer[k], top = FALSE)
      tab <- flextable::merge_at(tab, i = k, j = 1:length(header1), part = "footer")
    }

    ## format
    ### width
    tab <- flextable::autofit(tab)
    ### alignment
    tab <- flextable::align(tab, j = 1, align = "left", part = "all")
    ### faces of header
    tab <- flextable::bold(tab, part = "header")
    ### background
    tab <- flextable::bg(tab, i = seq(from = 1, to = nrow(value), by = 2), j = 1:length(header1), bg = bg, part = "body")
    ### border
    tabbd <- officer::fp_border(color="black", width = 1.5)
    tab <- flextable::border_remove(tab)
    tab <- flextable::hline(tab, border = tabbd, part = "header")
    tab <- flextable::hline_top(tab, border = tabbd, part = "all")
    tab <- flextable::hline_bottom(tab, border = tabbd, part = "body")

  } else {
    tab <- list(table = rbind(header1, header2, value),
                footer = footer)
    class(tab$table) <- c('baseline_tbl', 'ss_tbl', class(tab$table))
  }
  ## output
  if (!flextable) class(tab) <- c('ss_baseline','ss_obj')
  return(tab)
}

# create adverse event table ---------------------------------------------------

#' Create an adverse event summary table
#'
#' @description A function to create a simple adverse event summary table.
#'
#' @param ae_data a data frame contains adverse event data.
#' @param fullid_data a data frame contains treatment arm data of all participants (not just those had adverse event).
#' @param group_data a reference data frame contains the group name of each ae.
#' @param id.var a character specifies name of study id variable (exists in both adverse event data and treatment arm data).
#' @param aetype.var a character specifies name of adverse event type variable (exists in adverse event data).
#' @param grade.var NULL or character specifies name of adverse event grade variable (exists in adverse event data).
#' @param group.var a character specifies group (exists in adverse event data if group_data = NULL or group_data otherwise).
#' @param arm.var a character specifies name of treatment arm variable (exists in treatment arm data).
#' @param fullfreq a logical value specifies whether to show total frequency as a denominator
#' @param sort.by
#' An unquoted formula of sorting options.
#'
#' Available options are `ep`, `pt`, `p`, or a combination of them (eg ep+pt-p);
#' A minus sign indicates a descending order.
#' @param digits a number specifies number of significant digits for numeric statistics.
#' @param test a logical value specifies whether a statistical test will be performed to compare between treatment arms.
#' @param pdigits a number specifies number of significant digits for p value.
#' @param pcutoff a number specifies threshold value of p value to be displayed as "< pcutoff".
#' @param chisq.test
#' a logical value specifies whether Chi-squared test or Fisher's exact test will be used to compare between treatment arms.
#'
#' Be aware that even when chisq.test==TRUE, if expected values are < 1, the later test will take over.
#' @param test.anyae.only  a logical value specifies whether a statistical test will be performed for all variable or only for any adverse events.
#'
#'
#' Be aware that even when chisq.test==TRUE, if expected values are < 1, the later test will take over.
#' @param correct a parameter for chisq.test().
#' @param simulate.p.value a parameter for chisq.test() and fisher.test().
#' @param B a parameter for chisq.test() and fisher.test().
#' @param workspace a parameter for fisher.test().
#' @param hybrid a parameter for fisher.test().
#' @param footer a vector of strings to be used as footnote of table.
#' @param flextable a logical value specifies whether output will be a flextable-type table.
#' @param bg a character specifies color of the odd rows in the body of flextable-type table.
#' @param group.var.priority a vector that specifies which groups will be appear first in the table.
#' @param print.aetype.header a logical value, whether to print the label of aetype.header.
#' @param na.text [`(Missing)`] in-placed text for missing AE
#' @param matrix.raw a logical values specifies whether output will be a draw dataframe.
#'
#' @return a flextable-type table or a list with values/headers/footers
#' @import dplyr
#' @import tidyr
#' @export
sstable.ae <- function(ae_data, fullid_data, group_data = NULL, id.var,
                       aetype.var, grade.var = NULL,
                       group.var = NULL, group.var.priority = NULL, arm.var, sort.by, digits = 0, fullfreq = FALSE,
                       test = TRUE, test.anyae.only = TRUE, pdigits = 3, pcutoff = 0.001, chisq.test = TRUE, correct = FALSE,
                       simulate.p.value = FALSE, B = 2000, workspace = 1000000, hybrid = FALSE,
                       print.aetype.header =
                         length(aetype.var) > 1 | any(length(names(aetype.var)) > 0) | any(isTRUE(nchar(names(aetype.var)) > 0)),
                       na.text = '(Missing)', matrix.raw = FALSE,
                       footer = NULL, flextable = TRUE, bg = "#F2EFEE"){
  requireNamespace("dplyr")
  requireNamespace("tidyr")

  tmp <- match.call()
  print.aetype.header <- force(print.aetype.header)

  # if more than one aetype.var
  # perform sstable.ae for each for aetype.var
  # then do rbind
  if (length(aetype.var) > 1){

    # function to prepare
    make_tblcall <- function(orig_call, .aetype_var) {
      new_call <- orig_call
      new_call$aetype.var <- .aetype_var
      new_call$flextable <- FALSE
      new_call$print.aetype.header <- print.aetype.header
      new_call
    }
    env <- rlang::caller_env()
    n.grade <- if (length(grade.var)) length(unique(ae_data[[grade.var]])) else 0
    tbl1_call <- make_tblcall(tmp, aetype.var[1])
    tbl1 <- eval(tbl1_call, envir = env)
    tbl2p <-
      lapply(seq_along(aetype.var[-1]),
             function(i){
               .aetype.var <- aetype.var[i+1]
               tbl_call <- make_tblcall(tmp, .aetype.var)
               sstbl = eval(tbl_call, envir=env)
               sstbl
             })
    tbl2 <- tbl2p[[1]]

    # tbl2$table <- tbl2$table[-c(1:(3+n.grade)),]
    for (tbl3 in tbl2p[-1]) tbl2 <- ._do_rbind(tbl2, tbl3, header=c(1:(3+n.grade)))
    # browser()
    out <- ._do_rbind(tbl1, tbl2, header=c(1:(3+n.grade)))
    # rownames(out)[c(3,4,5,8)] <- 'section'
    if (flextable) return(ss_flextable(out))
    return(out)
  }

  ## check variable's name

  if (!id.var %in% names(ae_data)){stop(paste(tmp$id.var, "does not exist in", deparse(tmp$ae_data), "!!!"))}
  if (!id.var %in% names(fullid_data)){stop(paste(tmp$id.var, "does not exist in", deparse(tmp$fullid_data), "!!!"))}
  if (!aetype.var %in% names(ae_data)){stop(paste(tmp$aetype.var, "does not exist in", deparse(tmp$ae_data), "!!!"))}
  if (!is.null(grade.var)) {
    if (!grade.var %in% names(ae_data)){stop(paste(tmp$grade.var, "does not exist in", deparse(tmp$ae_data), "!!!"))}
  }
  if (!is.null(group.var)){
    if (!is.null(group_data)){
      if (!group.var %in% names(group_data)) stop(paste(tmp$group.var, 'does not exist in', deparse(tmp$group_data), '!!!'))
      if (!aetype.var %in% names(group_data)){stop(paste(tmp$aetype.var, "does not exist in", deparse(tmp$group_data), "!!!"))}
    } else
      if (!group.var %in% names(ae_data)) stop(paste(tmp$group.var, 'does not exist in', deparse(tmp$ae_data), '!!!'))
  }
  if (!arm.var %in% names(fullid_data)){stop(paste(tmp$arm.var, "does not exist in", deparse(tmp$fullid_data), "!!!"))}
  if (!missing(sort.by)){
    sort.by <- rlang::enquo(sort.by)
    sort.vars <- all.vars(sort.by)
    sort.signs <- getsign(sort.by)
    if (length(sort.signs) < length(sort.vars)) sort.signs <- purrr::prepend(sort.signs, quote(`+`))
    if (length(setdiff(sort.vars, c('pt','ep','p'))))
      stop('Sorting can only apply to `pt`, `ep`, and `p`')
  }



  #strip tibble class as it might cause problem
  ae_data <- as.data.frame(ae_data)
  fullid_data <- as.data.frame(fullid_data)
  if (!is.null(group.var)) {
    if (is.null(group_data)) {
                group_data <- unique(ae_data[,c(aetype.var, group.var)])
               name_counts <- table(group_data[[aetype.var]])
               misclassified_names <- names(name_counts[name_counts > 1])
               misclassifications_list <- list()
               warning_messages <- c()
               options (warn = 1)
               if (length(misclassified_names) > 0) {
                 cat("Misclassifications found:\n")
                 for (name in misclassified_names) {
                   cat("Name:", name, "\n")
                   ae.soc <- unique(group_data[group_data[[aetype.var]] == name, group.var])
                   cat("In classes:", paste(ae.soc, collapse = ", "), "\n")
                   misclassifications_list[[name]] <- ae.soc
                   warning_messages <- c(warning_messages, paste(name, ":", paste(ae.soc, collapse = ", "), ";"))
                 }
                 warning_message <- paste("Adverse events are being classified into two different SOCs.", paste(warning_messages, collapse = " "), sep = "\n")
                 warning(warning_message)}
              }}

  group_data <- as.data.frame(group_data)
  is.grouped <- !missing(group.var)

  # Check if grade.var is not NULL and has any NA values - hungtt
  if (!is.null(grade.var) && any(is.na(ae_data[[grade.var]]))) {
    # Replace NA values with "Grade NA"
    ae_data[[grade.var]][is.na(ae_data[[grade.var]])] <- "Grade N/A"
  }

  # If there is a designated name to aetype.var, populate that into the data
  if (!is.null(names(aetype.var)) & isTRUE(nchar(names(aetype.var))>0)){
    attr(ae_data[, aetype.var[[1]]], 'label') <- names(aetype.var)[[1]]
  }
  aetype.var <- aetype.var[[1]]

  # Check if any aetype.var is NA and replace with "NA"
  lbl = attr(ae_data[[aetype.var]], 'label')
  ae_data[[aetype.var]] = as.character(ae_data[[aetype.var]])
  ae_data[[aetype.var]] = structure(ae_data[[aetype.var]], label=lbl)
  if (any(is.na(ae_data[[aetype.var]]))) {
    #if (is.factor(ae_data[[var]]))
    #levels(ae_data[[var]]) <- c(levels(ae_data[[var]]), na.text )

    ae_data[[aetype.var]][is.na(ae_data[[aetype.var]])] <- na.text
  }

  ## format study arms
  idarm <- fullid_data[, c(id.var, arm.var)]; colnames(idarm) <- c("id", "arm")
  if (is.factor(idarm$arm)){
    ### [trinhdhk] 2024-04: reverse level of y for better summary
    idarm$arm <- addNA(idarm$arm, ifany = TRUE)
    idarm$arm <- ._lv_rev_(idarm$arm)
    arm_lev <- levels(idarm$arm)
  } else {
    ### [trinhdhk] 2024-04: reverse level of y for better summary
    arm_lev <- sort(unique(as.character(idarm$arm)), decreasing = TRUE, na.last = TRUE)
    idarm$arm <- with(idarm, factor(as.character(arm), levels = arm_lev, exclude = NULL))
  }

  # Original data frame ae_data assumed to exist
  # Ensure ae_data, ae_any, ae_grade, etc., are defined appropriately in your script

  replace_with_var_names <- function(df, var, aetype_var) {
    # Identify rows where aetype_var is not "NA"
    not_na_rows <- df[[aetype_var]] != na.text

    # Replace values with variable names or labels only where not "NA"
    if (!is.null(attr(df[[var]], "label"))) {
      df[[aetype_var]][not_na_rows] <- attr(df[[var]], "label")  # Use label if available
    } else {
      df[[aetype_var]][not_na_rows] <- var  # Fallback to default label
    }

    # Delete rows where aetype_var is "NA"
    df <- df[not_na_rows, , drop = FALSE]

    return(df)
  }

  # browser()

  # Example usage with ae_any data frame mutation
  ae_any <- ae_data  # Assuming ae_data is your original data frame

  mutated_data <- if(print.aetype.header)
    replace_with_var_names(ae_any, aetype.var, aetype.var) else NULL
  ae_any[, aetype.var] <- "Any selected adverse event"
  # Combine original and mutated data (assuming ae_data and ae_any exist)
  ae <- if(print.aetype.header)
    rbind(ae_data, ae_any, mutated_data) else
      rbind(ae_data, ae_any)

  # Extract grades of ae (assuming grade.var exists)
  if (!is.null(grade.var)) {
    grade <- sort(unique(na.omit(ae_data[, grade.var])))
    grade2 <- ifelse(grepl(pattern = "grade", ignore.case = TRUE, x = grade), grade, paste("Grade", grade))
    ae_grade <- do.call(rbind,
                        lapply(1:length(grade), function(i) {
                          tmpdat <- ae_data[ae_data[, grade.var] == grade[i], ]
                          tmpdat[, aetype.var] <- paste("-", grade2[i])
                          return(tmpdat)
                        }))
    ae <- rbind(ae, ae_grade)
  }

  # Select necessary columns and rename them
  ae <- ae[, c(id.var, aetype.var)]
  colnames(ae) <- c("id", "aetype")


  # Get unique levels of aetype
  aetype_lev.raw <- unique(as.character(ae_data[[aetype.var]]))

  # Check if aetype.var has a label attribute
  if (!is.null(attr(ae_data[[aetype.var]], "label"))) {
    aetype.var.label <- attr(ae_data[[aetype.var]], "label")
  } else {
    aetype.var.label <- aetype.var
    # Use default label
  }

  # Construct aetype_lev with labels where available
  aetype_lev <- c("Any selected adverse event",
                  if (!is.null(grade.var)) paste("-", grade2),
                  if (print.aetype.header) aetype.var.label,
                  aetype_lev.raw)

  ## add randomized arm to AE
  ae_arm <- merge(idarm, ae, by = "id", all.y = TRUE) %>%
    mutate(arm = addNA(factor(as.character(arm), levels = arm_lev, exclude = NULL), ifany = TRUE),
           aetype = addNA(factor(as.character(aetype), levels = aetype_lev, exclude = NULL), ifany = TRUE))
  idarm2 <- ae_arm |> select(id, arm)

  ## calculate episodes and patients
  ae_count <- ae_arm %>%
    group_by(aetype, arm) %>%
    summarise(n_episode = n(),
              n_patient = length(unique(id))) %>%
    ungroup() %>%
    complete(aetype, arm)
  if (!is.factor(ae_count$aetype)) {ae_count$aetype <- factor(ae_count$aetype, levels = levels(ae_arm$aetype))}
  if (!is.factor(ae_count$arm)) {ae_count$arm <- factor(ae_count$arm, levels = levels(ae_arm$arm))}

  episode_n <- unlist(spread(select(as.data.frame(ae_count), -n_patient), key = arm, value = n_episode)[, -1])
  episode_n[is.na(episode_n)] <- 0
  patient_n <- unlist(spread(select(ae_count, -n_episode), key = arm, value = n_patient)[, -1])
  patient_n[is.na(patient_n)] <- 0
  patient_N <- rep(table(idarm$arm), each = nlevels(ae_arm$aetype))
  # tryCatch(patient_n/patient_N, warning=function(w) browser())
  patient_p <- patient_n/patient_N

  ## table
  #browser()
  value <- matrix(ncol = nlevels(ae_arm$arm)*3, nrow = nlevels(ae_arm$aetype))
  value[, seq(from = 1, to = nlevels(ae_arm$arm)*2, by = 2)] <- episode_n
  #only show fullfreq when specify - hungttt
  if (fullfreq) {value[, seq(from = 2, to = nlevels(ae_arm$arm)*2, by = 2)] <- paste0(patient_n, "/", patient_N,
                                                                       " (", formatC(100 * patient_p, digits, format = "f"), "%)")}
  else {value[, seq(from = 2, to = nlevels(ae_arm$arm)*2, by = 2)] <- paste0(patient_n,
                                                                             " (", formatC(100 * patient_p, digits, format = "f"), "%)")}
  ## add raw values to 2 dummy columns for sorting purpose
  value[, (nlevels(ae_arm$arm)*2 + 1):(nlevels(ae_arm$arm)*3)] <- patient_n
  ae_value <- cbind(aename = levels(ae_arm$aetype), value)

  ## get the aehead -trinhdhk
  ae_value.head <- as.data.frame(ae_value, stringsAsFactors = FALSE) %>% filter(!aename %in% aetype_lev.raw)

  ## categorizing ae into groups
  ## author: trinhdhk
  if (!is.null(group.var)) {
    # Check if group.var is a factor and update levels
    if (is.factor(group_data[[group.var]])) {
      # Get current levels and add "Uncategorised"
      new_levels <- c(levels(group_data[[group.var]]), 'Uncategorised')

      # Update factor levels
      group_data[[group.var]] <- factor(group_data[[group.var]], levels = new_levels)

      # Replace NA with "Uncategorised"
      group_data[[group.var]] <- replace_na(as.character(group_data[[group.var]]), 'Uncategorised')

      # Convert back to factor
      group_data[[group.var]] <- factor(group_data[[group.var]], levels = new_levels)
    } else {
      # Handle case where group.var is not a factor
      group_data[[group.var]] <- replace_na(as.character(group_data[[group.var]]), 'Uncategorised')
    }
    if (!is.factor(group_data[, group.var])) group_data[, group.var] <- as.factor(group_data[, group.var])
    group_data <- group_data[, c(aetype.var, group.var)]
    group_data[,aetype.var] <- as.character(group_data[,aetype.var])
    names(group_data) <- c(aetype.var, '.tmp.group')

    if (!is.null(group.var.priority)) {
      group_data <- group_data %>%
        mutate(.tmp.group = factor(.tmp.group, levels = c(group.var.priority, setdiff(levels(.tmp.group), group.var.priority)), ordered = TRUE))
    }
    # } else {
    #   ## - create a fake grouping
    #   group_data <- data.frame(ae = aetype_lev.raw, group=1, stringsAsFactors = FALSE)
    #   names(group_data) <- c(aetype.var, '.tmp.group')
    # }

    # browser()

    ### bind group back to summary table -trinhdhk
    ae_value.headless <-
      as.data.frame(ae_value, stringsAsFactors = FALSE) %>%
      filter(aename %in% aetype_lev.raw) %>%
      rename({{aetype.var}} := aename) %>%
      right_join(group_data, by = aetype.var) %>%
      group_by(.tmp.group) %>%
      group_modify(~ rbind(c(as.character(.y$.tmp.group),
                             unlist(lapply(seq_along(arm_lev),
                                           function(i){
                                             return(c(
                                               sum(as.numeric(unlist(.x[, 2*i]))),
                                               ''))
                                             # paste(sum(as.numeric(unlist(.x[, length(arm_lev)*2+i+1])))))
                                           })),
                             rep('',ncol(ae_value)-1-2*length(arm_lev))
      ),.x))
    # browser()

    grouptitle_index <- ae_value.headless %>% group_rows() %>% sapply(., function(x) head(x, 1))

    ae_value.headless <-
      ae_value.headless %>%
      ungroup %>%
      select(-.tmp.group)

    #adjust the row number of group title by combining with the head
    grouptitle_index <- grouptitle_index + nrow(ae_value.head)
    ae_value <-
      ae_value.head %>%
      `names<-`(names(ae_value.headless)) %>%
      rbind(ae_value.headless)
  } else ae_value <- as.data.frame(ae_value, stringsAsFactors = FALSE) %>% rename({{aetype.var}} := aename)
  # browser()

  ## test
  if (test) {
    ## <trinhdhk:
    ## - requested by Ronald, add the ability to override the chisq.test with fisher.test when the expected var is <1.
    ## - this should work even when chisq.test == T
    ## >

    do_fisher <- function(mat, workspace = workspace, hybrid = hybrid,
                          simulate.p.value = simulate.p.value, B = B){
      tryCatch(format.pval(fisher.test(x = mat, workspace = workspace, hybrid = hybrid,
                                       simulate.p.value = simulate.p.value, B = B)$p.value,
                           eps = pcutoff, digits = pdigits, scientific = FALSE),
               error = function(e) NA)
    }

    do_chisq <- function(mat, correct = correct,
                         simulate.p.value = simulate.p.value, B = B,
                         workspace = workspace, hybrid = hybrid){
      chi.out <- tryCatch(suppressWarnings(chisq.test(x = mat, correct = correct,
                                                      simulate.p.value = simulate.p.value, B = B)),
                          error = function(e) NA)
      if (length(chi.out) == 1) # that implicits the NA value, so no more NA check here
        return(chi.out)

      if (any(chi.out$expected<1))
        return(do_fisher(mat=mat, workspace = workspace, hybrid = hybrid,
                         simulate.p.value = simulate.p.value, B = B))
      #else
      return(format.pval(chi.out$p.value,eps = pcutoff, digits = pdigits, scientific = FALSE))
    }

    pval <- sapply(1:nrow(value), function(i) {
      idx <- seq(from = i, to = length(patient_n), by = nrow(value))
      mat <- matrix(c(patient_n[idx], patient_N[idx] - patient_n[idx]), nrow = 2, byrow = TRUE)
      out <- if (chisq.test) {
        # tryCatch(format.pval(chisq.test(x = mat, correct = correct,
        #                                 simulate.p.value = simulate.p.value, B = B)$p.value,
        #                      eps = pcutoff, digits = pdigits, scientific = FALSE),
        #          error = function(c) NA)
        do_chisq(mat, correct = correct,
                 simulate.p.value = simulate.p.value, B = B,
                 workspace = workspace, hybrid = hybrid)
      } else {
        # tryCatch(format.pval(fisher.test(x = mat, workspace = workspace, hybrid = hybrid,
        #                                  simulate.p.value = simulate.p.value, B = B)$p.value,
        #                      eps = pcutoff, digits = pdigits, scientific = FALSE),
        #          error = function(c) NA)
        do_fisher(mat, workspace = workspace, hybrid = hybrid,
                  simulate.p.value = simulate.p.value, B = B)
      }
      return(out)
    })
    pval[is.na(pval)] <- "-"
    if (test.anyae.only) {pval[-1] <- ""} #only show test result for anyae - hungtt

    ## add grouping row - trinhdhk
    # browser()
    ae_value <-
      left_join(
        ae_value,
        cbind(aetype=aetype_lev, pval) %>%
          as.data.frame(stringsAsFactors = FALSE) %>%
          rename({{aetype.var}} := aetype),
        by = aetype.var
      ) %>%
      mutate(pval = replace_na(as.character(pval), ''))
  }

  # browser()
  ## sorting - trinhdhk
  if (!missing(sort.by)){
    ## - the head will not be sorted.
    split.parts <- c(1, if(is.null(group.var)) (nrow(ae_value.head)+1) else grouptitle_index)
    col.name <- arrange(mutate(expand.grid(c('ep', 'n.pt'), 1:nlevels(ae_arm$arm)), out = paste(Var1, Var2, sep='.')), Var2)$out
    dummy.col.name <- paste('pt', 1:nlevels(ae_arm$arm), sep='.')
    colnames(ae_value) <- c('aetype', col.name, dummy.col.name, if (test) 'p')
    ae_value <-
      mutate_at(as.data.frame(ae_value, stringsAsFactors = FALSE),
                vars(matches('(^pt.\\d)|(^ep.\\d)'), dummy.col.name), as.numeric) %>%
      mutate(
        pt.all = by(select(., starts_with('pt.')), 1:nrow(.), sum),
        ep.all = by(select(., starts_with('ep.')), 1:nrow(.), sum)
      )

    ae_value.group <- lapply(seq_along(split.parts),
                             function(i){
                               return(ae_value[split.parts[i]:(c(split.parts, nrow(ae_value) + 1)[i+1]-1),])
                             })
    ## - remove the head
    ae_value.head <- ae_value.group[[1]]
    ae_value.group.headless <- ae_value.group[-1]

    ## - convert the sign and var into arrange-friendly quosures.

    arrange.params <- rlang::as_quosures(
      unlist(lapply(seq_along(sort.vars),
                    function(i){
                      sort.var <- sort.vars[[i]]
                      sort.sign <- sort.signs[[i]]
                      if (identical(sort.sign, quote(`+`)))
                        lapply(paste0('~', sort.var, if (sort.var != 'p') paste0('.', c('all',seq_along(arm_lev)))), env = parent.frame(), as.formula)
                      else
                        lapply(paste0('~desc(', sort.var,if (sort.var != 'p') paste0('.', c('all',seq_along(arm_lev))), ')'), env = parent.frame(), as.formula)
                    })
      ), env=NULL)


    ae_value.sorted <- do.call(rbind,
                               lapply(ae_value.group.headless,
                                      function(grouped_ae){
                                        if (is.grouped){
                                          rbind(
                                            grouped_ae[1, ],
                                            do.call(arrange, rlang::flatten(list(
                                              .data=as.data.frame(grouped_ae, stringsAsFactors = FALSE)[-1, ],
                                              unlist(arrange.params))))
                                          )
                                        } else {
                                          do.call(arrange, rlang::flatten(list(
                                            .data=as.data.frame(grouped_ae, stringsAsFactors = FALSE),
                                            unlist(arrange.params))))
                                        }
                                      }))
    ## - bind the head again
    ae_value <- rbind(ae_value.head, ae_value.sorted) %>% mutate_all(as.character) %>% mutate_all(replace_na, '') %>% select(-pt.all, -ep.all)

    # browser()
  }

  ## move missing to bottom - hungtt
  # Move the row with na.text in the first column to the bottom
  df_bottom <- ae_value[!ae_value[,1] == na.text, ] # All rows except '(missing)'
  ae_value <- rbind(df_bottom, ae_value[ae_value[,1] == na.text, ])
  
  ## remove the dummy col -trinhdhk
  ae_value <- ae_value[,-(nlevels(ae_arm$arm)*2 + 2):-(nlevels(ae_arm$arm)*3+1)]
  ae_value <- as.matrix(ae_value) #convert back to matrix to avoid conflicts
  colnames(ae_value) <- NULL


  ## output
  ### header
  gr.lev <- levels(ae_arm$arm)
  header1 <- c("", c(rbind(rep("", length(gr.lev)), paste(gr.lev, " (N=", table(idarm$arm), ")", sep = ""))))
  header2 <- c("Type of adverse event", rep(c("n episode", "n patient"), length(gr.lev)))
  if (test) {
    header1 <- c(header1, "p value")
    header2 <- c(header2, "")
  }

  ### footer
  footer <- c("n episode refers to the number of adverse events in each study arm.",
              "n patient refers to the number of patients with at least one event in each study arm.",
              if (any(value == "-")) "- : value cannot be estimated." else NULL,
              if (test) {paste("p-values were based on",
                               ifelse(chisq.test == FALSE, "Fisher's exact test",
                                      "Chi-squared test if applicable and Fisher if expected value under null <1"))} else NULL,
              footer)


  if (matrix.raw) {tab <- ae_value}
  else
  {### flextable
  if (flextable) {
    requireNamespace("flextable")
    requireNamespace("officer")
    # return(ss_flextable({
    #   tab <- list(table = rbind(header1, header2, ae_value),
    #               footer = footer)
    #   class(tab$table) <- c('ae_tbl', 'ss_tbl', class(tab$table))
    #   class(tab) <- c('ss_ae','ss_obj')
    #   tab
    # }))

    ## main table
    colnames(ae_value) <- rep("", ncol(ae_value))
    tab <- flextable::flextable(as.data.frame(ae_value))

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
    tab <- flextable::bg(tab, i = seq(from = 1, to = nrow(ae_value), by = 2), j = 1:length(header1), bg = bg, part = "body")
    ### border
    tabbd <- officer::fp_border(color="black", width = 1.5)
    tab <- flextable::border_remove(tab)
    tab <- flextable::hline(tab, border = tabbd, part = "header")
    tab <- flextable::hline_top(tab, border = tabbd, part = "all")
    tab <- flextable::hline_bottom(tab, border = tabbd, part = "body")
    if (print.aetype.header){
      tab <- flextable::bold(
        tab,
        i = 2 + if (length(grade.var)) length(unique(ae_data[[grade.var]])) else 0,
        j=1, part = "body")
    }

    ### group-name rows-trinhdhk
    if (is.grouped) {
      tab <- flextable::merge_h_range(tab, grouptitle_index, 1, ncol(ae_value))
      tab <- flextable::bold(tab, i = grouptitle_index, part = "body") # Bold the group.var rows - hungtt
    }

  } else {
    tab <- list(table = rbind(header1, header2, ae_value),
                footer = footer)
    if (print.aetype.header){
      rownames(tab$table)[[4 + if (length(grade.var)) length(unique(ae_data[[grade.var]])) else 0]] <- 'section'
    }
    class(tab$table) <- c('ae_tbl', 'ss_tbl', class(tab$table))
  }
  if (!flextable) class(tab) <- c('ss_ae','ss_obj')}
  return(tab)
}

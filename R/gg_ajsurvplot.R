#' Plot the Aalen-Johansen curve with ggplot for competing risks
#' @description
#' Extend \link[survminer:ggsurvplot]{ggsurvplot} functionality to plot competing risk curve
#'
#' @param formula A standard model formula, with survival on the left and covariates on the right
#' @param data an optional data frame, list or environment (or object coercible by as.data.frame to a data frame) containing the variables in the model
#' @param weights,subset,na.action,etype,count,id,timefix parameter passed to finegray model. See \code{\link[survival:finegray]{finegray}},
#' @param ... optional parameters passed to \code{ggsurvplot}.
#' @export
gg_ajsurvplot <- function(formula, data, weights, subset, na.action, etype, count, id, timefix, ...){
  dot <- list(...)
  fgargs <- match.call()[-1]
  fgargs <- as.list(fgargs[setdiff(names(fgargs), names(dot))])
  fg <- do.call(survival::finegray, fgargs)
  # browser()
  fml <- force(update(formula, Surv(fgstart,fgstop,fgstatus)~.))
  sf <- eval(substitute(survival::survfit(fml, data = fg,  weights=fgwt), list(fml=fml)))
  survminer::ggsurvplot(
    sf,
    data=fg,
    ...
  )
}

#' ggsurvfit with strata saved
#' @description
#' Same as \link[ggsurvfit:ggsurvfit]{ggsurvfit} but with strata saved, so you can facet by covariables.
#'
#' @param x,type,linetype_aes,theme,... parameters passed directly \link[ggsurvfit:ggsurvfit]{ggsurvfit}.
#' @export
ggsurvfit2 <- rlang::new_function(
  args = rlang::fn_fmls(ggsurvfit::ggsurvfit),
  body = {
    # require(cli)
    require(ggsurvfit)
    tt <- ggsurvfit::ggsurvfit |>
    rlang::fn_body() |>
    as.character() |>
    gsub('tidy_survfit', 'C306::tidy_survfit2', x=_)|>
    c('}') |> paste(collapse = '\n') |>
    # parse(text=_) |>
    rlang::parse_expr()
    environment(tt) <- asNamespace('ggsurvfit')
    tt
    }
)

environment(ggsurvfit2) <- asNamespace('ggsurvfit')

#' Untidy the tidy_survfit
untidy_strata <- function(x, strata.col='strata') {
  if (!'strata' %in% names(x)) return(x)
  x2 <- x |>
    mutate(strata.split = strsplit(as.character(strata), ', ')) |>
    tidyr::unnest(cols=strata.split) |>
    mutate(
      strata.split = unlist(strata.split),
      strata.names = strsplit(strata.split, '=') %>% purrr::transpose() %>% `[[`(1) |> unlist(),
      strata.values = strsplit(strata.split, '=') %>% purrr::transpose() %>% `[[`(2)  |> unlist(),
      strata.split=NULL
    ) |>
    tidyr::pivot_wider(names_from = strata.names, values_from = strata.values)

  x2
}

#' Tidy survfit with strata save
#' @description
#' Same as \link[ggsurvfit:tidy_survfit]{tidy_survfit} but with strata saved.
#' @param x,times,type same as \link[ggsurvfit:tidy_survfit]{tidy_survfit}
#' @export
tidy_survfit2 <- function(x, times=NULL, type='survival'){
  ggsurvfit::tidy_survfit(x, times=times, type) |>
    untidy_strata()
}

#' Plot 2 Aalen-Johansen curves with ggplot for competing risks
#' @description
#' Extend \link[ggsurvfit:ggsurvfit]{ggsurvfit} functionality to plot competing risk curve, with main risk and competing risk in one plot
#'
#' @param formula A standard model formula, with survival on the left and covariates on the right
#' @param data an optional data frame, list or environment (or object coercible by as.data.frame to a data frame) containing the variables in the model
#' @param weights,subset,na.action,,count,id,timefix parameter passed to finegray model. See \code{\link[survival:finegray]{finegray}},
#' @param main.event,competing.event main and competing event to plot
#' @param facet.by formula. default to all strata
#' @param ... optional parameters passed to \code{ggsurvplot}.
#' @param .return_data [FALSE] if TRUE, get the data instead.
#' @import ggplot2
#' @export
gg_ajsurvplot2 <- function(formula, data, weights, subset, na.action, main.event, competing.event, facet.by = ~strata, count, id, timefix, ..., .return_data=FALSE){
  dot <- list(...)
  fgargs <- match.call()[-1]
  fgargs$facet.by <- NULL
  fgargs$.return_data <- NULL
  fgargs <- as.list(fgargs[setdiff(names(fgargs), names(dot))])

  etypes <- with(fgargs, list(main.event, competing.event))
  fgargs.each <- lapply(etypes, function(etype) {
    new.arg <- fgargs
    new.arg$etype <- etype
    new.arg$main.event <- new.arg$competing.event <- NULL
    new.arg
  })

  fg1 <- do.call(survival::finegray, fgargs.each[[1]])
  fg2 <- do.call(survival::finegray, fgargs.each[[2]])
  # browser()
  fml <- force(update(formula, survival::Surv(fgstart,fgstop,fgstatus)~.))

  sf1 <- eval(substitute(survival::survfit(fml, data = fg1,  weights=fgwt), list(fml=fml)))
  sf2 <- eval(substitute(survival::survfit(fml, data = fg2,  weights=fgwt), list(fml=fml)))

  tidy1 <- tidy_survfit2(sf1, type='risk') |> mutate(Event = main.event)
  tidy2 <- tidy_survfit2(sf2, type='survival') |> mutate(Event = competing.event)

  dt <- rbind(tidy1, tidy2)
  if (.return_data) return(dt)

  # facet <- if (formula.tools::is.formula(facet.by)) if (length(formula.tools::lhs.vars(facet.by)))
    # facet_grid(facet.by) else facet_wrap(facet.by)
  facet <- if (!is.null(facet.by))
    if (length(formula.tools::lhs.vars(facet.by))) facet_grid(facet.by) else facet_wrap(facet.by)

  if (is.null(facet.by))
    return(
      ggplot(dt,aes(x=time, y=estimate, ymin=conf.low, ymax=conf.high, fill=Event, color=strata)) +
        geom_step(linewidth=1) +
        ggsurvfit::theme_ggsurvfit_default()
    )
  # browser()
  facet.vars <- formula.tools::get.vars(facet.by)
  for (v in facet.vars) dt$strata <- gsub(
    paste0('(,\\s)?',v,'=.*(,\\s|$)'), '', dt$strata, perl=T)
  plt <- if (all(dt$strata == ''))
   ggplot(dt,aes(x=time, y=estimate, ymin=conf.low, ymax=conf.high, group=Event))
  else
    ggplot(dt,aes(x=time, y=estimate, ymin=conf.low, ymax=conf.high, fill=Event, color=strata))
  plt +
    geom_step(linewidth=1) +
    ggsurvfit::theme_ggsurvfit_default() + facet

}

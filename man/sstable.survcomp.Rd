% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sstable.R
\name{sstable.survcomp}
\alias{sstable.survcomp}
\title{Summarize results for a Cox survival model or restricted mean survival time with the treatment arm as the main covariate}
\usage{
sstable.survcomp(
  model,
  data,
  add.risk = TRUE,
  time = Inf,
  reference.arm = c("B", "A"),
  compare.method = c("cox", "rmst", "cuminc"),
  compare.args = list(),
  add.prop.haz.test = TRUE,
  medsum = TRUE,
  p.compare = TRUE,
  digits = 2,
  pdigits = 3,
  pcutoff = 0.001,
  footer = NULL,
  flextable = TRUE,
  bg = "#F2EFEE"
)
}
\arguments{
\item{model}{a formula which can be used to fit the survival model. This formula can include other covariates than arm BUT arm must be the first covariate in the model.}

\item{data}{a data frame to fit the survival model.}

\item{add.risk}{[\code{TRUE}] a logical value specifies whether the event probability ("absolute risk") at time "infinity" should be displayed.}

\item{time}{[\code{Inf}] the truncation time, affecting the descriptive and the RMST model, set to \code{Inf} to perform analyses at maximum time available
(minimax of the observed time across two arms in RMST model)}

\item{reference.arm}{[\code{B}] reference arm, default to the second arm ("B"), change to "A" for base on the first arm}

\item{compare.method}{[\code{cox}] a string, either "cox" for CoxPH model, "cuminc" for cumulative incidence, or "rmst" for restricted mean survival time.
Note that if "cox" is specified and model is a mstate model, a Fine-Gray model is used.
If CoxPH is preferred, used Surv(t, ev == 'event-of-interest') on the LHS.}

\item{add.prop.haz.test}{[\code{TRUE}] (legacy, depricated), please move this to compare.args}

\item{medsum}{[\code{TRUE}] a logical value, specifying whether median (IQR) of time to event should be described.}

\item{p.compare}{[\code{TRUE}] a logical value, specifying whether we should report p-value for the main comparison}

\item{digits}{[\code{2}] a number specifies number of significant digits for numeric statistics.}

\item{pdigits}{[\code{3}] a number specifies number of significant digits for p value.}

\item{pcutoff}{[\code{0.001}] a number specifies threshold value of p value to be displayed as "< pcutoff".}

\item{footer}{a [\code{NULL}] vector of strings to be used as footnote of table.}

\item{flextable}{[\code{TRUE}] a logical value specifies whether output will be a flextable-type table.}

\item{bg}{[\code{#F2EFEE}] a character specifies color of the odd rows in the body of flextable-type table.}

\item{compare.args:}{a list of additional args for compare.methods,

 For compare.method = 'cox', it is
 `add.prop.haz.test` [\code{TRUE}]: a logical value specifies whether a test for proportional hazards should be added,, additional args are fed directly to `survival::coxph`.
 only when model is a mstate model, `cause`, default to whatever the first cause.

 For compare.method = 'cuminc', args are fed to \code{\link[eventglm:cumincglm]{cumincglm}}
 `type`: [\code{diff}] a string, "diff" for difference in cumulative incidence, "ratio' for ratio of cumulative incidence,
 other optional args include: model.censoring, formula.censoring, ipcw.method. See \code{\link[eventglm:cumincglm]{cumincglm}} for more details.

 For compare.method = 'rmst', args are fed to \code{\link[eventglm:rmeanglm]{rmeanglm}} ,
 `type`: [\code{diff}] a string, "diff" for difference in RMST, "ratio' for ratio of RMST, "lost.diff" for difference in restricted mean time lost (RMTL, = -diff), and "lost.ratio" for ratio of RMTL
 other optional args include: model.censoring, formula.censoring, ipcw.method. See \code{\link[eventglm:rmeanglm]{rmeanglm}} for more details.}
}
\value{
a flextable-type table or a list with values/headers/footers
}
\description{
A function to summarize results for a survival model with the treatment arm (variable "arm") as the main covariate
}
\author{
This function was originally written by Marcel Wolbers. Lam Phung Khanh did some modification.
}

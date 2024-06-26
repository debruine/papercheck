% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/study_from_xml.R
\name{search_full_text}
\alias{search_full_text}
\title{Search the full text}
\usage{
search_full_text(
  study,
  term,
  section = NULL,
  refs = FALSE,
  ignore.case = TRUE,
  ...
)
}
\arguments{
\item{study}{a study object created by \code{study_from_xml} or a list of study objects}

\item{term}{the regex term to search for}

\item{section}{the section(s) to search in}

\item{refs}{whether to include references}

\item{ignore.case}{whether to ignore case when text searching}

\item{...}{additional arguments to pass to \code{grepl()}}
}
\value{
a data frame of matching sentences
}
\description{
Search the full text of a study paper. Currently only works with study objects that have full text imported from grobid (e.g., using \code{study_from_xml()}).
}
\examples{
grobid_dir <- system.file("grobid", package="scienceverse")
filename <- file.path(grobid_dir, "eyecolor.pdf.tei.xml")
study <- study_from_xml(filename)
sig <- search_full_text(study, "significant", "results")
}

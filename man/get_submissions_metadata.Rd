% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get-submission-metadata.R
\name{get_submissions_metadata}
\alias{get_submissions_metadata}
\title{Get all submission metadata by state filter}
\usage{
get_submissions_metadata(
  syn,
  group,
  all_users = TRUE,
  state_filter = "SUBMITTED_WAITING_FOR_REVIEW"
)
}
\arguments{
\item{syn}{Synapse login object}

\item{group}{The groupID.}

\item{all_users}{TRUE to get all submissions in group; FALSE to get
group submissions from caller only.}

\item{state_filter}{The filter that is desired to gather submissions by.
Filters are: `WAITING_FOR_SUBMISSION`, `SUBMITTED_WAITING_FOR_REVIEW`,
`ACCEPTED`, `REJECTED`. Only accepts one filter.}
}
\value{
A dataframe of the submission metadata, or NULL if there are
  no submissions that meet the criteria.
}
\description{
Get the metadata for all submissions by state filter:
WAITING_FOR_SUBMISSION, SUBMITTED_WAITING_FOR_REVIEW,
ACCEPTED, REJECTED.
}
\examples{
\dontrun{

# Note: Must have permissions to access group.
# The example group will not work for most individuals.

syn <- log_into_synapse()

sub_meta_to_review <- get_submissions_metadata(
  syn = syn,
  group = 13
)

sub_meta_accepted <- get_submissions_metadata(
  syn = syn,
  state_filter = "ACCEPTED",
  group = 13
)
}
}

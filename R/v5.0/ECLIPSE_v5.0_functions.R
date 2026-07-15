#### --------------------------------------------------------- ####
#### FUNCTIONS FOR CREATION OF ECLIPSE INPUTS or SCALED INPUTS ####

#### -------------------------------------------------------------------------
#### wrapper function to start the chosen process: scale prior made
#### emissions or use actual emissions.
eclipse_v5.0 <- function(type = c("scale", "actual")) {
  match.arg(type)

  if (type == "scale") {
    eclipse_scale()
  } else if (type == "actual") {
    eclipse_actual()
  } else {
    stop("Please choose either 'scale' or 'actual' for the type argument.")
  }
}

#### -------------------------------------------------------------------------
#### wrapper function to call scaling workflow via ECLIPSE v5.0.
eclipse_scale <- function() {
  # --------------------------------------------------- #
  # prepare the ECLIPSE emissions for scaling purposes. #
}

#### -------------------------------------------------------------------------
#### function to

#### -------------------------------------------------------------------------
#### function to

#### -------------------------------------------------------------------------
#### function to

#### -------------------------------------------------------------------------
#### wrapper function to call actual emissions workflow via ECLIPSE v5.0.
eclipse_actual <- function() {}

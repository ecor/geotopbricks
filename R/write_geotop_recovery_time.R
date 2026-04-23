NULL
#'
#' Create a GEOtop Recovery File 
#'
#'
#' @param ...  arguments for \code{\link{writeLines}}
#' @param sim_days Numeric. Number of simulated days completed at the moment of interruption. Defaults to  1  (24 hours).
#' @param elapsed_seconds Numeric. Real CPU time spent before interruption, expressed in seconds. Defaults to  3600  (1 hour).
#' @param i_run Integer. GEOtop run index. 
#' @param i_sim Integer. GEOtop simulation index. 
#' @param last_recover Integer. Flag indicating whether the run was already recovered (1) or not (0). Defaults to 0.
#'
#' @details Generate a valid GEOtop recovery file that allows the model to restart
#' from an arbitrary simulated time. This function is useful when a simulation
#' is interrupted or when you want to force GEOtop to resume from a specific
#' day of the simulation timeline.
#' GEOtop uses a simple CSV-like recovery file to determine the last completed
#' simulation time step. This function writes such a file by specifying the
#' number of simulated days, the elapsed CPU time, and optional run identifiers.
#' The recovery file written by this function contains the following fields:
#'
#' \itemize{
#'   \item \code{Time[s]}  - simulated time in seconds
#'   \item \code{Time[d]} - simulated time in days
#'   \item \code{n} - number of simulated days
#'   \item \code{i_run} - run index
#'   \item \code{i_sim} - simulation index
#'   \item \code{cum_time[s]} - cumulative simulated time in seconds
#'   \item \code{elapsed_time[s]} - real CPU time spent
#'   \item \code{last_recover} - recovery flag
#' }
#'
#' GEOtop will resume the simulation from the time specified in the recovery
#' file if present or suitably set.
#'
#'
#' @name Create a GEOtop Recovery File.
#' @return Invisibly returns the path of the written recovery file.
#' 
#' @export
#' 
#' @examples
#'
#' # Create a recovery file indicating that the simulation
#' # was interrupted after 24 hours (1 simulated day)
#' write_geotop_recovery_time(
#'                       sim_days = 1,
#'                       elapsed_seconds = 3600)
#' 
#' 
#' 



write_geotop_recovery_time <- function(...,
                                  sim_days = 1,
                                  elapsed_seconds = sim_days*12, ## 3600
                                  i_run = 1,
                                  i_sim = 1,
                                  last_recover = 0) {
  
 
  time_s <- sim_days * 24 * 3600
  
  # Header GEOtop
  header <- c(
    "Time[s]", "Time[d]", "n", "i_run", "i_sim",
    "cum_time[s]", "elapsed_time[s]", "last_recover"
  )
  
  # Valori
  values <- c(
    time_s,
    sim_days,
    sim_days,
    i_run,
    i_sim,
    time_s,
    elapsed_seconds,
    last_recover
  )
  
  # Scrittura file
  ff <- as.character(NA)
  ff[1] <- paste(header, collapse = ",") ##, con)
  ff[2] <- paste(values, collapse = ",") ## 
 ## ff[2] <- paste(format(values,justify="none", scientific = FALSE), collapse = ",") ## 
  out <- writeLines(text=ff,...)
  
  
  return(out)
}


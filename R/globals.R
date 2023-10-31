utils::globalVariables(c("prob", "section", "y"))

#' Find Unique List Elements
#'
#'
#' @param list sorted list of vectors
#'
#' @return same list where list elements have non-overlapping contents
#' @export
uniqueListElements <- function(list) {
  # Loop through each element of the list and compare to each other
  for (i in 1:length(list)) { # Set to compare
    for (j in 1:length(list)) { # Set to compare against
      if (i != j) { # Are the indices the same? Skip. Otherwise...
        if (any(list[[i]] %in% names(list)[j])) { # Is j's name in set i?
          list[[i]] <- setdiff(list[[i]], list[[j]]) # Remove j elements from i
        }
      }
    } # end j
  } # end i
  return(list)
}

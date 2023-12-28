# Lineage Related Functions for CCT

#' Check Cache vs Current
#'
#' Checks for cache file, makes one if it doesn't exist, updates if it's out of
#' date, and loads it otherwise. Returns a list with the cached values. Option
#' to skip overwrite of cache and just return lineages. Custom target files for
#' `updateHierarchies()` can be supplied using `aliases_url` and `lineages_url`
#'
#' @import rappdirs magrittr stringr
#'
#' @param overwrite set to false to compare cache to file and return data without writing
#' @param output logical, TRUE for network (produced by fromDFtoNetwork) FALSE for table
#' @param ... Pass alternate `lineages_url` and `aliases_url` to `updateHierarchies`
#'
#' @return depending on the value of output
#'  - table of lineage hierarchies with fixed aliases
#'  - a Node class network object describing the hierarchy of lineages
#' @importFrom stats na.omit
#' @importFrom utils read.table stack
#' @export

checkCache <- function(overwrite = TRUE,
                       network = FALSE,
                       ...) {
  # Do we need a new cache?
  if (Sys.info()["sysname"] == "Windows") {
    # Windows is dumb and will not allow a proper share
    # ALSO WHAT IS THIS PATTERN?! https://xkcd.com/1638/
    cache.dir <- stringr::str_replace_all(rappdirs::user_data_dir(appname = "CCT"), "/CCT/CCT", "\\\\CCT")
    cache.file <- paste0(cache.dir, "\\cct_cache.rda")
  } else {
    # Proper OS gets this treatment
    cache.dir <- rappdirs::user_data_dir(appname = "CCT")
    cache.file <- paste0(cache.dir, "/cct_cache.rda")
  }

  if (file.exists(cache.file)) {
    message("Loading and checking cache file: ", cache.file)
    load(cache.file)
    new.lineages <- data.frame()

    if (!is.null(curl::nslookup("github.com", error = FALSE))) {
      message("Check current status of remote files")
      updated.lineages <- updateHierarchies(...)

      if (is.null(updated.lineages)) {
        message("Error acquiring remote files, using cache")
      } else {
        new.lineages <- dplyr::anti_join(updated.lineages, lineages)
      }
    } else {
      message("No internet access, using cache")
    }


    if (nrow(new.lineages) > 0) {
      message("Lineages list differs from cached, updating lineage hierarchies")
      paste("Number of new lineages added:", message(nrow(new.lineages)))
      lineages <- updated.lineages
      if (overwrite) {
        message("Overwriting ", cache.file, " with new lineage hierarchies")
        # Save data
        save(lineages,
          file = cache.file
        )
      } else {
        message("No overwrite")
      }
    }
  } else {
    message("Getting new cache file data and preparing to write ", cache.file)
    # Make the directory for cache file
    if (!dir.exists(cache.dir)) {
      dir.create(cache.dir)
    }

    # Update data
    lineages <- updateHierarchies(...)
    if (is.null(lineages)) {
      message("Error acquiring remote files, try a different target file")
      message("This often occurs when the target file has bad formatting")
      message("Perhaps point to the previous commit if this error is new")
    } else {
      message("Writing ", cache.file, " with lineage hierarchy table")
      # Save data
      save(lineages,
        file = cache.file
      )
    }
  }


  message("Returning lineage hierarchies as...")

  if(network){
    message("Network")
    CCT::fromDFtoNetwork(lineages)[["network"]]
  }else{
    message("Table")
    lineages
  }

}

#' Update PANGO Hierarchies from Github Files
#'
#' Cov-lineages curates a file that contains all lineages and one that contains
#' all aliases. Use modified code from Julie Chen to build hierarchy table.
#'
#' @param aliases_url cov-lineages alias_key.json
#' @param lineages_url cov-lineages lineages_notes.txt
#'
#' @return dataframe of parent/lineage/children in long format
#' @export

updateHierarchies <- function(aliases_url = "https://raw.githubusercontent.com/cov-lineages/pango-designation/master/pango_designation/alias_key.json",
                              lineages_url = "https://raw.githubusercontent.com/cov-lineages/pango-designation/master/lineage_notes.txt") {
  if (is.null(lineages_url)) {
    lineages_url <- "https://raw.githubusercontent.com/cov-lineages/pango-designation/master/lineage_notes.txt"
  }
  if (is.null(aliases_url)) {
    aliases_url <- "https://raw.githubusercontent.com/cov-lineages/pango-designation/master/pango_designation/alias_key.json"
  }

  ## Get Data and test for errors
  ### load alias json
  aliasData <-
    tryCatch(jsonlite::fromJSON(txt = aliases_url, simplifyVector = F),
      error = function(e) e
    )

  ### load lineage list
  linlist <-
    tryCatch(read.table(
      lineages_url,
      header = T,
      sep = "\t",
      quote = "",
      stringsAsFactors = F
    ), error = function(e) e)

  # If either pull returns an error return NULL
  if (inherits(linlist, "error") | inherits(aliasData, "error")) {
    message(paste0(
      "Error acquiring remote files: \n",
      "lineages - ",
      dplyr::if_else(inherits(linlist, "error"), "FAILED", "SUCCEEDED"),
      "\naliases - ",
      dplyr::if_else(inherits(aliasData, "error"), "FAILED", "SUCCEEDED")
    ))
    message("This often occurs when the target file has bad formatting")
    message("Perhaps point to the previous commit if this error is new")
    return(NULL)
  }

  ## skipping recombination which has multiple parents here
  aliasData.s <-
    sapply(aliasData, function(y) {
      ret <- ifelse(length(y) == 1, y, NA)
      ret
    })
  aliasData.s <- na.omit(aliasData.s)

  ## remove withdrawn ones with the *
  linlist <- linlist[-grep("^\\*", linlist[, 1]), ]
  ## create a new full lineage name without the aliases
  linlist[, 3] <- linlist$Lineage
  for (y in 3:length(aliasData.s)) {
    ## skipping A and B
    id <-
      grep(paste0("^", names(aliasData.s)[y], "\\."), linlist$Lineage)
    linlist[id, 3] <- gsub(names(aliasData.s)[y], aliasData.s[y], linlist[id, 3])
  }

  ### get ancestors in list
  fullHier2 <- lapply(linlist[, 3], function(y) {
    ## split the levels
    tmp <- strsplit(y, "\\.")[[1]]

    ## reconstruct all ancestors, including itself (no alias).
    ancs <- sapply(1:length(tmp), function(x) {
      paste(tmp[1:x], collapse = ".")
    })

    ## output the ancestral lineage names with corresponding aliases, so we can match it up after.
    ## * this step is dependent on the sorting of lineage_note file, from ancestor to descendants, which has been the case.
    ## In the order of child (self) to parent and ancestries
    rev(linlist[, 1][linlist[, 3] %in% ancs])
  })
  names(fullHier2) <- linlist[, 1]


  ### generate the table of hierarchy file
  ancestry <- plyr::ldply(fullHier2, data.frame)
  colnames(ancestry) <- c("children", "lineage")

  directParent <- do.call(rbind, lapply(fullHier2, function(y) y[c(1:2)]))
  colnames(directParent) <- c("lineage", "parent")
  ancestry2 <- merge(data.frame(order = 1:nrow(ancestry), ancestry), directParent, by = "lineage", all.x = T, sort = F)
  ancestry2 <- ancestry2[order(ancestry2$order), c("lineage", "parent", "children")]

  ancestry2
}


#' Build Lineage Node Network from Dataframe
#'
#' Take a dataframe of parent child relationships and create a node network
#' that can be queried for relationships between them. Also returns a table of
#' orphans missing parents as these break the network
#'
#' @param x Dataframe from `updateHierarchies()`
#'
#' @return a list with two elements
#'  - a Node class network object describing the hierarchy of lineages
#'  - a table of so-called orphan lineages that call on non-existent parents
#' @export

fromDFtoNetwork <- function(x) {
  orphans <- # lineages that call on parents who don't exist
    x %>%
    dplyr::filter(
      !is.na(parent),
      !(parent %in% lineage)
    ) %>%
    dplyr::select(!dplyr::any_of("children")) %>%
    dplyr::distinct()

  lineages <-
    dplyr::anti_join(x, orphans) %>%
    dplyr::mutate(parent = tidyr::replace_na(parent, "MRCA"))

  l <-
    lineages %>%
    dplyr::mutate(
      id = lineage,
      parent_id = parent
    ) %>%
    dplyr::select(
      id,
      parent_id,
      lineage
    ) %>%
    dplyr::distinct() %>%
    data.tree::FromDataFrameNetwork() # Needs ID/Parent ID as first two columns or throws error: "Can not find root name. network is not a tree!"

  return(
    list(
      network = l,
      orphans = orphans
    )
  )
}

#' Pull Ancestors from a Specific Lineage
#'
#' Get a lineages ancestors out of a `fromDFtoNetwork()`node network.
#' Vector being TRUE supersedes table being TRUE.
#' Default return node network.
#'
#' @param x node network produce by `fromDFtoNetwork()`
#' @param node_name Lineage of interest ie. "BA.1"
#' @param table logical, return subtree or dataframe of level/lineage
#' @param vector logical, return vector  of lineage names
#'
#' @return one of two options depending on table param
#'  - a Node class network object of the nodes leading to node_name
#'  - a table from the node network with level and lineage names
#'  - a vector of lineage names
#' @export
pullAncestors <-
  function(x,
           node_name,
           table = FALSE,
           vector = FALSE) {
    initial_node <- data.tree::FindNode(x, node_name)

    # Export Subtree up to initial_node
    if (!vector && !table) {
      if (purrr::is_null(initial_node)) {
        return(NULL)
      }
      data.tree::as.Node(data.frame(pathString = initial_node$pathString))
    } else {
      if (vector) {
        # Return vector of lineages
        initial_node$path
      } else {
        # Build DF from tree
        new_tree <-
          data.tree::as.Node(data.frame(pathString = initial_node$pathString)) %>%
          data.tree::ToDataFrameTree(
            "level",
            "lineage"
          )
        new_tree$lineage <- stringr::str_split_1(initial_node$pathString, "/")
        # Return empty dataframe instead of null
        if (purrr::is_null(new_tree)) {
          data.frame(level = numeric(), lineage = character())
        } else {
          new_tree
        }
      }
    }
  }

#' Get Descendants from a Specific Lineage
#'
#' Get a lineage's descendants out of a `fromDFtoNetwork()`node network.
#' Vector being TRUE supersedes table being TRUE.
#' Default return node network.
#'
#' @param x node network produce by `fromDFtoNetwork()`
#' @param node_name Lineage of interest ie. "BA.1"
#' @param table logical, return tree as dataframe of level/lineage
#' @param vector logical, return vector  of lineage names
#'
#' @return one of three options depending on table and vector params
#'  - a Node class network object of the nodes beneath node_name
#'  - a table from the node network with level and lineage names
#'  - a vector of lineage names
#'
#' @export
getDescendants <-
  function(x,
           node_name,
           table = FALSE,
           vector = FALSE) {
    new_tree <- data.tree::FindNode(x, node_name)

    if (!vector && !table) {
      new_tree
    } else {
      # Build DF from tree
      new_tree <- new_tree %>% data.tree::ToDataFrameTree("level", "lineage")
      if (vector) {
        # Return vector of lineages
        new_tree %>%
          dplyr::pull("lineage") %>%
          return()
      } else {
        # Return empty dataframe instead of null
        if (purrr::is_null(new_tree)) {
          data.frame(level = numeric(), lineage = character())
        } else {
          new_tree
        }
      }
    }
  }

#' Get Descendants of a Table of Lineages
#'
#' Using the covariants variants table or other and the network provided by
#' updateHierarchy or checkChache
#'
#' @param x Table of lineages (lineage column necessary)
#' @param network Network produced by `fromDFtoNetwork()`
#' @param unique Each child only has one parent
#' @return Same table with all children added in child column
#' @export
#'
expandChildren <-
  function(x, network, unique = TRUE) {
    # Get descendents of all lineages
    listvars <- x %>%
      purrr::pmap(~ getDescendants(network, .x, table = T, vector = T)) %>%
      purrr::set_names(x$lineage)

    # Ensure uniqueness
    if (unique == TRUE) {
      listvars <- uniqueListElements(listvars)
    }

    # Build table
    listvars <-
      stack(listvars) %>%
      dplyr::select(lineage = ind, child = values)

    # Join with lineage table
    dplyr::left_join(listvars, x)
  }

#' Make a Watchlist Appropriate to Augur
#'
#' Uses a character vector of lineage names to output a yaml array for Augur.
#' Vector can be manually produced or with `getDescendants()`
#'
#' @param x Vector of lineage names
#'
#' @return A YAML ready array
#' @export

makeWatchlist <- function(x) {
  # Create Watchlist
  wl.raw <- paste(x, collapse = "\', \'")

  wl <- paste0("[\ '", wl.raw, "\' ]", collapse = "")
  return(wl)
}

#' Sort Lineages
#'
#' Sort lineages properly into the order defined by their sublineages
#' eg. A < A.1 < A.2 < A.2.1 < A.2.2 < A.2.3 < A.2.4 < A.2.5 < A.2.5.1
#' rather than: A.1 < A.10 < A.11 < A.12 < A.13 < A.14
#'
#'
#' @param x Table with a lineage column to sort by
#' @param column Name of lineage column if not 'lineage'
#' @return Table arranges by lineages in correct order
#' @export

sortLineages <- function(x, column = "lineage") {
  order <-
    x %>%
    dplyr::rename("sorter" = dplyr::all_of(column)) %>%
    splitstackshape::cSplit("sorter", ".", drop = F) %>%
    tibble::as_tibble() %>%
    dplyr::mutate(dplyr::across(any_of(c("sorter_1", "sorter_01", "sorter_001")), as.character)) %>%
    dplyr::select(dplyr::starts_with("sorter")) %>%
    dplyr::mutate(dplyr::across(
      tidyselect::where(is.numeric),
      ~ tidyr::replace_na(.x, 0)
    )) %>%
    dplyr::arrange(dplyr::across(dplyr::starts_with("sorter_"))) %>%
    dplyr::distinct() %>%
    dplyr::mutate(sorter = ordered(sorter, sorter)) %>%
    dplyr::pull(sorter)

  dplyr::arrange(x, match(x[[column]], order))
}

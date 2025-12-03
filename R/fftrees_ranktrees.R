#' Rank FFTs by current goal
#'
#' @description \code{fftrees_ranktrees} ranks trees in an \code{FFTrees} object \code{x}
#' based on the current goal (either \code{"cost"} or as specified in \code{x$params$goal}).
#'
#' \code{fftrees_ranktrees} is called by the main \code{\link{FFTrees}} function
#' when creating FFTs from and applying them to (training) data.
#'
#' @param x An \code{FFTrees} object.
#' @param data The type of data to be used (as character).
#' Default: \code{data = "train"}.
#'
#' @seealso
#' \code{\link{FFTrees}} for creating FFTs from and applying them to data.

fftrees_ranktrees <- function(x,
                              data = "train") {

  # Prepare: ------

  # Verify inputs: ----

  if (class(x) != "FFTrees") stop("x is not class(FFTrees)")
  # testthat::expect_s3_class(x, class = "FFTrees")
  if (!(data %in% c("train", "test"))) stop("data not in c(\"train\", \"test\")")
  # testthat::expect_true(data %in% c("train", "test"))


  # Initialize: ----

  tree_stats <- x$trees$stats[[data]]


  # Provide user feedback: ----

  if (!x$params$quiet$ini) {

    # msg <- paste0("Aiming to rank FFTs by '", data, "' data:\n")
    # cat(u_f_ini(msg))

    n_trees <- x$trees$n
    cli::cli_alert("Rank {n_trees} FFT{?s} by '{data}' data:",
                   class = "alert-start")

  }
  # print(x$trees$definitions) # 4debugging


  # 1. Rank trees by current goal: ----

  if (x$params$goal == "cost") {  # rank by cost:
    tree_rank <- rank(tree_stats$cost, ties.method = "first")
  } else { # rank by current goal:
    tree_rank <- rank(-tree_stats[[x$params$goal]], ties.method = "first")
  }


  # 2. Sort tree rankings by goal (in df): ----

  # Base R
  tree_rank_df <- data.frame(tree = 1:nrow(tree_stats), tree_new = tree_rank)
  tree_rank_df <- tree_rank_df[order(tree_rank_df$tree_new), ]

  # tree_rank_df <- data.frame(
  #   tree = 1:nrow(tree_stats),
  #   tree_new = tree_rank
  # ) %>%
  #   dplyr::arrange(tree_new)



  # 3. Update elements of FFTrees x: ----

  # Tree definitions:
  # Base R
  defs <- merge(x$trees$definitions, tree_rank_df, by = "tree", all.x = TRUE, sort = FALSE)
  defs$tree <- defs$tree_new
  defs$tree_new <- NULL
  defs <- defs[, c("tree", setdiff(names(defs), "tree"))]
  defs <- defs[order(defs$tree), ]
  x$trees$definitions <- tibble::as_tibble(defs)

  # x$trees$definitions <- x$trees$definitions %>%
  #   dplyr::left_join(tree_rank_df, by = "tree") %>%
  #   dplyr::select(-tree) %>%
  #   dplyr::rename(tree = tree_new) %>%
  #   dplyr::select(tree, dplyr::everything()) %>%
  #   dplyr::arrange(tree) %>%
  #   tibble::as_tibble()


  # For training data: ----

  if (data == "train"){

    # Training stats:
    # Base R
    stats_train <- merge(x$trees$stats$train, tree_rank_df, by = "tree", all.x = TRUE, sort = FALSE)
    stats_train$tree <- stats_train$tree_new
    stats_train$tree_new <- NULL
    stats_train <- stats_train[, c("tree", setdiff(names(stats_train), "tree"))]
    stats_train <- stats_train[order(stats_train$tree), ]
    x$trees$stats$train <- tibble::as_tibble(stats_train)

    # x$trees$stats$train <- x$trees$stats$train %>%
    #   dplyr::left_join(tree_rank_df, by = "tree") %>%
    #   dplyr::select(-tree) %>%
    #   dplyr::rename(tree = tree_new) %>%
    #   dplyr::select(tree, dplyr::everything()) %>%
    #   dplyr::arrange(tree) %>%
    #   tibble::as_tibble()

    # Training level_stats:
    l_stats <- merge(x$trees$level_stats$train, tree_rank_df, by = "tree", all.x = TRUE, sort = FALSE)
    l_stats$tree <- l_stats$tree_new
    l_stats$tree_new <- NULL
    l_stats <- l_stats[, c("tree", setdiff(names(l_stats), "tree"))]
    l_stats <- l_stats[order(l_stats$tree, l_stats$level), ]
    x$trees$level_stats$train <- tibble::as_tibble(l_stats)

    # x$trees$level_stats$train <- x$trees$level_stats$train %>%
    #   dplyr::left_join(tree_rank_df, by = "tree") %>%
    #   dplyr::select(-tree) %>%
    #   dplyr::rename(tree = tree_new) %>%
    #   dplyr::select(tree, dplyr::everything()) %>%
    #   dplyr::arrange(tree, level) %>%
    #   tibble::as_tibble()

    # Training decisions:
    x$trees$decisions$train <- x$trees$decisions$train[tree_rank_df$tree]
    names(x$trees$decisions$train) <- paste0("tree_", 1:nrow(tree_rank_df))

    # Get and set value of best training tree:
    x$trees$best$train <- get_best_tree(x, data = "train", goal = x$params$goal)

  } # if (data == "train").


  # Provide user feedback: ----

  if (!x$params$quiet$fin) {

    # msg <- paste0("Successfully ranked FFTs by '", data, "' data.\n")
    # cat(u_f_fin(msg))

    n_trees <- x$trees$n
    cli::cli_alert_success("Ranked {n_trees} FFT{?s} by '{data}' data.")

  }
  # print(x$trees$definitions) # 4debugging


  # Note: The analog (data == "test") case is currently NOT ranked.


  # Output: ----

  return(x)

} # fftrees_ranktrees().

# eof.


# Factor Ordering -----------------------------------------------------------------


#' Factor ordering functions
#'
#' @name fct_ord
#' @param x vector: object to be ordered. Can already be a factor
#' @param rev_lvl logical: whether the factor order should be ordered
#'   descending. IE reverse factor levels.
#' @param by vector: metric to determine factor order
#' @param .agg,.sort function: function to aggregate and sort values. `by` is
#'   grouped by `x` and aggregated by `.agg`. The end result is a named vector,
#'   where each name is a factor level and each value is the aggregated `by`.
#'   The factor levels can then be sorted into their new order by `.sort`.
#'   `.agg` should be a one-argument function that returns a single value.
#'   `.sort` should take and return a named vector of equal length. The order of
#'   the names of the output of `.sort` determines the final factor levels.
#' @param .args_agg,.args_sort list: arguments passed to `.agg` and `.sort`
#' @param ... arguments to pass to `factor`
#' @param data dist-able object: data to be passed to `.dist`. The names of the
#'   object should correspond to the factor levels of the factor that needs to
#'   be ordered.
#' @param .dist,.clust,.lvl function: functions to calculate a distance object
#'   from `data`, cluster based on that distance object, and extract factor
#'   levels from that cluster object. `.dist` should take `data` as its first
#'   arguments and return a distance object. The default `dist` calculates
#'   distances between the rows of a matrix.`.clust` should take that distance
#'   object as its first argument and return a cluster object. The default
#'   `hclust` performs hierarchical clustering. `.lvl` should take that cluster
#'   object and extract a character string to use as factor levels.
#' @param .args_clust,.args_dist list: arguments to pass to `.clust` and `.dist`
#' @param return_clust logical: should the cluster object be returned with the
#'   factor? If so, a list is returned with the factor in the first position and
#'   the cluster object in the second.
#'
#' @return factor: factor ordered accoring to functions. Possibly with cluster
#'   object.
NULL


#' @export
#' @rdname fct_ord
fct_ord_alphabet <- function(x, ..., rev_lvl = FALSE) {
    stopifnot(rlang::is_scalar_logical(rev_lvl))

    lvls <- sort(rlang::as_character(unique(x)))

    if (rev_lvl) {
        lvls %<>% rev()
    }

    factor(x, levels = lvls, ...)
}

#' @export
#' @rdname fct_ord
fct_ord_natural <- function(x, ...) {

    # set ordered default for better parallel with `factor`
    natural_factor <- naturalsort::naturalfactor
    formals(natural_factor)$ordered <- FALSE

    natural_factor(x,...)
}


#' @export
#' @rdname fct_ord
fct_ord_by <-
    function(x,
             by,
             ...,
             .agg = base::mean,
             .args_agg = list(),
             .sort = base::sort,
             .args_sort = list(),
             rev_lvl = FALSE) {

        stopifnot(rlang::is_function(.agg), rlang::is_function(.sort))
        stopifnot(rlang::is_list(.args_agg), rlang::is_list(.args_sort))
        stopifnot(rlang::is_scalar_logical(rev_lvl))

        # f <- function(.x)  {
        #     rlang::invoke(.agg, rlang::prepend(list(.x), .args_agg))
        # }
        #
        agg <-
            by %>%
            split(x) %>%
            purrr::map(
                ~rlang::invoke(.agg, rlang::prepend(list(.x), .args_agg))
            ) %>%
            unlist(use.names = TRUE)

        lvls <-
            rlang::invoke(.sort, rlang::prepend(list(agg), .args_sort)) %>%
            names()

        if (rev_lvl) {
            lvls %<>% rev()
        }

        factor(x, levels = lvls, ...)
    }


#' @export
#' @rdname fct_ord
fct_ord_clust <-
    function(x,
             data,
             ...,
             .dist = stats::dist,
             .args_dist = list(),
             .clust = stats::hclust,
             .args_clust = list(),
             .lvl = function(x) x$labels[x$order], # for hclust objects
             rev_lvl = FALSE,
             return_clust = FALSE) {

        stopifnot(
            rlang::is_function(.dist),
            rlang::is_function(.clust),
            rlang::is_function(.lvl)
        )
        stopifnot(
            rlang::is_scalar_logical(rev_lvl),
            rlang::is_scalar_logical(return_clust)
        )

        stopifnot(
            rlang::is_list(.args_dist),
            rlang::is_list(.args_clust)
        )



        dist_obj <- rlang::invoke(.dist, rlang::prepend(list(data), .args_dist))
        clust_obj <- rlang::invoke(.clust, rlang::prepend(list(dist_obj), .args_clust))
        lvls <- .lvl(clust_obj)

        if (rev_lvl) {
            lvls %<>% rev()
        }

        stopifnot(setequal(lvls, unique(x)))

        fct <- factor(x, levels = lvls, ...)

        if (return_clust) {
            list(fct = fct, clust = clust_obj)
        } else {
            fct
        }
    }


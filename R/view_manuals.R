#' View the various PROMISE manuals.
#'
#' The manuals are the documentations relevant to the PROMISE dataset, such as
#' data dictionary, the available datasets, and the methods for PROMISE.
#'
#' @param manual The manual to view.
#' @param view_from Whether to view the manual from the PROMISE website, as the
#'   html vignette in a browser, or as the vignette inside RStudio.
#'
#' @export
#'
view_manual <-
    function(manual = list_manuals,
             view_from = c('website', 'rstudio', 'browser')) {
        name <- match.arg(manual)
        switch(
            name,
            dictionary = manual('dictionary', view_from),
            datasets = manual('datasets', view_from),
            methods = manual('methods', view_from)
        )
    }

#' @rdname view_manual
#' @export
list_manuals <- c(
    'methods',
    "datasets",
    'dictionary'
)

manual <- function(doc, view_from = c('website', 'rstudio', 'browser')) {
    stopifnot(is.character(doc))
    viewing <- match.arg(view_from)

    pkg <- 'PROMISE.data'

    switch(
        viewing,
        browser = utils::RShowDoc(doc, 'html', pkg),
        rstudio = utils::vignette(doc, package = pkg),
        website = {
            main_url <- "https://promise-cohort.gitlab.io/"
            page <- paste0(main_url, "PROMISE/articles/", doc, ".html")
            utils::browseURL(page)
        }
    )
}

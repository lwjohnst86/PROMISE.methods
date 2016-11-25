#' View the various PROMISE manuals.
#'
#' These manualsthat contain more extended documentation on the data cleaning
#' and wrangling processes and on how to add to the dataset whenever new data
#' comes in.
#'
#' @param manual The manual to view.
#' @param view_from Whether to view the manual from a browser or inside RStudio.
#'
#' @export
#'
view_manual <-
    function(manual = list_manuals,
             view_from = c('rstudio', 'browser', 'website')) {
        name <- match.arg(manual)
        switch(
            name,
            dictionary = manual('dictionary', view_from),
            methods = manual('methods', view_from)
        )
    }

#' @rdname view_manual
#' @export
list_manuals <- c(
    'methods',
    'dictionary'
)

manual <- function(doc, view_from = c('rstudio', 'browser', 'website')) {
    stopifnot(is.character(doc))
    viewing <- match.arg(view_from)
    switch(
        viewing,
        browser = utils::RShowDoc(doc, 'html', 'PROMISE.methods'),
        rstudio = utils::vignette(doc, package = 'PROMISE.methods'),
        website = warning("Has not been implemented yet")
    )
}

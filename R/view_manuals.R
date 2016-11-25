#' View the various PROMISE manuals.
#'
#' These manualsthat contain more extended documentation on the data cleaning
#' and wrangling processes and on how to add to the dataset whenever new data
#' comes in.
#'
#' @param manual The manual to view.
#' @param view_from Whether to view the manual from a browser or inside RStudio.
#' @export
#'
view_manual <-
    function(manual = list_manuals,
             view_from = c('rstudio', 'browser', 'website')) {
        name <- match.arg(manual)
        switch(
            name,
            introduction = dictionary_manual(view_from),
            methods = methods_manual(view_from)
        )
    }

#' @rdname view_manual
#' @export
list_manuals <- c(
    'methods',
    'dictionary'
)

#' @rdname view_manual
#' @export
view_methods_manual <- function(view_from = c('rstudio', 'browser', 'website')) {
    manual('methods', view_from)
}

#' @rdname view_manual
#' @export
view_dictionary_manual <- function(view_from = c('rstudio', 'browser', 'website')) {
    manual('dictionary', view_from)
}

manual <- function(doc, view.from = c('browser', 'rstudio')) {
    stopifnot(is.character(doc))
    viewing <- match.arg(view_from)
    switch(
        viewing,
        browser = RShowDoc(doc, 'html', 'PROMISE.methods'),
        rstudio = vignette(doc, package = 'PROMISE.methods'),
        website = warning("Has not been implemented yet")
    )
}

#' @importFrom magrittr %>%
#' @export
magrittr::'%>%'

.onLoad <- function(libname, pkgname) {
    op <- options()
    op.PROMISE <- list(
        PROMISE.yaml.path = tempdir()
    )
    toset <- !(names(op.PROMISE) %in% names(op))
    if (any(toset))
        options(op.PROMISE[toset])

    invisible()
}

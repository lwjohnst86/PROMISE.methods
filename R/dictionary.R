#' Reset the added-variables dictionary file.
#'
#' Deletes the dictionary yaml file, so that it can be updated again. Really
#' should only be used for the \code{added-variables.yaml} dictionary.
#'
#' @param filename The name of the yaml dictionary file
#' @export
dictionary_reset <- function(filename = 'added-variables.yaml') {
    stopifnot(is.character(filename))
    dict_file <- dict_yaml_file(filename)
    if (file.exists(dict_file)) {
        message("* Resetting the 'added-variables' data dictionary.")
        invisible(file.remove(dict_file))
    } else {
        invisible()
    }
}
#' Create a properly formatted data dictionary entry.
#'
#' Can only be added to the data dictionary using \code{\link{dictionary_update}}.
#'
#' @param variable The name of the added variable (eg. 'BMI').
#' @param unit The unit of the variable (eg. 'kg')
#' @param type The data value type (eg. 'Numeric')
#' @param description A brief description of the dictionary entry.
#'
#' @export
dictionary_entry <-
    function(variable,
             unit,
             type = c('Numeric', 'Discrete', 'Date'),
             description) {

        stopifnot(!missing(variable),
                  !missing(unit),
                  !missing(description))
        stopifnot(is.character(variable),
                  is.character(unit),
                  is.character(description))

        type <- match.arg(type)

        dict_list <- list(name = list(
                unit = unit,
                type = type,
                description = description
            ))
        names(dict_list) <- variable

        return(dict_list)
    }

#' Update the dictionary with the entries created using \code{\link{dictionary_entry}}.
#'
#' Adds the entries to the \code{added-variables.yaml} file.
#'
#' @param entries Properly formatted entries piped (or not) from the
#'   \code{\link{dictionary_entry}} function.
#' @param force_update Force the dictionary file to be updated (ie. the entry is
#'   removed and added again).
#' @param dict_name Name of the dictionary yaml file.
#'
#' @export
dictionary_update <-
    function(entries,
             force_update = FALSE,
             dict_name = 'added-variables.yaml') {
        stopifnot(is.vector(entries), is.character(dict_name))

        filename <- dict_yaml_file(dict_name)
        var_names <- names(entries)

        if (!file.exists(filename)) {
            message(dict_name, ' file does not exist, creating one.')
            create_yaml_dictionary(filename, entries)
            message('... Added the entries to the dictionary.')
        } else {
            dictionary <- yaml::yaml.load_file(filename)
            if (any(var_names %in% names(dictionary))) {
                message('Some or all of the entries already exists in ', dict_name, '.')
                if (force_update) {
                    new_dict <- clear_dictionary_variables(dictionary, entries)
                    create_yaml_dictionary(filename, new_dict, create_file = FALSE)
                    message('... Forced update of the entries.')
                } else {
                    message('... skipping adding them')
                }
            } else {
                append_to_dictionary(filename, entries)
                message('Added the entries to the dictionary.')
            }
        }
    }

create_yaml_dictionary <-
    function(filename, entries, create_file = TRUE) {
        stopifnot(is.character(filename), is.list(entries))
        if (create_file)
            file.create(filename, showWarnings = FALSE)
        write(
            paste0(
                '## **Do NOT edit by hand, unless _absolutely necessary_.**\n',
                '## Please use the update_dictionary() function in the PROMISE.methods package.\n\n',
                yaml::as.yaml(entries)
            ),
            file = filename
        )
    }

append_to_dictionary <- function(filename, entries) {
    stopifnot(is.character(filename), is.list(entries))
    write(yaml::as.yaml(entries),
          file = filename,
          append = TRUE)
}

clear_dictionary_variables <-
    function(dictionary, new_entries) {
        stopifnot(is.list(dictionary), is.list(new_entries))

        dictionary[names(new_entries)] <- NULL

        if (length(dictionary) == 0) {
            new_dict <- c(new_entries)
        } else {
            new_dict <- c(dictionary, new_entries)
        }

        new_dict
    }

dict_yaml_file <- function(filename) {
    file.path(getOption('PROMISE.yaml.path'), filename)
}

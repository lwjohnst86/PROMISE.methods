#' Generate the data dictionary tables of all yaml files.
#'
#' This is used on the PROMISE data dictionary to create the dictionary from the
#' rename yaml files.
#'
#' @return Prints all tables related to the dictionary.
#'
#' @export
generate_dictionary <- function() {
    files <- basename(multiple_files('\\.yaml$', getOption('PROMISE.yaml.path')))
    for (fi in files[1:length(files)]) {
        print(suppressWarnings(create_dictionary_table(fi)))
    }
    invisible(NULL)
}

#' Create a dictionary table from YAML data.
#'
#' @param filename YAML filename.
#'
#' @return Creates a markdown table.
#' @export
create_dictionary_table <- function(filename) {
    dict_file <- dict_yaml_file(filename)
    stopifnot(file.exists(dict_file))

    name <- gsub('\\.yaml$', '', filename)
    caption <- paste0('Data dictionary for `', name, '` dataset.')

    dict_data <- yaml::yaml.load_file(dict_file)
    names(dict_data) <- gsub('\\.', '', names(dict_data))
    dict_data <- data.frame(dict_data, stringsAsFactors = FALSE) %>%
        tidyr::gather('Variable', 'Value') %>%
        tidyr::separate(Variable, into = c('Variable', 'Item'), sep = '\\.') %>%
        dplyr::mutate(Item = factor(Item, levels = unique(Item)),
               Variable = factor(Variable, levels = unique(Variable))) %>%
        tidyr::spread(Item, Value)

    names(dict_data) <- string_to_titlecase(names(dict_data))

    cat(paste('\n##', name))
    knitr::kable(dict_data, caption = caption)
}

multiple_files <- function(pattern, path) {
    list.files(path,
               pattern = pattern,
               full.names = TRUE)
}

string_to_titlecase <- function(x) {
    word_pattern <- "\\b([[:lower:]])([[:lower:]]+)"
    gsub(word_pattern, "\\U\\1\\E\\2", x, perl = TRUE)
}

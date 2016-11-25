#' Generate the data dictionary tables of all yaml files.
#'
#' This is used on the PROMISE data dictionary to create the dictionary from the
#' rename yaml files.
#'
#' @return Prints all tables related to the dictionary.
#'
#' @export
generate_dictionary <- function() {
    files <- basename(PROMISE.scrub::multiple_files('\\.yaml$', getOption('PROMISE.yaml.path')))
    for (fi in files[1:length(files)]) {
        print(suppressWarnings(create_dictionary_table(fi)))
    }
    invisible(NULL)
}

create_dictionary_table <- function(filename) {
    dict_file <- dict_yaml_file(filename)
    assertive::assert_all_are_existing_files(dict_file)

    name <- gsub('\\.yaml$', '', filename)
    caption <- paste0('Data dictionary for `', name, '` dataset.')

    dict_data <- yaml::yaml.load_file(dict_file)
    names(dict_data) <- gsub('\\.', '', names(dict_data))
    dict_data <- data.frame(dict_data, stringsAsFactors = FALSE) %>%
        tidyr::gather(Variable, Value) %>%
        tidyr::separate(Variable, into = c('Variable', 'Item'), sep = '\\.') %>%
        dplyr::mutate(Item = factor(Item, levels = unique(Item)),
               Variable = factor(Variable, levels = unique(Variable))) %>%
        tidyr::spread(Item, Value)

    names(dict_data) <- stringi::stri_trans_totitle(names(dict_data))

    cat(paste('\n##', name))
    knitr::kable(dict_data, caption = caption)
}

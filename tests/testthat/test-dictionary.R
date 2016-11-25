context("Data dictionary")

test_that("dictionary reset works", {
    test_file <- tempfile('test', fileext = '.yaml')
    file.create(test_file)
    expect_message(dictionary_reset(basename(test_file)))
})

test_that("dictionary entry is created properly", {
    entry <- dictionary_entry('Weight', 'kg', 'Numeric', 'Persons weight')
    expect_is(entry, 'list')
    expect_length(entry, 1)
    expect_length(entry[[1]], 3)
    expect_error(dictionary_entry(1, 1, description = 1))
    expect_error(dictionary_entry())
})

test_that("entry added to dictionary", {
    path <- getOption("PROMISE.yaml.path")
    entries <- c(
        dictionary_entry('bmi', 'kg/m2', 'Numeric', 'BMi of individual'),
        dictionary_entry('wt', 'kg', 'Numeric', 'Weight of individual')
    )
    dictionary_update(entries, dict_name = "test.yaml")
    expect_true(file.exists(file.path(path, 'test.yaml')))
    file.copy(file.path(path, 'test.yaml'), file.path(path, 'test1.yaml'))

    entries <- c(
        dictionary_entry('ht', 'm', 'Numeric', 'Height of individual'),
        dictionary_entry('sex', 'F/M', 'Discrete', 'Sex of individual')
    )
    dictionary_update(entries, dict_name = "test.yaml")
    expect_gt(
        file.info(file.path(path, 'test.yaml'))$size,
        file.info(file.path(path, 'test1.yaml'))$size
    )
    file.copy(file.path(path, 'test.yaml'), file.path(path, 'test2.yaml'))

    entries <- c(
        dictionary_entry('bmi', 'kg/m2', 'Numeric', 'BMi of individual'),
        dictionary_entry('wt', 'kg', 'Numeric', 'Weight of individual')
    )
    dictionary_update(entries, dict_name = "test.yaml")
    expect_equal(
        file.info(file.path(path, 'test.yaml'))$size,
        file.info(file.path(path, 'test2.yaml'))$size
    )

    dictionary_update(entries, dict_name = "test.yaml", force_update = TRUE)
    expect_equal(
        file.info(file.path(path, 'test.yaml'))$size,
        file.info(file.path(path, 'test2.yaml'))$size - 1 # since update removes empty lines
    )
})

test_that("dictionary tables are created", {
    path <- getOption("PROMISE.yaml.path")
    entries <- c(
        dictionary_entry('bmi', 'kg/m2', 'Numeric', 'BMi of individual'),
        dictionary_entry('wt', 'kg', 'Numeric', 'Weight of individual')
    )
    dictionary_update(entries, dict_name = "test_table.yaml")

    capture.output(expect_is(create_dictionary_table('test_table.yaml'), 'knitr_kable'))
    capture.output(expect_length(create_dictionary_table('test_table.yaml'), 4))
    expect_is(capture.output(generate_dictionary()), 'character')
})

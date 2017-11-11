context("utils")

test_that("string is converted to title case", {
    orig_str <- c(
        "first phrase",
        "another phrase to convert",
        "and here's another one",
        "last-one",
        "using AIC for model selection"
    )

    correct_str <- c(
        "First Phrase",
        "Another Phrase To Convert",
        "And Here's Another One",
        "Last-One",
        "Using AIC For Model Selection"
    )
    expect_equal(string_to_titlecase(orig_str), correct_str)
})

test_that("multiple files exist", {
    temp_dir <- tempdir()
    real_files <- tempfile(paste0("file", 1:10), tmpdir = temp_dir, fileext = "txt")
    file.create(real_files)
    expect_equal(multiple_files("file", temp_dir), sort(real_files))
    expect_equal(multiple_files("txt", temp_dir), sort(real_files))
})

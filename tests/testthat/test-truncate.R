test_n_byte <- function(n, input) {
  testthat::expect_equal(
    nchar(enc2utf8(input), type = "bytes"),
    nchar(input, type = "chars") * n
  )

  bytesize <- 3L * n
  truncated <- byte_trunc(input, bytesize, ellipsis = "")

  testthat::expect_lte(nchar(truncated, type = "bytes"), bytesize)
  testthat::expect_equal(truncated, substr(input, 1L, 3L))
}

test_that("single-byte characters works", {
  test_n_byte(1L, "abcde")
})

test_that("double-byte characters works", {
  input <- "\u00c4\u00cb\u00cf\u00d6\u00dc"
  test_n_byte(2L, input)
})

test_that("triple-byte characters works", {
  input <- "\uff21\uff22\uff23\uff24\uff25"
  test_n_byte(3L, input)
})

test_that("quadruple-byte characters works", {
  input <- "\U0001F600\U0001F601\U0001F602\U0001F603\U0001F604"
  test_n_byte(4L, input)
})

test_that("NA is ignored", {
  expect_equal(byte_trunc(NA_character_, 3L), NA_character_)
})

test_that("latin1 encoding works", {
  input <- "\xc0\xc1\xc2\xc3\xc4"
  Encoding(input) <- "latin1"
  expect_equal(byte_trunc(input, 2L, ellipsis = ""), "\u00c0")
})

test_that("vectors work", {
  input <- c(
    "AEIOU",
    "\u00c4\u00cb\u00cf\u00d6\u00dc",
    "\uff21\uff25\uff29\uff2f\uff35",
    "\U0001f130\U0001f131\U0001f132\U0001f133\U0001f134",
    NA_character_
  )

  bytesize <- 6L
  truncated <- byte_trunc(input, bytesize, ellipsis = "")

  expect_length(truncated, length(input))
  expect_true(all(nchar(truncated, type = "bytes") <= bytesize, na.rm = TRUE))
  expect_equal(
    truncated,
    c(
      "AEIOU",
      "\u00c4\u00cb\u00cf",
      "\uff21\uff25",
      "\U0001f130",
      NA_character_
    )
  )
})

test_that("ellipsis works", {
  expect_equal(byte_trunc("ABCDEF", 6L), "ABCDEF")
  expect_equal(byte_trunc("ABCDEFG", 6L), "ABC...")
  expect_equal(byte_trunc("ABCDEFG", 6L, ellipsis = ".."), "ABCD..")
  expect_equal(byte_trunc("ABCDEFG", 6L, ellipsis = "...."), "AB....")
  expect_equal(byte_trunc("ABCDEFG", 6L, ellipsis = "......"), "......")
  expect_error(byte_trunc("ABCDEFG", 6L, ellipsis = "......."))
})

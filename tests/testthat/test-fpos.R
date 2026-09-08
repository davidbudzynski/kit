# Tests for fpos() — fast matrix pattern search
# Migrated from tests/test_kit.R (custom check() suite) to testthat 3e — see issue #54.
# Each legacy check("id", actual, expected) maps to expect_identical();
# 31 tolerant cases (identical FALSE but all.equal+typeof TRUE) use expect_kit_equal();
# check(..., error=) maps to expect_error(regexp=, fixed=TRUE).
# Legacy IDs are preserved as test_that() labels (prefixed by function for uniqueness).
# Setup and expectations stay interleaved in legacy order: later sections may
# redefine x/y/out_vec/... so setup must not be hoisted above earlier tests.

set.seed(123)

mymatrix = matrix(
  c(30,-16,22,17,-14,13,43,-26,45,49,-46,-20,-48,-45,5,-43,34,-4,-46,-32,
    3,3,-46,-38,-2,21,-41,-29,12,-22,-1,-28,-19,-31,-3,42,47,12,-39,17,
    42,16,24,-41,9,1,-21,-11,24,-25,36,43,22,27,-32,-12,-16,-14,-47,36,
    -41,0,28,11,35,-4,42,42,28,10,13,-25,14,-36,-45,0,18,10,16,-6,
    -11,26,-14,19,-19,-30,-6,20,-28,-36,-34,-12,-45,-28,-41,-34,39,27,-34,15,
    -45,41,-10,33,34,-46,24,-15,-40,36,21,-4,-18,-3,-1,30,-18,-12,-46,44,
    30,3,-26,29,7,-8,38,-11,-19,24,-15,-13,20,-26,-19,46,-5,-1,26,41,
    6,-47,-4,29,27,-37,46,21,13,12,37,50,12,30,34,-35,-22,23,-31,22,
    -8,38,-16,14,-2,0,-4,47,-2,13,6,-26,-36,31,43,-36,20,37,45,37,
    8,37,-43,-48,37,-39,6,23,-8,-14,26,14,14,48,4,-3,3,-32,-35,1,
    8,42,28,-6,-16,-27,19,-38,-14,-43,-33,-35,-17,49,-7,22,36,-31,17,-45,
    -40,4,-32,-39,33,-41,18,-50,-48,38,-5,-27,-44,7,23,38,-13,9,31,29,
    -21,-6,-43,-42,-25,-46,-4,48,11,3,-43,42,-9,45,48,16,24,-38,-32,38,
    38,44,18,11,-5,45,-29,26,-50,18,-11,-43,-8,-37,24,-41,-37,44,-18,38,
    25,-39,-13,26,-20,30,-1,-5,-22,42,-11,-2,-42,-43,0,-49,12,-2,-16,34),
  nrow = 20, ncol = 15
)
mat_lgl_a = mat_cpl_a = mat_chr_a = mat_dbl_a = mat_int_a = mymatrix
mat_cpl_b = mat_dbl_b = mat_chr_b = mat_int_b = mat_lgl_b = mat_int_a
storage.mode(mat_dbl_a) = "numeric"
storage.mode(mat_int_a) = "integer"
storage.mode(mat_chr_a) = "character"
storage.mode(mat_cpl_a) = "complex"
storage.mode(mat_lgl_a) = "logical"
mat_raw_a = abs(mat_int_a); storage.mode(mat_raw_a) = "raw"
mat_dbl_b[2,2] = NA; storage.mode(mat_dbl_b) = "numeric"
mat_int_b[2,2] = NA; storage.mode(mat_int_b) = "integer"
mat_chr_b[2,2] = NA; storage.mode(mat_chr_b) = "character"
mat_lgl_b[2,2] = NA; storage.mode(mat_lgl_b) = "logical"
mat_cpl_b[2,2] = NA; storage.mode(mat_cpl_b) = "complex"
big_matrix = matrix(c(1:5), nrow = 10, ncol = 5)
small_matrix = matrix(c(2:3), nrow = 2, ncol = 2)
big_matrix_d = big_matrix_ch = big_matrix_l = big_matrix_cp = big_matrix
small_matrix_d = small_matrix_ch = small_matrix_l = small_matrix_cp = small_matrix
storage.mode(big_matrix_d) = "numeric"
storage.mode(big_matrix_ch) = "character"
storage.mode(big_matrix_cp) = "complex"
storage.mode(big_matrix_l) = "logical"
storage.mode(small_matrix_d) = "numeric"
storage.mode(small_matrix_ch) = "character"
storage.mode(small_matrix_cp) = "complex"
storage.mode(small_matrix_l) = "logical"
class2133 = setClass("class2133", slots=list(x="numeric"))
s1 = class2133(x=20191231)
s2 = class2133(x=20191230)

test_that("fpos-0004.001", {
  expect_identical(fpos(mat_dbl_a[1:2,1:2], mat_dbl_a, FALSE), matrix(c(1L,1L),nrow = 1))
})


test_that("fpos-0004.002", {
  expect_identical(fpos(mat_dbl_a[1:2,9:10], mat_dbl_a, FALSE), matrix(c(1L,9L),nrow = 1))
})


test_that("fpos-0004.003", {
  expect_identical(fpos(mat_dbl_a[19:20,1:2], mat_dbl_a, FALSE), matrix(c(19L,1L),nrow = 1))
})


test_that("fpos-0004.004", {
  expect_identical(fpos(mat_dbl_a[19:20,9:10], mat_dbl_a, FALSE), matrix(c(19L,9L),nrow = 1))
})


test_that("fpos-0004.005", {
  expect_identical(fpos(mat_int_a[1:2,1:2], mat_int_a, TRUE), matrix(c(1L,1L),nrow = 1))
})


test_that("fpos-0004.006", {
  expect_identical(fpos(mat_int_a[1:2,9:10], mat_int_a, TRUE), matrix(c(1L,9L),nrow = 1))
})


test_that("fpos-0004.007", {
  expect_identical(fpos(mat_int_a[19:20,1:2], mat_int_a, TRUE), matrix(c(19L,1L),nrow = 1))
})


test_that("fpos-0004.008", {
  expect_identical(fpos(mat_int_a[19:20,9:10], mat_int_a, TRUE), matrix(c(19L,9L),nrow = 1))
})


test_that("fpos-0004.009", {
  expect_identical(fpos(mat_chr_a[1:2,1:2], mat_chr_a, FALSE), matrix(c(1L,1L),nrow = 1))
})


test_that("fpos-0004.010", {
  expect_identical(fpos(mat_chr_a[1:2,9:10], mat_chr_a, FALSE), matrix(c(1L,9L),nrow = 1))
})


test_that("fpos-0004.011", {
  expect_identical(fpos(mat_chr_a[19:20,1:2], mat_chr_a, FALSE), matrix(c(19L,1L),nrow = 1))
})


test_that("fpos-0004.012", {
  expect_identical(fpos(mat_chr_a[19:20,9:10], mat_chr_a, FALSE), matrix(c(19L,9L),nrow = 1))
})


test_that("fpos-0004.013", {
  expect_identical(fpos(mat_lgl_a[2:3,4:5], mat_lgl_a, TRUE), matrix(c(2L,16L,6L,4L,4L,9L),nrow = 3))
})


test_that("fpos-0004.014", {
  expect_identical(fpos(mat_lgl_a[1:2,9:10], mat_lgl_a, FALSE), matrix(c(1L,1L),nrow = 1))
})


test_that("fpos-0004.015", {
  expect_identical(fpos(mat_lgl_a[19:20,1:2], mat_lgl_a, FALSE), matrix(c(1L,1L),nrow = 1))
})


test_that("fpos-0004.016", {
  expect_identical(fpos(mat_lgl_a[14:15,14:15], mat_lgl_a, TRUE), matrix(c(1L,15L,5L,14L,3L,3L,8L,14L),nrow = 4))
})


test_that("fpos-0004.017", {
  expect_error(fpos(mat_lgl_a[19:20,9:10], mat_lgl_a, c(TRUE,FALSE)), regexp = "Argument 'all' must be TRUE or FALSE and length 1.", fixed = TRUE)
})


test_that("fpos-0004.018", {
  expect_error(fpos(mat_lgl_a[19:20,9:10], c(1:5),TRUE), regexp = "One of the dimension of the small matrix is greater than the large matrix.", fixed = TRUE)
})


test_that("fpos-0004.019", {
  expect_identical(fpos(TRUE, mat_lgl_a[19:20,9:10], FALSE), matrix(c(1L,1L),nrow = 1))
})


test_that("fpos-0004.020", {
  expect_error(fpos(mat_lgl_a, mat_lgl_a[19:20,9:10], TRUE), regexp = "One of the dimension of the small matrix is greater than the large matrix.", fixed = TRUE)
})


test_that("fpos-0004.021", {
  expect_error(fpos(mat_raw_a[1:2,1:2], mat_raw_a, TRUE), regexp = "Type raw for 'haystack' is not supported.", fixed = TRUE)
})


test_that("fpos-0004.022", {
  expect_error(fpos(mat_raw_a[1:2,1:2], mat_int_a, TRUE), regexp = "Type raw for 'needle' is not supported.", fixed = TRUE)
})


test_that("fpos-0004.023", {
  expect_identical(fpos(mat_dbl_b[1:2,1:2], mat_dbl_b, FALSE), matrix(c(1L,1L),nrow = 1))
})


test_that("fpos-0004.024", {
  expect_identical(fpos(mat_int_b[1:2,1:2], mat_int_b, FALSE), matrix(c(1L,1L),nrow = 1))
})


test_that("fpos-0004.025", {
  expect_identical(fpos(mat_chr_b[1:2,1:2], mat_chr_b, FALSE), matrix(c(1L,1L),nrow = 1))
})


test_that("fpos-0004.026", {
  expect_identical(fpos(mat_lgl_b[1:2,1:2], mat_lgl_b, FALSE), matrix(c(1L,1L),nrow = 1))
})


test_that("fpos-0004.027", {
  expect_identical(fpos(mat_cpl_b[1:2,1:2], mat_cpl_b, FALSE), matrix(c(1L,1L),nrow = 1))
})


test_that("fpos-0004.028", {
  expect_identical(fpos(mat_cpl_a[1:2,1:2], mat_cpl_a, TRUE), matrix(c(1L,1L),nrow = 1))
})


test_that("fpos-0004.029", {
  expect_identical(fpos(mat_cpl_a[2:3,2:3], mat_cpl_a, TRUE), matrix(c(2L,2L),nrow = 1))
})


test_that("fpos-0004.030", {
  expect_identical(fpos(matrix(13), matrix(c(1:30),nrow = 5), TRUE), matrix(c(3L, 3L),nrow = 1))
})


test_that("fpos-0004.031", {
  expect_identical(fpos(matrix(c(19,24), nrow = 1), matrix(c(1:30),nrow = 5), TRUE), matrix(c(4L, 4L),nrow = 1))
})


test_that("fpos-0004.032", {
  expect_error(fpos(mat_int_a[19:20,9:10], mat_lgl_a), regexp = "Haystack type (logical) and needle type (integer) are different. Please make sure that they have the same type.", fixed = TRUE)
})


test_that("fpos-0004.033", {
  expect_identical(fpos(mat_int_a[19:20,9:10], mat_dbl_a), matrix(c(19L,9L),nrow = 1))
})


test_that("fpos-0004.034", {
  expect_identical(fpos(mat_dbl_a[19:20,9:10], mat_int_a), matrix(c(19L,9L),nrow = 1))
})


test_that("fpos-0004.035", {
  expect_identical(fpos(small_matrix, big_matrix), matrix(c(2L,7L,2L,7L,2L,7L,2L,7L,1L,1L,2L,2L,3L,3L,4L,4L),nrow = 8))
})


test_that("fpos-0004.036", {
  expect_identical(fpos(small_matrix, big_matrix, all = FALSE), matrix(c(2L,1L),nrow = 1))
})


test_that("fpos-0004.037", {
  expect_identical(fpos(small_matrix, big_matrix, overlap = FALSE), matrix(c(2L,7L,2L,7L,1L,1L,3L,3L),nrow = 4))
})


test_that("fpos-0004.038", {
  expect_identical(fpos(small_matrix_d, big_matrix_d), matrix(c(2L,7L,2L,7L,2L,7L,2L,7L,1L,1L,2L,2L,3L,3L,4L,4L),nrow = 8))
})


test_that("fpos-0004.039", {
  expect_identical(fpos(small_matrix_d, big_matrix_d, all = FALSE), matrix(c(2L,1L),nrow = 1))
})


test_that("fpos-0004.040", {
  expect_identical(fpos(small_matrix_d, big_matrix_d, overlap = FALSE), matrix(c(2L,7L,2L,7L,1L,1L,3L,3L),nrow = 4))
})


test_that("head-0004.041", {
  expect_identical(head(fpos(small_matrix_l, big_matrix_l),6), matrix(c(1L,2L,3L,4L,5L,6L,1L,1L,1L,1L,1L,1L),nrow = 6))
})


test_that("fpos-0004.042", {
  expect_identical(fpos(small_matrix_l, big_matrix_l, all = FALSE), matrix(c(1L,1L),nrow = 1))
})


test_that("fpos-0004.043", {
  expect_identical(fpos(small_matrix_l, big_matrix_l, overlap = FALSE), matrix(c(1L,3L,5L,7L,9L,1L,3L,5L,7L,9L,1L,1L,1L,1L,1L,3L,3L,3L,3L,3L),nrow = 10))
})


test_that("fpos-0004.044", {
  expect_identical(fpos(small_matrix_cp, big_matrix_cp), matrix(c(2L,7L,2L,7L,2L,7L,2L,7L,1L,1L,2L,2L,3L,3L,4L,4L),nrow = 8))
})


test_that("fpos-0004.045", {
  expect_identical(fpos(small_matrix_cp, big_matrix_cp, all = FALSE), matrix(c(2L,1L),nrow = 1))
})


test_that("fpos-0004.046", {
  expect_identical(fpos(small_matrix_cp, big_matrix_cp, overlap = FALSE), matrix(c(2L,7L,2L,7L,1L,1L,3L,3L),nrow = 4))
})


test_that("fpos-0004.047", {
  expect_identical(fpos(small_matrix_ch, big_matrix_ch), matrix(c(2L,7L,2L,7L,2L,7L,2L,7L,1L,1L,2L,2L,3L,3L,4L,4L),nrow = 8))
})


test_that("fpos-0004.048", {
  expect_identical(fpos(small_matrix_ch, big_matrix_ch, all = FALSE), matrix(c(2L,1L),nrow = 1))
})


test_that("fpos-0004.049", {
  expect_identical(fpos(small_matrix_ch, big_matrix_ch, overlap = FALSE), matrix(c(2L,7L,2L,7L,1L,1L,3L,3L),nrow = 4))
})


test_that("fpos-0004.050", {
  expect_error(fpos(small_matrix_ch, big_matrix_ch, overlap = c(TRUE,FALSE)), regexp = "Argument 'overlap' must be TRUE or FALSE and length 1.", fixed = TRUE)
})


test_that("fpos-0004.051", {
  expect_identical(fpos(TRUE, TRUE), 1L)
})


test_that("fpos-0004.052", {
  expect_identical(fpos(FALSE, TRUE), NULL)
})


test_that("fpos-0004.053", {
  expect_identical(fpos(NA, TRUE), NULL)
})


test_that("fpos-0004.054", {
  expect_identical(fpos(TRUE, matrix(c(FALSE, TRUE, TRUE, FALSE), nrow = 2)), matrix(c(2L,1L,1L,2L),nrow = 2))
})


test_that("fpos-0004.055", {
  expect_error(fpos(s1, TRUE), regexp = "S4 class objects are not supported.", fixed = TRUE)
})


test_that("fpos-0004.056", {
  expect_error(fpos(TRUE, s2), regexp = "S4 class objects are not supported.", fixed = TRUE)
})


test_that("fpos-0004.057", {
  expect_error(fpos(TRUE, data.frame(c(1:10))), regexp = "Please note that data.frame(s) are not supported.", fixed = TRUE)
})


test_that("fpos-0004.058", {
  expect_error(fpos(TRUE, list(1:10)), regexp = "Type list for 'haystack' is not supported.", fixed = TRUE)
})


test_that("fpos-0004.059", {
  expect_error(fpos(iris3, TRUE), regexp = "Arrays are not supported for argument 'needle'.", fixed = TRUE)
})


test_that("fpos-0004.060", {
  expect_error(fpos(TRUE, iris3), regexp = "Arrays are not supported for argument 'haystack'.", fixed = TRUE)
})


test_that("fpos-0004.061", {
  expect_error(fpos(TRUE, TRUE,c(TRUE,FALSE)), regexp = "Argument 'all' must be TRUE or FALSE and length 1.", fixed = TRUE)
})


test_that("fpos-0004.062", {
  expect_error(fpos(TRUE, TRUE,TRUE,c(TRUE,FALSE)), regexp = "Argument 'overlap' must be TRUE or FALSE and length 1.", fixed = TRUE)
})


test_that("fpos-0004.063", {
  expect_error(fpos(list(1:10),TRUE), regexp = "Type list for 'needle' is not supported.", fixed = TRUE)
})


test_that("fpos-0004.064", {
  expect_error(fpos(c(TRUE,FALSE),TRUE), regexp = "The 'needle' vector length is greater than the 'haystack' vector length.", fixed = TRUE)
})


test_that("fpos-0004.065", {
  expect_error(fpos(1L,TRUE), regexp = "Haystack type (logical) and needle type (integer) are different. Please make sure that they have the same type.", fixed = TRUE)
})


test_that("fpos-0004.066", {
  expect_identical(fpos(mat_dbl_b[2,1:2],mat_dbl_b[2,]), 1L)
})


test_that("fpos-0004.067", {
  expect_identical(fpos(mat_chr_b[2,1:2],mat_chr_b[2,]), 1L)
})


test_that("fpos-0004.068", {
  expect_identical(fpos(mat_cpl_b[2,1:2],mat_cpl_b[2,]), 1L)
})


test_that("fpos-0004.069", {
  expect_identical(fpos(mat_int_b[2,1:2],mat_int_b[2,]), 1L)
})


test_that("fpos-0004.070", {
  expect_identical(fpos(mat_lgl_b[2,1:2],mat_lgl_b[2,]), 1L)
})


test_that("fpos-0004.071", {
  expect_identical(fpos(mat_dbl_b,mat_dbl_a), NULL)
})


test_that("fpos-0004.072", {
  expect_identical(fpos(mat_dbl_b[2,1:2],as.integer(mat_dbl_b[2,])), 1L)
})


test_that("fpos-0004.073", {
  expect_identical(fpos(as.integer(mat_dbl_b[2,1:2]),mat_dbl_b[2,]), 1L)
})


test_that("fpos-0004.074", {
  expect_identical(fpos(c(1L,2L), c(1L,2L,5L,1L,2L), all = TRUE, overlap = TRUE), c(1L,4L))
})


test_that("fpos-0004.075", {
  expect_identical(fpos(c(1L,1L), c(1L,1L,1L,1L,1L), all = TRUE, overlap = TRUE), c(1L,2L,3L,4L))
})


test_that("fpos-0004.076", {
  expect_identical(fpos(c(1L,1L), c(1L,1L,1L,1L,1L), all = FALSE, overlap = TRUE), c(1L))
})


test_that("fpos-0004.077", {
  expect_identical(fpos(c(1L,1L), c(1L,1L,1L,1L,1L), all = TRUE, overlap = FALSE), c(1L,3L))
})


test_that("fpos-0004.078", {
  expect_identical(fpos(c(1L,NA_integer_), c(1L,NA_integer_,1L,NA_integer_,1L), all = TRUE, overlap = FALSE), c(1L,3L))
})


test_that("fpos-0004.079", {
  expect_identical(fpos(c(1,2), c(1L,2L,5L,1L,2L), all = TRUE, overlap = TRUE), c(1L,4L))
})


test_that("fpos-0004.080", {
  expect_identical(fpos(c(1,1), c(1L,1L,1L,1L,1L), all = TRUE, overlap = TRUE), c(1L,2L,3L,4L))
})


test_that("fpos-0004.081", {
  expect_identical(fpos(c(1,1), c(1L,1L,1L,1L,1L), all = FALSE, overlap = TRUE), c(1L))
})


test_that("fpos-0004.082", {
  expect_identical(fpos(c(1,1), c(1L,1L,1L,1L,1L), all = TRUE, overlap = FALSE), c(1L,3L))
})


test_that("fpos-0004.083", {
  expect_identical(fpos(c(1,NA_real_), c(1L,NA_integer_,1L,NA_integer_,1L), all = TRUE, overlap = FALSE), c(1L,3L))
})


test_that("fpos-0004.084", {
  expect_identical(fpos(as.complex(c(1L,2L)), as.complex(c(1L,2L,5L,1L,2L)), all = TRUE, overlap = TRUE), c(1L,4L))
})


test_that("fpos-0004.085", {
  expect_identical(fpos(as.complex(c(1L,1L)), as.complex(c(1L,1L,1L,1L,1L)), all = TRUE, overlap = TRUE), c(1L,2L,3L,4L))
})


test_that("fpos-0004.086", {
  expect_identical(fpos(as.complex(c(1L,1L)), as.complex(c(1L,1L,1L,1L,1L)), all = FALSE, overlap = TRUE), c(1L))
})


test_that("fpos-0004.087", {
  expect_identical(fpos(as.complex(c(1L,1L)), as.complex(c(1L,1L,1L,1L,1L)), all = TRUE, overlap = FALSE), c(1L,3L))
})


test_that("fpos-0004.088", {
  expect_identical(fpos(as.complex(c(1L,NA_integer_)), as.complex(c(1L,NA_integer_,1L,NA_integer_,1L)), all = TRUE, overlap = FALSE), c(1L,3L))
})


test_that("fpos-0004.089", {
  expect_identical(fpos(as.character(c(1L,2L)), as.character(c(1L,2L,5L,1L,2L)), all = TRUE, overlap = TRUE), c(1L,4L))
})


test_that("fpos-0004.090", {
  expect_identical(fpos(as.character(c(1L,1L)), as.character(c(1L,1L,1L,1L,1L)), all = TRUE, overlap = TRUE), c(1L,2L,3L,4L))
})


test_that("fpos-0004.091", {
  expect_identical(fpos(as.character(c(1L,1L)), as.character(c(1L,1L,1L,1L,1L)), all = FALSE, overlap = TRUE), c(1L))
})


test_that("fpos-0004.092", {
  expect_identical(fpos(as.character(c(1L,1L)), as.character(c(1L,1L,1L,1L,1L)), all = TRUE, overlap = FALSE), c(1L,3L))
})


test_that("fpos-0004.093", {
  expect_identical(fpos(as.character(c(1L,NA_integer_)), as.character(c(1L,NA_integer_,1L,NA_integer_,1L)), all = TRUE, overlap = FALSE), c(1L,3L))
})


test_that("fpos-0004.094", {
  expect_identical(fpos(as.logical(c(1L,2L)), as.logical(c(1L,2L,5L,1L,2L)), all = TRUE, overlap = TRUE), c(1L,2L,3L,4L))
})


test_that("fpos-0004.095", {
  expect_identical(fpos(as.logical(c(1L,1L)), as.logical(c(1L,1L,1L,1L,1L)), all = TRUE, overlap = TRUE), c(1L,2L,3L,4L))
})


test_that("fpos-0004.096", {
  expect_identical(fpos(as.logical(c(1L,1L)), as.logical(c(1L,1L,1L,1L,1L)), all = FALSE, overlap = TRUE), c(1L))
})


test_that("fpos-0004.097", {
  expect_identical(fpos(as.logical(c(1L,1L)), as.logical(c(1L,1L,1L,1L,1L)), all = TRUE, overlap = FALSE), c(1L,3L))
})


test_that("fpos-0004.098", {
  expect_identical(fpos(as.logical(c(1L,NA_integer_)), as.logical(c(1L,NA_integer_,1L,NA_integer_,1L)), all = TRUE, overlap = FALSE), c(1L,3L))
})

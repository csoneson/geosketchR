set.seed(1)
xd <- matrix(stats::rnorm(5000), nrow = 1000)
xs <- methods::as(xd, "dgCMatrix")
n <- 10

test_that("geosketch works", {
    ## ------------------------------------------------------------------------- ##
    ## Mis-specified arguments
    ## ------------------------------------------------------------------------- ##
    ## geosketch
    expect_error(geosketch(mat = "error"), "of class 'matrix'")
    expect_error(geosketch(mat = xd), '"N" is missing')
    expect_error(geosketch(N = n), '"mat" is missing')
    expect_error(geosketch(mat = xd, N = "error"), "class 'numeric'")
    expect_error(geosketch(mat = xd, N = n, replace = "error"), "class 'logical'")
    expect_error(geosketch(mat = xd, N = n, k = "error"), "class 'numeric'")
    expect_error(geosketch(mat = xd, N = n, alpha = "error"), "class 'numeric'")
    expect_error(geosketch(mat = xd, N = n, seed = "error"), "class 'numeric'")
    expect_error(geosketch(mat = xd, N = n, max_iter = "error"), "class 'numeric'")
    expect_error(geosketch(mat = xd, N = n, one_indexed = "error"), "class 'logical'")
    expect_error(geosketch(mat = xd, N = n, verbose = "error"), "class 'logical'")

    ## ------------------------------------------------------------------------- ##
    ## Checks, geosketch
    ## ------------------------------------------------------------------------- ##
    id1 <- geosketch(mat = xd, N = n, seed = 42)
    id2 <- geosketch(mat = xd, N = n, seed = 44)
    id3 <- geosketch(mat = xd, N = 2 * n, seed = 42)
    id4 <- geosketch(mat = xd, N = 2 * n, seed = 44)
    id5 <- geosketch(mat = xd, N = n, k = n, seed = 99)
    id6 <- geosketch(mat = xd, N = n, k = n, seed = 99)
    is1 <- geosketch(mat = xs, N = n, seed = 42)
    is2 <- geosketch(mat = xs, N = n, seed = 44)

    expect_type(id1, "double")
    expect_type(id2, "double")
    expect_type(id3, "double")
    expect_type(id4, "double")
    expect_type(id5, "double")
    expect_type(id6, "double")
    expect_type(is1, "double")
    expect_type(is2, "double")

    expect_length(id1, n)
    expect_length(id2, n)
    expect_length(id3, 2 * n)
    expect_length(id4, 2 * n)
    expect_length(id5, n)
    expect_length(id6, n)
    expect_length(is1, n)
    expect_length(is2, n)

    expect_equal(id1, c(49, 248, 287, 433, 487, 615, 651, 702, 704, 843))
    expect_equal(id2, c(37, 46, 49, 253, 615, 651, 829, 838, 841, 843))
    expect_equal(id3, c(39, 54, 56, 79, 147, 171, 196, 312, 355, 474, 492,
                        578, 620, 629, 651, 704, 739, 841, 920, 979))
    expect_equal(id4, c(16, 39, 147, 178, 248, 295, 312, 364, 416, 433, 440,
                        492, 615, 702, 729, 770, 828, 920, 979, 982))
    expect_equal(id5, c(49, 54, 107, 253, 274, 290, 331, 615, 651, 849))

    expect_false(all(id1 == id2))

    expect_identical(id1, is1)
    expect_identical(id2, is2)
    expect_identical(id5, id6)
})

test_that("getGeosketchNames works", {
    ## ------------------------------------------------------------------------- ##
    ## Checks, getGeosketchNames
    ## ------------------------------------------------------------------------- ##
    nms <- getGeosketchNames()

    expect_type(nms, "character")
    expect_true("gs" %in% nms)
})

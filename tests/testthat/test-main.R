box::use(
  shiny[testServer],
  testthat[expect_s3_class, test_that],
)
box::use(
  app / main[server],
)

test_that("main server initialises shared_data reactiveValues", {
  testServer(server, {
    expect_s3_class(shared_data, "reactivevalues")
  })
})

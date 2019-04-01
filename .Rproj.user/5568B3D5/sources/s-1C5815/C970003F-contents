context("Zelda's basic functionality")

library(testthat)
library(zelda)

data(degs)
degs_alzheimer <- degs$degs_alzheimer
degs_periodontitis <- degs$degs_periodontitis
p <- zelda(receptors = degs_alzheimer, ligands = degs_periodontitis, return_ggplot = TRUE, plot_it = FALSE)
links <- zelda(receptors = degs_alzheimer, ligands = degs_periodontitis, plot_it = FALSE, return_links = TRUE)

test_that('Output is a ggplot object as expected when return_ggplot = TRUE',
          {
          expect_output(str(p), "List of 9")
          }
          )

test_that('Output is a dataframe with 2 columnswhen return_links = TRUE',
          {
            expect_equal(ncol(links), 2)
          }
)


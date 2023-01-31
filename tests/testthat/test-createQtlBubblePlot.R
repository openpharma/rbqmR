test_that("createQtlBubblePlot fails elegantly with bad inputs", {
  expect_error(createQtlBubblePlot(NULL), "data cannot be NULL")
  
  d <- tibble::tibble(X=NA, Y=NA, Size=NA, Group=NA)
  expect_error(
    d %>% createQtlBubblePlot(x=BadX), 
    "BadX is not a column in .+"
  )
  expect_error(
    d %>% createQtlBubblePlot(x=X, y=BadY), 
    "BadY is not a column in .+"
  )
  expect_error(
    d %>% createQtlBubblePlot(x=X, y=Y, size=BadSize), 
    "BadSize is not a column in .+"
  )
  expect_error(
    d %>% createQtlBubblePlot(x=X, y=Y, size=Size, group=BadGroup), 
    "BadGroup is not a column in .+"
  )
  # No stat error when summarise is FALSE
  expect_error(
    d %>% createQtlBubblePlot(x=X, y=Y, size=Size, group=Group, stat="notAFunction"), 
    NA
  )
  expect_error(
    d %>% createQtlBubblePlot(x=X, y=Y, size=Size, group=Group, summarise=TRUE, stat="notAFunction"), 
    "stat must be a function when summarise is TRUE"
  )
  expect_error(
    d %>% createQtlBubblePlot(x=X, y=Y, size=Size, group=Group, summarise=TRUE, summarize=FALSE), 
    "summarise and summarize are inconsistent"
  )
  expect_error(
    d %>% createQtlBubblePlot(x=X, y=Y, size=Size, group=Group, limits=NA), 
    "limits is not a list"
  )
  expect_error(
    d %>% createQtlBubblePlot(x=X, y=Y, size=Size, group=Group, limits="notAList"), 
    "limits is not a list"
  )
  futile.logger::flog.threshold(futile.logger::FATAL)
  expect_error(
    d %>% createQtlBubblePlot(x=X, y=Y, size=Size, group=Group, limits=list(list(), "notAList")), 
    "Element 2 of limits is not a list"
  )
})

test_that("createQtlBubblePlot produces a ggplot object", {
  set.seed(1)
  
  referenceLines <- list(
                      list(label = "QTL (12%)", colour = "red", type = "dashed", y = 0.12, x = 1.4, vjust = -1),
                      list(label = "Sec Lim (8%)", colour = "goldenrod", type = "dotted", y = 0.08, x = 1.25, vjust = 1.25)
                    )
  d <- tibble::tibble() %>% 
         tidyr::expand(Snapshot=1:2, Site=1:10, Group=1:3) %>% 
         dplyr::mutate(N=floor(stats::runif(nrow(.), 2, 25)), P=runif(nrow(.))) %>% 
         dplyr::rowwise() %>% 
         dplyr::mutate(Rate=stats::rbinom(n=1, size=N, prob=P)/N) %>% 
         dplyr::ungroup()
  p <- d %>% createQtlBubblePlot(x=Snapshot, y=Rate, size=N, group=Group, limits=referenceLines)
  expect_s3_class(p, "gg")
})

test_that("creatQtlBubblePlot output contains correct elements", {
  d <- berrySummary %>% 
         tibble::add_column(Snapshot="End of Study") %>% 
         tibble::add_column(Region=c(rep("EU", 4), rep("US", 5)))
  # No limit lines
  p <- d %>% 
         createQtlBubblePlot(
           x = Snapshot,
           y = ObservedResponse,
           group = Region,
           size = Subjects
         )
  # data
  expect_equal(p$data, d)
  # layers
  # scales
  # mapping
  # theme
  expect_equal(p$theme, list())
  # coordinates
  # facet
  expect_equal(class(p$facet), c("FacetNull", "Facet", "ggproto", "gg"))
  # plot_env
  # labels
  expect_equal(p$labels, list("x"="Snapshot", "y"= "ObservedResponse", "colour"="Region", "size"="Subjects", "group"="Snapshot"))
  # guides
  expect_equal(names(p$guides), "size")
  expect_equal(p$guides$size, "none")
  
  # With limit lines
  referenceLines <- list(
    list(label = "QTL (12%)",  colour = "red", type = "dashed", y = 0.12,  x = 1.4, vjust = -1),
    list(label = "Sec Lim (8%)",  colour = "goldenrod",  type = "dotted", y = 0.08, x = 1.25, vjust = 1.25)
  )
  p <- d %>% 
    createQtlBubblePlot(
      x = Snapshot,
      y = ObservedResponse,
      group = Region,
      size = Subjects,
      limits=referenceLines
    )
  # data
  expect_equal(p$data, d)
  # layers
  # scales
  # mapping
  # theme
  expect_equal(p$theme, list())
  # coordinates
  # facet
  expect_equal(class(p$facet), c("FacetNull", "Facet", "ggproto", "gg"))
  # plot_env
  # labels
  expect_equal(p$labels, list("x"="Snapshot", "y"= "ObservedResponse", "colour"="Region", "size"="Subjects", "group"="Snapshot", "yintercept"="yintercept"))
  # guides
  expect_equal(names(p$guides), "size")
  expect_equal(p$guides$size, "none")
})
  

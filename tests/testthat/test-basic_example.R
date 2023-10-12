set.seed(9999)
envSet <- create_env_nsets(cellDims = c(100, 100),
                           sets     = c(4, 4, 3, 1),
                           model    = "Sph",
                           psill    = 1.5,
                           dep1     = 1,
                           rangeFun = function() exp(runif(1, 1, 6)),
                           propSamp = 0.25)

### generate a virtual species from the variables
sp <- create_sp(envStack = envSet,
                spFun    = "x[1] * x[5] * x[9]",
                spModel  = "Sph",
                spPsill  = 1,
                spRange  = 500,
                propSamp = 0.5,
                prev     = 0.1)

### an initial 'sample' of the species (assuming perfect detection)
sampPts <- data.frame(sampleRandom(sp$presence, 50, na.rm = TRUE, xy = TRUE))

### a formula to fit to random forest (additive for all vars + quadratics)
form <- paste0("presence ~ ", paste(names(envSet), collapse = " + "), "+ I(",
                paste(names(envSet), collapse = " ^ 2) + I("), " ^ 2)")

### run the initial model
spMod <- sdmModelling(samples = sampPts,
                      envStack = envSet,
                      modFormula = form,
                      ntrees = 500,
                      plot = FALSE)

### a random set of 500 points to profile
unsampPts <- data.frame(x = runif(500, 1, 100), y = runif(500, 1, 100))
unsampPts <- unsampPts[!paste(unsampPts$x, unsampPts$y) %in%
                         paste(sampPts$x, sampPts$y), ]

profile <- sdmProfiling(unsampledCoords = unsampPts,
                        sampledCoords   = sampPts,
                        origSDM         = spMod,
                        envStack        = envSet,
                        sdmFun          = "sdmModelling",
                        sdmFunArgs      = list(samples    = NULL,
                                               envStack   = envSet,
                                               modFormula = form,
                                               ntrees     = 50),
                        parallel = FALSE)


test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

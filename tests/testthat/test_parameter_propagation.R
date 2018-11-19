context("Propagation of parameters to IsoCorrection function via GUI")

test_that("check propagation of default input values", {
  
  correctionValues <- list()
  
  #Files and directories
  
  correctionValues["MeasurementFile"] <- ""
  correctionValues["ElementFile"] <- ""
  correctionValues["MoleculeFile"] <- ""
  correctionValues["DirOut"] <- ""
  correctionValues["FileOut"] <- "result"
  correctionValues["FileOutFormat"] <- "csv"
  
  #Logical values
  
  correctionValues["UltraHighRes"] <- FALSE
  correctionValues["CorrectTracerImpurity"] <- FALSE
  correctionValues["CorrectTracerElementCore"] <- TRUE
  correctionValues["CalculateMeanEnrichment"] <- TRUE
  correctionValues["CorrectAlsoMonoisotopic"] <- FALSE
  
  #Numeric values
  
  correctionValues["CalculationThreshold"] <- 10^(-8)
  correctionValues["CalculationThreshold_UHR"] <- 8
  
  expect_equal(initGUI(testmode="default_input"), correctionValues)
  expect_equal(initGUI(testmode="default_input_advanced_options"), correctionValues)
  
})

test_that("check propagation of modified input values", {
  
  correctionValues <- list()
  
  #Files and directories
  
  correctionValues["MeasurementFile"] <- "MeasurementFileTest.csv"
  correctionValues["ElementFile"] <- "ElementFileTest.csv"
  correctionValues["MoleculeFile"] <- "MoleculeFileTest.csv"
  correctionValues["DirOut"] <- "DirOutTest"
  correctionValues["FileOut"] <- "resultTest"
  correctionValues["FileOutFormat"] <- "xls"
  
  #Logical values
  
  correctionValues["UltraHighRes"] <- TRUE
  correctionValues["CorrectTracerImpurity"] <- TRUE
  correctionValues["CorrectTracerElementCore"] <- FALSE
  correctionValues["CalculateMeanEnrichment"] <- FALSE
  correctionValues["CorrectAlsoMonoisotopic"] <- TRUE
  
  #Numeric values
  
  correctionValues["CalculationThreshold"] <- 10^(-6)
  correctionValues["CalculationThreshold_UHR"] <- 6
  
  expect_equal(initGUI(testmode="modified_input"), correctionValues)
  expect_equal(initGUI(testmode="modified_input_advanced_options"), correctionValues)
  
})
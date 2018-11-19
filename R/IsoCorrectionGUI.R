#' Graphical User Interface for IsoCorrectoR
#'
#' @import IsoCorrectoR
#' @import tcltk
#' @importFrom tcltk2 tk2tip
#' @importFrom tcltk2 tk2frame
#' @importFrom utils browseURL
#' @importFrom utils packageVersion
#'
#' @export
#'
#' @examples
#'
#'  IsoCorrectionGUI()
#'
#' @return Calls internal function to display IsoCorrectoR's Graphical User Interface
IsoCorrectionGUI <- function() {
  initGUI()
}

# Main function of IsoCorrectoR GUI

initGUI <- function(advancedOptions = FALSE, filenameStartvalue = "", elementfileStartvalue = "", moleculefileStartvalue = "", DirOutStartvalue = "", FileOutStartvalue = "result",
                    UHRStartvalue = FALSE, CorrectTracerImpurityStartvalue = FALSE, CorrectTracerElementCoreStartvalue = TRUE, CalculateMeanEnrichmentStartvalue = TRUE, Calculation_thresholdStartvalue = 1e-08,
                    CalcThreshUHRStartvalue = 8, CalculateMonoisotopicProbStartvalue = FALSE, FileFormatStartvalue = "csv", testmode=NA) {

  # Set this base environment variable as a flag to determine if execution should continue or be aborted in the case of directly starting from a batch file

  baseEnvRef <- baseenv()
  baseEnvRef$continueIsoCorrection <- TRUE

  # Define internal functions (must be defined within initGUI() to work)

  # Function to destroy top-level window

  windestroy <- function() {
    tcltk::tkdestroy(win)
    baseEnvRef <- baseenv()
    baseEnvRef$continueIsoCorrection <- FALSE
  }

  # Functions for file/directory browsing

  # Function to select measurementfile via filechooser/browser

  filebrowser.filename <- function() {
    filename.global <<- tcltk::tclVar(tk_choose.files())
    tcltk::tkconfigure(entry.filename, textvariable = filename.global)
  }

  # Function to select elementfile via filechooser/browser

  filebrowser.elementfile <- function() {
    elementfile.global <<- tcltk::tclVar(tk_choose.files())
    tcltk::tkconfigure(entry.elementfile, textvariable = elementfile.global)
  }

  # Function to select moleculefile via filechooser/browser

  filebrowser.moleculefile <- function() {
    moleculefile.global <<- tcltk::tclVar(tk_choose.files())
    tcltk::tkconfigure(entry.moleculefile, textvariable = moleculefile.global)
  }

  # Function to select Output Directory via directorychooser/browser

  dirbrowser.DirOut <- function() {
    DirOut.global <<- tcltk::tclVar(tk_choose.dir())
    tcltk::tkconfigure(entry.DirOut, textvariable = DirOut.global)
  }

  # Specify styles and fonts

  .Tcl("ttk::style theme use default")

  fontset <- list()

  fontset$fontTextLabel <- tcltk::tkfont.create(family = "font", size = 12)
  fontset$fontHelptext <- tcltk::tkfont.create(family = "font", size = 13)
  fontset$fontErrortext <- tcltk::tkfont.create(family = "font", size = 12)
  fontset$fontSmall <- tcltk::tkfont.create(family = "font", size = 4)

  tcltk::tcl("ttk::style", "configure", "But.TButton", "-background", "cornflower blue", "-borderwidth", "5", font = "helvetica 12", padding = 0, focusthickness = 5)
  tcltk::tcl("ttk::style", "configure", "Select.TButton", "-background", "cornflower blue", "-borderwidth", "2", font = "helvetica 10", padding = 0, focusthickness = 5)
  tcltk::tcl("ttk::style", "configure", "Check.TCheckbutton",
    relief = "raised", shiftrelief = 0, padding = 0, background = "cornflower blue", indicatorcolor = "white",
    indicatorrelief = "sunken", indicatordiameter = 20, indicatormargin = 0, borderwidth = 4, focusthickness = 0, text = "test", justify = "left", foreground = "red"
  )
  tcltk::tcl("ttk::style", "configure", "Test.TFrame", "-background", "white", "-borderwidth", "10", relief = "raised")
  tcltk::tcl("ttk::style", "configure", "Gray.TButton", "-background", "lightgray", "-borderwidth", "5", font = "helvetica 12", padding = 0, focusthickness = 5)
  tcltk::tcl("ttk::style", "configure", "Rad.TRadiobutton", "-background", "SystemMenu",
    fg = "cornflower blue", shiftrelief = 0, padding = 0, bordercolor = "cornflower blue",
    indicatorcolor = "white", indicatorrelief = "sunken", indicatordiameter = 20, indicatormargin = 0, borderwidth = 3, focusthickness = 5, font = "helvetica 12"
  )

  # Create window
  win <- tcltk::tktoplevel()

  # Rewrite window manager function to call windestroy()

  tcltk::tcl("wm", "protocol", win, "WM_DELETE_WINDOW", windestroy)

  tcltk::tkwm.title(win, "IsoCorrectoR")
  tcltk::tkfocus(win)

  # Add help menu
  topMenu <- tcltk::tkmenu(win)
  tcltk::tkconfigure(win, menu = topMenu, bd = 0)
  helpMenu <- tcltk::tkmenu(topMenu, tearoff = FALSE, activebackground = "cornflower blue", bg = "white", bd = 0)
  tcltk::tkadd(helpMenu, "command", label = "How to use IsoCorrectoR", command = function() openhelp())
  tcltk::tkadd(helpMenu, "command", label = "About", command = function() showAbout())
  tcltk::tkadd(topMenu, "cascade", label = "Help", menu = helpMenu)

  # Enter measurement file
  filename.global <- tcltk::tclVar(filenameStartvalue)
  filename.tip <- tcltk::tklabel(win, text = "Measurement File:", font = fontset$fontTextLabel, justify = "left")
  entry.filename <- tcltk::tkentry(win, width = "60", textvariable = filename.global, bg = "white")
  browse.button.filename <- tcltk::ttkbutton(win, text = "Select", command = filebrowser.filename)
  tcltk::tkgrid(filename.tip, row = 0, column = 0, sticky = "w")
  tcltk::tkgrid(entry.filename, row = 0, column = 2)
  tcltk::tkgrid(browse.button.filename, row = 0, column = 3)
  tcltk2::tk2tip(filename.tip, "Please enter or choose file with measured data")

  # Enter Molecule File
  moleculefile.global <- tcltk::tclVar(moleculefileStartvalue)
  moleculefile.tip <- tcltk::tklabel(win, text = "Molecule File:", font = fontset$fontTextLabel, justify = "left")
  entry.moleculefile <- tcltk::tkentry(win, width = "60", textvariable = moleculefile.global, bg = "white")
  browse.button.moleculefile <- tcltk::ttkbutton(win, text = "Select", command = filebrowser.moleculefile)
  tcltk::tkgrid(moleculefile.tip, row = 1, column = 0, sticky = "w")
  tcltk::tkgrid(entry.moleculefile, row = 1, column = 2)
  tcltk::tkgrid(browse.button.moleculefile, row = 1, column = 3)
  tcltk2::tk2tip(moleculefile.tip, "Please enter or choose file with molecule data")

  # Enter Element File
  elementfile.global <- tcltk::tclVar(elementfileStartvalue)
  elementfile.tip <- tcltk::tklabel(win, text = "Element File:", font = fontset$fontTextLabel, justify = "left")
  entry.elementfile <- tcltk::tkentry(win, width = "60", textvariable = elementfile.global, bg = "white")
  browse.button.elementfile <- tcltk::ttkbutton(win, text = "Select", command = filebrowser.elementfile)
  tcltk::tkgrid(elementfile.tip, row = 2, column = 0, sticky = "w")
  tcltk::tkgrid(entry.elementfile, row = 2, column = 2)
  tcltk::tkgrid(browse.button.elementfile, row = 2, column = 3)
  tcltk2::tk2tip(elementfile.tip, "Please enter or choose file with element data")

  # Enter DirOut

  DirOut.global <- tcltk::tclVar(DirOutStartvalue)
  DirOut.tip <- tcltk::tklabel(win, text = "Output Directory:", font = fontset$fontTextLabel, justify = "left")
  entry.DirOut <- tcltk::tkentry(win, width = "60", textvariable = DirOut.global, bg = "white")
  browse.button.DirOut <- tcltk::ttkbutton(win, text = "Select", command = dirbrowser.DirOut)
  tcltk::tkgrid(DirOut.tip, row = 3, column = 0, sticky = "w")
  tcltk::tkgrid(entry.DirOut, row = 3, column = 2)
  tcltk::tkgrid(browse.button.DirOut, row = 3, column = 3)
  tcltk2::tk2tip(DirOut.tip, "Please enter or choose directory path to write results to")

  # Name Output file
  if (FileOutStartvalue == "") {
    FileOutStartvalue <- "result"
  }
  FileOut.global <- tcltk::tclVar(FileOutStartvalue)
  FileOut.tip <- tcltk::tklabel(win, text = "Name Outputfile:", font = fontset$fontTextLabel, justify = "left")
  entry.FileOut <- tcltk::tkentry(win, width = "60", textvariable = FileOut.global, bg = "white")
  tcltk::tkgrid(FileOut.tip, row = 4, column = 0, sticky = "w")
  tcltk::tkgrid(entry.FileOut, row = 4, column = 2)
  tcltk2::tk2tip(FileOut.tip, "Please enter name for your output file")

  # Choose output file format via radiobuttons
  if (FileFormatStartvalue == "") {
    FileFormatStartvalue <- "csv"
  }
  FileFormatValue <- tcltk::tclVar(FileFormatStartvalue)
  FileFormat.tip <- tcltk::tklabel(win, text = "Format Outputfile:", font = fontset$fontTextLabel, justify = "left")
  frame <- tcltk2::tk2frame(win, borderwidth = 0)
  tcltk::tkgrid(frame, row = 5, column = 2)
  win$env$rb1 <- tcltk::ttkradiobutton(frame, text = ".csv  ")
  win$env$rb2 <- tcltk::ttkradiobutton(frame, text = ".xls")
  tcltk::tkconfigure(win$env$rb1, variable = FileFormatValue, value = "csv")
  tcltk::tkconfigure(win$env$rb2, variable = FileFormatValue, value = "xls")
  tcltk::tkgrid(FileFormat.tip, row = 5, column = 0, sticky = "w")
  tcltk::tkgrid(win$env$rb1, win$env$rb2)

  UltraHighResvalue <- tcltk::tclVar(as.character(as.integer(UHRStartvalue)))
  CorrectTracerImpurityvalue <- tcltk::tclVar(as.character(as.integer(CorrectTracerImpurityStartvalue)))
  CorrectTracerElementCorevalue <- tcltk::tclVar(as.character(as.integer(CorrectTracerElementCoreStartvalue)))
  CalculateMeanEnrichmentvalue <- tcltk::tclVar(as.character(as.integer(CalculateMeanEnrichmentStartvalue)))
  Calculation_thresholdvalue <- tcltk::tclVar(as.character(Calculation_thresholdStartvalue))
  CalcThreshUHRvalue <- tcltk::tclVar(as.character(CalcThreshUHRStartvalue))
  CalcMonoisotopicProbabilitiesvalue <- tcltk::tclVar(as.character(as.integer(CalculateMonoisotopicProbStartvalue)))

  # Check Button CorrectTracerImpurity
  check.CorrectTracerImpurity <- tcltk::ttkcheckbutton(win)
  CorrectTracerImpurity.tip <- tcltk::tklabel(win, text = "Correct Tracer Impurity:", font = fontset$fontTextLabel, justify = "left")
  tcltk::tkgrid(CorrectTracerImpurity.tip, sticky = "w")
  tcltk::tkgrid(check.CorrectTracerImpurity, row = 6, column = 1)
  tcltk::tkconfigure(check.CorrectTracerImpurity, variable = CorrectTracerImpurityvalue)
  tcltk2::tk2tip(CorrectTracerImpurity.tip, "Correct for isotopic impurity of the tracer substrate?")

  # Check Button CorrectTracerElementCore
  check.CorrectTracerElementCore <- tcltk::ttkcheckbutton(win)
  CorrectTracerElementCore.tip <- tcltk::tklabel(win, text = "Corr. Tracer Element Core:", font = fontset$fontTextLabel, justify = "left")
  tcltk::tkgrid(CorrectTracerElementCore.tip, sticky = "w")
  tcltk::tkgrid(check.CorrectTracerElementCore, row = 7, column = 1)
  tcltk::tkconfigure(check.CorrectTracerElementCore, variable = CorrectTracerElementCorevalue)
  tcltk2::tk2tip(CorrectTracerElementCore.tip, "Take into account the natural isotope abundance of the tracer element atoms in the core molecule when correcting?")

  # Check Button CalculateMeanEnrichment
  check.CalculateMeanEnrichment <- tcltk::ttkcheckbutton(win)
  CalculateMeanEnrichment.tip <- tcltk::tklabel(win, text = "Calculate Mean Enrichment:   ", font = fontset$fontTextLabel, justify = "left")
  tcltk::tkgrid(CalculateMeanEnrichment.tip, sticky = "w")
  tcltk::tkgrid(check.CalculateMeanEnrichment, row = 8, column = 1)
  tcltk::tkconfigure(check.CalculateMeanEnrichment, variable = CalculateMeanEnrichmentvalue)
  tcltk2::tk2tip(CalculateMeanEnrichment.tip, "Calculate mean enrichment?")

  # Check Button UltraHighRes
  check.UltraHighRes <- tcltk::ttkcheckbutton(win)
  UltraHighRes.tip <- tcltk::tklabel(win, text = "High Resolution Mode:", font = fontset$fontTextLabel, justify = "left")
  tcltk::tkgrid(UltraHighRes.tip, sticky = "w")
  tcltk::tkgrid(check.UltraHighRes, row = 9, column = 1)
  tcltk::tkconfigure(check.UltraHighRes, variable = UltraHighResvalue)
  tcltk2::tk2tip(UltraHighRes.tip, "Perform correction of high resolution MS data (high resolution data allows handling of multiple different tracer elements in the same measurement)?")

  # Advanced options

  if (advancedOptions == FALSE) {
    AdvancedOptionsText <- "Advanced Options"
  } else {
    AdvancedOptionsText <- "Hide Advanced Options"

    # Check Monoisotopic Probability
    check.CalcMonoisotopicProbabilities <- tcltk::ttkcheckbutton(win)
    CalcMonoisotopicProbabilities.tip <- tcltk::tklabel(win, text = "Monoisotopic Results:", font = fontset$fontTextLabel, justify = "left")
    tcltk::tkgrid(CalcMonoisotopicProbabilities.tip, sticky = "w")
    tcltk::tkgrid(check.CalcMonoisotopicProbabilities, row = 10, column = 1)
    tcltk::tkconfigure(check.CalcMonoisotopicProbabilities, variable = CalcMonoisotopicProbabilitiesvalue)
    tcltk2::tk2tip(CalcMonoisotopicProbabilities.tip, "Get monoisotopic correction results in addition to normal results, see Help for more information.")

    # Enter Calculation threshold
    Calculation_threshold.tip <- tcltk::tklabel(win, text = "Calculation Threshold:", font = fontset$fontTextLabel, justify = "left")
    entry.Calculation_threshold <- tcltk::tkentry(win, width = "20", textvariable = Calculation_thresholdvalue, bg = "white")
    tcltk::tkgrid(Calculation_threshold.tip, sticky = "w")
    tcltk::tkgrid(entry.Calculation_threshold, row = 11, column = 2)
    tcltk2::tk2tip(Calculation_threshold.tip, "Usually, this parameter should not be changed. See Help for further information")

    # Enter Calculation threshold regarding UHR
    CalcThreshUHR.tip <- tcltk::tklabel(win, text = "Calc. Thresh. UHR:", font = fontset$fontTextLabel, justify = "left")
    entry.CalcThreshUHR <- tcltk::tkentry(win, width = "20", textvariable = CalcThreshUHRvalue, bg = "white")
    tcltk::tkgrid(CalcThreshUHR.tip, sticky = "w")
    tcltk::tkgrid(entry.CalcThreshUHR, row = 12, column = 2)
    tcltk2::tk2tip(CalcThreshUHR.tip, "Usually, this parameter should not be changed. See Help for further information")
  }

  AO.button <- tcltk::ttkbutton(win,
    text = AdvancedOptionsText,
    command = function() AdvancedOptions(
        win = win, advancedOptions = !advancedOptions, filenameStartvalue = tclvalue(filename.global), elementfileStartvalue = tclvalue(elementfile.global), moleculefileStartvalue = tclvalue(moleculefile.global),
        DirOutStartvalue = tclvalue(DirOut.global), FileOutStartvalue = tclvalue(FileOut.global), UHRStartvalue = as.logical(as.integer(tclvalue(UltraHighResvalue))),
        CorrectTracerImpurityStartvalue = as.logical(as.integer(tclvalue(CorrectTracerImpurityvalue))), CorrectTracerElementCoreStartvalue = as.logical(as.integer(tclvalue(CorrectTracerElementCorevalue))),
        CalculateMeanEnrichmentStartvalue = as.logical(as.integer(tclvalue(CalculateMeanEnrichmentvalue))), Calculation_thresholdStartvalue = as.double(tclvalue(Calculation_thresholdvalue)),
        CalcThreshUHRStartvalue = as.double(tclvalue(CalcThreshUHRvalue)), CalculateMonoisotopicProbStartvalue = as.logical(as.integer(tclvalue(CalcMonoisotopicProbabilitiesvalue))),
        FileFormatStartvalue = tclvalue(FileFormatValue)
      )
  )
  tcltk::tkgrid(AO.button, row = 9, column = 2)

  blankspace <- tcltk::tklabel(win, text = " ", font = fontset$fontSmall)
  tcltk::tkgrid(blankspace)

  Correct.button <- tcltk::ttkbutton(win,
    text = "Start Correction",
    command = function() correction(
        win = win, filename = filename.global, elementfile = elementfile.global,
        moleculefile = moleculefile.global, FileOut = FileOut.global,
        UltraHighResvalue = UltraHighResvalue,
        CorrectTracerImpurityvalue = CorrectTracerImpurityvalue,
        CorrectTracerElementCorevalue = CorrectTracerElementCorevalue,
        CalculateMeanEnrichmentvalue = CalculateMeanEnrichmentvalue,
        Calculation_thresholdvalue = Calculation_thresholdvalue,
        CalcThreshUHRvalue = CalcThreshUHRvalue,
        CalcMonoisotopicProbabilitiesvalue = CalcMonoisotopicProbabilitiesvalue,
        DirOut = DirOut.global,
        FileFormatValue = FileFormatValue,
        fontset = fontset
      )
  )

  tcltk::tkgrid(Correct.button, column = 2)

  tcltk::tkfocus(win)
  
  if(!is.na(testmode)) {
    
    #Imitate functions that assign parameters upon user input in the GUI
    
    if(testmode == "modified_input" || (testmode == "modified_input_advanced_options" && !advancedOptions)) {
      
      filename.global <- tcltk::tclVar("MeasurementFileTest.csv")
      
      moleculefile.global <- tcltk::tclVar("MoleculeFileTest.csv")
      
      elementfile.global <- tcltk::tclVar("ElementFileTest.csv")
      
      DirOut.global <- tcltk::tclVar("DirOutTest")
      
      FileOut.global <- tcltk::tclVar("resultTest")
      
      FileFormatValue <- tcltk::tclVar("xls")
      
      UltraHighResvalue <- tcltk::tclVar(as.character(as.integer(TRUE)))
      
      CorrectTracerImpurityvalue <- tcltk::tclVar(as.character(as.integer(TRUE)))
      
      CorrectTracerElementCorevalue <- tcltk::tclVar(as.character(as.integer(FALSE)))
      
      CalculateMeanEnrichmentvalue <- tcltk::tclVar(as.character(as.integer(FALSE)))
      
      CalcMonoisotopicProbabilitiesvalue <- tcltk::tclVar(as.character(as.integer(TRUE)))
      
      Calculation_thresholdvalue <- tcltk::tclVar(as.character(10^(-6)))
      
      CalcThreshUHRvalue <- tcltk::tclVar(as.character(6))
      
    }
    
    if((testmode == "default_input_advanced_options" || testmode == "modified_input_advanced_options") && !advancedOptions) {
      
      correctionValues <- AdvancedOptions(
        win = win, advancedOptions = !advancedOptions, filenameStartvalue = tclvalue(filename.global), elementfileStartvalue = tclvalue(elementfile.global), moleculefileStartvalue = tclvalue(moleculefile.global),
        DirOutStartvalue = tclvalue(DirOut.global), FileOutStartvalue = tclvalue(FileOut.global), UHRStartvalue = as.logical(as.integer(tclvalue(UltraHighResvalue))),
        CorrectTracerImpurityStartvalue = as.logical(as.integer(tclvalue(CorrectTracerImpurityvalue))), CorrectTracerElementCoreStartvalue = as.logical(as.integer(tclvalue(CorrectTracerElementCorevalue))),
        CalculateMeanEnrichmentStartvalue = as.logical(as.integer(tclvalue(CalculateMeanEnrichmentvalue))), Calculation_thresholdStartvalue = as.double(tclvalue(Calculation_thresholdvalue)),
        CalcThreshUHRStartvalue = as.double(tclvalue(CalcThreshUHRvalue)), CalculateMonoisotopicProbStartvalue = as.logical(as.integer(tclvalue(CalcMonoisotopicProbabilitiesvalue))),
        FileFormatStartvalue = tclvalue(FileFormatValue), testmode = testmode
      )
      
    } else {
    
      correctionValues <- correction(
        win = win, filename = filename.global, elementfile = elementfile.global,
        moleculefile = moleculefile.global, FileOut = FileOut.global,
        UltraHighResvalue = UltraHighResvalue,
        CorrectTracerImpurityvalue = CorrectTracerImpurityvalue,
        CorrectTracerElementCorevalue = CorrectTracerElementCorevalue,
        CalculateMeanEnrichmentvalue = CalculateMeanEnrichmentvalue,
        Calculation_thresholdvalue = Calculation_thresholdvalue,
        CalcThreshUHRvalue = CalcThreshUHRvalue,
        CalcMonoisotopicProbabilitiesvalue = CalcMonoisotopicProbabilitiesvalue,
        DirOut = DirOut.global,
        FileFormatValue = FileFormatValue,
        fontset = fontset,
        testmode = testmode
      )
      
    }
    
    return(correctionValues)
    
  }
}

# SUBFUNCTIONS OF initGUI()

# Function of the 'Correct' button

correction <- function(win, filename, elementfile, moleculefile, FileOut,
                       UltraHighResvalue, CorrectTracerImpurityvalue,
                       CorrectTracerElementCorevalue, CalculateMeanEnrichmentvalue,
                       Calculation_thresholdvalue, CalcThreshUHRvalue,
                       CalcMonoisotopicProbabilitiesvalue, DirOut,
                       FileFormatValue, fontset, testmode=NA) {

  # Function for creating the error-pop-up-windows
  errorwindow <- function(text, fontset) {

    # Function of the 'OK' button in error-pop-up-windows
    destroy <- function() {
      tcltk::tkdestroy(errwin)
    }

    errwin <- tcltk::tktoplevel()
    tcltk::tkwm.title(errwin, "Error")
    errtext <- tcltk::tklabel(errwin, text = text, font = fontset$fontErrortext)
    tcltk::tkgrid(errtext)
    OK.button <- tcltk::ttkbutton(errwin, text = "OK", command = destroy)
    tcltk::tkgrid(OK.button)
  }

  # Changing from tclvalues to R classes
  filenameVal <- tcltk::tclvalue(filename)
  elementfileVal <- tcltk::tclvalue(elementfile)
  moleculefileVal <- tcltk::tclvalue(moleculefile)
  FileOutVal <- tcltk::tclvalue(FileOut)
  UltraHighResVal <- as.logical(as.integer(tcltk::tclvalue(UltraHighResvalue)))
  CorrectTracerImpurityVal <- as.logical(as.integer(tcltk::tclvalue(CorrectTracerImpurityvalue)))
  CorrectTracerElementCoreVal <- as.logical(as.integer(tcltk::tclvalue(CorrectTracerElementCorevalue)))
  CalculateMeanEnrichmentVal <- as.logical(as.integer(tcltk::tclvalue(CalculateMeanEnrichmentvalue)))
  Calculation_thresholdVal <- as.double(tcltk::tclvalue(Calculation_thresholdvalue))
  CalcThreshUHRVal <- as.integer(tcltk::tclvalue(CalcThreshUHRvalue))
  CalcMonoisotopicProbabilitiesVal <- as.logical(as.integer(tcltk::tclvalue(CalcMonoisotopicProbabilitiesvalue)))
  DirOutVal <- tcltk::tclvalue(DirOut)
  FileFormatVal <- tcltk::tclvalue(FileFormatValue)
  
  if(is.na(testmode)) {

    # Intercept input errors
  
    errorConditionNames <- c(
      "filenameValGiven", "elementfileValGiven", "moleculefileValGiven",
      "DirOutValGiven", "FileOutValGiven", "filenameValFileExists",
      "elementfileValFileExists", "moleculefileValFileExists",
      "DirOutValDirExists", "Calculation_thresholdVal", "CalcThreshUHRVal"
    )
  
    errorConditions <- vector(mode = "logical", length = length(errorConditionNames))
  
    names(errorConditions) <- errorConditionNames
  
    errorConditions["filenameValGiven"] <- filenameVal != ""
    errorConditions["elementfileValGiven"] <- elementfileVal != ""
    errorConditions["moleculefileValGiven"] <- moleculefileVal != ""
    errorConditions["DirOutValGiven"] <- DirOutVal != ""
    errorConditions["FileOutValGiven"] <- FileOutVal != ""
  
    errorConditions["filenameValFileExists"] <- file.exists(filenameVal)
    errorConditions["elementfileValFileExists"] <- file.exists(elementfileVal)
    errorConditions["moleculefileValFileExists"] <- file.exists(moleculefileVal)
    errorConditions["DirOutValDirExists"] <- dir.exists(DirOutVal)
  
    errorConditions["Calculation_thresholdVal"] <- !is.na(Calculation_thresholdVal)
    errorConditions["CalcThreshUHRVal"] <- !is.na(CalcThreshUHRVal)
  
    errorMessages <- vector(mode = "character", length = length(errorConditions))
  
    names(errorMessages) <- names(errorConditions)
  
    errorMessages["filenameValGiven"] <- "Please choose measurement file"
    errorMessages["elementfileValGiven"] <- "Please choose element file"
    errorMessages["moleculefileValGiven"] <- "Please choose molecule file"
    errorMessages["DirOutValGiven"] <- "Please choose output directory"
    errorMessages["FileOutValGiven"] <- "Please enter name of the output file"
  
    errorMessages["filenameValFileExists"] <- "Measurement File does not exist"
    errorMessages["elementfileValFileExists"] <- "Element File does not exist"
    errorMessages["moleculefileValFileExists"] <- "Molecule File does not exist"
    errorMessages["DirOutValDirExists"] <- "Output directory does not exist"
  
    errorMessages["Calculation_thresholdVal"] <- "Please enter numeric value for Calculation threshold"
    errorMessages["CalcThreshUHRVal"] <- "Please enter numeric value for Calculation threshold UHR"
  
    errorFlag <- FALSE
  
    for (errorCondition in names(errorConditions)) {
      if (errorFlag == FALSE && errorConditions[errorCondition] == FALSE) {
        errorwindow(text = errorMessages[errorCondition], fontset = fontset)
        errorFlag <- TRUE
      }
    }
  
    if (errorFlag == FALSE) {
      print("Correction")
      tcltk::tkdestroy(win)
      results <- IsoCorrectoR::IsoCorrection(
        MeasurementFile = filenameVal, ElementFile = elementfileVal, FileOutFormat = FileFormatVal, MoleculeFile = moleculefileVal,
        UltraHighRes = UltraHighResVal, CorrectTracerImpurity = CorrectTracerImpurityVal, CorrectTracerElementCore = CorrectTracerElementCoreVal,
        CalculateMeanEnrichment = CalculateMeanEnrichmentVal, CorrectAlsoMonoisotopic = CalcMonoisotopicProbabilitiesVal, DirOut = DirOutVal,
        CalculationThreshold = Calculation_thresholdVal, CalculationThreshold_UHR = CalcThreshUHRVal, FileOut = FileOutVal, ReturnResultsObject = FALSE
      )
  
      finish(results = results, fontset = fontset)
    }
    
  } else {
    
    correctionValues <- list()
    
    #Files and directories
    
    correctionValues["MeasurementFile"] <- filenameVal
    correctionValues["ElementFile"] <- elementfileVal
    correctionValues["MoleculeFile"] <- moleculefileVal
    correctionValues["DirOut"] <- DirOutVal
    correctionValues["FileOut"] <- FileOutVal
    correctionValues["FileOutFormat"] <- FileFormatVal
    
    #Logical values
    
    correctionValues["UltraHighRes"] <- UltraHighResVal
    correctionValues["CorrectTracerImpurity"] <- CorrectTracerImpurityVal
    correctionValues["CorrectTracerElementCore"] <- CorrectTracerElementCoreVal
    correctionValues["CalculateMeanEnrichment"] <- CalculateMeanEnrichmentVal
    correctionValues["CorrectAlsoMonoisotopic"] <- CalcMonoisotopicProbabilitiesVal
    
    #Numeric values
    
    correctionValues["CalculationThreshold"] <- Calculation_thresholdVal
    correctionValues["CalculationThreshold_UHR"] <- CalcThreshUHRVal
    
    tcltk::tkdestroy(win)
    
    return(correctionValues)
    
  }
}

# Function creating the window after hitting the Correct-Button

finish <- function(results, fontset) {
  continuewindow <- tcltk::tktoplevel()
  tcltk::tkwm.title(continuewindow, "Continue")
  tcltk::tkfocus(continuewindow)

  contdestroy <- function() {
    tcltk::tkdestroy(continuewindow)
    baseEnvRef <- baseenv()
    baseEnvRef$continueIsoCorrection <- FALSE
  }

  # Rewrite window manager function to call contdestroy()

  tcltk::tcl("wm", "protocol", continuewindow, "WM_DELETE_WINDOW", contdestroy)

  # Function to start a completely new correction
  newcorrection <- function() {
    tcltk::tkdestroy(continuewindow)
    initGUI()
  }

  if (results$success == "TRUE") {
    
    Headtext <- tcltk::tklabel(continuewindow, text = "Correction successful!", font = fontset$fontErrortext)
    tcltk::tkgrid(Headtext)
    newcorrection.button <- tcltk::ttkbutton(continuewindow, text = "Start new correction", command = newcorrection)
    tcltk::tkgrid(newcorrection.button)
    
  } else if (results$success == "WARNINGS") {
    
    Headtext <- tcltk::tklabel(continuewindow,
                               text = "Correction successful with warnings.\nPlease see the logfile for more information",
                               font = fontset$fontErrortext)
    
    tcltk::tkgrid(Headtext)
    newcorrection.button <- tcltk::ttkbutton(continuewindow, text = "Start new correction", command = newcorrection)
    tcltk::tkgrid(newcorrection.button)
    
  } else if (results$success == "FALSE") {
    
    Headtext <- tcltk::tklabel(continuewindow,
      text = paste0("Correction was aborted because an error has occured. Error:\n\n", results$error, "\n"),
      font = fontset$fontErrortext)
    
    tcltk::tkgrid(Headtext)
    newcorrection.button <- tcltk::ttkbutton(continuewindow, text = "Start new correction", command = newcorrection)
    tcltk::tkgrid(newcorrection.button)
    
  }
  blankspace <- tcltk::tklabel(continuewindow, text = " ", font = fontset$fontSmall)
  tcltk::tkgrid(blankspace)
  OK.button <- tcltk::ttkbutton(continuewindow, text = "Close", command = contdestroy)
  tcltk::tkgrid(OK.button)
}

# Function of the 'Advanced Options' button

AdvancedOptions <- function(win, advancedOptions, filenameStartvalue, elementfileStartvalue, moleculefileStartvalue,
                            DirOutStartvalue, FileOutStartvalue, UHRStartvalue,
                            CorrectTracerImpurityStartvalue, CorrectTracerElementCoreStartvalue,
                            CalculateMeanEnrichmentStartvalue, Calculation_thresholdStartvalue,
                            CalcThreshUHRStartvalue, CalculateMonoisotopicProbStartvalue,
                            FileFormatStartvalue, testmode = NA) {
  
  tcltk::tkdestroy(win)
    
  correctionValues <- initGUI(
    advancedOptions = advancedOptions, filenameStartvalue = filenameStartvalue,
    elementfileStartvalue = elementfileStartvalue,
    moleculefileStartvalue = moleculefileStartvalue,
    DirOutStartvalue = DirOutStartvalue,
    FileOutStartvalue = FileOutStartvalue,
    UHRStartvalue = UHRStartvalue,
    CorrectTracerImpurityStartvalue = CorrectTracerImpurityStartvalue,
    CorrectTracerElementCoreStartvalue = CorrectTracerElementCoreStartvalue,
    CalculateMeanEnrichmentStartvalue = CalculateMeanEnrichmentStartvalue,
    Calculation_thresholdStartvalue = Calculation_thresholdStartvalue,
    CalcThreshUHRStartvalue = CalcThreshUHRStartvalue,
    CalculateMonoisotopicProbStartvalue = CalculateMonoisotopicProbStartvalue,
    FileFormatStartvalue = FileFormatStartvalue, testmode = testmode
  )
  
  if(!is.na(testmode)) {
    
    return(correctionValues)
    
  }
  
}

# Function to open general help html-file

openhelp <- function() {
  utils::browseURL(system.file("doc/IsoCorrectoR.html", package = "IsoCorrectoR"))
}

# Function to show information on IsoCorrectoR

showAbout <- function() {

  # Function of the 'OK' button in error-pop-up-windows
  destroy <- function() {
    tcltk::tkdestroy(aboutwin)
  }

  aboutwin <- tcltk::tktoplevel()
  tcltk::tkwm.title(aboutwin, "About")
  abouttext <- tcltk::tklabel(aboutwin, text = paste0(
    "IsoCorrectoRGUI version ", packageVersion("IsoCorrectoRGUI"), "\n", "Institute of Functional Genomics , University of Regensburg\n",
    "IsoCorrectoRGUI is licensed under ", packageDescription("IsoCorrectoRGUI", fields = "License"), ". It is free software and comes without any warranty.\n Please run 'citation(\"IsoCorrectoRGUI\")' for correct citation when using IsoCorrectoRGUI for your research."
  ))
  tcltk::tkgrid(abouttext)
  OK.button <- tcltk::ttkbutton(aboutwin, text = "OK", command = destroy)
  tcltk::tkgrid(OK.button)
}

# Function of the different categories in the help menu (currently not in use)

help <- function(text, fontset) {
  # Function of the 'OK' button in helpmenu window
  destroy <- function() {
    tcltk::tkdestroy(helpwin)
  }
  helpwin <- tcltk::tktoplevel(background = "white")
  tcltk::tkwm.title(helpwin, "Help")
  helptext <- tcltk::tklabel(helpwin, text = text, justify = "left", font = fontset$fontHelptext, background = "white")
  tcltk::tkgrid(helptext)
  OK.button <- tcltk::ttkbutton(helpwin, text = "OK", command = destroy)
  tcltk::tkgrid(OK.button)
}

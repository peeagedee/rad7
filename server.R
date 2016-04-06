
shinyServer(function(input, output) {
  
  #This function is repsonsible for loading in the selected file
  filedata <- reactive({
    
    validate( need(input$datafile, "" ))
    
    infile <- input$datafile
    if (is.null(infile)) {
      # User has not uploaded a file yet
      return(NULL)
    }

    # Start from line containing  "Data transfer..." until line ">Special SprAll" - 1 
    # file.path designed to be faste than paste0()
    lines = grep(">Special SprAll", readLines(infile$datapath)) -1  - grep("Data transfer...", readLines(infile$datapath))
    
    raw <- fread(infile$datapat, skip = "Data transfer...", nrows = lines )
    
    ## see peeagedee/literature/manuals/RAD7 Manual.pdf - page 73
    colnames <- c("RecordNumber","Year","Month","Day","Hour","Minute","TotalCounts","LiveTime_min","TotalCounts_A","TotalCounts_B","TotalCounts_C","TotalCounts_D","HighVoltageLevel_V",
                  "HighVoltageDutyCycle_percent","Temperature_C","RH_percent","LeakageCurrent","BatteryVoltage_V","PumpCurrent_mA","FlagsByte","RadonConc","RadonUncertainty","UnitsByte")
    ## update column names
    setnames(raw,colnames)

    ## convert integer to binary for Flagsbyte and UnityByte
    raw[,':=' ( FlagsByte = as.character(FlagsByte),
                UnitsByte = as.character(UnitsByte))]
    raw[, ':=' ( FlagsByte = paste( as.integer((intToBits( as.integer(FlagsByte))[8:1])), collapse = ''),
                 UnitsByte = paste( as.integer((intToBits( as.integer(UnitsByte))[8:1])), collapse = '')
    )]  
    
    ## create date time column; only POSIXct supported in ggplot2 
    ## as.character because xtable used in renderTable will discar POSIXct 
    raw[,date_time := as.POSIXct(strptime( paste(
      paste(raw[,Year], raw[,Month],raw[,Day], sep = "-"),
      paste(raw[,Hour], raw[,Minute], sep = ":"), sep = " "), format = "%y-%m-%d %H:%M"))]
    
  })
  
## Select x and y axis columns   
  output$xvars <- renderUI( {

    axis_vars_x <- colnames(filedata())

    selectInput('xaxis',label = "Select variable for x-axis", choices = axis_vars_x, selected = "date_time" )

  })
  
  output$xSlider <- renderUI({
    
    #validate( need(input$xaxis,
    
    xvariable <- input$xaxis

    dt <- filedata()[,.(Min = min(.SD[[1]]), Max = max(.SD[[1]])), .SDcols = xvariable]

    if (is.null(dt)) return(NULL)
    #if (!is.numeric(df[,xvariable]) ) return(NULL)

    sliderInput("xSlider", paste(xvariable,"Range"),
                min=dt[,Min],
                max=dt[,Max],
                value=c(dt[,Min],dt[,Max])
    )
  })

  output$yvars <- renderUI( {

    axis_vars_y <- colnames(filedata())

    selectInput('yaxis',label = "Select variable for y-axis", choices = axis_vars_y, selected = "RadonConc" )

  })
  
  subsetdata  <- reactive({

    is.POSIXct <- function(x) inherits(x, "POSIXct")
    
    dt <- filedata()
    
 
    if( is.POSIXct(input$xSlider[1])) {
    
    dt <- dt[ date_time %between% c(input$xSlider[1],input$xSlider[2] )]

    }
    
    dt

  })
  

  # This previews the data file
  # output$filetable <- renderTable(
  #   { filedata() }, booktabs = TRUE, include.rownames = FALSE
  #   )
  output$fileDtable <- DT::renderDataTable(
    { datatable( subsetdata(), filter = 'top', rownames = FALSE, options = list( pageLength = 6)) }
  )
  
## create plot
  output$plotTS <- renderPlot( {
    
    validate( need(input$datafile, "Please select file containing the data" ))
    
    plotdata <- subsetdata()
    
    if(!is.null(plotdata)) {

    s1 = input$fileDtable_rows_current # rows on the current page
    #s2 = input$fileDtable_rows_all      # rows on all pages (after being filtered)

    # basic plot
    p <- ggplot(data = plotdata
                , aes_string(x = input$xaxis, y = input$yaxis))
    p <- p + geom_point()

    # red dots for current page
    if (length(s1)) {
     p <-  p + geom_point( aes(colour = "red"), data = plotdata[RecordNumber %in% s1])
    }
    
    # cyan dot  when performing searching
    # if (length(s2) > 0 && length(s2) < max(dat[,RecordNumber])) {
    #   
    # p + geom_point( aes( colour = "yellow"), data = dat[RecordNumber %in% s2])
    # }
    
    p
    }
    
  })

})
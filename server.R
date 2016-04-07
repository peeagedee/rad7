
shinyServer(function(input, output) {
  
  #This function is responsible for loading in the selected file
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

    selectInput('yaxis',label = "Select variables for y-axis", choices = axis_vars_y,
                multiple = TRUE, 
                selected = "RadonConc" )

  })
  
  # 
  # output$WaterTemp <- renderUI({
  #   
  #   numericInput( "WaterTemp", label = expression( Avg ~ group("[",degree,"]")), 5)
  #   
  # })
  # 
  subsetdata  <- reactive({

    is.POSIXct <- function(x) inherits(x, "POSIXct")
    
    dt <- filedata()
    
 
    if( is.POSIXct(input$xSlider[1])) {
    
    dt <- dt[ date_time %between% c(input$xSlider[1],input$xSlider[2] )]

    }
    
    # setcolorder(dt, c("date_time",names(dt)[-length(names(dt))]))
    dt

  })
  
  avgdata <- reactive({
    
    # calculate mean only if 6 readings per hour are available
    mean_check <- function(x) { ifelse(length(x) == 6 , mean(x), NA_real_)}
    # calculate 2 sigma, 95 percent confidence intervall if 6 readings/hour are available
    ci <- function(x) { ifelse(length(x) == 6, 1.96 * ( sd(x) / sqrt(length(x))),NA_real_)}

    avgdt <- subsetdata()
    
    avgdt <- avgdt[ , .( date_time = as.POSIXct( unique( format( date_time, "%Y-%m-%d %H:30:00"))), # set time to mid points of averaged hour,
                                Readings = .N,
                                RadonConc = mean_check(RadonConc),
                                RadonUnc = ci(RadonConc)
                              ) , by = .(Year,Month,Day,Hour)]
  })
  
  output$fileAvgDtable <- DT::renderDataTable(
    
    { datatable( avgdata(), filter = 'top', rownames = FALSE, options = list( pageLength = 10)) }
    
  )
  
  

  # This previews the data file
  # output$filetable <- renderTable(
  #   { filedata() }, booktabs = TRUE, include.rownames = FALSE
  #   )
  output$fileDtable <- DT::renderDataTable(
    
    { datatable( subsetdata(), filter = 'top', rownames = FALSE, options = list( pageLength = 10)) }
  
    )
  
## create plot
  output$plotTS <- renderPlot( {
    
    validate( need(input$datafile, "Please select file containing the data" ))
    
    plotdata <- subsetdata()
    plotavg <- avgdata()
    
    plotavg <- plotavg[, ':=' (ymin = RadonConc - RadonUnc,
                  ymax = RadonConc + RadonUnc )]
    
    if(!is.null(plotdata)) {

    #s1 <- input$fileDtable_rows_current # rows on the current page
    #s2 = input$fileDtable_rows_all      # rows on all pages (after being filtered)
    
    cols <- c(input$xaxis, input$yaxis)

    subfunc <- function(cols,dt){ 
                    # subset data
                    sub <- dt[, cols, with = F]
                    # convert to long data format
                    sub <- melt(sub, id.vars = input$xaxis)
                    sub
    }
    
    sub <- subfunc(cols,plotdata)
     # sub_avg <- subfunc(c(cols,"RadonUnc","ymin","ymax"),avgdt)
    
    # create plot
    fp <- ggplot(sub, aes_string(x = input$xaxis, y = "value")) + 
              geom_point() + 
              labs(x = input$xaxis,  y = "") #+ scale_y_continuous(expand=c(0,0)) 
    fp <- fp + facet_wrap( ~variable, dir = "v", switch = "y", scales = "free_y" )
    
    if( isTRUE(input$avgcheck) & identical(input$yaxis,"RadonConc")){
      
      fp <- fp + geom_line(data = plotavg, inherit.aes = FALSE, size = 1, 
                           aes( x = date_time, y = RadonConc, colour = "Hourly Average"))
      fp <- fp + geom_ribbon(data = plotavg, inherit.aes = FALSE,
                             aes(x = date_time, ymin = ymin, ymax = ymax, fill = "2 Sigma Confidence Intervall"), alpha = 0.3 )
      fp <- fp  + scale_fill_manual("Legend", values = "grey12") +
                  scale_colour_manual("", values = c("Hourly Average" = "blue")) 
    
      fp

    }
    
    fp
    
    # # basic plot
    # p <- ggplot(data = plotdata
    #             , aes_string(x = input$xaxis, y = input$yaxis))
    # p <- p + geom_point()
    # 
    # # red dots for current page
    # if (length(s1)) {
    #  p <-  p + geom_point( aes(colour = "red"), data = plotdata[RecordNumber %in% s1])
    # }
    # 
    # # cyan dot  when performing searching
    # # if (length(s2) > 0 && length(s2) < max(dat[,RecordNumber])) {
    # #   
    # # p + geom_point( aes( colour = "yellow"), data = dat[RecordNumber %in% s2])
    # # }
    # 
    # p
    }
    
  })

})
#
shinyUI(fluidPage( 

  
      titlePanel("Durridge Rad 7  File Upload "),
      hr(),
      
      fluidRow( 
 
        column(3, 
               tabsetPanel(
                 tabPanel( "Input", 
                     #Selector for file upload
                     fileInput('datafile', 'Choose data file',
                               accept=c('.r7cdt')),
                     hr(),
                     uiOutput("yvars"),
                     checkboxInput("avgcheck", label = " Plot hourly average (only implemented for RadonConc atm)"),
                     hr(),
                     uiOutput("xvars"),
                     uiOutput("xSlider"),
                     hr(),
                     uiOutput("WaterTemp")
                     ),
                tabPanel("Notes",
                      h5("Input file needs to be a raw data file from DURRIGE Rad7!")
                         
                         ))),

        
        column(8,
               h3("Plot Window"),
               plotOutput("plotTS"))
      ),
      
      fluidRow(
        column(12,
               tabsetPanel(
                 tabPanel("Raw data",
               DT::dataTableOutput("fileDtable")),
                 tabPanel("Averages",
              DT::dataTableOutput("fileAvgDtable"))
               ))
      )
      ))

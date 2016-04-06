#
shinyUI(fluidPage( 

  
      titlePanel("Durridge Rad 7  File Upload "),
      hr(),
      
      fluidRow( 
        
        column(3, 
               #Selector for file upload
               fileInput('datafile', 'Choose data file',
                         accept=c('text/plain','.r7cdt')),
               hr(),
               uiOutput("yvars"),
               hr(),
               uiOutput("xvars"),
               uiOutput("xSlider")
               ),
        
        column(8,
               h3("Plot Window"),
               plotOutput("plotTS"))
      ),
      
      fluidRow(
        column(12,
               DT::dataTableOutput("fileDtable"))
      )
      ))

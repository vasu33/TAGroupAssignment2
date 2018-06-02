#TA Group Assignment -2 
# ui File
# Satya Venkata Rao Bikkina - 11910016
# Pavan Debbadi - 11910025
# Srikeerti V - 11910020

library("shiny")

#ui function
ui <- shinyUI(
  fluidPage(
    
    titlePanel("TAGroupAssignment2 - ShinyApp with UDPipe NLP workflow"),
    titlePanel(""),
    
    sidebarLayout( 
      
      sidebarPanel(  
        
        fileInput("file", "Upload any text file"),
        p("\n"),
        downloadButton("downloadData", "Download_annotated_DF"),
        p("\n"),
        p("\n"),
        p("Select UPOs for plotting co-occurrences"),
        checkboxGroupInput('show_vars', "Please note that this check boxes are for TAB-4", c("adjective (ADJ)" = "ADJ","noun(NOUN)" = "NOUN","proper noun (PROPN)" = "PROPN","adverb (ADV)","verb (VERB)" = "VERB"), selected = c("ADJ","NOUN","PROPN"))),
      
      fluidPage(mainPanel(
        
        tabsetPanel(type = "tabs",
                    
                    tabPanel("Overview",
                             h4('Developed By'),
                             P("Satya Venkata Rao Bikkina - 11910016, Pavan Debbadi - 11910025, Srikeerti V - 11910020"),
                             h4(p("Data input")),
                             p("This app takes only text files which are in redable format",align="justify"),
                             p("Please refer to the link below for sample text file."),
                             a(href="https://github.com/vasu33/sampledatasets/blob/master/sampletextdocument.txt"
                               ,"Sample text input file"),   
                             br(),
                             h4('How to use this App'),
                             p('To use this app, click on', 
                               span(strong("Upload any text file")),
                               'and uppload the text data file. You can also change the number of clusters to fit in k-means clustering'),
                             h4('The purpose of this APP'),
                             p('1-	First tab will describe this app.'),
                             p('2-	Second tab will display a table of annotated documents .'),
                             p('3-	Third tab will display two wordclouds (NOUNS & VERBS)'),
                             p('4-	Fourth tab will display a plot of top-30 co-occurrences ')),
                    tabPanel("Annotated Documents", 
                             dataTableOutput('clust_data')),
                    tabPanel("Word Clouds", 
                             fluidRow(12,
                                      column(12,plotOutput('plot1'))
                                      ,column(12,plotOutput('plot2')))),
                    tabPanel("top-30 co-occurrences",plotOutput('plot3'))
                    
                 ) #end of tabsetPanel
            )  # end if fluidPage
        )# end of main panel
     ) # end of sidebarLayout
  )  # end if fluidPage
) # end of UI


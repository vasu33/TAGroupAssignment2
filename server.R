#TA Group Assignment -2 
# Server File
# Satya Venkata Rao Bikkina - 11910016
# Pavan Debbadi - 11910025
# Srikeerti V - 11910020


library("shiny")

server <- shinyServer(function(input, output) {
  
  Dataset <- reactive({
    if (is.null(input$file)) { return(NULL) }						  #Check if the file is uploaded otherwise return NULL			
    else{
      require(stringr)
      inputtextdate = readLines(input$file$datapath) 				  #Readlines from the uploaded file
      inputtextdate  =  str_replace_all(inputtextdate, "<.*?>", "")   #Removes HTML tags
      ud_model_english <- udpipe_download_model(language = "english") #Load the English model
      library(udpipe)											      #Load the required libraries				
      library(shiny)
      
      english_model = udpipe_load_model("./english-ud-2.0-170801.udpipe")   #set English model
      x <- udpipe_annotate(english_model, x = inputtextdate)                #Get the annotated data
      Data <- as.data.frame(x)												#Generate the DataFrame							
      Data <- subset(Data, select = -c(sentence))							#remove column Sentence		
      return(Data)															#Return Data	
    }
  })
  
  output$clust_data <- renderDataTable(Dataset(),
                                       options = list(pageLength = 100))    #Render table witj 100 rows per page
  
  output$downloadData <- downloadHandler(									# Generate a function to give a download option
    filename = function() {
      if (is.null(input$file)) { return(NULL) }
      else{
        paste(input$dataset, ".csv", sep = "")
      }
    },
    content = function(file) {
      write.csv(Dataset(), file, col.names = T)								# Write CSV file		
    }
    
  )
  df1 <- reactive({															#reactive function to generate dataoutput		
    require(tidytext); require(tibble); require(tidyverse)					#Load the required libraries			
    library(udpipe)
    library(textrank)
    library(lattice)
    library(igraph)
    library(ggraph)
    library(ggplot2)
    library(wordcloud)
    library(stringr)
    if (is.null(input$file)) { return(NULL) }								#Check if the file is uploaded otherwise return NULL					
    else{                                                                   
      require(stringr)                                                      
      inputtextdate = readLines(input$file$datapath)                        #Readlines from the uploaded file
      inputtextdate  =  str_replace_all(inputtextdate, "<.*?>", "")         #Removes HTML tags
      library(udpipe)                                                       #Load the required libraries
      library(shiny)                                                        				
      
      english_model = udpipe_load_model("./english-ud-2.0-170801.udpipe")   #set English model
      x <- udpipe_annotate(english_model, x = inputtextdate)                #Get the annotated data
      x <- as.data.frame(x)                                                 #Generate the DataFrame	
      df1 = x[x$upos=='VERB',]                                              #remove column Sentence	
      new.vector <- subset.data.frame(df1,select=c(lemma))                  #Return Data	
      new.vector <- data_frame(text = new.vector$lemma)
      textdf  = new.vector
      tidy_df = textdf %>%   
        mutate(doc = row_number()) %>%
        unnest_tokens(word, text) %>% 
        anti_join(stop_words) %>%
        group_by(doc) %>%
        count(word, sort=TRUE)
      textdf1 = tidy_df %>% rename(value = n)		
      dtm = textdf1 %>% cast_sparse(doc, word, value); 
      colsum = apply(dtm, 2, sum)    
      col.order = order(colsum, decreasing=TRUE)
      row.order = order(rownames(dtm) %>% as.numeric())
      
      dtm1 = dtm[row.order, col.order]; 
      dtm = dtm1
      max.words1=150     # max no. of words to accommodate
      min.freq=5       # min.freq of words to consider
      plot.title="wordcloud"
      require(wordcloud)
      if (ncol(dtm) > 20000){   # if dtm is overly large, break into chunks and solve
        
        tst = round(ncol(dtm)/100)  # divide DTM's cols into 100 manageble parts
        a = rep(tst,99)
        b = cumsum(a);rm(a)
        b = c(0,b,ncol(dtm))
        
        ss.col = c(NULL)
        for (i in 1:(length(b)-1)) {
          tempdtm = dtm[,(b[i]+1):(b[i+1])]
          s = colSums(as.matrix(tempdtm))
          ss.col = c(ss.col,s)
          print(i)      } # i loop ends
        
        tsum = ss.col
        
      } else { tsum = apply(dtm, 2, sum) }
      
      tsum = tsum[order(tsum, decreasing = T)]       # terms in decreasing order of freq
      return(tsum)
    }
  })
  
  df2 <- reactive({
    if (is.null(input$file)) { return(NULL) }							#Check if the file is uploaded otherwise return NULL	
    else{                                                               
      require(stringr)                                                  
      inputtextdate = readLines(input$file$datapath)                    #Readlines from the uploaded file
      inputtextdate  =  str_replace_all(inputtextdate, "<.*?>", "")     #Removes HTML tags
      library(udpipe)                                                   #Load the required libraries
      library(shiny)
      
      english_model = udpipe_load_model("./english-ud-2.0-170801.udpipe")  #set English model
      x <- udpipe_annotate(english_model, x = inputtextdate)               #Get the annotated data
      x <- as.data.frame(x)                                                #Generate the DataFrame	
      df1 = x[x$upos=='NOUN',]                                             #NOUN specific data
      new.vector <- subset.data.frame(df1,select=c(lemma))                 #Subset of dataframe with Lemma
      new.vector <- data_frame(text = new.vector$lemma)
      textdf  = new.vector
      require(tidytext); require(tibble); require(tidyverse)			 #Load the required libraries
      library(udpipe)
      library(textrank)
      library(lattice)
      library(igraph)
      library(ggraph)
      library(ggplot2)
      library(wordcloud)
      library(stringr)
      tidy_df = textdf %>%   											#Removes stop words and gets a clean data
        mutate(doc = row_number()) %>%
        unnest_tokens(word, text) %>% 
        anti_join(stop_words) %>%
        group_by(doc) %>%
        count(word, sort=TRUE)
      textdf1 = tidy_df %>% rename(value = n)
      dtm = textdf1 %>% cast_sparse(doc, word, value); 					#Generate DTM
      colsum = apply(dtm, 2, sum)    
      col.order = order(colsum, decreasing=TRUE)						#with decreasing order
      row.order = order(rownames(dtm) %>% as.numeric())
      
      dtm1 = dtm[row.order, col.order]; 
      dtm = dtm1
      max.words1=150     # max no. of words to accommodate
      min.freq=5       # min.freq of words to consider
      plot.title="wordcloud"
      require(wordcloud)
      if (ncol(dtm) > 20000){   # if dtm is overly large, break into chunks and solve
        
        tst = round(ncol(dtm)/100)  # divide DTM's cols into 100 manageble parts
        a = rep(tst,99)
        b = cumsum(a);rm(a)
        b = c(0,b,ncol(dtm))
        
        ss.col = c(NULL)
        for (i in 1:(length(b)-1)) {
          tempdtm = dtm[,(b[i]+1):(b[i+1])]
          s = colSums(as.matrix(tempdtm))
          ss.col = c(ss.col,s)
          print(i)      } # i loop ends
        
        tsum = ss.col
        
      } else { tsum = apply(dtm, 2, sum) }
      
      tsum = tsum[order(tsum, decreasing = T)]       # terms in decreasing order of freq
      return(tsum)
    }
  })
  
  
  df3 <- reactive({
    if (is.null(input$file)) { return(NULL) }                            #Check if the file is uploaded otherwise return NULL	
    else{                                                                
      require(stringr)                                                   
      inputtextdate = readLines(input$file$datapath)                     #Readlines from the uploaded file
      inputtextdate  =  str_replace_all(inputtextdate, "<.*?>", "")      #Removes HTML tags
      #ud_model_english <- udpipe_download_model(language = "english")   #Load the required libraries
      library(udpipe)
      library(shiny)
      
      english_model = udpipe_load_model("./english-ud-2.0-170801.udpipe")    #loads english model
      x <- udpipe_annotate(english_model, x = inputtextdate) 				 #Annotate the Data
      x <- as.data.frame(x)													 #Generate a DataFrame
      textdata_colloc <- keywords_collocation(x = x,   
                                              term = "token", 
                                              group = c("doc_id", "paragraph_id", "sentence_id"),
                                              ngram_max = 4)  # 0.42 secs
      perm.vector <- as.vector(input$show_vars)
      textdata_cooc <- cooccurrence(   											#cooccurrence data		
        x = subset(x, upos %in% c(perm.vector)), 
        term = "lemma", 
        group = c("doc_id", "paragraph_id", "sentence_id")) 
      
      wordnetwork <- head(textdata_cooc[order(textdata_cooc$cooc,decreasing = TRUE),], 30) #Generate Wordnetwork
      wordnetwork <- igraph::graph_from_data_frame(wordnetwork)
      return(wordnetwork)														#return Wordnetwork
    }
  })
  
  
  ##  WordClod for VERBs
  
  output$plot1 <- renderPlot({
    tsum <- df1()
    wordcloud(names(tsum), tsum,     # words, their freqs 
              scale = c(3.5, 0.5),     # range of word sizes
              min.freq = 1,                     # min.freq of words to consider
              max.words = 100,       # max #words
              colors = brewer.pal(8, "Dark2"))
    title(main = "VERB WordClod") 
  })
  
  ## WordClod for NOUNs
  
  output$plot2 <- renderPlot({
    tsum <- df2()
    wordcloud(names(tsum), tsum,     # words, their freqs 
              scale = c(3.5, 0.5),     # range of word sizes
              min.freq = 1,                     # min.freq of words to consider
              max.words = 100,       # max #words
              colors = brewer.pal(8, "Dark2"))
    title(main = "NOUN WordClod") 
  })
  
  ## Cooccurrences plot for the choosen options
  
  output$plot3 <- renderPlot({
    wordnetwork <- df3()
    perm.vector <- as.vector(input$show_vars)
    ggraph(wordnetwork, layout = "fr") + 
      geom_edge_link(aes(width = cooc, edge_alpha = cooc), edge_colour = "orange") +  
      geom_node_text(aes(label = name), col = "darkgreen", size = 4) +
      theme_graph(base_family = "Arial Narrow") +  
      theme(legend.position = "none") +
      labs(title = "Cooccurrences plots")
  })
})
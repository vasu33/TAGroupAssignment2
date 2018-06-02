# TAGroupAssignment2

Building a Shiny App around the UDPipe NLP workflow.

Shiny app should have these features.
A – Should be able to read any text file using standard upload functionality.
B – English language model should be included within the app.
C – User should be able to select list of Universal part-of-speech tags (upos) using check box for plotting co-occurrences. List of upos required in app – 
adjective (ADJ)
noun(NOUN)
proper noun (PROPN)
adverb (ADV)
verb (VERB)
Default selection should be adjective (ADJ), noun (NOUN), proper noun (PROPN). Based on upos selection, filter annotated document and build co-occurrences plot
D – App should have at least 4 output tabs. Details of output tab is as follows – 
1-	First tab should describe your app. 
2-	Second tab should display a table of annotated documents (use udpipe_annotate function from udpipe) as data frame. Drop sentence column from the data frame. Use dataTableOutput function for displaying the output in shiny. Show only about a 100 rows of the annotated DF in the app and give user an option to download the full file as a .csv.
3-	Third tab should display two wordclouds, one for all the nouns in the corpus and another for all the verbs in the corpus
4-	Fourth tab should display a plot of top-30 co-occurrences at document level using a network plot as mentioned in point C.  

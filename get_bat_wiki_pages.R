##Wikipedia querying from Noam Ross https://gist.github.com/noamross/88a51bb880f18da88e4b259eefdefe87



library(tidyverse)
library(xml2)
library(rvest)
library(WikipediR)
library(urltools)

library(stringr)
library(tokenizers)
library(rtweet)



#####Noam's wikipedia querying ####
#First, we get some basic information from wikipedia

# Get all speceies-level page titles from the Wikipedia list of bats
bat_titles <- read_html("https://en.wikipedia.org/wiki/List_of_bats") %>%
  html_nodes(xpath="//ul/li[contains(., 'Genus')]/ul/li/a[starts-with(@href, '/wiki/')]") %>%
  xml_attr("href") %>%
  basename() %>%
  url_decode()



#####And now we're on Dave's far less elegant code #####


#Now we select a random bat-page, then some random sequential sentences from it, then if they pass some
#QC steps, we output it
tweetable <- FALSE 
while(tweetable==FALSE){  
  bat_info <- map_df(bat_titles[sample(seq(1,length(bat_titles)),1)], function(x) {
    return <- page_content(language="en", project="wikipedia", page_name=x)
    data_frame(title = return$parse$title,
               content = return$parse$text$`*`)
  })#Get the info from a random page
  
  # Extract just the text from the HTML
  bat_text <- bat_info %>%
    mutate(content = map_chr(content, ~html_text(read_html(.))))
  
  row <- sample(seq(1,nrow(bat_text)), 1) #Chose a random entry
  sp_name <- bat_text[row,1]$title #Get the bat species' name
  section_names <- str_extract_all(bat_text[row,2], ".+\\[edit\\]")[[1]] #Break the wall of text up into names and items
  section_names <- gsub('\\[edit\\]', '', section_names)
  sections <- str_split(bat_text[row,2], ".+\\[edit\\]")[[1]]
  sections <- sections[-1] # I don't want the first entry, its a bit dull and gets in the way of the next line
  names(sections) <- section_names
  sections <- gsub('\\\n', '', sections) #Clean out all the newline characters
  sections <- gsub('\\[.\\]', '', sections) #Clean out all the references, they'll make no sense out of context
  sections <- gsub('\\\\', '', sections)
  if(length(sections)<2){#Some pages only contain references (length == 1) or are completely blank (length ==0). These are garbage and should be skipped
    next()
  }
  if('References' %in% names(sections)){
    ref_pos <- which(names(sections)=='References') #Find where the references section is, get rid of it as it would make for a terrible tweet
    sections <- sections[-ref_pos]
  }
  if('Sources' %in% names(sections)){
    source_pos <- which(names(sections)=='Sources') #Ditto for sources
    sections <- sections[-source_pos]
  }
  if(length(sections)==0){
    next()
  }
  
  if(length(grep('easurements', names(sections)))>0){ #Measurement based tweets would be very boring, delete the section now
    measurement_position <- grep('easurements', names(sections))
    sections <- sections[-measurement_position]
  }
  if(length(sections)==0){
    next()
  }
  
  section_choice <- sample(seq(1,length(sections)),1)#Choose a random section to tweet from
  
  sentences <- tokenize_sentences(sections[section_choice])[[1]] #Convert the block of text into a vector wher each item is a sentence
  if(length(sentences)<2){ #Skip if the section is empty of tiny
    next()
  }
  n_sentences<- length(sentences) #Get how many sentences there are, required for the next two lines
  start_point <- sample(seq(1,n_sentences-1), 1) #The position of 
  end_point <- start_point+ sample(c(1,2),1) #Where will we end our text chunk
  
  outstring <- paste(sentences[c(start_point, end_point)], collapse = ' ') #Make a string out of this
  if(grepl("NA", outstring)){ #If we've selected empty space, skip to a new iteration of the while loop
    next()
  }
  outstring <- paste(sp_name, ': ', outstring, sep ='') #Start making the tweet
  #print(outstring)
  n_chars <- nchar(outstring)
  extra_length <- 29 #all urls take up 23 characters, then 6 more for ' #bats'. So we need 29 characters for these sections
  if(n_chars <240-extra_length & n_chars >120){ #If the string is a tweetable length and long enough to be interesting, tweet it
    
    page <- page_info("en", "wikipedia", page=sp_name , as_wikitext=TRUE)
    
    url <- page$query$pages[[1]]$fullurl
    
    outstring <- paste(outstring, url, '#bats', sep =' ')
    print(outstring)
    tweetable <- TRUE
  }
  #outstring <- paste(outstring, 'put_url_here', '#bats', sep =' ')
  #print(outstring)
  #cat('outstring is ', n_chars, ' long')
  #readline(prompt="Press [enter] to continue")
}

#####twitter things ####
#now we just tweet the output

# ##Vignette here http://rtweet.info/articles/auth.html

##twitter token was generated with the instructions above, but I found it easier to just source the  
twitter_token <- readRDS('/Users/davehemprichbennett/twitter_token.rds')

post_tweet(status = outstring, media = NULL, token = twitter_token,
           in_reply_to_status_id = NULL)


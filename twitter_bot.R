##Wikipedia querying from Noam Ross https://gist.github.com/noamross/88a51bb880f18da88e4b259eefdefe87


library(tidyverse)
library(xml2)
library(rvest)
library(WikipediR)
library(urltools)

library(stringr)
library(tokenizers)
library(rtweet)
library(here)

setwd(here())

wait_in_r <- F
wait_duration <- 211*60 #Number of seconds to wait
base_url <- "https://en.wikipedia.org/wiki/List_of_bats"
hashtag <- "#bats" #If you don't want a hashtag just assign this an empty character string

##twitter token was generated with the instructions here(http://rtweet.info/articles/auth.html), but I found it easier to just load the token rather than making it an environment variable
twitter_token <- readRDS(here('twitter_token.rds'))


#####Noam's wikipedia querying ####
#First, we get some basic information from wikipedia
A <- FALSE
while(A==FALSE){ #I schedule this by using an infinite loop with Sys.sleep used. This is a bad way of doing it, but works. To be ironed out later

  # Get all speceies-level page titles from the Wikipedia list of bats
  bat_titles <- read_html(base_url) %>%
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
    print(sp_name)
    section_names <- str_extract_all(bat_text[row,2], ".+\\[edit\\]")[[1]] #Break the wall of text up into names and items
    section_names <- gsub('\\[edit\\]', '', section_names)
    print(section_names)
    sections <- str_split(bat_text[row,2], ".+\\[edit\\]")[[1]]
    if(grepl('This article needs additional citations for verification', sections[1])){
      sections <- sections[-1] # I don't want the first entry, its a bit dull and gets in the way of the next line
      names(sections) <- section_names
    }else{
      names(sections[2:length(sections)]) <- section_names
    }
 
    sections <- gsub('\\\n', '', sections) #Clean out all the newline characters
    sections <- gsub('\\[.\\]', '', sections) #Clean out all the references, they'll make no sense out of context
    sections <- gsub('\\\\', '', sections)
    if(length(sections)<2){#Some pages only contain references (length == 1) or are completely blank (length ==0). These are garbage and should be skipped
      print('too short')
      next()
    }
    mismatched_sections <- F #The QC gets confused as the first section frequently doesn't have a length, so we need to control for this when removing bad sections
    if(length(sections) != length(section_names)){
      mismatched_sections <- T
    }
    if('References' %in% section_names){
      print('killing refs')
      ref_pos <- which(section_names=='References') #Find where the references section is, get rid of it as it would make for a terrible tweet
      cat('ref_pos is', ref_pos,'\n')
      cat('sections were ', sections,'\n')
      if(mismatched_sections==T){
        sections <- sections[-(ref_pos+1)]
      }else{sections <- sections[-ref_pos]}
      
      cat('sections are now', sections,'\n')
    }
    if('Sources' %in% section_names){
      
      source_pos <- which(section_names=='Sources') #Ditto for sources
      if(mismatched_sections==T){
        sections <- sections[-(source_pos+1)]
      }else{sections <- sections[-source_pos]}
      
    }
    if('Footnotes' %in% section_names){
      source_pos <- which(section_names=='Footnotes') #Ditto for Footnotes
      if(mismatched_sections==T){
        sections <- sections[-(source_pos+1)]
      }else{sections <- sections[-source_pos]}
    }
    if('Literature cited' %in% names(sections)){
      source_pos <- which(names(sections)=='Literature cited') #Ditto for Footnotes
      if(mismatched_sections==T){
        sections <- sections[-(source_pos+1)]
      }else{sections <- sections[-source_pos]}
    }
    if('Notes' %in% names(sections)){
      source_pos <- which(names(sections)=='Notes') #Ditto for Footnotes
      if(mismatched_sections==T){
        sections <- sections[-(source_pos+1)]
      }else{sections <- sections[-source_pos]}
    }
    if('External links' %in% names(sections)){
      source_pos <- which(names(sections)=='Literature cited') #Ditto for Footnotes
      if(mismatched_sections==T){
        sections <- sections[-(source_pos+1)]
      }else{sections <- sections[-source_pos]}
    }
    if(length(sections)==0){
      next()
    }
    if(is.na(sections)[1]){
      next()
    }

    if(length(grep('easurements', names(sections)))>0){ #Measurement based tweets would be very boring, delete the section now
      measurement_position <- grep('easurements', names(sections))
      sections <- sections[-measurement_position]
    }
    if(length(sections)==0){
      print('sections are 0 zero length')
      next()
    }

    section_choice <- sample(seq(1,length(sections)),1)#Choose a random section to tweet from

    sentences <- tokenize_sentences(sections[section_choice])[[1]] #Convert the block of text into a vector wher each item is a sentence
    if(length(sentences)<2){ #Skip if the section is empty or tiny
      cat('small sentence:', sentences, '\n')
      next()
    }
    n_sentences<- length(sentences) #Get how many sentences there are, required for the next two lines
    start_point <- sample(seq(1,n_sentences-1), 1) #The position of
    end_point <- start_point+ sample(c(1,2),1) #Where will we end our text chunk

    outstring <- paste(sentences[c(start_point, end_point)], collapse = ' ') #Make a string out of this

    if(grepl('You can help Wikipedia by expanding it', outstring)){
      next()#This stops tweeting out info about stubs
    }
    if(grepl('Contents', outstring)){
      next()#This stops tweeting out info about stubs
    }
    if(grepl('/(\b201[2-5]\b)/g)', outstring)){
      print('year in outstring')
      next()
    }
    ####Now its wikimedia time to get an image and its creator ####

    photo_details <- str_split(bat_info[1,2], pattern = '\" src')[[1]][1]
    photo_details <- str_split(photo_details, pattern = "<img alt=\\\"")[[1]][2] #This is the NAME of the image, to be queried on wikimedia
    photo_details <- gsub(' ', '_', photo_details)
    if(is.null(photo_details) | is.na(photo_details)| nchar(photo_details)==0){#Unable to get a decent photo (another good photo may be available in the page in a different position but the code isn't complex enough to search for it) 
      print('no photo available by current means')
      photo <- F
    }else{photo <- T}
    if(photo==T){
      photo_credit_url <- paste('https://commons.wikimedia.org/wiki/File:', photo_details, sep ='') #Becuase wikipedia doesn't include creditation for images from wikimedia on the wikipedia page in question, we instead have to query wikimedia for the creditation instead *eyeroll emoji*

      wikimedia_text <- html_text(read_html(photo_credit_url))
      author <- str_split(pattern='Author', wikimedia_text)[[1]][2]
      author <- str_split(pattern='\n', author)[[1]][2]
      if(is.na(author)){ #If we were unable to get any info for the author, skip it
        next()
      }
      if(nchar(author)==0){ #If we were unable to get any info for the author, skip it
        next()
      }
      #Now to begin getting the bat image
      bat_wiki <- read_html(photo_credit_url)
      bat_media <- html_nodes(bat_wiki, ".internal")

      bat_media_inf <- html_attrs(bat_media)[[1]]
      photo_url <- bat_media_inf[1]

      if(length(photo_url)!=1){ #If theres no url available for the image, or our regex gets confused and gives us too many potential images, skip
        next()
      }


    }
    ####Now we put all the text together to make the tweet string####

    outstring <- gsub("\\s*\\[[^\\)]+\\]","",outstring) #Kill any references that have made it through, as nobody wants [11] in their tweet
    outstring <- gsub(".*\\] ","",outstring) #Also occasionally your tweet will begin with half a reference e.g. '"16] Nothing is known about the diet...' This sorts that
    outstring <- gsub(".*\\]","",outstring) #Also occasionally your tweet will begin with half a reference e.g. '"16]Nothing is known about the diet...' This sorts that
    outstring <- gsub("\\[","",outstring) #Or sometimes ends like ' Leaves from Balanites species and several insects may also be eaten.['
    if(grepl("NA", outstring)){ #If we've selected empty space, skip to a new iteration of the while loop
      next()
    }
    outstring <- paste(sp_name, ': ', outstring, sep ='') #Start making the tweet
    #print(outstring)
    n_chars <- nchar(outstring)
    if(photo ==T){
      extra_length <- 24 + nchar(hashtag) + nchar(paste('Image by ', author, sep = '')) #all urls take up 23 characters, then one for a space after it, then more for image attribution
    }else{
      extra_length <- 24 + nchar(hashtag)
    }

    if(n_chars <240-extra_length & n_chars >120){ #If the string is a tweetable length and long enough to be interesting, tweet it

      page <- page_info("en", "wikipedia", page=sp_name , as_wikitext=TRUE)

      url <- page$query$pages[[1]]$fullurl
      if(photo==T){
        outstring <- paste(outstring, 'Image by', author, url, hashtag, sep =' ')
      }else{
        outstring <- paste(outstring, url, hashtag, sep =' ')
      }

      print(outstring)
      tweetable <- TRUE
    }
    #outstring <- paste(outstring, 'put_url_here', '#bats', sep =' ')
    #print(outstring)
    #cat('outstring is ', n_chars, ' long')
    #readline(prompt="Press [enter] to continue")
  }


  if(photo==T){
    download.file(photo_url, 'temp.jpg', mode = 'wb') #This is done near the bottom so we don't regularly download images for potential tweets that don't pass our QC
  }


  #####twitter things ####
  ##Vignette of instructions for using it here http://rtweet.info/articles/auth.html
  #now we just tweet the output
   if(photo==T){
     post_tweet(status = outstring, token = twitter_token,
               in_reply_to_status_id = NULL, media = './temp.jpg')
     file.remove('temp.jpg')
   }else{
     post_tweet(status = outstring, token = twitter_token,
                in_reply_to_status_id = NULL)
   }

  print(Sys.time())

  if(wait_in_r==TRUE){
    Sys.sleep(wait_duration) #The number of seconds to sleep for
  }else{
    A <- TRUE
  }


}

library(tidyverse)
library(xml2)
library(rvest)
library(WikipediR)
library(urltools)

# Get all speceies-level page titles from the Wikipedia list of bats
bat_titles <- read_html("https://en.wikipedia.org/wiki/List_of_bats") %>%
html_nodes(xpath="//ul/li[contains(., 'Genus')]/ul/li/a[starts-with(@href, '/wiki/')]") %>%
xml_attr("href") %>%
basename() %>%
url_decode()

# Get the content of all those pages (takes a couple of mins!)
bat_info <- map_df(bat_titles, function(x) {
return <- page_content(language="en", project="wikipedia", page_name=x)
data_frame(title = return$parse$title,
content = return$parse$text$`*`)
})

# Extract just the text from the HTML
bat_text <- bat_info %>%
mutate(content = map_chr(content, ~html_text(read_html(.))))


library(rvest)
url <- "https://en.wikipedia.org/wiki/Arizona_League"

# get the lines of the table
lines <- url %>%
  read_html() %>%
  html_nodes(xpath="//table[starts-with(@class, 'wikitable')]") %>%
  html_nodes(xpath = 'tbody/tr')
#define the empty table
ncol <-  lines %>%
  .[[1]] %>%
  html_children()%>%
  length()
nrow <- length(lines)
table <- as.data.frame(matrix(nrow = nrow,ncol = ncol))
# fill the table
for(i in 2:2){
  
  # get content of the line
  linecontent <- lines[[i]]%>%
    html_children()%>%
    html_text()%>%
    gsub("\n","",.)
  
  # attribute the content to free columns
  colselect <- is.na(table[i,])
  
  table[i,colselect] <- linecontent
  
  # get the line repetition of each columns
  repetition <- lines[[i]]%>%
    html_children()%>%
    html_attr("rowspan")%>%
    ifelse(is.na(.),1,.) %>% # if no rowspan, then it is a normal row, not a multiple one
    as.numeric
  
  # repeat the cells of the multiple rows down
  for(j in 1:length(repetition)){
    span <- repetition[j]
    if(span > 1){
      table[(i+1):(i+span-1),colselect][,j] <- rep(linecontent[j],span-1)
    }
  }
}

i <- 1
# get content of the line
linecontent <- lines[[i]]%>%
  html_children()%>%
  html_text()%>%
  gsub("\n","",.)

# attribute the content to free columns
colselect <- is.na(table[i,])
table[i,colselect] <- linecontent

# get the line repetition of each columns
repetition <- lines[[i]]%>%
  html_children()%>%
  html_attr("rowspan")%>%
  ifelse(is.na(.),1,.) %>% # if no rowspan, then it is a normal row, not a multiple one
  as.numeric


linecontent <- lines[[3]]%>%
  html_children()%>%
  html_text()%>%
  gsub("\n","",.)

# attribute the content to free columns
colselect <- is.na(table[3,])
table[3,colselect] <- linecontent

#----------------------------------------------------------
# TN_Beds<-read_html(remDr$getPageSource()[[1]])

# get the lines of the table
lines_TN <-TN_Beds %>% 
  html_nodes(xpath='/html/body/div/div/div/div[1]/div/div/div[3]/div/div/div/div/div[1]/table') %>%
  html_nodes(xpath = 'tbody/tr')

#define the empty table
ncol_TN <- lines_TN %>%
  .[[1]] %>%
  html_children()%>%
  html_children()%>%
  length()
nrow_TN <- length(lines_TN)
table_TN <- as.data.frame(matrix(nrow = nrow_TN, ncol = ncol_TN))

# fill the table
table_A <-character(0)
for(i in 1:nrow_TN){
  # get content of the line
  linecontent_TN <- lines_TN[[i]]%>%
    html_children()%>%
    html_children()%>%
    html_text()
  # attribute the content to free columns
  colselect_TN <- is.na(table_TN[i,])
  table_TN[i,colselect_TN] <- linecontent_TN
  table_A<-rbind(table_TN,table_A)
}

for(i in 1:nrow_TN_3){
  # get content of the line
  linecontent_TN_3 <- lines_TN_3[[i]]%>%
    html_children()%>%
    html_children()%>%
    html_text()
  # attribute the content to free columns
  colselect_TN_3 <- is.na(table_TN_3[i,])
  table_TN_3[i,colselect_TN_3] <- linecontent_TN_3
  table_A<-rbind(table_TN_3,table_A)
}


lines[[2]]
lines_TN[[2]]

linecontent
linecontent_TN <- lines_TN[[2]] %>%
  html_children()%>%
  html_children() %>%
  html_text()


# attribute the content to free columns
colselect <- is.na(table[i,])
print(colselect)

table[i,colselect] <- linecontent
print(table)



require(xml2)
require(purrr)
require(tidyr)
urls <- rep("http://www.ebi.ac.uk/ena/data/view/ERS445758&display=xml", 2)
identifier <- LETTERS[seq_along(urls)] # Take a unique identifier per url here

parse_attribute <- function(x){
  out <- data.frame(tag = xml_text(xml_find_all(x, "./TAG")),
                    value = xml_text(xml_find_all(x, "./VALUE")), stringsAsFactors = FALSE)
  spread(out, tag, value)
}

doc <- map(urls, read_xml)

out <- doc %>% 
  map(xml_find_all, "//SAMPLE_ATTRIBUTE") %>% 
  set_names(identifier) %>%
  map_df(parse_attribute, .id="url")
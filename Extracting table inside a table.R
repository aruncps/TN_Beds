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
for(i in 1:nrow){
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

lines[[3]]%>%
  html_children()%>%
  html_text()%>%
  gsub("\n","",.)


linecontent <- lines[[3]]%>%
  html_children()%>%
  html_text()%>%
  gsub("\n","",.)

# attribute the content to free columns
colselect <- is.na(table[3,])
table[3,colselect] <- linecontent


#----------------------------------------------------------
# get the lines of the table
lines <-TN_Beds %>% 
  html_nodes(xpath='/html/body/div/div/div/div[1]/div/div/div[3]/div/div/div/div/div[1]/table') %>%
  html_nodes(xpath = 'tbody/tr')

#define the empty table
ncol <-  lines %>%
  .[[1]] %>%
  html_children()%>%
  length()
nrow <- length(lines)
table <- as.data.frame(matrix(nrow = nrow,ncol = ncol))

lines[[6]]%>%
  html_children() %>%
  .[[1]] %>%
  html_children() %>%
  html_text()

lines[[14]]%>%
  html_children() %>%
  .[[1]] %>%
  html_children() %>%
  html_text()

  html_nodes(xpath = 'tbody/tr')
  html_text()%>%
  gsub("\n","",.)
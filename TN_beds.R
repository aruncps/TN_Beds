# IMPORT Packages 
lapply(c("tidyverse","rvest","stringr","hrbrthemes","viridis","plotly","scales","fs","RSelenium"), library, character.only = TRUE)

#--------- Working code
rD <- rsDriver(browser="chrome", verbose = FALSE, port=1234L, chromever = "90.0.4430.24")
remDr <- rD[["client"]]

# Navigate to site
remDr$navigate("https://tncovidbeds.tnega.org")
# Establish a wait for an element
remDr$setTimeout(type = "page load", milliseconds = 60000)
Sys.sleep(10)

# Loop_01 to select the district 

# Select district of interest
District_dropdown <- remDr$findElement(using = 'class', "react-select__input")
District_input <- District_dropdown$findChildElement(using = 'xpath', "input")
District_input$sendKeysToElement(list("Chennai", "\uE007"))
Sys.sleep(10)

# Select Modal Agreement
modalAgree<-remDr$findElement(using = 'xpath', '/html/body/div[2]/div/div[1]/div/div/center/button')
modalAgree$clickElement()

# Select # of Hospitals to display in 1 page
HospPerPg_dropdown <- remDr$findElement(using = 'css', '[class="d-inline-block dropdown"]')
HospPerPg_dropdown$clickElement()

# Select 100 as # of Hospitals to be displayed in 1 page
HospPerPg_100 <-HospPerPg_dropdown$findChildElement(using = 'xpath', '/html/body/div/div/div/div[1]/div/div/div[3]/div/div/div/div/div[1]/div[4]/div/div/div/div/div/div/div/button[5]')
HospPerPg_100$sendKeysToElement(list("\uE007"))
Sys.sleep(10)

# Find the number of pages available
TN_Beds<-read_html(remDr$getPageSource()[[1]])
NoBeds_txt<-TN_Beds %>% html_nodes(xpath="/html/body/div/div/div/div[1]/div/div/div[3]/div/div/div/div/div[1]/div[4]/div/div/div/div/div/span[2]") %>% html_text() 
bed_txt<-str_sub(NoBeds_txt, 9, str_length(NoBeds_txt))
num <- as.numeric(str_trim(bed_txt))%/%100
remin <- as.numeric(str_trim(bed_txt))%%100
pages<-if (remin <= 0) {num} else {num+1}

# Loop_02 to click the page of interest
for(var in 1:pages)
{
  PgXpath<-paste0('/html/body/div/div/div/div[1]/div/div/div[3]/div/div/div/div/div[2]/ul/li[',var+2,']/a')
  print(PgXpath)
  Pg_click <-remDr$findElement(using = 'xpath', PgXpath)
  Pg_click$clickElement()
  remDr$setTimeout(type = "page load", milliseconds = 60000)
  Sys.sleep(10)
}

# Extract table
export_html<-read_html(html_covid)
TN_Beds %>% 
  html_nodes(xpath='/html/body/div/div/div/div[1]/div/div/div[3]/div/div/div/div/div[1]/table') %>%
  html_table(fill=TRUE) 
  
TN_Beds %>% 
  html_nodes('td.text-left.w-20 > span:nth-child(1) > a') %>%
  html_attr("href")
    html_text()  
  #AutoNumber1 > tbody > tr:nth-child(3) > td:nth-child(2) > font > a
    #tableBody > tr:nth-child(1) > td.text-left.w-20 > span:nth-child(7) > a
  #tableBody > tr:nth-child(1) > td.text-left.w-20 > span:nth-child(1) > a
    
    //*[@id="tableBody"]/tr[1]/td[1]/span[4]/a
    

# Extract for table attributes hidden as array

# Rbind all page data to variable
# End of Loop_02

# Rbind all district page data to variable
# End of Loop_01


# 12 Final as clean csv extract for a day

# Test 
# HospPerPg_dropdown$highlightElement()
# HospPerPg_dropdown$clickElement()

remDr$close()
rD[["server"]]$stop()
rm(rD)
gc()







#--------- Explore


html_covid<-paste0("https://tncovidbeds.tnega.org/")
export_html<-read_html(html_covid)


export_html %>% 
  html_nodes(xpath='/html/body/div/div/div/div[1]/div/div/div[3]/div/div/div/div/div[1]/table') %>%
  html_table()



doc <- read_html("http://www.soccer24.com/kosovo/superliga/results/")

doc %>%
  html_nodes(xpath='/html/body/div[6]/div[1]/div/div[1]/div[2]/div[4]/div[2]') %>%
  html_text()



system("/usr/local/bin/phantomjs /home/arunkumar/Documents/GitHub/TN_Beds/scraper_PaddyPower.js")

withJS <- xml2::read_html("odds_PaddyPower.html") %>%
  +   rvest::html_nodes(".avb-item") %>%
  +   rvest::html_text()

#--------- Working code
rD <- rsDriver(browser="chrome", verbose = FALSE, port=1234L, chromever = "90.0.4430.24")
remDr <- rD[["client"]]
remDr$navigate("https://tncovidbeds.tnega.org")
District_dropdown <- remDr$findElement(using = 'class', "react-select__input")
District_input <- District_dropdown$findChildElement(using = 'xpath', "input")
District_input$sendKeysToElement(list("Viru", "\uE007"))

HospPerPg_dropdown <- remDr$findElement(using = 'css', '[class="d-inline-block dropdown"]')
HospPerPg_100 <-HospPerPg_dropdown$findChildElement(using = 'xpath', '//*[@id="root"]/div/div/div[1]/div/div/div[3]/div/div/div/div/div[1]/div[4]/div/div/div/div/div/div/div/button[5]')
HospPerPg_100$sendKeysToElement(list("\uE007"))

#--------- Explore

Button2$highlightElement()
Button$clickElement()
Button$sendKeysToElement(list("\uE015"))
Button$sendKeysToElement(list("\uE007"))



<button type="button" aria-haspopup="true" aria-expanded="false" class="outlineBtn dropdown-toggle btn btn-outline-dark btn-xs">10</button>
<button type="button" tabindex="0" role="menuitem" class="ddlItem dropdown-item">100</button>

#---- Read html -- Data import
Chennai_Beds<-read_html(remDr$getPageSource()[[1]])
NoBeds_txt<-Chennai_Beds %>% html_nodes(xpath="/html/body/div/div/div/div[1]/div/div/div[3]/div/div/div/div/div[1]/div[4]/div/div/div/div/div/span[2]") %>% html_text() 
bed_txt<-str_sub(NoBeds_txt, 9, str_length(NoBeds_txt))
num <- as.numeric(str_trim(bed_txt))%/%100
remin <- as.numeric(str_trim(bed_txt))%%100

pages<-if (remin <= 0) {num} else {num+1}

TN_Beds<-Chennai_Beds %>% html_nodes(xpath="//*[@id='bedTable']") %>% html_table()
TN_Beds
View(tibble(data.frame(TN_Beds)))

remDr$navigate("https://www.amazon.com/Eagles-Nest-Outfitters-DoubleNest-Portable/product-reviews/B00K30GXK8/ref=cm_cr_dp_d_show_all_btm?ie=UTF8&reviewerType=all_reviewshttps://www.amazon.com/Eagles-Nest-Outfitters-DoubleNest-Portable/product-reviews/B00K30GXK8/ref=cm_cr_dp_d_show_all_btm?ie=UTF8&reviewerType=all_reviews")
webElem <- remDr$findElement("css", "body")
webElem$sendKeysToElement(list(key = "end"))
morereviews <- remDr$findElement(using = 'css selector', "#BVRRContainer div.bv-content-pagination-container button")
# click button
webElem$highlightElement()

WebElemP1$getElementTag()
WebElemP1$sendKeysToElement(list("\uE015"))
WebElemP1$sendKeysToElement(list("Chennai"))

<input name="form-field-name" type="hidden" value="5ea0abd4d43ec2250a483a61">
//*[@id="root"]/div/div/div[1]/div/div/div[2]/div/div/div[1]/div/div/p[2]/div/input


TN_BEd %>% 
    html_nodes(xpath="/html/body/div/div/div/div[1]/div/div/div[2]/div/div/div[1]/div/div/p[2]/div[1]") %>%
    html_text()
    

WebElemFileFormat$getElementAttribute("class")

WebElemFileFormat$highlightElement()
WebElemFileFormat$sendKeysToActiveElement(list(key = 'arrow_down'))


TN_Beds<-read_html(WebElemFileFormat$getPageSource()[[1]])
TN_Beds %>% 
    html_nodes("[class='react-select__menu']") %>% 
    html_text()



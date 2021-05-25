# IMPORT Packages 
lapply(c("tidyverse","rvest","stringr","hrbrthemes","viridis","plotly","scales","fs","RSelenium"), library, character.only = TRUE)
# DEFINE Variables
currDate<-Sys.Date()
start_time<-Sys.time()

#--------- Working code
rD <- rsDriver(browser="chrome", verbose = FALSE, port=1234L, chromever = "90.0.4430.24")
remDr <- rD[["client"]]

# Navigate to site
remDr$navigate("https://tncovidbeds.tnega.org")
# Establish a wait for an element
remDr$setTimeout(type = "page load", milliseconds = 60000)
Sys.sleep(runif(1,10,15))

# Loop_01 to select the district 
Dist<- c(
        "Ariyalur", "Chengalpattu", "Chennai", "Coimbatore", "Cuddalore",
        "Dharmapuri", "Dindigul", "Erode", "Kallakurichi", "Kancheepuram",
        "Kanniyakumari", "Karur", "Krishnagiri", "Madurai", "Mayiladuthurai",
        "Nagapattinam", "Namakkal", "Nilgiris", "Perambalur", "Pudukkottai",
        "Ramanathapuram", "Ranipet", "Salem", "Sivagangai", "Tenkasi",
        "Thanjavur", "Theni", "Thiruchirappalli", "Thirupathur", "Thiruvarur",
        "Thoothukudi", "Tirunelveli", "Tiruppur", "Tiruvallur", "Tiruvannamalai",
        "Vellore", "Villupuram", "Virudhunagar"
         )

# Dist<- c("Mayiladuthurai")

table_A <-as.data.frame(character(0))
for (i in Dist) {
    print(paste0("Dist - ",i))
  
    # Select district of interest
    District_dropdown <- remDr$findElement(using = 'class', "react-select__input")
    District_input <- District_dropdown$findChildElement(using = 'xpath', "input")
    District_input$sendKeysToElement(list(i, "\uE007"))
    Sys.sleep(runif(1,10,15))
    
    # Select Modal Agreement
    modalAgree<-remDr$findElement(using = 'xpath', '/html/body/div[2]/div/div[1]/div/div/center/button')
    modalAgree$clickElement()
    
    # Select # of Hospitals to display in 1 page
    HospPerPg_dropdown <- remDr$findElement(using = 'css', '[class="d-inline-block dropdown"]')
    HospPerPg_dropdown$clickElement()
    
    # Select 100 as # of Hospitals to be displayed in 1 page
    HospPerPg_100 <-HospPerPg_dropdown$findChildElement(using = 'xpath', '/html/body/div/div/div/div[1]/div/div/div[3]/div/div/div/div/div[1]/div[4]/div/div/div/div/div/div/div/button[5]')
    HospPerPg_100$sendKeysToElement(list("\uE007"))
    Sys.sleep(runif(1,10,15))
    
    # Find the number of pages available
    Dist_Pages<-read_html(remDr$getPageSource()[[1]])
    NoBeds_txt<-Dist_Pages %>% html_nodes(xpath="/html/body/div/div/div/div[1]/div/div/div[3]/div/div/div/div/div[1]/div[4]/div/div/div/div/div/span[2]") %>% html_text() 
    bed_txt<-str_sub(NoBeds_txt, str_locate(NoBeds_txt,"of ")[2], str_length(NoBeds_txt))
    NoHosp<- as.numeric(str_trim(bed_txt))
    num <- as.numeric(str_trim(bed_txt))%/%100
    remin <- as.numeric(str_trim(bed_txt))%%100
    pages<-if (remin <= 0) {num} else {num+1}
  
    # Loop_02 to click the page of interest
    for(var in 1:pages)
    {
      body_b <- remDr$findElement("css", "body")
      body_b$sendKeysToElement(list(key = "end"))
      
      print(paste0("page - ",var))
      #PgXpath<-paste0('#root > div > div > div.DesktopView > div > div > div:nth-child(4) > div > div > div > div > div.mt-3.col-12 > ul > li:nth-child(',var+2,')')
      #Pg_click <-remDr$findElement(using = 'css selector', PgXpath)
      webElem <-NULL
      while(is.null(webElem)){
        webElem <- tryCatch({remDr$findElement(using = 'class', value = 'justify-content-center')},
                            error = function(e){NULL})
        #loop until element with name <value> is found in <webpage url>
      }
      print("webElemPASS")
      webElem$highlightElement()
      Sys.sleep(runif(1,10,15))
      
      print("highlight")
      PgXpath_m<-remDr$findElement(using = 'class', 'justify-content-center')
      Pg_click <- PgXpath_m$findChildElement(using = 'xpath', paste0('li[',var+2,']'))
      Pg_click$highlightElement()
      

      print("click")
      Pg_click$clickElement()
      Sys.sleep(runif(1,10,15))
      
      
      TN_Beds<-read_html(remDr$getPageSource()[[1]])
      Sys.sleep(runif(1,10,15))
      
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
      print(paste0(nrow_TN,',',ncol_TN))
      
      linecontent_TN <- lines_TN[[1]]%>%
        html_children()%>%
        html_children()%>%
        html_text() %>%
        .[[1]]
    
      print(linecontent_TN)
    
      # fill the table
      for(i in 1:nrow_TN){
        # get content of the line
        linecontent_TN <- lines_TN[[i]]%>%
          html_children()%>%
          html_children()%>%
          html_text()
        # attribute the content to free columns
        colselect_TN <- is.na(table_TN[i,])
        table_TN[i,colselect_TN] <- linecontent_TN
      }
      table_A<-rbind(table_TN,table_A)
      #print(str(table_A))
      Sys.sleep(runif(1,10,15))
    }
      
}

# # or scroll at the bottom of page
# body_b <- remDr$findElement("css", "body")
# body_b$sendKeysToElement(list(key = "end"))
# # then you can go to top
# # body_b$sendKeysToElement(list(key = "home"))
end_time<-Sys.time()
end_time-start_time

remDr$close()
rD[["server"]]$stop()
rm(rD)
gc()

#--------- Explore
# View(table_A)
colnames(table_A)[1] <- "HospitalName"
colnames(table_A)[2] <- "Remove1"
colnames(table_A)[3] <- "Address"
colnames(table_A)[4] <- "Remove2"
colnames(table_A)[5] <- "District"
colnames(table_A)[6] <- "Remove3"
colnames(table_A)[7] <- "Phone"
colnames(table_A)[8] <- "Remove4"
colnames(table_A)[9] <- "Update_DtTm"
colnames(table_A)[10] <- "Remove5"
colnames(table_A)[11] <- "Normal_Bed_Total"
colnames(table_A)[12] <- "Normal_Bed_Vacant"
colnames(table_A)[13] <- "O2_Bed_Total"
colnames(table_A)[14] <- "O2_Bed_Vacant"
colnames(table_A)[15] <- "ICU_Bed_Total"
colnames(table_A)[16] <- "ICU_Bed_Vacant"
colnames(table_A)[17] <- "All_Bed_Total"
colnames(table_A)[18] <- "All_Bed_Vacant"

tbl_TN_CovidBeds <- table_A %>% 
                        select(HospitalName,Address,District,Phone,Update_DtTm,
                               Normal_Bed_Total,Normal_Bed_Vacant,
                               O2_Bed_Total,O2_Bed_Vacant,
                               ICU_Bed_Total,ICU_Bed_Vacant,
                               All_Bed_Total,All_Bed_Vacant) %>% 
                        mutate(
                          Normal_Bed_Total=as.numeric(Normal_Bed_Total),Normal_Bed_Vacant=as.numeric(Normal_Bed_Vacant),
                          O2_Bed_Total=as.numeric(O2_Bed_Total),O2_Bed_Vacant=as.numeric(O2_Bed_Vacant),
                          ICU_Bed_Total=as.numeric(ICU_Bed_Total),ICU_Bed_Vacant=as.numeric(ICU_Bed_Vacant),
                          All_Bed_Total=as.numeric(All_Bed_Total),All_Bed_Vacant=as.numeric(All_Bed_Vacant),
                          importDate=currDate
                          )
tbl_TN_CovidBeds<-tibble(tbl_TN_CovidBeds)

tbl_byDist_CovidBeds <- tbl_TN_CovidBeds %>% 
      group_by(importDate,District) %>% 
      summarize(
          No_of_Hospitals=length(HospitalName),
          Normal_Bed_Vacant=sum(Normal_Bed_Vacant),Normal_Bed_Total=sum(Normal_Bed_Total),
          O2_Bed_Vacant=sum(O2_Bed_Vacant),O2_Bed_Total=sum(O2_Bed_Total),
          ICU_Bed_Vacant=sum(ICU_Bed_Vacant),ICU_Bed_Total=sum(ICU_Bed_Total),
          All_Bed_Vacant=sum(All_Bed_Vacant),All_Bed_Total=sum(All_Bed_Total)
          ) %>% 
      mutate(
          Normal_Bed_Occupancy=(Normal_Bed_Total-Normal_Bed_Vacant)/Normal_Bed_Total,
          O2_Bed_Occupancy=(O2_Bed_Total-O2_Bed_Vacant)/O2_Bed_Total,
          ICU_Bed_Occupancy=(ICU_Bed_Total-ICU_Bed_Vacant)/ICU_Bed_Total,
          All_Bed_Occupancy=(All_Bed_Total-All_Bed_Vacant)/All_Bed_Total
          ) %>% 
      ungroup()

# Write a copy 
file_TN_CovidBeds<-paste0("/home/arunkumar/Documents/GitHub/TN_Beds/TN_CovidBeds.csv")
file_byDist_CovidBeds<-paste0("/home/arunkumar/Documents/GitHub/TN_Beds/byDist_CovidBeds.csv")

# write.table(tbl_TN_CovidBeds, file_TN_CovidBeds, append = TRUE, col.names = FALSE, row.names = FALSE)
# write.table(tbl_byDist_CovidBeds, file_byDist_CovidBeds, append = TRUE, col.names = FALSE, row.names = FALSE)

TN_CovidBeds<-read.table(file_TN_CovidBeds,header=TRUE, row.names=NULL)
TN_CovidBeds<-tibble(TN_CovidBeds)
TN_CovidBeds %>% group_by(importDate) %>% summarise(n()) %>% arrange(desc(importDate))

byDist_CovidBeds<-read.table(file_byDist_CovidBeds,header=TRUE, row.names=NULL)
byDist_CovidBeds<-tibble(byDist_CovidBeds)
byDist_CovidBeds %>% group_by(importDate) %>% summarise(n()) %>% arrange(desc(importDate))

View(byDist_CovidBeds %>% select(importDate, District, No_of_Hospitals, Normal_Bed_Occupancy, O2_Bed_Occupancy, ICU_Bed_Occupancy, All_Bed_Occupancy ))

#--------------------------------------------------
lapply(c("tidyverse","rvest","hrbrthemes","viridis","plotly","scales","stringr","zoo","tidyquant"), library, character.only = TRUE)

Dist_Beds <- byDist_CovidBeds %>% 
                group_by(importDate, District, Normal_Bed_Occupancy) %>%
                summarize(
                  No_of_Hospitals=sum(No_of_Hospitals),
                  Normal_Bed_Occupancy=sum(Normal_Bed_Occupancy),
                  O2_Bed_Occupancy=sum(O2_Bed_Occupancy),
                  ICU_Bed_Occupancy=sum(ICU_Bed_Occupancy),
                  All_Bed_Occupancy=sum(All_Bed_Occupancy)
                ) %>%
                select(importDate, District, No_of_Hospitals, Normal_Bed_Occupancy, O2_Bed_Occupancy, ICU_Bed_Occupancy, All_Bed_Occupancy ) %>%
                ungroup()

Dist_Beds$importDate<-as.Date(as.character(Dist_Beds$importDate))

byDist_CovidBeds %>% filter(District=='Mayiladuthurai')


Dist_Beds$Normal_Bed_Occupancy_Y1<-cut(Dist_Beds$Normal_Bed_Occupancy,breaks = c(0,0.2,0.4,0.6,0.8,1,Inf),right = FALSE)
Dist_Beds$O2_Bed_Occupancy_Y1<-cut(Dist_Beds$O2_Bed_Occupancy,breaks = c(0,0.2,0.4,0.6,0.8,1,Inf),right = FALSE)
Dist_Beds$ICU_Bed_Occupancy_Y1<-cut(Dist_Beds$ICU_Bed_Occupancy,breaks = c(0,0.2,0.4,0.6,0.8,1,Inf),right = FALSE)
Dist_Beds$All_Bed_Occupancy_Y1<-cut(Dist_Beds$All_Bed_Occupancy,breaks = c(0,0.2,0.4,0.6,0.8,1,Inf),right = FALSE)



ggplot(Dist_Beds
       ,aes(importDate, District)) + 
  geom_tile(aes(fill= Normal_Bed_Occupancy_Y1),colour="white",size=0.25) +
  # scale_fill_gradient(low="#fffaaf", high="#ff4747") +
  scale_fill_manual(breaks=c("[0,0)", "[0,0.2)", "[0.2,0.4)", "[0.4,0.6)", "[0.6,0.8)", "[0.8,1)","[1,Inf)"),
                    values = c("#e6e6e6", "#c9ff96", "#fffa96","#ffc800", "#ff7d00", "#ff0000","#af0000"),
                    labels = c("0", "0-20", "20-40", "40-60", "60-80", "80-100","100" ),
                    name = "Normal Bed Occupancy (%)") +
  ylab(label = "District") +
  xlab(label = "Date") +
  scale_y_discrete(limits = rev(levels(as.factor(droplevels(Dist_Beds$District) ))) 
                   ,position = "left") +
  theme_classic() +
  theme(strip.placement = "outside",
        plot.title = element_text(hjust = 0.5),
        axis.title.y = element_blank(), # Remove y-axis title
        strip.background = element_rect(fill = "#EEEEEE", color = "#FFFFFF"),
        legend.position="top"
  ) +
  scale_x_date(date_breaks = "1 day", date_minor_breaks = "1 day", date_labels = "%d/%m")

ggplot(Dist_Beds
       ,aes(importDate, District)) + 
  geom_tile(aes(fill= O2_Bed_Occupancy_Y1),colour="white",size=0.25) +
  # scale_fill_gradient(low="#fffaaf", high="#ff4747") +
  scale_fill_manual(breaks=c("[0,0)", "[0,0.2)", "[0.2,0.4)", "[0.4,0.6)", "[0.6,0.8)", "[0.8,1)","[1,Inf)"),
                    values = c("#e6e6e6", "#c9ff96", "#fffa96","#ffc800", "#ff7d00", "#ff0000","#af0000"),
                    labels = c("0", "0-20", "20-40", "40-60", "60-80", "80-100","100" ),
                    name = "O2 Bed Occupancy (%)") +
  ylab(label = "District") +
  xlab(label = "Date") +
  scale_y_discrete(limits = rev(levels(as.factor(droplevels(Dist_Beds$District) ))) 
                   ,position = "left") +
  theme_classic() +
  theme(strip.placement = "outside",
        plot.title = element_text(hjust = 0.5),
        axis.title.y = element_blank(), # Remove y-axis title
        strip.background = element_rect(fill = "#EEEEEE", color = "#FFFFFF"),
        legend.position="top"
  ) +
  scale_x_date(date_breaks = "1 day", date_minor_breaks = "1 day", date_labels = "%d/%m")

ggplot(Dist_Beds
       ,aes(importDate, District)) + 
  geom_tile(aes(fill= ICU_Bed_Occupancy_Y1),colour="white",size=0.25) +
  # scale_fill_gradient(low="#fffaaf", high="#ff4747") +
  scale_fill_manual(breaks=c("[0,0)", "[0,0.2)", "[0.2,0.4)", "[0.4,0.6)", "[0.6,0.8)", "[0.8,1)","[1,Inf)"),
                    values = c("#e6e6e6", "#c9ff96", "#fffa96","#ffc800", "#ff7d00", "#ff0000","#af0000"),
                    labels = c("0", "0-20", "20-40", "40-60", "60-80", "80-100","100" ),
                    name = "ICU Bed Occupancy (%)") +
  ylab(label = "District") +
  xlab(label = "Date") +
  scale_y_discrete(limits = rev(levels(as.factor(droplevels(Dist_Beds$District) ))) 
                   ,position = "left") +
  theme_classic() +
  theme(strip.placement = "outside",
        plot.title = element_text(hjust = 0.5),
        axis.title.y = element_blank(), # Remove y-axis title
        strip.background = element_rect(fill = "#EEEEEE", color = "#FFFFFF"),
        legend.position="top"
  ) +
  scale_x_date(date_breaks = "1 day", date_minor_breaks = "1 day", date_labels = "%d/%m")


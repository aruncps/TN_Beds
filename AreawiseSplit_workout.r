
#--------------------------------------------------
# INPROGRESS
# file_AreaSplit_TN_CovidHospitals<-paste0("/home/arunkumar/Documents/GitHub/TN_Beds/AreaSplit_TN_CovidHospitals.csv")
# 
# AreaSplit_TN_CovidHospitals<-read.csv(file_AreaSplit_TN_CovidHospitals,header=TRUE, row.names=NULL)
# AreaSplit_TN_CovidHospitals<-tibble(AreaSplit_TN_CovidHospitals)

tbl_TN_Area_CovidBeds <- TN_CovidBeds %>% mutate(HospitalName=str_trim(HospitalName)) %>%
  inner_join(AreaSplit_TN_CovidHospitals, by = c("District"="District","HospitalName"="HospitalName"))

View(
tbl_TN_Area_CovidBeds %>% 
  filter(District=="Chengalpattu" & importDate=="2021-05-24" & ICU_Bed_Total > 0) %>% 
  arrange(desc(ICU_Bed_Total))
)

TN_Area_CovidBeds <- tbl_TN_Area_CovidBeds %>% 
  group_by(importDate,District,Area) %>% 
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
  select(importDate, District, Area, No_of_Hospitals, Normal_Bed_Occupancy, O2_Bed_Occupancy, ICU_Bed_Occupancy, All_Bed_Occupancy ) %>%
  ungroup()


TN_Area_CovidBeds$importDate<-as.Date(as.character(TN_Area_CovidBeds$importDate))

TN_Area_CovidBeds$Normal_Bed_Occupancy_Y1<-cut(TN_Area_CovidBeds$Normal_Bed_Occupancy,breaks = c(0,0.2,0.4,0.6,0.8,1,Inf),right = FALSE)
TN_Area_CovidBeds$O2_Bed_Occupancy_Y1<-cut(TN_Area_CovidBeds$O2_Bed_Occupancy,breaks = c(0,0.2,0.4,0.6,0.8,1,Inf),right = FALSE)
TN_Area_CovidBeds$ICU_Bed_Occupancy_Y1<-cut(TN_Area_CovidBeds$ICU_Bed_Occupancy,breaks = c(0,0.2,0.4,0.6,0.8,1,Inf),right = FALSE)
TN_Area_CovidBeds$All_Bed_Occupancy_Y1<-cut(TN_Area_CovidBeds$All_Bed_Occupancy,breaks = c(0,0.2,0.4,0.6,0.8,1,Inf),right = FALSE)

Ariyalur_Area_CovidBeds <- TN_Area_CovidBeds %>% filter(District == "Chen")
ggplot(Ariyalur_Area_CovidBeds
       ,aes(importDate, Area)) + 
  geom_tile(aes(fill= Normal_Bed_Occupancy_Y1),colour="white",size=0.25) +
  # scale_fill_gradient(low="#fffaaf", high="#ff4747") +
  scale_fill_manual(breaks=c("[0,0)", "[0,0.2)", "[0.2,0.4)", "[0.4,0.6)", "[0.6,0.8)", "[0.8,1)","[1,Inf)"),
                    values = c("#e6e6e6", "#c9ff96", "#fffa96","#ffc800", "#ff7d00", "#ff0000","#af0000"),
                    labels = c("0", "0-20", "20-40", "40-60", "60-80", "80-100","100" ),
                    name = "Normal Bed Occupancy (%)") +
  ylab(label = "Area") +
  xlab(label = "Date") +
  scale_y_discrete(limits = rev(levels(as.factor(droplevels(Ariyalur_Area_CovidBeds$Area) ))) 
                   ,position = "left") +
  theme_classic() +
  theme(strip.placement = "outside",
        plot.title = element_text(hjust = 0.5),
        axis.title.y = element_blank(), # Remove y-axis title
        strip.background = element_rect(fill = "#EEEEEE", color = "#FFFFFF"),
        legend.position="top"
  ) +
  scale_x_date(date_breaks = "1 week", date_minor_breaks = "1 day", date_labels = "%d/%m")


Chennai_Area_CovidBeds <- TN_Area_CovidBeds %>% filter(District == "Ariyalur")
ggplot(Chennai_Area_CovidBeds
       ,aes(importDate, Area)) + 
  geom_tile(aes(fill= ICU_Bed_Occupancy_Y1),colour="white",size=0.25) +
  # scale_fill_gradient(low="#fffaaf", high="#ff4747") +
  scale_fill_manual(breaks=c("[0,0)", "[0,0.2)", "[0.2,0.4)", "[0.4,0.6)", "[0.6,0.8)", "[0.8,1)","[1,Inf)"),
                    values = c("#e6e6e6", "#c9ff96", "#fffa96","#ffc800", "#ff7d00", "#ff0000","#af0000"),
                    labels = c("0", "0-20", "20-40", "40-60", "60-80", "80-100","100" ),
                    name = "Normal Bed Occupancy (%)") +
  ylab(label = "Area") +
  xlab(label = "Date") +
  scale_y_discrete(limits = rev(levels(as.factor(droplevels(Chennai_Area_CovidBeds$Area) ))) 
                   ,position = "left") +
  theme_classic() +
  theme(strip.placement = "outside",
        plot.title = element_text(hjust = 0.5),
        axis.title.y = element_blank(), # Remove y-axis title
        strip.background = element_rect(fill = "#EEEEEE", color = "#FFFFFF"),
        legend.position="top"
  ) +
  scale_x_date(date_breaks = "1 week", date_minor_breaks = "1 day", date_labels = "%d/%m")

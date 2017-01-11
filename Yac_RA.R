#SET WORKING DIRECTORY
setwd("~/Dropbox/YAC/Data")


# PACKAGES ----------------------------------------------------------------
  require(readxl)
  require(dplyr)
  require(tidyr)
  require(ggplot2)
  require(ggmap)
  require(readr)
  require(RgoogleMaps)
  require(rgdal)
  require(sp)
  require(plotGoogleMaps)

# DATA IMPORT -------------------------------------------------------------
  ##YAC DATA
  YAC_MASTER<-read_excel("Yac_data.xlsx",sheet=1, skip=1)
  
  ##POSTCODE DATA
  Postcodes_master <- read_csv("~/Dropbox/yac/Data/postcodes.csv")
  
  ##ONS DATA
  ONS_Master <- read_csv("~/Dropbox/yac/Data/ONSPD_FEB_2016_UK.csv")
 
 

# CLEANING ----------------------------------------------------------------

  #If patient has no postcode at time of referral, assume that this is the same as current postcode
  YAC_MASTER$`Post Code at Referral (or First NEPHYAC Appt)`<-ifelse(is.na(YAC_MASTER$`Post Code at Referral (or First NEPHYAC Appt)`), YAC_MASTER$`Post Code Now`, YAC_MASTER$`Post Code at Referral (or First NEPHYAC Appt)`)
  
  #Expand this to the deprivation decile at time of referral
  YAC_MASTER$`Deprivation Decile at Referral (or First NEPHYAC appt)`<-ifelse(is.na(YAC_MASTER$`Deprivation Decile at Referral (or First NEPHYAC appt)`), YAC_MASTER$`Deprivation Decile Now`, YAC_MASTER$`Deprivation Decile at Referral (or First NEPHYAC appt)`)
  
  #Update missing age data
  YAC_MASTER$`Age at Referral`<-round(((YAC_MASTER$`First NEPHYAC Appt`-YAC_MASTER$`Date of Birth`)/365.25), digits=1)
  YAC_MASTER$`Age at Referral`<-as.numeric(as.character(YAC_MASTER$`Age at Referral`))
  
  #Time difference from first renal OPD to YAC
    #Function to calculate this in months
    elapsed_months <- function(end_date, start_date) {
      ed <- as.POSIXlt(end_date)
      sd <- as.POSIXlt(start_date)
      12 * (ed$year - sd$year) + (ed$mon - sd$mon)
    }
    #Apply this
    YAC_MASTER$Clinic_time_difference<-elapsed_months(YAC_MASTER$`First NEPHYAC Appt`, YAC_MASTER$`First Attended Renal Appt (any clinic)`)
    #Then convert negative numebrs to zero (i.e. seen in YAC first then t/f)
    #YAC_MASTER$Clinic_time_difference<-ifelse(YAC_MASTER$Clinc_time_difference<0, 0, YAC_MASTER$Clinc_time_difference)
    
# BASIC DESCRIPTIVES ------------------------------------------------------
  #Number of patients (in report)
  Num_patients<-nrow(YAC_MASTER)
    
  #Number of patients who have moved postcode during f/u
  YAC_MASTER$Change<-ifelse(YAC_MASTER$`Post Code at Referral (or First NEPHYAC Appt)`==YAC_MASTER$`Post Code Now`,0,1)
  Change_postcode<-sum(YAC_MASTER$Change)
  
  #Number of patients who changed their deprivation decile when moving
  Change<-subset(YAC_MASTER, YAC_MASTER$Change=="1")
    Change$Deprivation_change<-ifelse(Change$`Deprivation Decile at Referral (or First NEPHYAC appt)`==Change$`Deprivation Decile Now`, "No change", ifelse(Change$`Deprivation Decile at Referral (or First NEPHYAC appt)`<Change$`Deprivation Decile Now`, "INCREASE", "DECREASE"))
    Change_decile_table<-table(Change$Deprivation_change)
    
  #Number of patients who have not had a YAC attendance (in report)
  Never_attented<-sum(is.na(YAC_MASTER$`First Attended NEPHYAC Appt`))
  
  #Number of patients who have attended a renal clinic prior to YAC (in report)
  Attended_df<-filter(YAC_MASTER, Clinic_time_difference>0) %>% summarise(Clinic_time_difference=n())
  Attended_before<-Attended_df$Clinic_time_difference
  
  #Number of patients with no previous renal apt and no YAC (in report)
  Never_attented_clinic_before<-sum(is.na(YAC_MASTER$Clinic_time_difference))
  
  #Number of patients who  attended an other renal clinic following coming to YAC (in report)
  Further_clinic_df<-filter(YAC_MASTER, Clinic_time_difference<0) %>% summarise(Clinic_time_difference=n())
  Further_clinic<-Further_clinic_df$Clinic_time_difference
  
  #Age (in report)
  Mean.age<-round(mean(YAC_MASTER$`Age at Referral`), digits=1)
  SD.age<-round(sd(YAC_MASTER$`Age at Referral`), digits=1)
  Median.age<-round(median(YAC_MASTER$`Age at Referral`), digits=1)
  
  
  
  
  
  
  

# OLD MAPPING -------------------------------------------------------------
  list <- as.list(unique(YAC_MASTER$`Post Code at Referral (or First NEPHYAC Appt)`), na.rm=T)
  datamap <- subset(postcodes, postcodes$Postcode %in% list, select= c("Postcode", "Latitude",  "Longitude"))  
  row.names(datamap) <- 1:nrow(datamap)
  
  datamap_mat<- cbind(datamap$Longitude,datamap$Latitude)
  row.names(datamap_mat) <- 1:nrow(datamap_mat)
  AACRS <- CRS("+proj=longlat +ellps=WGS84")
  
  UK_Map <- SpatialPointsDataFrame(datamap_mat, datamap, proj4string = AACRS, match.ID = TRUE) 
  
  
  #Map Points on Googlemaps
  m <- plotGoogleMaps(UK_Map , filename='MAP_UK.html')
  
  
# OLD TRAJECTORY ETC ---------------------------------------------------------------------

#GFR TRAJECTORY DATA
#replace >90 with 90
  YAC_MASTER$`First Seen NEPHYAC eGFR`[YAC_MASTER$`First Seen NEPHYAC eGFR`==">90"]<-"90"
  YAC_MASTER$`First Seen NEPHYAC eGFR`<-as.numeric(as.character(YAC_MASTER$`First Seen NEPHYAC eGFR`))

  YAC_MASTER$`12 month eGFR`[YAC_MASTER$`12 month eGFR`==">90"]<-"90"
  YAC_MASTER$`12 month eGFR`<-as.numeric(as.character(YAC_MASTER$`12 month eGFR`))


  YAC_MASTER$`24 month eGFR`[YAC_MASTER$`24 month eGFR`==">90"]<-"90"
  YAC_MASTER$`24 month eGFR`<-as.numeric(as.character(YAC_MASTER$`24 month eGFR`))


  YAC_MASTER$`36 month eGFR`[YAC_MASTER$`36 month eGFR`==">90"]<-"90"
  YAC_MASTER$`36 month eGFR`<-as.numeric(as.character(YAC_MASTER$`36 month eGFR`))
  
  #REMOVE EGFR DATA OBTAINED AFTER START OF RRT
  #baseline
  YAC_MASTER$GFR_0<-ifelse(is.na(YAC_MASTER$`Start of Dialysis`), YAC_MASTER$`First Seen NEPHYAC eGFR`, 
                           ifelse(YAC_MASTER$`Start of Dialysis`>0 & YAC_MASTER$`First Seen NEPHYAC eGFRDate`>YAC_MASTER$`Start of Dialysis`,  
                                  "", YAC_MASTER$`First Seen NEPHYAC eGFR`))
  
  YAC_MASTER$GFR_0<-as.numeric(YAC_MASTER$GFR_0)
  
  #year 1
  YAC_MASTER$GFR_1<-ifelse(is.na(YAC_MASTER$`Start of Dialysis`), YAC_MASTER$`12 month eGFR`, 
                           ifelse(YAC_MASTER$`Start of Dialysis`>0 & YAC_MASTER$`12 month eGFR Date`>YAC_MASTER$`Start of Dialysis`,  
                                  "", YAC_MASTER$`12 month eGFR`)) 
  
  YAC_MASTER$GFR_1<-as.numeric(YAC_MASTER$GFR_1)
  
  #year 2
  YAC_MASTER$GFR_2<-ifelse(is.na(YAC_MASTER$`Start of Dialysis`), YAC_MASTER$`24 month eGFR`, 
                           ifelse(YAC_MASTER$`Start of Dialysis`>0 & YAC_MASTER$`24 month eGFR Date`>YAC_MASTER$`Start of Dialysis`,  
                                  "", YAC_MASTER$`24 month eGFR`))
  
  YAC_MASTER$GFR_2<-as.numeric(YAC_MASTER$GFR_2)
  
  #year 3
  YAC_MASTER$GFR_3<-ifelse(is.na(YAC_MASTER$`Start of Dialysis`), YAC_MASTER$`36 month eGFR`, 
                           ifelse(YAC_MASTER$`Start of Dialysis`>0 & YAC_MASTER$`36 month eGFR Date`>YAC_MASTER$`Start of Dialysis`,  
                                  "", YAC_MASTER$`36 month eGFR`)) 
  
  YAC_MASTER$GFR_3<-as.numeric(YAC_MASTER$GFR_3)
  
#create a single file and reshape to allow calculation of individual regression slopes
  YAC_Slope <- YAC_MASTER[, c(1,77,78,79,80)]
  
  #simplify column names for regression / reshaping
  colnames(YAC_Slope) <- c("Number", "0", "1", "2", "3")
  
  YAC_Slope_tidy <- gather(YAC_Slope, Year, GFR, -Number)
  YAC_Slope_tidy<-arrange(YAC_Slope_tidy, Number, Year)
  YAC_Slope_tidy$Year<-as.numeric(as.character(YAC_Slope_tidy$Year))
  
  #remove na values
  YAC_Slope_tidy<-subset(YAC_Slope_tidy, YAC_Slope_tidy$GFR >1)
  
  #add a slope column to the data frame
  Slope<-YAC_Slope_tidy %>%
    group_by(Number) %>% 
    do(mod = lm(GFR ~ Year, data = .)) %>%
    mutate(Slope = summary(mod)$coeff[2]) %>%
    select(-mod)
  
  YAC_Slope_tidy <-left_join(YAC_Slope_tidy, Slope)
  #convert into easy to digest integer values
  YAC_Slope_tidy$Slope<-round(YAC_Slope_tidy$Slope, digits=0)
  
  #sort to remove duplication and then NA values
  YAC_Slope_tidy_nodup<-YAC_Slope_tidy[!duplicated(YAC_Slope_tidy$Number),]
  
  Slope_list<-is.na(YAC_Slope_tidy_nodup$Slope)
  YAC_Slope_tidy_nodup<-subset(YAC_Slope_tidy_nodup, Slope_list==F)
  
#PSYCHOLOGY DATA
  #subset to look just at those who have had evidence of psychology input
  Psych_list<-is.na(YAC_MASTER$PsychologyDocument)
  Psychology<-subset(YAC_MASTER, Psych_list==F)
  
  #remove patients without a first date for YAC
  Psych_list<-is.na(Psychology$`First Attended NEPHYAC Appt`)
  Psychology<-subset(Psychology, Psych_list==F)
  
  #calculate time from first YAC to first psychology appointment
  Psychology$Psych_time<-Psychology$`First Psychology Document Since Referral To Renal`-Psychology$`First Attended NEPHYAC Appt`
  
  #remove patients seen by pschology team prior to YAC
  Psychology<-subset(Psychology, Psychology$Psych_time>0)
  
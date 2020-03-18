install.packages("dplyr")
install.packages("readxl")
install.packages("stringr")
install.packages("data.table")
library(dplyr)
library(readxl)
library(stringr)
library(data.table)

wd <- "C:/Users/danielem/FTS API R"
setwd(wd)

data <- read.csv("all.csv")
names(data) <- make.names(names(data))

#removing FTS flow status pledge
data2 <- subset(data, data$status %in% c('paid','commitment'))

#removing columns that will not be used
data2$createdAt <- NULL
data2$date <- NULL
data2$decisionDate <- NULL
data2$firstReportedDate <- NULL
data2$updatedAt <- NULL
data2$versionId <- NULL
data2$refCode <- NULL
data2$source_Organization_id <- NULL
data2$source_Location_id <- NULL
data2$source_UsageYear_id <- NULL
data2$destination_Organization_id <- NULL
data2$destination_GlobalCluster_id <- NULL
data2$destination_Location_id <- NULL
data2$destination_UsageYear_id <- NULL
data2$destination_Plan_id <- NULL
data2$destination_Project_id <- NULL
data2$destination_Plan_id <- NULL
data2$destination_Cluster_id <- NULL
data2$destination_Emergency_id <- NULL
data2$source_Emergency_id <- NULL
data2$source_GlobalCluster_id <- NULL
data2$source_Cluster_id <- NULL

#changing FTS column headers to allign with the code
if("source_Organization_name" %in% names(data2)){
  names(data2)[which(names(data2)=="source_Organization_name")] = "Donor"
}

if("destination_Organization_name" %in% names(data2)){
  names(data2)[which(names(data2)=="destination_Organization_name")] = "Recipient.Organization"
}

if("destination_Location_name" %in% names(data2)){
  names(data2)[which(names(data2)=="destination_Location_name")] = "Destination.Country"
}

#lower columns rows will be NULL before final output.
#The below function still works even if columns are not removed.

lowerConv <- function(x){
  return(tolower(iconv(x,"latin1","latin1")))
}

install.packages("plyr")
library(plyr)

# Remove government of
data2$Donor <- gsub(", Government of","",data2$Donor)
unique(data2$Donor)
data2$Recipient.Organization <- gsub(", Government of","",data2$Recipient.Organization)
unique(data2$Recipient.Organization)

#Replace with special characters
data2$Donor <- gsub("Ã¡","á",data2$Donor)
data2$Donor <- gsub("Ã¢","â",data2$Donor)
data2$Donor <- gsub("Ã¤","ä",data2$Donor)
data2$Donor <- gsub("Ã©","é",data2$Donor)
data2$Donor <- gsub("Ã«","ë",data2$Donor)
data2$Donor <- gsub("Ã³","ó",data2$Donor)
data2$Donor <- gsub("Ã´","ô",data2$Donor)
data2$Donor <- gsub("Ã¶","ö",data2$Donor)
data2$Donor <- gsub("Ãº","ú",data2$Donor)
data2$Donor <- gsub("Ã¼","ü",data2$Donor)
data2$Donor <- gsub("Äf","ã",data2$Donor)
data2$Donor <- gsub("ÄT","ê",data2$Donor)
data2$Donor <- gsub('Å"','ñ',data2$Donor)
data2$Donor <- gsub("Å^","ò",data2$Donor)
data2$Donor <- gsub("Å'","õ",data2$Donor)
data2$Donor <- gsub("Å.","à",data2$Donor)
data2$Donor <- gsub("Å>","o",data2$Donor)
data2$Donor <- gsub("Å¯","ù",data2$Donor)
data2$Donor <- gsub("Å±","û",data2$Donor)
data2$Donor <- gsub("Ã¨","è",data2$Donor)
data2$Donor <- gsub("Ã§","ç",data2$Donor)
data2$Donor <- gsub("Ã¸","ø",data2$Donor)
data2$Donor <- gsub('â???"','-',data2$Donor)
data2$Donor <- gsub("â???T","'",data2$Donor)
data2$Donor <- gsub("Ã®","î",data2$Donor)
data2$Donor <- gsub("Ã±","ñ",data2$Donor)
data2$Donor <- gsub("Ãª","ê",data2$Donor)
data2$Donor <- gsub("Ã???","Ç",data2$Donor)
data2$Donor <- gsub("Ã®","î",data2$Donor)
data2$Donor <- gsub("Ã±","ñ",data2$Donor)
data2$Donor <- gsub("â???T","'",data2$Donor)
data2$Donor <- gsub('â???"','-',data2$Donor)
data2$Donor <- gsub('Ã"','Ä',data2$Donor)
data2$Donor <- gsub("Ã","í",data2$Donor)
data2$Donor <- gsub("Ã¯","ï",data2$Donor)
data2$Donor <- gsub("í£","ã",data2$Donor)
data2$Donor <- gsub("í¯","ï",data2$Donor)
data2$Donor <- gsub("í¦","æ",data2$Donor)
data2$Donor <- gsub("í¥","å",data2$Donor)

data2$Recipient.Organization <- gsub("Ã¡","á",data2$Recipient.Organization)
data2$Recipient.Organization <- gsub("Ã¢","â",data2$Recipient.Organization)
data2$Recipient.Organization <- gsub("Ã¤","ä",data2$Recipient.Organization)
data2$Recipient.Organization <- gsub("Ã©","é",data2$Recipient.Organization)
data2$Recipient.Organization <- gsub("Ã«","ë",data2$Recipient.Organization)
data2$Recipient.Organization <- gsub("Ã³","ó",data2$Recipient.Organization)
data2$Recipient.Organization <- gsub("Ã´","ô",data2$Recipient.Organization)
data2$Recipient.Organization <- gsub("Ã¶","ö",data2$Recipient.Organization)
data2$Recipient.Organization <- gsub("Ãº","ú",data2$Recipient.Organization)
data2$Recipient.Organization <- gsub("Ã¼","ü",data2$Recipient.Organization)
data2$Recipient.Organization <- gsub("Äf","ã",data2$Recipient.Organization)
data2$Recipient.Organization <- gsub("ÄT","ê",data2$Recipient.Organization)
data2$Recipient.Organization <- gsub('Å"','ñ',data2$Recipient.Organization)
data2$Recipient.Organization <- gsub("Å^","ò",data2$Recipient.Organization)
data2$Recipient.Organization <- gsub("Å'","õ",data2$Recipient.Organization)
data2$Recipient.Organization <- gsub("Å.","à",data2$Recipient.Organization)
data2$Recipient.Organization <- gsub("Å>","o",data2$Recipient.Organization)
data2$Recipient.Organization <- gsub("Å¯","ù",data2$Recipient.Organization)
data2$Recipient.Organization <- gsub("Å±","û",data2$Recipient.Organization)
data2$Recipient.Organization <- gsub("Ã¨","è",data2$Recipient.Organization)
data2$Recipient.Organization <- gsub("Ã§","ç",data2$Recipient.Organization)
data2$Recipient.Organization <- gsub("Ã¸","ø",data2$Recipient.Organization)
data2$Recipient.Organization <- gsub('â???"','-',data2$Recipient.Organization)
data2$Recipient.Organization <- gsub("â???T","'",data2$Recipient.Organization)
data2$Recipient.Organization <- gsub("Ã®","î",data2$Recipient.Organization)
data2$Recipient.Organization <- gsub("Ã±","ñ",data2$Recipient.Organization)
data2$Recipient.Organization <- gsub("Ãª","ê",data2$Recipient.Organization)
data2$Recipient.Organization <- gsub("Ã???","Ç",data2$Recipient.Organization)
data2$Recipient.Organization <- gsub("Ã®","î",data2$Recipient.Organization)
data2$Recipient.Organization <- gsub("Ã±","ñ",data2$Recipient.Organization)
data2$Recipient.Organization <- gsub("â???T","'",data2$Recipient.Organization)
data2$Recipient.Organization <- gsub('â???"','-',data2$Recipient.Organization)
data2$Recipient.Organization <- gsub('Ã"','Ä',data2$Recipient.Organization)
data2$Recipient.Organization <- gsub("Ã","í",data2$Recipient.Organization)
data2$Recipient.Organization <- gsub("Ã¯","ï",data2$Recipient.Organization)
data2$Recipient.Organization <- gsub("í£","ã",data2$Recipient.Organization)
data2$Recipient.Organization <- gsub("í¯","ï",data2$Recipient.Organization)
data2$Recipient.Organization <- gsub("í¦","æ",data2$Recipient.Organization)
data2$Recipient.Organization <- gsub("í¥","å",data2$Recipient.Organization)

#Merge to create new column "codename", "recipientcodename1", "incomegroups" and "odaeligible" based on donor type
codenames <- read.csv("Previous year_coding lists/codename.csv",na.strings="",as.is=TRUE)
data2 <- merge(data2, codenames, by.x = "Donor", by.y = "Donor", all.x = TRUE, all.y = FALSE)

recipientcodename1 <- read.csv("Previous year_coding lists/recipientcodename1.csv",na.strings="",as.is=TRUE)
data2 <- merge(data2, recipientcodename1, by.x = "Recipient.Organization", by.y = "Recipient.Organization", all.x = TRUE, all.y = FALSE)

incomegroups <- read.csv("Previous year_coding lists/incomegroups.csv",na.strings="",as.is=TRUE)
data2 <- merge(data2, incomegroups, by.x = "Destination.Country", by.y = "Destination.Country", all.x = TRUE, all.y = FALSE)

odaeligible <- read.csv("Previous year_coding lists/odaeligible.csv",na.strings="",as.is=TRUE)
data2 <- merge(data2, odaeligible, by.x = "Destination.Country", by.y = "Destination.Country", all.x = TRUE, all.y = FALSE)

#Select donors without codename, privatemoney and dacregion
codenames2 <- subset(data2, data2$codename %in% NA, select = c(Donor, codename))

recipientcodename2 <- subset(data2, data2$recipientcodename %in% NA, select = c(Recipient.Organization, recipientcodename))

incomegroups2 <- subset(data2, data2$incomegroup %in% NA, select = c(Destination.Country, incomegroup))

odaeligible2 <- subset(data2, data2$ODA.eligible %in% NA, select = c(Destination.Country, ODA.eligible))

#Removing repeated rows in the codename2, privatemoney2 and dacregion2
library(dplyr)

codenames2 <- distinct(codenames2,Donor, .keep_all = TRUE)
codenamesfinal <- subset(codenames2, select = c (1,2))

recipientcodename2 <- distinct(recipientcodename2,Recipient.Organization, .keep_all = TRUE)
recipientcodenamefinal <- subset(recipientcodename2, select = c (1,2))

incomegroups2 <- distinct(incomegroups2,Destination.Country, .keep_all = TRUE)
incomegroupsfinal <- subset(incomegroups2, select = c (1,2))

odaeligible2 <- distinct(odaeligible2,Destination.Country, .keep_all = TRUE)
odaeligiblefinal <- subset(odaeligible2, select = c (1,2))

#exporting file as CSV
write.csv(codenamesfinal,"New coding lists/codenamesfinal.csv",na="",row.names=FALSE)

write.csv(recipientcodenamefinal,"New coding lists/recipientcodenamefinal.csv",na="",row.names=FALSE)

write.csv(incomegroupsfinal,"New coding lists/incomegroupsfinal.csv",na="",row.names=FALSE)

write.csv(odaeligiblefinal,"New coding lists/odaeligiblefinal.csv",na="",row.names=FALSE)

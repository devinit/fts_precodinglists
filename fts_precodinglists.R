list.of.packages <- c("dplyr","readxl","stringr","data.table","plyr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

wd <- "C:/Users/danielem/FTS API R"
setwd(wd)

data <- read.csv("all.csv")
names(data) <- make.names(names(data))

#removing FTS flow status pledge
data2 <- subset(data, data$status %in% c('paid','commitment'))

#removing columns that will not be used
columns_to_remove = c(
  "createdAt", "date", "decisionDate",
  "firstReportedDate", "updatedAt", "versionId",
  "refCode", "source_Organization_id", "source_Location_id",
  "source_UsageYear_id", "destination_Organization_id", "destination_GlobalCluster_id",
  "destination_Location_id", "destination_UsageYear_id", "destination_Plan_id",
  "destination_Project_id", "destination_Plan_id", "destination_Cluster_id",
  "destination_Emergency_id", "source_Emergency_id", "source_GlobalCluster_id",
  "source_Cluster_id"
)

data2[,columns_to_remove] <- NULL

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

# Remove government of
data2$Donor <- gsub(", Government of","",data2$Donor)
unique(data2$Donor)
data2$Recipient.Organization <- gsub(", Government of","",data2$Recipient.Organization)
unique(data2$Recipient.Organization)

#Replace with special characters
character_replacements = list(
  c("Ã¡","á"), c("Ã¢","â"), c("Ã¤","ä"),
  c("Ã©","é"), c("Ã«","ë"), c("Ã³","ó"),
  c("Ã´","ô"), c("Ã¶","ö"), c("Ãº","ú"),
  c("Ã¼","ü"), c("Äf","ã"), c("ÄT","ê"),
  c('Å"','ñ'), c("Å^","ò"), c("Å'","õ"),
  c("Å.","à"), c("Å>","o"), c("Å¯","ù"),
  c("Å±","û"), c("Ã¨","è"), c("Ã§","ç"),
  c("Ã¸","ø"), c('â???"','-'), c("â???T","'"),
  c("Ã®","î"), c("Ã±","ñ"), c("Ãª","ê"),
  c("Ã???","Ç"), c("Ã®","î"), c("Ã±","ñ"),
  c("â???T","'"), c('â???"','-'), c('Ã"','Ä'),
  c("Ã","í"), c("Ã¯","ï"), c("í£","ã"),
  c("í¯","ï"), c("í¦","æ"), c("í¥","å")
)

for(character_replacement in character_replacements){
  from_character = character_replacement[1]
  to_character = character_replacement[2]
  data2$Donor <- gsub(
    from_character,
    to_character,
    data2$Donor
  )
  data2$Recipient.Organization <- gsub(
    from_character,
    to_character,
    data2$Recipient.Organization
  )
}

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

list.of.packages <- c("dplyr","readxl","data.table","plyr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)
library("plyr")
library("dplyr")
library("readxl")
library("data.table")

wd <- "C:/Users/danielem/Desktop/FTS API R"
setwd(wd)

data <- read.csv("all.csv")
names(data) <- make.names(names(data))

#removing FTS flow status pledge
data <- subset(data, data$status %in% c('paid','commitment'))

#select columns that will be used
data <- subset(data, select = c("id","amountUSD","budgetYear","description",
                                "flowType","newMoney","originalAmount","originalCurrency",
                                "method","status","boundary","onBoundary","source_Organization_name",
                                "source_Location_name","source_UsageYear_name","destination_Organization_name",
                                "destination_GlobalCluster_name","destination_Location_name","destination_UsageYear_name",
                                "destination_Plan_name","destination_Project_name","parentFlowId","grandBargainEarmarkingType",
                                "source_Plan_id","source_Plan_name","destination_Cluster_name","destination_Emergency_name",
                                "exchangeRate","source_Emergency_name","source_GlobalCluster_name"))

#changing FTS column headers to allign with the code
if("source_Organization_name" %in% names(data)){
  names(data)[which(names(data)=="source_Organization_name")] = "Donor"
}

if("destination_Organization_name" %in% names(data)){
  names(data)[which(names(data)=="destination_Organization_name")] = "Recipient.Organization"
}

if("destination_Location_name" %in% names(data)){
  names(data)[which(names(data)=="destination_Location_name")] = "Destination.Country"
}

#lower columns rows will be NULL before final output.
#The below function still works even if columns are not removed.

lowerConv <- function(x){
  return(tolower(iconv(x,"latin1","latin1")))
}

# Remove government of
data$Donor <- gsub(", Government of","",data$Donor)
unique(data$Donor)
data$Recipient.Organization <- gsub(", Government of","",data$Recipient.Organization)
unique(data$Recipient.Organization)

#Replace with special characters
character_replacements = list(
  c("Ã¡","á"), c("Ã¢","â"), c("Ã¤","ä"),
  c("Ã©","é"), c("Ã«","ë"), c("Ã³","ó"),
  c("Ã´","ô"), c("Ã¶","ö"), c("Ãº","ú"),
  c("Ã¼","ü"), c("Äf","ã"), c("ÄT","ê"),
  c('Å"','ñ'), c("Å^","ò"), c("Å'","õ"),
  c("Å.","à"), c("Å>","o"), c("Å¯","ù"),
  c("Å±","û"), c("Ã¨","è"), c("Ã§","ç"),
  c("Ã¸","ø"), c('â???"','-'), c("â???T","’"),
  c("Ã®","î"), c("Ã±","ñ"), c("Ãª","ê"),
  c("Ã???","Ç"), c("Ã®","î"), c("Ã±","ñ"),
  c('â???"','-'), c('Ã"','Ä'),
  c("Ã","í"), c("Ã¯","ï"), c("í£","ã"),
  c("í¯","ï"), c("í¦","æ"), c("í¥","å"),
  c("â???T","'"),c('â???"',"-"), c("í¦","à"),
  c('Å"',"o"), c("Å,","à"), c('Å"',"à"),
  c('â???"','-'),c("â???T","'"),c('â???"',"-"),
  c('â???T',"'"), c('Å"',"à"), c('â€“',"-"),
  c('â€™',"’")
)

for(character_replacement in character_replacements){
  from_character = character_replacement[1]
  to_character = character_replacement[2]
  data$Donor <- gsub(
    from_character,
    to_character,
    data$Donor,
    ignore.case = FALSE, perl = FALSE,
    fixed = TRUE, useBytes = FALSE
  )
  data$Recipient.Organization <- gsub(
    from_character,
    to_character,
    data$Recipient.Organization,
    ignore.case = FALSE, perl = FALSE,
    fixed = TRUE, useBytes = FALSE
  )
}

#Merge to create new column "Code name" based on donor name
codename <- read.csv("Final lists coded/codenames2020.csv",na.strings="",as.is=TRUE)
codename$lower.Donor <- lowerConv(codename$Donor)
codename <- codenames[!duplicated(codename$Donor),]
codename$Donor <- NULL
data$lower.Donor <- lowerConv(data$Donor)
data$lower.Recipient.Organization <- lowerConv(data$Recipient.Organization)

data <- join(data, codename, by='lower.Donor', type='left', match='first')

withoutCodename <- subset(data,is.na(codename))
unique(withoutCodename$Donor)

#Merge to create new column "Private money" based on donor name

privatemoney <- read.csv("Final lists coded/privatemoney2020.csv",na.strings="",as.is=TRUE)
privatemoney$lower.Donor <- lowerConv(privatemoney$Donor)
privatemoney <- privatemoney[!duplicated(privatemoney$Donor),]
privatemoney$Donor <- NULL
data <- join(data, privatemoney, by='lower.Donor', type='left', match='first')

withoutPrivate <- subset(data,is.na(privatemoney))
unique(withoutPrivate$Donor)

#Merge to create new column "Donor DAC region" based on donor name
donordacregion <- read.csv("Final lists coded/dacregion2020.csv",na.strings="",as.is=TRUE)
donordacregion$lower.Donor <- lowerConv(donordacregion$Donor)
donordacregion <- donordacregion[!duplicated(donordacregion$Donor),]
donordacregion$Donor <- NULL
data <- join(data, donordacregion, by='lower.Donor', type='left', match='first')

withoutDACRegion <- subset(data,is.na(donordacregion))
unique(withoutDACRegion$Donor)

#Merge to create new column "Donor Country ID" based on donor name
donorscountryid <- read.csv("Final lists coded/donorscountryid2020.csv",na.strings="",as.is=TRUE)
donorscountryid$lower.Donor <- lowerConv(donorscountryid$Donor)
donorscountryid <- donorscountryid[!duplicated(donorscountryid$Donor),]
donorscountryid$Donor <- NULL
data <- join(data, donorscountryid, by='lower.Donor', type='left', match='first')

withoutDonorID <- subset(data,is.na(donorscountryid))
unique(withoutDonorID$Donor)

#Merge to create new column "Appealing agency code name" based on recipient organisation name
recipientcodename <- read.csv("Final lists coded/recipientcodename2020.csv",na.strings="",as.is=TRUE)
recipientcodename$lower.Recipient.Organization <- lowerConv(recipientcodename$Recipient.Organization)
recipientcodename <- recipientcodename[!duplicated(recipientcodename$Recipient.Organization),]
recipientcodename$Recipient.Organization <- NULL 
data <- join(data, recipientcodename, by='lower.Recipient.Organization', type='left', match='first')

withoutRecipientcode <- subset(data,is.na(recipientcodename))
unique(withoutRecipientcode$Recipient.Organization)

#Merge to create new column "Recip Org NGO type" based on recipient organisation name
ngotype <- read.csv("Final lists coded/ngotype2020.csv",na.strings="",as.is=TRUE)
ngotype$lower.Recipient.Organization <- lowerConv(ngotype$Recipient.Organization)
ngotype <- ngotype[!duplicated(ngotype$Recipient.Organization),]
ngotype$Recipient.Organization <- NULL
data <- join(data, ngotype, by='lower.Recipient.Organization', type='left', match='first')

withoutngos <- subset(data,is.na(ngotype))
unique(withoutngos$Recipient.Organization)

#Merge to create new column "Channels of delivery" based on recipient organisation name
deliverychannels <- read.csv("Final lists coded/deliverychannels2020.csv",na.strings="",as.is=TRUE)
deliverychannels$lower.Recipient.Organization <- lowerConv(deliverychannels$Recipient.Organization)
deliverychannels <- deliverychannels[!duplicated(deliverychannels$Recipient.Organization),]
deliverychannels$Recipient.Organization <- NULL
data <- join(data, deliverychannels, by='lower.Recipient.Organization', type='left', match='first')

withoutchannels <- subset(data,is.na(deliverychannels))
unique(withoutchannels$Recipient.Organization)

#Merge to create new column "Recipient country ID" based on recipient organisation name
recipientcountryid <- read.csv("Final lists coded/recipientcountryid2020.csv",na.strings="",as.is=TRUE)
recipientcountryid$lower.Recipient.Organization <- lowerConv(recipientcountryid$Recipient.Organization)
recipientcountryid <- recipientcountryid[!duplicated(recipientcountryid$Recipient.Organization),]
recipientcountryid$Recipient.Organization <- NULL
data <- join(data, recipientcountryid, by='lower.Recipient.Organization', type='left', match='first')

withoutRecipientID <- subset(data,is.na(recipientcountryid))
unique(withoutRecipientID$Recipient.Organization)

#Merge to create new column "ODA eligible" based on destination country
odaeligible <- read.csv("Final lists coded/odaeligible2020.csv",na.strings="",as.is=TRUE)
data$lower.Destination.Country <- lowerConv(data$Destination.Country)
odaeligible$lower.Destination.Country <- lowerConv(odaeligible$Destination.Country)
odaeligible <- odaeligible[!duplicated(odaeligible$Destination.Country),]
odaeligible$Destination.Country <- NULL 
data <- join(data, odaeligible, by='lower.Destination.Country', type='left', match='all')

withoutincome <- subset(data,is.na(odaeligible))
unique(withoutincome$Destination.Country)

#Merge to create new column "Destination country ID" based on destination country
destinationcountryid <- read.csv("Final lists coded/destinationcountryid2020.csv",na.strings="",as.is=TRUE)
data$lower.Destination.Country <- lowerConv(data$Destination.Country)
destinationcountryid$lower.Destination.Country <- lowerConv(destinationcountryid$Destination.Country)
destinationcountryid <- destinationcountryid[!duplicated(destinationcountryid$Destination.Country),]
destinationcountryid$Destination.Country <- NULL 
data <- join(data, destinationcountryid, by='lower.Destination.Country', type='left', match='all')

withoutDestinationcountryID <- subset(data,is.na(destinationcountryid))
unique(withoutDestinationcountryID$Destination.Country)

#Create new column "Destination country type"
data$destinationcountrytype <- paste(data$destinationcountryid,data$source_UsageYear_name)
data$destinationcountrytype[is.na(data$destinationcountrytype)] <- FALSE

#Merge to create new column "Income group" based on destination country
incomegroups <- read.csv("Final lists coded/incomegroups2020.csv",na.strings="",as.is=TRUE)
data$lower.destinationcountrytype <- lowerConv(data$destinationcountrytype)
incomegroups$lower.destinationcountrytype <- lowerConv(incomegroups$destinationcountrytype)
incomegroups <- incomegroups[!duplicated(incomegroups$destinationcountrytype),]
incomegroups$destinationcountrytype <- NULL 
data <- join(data, incomegroups, by='lower.destinationcountrytype', type='left', match='all')

withoutdestinationtype <- subset(data,is.na(incomegroups))
unique(withoutincome$destinationcountrytype)

#Create new column "Domestic" 
data$domesticresponse <- ifelse(data$donorcountryid==data$destinationcountryid,TRUE,FALSE)
data$domesticresponse[is.na(data$domesticresponse)] <- FALSE

#Create new column "Deflator type"
data$deflatortype <- paste(data$donorcountryid,data$source_UsageYear_name)
data$deflatortype[is.na(data$deflatortype)] <- FALSE

#Merge to create new column "Deflator" based on Deflator type
deflators <- read.csv("Final lists coded/deflators2020.csv",na.strings="",as.is=TRUE)
data <- join(data, deflators, by='deflatortype', type='left', match='all')
data <- transform(data,amountDeflated=as.numeric(amountUSD)/as.numeric(Deflators))
data <- transform(data,amountDeflatedMillions=amountDeflated/1000000)

withoutdeflators <- subset(data,is.na(deflators))
unique(withoutdeflators$Donor)

#Remove deflatortype column
data$deflatortype <- NULL

#Remove destinationcountrytype column
data$lower.destinationcountrytype <- NULL
data$destinationcountrytype <- NULL

#Remove lower.deflatortype column
data$lower.deflatortype <- NULL

#Remove lower.Donor column
data$lower.Donor <- NULL

#Remove lower.recipient column
data$lower.Recipient.Organization <- NULL

#Remove lower.Destination.Country column
data$lower.Destination.Country <- NULL

write.csv(data,"finalcoded2.csv",na="",row.names=FALSE)

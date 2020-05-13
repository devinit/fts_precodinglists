#fts_precodinglists
The fts_finalcode.R is used to merge the FTS database extracted from API with the CSV files that contains new variables to be added to the FTS database. 

#Prepare and clean the the database before merge the csv files
We select only the appropriate flows from status ('paid' and 'commitment' contributions, excluding the pledges).

```R
data <- subset(data, data$status %in% c('paid','commitment'))
```

We also select the columns from the original database that is required in the final visualisation of the database

```R
data <- subset(data, select = c("id","amountUSD","budgetYear","description",
                                "flowType","newMoney","originalAmount","originalCurrency",
                                "method","status","boundary","onBoundary","source_Organization_name",
                                "source_Location_name","source_UsageYear_name","destination_Organization_name",
                                "destination_GlobalCluster_name","destination_Location_name","destination_UsageYear_name",
                                "destination_Plan_name","destination_Project_name","parentFlowId","grandBargainEarmarkingType",
                                "source_Plan_id","source_Plan_name","destination_Cluster_name","destination_Emergency_name",
                                "exchangeRate","source_Emergency_name","source_GlobalCluster_name"))
```

Rename the source_Organization_name, destination_Organization_name and destination_Location_name to Donor, Recipient.Organization and Destination.Country, respectively. The cvs files will be join to the database by Donor, Recipient.Organization and Destination.Country.

```R
if("source_Organization_name" %in% names(data)){
  names(data)[which(names(data)=="source_Organization_name")] = "Donor"
}
```

Remove ", Government of" of Donor and Recipient.Organization columns

```R
data$Donor <- gsub(", Government of","",data$Donor)
unique(data$Donor)
```
Substitute the special to the foreign characters from Donor and Recipient.Organization columns

```R
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
  c('â???"','-'), c('Ã"','Ä'),
  c("Ã","í"), c("Ã¯","ï"), c("í£","ã"),
  c("í¯","ï"), c("í¦","æ"), c("í¥","å"),
  c("â???T","'"),c('â???"',"-"), c("í¦","à"),
  c('Å"',"o"), c("Å,","à"), c('Å"',"à"),
  c('â???"','-'),c("â???T","'"),c('â???"',"-"),
  c('â???T',"'"), c('Å"',"à"), c('â???"',"-"),
  c('â???T',"'")
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
```

#Join the CVS to the FTS database

The following CVS files are linked to the FTS database by the Donor column: codenames2020, privatemoney2020, dacregion2020 and donorscountryid2020.
The following CVS files are linked to the FTS database by the Recipient.Organization column: recipientcodename2020, ngotype2020, deliverychannels2020 and recipientcountryid2020.
The following CVS files are linked to the FTS database by the Destination.Country column: odaeligible2020, destinationcountryid2020 and incomegroups2020.

```R
ngotype <- read.csv("Final lists coded/ngotype2020.csv",na.strings="",as.is=TRUE)
ngotype$lower.Recipient.Organization <- lowerConv(ngotype$Recipient.Organization)
ngotype <- ngotype[!duplicated(ngotype$Recipient.Organization),]
ngotype$Recipient.Organization <- NULL
data <- join(data, ngotype, by='lower.Recipient.Organization', type='left', match='first')

withoutngos <- subset(data,is.na(ngotype))
unique(withoutngos$Recipient.Organization)
```

#create extra columns based on the existing final columns

Domestic response is TRUE when donor country id is equal to the destination country id. When both columns are balnk, I consider it as non-domestic response (FALSE)

```R
data$domesticresponse <- ifelse(data$donorcountryid==data$destinationcountryid,TRUE,FALSE)
data$domesticresponse[is.na(data$domesticresponse)] <- FALSE
```

Create the column Deflator type merging the column Source_Usage_Year_name and donor country id

```R
data$deflatortype <- paste(data$donorcountryid,data$source_UsageYear_name)
data$deflatortype[is.na(data$deflatortype)] <- FALSE
```

#Merge deflator file and deflate amount USD values
The deflators2020 (csv file) is linked to the deflatortype column and the calculation are conduct according to the following code line:

```R
deflators <- read.csv("Final lists coded/deflators2020.csv",na.strings="",as.is=TRUE)
data <- join(data, deflators, by='deflatortype', type='left', match='all')
data <- transform(data,amountDeflated=as.numeric(amountUSD)/as.numeric(Deflators))
data <- transform(data,amountDeflatedMillions=amountDeflated/1000000)

withoutdeflators <- subset(data,is.na(deflators))
unique(withoutdeflators$Donor)
```

#Exclude the columns that are not necessary in the final file

Exclude the following columns: deflatortype, lower.destinationcountrytype, destinationcountrytype, lower.deflatortype, lower.Donor and lower.Recipient.Organization.



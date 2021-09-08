

#function subdivision_of_countries merges regions, sub-regions,
#iso2, iso3 information to given list_of_countries

subdivision_of_countries <- function(list_of_countries){
#information about regions
regions <- as.data.frame(data.table::fread("https://raw.githubusercontent.com/lukes/ISO-3166-Countries-with-Regional-Codes/master/all/all.csv"))
head(regions)
regions <- regions[,c(1,2,3,6,7)]
colnames(regions)[1] <- "Country"
colnames(regions)[2] <- "iso2"
colnames(regions)[3] <- "iso3"


#information about iso2, iso3
iso_internal <- as.data.frame(data.table::fread("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/UID_ISO_FIPS_LookUp_Table.csv"))
head(iso_internal)
iso_internal <- iso_internal[,c("iso2","iso3","Country_Region", "Province_State")]
iso_internal$Country_Prov <- trimws(paste(iso_internal$"Country_Region", iso_internal$"Province_State"),"r")
iso_internal <- iso_internal[,-4]
colnames(iso_internal)[3] <- "Country"




Countries_info <- as.data.frame(cbind(list_of_countries))
colnames(Countries_info) <- c("Country_Prov")


#merge iso2, iso3
Countries_info_1 <- merge(iso_internal,Countries_info,by=c("Country_Prov"), all.y = T)
dim(Countries_info_1)

#merge regions and subregions
Countries_info_2 <- merge(regions,Countries_info_1,by=c("iso2","iso3"), all.y = T)

Countries_info_2 <- Countries_info_2[,-3]
colnames(Countries_info_2)[6] <- "Country"



Countries_info_2 <- Countries_info_2[-which(Countries_info_2$region %in% NA),] 
return(Countries_info_2)
}

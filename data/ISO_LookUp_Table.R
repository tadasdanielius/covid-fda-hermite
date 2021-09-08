
ISO_LookUp_Table <- function(){

#information about regions
regions <- as.data.frame(data.table::fread("https://raw.githubusercontent.com/lukes/ISO-3166-Countries-with-Regional-Codes/master/all/all.csv"))
head(regions)
colnames(regions)[2] <- "iso2"
colnames(regions)[3] <- "iso3"

#249 locations
#dim(regions)

#information about iso2, iso3
iso_internal <- as.data.frame(data.table::fread("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/UID_ISO_FIPS_LookUp_Table.csv"))
head(iso_internal)
#iso_internal$jhu_cid <- trimws(paste(iso_internal$"Country_Region", iso_internal$"Province_State"),"r")
#4196 locations
#dim(iso_internal)
iso_internal_onlyc <- iso_internal[which(iso_internal$Province_State == ""),]
#dim(iso_internal_onlyc)

Countries_info_1 <- merge(regions,iso_internal_onlyc,by=c("iso2","iso3"), all=T)


#Owid and JHU country level info
owid_jhu_countries <- as.data.frame(data.table::fread("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/jhu/locations.csv"))
# head(owid_jhu_countries)
# dim(owid_jhu_countries)

#
Countries_info_2 <- merge(owid_jhu_countries,Countries_info_1,by.x=c("Country/Region"), by.y=c("Country_Region"), all.x=T)
Countries_info_2 <- Countries_info_2[,-which(colnames(Countries_info_2) %in% "Population")]
# dim(Countries_info_2)
return(Countries_info_2)
}

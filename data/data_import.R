


cases_per_pop = 100000

loc_table_owid <- as.data.frame(data.table::fread("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/jhu/locations.csv"))
head(loc_table_owid)

pop <- loc_table_owid[, c("location","population")]


################################################################
world <- c("World")
continents <- c("Africa", "Asia", "Europe", "North America", "South America", "Oceania")


####### vaccinated ################################################
vac_table_owid <- as.data.frame(data.table::fread("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/vaccinations.csv"))
head(vac_table_owid)

vac_table_owid <- merge(pop,vac_table_owid,by=c("location"), all.y=T)
vac_table_owid$vacc_normalized = vac_table_owid$people_vaccinated / (vac_table_owid$population/cases_per_pop)

#not countries
not_countries_list <- unique(vac_table_owid[which(grepl("^OWID", vac_table_owid$iso_code)), c("location", "iso_code")])
not_countries_list <- rbind(not_countries_list, c("2020 Summer Olympics athletes & staff", "NA"))

vac_table_C <- vac_table_owid[which(!(vac_table_owid$location %in% not_countries_list$location)),]

vac_table_C <- vac_table_C[order(vac_table_C$location, vac_table_C$date),]

##### Totals ######
#vaccinated countries
vac_tmp <- vac_table_C[,c("people_vaccinated", "location", "date")]
suppressWarnings({vac_tmp[,"people_vaccinated"] <- as.integer(vac_tmp[,"people_vaccinated"])})

date_range <- seq(min(vac_tmp[,"date"]), max(vac_tmp[,"date"]), by=1)
unq_countries <- unique(vac_tmp[,"location"])

vacc_countries <- matrix(NA, nrow=length(unique(vac_tmp[,"location"])), ncol=length(date_range))
colnames(vacc_countries) <- as.character(date_range)
rownames(vacc_countries) <- unq_countries

for(cntry_j in 1:length(unq_countries)){
  
  tmp_data <- vac_tmp %>% filter(location==  unq_countries[cntry_j])
  dt <- data.frame("people_vaccinated" = tmp_data[,"people_vaccinated"])
  
  tmp_data[,"people_vaccinated"] <- as.integer(nafill(dt, type = "locf")[[1]])
  tmp_location <- which(rownames(vacc_countries) %in% unq_countries[cntry_j])
  for(i in 1:length(tmp_data$date)){
    vacc_countries[tmp_location,which(colnames(vacc_countries) %in% as.character(tmp_data$date[i]))] <- tmp_data$people_vaccinated[i]
  }
}
vacc_countries <- as.data.frame(vacc_countries)


# matplot(t(vacc_countries), type="l")


#vaccinated world
vacc_world0 <- vac_table_owid[which(vac_table_owid$location %in% world),c("people_vaccinated", "location", "date")]
vacc_world <- t(vacc_world0$people_vaccinated)
colnames(vacc_world) <- as.character(vacc_world0$date)
rownames(vacc_world) <- unique(vacc_world0$location)

#vaccinated continents
vacc_cont <- vac_table_owid[which(vac_table_owid$location %in% continents),c("people_vaccinated", "location", "date")]
suppressWarnings({vacc_cont[,"people_vaccinated"] <- as.integer(vacc_cont[,"people_vaccinated"])})
date_range <- seq(min(vacc_cont[,"date"]), max(vacc_cont[,"date"]), by=1)


vac_continents <- matrix(NA, nrow=length(unique(vacc_cont[,"location"])), ncol=length(date_range))
colnames(vac_continents) <- as.character(date_range)
rownames(vac_continents) <- continents

for(cont_j in 1:length(continents)){
  tmp_data <- vacc_cont %>% filter(location==  continents[cont_j])
  dt <- data.frame("people_vaccinated" = tmp_data[,"people_vaccinated"])
  
  tmp_data[,"people_vaccinated"] <- as.integer(nafill(dt, type = "locf")[[1]])
  tmp_location <- which(rownames(vac_continents) %in% continents[cont_j])
  for(i in 1:length(tmp_data$date)){
    vac_continents[tmp_location,which(colnames(vac_continents) %in% as.character(tmp_data$date[i]))] <- tmp_data$people_vaccinated[i]
  }
}
vac_continents[is.na(vac_continents)] <- 0
vac_continents <- as.data.frame(vac_continents)


##### Per 1000,000 inhabitants ######
#vaccinated countries


tmp_vacc <- vacc_countries
tmp_vacc$location <- rownames(vacc_countries)
pop_vac <- merge(tmp_vacc, pop, by=("location"), all.x=T)

vacc_norm_countries <-  pop_vac[, 2:(dim(pop_vac)[2]-1) ] / (pop_vac$population/cases_per_pop)
rownames(vacc_norm_countries) <- pop_vac$location

# matplot(t(vacc_norm_countries), type="l")



#############Confirmed cases
cases_table_owid <- as.data.frame(data.table::fread("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/jhu/total_cases.csv"))
head(cases_table_owid)

#cases countries
cases_c <- cases_table_owid[,-which(colnames(cases_table_owid) %in% not_countries_list$location)]
cases_countries <- t(cases_c[,-which(colnames(cases_c)  %in% "date")])
colnames(cases_countries) <- as.character(cases_c$date)
cases_countries <- as.data.frame(cases_countries)

#cases countries per 100,000 inhabitants
tmp_vacc <- cases_countries
tmp_vacc$location <- rownames(cases_countries)
pop_vac <- merge(tmp_vacc, pop, by=("location"), all.x=T)

cases_norm_countries <-  pop_vac[, 2:(dim(pop_vac)[2]-1) ] / (pop_vac$population/cases_per_pop)
rownames(cases_norm_countries) <- pop_vac$location

# matplot(t(cases_norm_countries), type="l")


#cases world
cases_world0 <- cases_table_owid[,c("date", world)]
cases_world <- t(cases_world0[,-which(colnames(cases_world0)  %in% "date")])
colnames(cases_world) <- as.character(cases_world0$date)
rownames(cases_world) <- colnames(cases_world0)[2]
cases_world <- as.data.frame(cases_world)

#cases continents
cases_cont <- cases_table_owid[,c("date",continents)]
cases_continents <- t(cases_cont[,-which(colnames(cases_cont)  %in% "date")])
colnames(cases_continents) <- as.character(cases_cont$date)
cases_continents <- as.data.frame(cases_continents)

#############Death cases
deaths_table_owid <- as.data.frame(data.table::fread("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/jhu/total_deaths.csv"))
head(deaths_table_owid)

#deaths countries
deathes_c <- deaths_table_owid[,-which(colnames(deaths_table_owid) %in% not_countries_list$location)]
deaths_countries <- t(deathes_c[,-which(colnames(deathes_c)  %in% "date")])
colnames(deaths_countries) <- as.character(deathes_c$date)
deaths_countries <- as.data.frame(deaths_countries)

#cases countries per 100,000 inhabitants
tmp_vacc <- deaths_countries
tmp_vacc$location <- rownames(deaths_countries)
pop_vac <- merge(tmp_vacc, pop, by=("location"), all.x=T)

deaths_norm_countries <-  pop_vac[, 2:(dim(pop_vac)[2]-1) ] / (pop_vac$population/cases_per_pop)
rownames(deaths_norm_countries) <- pop_vac$location

# matplot(t(deaths_norm_countries), type="l")

#deaths world
deaths_world0 <- deaths_table_owid[,c("date", world)]
deaths_world <- t(deaths_world0[,-which(colnames(deaths_world0)  %in% "date")])
colnames(deaths_world) <- as.character(cases_world0$date)
rownames(deaths_world) <- colnames(deaths_world0)[2]
deaths_world <- as.data.frame(deaths_world)

#deaths continents
deaths_cont <- deaths_table_owid[,c("date",continents)]
deaths_continents <- t(deaths_cont[,-which(colnames(deaths_cont)  %in% "date")])
colnames(deaths_continents) <- as.character(deaths_cont$date)
deaths_continents <- as.data.frame(deaths_continents)

# matplot(t(deaths_continents), type="l")




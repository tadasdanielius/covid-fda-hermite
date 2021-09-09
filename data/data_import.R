
#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>


long_to_wide <- function(long_table, column_value){
  
  #sort table
  long_table <- long_table[order(long_table$location, long_table$date),]
  
  #columns and rows
  date_range <- seq(min(long_table[,"date"]), max(long_table[,"date"]), by=1)
  unq_loc <- unique(long_table[,"location"])
  
  #create wider table
  wider_table <- matrix(NA, nrow=length(unique(long_table[,"location"])), ncol=length(date_range))
  colnames(wider_table) <- as.character(date_range)
  rownames(wider_table) <- unq_loc
  
  for(loc_j in 1:length(unq_loc)){
    tmp_data <- long_table %>% filter(location==  unq_loc[loc_j])
    dt <- data.frame(column_value = tmp_data[,column_value])
    
    tmp_data[,column_value] <- nafill(dt, type = "locf")[[1]]
    tmp_location <- which(rownames(wider_table) %in% unq_loc[loc_j])
    for(i in 1:length(tmp_data$date)){
      wider_table[tmp_location,which(colnames(wider_table) %in% as.character(tmp_data$date[i]))] <- tmp_data[i,column_value]
    }
  }
  wider_table <- as.data.frame(wider_table)
}




#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>



cases_per_pop = 100000

pop_table_owid <- as.data.frame(data.table::fread("https://raw.githubusercontent.com/owid/covid-19-data/master/scripts/input/un/population_2020.csv"))
head(pop_table_owid)

pop <- pop_table_owid[, c("entity","population")]
colnames(pop)[1] <- "location"

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
world <- c("World")
continents <- c("Africa", "Asia", "Europe", "North America", "South America", "Oceania")


#>>>>>>>> Vaccinated <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
vac_table_owid <- as.data.frame(data.table::fread("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/vaccinations.csv"))
head(vac_table_owid)

vac_table_owid$people_vaccinated <- as.numeric(vac_table_owid$people_vaccinated)

vac_table_owid <- merge(pop,vac_table_owid,by=c("location"), all.y=T)
vac_table_owid$vacc_normalized = vac_table_owid$people_vaccinated / (vac_table_owid$population/cases_per_pop)



#not countries
not_countries_list <- unique(vac_table_owid[which(grepl("^OWID", vac_table_owid$iso_code)), c("location", "iso_code")])
not_countries_list <- rbind(not_countries_list, c("2020 Summer Olympics athletes & staff", "NA"))

vac_table_C <- vac_table_owid[which(!(vac_table_owid$location %in% not_countries_list$location)),]

#vac_table_C <- vac_table_C[order(vac_table_C$location, vac_table_C$date),]


##### Totals ######
# [vaccinated countries]
vac_tmp <- vac_table_C[,c("people_vaccinated", "location", "date")]
vacc_countries <- long_to_wide(vac_tmp, "people_vaccinated")
# matplot(t(vacc_countries), type="l")


vacc_countries[which(vacc_countries$`2021-09-07` %in% max(na.omit(vacc_countries$`2021-09-07`))),]

##### Per 1000,000 inhabitants ######
# [vaccinated countries per 100,000 inhabitant]
vac_tmp_norm <- vac_table_C[,c("vacc_normalized", "location", "date")]
vacc_norm_countries <- long_to_wide(vac_tmp_norm, "vacc_normalized")
# matplot(t(vacc_norm_countries), type="l")

#100% vaccinated
vacc_norm_countries[which(vacc_norm_countries$`2021-09-07` %in% max(na.omit(vacc_norm_countries$`2021-09-07`))),]
#Gibraltar exceeds 100% due to vaccination of non-residents
vacc_norm_countries[which(vacc_norm_countries$`2021-09-01` %in% max(na.omit(vacc_norm_countries$`2021-09-01`))),]


##### Totals ######
#vaccinated world
vacc_world0 <- vac_table_owid[which(vac_table_owid$location %in% world),c("people_vaccinated", "location", "date")]
vacc_world <- long_to_wide(vacc_world0, "people_vaccinated")
# matplot(t(vacc_world), type="l")


##### Per 1000,000 inhabitants ######
# vaccinated world per 100,000 inhabitant
vacc_world_norm0 <- vac_table_owid[which(vac_table_owid$location %in% world),c("vacc_normalized", "location", "date")]
vacc_norm_world <- long_to_wide(vacc_world_norm0, "vacc_normalized")
# matplot(t(vacc_norm_world), type="l")


##### Totals ######
#vaccinated continents
vacc_cont <- vac_table_owid[which(vac_table_owid$location %in% continents),c("people_vaccinated", "location", "date")]
vacc_continents <- long_to_wide(vacc_cont, "people_vaccinated")
vacc_continents[is.na(vacc_continents)] <- 0
# matplot(t(vacc_continents), type="l")

#Asia (data for China added onJun 10, Aug 12, Aug 26)
vacc_continents[which(vacc_continents$`2021-09-07` %in% max(na.omit(vacc_continents$`2021-09-07`))),]


##### Per 1000,000 inhabitants ######
#vaccinated continents per 100,000 inhabitant
vacc_cont_norm <- vac_table_owid[which(vac_table_owid$location %in% continents),c("vacc_normalized", "location", "date")]
vacc_norm_continents <- long_to_wide(vacc_cont_norm, "vacc_normalized")
vacc_norm_continents[is.na(vacc_norm_continents)] <- 0
# matplot(t(vacc_norm_continents), type="l")






# >>>>>>>>>>>>>>  Confirmed cases <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
cases_table_owid <- as.data.frame(data.table::fread("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/jhu/total_cases.csv"))
head(cases_table_owid)


# <<<<<<<<<<<<<< TOTALS >>>>>>>>>>>>>>>>>>>>>>>>
#cases countries
cases_c <- cases_table_owid[,-which(colnames(cases_table_owid) %in% not_countries_list$location)]
cases_countries <- t(cases_c[,-which(colnames(cases_c)  %in% "date")])
colnames(cases_countries) <- as.character(cases_c$date)
cases_countries <- as.data.frame(cases_countries)
# matplot(t(cases_countries), type="l")

#France 
# matplot(t(cases_countries[which(rownames(cases_countries) %in% "France"),]), type="l")
# cases_countries[which(rownames(cases_countries) %in% "France"),]



#cases world
cases_world0 <- cases_table_owid[,c("date", world)]
cases_world <- t(cases_world0[,-which(colnames(cases_world0)  %in% "date")])
colnames(cases_world) <- as.character(cases_world0$date)
rownames(cases_world) <- colnames(cases_world0)[2]
cases_world <- as.data.frame(cases_world)
# matplot(t(cases_world), type="l")


# [cases continents]
cases_cont <- cases_table_owid[,c("date",continents)]
cases_continents <- t(cases_cont[,-which(colnames(cases_cont)  %in% "date")])
colnames(cases_continents) <- as.character(cases_cont$date)
cases_continents <- as.data.frame(cases_continents)
# matplot(t(cases_continents), type="l")


# <<<<<<<<<<<<<< per 100,000 inhabitants >>>>>>>>>>>>>>>>>>>>>>>>

# [cases countries per 100,000 inhabitants]
tmp_vacc <- cases_countries
tmp_vacc$location <- rownames(cases_countries)
pop_vac <- merge(tmp_vacc, pop, by=("location"), all.x=T)

cases_norm_countries <-  pop_vac[, 2:(dim(pop_vac)[2]-1) ] / (pop_vac$population/cases_per_pop)
rownames(cases_norm_countries) <- pop_vac$location
# matplot(t(cases_norm_countries), type="l")


# [cases world per 100,000 inhabitants]

cases_world0 <- cases_table_owid[,c("date", world)]
cases_world0 <- pivot_longer(cases_world0, cols= !("date"), names_to = "location", values_to = "cases")
cases_world0 <- merge(pop,cases_world0,by=c("location"), all.y=T)
cases_world0$cases_norm <- cases_world0$cases / (cases_world0$population/cases_per_pop)
cases_world0 <- cases_world0[,c("location", "date", "cases_norm")]
cases_norm_world <- long_to_wide(cases_world0, "cases_norm")
# matplot(t(cases_norm_world), type="l")

# [cases continents per 100,000 inhabitants]
cases_cont <- cases_table_owid[,c("date",continents)]
cases_cont <- pivot_longer(cases_cont, cols= !("date"), names_to = "location", values_to = "cases")
cases_cont <- merge(pop,cases_cont,by=c("location"), all.y=T)
cases_cont$cases_norm <- cases_cont$cases / (cases_cont$population/cases_per_pop)
cases_cont <- cases_cont[order(cases_cont$location, cases_cont$date),]
cases_cont <- cases_cont[,c("location", "date", "cases_norm")]
cases_norm_continents <- long_to_wide(cases_cont, "cases_norm")
# matplot(t(cases_norm_continents), type="l")



# >>>>>>>>>>>>>  Death cases <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
deaths_table_owid <- as.data.frame(data.table::fread("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/jhu/total_deaths.csv"))
head(deaths_table_owid)


# <<<<<<<<<<<<<< TOTALS >>>>>>>>>>>>>>>>>>>>>>>>
#deaths countries
deathes_c <- deaths_table_owid[,-which(colnames(deaths_table_owid) %in% not_countries_list$location)]
deaths_countries <- t(deathes_c[,-which(colnames(deathes_c)  %in% "date")])
colnames(deaths_countries) <- as.character(deathes_c$date)
deaths_countries <- as.data.frame(deaths_countries)
# matplot(t(deaths_countries), type="l")


#deaths world
deaths_world0 <- deaths_table_owid[,c("date", world)]
deaths_world <- t(deaths_world0[,-which(colnames(deaths_world0)  %in% "date")])
colnames(deaths_world) <- as.character(cases_world0$date)
rownames(deaths_world) <- colnames(deaths_world0)[2]
deaths_world <- as.data.frame(deaths_world)
# matplot(t(deaths_world), type="l")


#deaths continents
deaths_cont <- deaths_table_owid[,c("date",continents)]
deaths_continents <- t(deaths_cont[,-which(colnames(deaths_cont)  %in% "date")])
colnames(deaths_continents) <- as.character(deaths_cont$date)
deaths_continents <- as.data.frame(deaths_continents)
# matplot(t(deaths_continents), type="l")



# <<<<<<<<<<<<<< per 100,000 inhabitants >>>>>>>>>>>>>>>>>>>>>>>>

#cases countries per 100,000 inhabitants
tmp_vacc <- deaths_countries
tmp_vacc$location <- rownames(deaths_countries)
pop_vac <- merge(tmp_vacc, pop, by=("location"), all.x=T)

deaths_norm_countries <-  pop_vac[, 2:(dim(pop_vac)[2]-1) ] / (pop_vac$population/cases_per_pop)
rownames(deaths_norm_countries) <- pop_vac$location
# matplot(t(deaths_norm_countries), type="l")


# [deaths world per 100,000 inhabitants]
deaths_world0 <- deaths_table_owid[,c("date", world)]
deaths_world0 <- pivot_longer(deaths_world0, cols= !("date"), names_to = "location", values_to = "cases")
deaths_world0 <- merge(pop,deaths_world0,by=c("location"), all.y=T)
deaths_world0$cases_norm <- deaths_world0$cases / (deaths_world0$population/cases_per_pop)
deaths_world0 <- deaths_world0[,c("location", "date", "cases_norm")]
deaths_norm_world <- long_to_wide(deaths_world0, "cases_norm")
# matplot(t(deaths_norm_world), type="l")



# [deaths continent per 100,000 inhabitants]

deaths_cont <- deaths_table_owid[,c("date",continents)]
deaths_cont <- pivot_longer(deaths_cont, cols= !("date"), names_to = "location", values_to = "cases")
deaths_cont <- merge(pop,deaths_cont,by=c("location"), all.y=T)
deaths_cont$cases_norm <- deaths_cont$cases / (deaths_cont$population/cases_per_pop)
deaths_cont <- deaths_cont[order(deaths_cont$location, deaths_cont$date),]
deaths_cont <- deaths_cont[,c("location", "date", "cases_norm")]
deaths_norm_continents <- long_to_wide(deaths_cont, "cases_norm")
# matplot(t(deaths_norm_continents), type="l")



rm(list=setdiff(ls(), c("cases_countries", # [cases countries]
                        "cases_world", # [cases world]
                        "cases_continents",  # [cases continents]
                        "cases_norm_countries", # [cases countries per 100,000 inhabitants]
                        "cases_norm_world", # [cases world per 100,000 inhabitants]
                        "cases_norm_continents",  # [cases continents per 100,000 inhabitants]
                        "deaths_countries", # [deaths countries]
                        "deaths_world", # [deaths world]
                        "deaths_continents",  # [deaths continents]
                        "deaths_norm_countries", # [deaths countries per 100,000 inhabitants]
                        "deaths_norm_world", # [deaths world per 100,000 inhabitants]
                        "deaths_norm_continents",  # [deaths continents per 100,000 inhabitants]
                        "vacc_countries", # [vaccinated countries]
                        "vacc_world", # [vaccinated world]
                        "vacc_continents", # [vaccinated continents]
                        "vacc_norm_countries", # [vaccinated countries per 100,000 inhabitants]
                        "vacc_norm_world", # [vaccinated world per 100,000 inhabitants]
                        "vacc_norm_continents" # [vaccinated continents per 100,000 inhabitants]
                        )))
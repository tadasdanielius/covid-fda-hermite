


cases_per_pop = 100000
min_pop = 500000


#Population data
pop <- as.data.frame(data.table::fread("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/UID_ISO_FIPS_LookUp_Table.csv"))
pop <- pop[c("Province_State", "Country_Region", "Combined_Key", "Population")]
colnames(pop) <- c("Province/State", "Country/Region", "Combined_Key", "Population")


#Confirmed cases
cases_table <- as.data.frame(data.table::fread("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"))
acc_cases = cases_table[, 7:dim(cases_table)[2]]
rownames(acc_cases) <- trimws(paste(cases_table$"Country/Region", cases_table$"Province/State"),"r")
colnames(acc_cases) <- as.Date(colnames(acc_cases), format="%m/%d/%y")

#accumulated new cases per 100,000 inhabitants
pop_dat <- merge(pop,cases_table,by=c("Country/Region",  "Province/State"))
normalized = pop_dat[, 7:dim(pop_dat)[2] ] / (pop_dat$Population/cases_per_pop)
rownames(normalized) <- trimws(paste(pop_dat$"Country/Region", pop_dat$"Province/State"),"r")
colnames(normalized) <- as.Date(colnames(normalized), format="%m/%d/%y")


#Recovered
recov_table <- as.data.frame(data.table::fread("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv"))
recovered <- recov_table[, 5:dim(recov_table)[2] ]
rownames(recovered) <- trimws(paste(recov_table$"Country/Region", recov_table$"Province/State"),"r")
colnames(recovered) <- as.Date(colnames(recovered), format="%m/%d/%y")

#Deaths
death_table <- as.data.frame(data.table::fread("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"))
deaths <- death_table[, 5:dim(death_table)[2] ]
rownames(deaths) <- trimws(paste(death_table$"Country/Region", death_table$"Province/State"),"r")
colnames(deaths) <- as.Date(colnames(deaths), format="%m/%d/%y")


#Deaths per 100,000 inhabitants
pop_death <- merge(pop,death_table,by=c("Country/Region",  "Province/State"))
normalized_deaths = pop_death[, 7:dim(pop_death)[2] ] / (pop_death$Population/cases_per_pop)
rownames(normalized_deaths) <- trimws(paste(pop_death$"Country/Region", pop_death$"Province/State"),"r")
colnames(normalized_deaths) <- as.Date(colnames(normalized_deaths), format="%m/%d/%y")



source('common.R')


cases_per_pop = 100000
min_pop = 500000

loaded_csv <- load_data()
dat = loaded_csv$dat
dates = loaded_csv$dates

pop <- read.csv('../COVID-19/csse_covid_19_data/UID_ISO_FIPS_LookUp_Table.csv')
pop <- pop[c("Province_State", "Country_Region", "Combined_Key", "Population")]

colnames(pop) <- c("Province.State", "Country.Region", "Combined_Key", "Population")
pop_dat <- merge(pop,dat,by=c("Country.Region",  "Province.State"))

# Filter out Countries with small pop
pop_dat <- pop_dat[pop_dat$Population > min_pop, ]

pop_dat = pop_dat[complete.cases(pop_dat), ]



#accumulated new cases
acc_cases = pop_dat[, 7:dim(pop_dat)[2] ]
rownames(acc_cases) <- trimws(paste(pop_dat$Country.Region, pop_dat$Province.State),"r")
all_date <- as.Date(matrix(unlist(strsplit(colnames(acc_cases), "X")), ncol=2, byrow=T)[,2], format="%m.%d.%y")
colnames(acc_cases) <- all_date

#accumulated new cases per 100,000 inhabitants
normalized = pop_dat[, 7:dim(pop_dat)[2] ] / (pop_dat$Population/cases_per_pop)
rownames(normalized) <- trimws(paste(pop_dat$Country.Region, pop_dat$Province.State),"r")
all_date <- as.Date(matrix(unlist(strsplit(colnames(normalized), "X")), ncol=2, byrow=T)[,2], format="%m.%d.%y")
colnames(normalized) <- all_date

#Recovered
recov_table <- read.csv('../COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv')

recovered <- recov_table[, 5:dim(recov_table)[2] ]
rownames(recovered) <- trimws(paste(recov_table$Country.Region, recov_table$Province.State),"r")
all_date <- as.Date(matrix(unlist(strsplit(colnames(recovered), "X")), ncol=2, byrow=T)[,2], format="%m.%d.%y")
colnames(recovered) <- all_date

#Deaths
death_table <- read.csv('../COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv')


deaths <- death_table[, 5:dim(death_table)[2] ]
rownames(deaths) <- trimws(paste(death_table$Country.Region, death_table$Province.State),"r")
all_date <- as.Date(matrix(unlist(strsplit(colnames(deaths), "X")), ncol=2, byrow=T)[,2], format="%m.%d.%y")
colnames(deaths) <- all_date
plot(t(deaths[1,]))
#Deaths per 100,000 inhabitants

pop_death <- merge(pop,death_table,by=c("Country.Region",  "Province.State"))

normalized_deaths = pop_death[, 7:dim(pop_death)[2] ] / (pop_death$Population/cases_per_pop)
rownames(normalized_deaths) <- trimws(paste(pop_death$Country.Region, pop_death$Province.State),"r")
all_date <- as.Date(matrix(unlist(strsplit(colnames(normalized_deaths), "X")), ncol=2, byrow=T)[,2], format="%m.%d.%y")
colnames(normalized_deaths) <- all_date


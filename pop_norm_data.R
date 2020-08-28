
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
normalized = pop_dat[, 7:dim(pop_dat)[2] ] * (cases_per_pop/pop_dat$Population)

rownames(normalized) <- trimws(paste(pop_dat$Country.Region, pop_dat$Province.State),"r")
all_date <- as.Date(matrix(unlist(strsplit(colnames(normalized), "X")), ncol=2, byrow=T)[,2], format="%m.%d.%y")
colnames(normalized) <- all_date



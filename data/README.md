

# Data on COVID-19 (coronavirus) 

All data sets analyzed in this research where taken from [_Our World in Data_](https://ourworldindata.org/coronavirus) and [_Johns Hopkins Centers for Civic Impact for the Coronavirus Resource Center (CRC)_](https://github.com/govex/COVID-19).



## Global Covid-19 data

### Datasets metadata

Stored in [`data_import.R`](data_import.R)

* `cases_countries`: total confirmed cases of COVID-19 for 193 countries.
* `cases_world`: total confirmed cases of COVID-19 of a world.
* `cases_continents`: total confirmed cases of COVID-19 for 6 continents.
* `cases_norm_countries`: total confirmed cases of COVID-19 per 100,000 inhabitants for 193 countries.
* `cases_norm_world`: total confirmed cases of COVID-19 per 100,000 inhabitants of a world.
* `cases_norm_continents`: total confirmed cases of COVID-19 per 100,000 inhabitants for 6 continents.
* `deaths_countries`: total deaths attributed to COVID-19 for 193 countries.
* `deaths_world`:  total deaths attributed to COVID-19 of a world.
* `deaths_continents`:  total deaths attributed to COVID-19 for 6 continents.
* `deaths_norm_countries`: total deaths attributed to COVID-19 per 100,000 inhabitants for 193 countries.
* `deaths_norm_world`: total deaths attributed to COVID-19 per 100,000 inhabitants of a world.
* `deaths_norm_continents`: total deaths attributed to COVID-19 per 100,000 inhabitants for 6 continents. 
* `vacc_countries`: total number of people who received at least one vaccine dose for 193 countries.
* `vacc_world`: total number of people who received at least one vaccine dose of a world.
* `vacc_continents`: total number of people who received at least one vaccine dose for 6 continents.
* `vacc_norm_countries`: total number of people who received at least one vaccine dose per 100,000 inhabitants for 193 countries.
* `vacc_norm_world`: total number of people who received at least one vaccine dose per 100,000 inhabitants of a world.
* `vacc_norm_continents`: total number of people who received at least one vaccine dose per 100,000 inhabitants for 6 continents. 
* `vacc_US_states`: total number of people who received at least one vaccine dose for 56 states or territories of US.
* `vacc_norm_US_states`: total number of people who received at least one vaccine dose per 100,000 inhabitants for 56 states or territories of US.
* `cases_US_states`: total confirmed cases of COVID-19 for 56 states or territories of US.
* `cases_norm_US_states`: total confirmed cases of COVID-19 per 100,000 inhabitants for 56 states or territories of US.
* `deaths_US_states`: total deaths attributed to COVID-19 for 56 states or territories of US.
* `deaths_norm_US_states`: total deaths attributed to COVID-19 per 100,000 inhabitants for 56 states or territories of US.


## Functional global Covid-19 data

### Datasets metadata

Stored in [`smooth_data.R`](smooth_data.R) 

* `cases_norm_countries_fd`: functional object of total confirmed cases of COVID-19 per 100,000 inhabitants for 193 countries.
* `cases_norm_continents_fd`: functional object of total confirmed cases of COVID-19 per 100,000 inhabitants for 6 continents.
* `cases_norm_world_fd`: functional object of total confirmed cases of COVID-19 per 100,000 inhabitants of a world.
* `cases_US_states_fd`: functional object of total confirmed cases of COVID-19 per 100,000 inhabitants for 56 states or territories of US.
* `deaths_norm_countries_fd`: functional object of total deaths attributed to COVID-19 per 100,000 inhabitants for 193 countries.
* `deaths_norm_continents_fd`: functional object of total deaths attributed to COVID-19 per 100,000 inhabitants for 6 continents.
* `deaths_norm_world_fd`: functional object of total deaths attributed to COVID-19 per 100,000 inhabitants of a world.
* `deaths_US_states_fd`: functional object of total deaths attributed to COVID-19 per 100,000 inhabitants for 56 states or territories of US.
* `vacc_norm_countries_fd`: functional object of total number of people who received at least one vaccine dose per 100,000 inhabitants for countries.
* `vacc_norm_continents_fd`: functional object of total number of people who received at least one vaccine dose per 100,000 inhabitants for 6 continents.
* `vacc_norm_world_fd`: functional object of total number of people who received at least one vaccine dose per 100,000 inhabitants of a world.
* `vacc_US_states_fd`: functional object of total number of people who received at least one vaccine dose per 100,000 inhabitants for 56 states or territories of US.

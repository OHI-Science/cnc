# README

## fis_meancatch
### Source
Direction des Affaires Maritimes (DAM)
Contains time series data from 2011-2015 for 15 species and "others" category

### Data url/website
http://www.affmar.gouv.nc/portal/page/portal/affmar/peche/Lesstatistiquesdelapecheetdesproduitsdelamer

### Date accessed
21 July 2016

### Processing plan
Merged catch_per_species with reef_catch_per_species to create fis_meancatch
Replaced french names with scientific names
Added resilience factors from fishbase.org
created fis_meancatch_resilience, which can be used with the catch-MSY model to produce bmsy values

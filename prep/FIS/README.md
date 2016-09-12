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
created fis_meancatch_resilience, which can be used with the catch-MSY model to produce bmsy values if catch data time series is a mimimum of 20 years. this is not the case currently. Instead we used only stocks that had formal stock assessments and local catch data. 

###Data used###
Only fish species that had formal stock assessments and regional catch reported were used in the analysis. Year range is from 2008-2012. For Kajikia audaz catch data was only available from 2011-2015 but stock assessment was available from 2008-2013. To gap fill in the catch for years 2008-2010 we used the mean catch from 2011-2015. More recent catch data, up to 2015 is available for all assessed stocks but stock assessments are only current to 2012 for all assessed stocks and to 2013 for some of the stocks. This constricted our available data range to 2008 through 2012.

###model used###
We looked at two models. We compared the global model (with sb/sbMSY replacing b/bmsy) with a modifed model for NC that does not penalize for underexploitation. If sb/sbmsy was 0.8 or greater than no penalty was applied to the score. 

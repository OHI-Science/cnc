---
title: "README_all_raw_fisheries_data"
author: "Erich J Pacheco"
date: "August 10, 2016"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

# All_Raw_Fisheriex_Data.xlsx -- Workbook descritions

## Total_CPUE_Offshore_Fisheries
The New-Caledonian Offshore fisheries total Catch Per Unit Effort (expressed in g/ham = gram per hook). CPUE is express per year from 2011 to 2015. Offshore fisheries are only longliners. Note that this CPUE concern only the Targeted species (Exclude bycatch) : 10 species (which you’ll find in the “Obs_Data_TargetSp_... files)

## Reef_Fisheries_License_Number
This data expresses the number of License delivered each year btw 2011-2015 for the “reef fisheries” within the EEZ. Most of the fisheries occurring in the EEZ are offshore fisheries due to remoteness of any islands or reef. However, a very small number of boat keep on fishing sea cucumber and lobster in these remote areas. So these fisheries is considered as “Reef Fisheries” but is still happening in the EEZ. Only 2-3 licenses were ever delivered. 

## Reef_Fish_Direct_Employement
This tab is closely linked to the previous “Reef_Fisheries_License_Number”. In addition to the number of licenses delivered, these are the numbers of people unemployed by these reef fisheries (Very small number of people involved). 
(Again this is what is happening in the EEZ and exclude data from Lagoon Areas around New-Cal main islands which would have significantly raised the numbers for reef fisheries…but for the purpose of this OHI, this is out of scope).

## Reef_Catch_Tonne_Per_Species
Still focusing on fisheries within the EEZ, this give the total catch in tonnes per year between 2011 and 2015. Couldn’t have detailed information regarding the species so here we have the “Groups” : Lobsters, Sea Cucumbers, Deep-sea Snappers… With no distinction btw species. 

## OffshoreFisheries_Tot_Diversity
This gives information regarding the total diversity of species that were caught by offshore fisheries (longliners). Note that this number includes the ByCatch as well. To keep in mind : btwn 2011-2015 10 species were targeted and sold on the market. So if needed, we can have the total bycatch diversity by simply taking 10 out of the “count” numbers. 

## Offshore_Fishing_License_Number
This data expresses the number of License delivered each year btw 2011-2015 for the “offshore fisheries” within the EEZ. Again, this concern only the longliners. 

## ObsData_TargetSp_Total_Weight
So this information comes from On-Board Observers (labelled “ObsData”). Here you have for Each Species targeted and caught by the longline fisheries, for each year between 2011 and 2015, the total annual caught (or cumulated weight) expressed in tonnes. 

Important note : As observers weren’t on board for every single fishing campaign, these total annual caught isn’t really relevant and doesn’t expressed what is really caught annually. To get a real idea of what was caught every single year for each species, you want to check the tab “Catch_Tonnes_per_Species”

Also note : Swordfish, Spearfish, Sailfish, Mahimahi, Wahoo and Opah are not targeted but are regularly caught by offshore fisheries. As these fish are valuable, once fished they are kept and sold. Basically the longliners are only looking for tunas but end up catching other valuable species. 

## ObsData_TargetSp_AverageWeigth
So this information comes from On-Board Observers (labelled “ObsData”). Here you have for Each Species targeted and caught by the longline fisheries, for each year between 2011 and 2015, the average weight (expressed in Kg) of individual fish that was caught.

Also note : Swordfish, Spearfish, Sailfish, Mahimahi, Wahoo and Opah are not targeted but are regularly caught by offshore fisheries. As these fish are valuable, once fished they are kept and sold. Basically the longliners are only looking for tunas but end up catching other valuable species. 

## ObsData_TargetSp_AverageLenght
So this information comes from On-Board Observers (labelled “ObsData”). Here you have for Each Species targeted and caught by the longline fisheries, for each year between 2011 and 2015, the average size (expressed in Cm) of individual fish that was caught.

Also note : Swordfish, Spearfish, Sailfish, Mahimahi, Wahoo and Opah are not targeted but are regularly caught by offshore fisheries. As these fish are valuable, once fished they are kept and sold. Basically the longliners are only looking for tunas but end up catching other valuable species. 

## ObsData_TargetSp_Abundance
So this information comes from On-Board Observers (labelled “ObsData”). Here you have for Each Species targeted and caught by the longline fisheries, for each year between 2011 and 2015, the total abundance : The total number of fishes from each species that were caught on a specific year.

Also note : Swordfish, Spearfish, Sailfish, Mahimahi, Wahoo and Opah are not targeted but are regularly caught by offshore fisheries. As these fish are valuable, once fished they are kept and sold. Basically the longliners are only looking for tunas but end up catching other valuable species. 

## ObsData_Bycatch_Total_Weight
So this information comes from On-Board Observers (labelled “ObsData”). Here you have for Each bycatch Species caught by the longline fisheries, for each year between 2011 and 2015, the total annual caught (or cumulated weight) expressed in tonnes. Note that there is a lot of “no data” : This means that no individual of the concerned species was caught this year so there is no information available for this metric. 
Note that Bycatch aren’t not only fish or sharks/rays but includes also turtles and seabirds.

## ObsData_Bycatch_AverageWeight
So this information comes from On-Board Observers (labelled “ObsData”). Here you have for Each bycatch Species caught by the longline fisheries, for each year between 2011 and 2015, the average weight (expressed in Kg) of individual that was caught. Note that there is a lot of “no data” : This means that no individual of the concerned species was caught this year so there is no information available for this metric. 
Note that Bycatch aren’t not only fish or sharks/rays but includes also turtles and seabirds.

## ObsData_Bycatch_AverageLength
So this information comes from On-Board Observers (labelled “ObsData”). Here you have for Each bycatch Species caught by the longline fisheries, for each year between 2011 and 2015, the average size (expressed in Cm) of individual that was caught. Note that there is a lot of “no data” : This means that no individual of the concerned species was caught this year so there is no information available for this metric. 

Note that Bycatch aren’t not only fish or sharks/rays but includes also turtles and seabirds.

## ObsData_Bycatch_Abundance
So this information comes from On-Board Observers (labelled “ObsData”). Here you have for Each bycatch Species caught by the longline fisheries, for each year between 2011 and 2015, the total abundance : The total number of individuals from each species that were caught on a specific year. 
Note that Bycatch aren’t not only fish or sharks/rays but includes also turtles and seabirds.

## Nb_Fishery_Campaing_Wt_Observer
This tab correspond to the number of time an observer was on-board during an offshore fishing campaign. 

## Market_Prices
This gives you the evolution of prices on the Market between 2011 and 2015 for each species that are targeted, caught and sold by the offshore fisheries (longliners). Note that these are not the prices for export or anything. It’s only the price people from NC have to pay on common markets to buy these fishes. 

Note : There is no difference in prices on market for the different species of Spearfish & Sailfish. So Blue Marlin, Black Marlin, Stripped Marlin, Sailfish and Spearfish are all included under the label “Marlin”. 
Note :There is no data for the Skipjack in this dataset even if according to the On-Board Observers Data set, the species seems to be fished regularly and in fair quantities. 
Note : Mahimahi, Wahoo and Opah are not targeted but are regularly caught by offshore fisheries. As these fish are valuable, once fished they are kept and sold. Basically the longliners are only looking for tunas but end up catching other valuable species. 

## Catch_Tonnes_per_Species
So this tab is more relevant than « ObsData_TargetSp_Total_Weight”. Based on fisheries datasheet completed by fishermen themselves, this numbers represent the annual Total landing expressed in Tonnes for each species that are targeted, caught and sold on the market between 2011-2015. 
Note :There is no data for the Skipjack in this dataset even if according to the On-Board Observers Data set, the species seems to be fished regularly and in fair quantities. 

##Catch_Average_Weight
Can delete this file as it actually contain the same information as “ObsData_TargetSp_AverageWeigth”

## Export_Tuna_Total_Tonnes : 
Here are the quantities of tunas exported Japan, Europe and Canneries (no country information). Quantities are expressed in tonnes. Information are only available for the years 2011-2013 and for only two species. 

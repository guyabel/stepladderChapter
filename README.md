# stepladderChapter

Scripts for data analysis in support of Ch. 7 of WMR: Migration as Stepladders of Opportunity
This workflow uses three data sources to measure migrant stocks as analyzed through the lens of the Human Development Index.
The following files can be found below. Some prep work was done in excel prior to importing into the R interface.

#Data#
DESA Population Stock Origin and Destination 2019;
DESA Population Totals 2019;
HDI Index 2018;
UNHCR Total Persons of Concern 2018;

#Data cleaning/files#
Origin_dest_long.r for DESA migrant stock
Pop_clean.r for DESA total population
HDI_desc for HDI
Unhcr_clean.r for UNCHR
Merge_unhcr_desa.r to merge and align names for UNHCR and DESA data to create stock data
Merge_hdi_flow.r to merge and align the stock data with HDI data

#Visualizations#
Visualize classifications.r converts the diverging bar charts and the to_within_from charts. This is more of a draft script.
Visualize classifications_95_control.r does visualizations but holds countries to the same HDI classifications as they had in 1995.
To_within_from_with95.r visualizes the to-within-from graphs, but with a line that represents if the HDI classifications were held constant from 1995. Produces one of the final outputs.
Diverge.all.r visualizes the diverging stacked bar charts which display total migrant stock and total migrant stock as a percent of population. The 2019 version is currently in the draft.
deHaas_replication.r tries to emulate the style and answer the same question of de Haas’s 2010 graph. Notice we do not create a fifth “very low” classification.
Top20s.r creates the top20 charts for the chapter. Currently, these are compiled into a single table in the first draft of the chapter.
Region_imm_em.r was used in part for some of the info in the map included in the chapter itself. Some of the computations were done in ArcGIS pro. Can provide these files if requested.



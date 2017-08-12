library(data.table,quietly = TRUE)
library(dplyr,quietly = TRUE)
library(ggplot2,quietly = TRUE)
library(stringr,quietly = TRUE)
library(DT,quietly = TRUE)
library(tidyr,quietly = TRUE)
library(corrplot,quietly = TRUE)
library(leaflet,quietly = TRUE)
library(lubridate,quietly = TRUE)
library(gbm3,quietly = TRUE)
library(xgboost,quietly = TRUE)


setwd("S:/UNIT1216/xmiao/STUDY")

properties <- fread("properties_2016.csv")
transactions <- fread("train_2016.csv")
sub<-fread("sample_submission.csv")

properties <- properties %>% rename(
  id_parcel = parcelid,
  build_year = yearbuilt,
  area_basement = basementsqft,
  area_patio = yardbuildingsqft17,
  area_shed = yardbuildingsqft26, 
  area_pool = poolsizesum,  
  area_lot = lotsizesquarefeet, 
  area_garage = garagetotalsqft,
  area_firstfloor_finished = finishedfloor1squarefeet,
  area_total_calc = calculatedfinishedsquarefeet,
  area_base = finishedsquarefeet6,
  area_live_finished = finishedsquarefeet12,
  area_liveperi_finished = finishedsquarefeet13,
  area_total_finished = finishedsquarefeet15,  
  area_unknown = finishedsquarefeet50,
  num_unit = unitcnt, 
  num_story = numberofstories,  
  num_room = roomcnt,
  num_bathroom = bathroomcnt,
  num_bedroom = bedroomcnt,
  num_bathroom_calc = calculatedbathnbr,
  num_bath = fullbathcnt,  
  num_75_bath = threequarterbathnbr, 
  num_fireplace = fireplacecnt,
  num_pool = poolcnt,  
  num_garage = garagecarcnt,  
  region_county = regionidcounty,
  region_city = regionidcity,
  region_zip = regionidzip,
  region_neighbor = regionidneighborhood,  
  tax_total = taxvaluedollarcnt,
  tax_building = structuretaxvaluedollarcnt,
  tax_land = landtaxvaluedollarcnt,
  tax_property = taxamount,
  tax_year = assessmentyear,
  tax_delinquency = taxdelinquencyflag,
  tax_delinquency_year = taxdelinquencyyear,
  zoning_property = propertyzoningdesc,
  zoning_landuse = propertylandusetypeid,
  zoning_landuse_county = propertycountylandusecode,
  flag_fireplace = fireplaceflag, 
  flag_tub = hashottuborspa,
  quality = buildingqualitytypeid,
  framing = buildingclasstypeid,
  material = typeconstructiontypeid,
  deck = decktypeid,
  story = storytypeid,
  heating = heatingorsystemtypeid,
  aircon = airconditioningtypeid,
  architectural_style= architecturalstyletypeid
)
transactions <- transactions %>% rename(
  id_parcel = parcelid,
  date = transactiondate
)

properties <- properties %>% mutate(
  tax_delinquency = ifelse(tax_delinquency=="Y",1,0),
  flag_fireplace = ifelse(flag_fireplace=="Y",1,0),
  flag_tub = ifelse(flag_tub=="Y",1,0))

get_feature_names<-function()

#merge data
full<-merge(properties,transactions,key=parcelid,all.y=TRUE)
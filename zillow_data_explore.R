# install.packages("data.table",dependencies=TRUE)
# install.packages("DT",dependencies=TRUE)
# install.packages("tidyr",dependencies = TRUE)
# install.packages("corrplot",dependencies = TRUE)
# install.packages("leaflet",dependencies = TRUE)
# install.packages("lubridate",dependencies = TRUE)

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

#bar chart
transactions<-transactions %>% 
  mutate(year_month=make_date(year=year(date),month=month(date)))
transactions %>% group_by(year_month) %>% count() %>% 
  ggplot(aes(x=year_month,y=n))+
  geom_bar(stat="identity",fill="red")

#histogram
transactions %>% 
  ggplot(aes(x=logerror))+geom_histogram(bins=200,fill="red")+
  theme_bw()+theme(axis.title = element_text(size=14),axis.text = element_text(size=12))+ylab("count")+coord_cartesian(x=c(-0.5,0.5))

transactions<-transactions%>% mutate(abs_logerror=abs(logerror))
transactions%>% ggplot(aes(x=abs_logerror))+
  geom_histogram(bins=400,fill="Red")+ylab("count")+coord_cartesian(x=c(0,0.5))

#line chart
transactions %>%
  mutate(year_month=make_date(year=year(date),month=month(date)) )%>%
  group_by(year_month) %>% summarise(mean_abs_logerror=mean(abs_logerror)) %>%
  ggplot(aes(x=year_month,y=mean_abs_logerror))+geom_line(size=1.5,color="red")+geom_point(size=4,color="red")

transactions%>% mutate(year_month=make_date(year=year(date),month=month(date))) %>% group_by(year_month) %>% 
  summarise(mean_logerror=mean(logerror))%>%ggplot(aes(x=year_month,y=mean_logerror))+geom_line(size=1.5,color="Red")+
  geom_point(size=4,color="red")+theme_bw()

#merge data
full<-merge(properties,transactions,key=parcelid,all.y=TRUE)
#identify variables with too many missing value
missing_values<-full%>%summarize_each(funs(sum(is.na(.))/n()))
missing_values<-gather(missing_values,key="feature",value="missing_pct")

missing_values%>% ggplot(aes(x=reorder(feature,-missing_pct),y=missing_pct))+
  geom_bar(stat="identity",fill="red")+
  coord_flip()+theme_bw()

good_feature<-filter(missing_values,missing_pct<0.8)

#exploration on full data
full%>%
  ggplot(aes(x=build_year))+
  geom_line(stat="density",color="red",size=1.2)+theme_bw()

full%>% group_by(build_year) %>%
  summarize(mean_abs_logerror=mean(abs(logerror)),n())%>%
  ggplot(aes(x=build_year,y=mean_abs_logerror))+
  geom_smooth(color="grey40")+
  geom_point(color="red")+coord_cartesian(ylim=c(0,0.3))+theme_bw()

#key variables
#1st sumbission
x.dev1<-full[,names(full)%in%c("num_bathroom","num_bedroom","num_bathroom_calc","num_bath","num_garage",
                               "num_pool","num_room","num_unit","area_lot","area_total_calc",
                               "area_live_finished", "tax_building","tax_total","tax_year",
                               "tax_land","tax_property","tax_delinquency",
                               "region_city", "region_county","region_neighbor","region_zip",           
                                "quality","fips","heating","build_year",
                                "longitude","latitude","censustractandblock")]

#derive variables;

x.dev1<-x.dev1 %>% mutate(
                          age_build=2017-build_year,
                          utilization=area_live_finished/area_lot,
                          bath_per=num_bathroom_calc/num_bedroom,
                          garage_per=num_garage/num_bedroom,
                          importance_land=tax_land/tax_total)
y.dev<-full$logerror
x.dev<-x.dev1[,names(x.dev1)%in%c("censustractandblock","heating","quality",
                                  "age_build","tax_total","tax_delinquency","tax_year",
                                  "importance_land","area_total_calc","area_live_finished",
                                  "utilization","num_bedroom","num_bathroom","bath_per",
                                  "num_garage","garage_per","num_pool")]

x.dev<-x.dev %>% 
  mutate(num_garage=ifelse(is.na(num_garage),0,num_garage),
         num_pool=ifelse(is.na(num_pool),0,num_pool),
         garage_per=ifelse(is.na(garage_per),0,garage_per))



properties<-properties %>% mutate(
  age_build=2017-build_year,
  utilization=area_live_finished/area_lot,
  bath_per=num_bathroom_calc/num_bedroom,
  garage_per=num_garage/num_bedroom,
  importance_land=tax_land/tax_total)

properties<-properties%>% 
  mutate(num_garage=ifelse(is.na(num_garage),0,num_garage),
         num_pool=ifelse(is.na(num_pool),0,num_pool),
         garage_per=ifelse(is.na(garage_per),0,garage_per))

prop<-properties[,names(properties)%in%c("censustractandblock","heating","quality",
                                  "age_build","tax_total","tax_delinquency","tax_year",
                                  "importance_land","area_total_calc","area_live_finished",
                                   "utilization","num_bedroom","num_bathroom","bath_per",
                                   "num_garage","garage_per","num_pool")]

# x.dev1<-x.dev1 %>% mutate(
#   tax_delinquency=factor(tax_delinquency)
# )

gbt_base<-  gbmt_fit(x.dev, y.dev,
           
           distribution = gbm_dist('Gaussian'),
           # offset = rep(0,nrow(x.dev1)),
           
           train_params = training_params(num_trees = 1000,
                                          interaction_depth = 4,
                                          min_num_obs_in_node = 50,
                                          shrinkage = 0.05,
                                          bag_fraction = 0.5,
                                          num_train = nrow(x.dev)),
        
           
           keep_gbm_data = FALSE,
           cv_folds = 3,
           par_details = gbmParallel(num_threads = 3),
           is_verbose = TRUE)


save(gbt_base,file="gbt_base.RData")
#get prediction
start.time<-Sys.time()
submission_gbm<-prop %>% 
  mutate("201610"=gbm3:::predict.GBMFit(object=gbt_base, newdata = prop,n.trees=1000,type="response"),
         transactiondate=as.factor("2016-11-01"),
         "201611"=gbm3:::predict.GBMFit(object=gbt_base, newdata = prop,n.trees=1000,type="response"),
         transactiondate=as.factor("2016-12-01"),
         "201612"=gbm3:::predict.GBMFit(object=gbt_base, newdata = prop,n.trees=1000,type="response"),
         transactiondate=as.factor("2017-10-01"),
         "201710"=gbm3:::predict.GBMFit(object=gbt_base, newdata = prop,n.trees=1000,type="response"),
         transactiondate=as.factor("2017-11-01"),
         "201711"=gbm3:::predict.GBMFit(object=gbt_base, newdata = prop,n.trees=1000,type="response"),
         transactiondate=as.factor("2017-12-01"),
         "201712"=gbm3:::predict.GBMFit(object=gbt_base, newdata = prop,n.trees=1000,type="response"),
         transactiondata=as.factor("2017-12-31")) 

first_gbm<-submission_gbm %>% select(`201610`,`201611`,`201612`,`201710`,`201711`,`201712`)
first_gbm$ParcelId<-properties$id_parcel

end.time<-Sys.time()                   
message(end.time-start.time)
write.csv(first_gbm,file="first_gbm.csv")


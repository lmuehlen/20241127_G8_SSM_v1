## ----setup, include=FALSE-------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## -------------------------------------------------------------------------------------------------------------------------------------
get_ifo_indu<-function(){
  query <- CreateSearchQuery()
addAttributeValueFilter(query,"Release","rel_deifodr")
setSearchText(query, "Manufacturing by Sector Yes-Answers")
entities<-c(query)%>%SearchEntities()%>%getEntities()
release_tickers<-c(map_chr(entities,getName))

y<-release_tickers%>%FetchTimeSeries()

df<-y%>%map(as.xts)%>%do.call("cbind",.)%>%fortify.zoo(name="date")%>%as_tibble()%>%
  pivot_longer(-date,names_to = "code",values_to = "values")

m<-map(y,function(x)
  {cbind(title=getTitle(x),
      code=getMetadataValues(getMetadata(x),"PrimName"),
      countrycode=getMetadataValues(getMetadata(x),"Region"))
})

m2<-do.call("rbind",m)%>%as_tibble()

data<-left_join(df,m2,by="code")

data.indu<-data%>%
  filter(grepl("deifo_c(\\d{4}000)_(tkj|maj|fej|afj|arj|auj|bhj)_bdu",code))%>%
  mutate(
    sector="c",#sector=sub("deifo_(.).*", "\\1", sectorcode),
    nace2=sub(".*deifo_.(\\d{4}).*", "\\1", code),
    level=case_when(grepl("00$",nace2)~2,
                    grepl("0$",nace2)~3,
                    TRUE~4),
    nace2=gsub("0{1,2}$", "", nace2), #delete last two zeros (if exist)
    var=sub(".*deifo_(.*)_(.*)_bdu","\\2",code)
  )%>%
  select(-c(code,title))%>%
  filter(date>="2015-01-01",nace2!="00")%>%
  pivot_wider(values_from = values,names_from = var)%>%
  rename("ifo_capacity.bottleneck"=tkj,
         "ifo_shortage.material"=maj,
         "ifo_financial.bottleneck"=fej,
         "ifo_lack.orders"=afj,
         "ifo_labour.bottleneck"=arj,
         "ifo_other.obstructive.factors"=auj,
         "ifo_obstruction.production"=bhj)
  

}


## -------------------------------------------------------------------------------------------------------------------------------------
## Does not work, yet (Macrobond is shit)
get_prices_destatis_macrobond<-function(){
  query <- CreateSearchQuery()
addAttributeValueFilter(query,"Release","rel_deifodr")
setSearchText(query, "Services Yes-Answers")
entities<-c(query)%>%SearchEntities()%>%getEntities()
release_tickers<-c(map_chr(entities,getName))

y<-release_tickers%>%FetchTimeSeries()

df<-y%>%map(as.xts)%>%do.call("cbind",.)%>%fortify.zoo(name="date")%>%as_tibble()%>%
  pivot_longer(-date,names_to = "code",values_to = "values")

m<-map(y,function(x)
  {cbind(title=getTitle(x),
      code=getMetadataValues(getMetadata(x),"PrimName"),
      countrycode=getMetadataValues(getMetadata(x),"Region"))
})

m2<-do.call("rbind",m)%>%as_tibble()

data<-left_join(df,m2,by="code")

data.serv<-data%>%
  filter(grepl("desurv",code))%>%
  mutate(
    title=sub("Germany, Business Surveys, Ifo, Business Survey, Services, (.*), Yes-Answers","\\1",title),
    sector=sub("(.*),(\\s*)?","\\1",title),
    var=sub("(.*),(\\s*)?","\\2",title)
  )%>%
  #select(-title)%>%
  filter(date>="2015-01-01")
  

}


## -------------------------------------------------------------------------------------------------------------------------------------

get_p_eurostat<-function(){
query1 <- CreateSearchQuery()
setSearchText(query1, "sts_inpp_m,nsa,i21,de")
entities1<-c(query1)%>%SearchEntities()%>%getEntities()
release_tickers1<-c(map_chr(entities1,getName))

query2 <- CreateSearchQuery()
setSearchText(query2, "sts_sepp_q,nsa,i21,de")
entities2<-c(query2)%>%SearchEntities()%>%getEntities()
release_tickers2<-c(map_chr(entities2,getName))


release_tickers<-c(release_tickers1,release_tickers2)


#entities<-c(query)%>%SearchEntities()%>%getEntities()
# entities2<-c(query2)%>%SearchEntities()%>%getEntities()
# release_tickers<-c(map_chr(entities1,getName),map_chr(entities2,getName))
#release_tickers<-c(map_chr(entities,getName))

y<-release_tickers%>%FetchTimeSeries()
#getMetadata(y[[1]])
#getConcepts(y[[3]])

df<-y%>%map(as.xts)%>%do.call("cbind",.)%>%fortify.zoo(name="date")%>%as_tibble()%>%
  pivot_longer(-date,names_to = "code",values_to = "producer_prices")%>%
  mutate()

m<-map(y,function(x)
  {cbind(title=getTitle(x),
      code=getMetadataValues(getMetadata(x),"PrimName"),
      countrycode=getMetadataValues(getMetadata(x),"Region"))
})

m<-do.call("rbind",m)%>%as_tibble()

data<-left_join(df,m,by="code")%>%
  mutate(
    sectorcode=sub("prcprr(.*)nsai21.*","\\1",code))%>%
  filter(grepl("^[A-Za-z](\\d{2,4})?$", sectorcode))%>%
  mutate(sector=sub("(.)(.*)","\\1",sectorcode),
         subsector=sub("(.)(.*)","\\2",sectorcode),
    level=case_when(nchar(subsector)==0~1,
                    nchar(subsector)==2~2,
                    nchar(subsector)==3~3,
                    nchar(subsector)==4~4,
                    TRUE~NA_real_),
    subsector_name=sub("Germany, Eurostat, Producer Prices in Industry, Producer Prices, (.*), 2021=100, Index","\\1", title))

data%>%select(countrycode,date,level,sector,nace2=subsector,subsector_name,producer_prices)
}




## -------------------------------------------------------------------------------------------------------------------------------------
get_q_eurostat<-function(){
query1 <- CreateSearchQuery()
setSearchText(query1, "sts_inpr_m,nsa,i21,de")
entities1<-c(query1)%>%SearchEntities()%>%getEntities()
release_tickers1<-c(map_chr(entities1,getName))

query2 <- CreateSearchQuery()
setSearchText(query2, "sts_sepr_m,nsa,i21,de")
entities2<-c(query2)%>%SearchEntities()%>%getEntities()
release_tickers2<-c(map_chr(entities2,getName))


release_tickers<-c(release_tickers1,release_tickers2)


#entities<-c(query)%>%SearchEntities()%>%getEntities()
# entities2<-c(query2)%>%SearchEntities()%>%getEntities()
# release_tickers<-c(map_chr(entities1,getName),map_chr(entities2,getName))
#release_tickers<-c(map_chr(entities,getName))

y<-release_tickers%>%FetchTimeSeries()
#getMetadata(y[[1]])
#getConcepts(y[[3]])

df<-y%>%map(as.xts)%>%do.call("cbind",.)%>%fortify.zoo(name="date")%>%as_tibble()%>%
  pivot_longer(-date,names_to = "code",values_to = "production_volume")%>%
  mutate()

m<-map(y,function(x)
  {cbind(title=getTitle(x),
      code=getMetadataValues(getMetadata(x),"PrimName"),
      countrycode=getMetadataValues(getMetadata(x),"Region"))
})

m<-do.call("rbind",m)%>%as_tibble()

data<-left_join(df,m,by="code")%>%
  mutate(
    sectorcode=sub("prd(.*)nsai21.*","\\1",code))%>%
  filter(grepl("^[A-Za-z](\\d{2,4})?$", sectorcode))%>%
  mutate(sector=sub("(.)(.*)","\\1",sectorcode),
         subsector=sub("(.)(.*)","\\2",sectorcode),
    level=case_when(nchar(subsector)==0~1,
                    nchar(subsector)==2~2,
                    nchar(subsector)==3~3,
                    nchar(subsector)==4~4,
                    TRUE~NA_real_),
    subsector_name=sub("Germany, Eurostat, Producer Prices in Industry, Producer Prices, (.*), 2021=100, Index","\\1", title))

data%>%select(date,sector,nace2=subsector,production_volume)
}




## -------------------------------------------------------------------------------------------------------------------------------------
get_p.imp_eurostat<-function(){
query <- CreateSearchQuery()
setSearchText(query, "sts_inpi_m;prc_imp,nsa,i21,de")
entities<-c(query)%>%SearchEntities()%>%getEntities()
release_tickers<-c(map_chr(entities,getName))


y<-release_tickers%>%FetchTimeSeries()

df<-y%>%map(as.xts)%>%do.call("cbind",.)%>%fortify.zoo(name="date")%>%as_tibble()%>%
  pivot_longer(-date,names_to = "code",values_to = "import_prices")

m<-map(y,function(x)
  {cbind(title=getTitle(x),
      code=getMetadataValues(getMetadata(x),"PrimName"),
      countrycode=getMetadataValues(getMetadata(x),"Region"))
})

m<-do.call("rbind",m)%>%as_tibble()

data<-left_join(df,m,by="code")%>%
  mutate(
    sectorcode=sub("(prcimpcpa|prcimpeucpa|prcimpneucpa)(.*)nsai21.*","\\2",code))%>%
  filter(grepl("^[A-Za-z](\\d{2,4})?$", sectorcode))%>%
  mutate(sector=sub("(.)(.*)","\\1",sectorcode),
         subsector=sub("(.)(.*)","\\2",sectorcode),
    level=case_when(nchar(subsector)==0~1,
                    nchar(subsector)==2~2,
                    nchar(subsector)==3~3,
                    nchar(subsector)==4~4,
                    TRUE~NA_real_),
    import_region=case_when(grepl("prcimpcpa",code)~"Total",
                            grepl("prcimpneucpa",code)~"Non-Euro Area",
                            grepl("prcimpeucpa",code)~"Euro Area"),
    subsector_name=sub("Germany, Eurostat, Import Prices in Industry, Import Prices(, Euro Area|, Non-Euro Area|), (.*), 2021=100, Index","\\2", title))

data%>%select(date,sector,nace2=subsector,import_region,import_prices)%>%
  pivot_wider(values_from = "import_prices",names_from = "import_region",names_prefix = "import_prices")
}



## ----eval=FALSE, include=FALSE--------------------------------------------------------------------------------------------------------
# data<-gen_table("61241-0004","genesis")
# cubes<-gen_objects2stat(code = "51000",database = "genesis", category = "cubes", detailed = T)
# 
# 
# search_results <- gen_find(term = "Dienstleistung",
#                            detailed = FALSE,
#                            ordering = TRUE,
#                            category = "all",
#                            database = "genesis",pagelength = 2500)
# 
# search_results$Cubes


## -------------------------------------------------------------------------------------------------------------------------------------
get_p.indu_destatis<-function(){
data_2d<-gen_cube("61241BM002", database = "genesis")%>%  
  mutate(nace2=sub("GP19-(.*)","\\1",GP19M2),
         ym=paste0(as.character(JAHR),sub("MONAT(.*)","\\1",MONAT)),
         date=ym(ym),
         producerprices=PRE001_WERT)%>%
  select(nace2,date,producerprices)

data_3d<-gen_cube("61241BM003", database = "genesis")%>%  
  mutate(nace2=sub("GP19-(.*)","\\1",GP19M3),
         ym=paste0(as.character(JAHR),sub("MONAT(.*)","\\1",MONAT)),
         date=ym(ym),
         producerprices=PRE001_WERT)%>%
  select(nace2,date,producerprices)

data_4d<-gen_cube("61241BM004", database = "genesis")%>%  
  mutate(
    nace2=sub("GP19-(.*)","\\1",GP19M4),
         level=4,
         ym=paste0(as.character(JAHR),sub("MONAT(.*)","\\1",MONAT)),
         date=ym(ym),
         producerprices=PRE001_WERT)%>%
  select(nace2,date,producerprices)

rbind(data_2d,data_3d,data_4d)%>%
  mutate(producerprices=ifelse(producerprices==0,NA_real_,producerprices))
}


## -------------------------------------------------------------------------------------------------------------------------------------
# Note: WZ08 gleichbedeutend mit NACE2
get_q.indu_industry_destatis<-function(){
data_2d<-gen_cube("42153BM002", database = "genesis")%>%
  filter(WERT03=="WERTORG")%>%
  mutate(nace2=sub("WZ08-(.*)","\\1",WZ08V2),
         ym=paste0(as.character(JAHR),sub("MONAT(.*)","\\1",MONAT)),
         date=ym(ym),
         production=PRO101_WERT)%>%
  select(nace2,date,production)

data_3d<-gen_cube("42153BM003", database = "genesis")%>%
  filter(WERT03=="WERTORG")%>%
  mutate(nace2=sub("WZ08-(.*)","\\1",WZ08V3),
         ym=paste0(as.character(JAHR),sub("MONAT(.*)","\\1",MONAT)),
         date=ym(ym),
         production=PRO101_WERT)%>%
  select(nace2,date,production)

data_4d<-gen_cube("42153BM004", database = "genesis")%>%
  filter(WERT03=="WERTORG")%>%
  mutate(nace2=sub("WZ08-(.*)","\\1",WZ08V4),
         ym=paste0(as.character(JAHR),sub("MONAT(.*)","\\1",MONAT)),
         date=ym(ym),
         production=PRO101_WERT)%>%
  select(nace2,date,production)

rbind(data_2d,data_3d,data_4d)%>%
  mutate(production=ifelse(production==0,NA_real_,production))
}


## -------------------------------------------------------------------------------------------------------------------------------------
get_p.serv_destatis<-function(){
data<-gen_cube("61311BV002", database = "genesis")%>%
#  filter(WZ08D6!="WZ08-501-01")%>% #getting rid of one composed thing
  mutate(nace2=sub("WZ08-(.*)","\\1",WZ08D6),
        # level=2,
         ym=paste0(as.character(JAHR),as.character( as.numeric(sub("QUART(.*)","\\1",QUARTG))*3)),
         date=ym(ym),
         producerprices=PRE023_WERT)%>%
  select(nace2,date,producerprices)

data%>%
  mutate(producerprices=ifelse(producerprices==0,NA_real_,producerprices))
}


## -------------------------------------------------------------------------------------------------------------------------------------

get_q.serv_destatis<-function(){
data<-gen_cube("47414BM004", database = "genesis")%>%
  filter(WERT01=="WERTORG")%>%
#  filter(WZ08D6!="WZ08-501-01")%>% #getting rid of one composed thing
  mutate(nace2=sub("WZ08-(.*)","\\1",WZ08N8),
        # level=2,
         ym=paste0(as.character(JAHR),sub("MONAT(.*)","\\1",MONAT)),
         date=ym(ym),
         production=PRO102_WERT)%>%
  select(nace2,date,production)

data%>%
  mutate(production=ifelse(production==0,NA_real_,production))
}


## -------------------------------------------------------------------------------------------------------------------------------------
get_pimp_destatis<-function(){
data_2d<-gen_cube("61411BM002", database = "genesis")%>%  
  mutate(nace2=sub("GP19-(.*)","\\1",GP19X2),
         level=2,
         ym=paste0(as.character(JAHR),sub("MONAT(.*)","\\1",MONAT)),
         date=ym(ym),
         importprices=PRE004_WERT)%>%
  select(nace2,level,date,importprices)


data_4d<-gen_cube("61411BM004", database = "genesis")%>%  
  mutate(nace2=sub("GP19-(.*)","\\1",GP19X4),
         level=4,
         ym=paste0(as.character(JAHR),sub("MONAT(.*)","\\1",MONAT)),
         date=ym(ym),
         importprices=PRE004_WERT)%>%
  select(nace2,level,date,importprices)

rbind(data_2d,data_4d)%>%
  mutate(importprices=ifelse(importprices==0,NA_real_,importprices))
}


## -------------------------------------------------------------------------------------------------------------------------------------
get_q.impfull_destatis<-function(){
  
data_import_2d<-map_df(2008:2024,function(x){
gen_cube("51000BM221", database = "genesis",startyear=x,endyear=x,contents="GEWE")%>%  
  mutate(nace2=sub("GP19-(.*)","\\1",GP19B2),
         level=2,
         countrycode=STLAH,
         ym=paste0(as.character(JAHR),sub("MONAT(.*)","\\1",MONAT)),
         date=ym(ym),
         importvolume=GEWE_WERT)%>%
  select(nace2,countrycode,level,date,importvolume)  
})


data_import_4d1<-map_df(2008:2014,function(x){
gen_cube("51000BM241", database = "genesis",startyear=x,endyear=x,contents="GEWE")%>%  
  mutate(nace2=sub("GP19-(.*)","\\1",GP19B4),
         level=4,
         countrycode=STLAH,
         ym=paste0(as.character(JAHR),sub("MONAT(.*)","\\1",MONAT)),
         date=ym(ym),
         importvolume=GEWE_WERT)%>%
  select(nace2,countrycode,level,date,importvolume)  
})

data_import_4d2<-map_df(2015:2024,function(x){
gen_cube("51000BM241", database = "genesis",startyear=x,endyear=x,contents="GEWE")%>%  
  mutate(nace2=sub("GP19-(.*)","\\1",GP19B4),
         level=4,
         countrycode=STLAH,
         ym=paste0(as.character(JAHR),sub("MONAT(.*)","\\1",MONAT)),
         date=ym(ym),
         importvolume=GEWE_WERT)%>%
  select(nace2,countrycode,level,date,importvolume)  
})

data_import<-rbind(data_import_2d,data_import_4d1,data_import_4d2)

countrycodes<-readxl::read_excel("Data/code_countries_trade.xlsx")
data_import2<-left_join(data_import,countrycodes,by="countrycode")

return(data_import2)
}

gen_q.imp_agg<-function(data){
  
data%>%
  select(nace2,date,country_group,importvolume)%>%
  mutate(year=year(date))%>%# for scaling (mean 2021 = 100)
  group_by(nace2,date,country_group)%>%
  summarise(
    import_sum=sum(importvolume,na.rm=T))%>% # importvolume
  ungroup()%>%
  # gen sum world
  pivot_wider(names_from = country_group,values_from = import_sum)%>%
  mutate(TOT=EU+ROW)%>%
  pivot_longer(-c(nace2,date),names_to = "country_group",values_to = "import_sum")%>%
  group_by(nace2,country_group)%>%
  mutate(
    import_avg_2021=mean(import_sum[year(date) == 2021], na.rm = TRUE),
    import_avg_2021=case_when(import_avg_2021==0~NA_real_,
                          TRUE~import_avg_2021))%>%
  ungroup()%>%
  mutate(import=100*import_sum/import_avg_2021)%>%
  select(-c(import_sum,import_avg_2021))%>%
  mutate(import=ifelse(import==0,NA_real_,import))%>%
  pivot_wider(values_from = import,names_from = country_group,names_prefix = "import")


}




#normalize
# data_import3<-data_import2%>%
#   group_by(nace2,countrycode)%>%
#   mutate(
#     importprice_avg_2021=mean(importvolume[year(date) == 2021], na.rm = TRUE),
#     importprice_avg_2021=case_when(importprice_avg_2021==0~NA_real_,
#                            TRUE~importprice_avg_2021),
#     importprice_normalized = 100*importvolume / importprice_avg_2021
#   )%>%
#   ungroup()%>%
#   select(-importprice_avg_2021)


## -------------------------------------------------------------------------------------------------------------------------------------
do_seasonadj<-function(x,date){
      
  
  
          vals <- x
        dates <- date
        
        # Identify the indices of non-NA values
        non_na_idx <- which(!is.na(vals))
      
        # need 2 or more non-NA
         if (length(non_na_idx) < 2) {
    return(vals*NA) 
  }

        # Extract the non-NA segment
        vals_sub <- vals[non_na_idx]
        dates_sub <- dates[non_na_idx]
    
    # differ between monthly and quarterly series    
     if(mean(diff(non_na_idx))-1<1e-6){
        ts_data <- ts(vals_sub, start = c(year(dates_sub[1]), month(dates_sub[1])), frequency = 12)
     }else if(mean(diff(non_na_idx))-3<1e-6){
       ts_data <- ts(vals_sub, start = c(year(dates_sub[1]), month(dates_sub[1])), frequency = 4)
     }else{
       return(vals*NA)
     }
      
        # Try running x13; if it fails, return the original NA vector
        sa_result <- tryCatch({
      x13(ts_data)
        }, error = function(e) {
          return(NULL)
        })
        
        if (is.null(sa_result)) {
          # If x13 fails, return NAs
          return(vals * NA)
        } else {
          # Extract the final seasonally adjusted series
          sa_vals_sub <- sa_result$final$series[,2]
        
          res <- rep(NA_real_, length(vals))
          res[non_na_idx] <- sa_vals_sub
          return(res)
        } 
}



## -------------------------------------------------------------------------------------------------------------------------------------
get_matching<-function(matching_base,p.indu,q.indu,p.serv,q.serv,pimp,qimp){
  
dates <-  seq(as.Date("1991-01-01"), floor_date(Sys.Date(), unit = "month"), by = "month") 
expanded_dataset <- expand.grid(nace2 = matching_base$nace2, date = dates)
  
data<-expanded_dataset%>%
  left_join(matching_base,by="nace2")%>%
  #Production
  left_join(q.indu,by=c("nace2","date"))%>%
  left_join(q.serv,by=c("nace2","date"))%>%
  mutate(production=case_when(!is.na(production.x)~production.x, # get production in one 
                        !is.na(production.y)~production.y,
                        TRUE~NA_real_))%>%
  #Price
  left_join(p.indu,by=c("nace2","date"))%>%
  left_join(p.serv,by=c("nace2","date"))%>%
  mutate(producerprices=case_when(!is.na(producerprices.x)~producerprices.x, #get prices in one
                        !is.na(producerprices.y)~producerprices.y,
                        TRUE~NA_real_))%>%
  select(-c(production.x,production.y,producerprices.x,producerprices.y))%>%
  # Imports
  left_join(pimp%>%select(-level),by=c("nace2","date"))%>%
  left_join(qimp,by=c("nace2","date"))  

return(data)
}


## -------------------------------------------------------------------------------------------------------------------------------------
gen_transformations<-function(data){
 data%>%
  group_by(nace2)%>%
  arrange(date)%>%
  mutate(
    across(c(production,producerprices,importprices,importEU,importROW,importTOT),~do_seasonadj(.x,date),.names = "{.col}_sa"))%>%
  rename(production_nsa=production,producerprices_nsa=producerprices,importprices_nsa=importprices,importEU_nsa=importEU,importROW_nsa=importROW,importTOT_nsa=importTOT)%>%
  mutate(
    across(c(production_nsa,producerprices_nsa,importprices_nsa,importEU_nsa,importROW_nsa,importTOT_nsa),~round(100*(./lag(.,12)-1),1),.names = "{.col}_c12m"),
    across(c(production_nsa,producerprices_nsa,importprices_nsa,importEU_nsa,importROW_nsa,importTOT_nsa),~round(100*(./lag(.,1)-1),1),.names = "{.col}_c1m"),
    across(c(production_sa,producerprices_sa,importprices_sa,importEU_sa,importROW_sa,importTOT_sa),~round(100*(./lag(.,1)-1),1),.names = "{.col}_c1m"),
    across(c(production_sa,producerprices_sa,importprices_sa,importEU_sa,importROW_sa,importTOT_sa),~round(100*((.+lag(.,1)+lag(.,2))/(lag(.,12)+lag(.,12+1)+lag(.,12+2))-1),1),.names = "{.col}_c12mr3"))%>%
  ungroup()%>%
    rename(production_nsa_level=production_nsa,
           producerprices_nsa_level=producerprices_nsa,
           importprices_nsa_level=importprices_nsa,
           importEU_nsa_level=importEU_nsa,
           importROW_nsa_level=importROW_nsa,
           importTOT_nsa_level=importTOT_nsa,
           production_sa_level=production_sa,
           producerprices_sa_level=producerprices_sa,
           importprices_sa_level=importprices_sa,
           importEU_sa_level=importEU_sa,
           importROW_sa_level=importROW_sa,
           importTOT_sa_level=importTOT_sa)%>%
    pivot_longer(-c(nace2,date,category,nace2_main_code,nace2_main_name,level,nace2_sub_name),names_pattern = "(.*)_(.*)_(.*)",names_to = c("vars","adj","transf"),values_to = "values")%>%
    pivot_wider(names_from = "vars",values_from = "values")

}


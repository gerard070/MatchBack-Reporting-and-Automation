


library(dplyr)
library(xlsx)

#----------------------------------------------------------------------------------------------------------------------------------------------------
#Start Setting Up Last Years Dataframe
#----------------------------------------------------------------------------------------------------------------------------------------------------
#load in 2018 Fall Model Scores

#server File Paths
Fall_2018_Model_Scores <- read.csv("/opt/mnt/publicdrive/Analytics/Gerard/Matchback/MatchBack_8G_and_7e/Data/Fall_2018_July_Housefile_Scores.csv")
Fall_2017_Model_Scores_Full <- read.csv("/opt/mnt/publicdrive/Analytics/Gerard/Matchback/MatchBack_8G_and_7e/Data/Drop7E_Housefiles_New_2017-06-13.csv")
Fall_2017_Model_Scores = subset(Fall_2017_Model_Scores_Full, select = -(deciles))
head(Fall_2017_Model_Scores)

#Local File Paths
#Fall_2018_Model_Scores <- read.csv("/Volumes/ugcompanystorage/Company/public/Analytics/Catalog/Fall2018/Fall_2018_July_Housefile_Scores.csv")
#Fall_2017_Model_Scores <- read.csv("/Volumes/ugcompanystorage/Company/public/Analytics/Catalog/Fall2017/Drop7E_Housefiles_New_2017-06-13.csv")
#Calculate Avarage Model Score to assign to any value missing from last year mail promos
avg_Model_Score = mean(Fall_2017_Model_Scores$scores)

#Load in 2017 Mail Promo Data
#Server File Path
Ly_Mail_Promos <- read.csv("/opt/mnt/publicdrive/Analytics/Gerard/Matchback/MatchBack_8G_and_7e/Data/7E_Mail_Promos.csv")
nrow(Ly_Mail_Promos)




#Local File path
#Ly_Mail_Promos <- read.csv("/Volumes/ugcompanystorage/Company/public/Analytics/Gerard/Matchback/MatchBack_8G_and_7e/Data/7E_Mail_Promos.csv")




#books = Ly_Mail_Promos %>% 
#mutate(list=as.numeric(as.character(list))) %>% 
#group_by (list) %>%
#summarise(books = count(list))



books_groups <- Ly_Mail_Promos %>% 
  group_by(list)

books = count(books_groups, list)
books

colnames(books) <- c("list", "Books")
books

Books_with_Description <- books%>% 
  mutate(description = ifelse(any(list %in% "1"),
                              as.character("House_Decile")[list %in% "1"],
                              ifelse(any(list %in% "2"),
                                     as.character("House_Decile")[list %in% "2"],
                                     ifelse(any(list %in% "3"),
                                            as.character("House_Decile")[list %in% "3"],
                                            ifelse(any(list %in% "4"),
                                                   as.character("House_Decile")[list %in% "4"],
                                                   ifelse(any(list %in% "5"),
                                                          as.character("House_Decile")[list %in% "5"],
                                                          ifelse(any(list %in% "6"),
                                                                 as.character("House_Decile")[list %in% "6"],
                                                                 ifelse(any(list %in% "7"),
                                                                        as.character("House_Decile")[list %in% "7"],
                                                                        ifelse(any(list %in% "8"),
                                                                               as.character("House_Decile")[list %in% "8"],
                                                                               ifelse(any(list %in% "9"),
                                                                                      as.character("House_Decile")[list %in% "9"],
                                                                                      ifelse(any(list %in% "0"),
                                                                                             as.character("House_Decile")[list %in% "0"],
                                                                                            ifelse(any(list %in% "8101"),
                                                                                                  as.character("Abacus ONE Seg. 1")[list %in% "8101"],
                                                                                                  ifelse(any(list %in% "8102"),
                                                                                                        as.character("Abacus ONE")[list %in% "8102"],
                                                                                                        ifelse(any(list %in% "8201"),
                                                                                                              as.character("DataLogix NextGen Seg. 1")[list %in% "8201"],
                                                                                                              ifelse(any(list %in% "8202"),
                                                                                                                    as.character("DataLogix Binning Seg. 1")[list %in% "8202"],
                                                                                                                    ifelse(any(list %in% "8301"),
                                                                                                                          as.character("Wiland Direct  Comprehensive Response Seg. 1")[list %in% "8301"],
                                                                                                                          ifelse(any(list %in% "8401"),
                                                                                                                           as.character("Path2Response")[list %in% "8401"],
                                                                                                                          "NA")))))))))))))))))







#Join LYMailPromos to FDAY MODELS
Ly_Mail_Promos_Plus_Model_Scores = Ly_Mail_Promos %>% 
  left_join(Fall_2017_Model_Scores,  by =c('orig_hlink'= 'hlink'))
nrow(Ly_Mail_Promos_Plus_Model_Scores)
#Ly_Mail_Promos_Plus_Model_Scores

#fill in the blanks for any missing orig hlinks with average scores, and missing Deciles with 'X'
Ly_Mail_Promos_Plus_Model_Scores_NA_Replaced_W_Avg_Score = Ly_Mail_Promos_Plus_Model_Scores %>% 
  mutate(scores = replace(scores, is.na(scores), avg_Model_Score)) 


#Remove duplicate hlinks
Ly_Mail_Promos_Plus_Avg_Model_Score_Curr_Hlink_Grouped = Ly_Mail_Promos_Plus_Model_Scores_NA_Replaced_W_Avg_Score%>% 
  group_by (current_hlink)%>%
  summarise(list_number = max(list_number))

#join Mail Promos columns with deduped hlinks to return Mail Promos without duplicate Hlinks
Ly_Mail_Promos_Plus_Avg_Model_Score_Curr_Hlink_Deduped = Ly_Mail_Promos_Plus_Avg_Model_Score_Curr_Hlink_Grouped %>% 
  left_join(Ly_Mail_Promos_Plus_Model_Scores_NA_Replaced_W_Avg_Score, by = c('current_hlink' = 'current_hlink', 'list_number' = 'list_number'))

#Segment is included in LY_Day_Orders as well, so this will remove the duplicate column
Ly_Mail_Promos_Plus_Avg_Model_Score_Curr_Hlink_Deduped = subset(Ly_Mail_Promos_Plus_Avg_Model_Score_Curr_Hlink_Deduped, select = -(segment))


#load in ly order (Server File Path)										
Ly_Fall_Orders = read.csv("/opt/mnt/publicdrive/Analytics/Gerard/Matchback/MatchBack_8G_and_7e/Data/7E_Orders.csv")
head(Ly_Fall_Orders)
nrow(Ly_Fall_Orders)
#load in ly order (Local File Path)										
#Ly_Fall_Orders = read.csv("/Volumes/ugcompanystorage/Company/public/Analytics/Gerard/Matchback/MatchBack_8G_and_7e/Data/7E_Orders.csv")


#Join Orders Data with Deduped Mail Promo Data
Last_Year_Fall_Orders_Dataframe = Ly_Fall_Orders %>%
  left_join(Ly_Mail_Promos_Plus_Avg_Model_Score_Curr_Hlink_Deduped, by =c('hlink'= 'current_hlink') ) %>%
  filter(!is.na(list_number))
colnames(Last_Year_Fall_Orders_Dataframe) <- c("segment", "orders","sale_skus", "cost_skus", "mb_program", "hlink", "order_date", "list_number", "descrip", "orig_hlink", "maildate","List", "score")
head(Last_Year_Fall_Orders_Dataframe)
nrow(Last_Year_Fall_Orders_Dataframe)



Last_Year_Fall_Orders_Dataframe <- Last_Year_Fall_Orders_Dataframe %>%
  mutate(
    order_date=as.Date(order_date)
    #order_date=as.Date(order_date,"%m/%d/%y")
  )


head(Last_Year_Fall_Orders_Dataframe)
nrow(Last_Year_Fall_Orders_Dataframe)



Last_Year_Fall_Orders_Dataframe <- Last_Year_Fall_Orders_Dataframe %>%
   group_by(orders) %>%
   slice(1) %>%
   ungroup()

head(Last_Year_Fall_Orders_Dataframe)
nrow(Last_Year_Fall_Orders_Dataframe)
#str(Last_Year_Fall_Orders_Dataframe)
#Dr. Ken Attribution Scores Server File Path
source("/opt/mnt/publicdrive/Analytics/Ken/Attribution_Model/Matchback_Functions/R/Catalog_Matchback.R")

#Dr. Ken Attribution Scores Local
#source("/Volumes/ugcompanystorage/Company/public/Analytics/Ken/Attribution_Model/Matchback_Functions/R/Catalog_Matchback.R")

#Pass last year Fday orders df to Dr. Kens Attribution Function to ad attribution column

Catalog_Attribution_Rate_DF = 
Last_Year_Final_DF = mutate(Last_Year_Fall_Orders_Dataframe, attribution = catalog_attribution_rate(Last_Year_Fall_Orders_Dataframe), order_sale_attribution = (attribution * sale_skus), 
                            sku_cost_attribution = attribution *cost_skus)
head(Last_Year_Final_DF)



ly_Totals_DF = Last_Year_Final_DF %>%
  left_join(Books_with_Description, by =c("List" = "list"))


Last_Year_Summary_Statistics_Columns = 
  
  ly_Totals_DF[,c("segment","attribution","sale_skus", "cost_skus", "List", "order_sale_attribution", "sku_cost_attribution", "Books", "description")] %>%
  #gift certificates are causing problems bc sku cost is NA, so we are removing them for now
  filter(!is.na(cost_skus))


#Mailing Costs for Last Year

Ly_House_Mailing_Cost = 0.423
Ly_Coops_Mailing_Cost = 0.480


#Get Groupings right and calculate some basic summary statiscit
Last_Year_Summary_Statistics = Last_Year_Summary_Statistics_Columns %>% 
  group_by (segment, description, List, Books) %>% 
  summarize(Total_orders = sum(attribution), SKU_Sales = sum(sale_skus), "Sku Costs"= sum(cost_skus), "Allocated_Sales" = sum(order_sale_attribution), 
            Allocated_Sku_Cost = sum(sku_cost_attribution), "Revenue Share" = (Allocated_Sales/(sum(Allocated_Sales))))
head(Last_Year_Summary_Statistics)


#Calculate summary statistics


#Calculate A/S based on catalog costs
Last_Year_Summary_Statistics = mutate(Last_Year_Summary_Statistics,
                                      "A/S" = ifelse(segment %in% "Coops", as.numeric((Books*Ly_Coops_Mailing_Cost)/Allocated_Sales),
                                                     ifelse(segment %in% "House_List", as.numeric((Books*Ly_House_Mailing_Cost)/Allocated_Sales),
                                                            ifelse(segment %in% "NA", "NA" ))
                                                     
                                      ))
#Calculate RR
Last_Year_Summary_Statistics = mutate(Last_Year_Summary_Statistics,
                                      RR = (Total_orders/Books)
)

#Add In Cost Per Book
Last_Year_Summary_Statistics = mutate(Last_Year_Summary_Statistics,
                                      "Cost/Book" = ifelse(segment %in% "Coops", as.numeric(Ly_Coops_Mailing_Cost),
                                                           ifelse(segment %in% "House_List", as.numeric(Ly_House_Mailing_Cost),
                                                                  ifelse(segment %in% "NA", 0/0, 0/0))
                                                           
                                                           
                                      ))

#Add in rev/book, Gross Benefit per book
str(Last_Year_Summary_Statistics)
Last_Year_Summary_Statistics = mutate(Last_Year_Summary_Statistics,
                                      "Revenue Per Book" = (SKU_Sales/Books), "Gross Benefit Per Book" = (Allocated_Sales)*0.439/(Books)
)





#Write to file on server
write.xlsx2(Last_Year_Summary_Statistics %>% as.data.frame(),
            file = "/opt/mnt/publicdrive/Analytics/Matchback/fall2018/7E_and_8G/data/Last_Year_Summary_Statistics_7E.xlsx", sheetName="Last_Year_Summary_Statistics.cs", row.names = FALSE)

#Write to file on Local
#write.xlsx2(Last_Year_Summary_Statistics %>% as.data.frame(),
            #file = "/Volumes/ugcompanystorage/Company/public/Analytics/Matchback/mday2018/7E_and_8F/data/Last_Year_Summary_Statistics.xlsx", sheetName="Last_Year_Summary_Statistics.cs", row.names = FALSE)
#Ly_Mail_Promos_Plus_Model_Scores <- merge(FDay_Model_Scores, Ly_Mail_Promos, by.x ="hlink", by.y ="orig_hlink")




#----------------------------------------------------------------------------------------------------------------------------------------------------
#End Setting Up Last Years Dataframe
#----------------------------------------------------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------------------------------------------
#Start Setting Up This Years Dataframe
#----------------------------------------------------------------------------------------------------------------------------------------------------

#Load in 2018 Mail Promo Data
Ty_Mail_Promos <- read.csv("/opt/mnt/publicdrive/Analytics/Gerard/Matchback/MatchBack_8G_and_7e/Data/8G_Mail_Promos.csv")


#Get Book Counts to merge to final DF later
TY_books_groups <- Ty_Mail_Promos %>% 
  group_by(list)

TY_books = count(TY_books_groups, list)
colnames(TY_books) <- c("list", "Books")

TY_books



TY_Books_with_Description <- TY_books%>% 
  #filter(!is.na(list))%>% 
  mutate(description = ifelse(any(list %in% "0"),
                              as.character("House_Decile")[list %in% "0"],
                              ifelse(any(list %in% "1"),
                                     as.character("House_Decile")[list %in% "1"],
                                     ifelse(any(list %in% "2"),
                                            as.character("House_Decile")[list %in% "2"],
                                            ifelse(any(list %in% "3"),
                                                   as.character("House_Decile")[list %in% "3"],
                                                   ifelse(any(list %in% "4"),
                                                          as.character("House_Decile")[list %in% "4"],
                                                          ifelse(any(list %in% "5"),
                                                                 as.character("House_Decile")[list %in% "5"],
                                                                 ifelse(any(list %in% "6"),
                                                                        as.character("House_Decile")[list %in% "6"],
                                                                        ifelse(any(list %in% "7"),
                                                                               as.character("House_Decile")[list %in% "7"],
                                                                               ifelse(any(list %in% "8"),
                                                                                      as.character("House_Decile")[list %in% "8"],
                                                                                      ifelse(any(list %in% "9"),
                                                                                             as.character("House_Decile")[list %in% "9"],
                                                                                             ifelse(any(list %in% "4001"),
                                                                                                    as.character("Abacus Non Buyers Seg 1")[list %in% "4001"],
                                                                                                    ifelse(any(list %in% "8101"),
                                                                                                          as.character("Abacus ONE Seg. 1")[list %in% "8101"],
                                                                                                          ifelse(any(list %in% "8102"),
                                                                                                                as.character("Abacus ONE Seg. 2")[list %in% "8102"],
                                                                                                                ifelse(any(list %in% "8103"),
                                                                                                                      as.character("Abacus ONE Seg. 3")[list %in% "8103"],
                                                                                                                      ifelse(any(list %in% "8201"),
                                                                                                                            as.character("DataLogix NextGen Seg. 1")[list %in% "8201"],
                                                                                                                            ifelse(any(list %in% "8401"),
                                                                                                                                  as.character("Wiland Direct Seg 1")[list %in% "8401"],
                                                                                                                                  ifelse(any(list %in% "8501"),
                                                                                                                                        as.character("Path2Response Seg 1")[list %in% "8501"],
                                                                                                                                        ifelse(any(list %in% "X"),
                                                                                                                                              as.character("House_Decile")[list %in% "X"],
                                                                                                                                              ifelse(list %in% "8301",
                                                                                                                                                    as.character("DataLogix Binning Seg 1")[list %in% "8301"], "NA") 
                                                                                      
                                                                                                                                                    )))))))))))))))))))




#Join TYMailPromos to 2018 FDAY MODELS
Ty_Mail_Promos_Plus_Model_Scores = Ty_Mail_Promos %>% 
  left_join(Fall_2018_Model_Scores,  by =c('orig_hlink'= 'hlink'))


#Calc 2018 average model score to fill in blanks
avg_Model_Score_2018 = mean(Fall_2018_Model_Scores$scores)

#fill in the blanks for any missing orig hlinks with average scores
Ty_Mail_Promos_Plus_Model_Scores_NA_Replaced_W_Avg_Score = Ty_Mail_Promos_Plus_Model_Scores %>% 
  mutate(scores = replace(scores, is.na(scores), avg_Model_Score_2018)) 



#Remove duplicate hlinks
Ty_Mail_Promos_Plus_Avg_Model_Score_Curr_Hlink_Grouped = Ty_Mail_Promos_Plus_Model_Scores_NA_Replaced_W_Avg_Score%>% 
  group_by (current_hlink)%>%
  summarise(list_number = max(list_number))


#join Mail Promos columns with deduped hlinks to return Mail Promos without duplicate Hlinks
Ty_Mail_Promos_Plus_Avg_Model_Score_Curr_Hlink_Deduped = Ty_Mail_Promos_Plus_Avg_Model_Score_Curr_Hlink_Grouped %>% 
  left_join(Ty_Mail_Promos_Plus_Model_Scores_NA_Replaced_W_Avg_Score, by = c('current_hlink' = 'current_hlink', 'list_number' = 'list_number'))


#Segment is included in TY_Day_Orders as well, so this will remove the duplicate column
Ty_Mail_Promos_Plus_Avg_Model_Score_Curr_Hlink_Deduped = subset(Ty_Mail_Promos_Plus_Avg_Model_Score_Curr_Hlink_Deduped, select = -(segment))



#load in Ty order  										
Ty_Fall_Orders = read.csv("/opt/mnt/publicdrive/Analytics/Gerard/Matchback/MatchBack_8G_and_7e/Data/8G_Orders.csv")


#Join Orders Data with Deduped Mail Promo Data
This_Year_Fall_Orders_Dataframe = Ty_Fall_Orders %>%
  left_join(Ty_Mail_Promos_Plus_Avg_Model_Score_Curr_Hlink_Deduped, by =c('hlink'= 'current_hlink') ) %>%
  filter(!is.na(list_number))
colnames(This_Year_Fall_Orders_Dataframe) <- c("segment", "orders","sale_skus", "cost_skus", "mb_program", "hlink", "order_date", "list_number", "descrip", "orig_hlink", "maildate","List", "score", "Decile")



#Bring in Dr. Kens attribution function and add to this years orders
This_Year_Final_DF = mutate(This_Year_Fall_Orders_Dataframe, attribution = catalog_attribution_rate(This_Year_Fall_Orders_Dataframe), order_sale_attribution = (attribution * sale_skus), 
                            sku_cost_attribution = attribution *cost_skus)



Ty_Totals_DF = This_Year_Final_DF %>%
  left_join(TY_Books_with_Description, by =c("List" = "list"))
head(Ty_Totals_DF)



This_Year_Summary_Statistics_Columns = 
  
  Ty_Totals_DF[,c("segment","attribution","sale_skus", "cost_skus", "List", "order_sale_attribution", "sku_cost_attribution", "Books", "description")] %>%
  #gift certificates are causing problems bc sku cost is NA, so we are removing them for now
  filter(!is.na(cost_skus))
head(This_Year_Summary_Statistics_Columns)
#This Years Mailing Costs

Ty_House_Mailing_Cost = 0.464
Ty_Coops_Mailing_Cost = 0.491

#Get Groupings right and calculate some basic summary statistic
This_Year_Summary_Statistics = This_Year_Summary_Statistics_Columns %>% 
  group_by (segment, description, List, Books) %>% 
  summarize(Total_orders = sum(attribution), SKU_Sales = sum(sale_skus), "Sku Costs"= sum(cost_skus), "Allocated_Sales" = sum(order_sale_attribution), 
            Allocated_Sku_Cost = sum(sku_cost_attribution), "Revenue Share" = (Allocated_Sales/(sum(Allocated_Sales)))) %>%
  filter((segment=="Coops" & description!="House_Decile") | (segment=="House_List" & description=="House_Decile"))



#Calculate A/S based on catalog costs
This_Year_Summary_Statistics = mutate(This_Year_Summary_Statistics,
                                      "A/S" = ifelse(segment %in% "Coops", as.numeric((Books*Ty_Coops_Mailing_Cost)/Allocated_Sales),
                                                     ifelse(segment %in% "House_List", as.numeric((Books*Ty_House_Mailing_Cost)/Allocated_Sales),
                                                            ifelse(segment %in% "NA", "NA" ))
                                                     
                                      ))

#Calculate RR
This_Year_Summary_Statistics = mutate(This_Year_Summary_Statistics,
                                      RR = (Total_orders/Books)
)


#Add In Cost Per Book
This_Year_Summary_Statistics = mutate(This_Year_Summary_Statistics,
                                      "Cost/Book" = ifelse(segment %in% "Coops", as.numeric(Ty_Coops_Mailing_Cost),
                                                           ifelse(segment %in% "House_List", as.numeric(Ty_House_Mailing_Cost),
                                                                  ifelse(segment %in% "NA", 0/0, 0/0))
                                                           
                                                           
                                      ))

#Add in rev/book, Gross Benefit per book
This_Year_Summary_Statistics = mutate(This_Year_Summary_Statistics,
                                      "Revenue Per Book" = (SKU_Sales/Books), "Gross Benefit Per Book" = (Allocated_Sales)*0.439/(Books)
)



write.xlsx2(This_Year_Summary_Statistics %>% as.data.frame(),
            file = "/opt/mnt/publicdrive/Analytics/Matchback/fall2018/7E_and_8G/data/This_Year_Summary_Statistics_8G.xlsx", sheetName="This_Year_Summary_Statistics.cs", row.names = FALSE)



































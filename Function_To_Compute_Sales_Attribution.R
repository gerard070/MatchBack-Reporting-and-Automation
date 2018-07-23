##############################################################
###
### Function for implementing catalog matchback allocation ###
###
##############################################################

### Required Libraries
library(dplyr)
library(boot)
library(tidyr)

### Load GLM model files
load("~/Analytics/Ken/Attribution_Model/Matchback_Functions/Models/House_Attribution_Model.Rsave")
load("~/Analytics/Ken/Attribution_Model/Matchback_Functions/Models/Prospect_Attribution_Model.Rsave")

### Define program categories
high_catalog <- c("CUSTOMER CATALOG","PHONE")
mid_catalog <- c("NON-BRANDED ORGANIC","BRANDED SEARCH","DIRECT","UNKNOWN SEARCH")
low_catalog <- c("REFERRALS","EMAIL","NON-BRANDED CPC")

### Main function

catalog_attribution_rate <- function( order_data ) {

			 ### Add general columns
			 order_data <- order_data %>%
			 	    mutate(
					#days=as.numeric(as.Date(order_date,"%m/%d/%y")-as.Date(maildate,"%m/%d/%y")),
					days=as.numeric(as.Date(order_date)-as.Date(maildate)),
					decay=-log(abs(days-2)^.5+1),
					score=if_else(score>.4,.4,score),
					score=if_else(is.na(score),mean(.$score,na.rm=T),score),
					score=logit(score),
					program="other",
					program=if_else(mb_program %in% high_catalog,"high",program),
					program=if_else(mb_program %in% mid_catalog,"mid",program),
					program=if_else(mb_program %in% low_catalog,"low",program)
				    )

			 ### Divide into house and prospect
			 house <- order_data %>%
			       filter(
				segment=="House_List"
			       )

			 prospect <- order_data %>%
			       filter(
				segment!="House_List"
			       )

			 ### Add predictions
			 house <- house %>%
			       mutate(
				status="mailed"
			       ) %>%
			       mutate(
				mailed=predict(glm.house,newdata=.) %>% inv.logit()
			       ) %>%
			       mutate(
				status="holdout",
				decay=0
			       ) %>%
			       mutate(
				unmailed=predict(glm.house,newdata=.) %>% inv.logit(),
				attribution=if_else(mailed>unmailed, 1-unmailed/mailed, 0)
			       ) %>%
			       select(hlink,orders,attribution)

			 prospect <- prospect %>%
			       mutate(
				status="mailed"
			       ) %>%
			       mutate(
				mailed=predict(glm.house,newdata=.) %>% inv.logit()
			       ) %>%
			       mutate(
				status="holdout",
				decay=0
			       ) %>%
			       mutate(
				unmailed=predict(glm.house,newdata=.) %>% inv.logit(),
				attribution=if_else(mailed>unmailed, 1-unmailed/mailed, 0)
			       ) %>%
			       select(hlink,orders,attribution)

			 #print(order_data)
			 #print(house,n=10)
			 #print(prospect,n=10)

			 ### Recombine house and prospect
			 order_data <- order_data %>%
			 	    select(hlink,orders) %>%
				    left_join(
					dplyr::union(
						house %>% select(hlink,orders,attribution),
						prospect %>% select(hlink,orders,attribution)
					),
					by=c("hlink"="hlink","orders"="orders")
				    )

			 ### Return attribution rate
			 order_data$attribution
}
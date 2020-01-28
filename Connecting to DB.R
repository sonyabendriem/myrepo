library("dplyr")
library("ggplot2")
library("ggthemes")
library("reshape2")
library("scales")
library("stringr")
library("modes")
library("RPostgreSQL")
theme_set(theme_fivethirtyeight())

#connect to the DB: NO PERFORMANCE DATA

#```{r,echo = TRUE,warning = TRUE}
pgsql = PostgreSQL()
#con = dbConnect(pgsql,host = "tim-snapshot-2018-03-08-10-06.ccwpaoumfdye.us-east-1.rds.amazonaws.com",port = 5432,user = "aptology",dbname = "aptology")
con = dbConnect(pgsql,host = "snapshot-2018-05-16-10-06.ccwpaoumfdye.us-east-1.rds.amazonaws.com",port = 5432,user = "aptology",dbname = "aptology", password="xXePPfHZuYRcsHKvnbpKJEeZdtjJtz2f")
print(con)
#```

#chose the peer group by talent pool ID ("id1|id2" for multiple)

#```{r}
mygrp = "bf088db1-4db7-4a43-839a-bcd7fc62a8ef"
#```

#load existing data

#```{r, message=TRUE, warning=TRUE}
qry = "select 
first_name || ' ' || last_name as name, 
c.score, c.quartile as quad,
display_sequence as item, 
q.external_id as label,
tr.name as trait,
f.name as factor,
qt.sign as sign,
answer_numeric as value
from talent_prospect as t
join talent_pool as tp
on tp.talent_pool_id = t.talent_pool_id
join network_member as n 
on n.network_member_id = t.network_member_id
join account as a 
on a.account_id = n.account_id 
left join current_performance_score as c 
on c.network_member_id = n.network_member_id
join survey_response as s 
on s.network_member_id = n.network_member_id
and s.survey_id = tp.survey_id 
join survey_question_response as sqr 
on sqr.survey_response_id = s.survey_response_id 
join survey_question as sq 
on sq.survey_id = s.survey_id 
and sq.question_id = sqr.question_id 
join question as q 
on q.question_id = sq.question_id 
join question_trait as qt
on qt.question_id = q.question_id 
join trait as tr
on tr.trait_id = qt.trait_id 
join factor as f
on f.factor_id = tr.factor_id 
where t.talent_pool_id similar to '%("
qry = paste0(qry,mygrp,")%';")
#print(paste0("Sending the following query to the DB: ",qry,";"))
model.data = dbGetQuery(con,qry)
dim(model.data)
print(paste("# of individuals:",length(unique(model.data$name))))
# temporary — to filter down to original LSI traits
model.data = subset(model.data,factor %in% c("Execution","Process and Methods","Strategy","Information Gathering","Relationships"))


# score attributes (3+ 3- items for each)
temp = temp %>% group_by(agent.name,quad,score,attribute) %>% summarise(at_score = mean(ifelse(sign.x > 0,value,11-value),na.rm = TRUE)) %>% as.data.frame()
# standardize scores by attribute
temp = temp %>% group_by(attribute) %>% mutate(newval = scale(at_score))
# cleanup
temp$newval = as.numeric(temp$newval)
temp$quad = as.factor(temp$quad)
temp = as.data.frame(temp)
print(head(temp,1))

# record peer group stats by trait
mystats = temp %>% na.omit() %>% group_by(attribute) %>% summarise(m = mean(at_score),sd = sd(at_score),s = skewness(at_score),k = kurtosis(at_score,TRUE),b = bimodality_coefficient(at_score)) %>% arrange(attribute) %>% as.data.frame()
mystats[-1] = round(mystats[-1],2)

# count deviations above 2.5 and remove individuals with > 4 deviations
tmp = temp %>% group_by(agent.name,quad,score) %>% tally(abs(newval) > 2.5) %>% dplyr::filter(n > 4)
if (nrow(tmp) > 0) 
{
  print(tmp)
  temp = temp %>% dplyr::filter(!(agent.name %in% intersect(tmp$agent.name,temp$agent.name)))
}
# otherwise, just remove those outlier scores from corresponding traits
#temp = temp %>% dplyr::filter(abs(newval) <= 2.5) %>% as.data.frame()
# rescale attributes
temp = temp %>% group_by(attribute) %>% mutate(at_score = as.numeric(scale(at_score))) %>% as.data.frame()
temp$at_score = as.numeric(temp$at_score)
temp$quad = as.factor(temp$quad)
print(head(temp,1))

print(paste("# of individuals:",length(unique(temp$agent.name))))

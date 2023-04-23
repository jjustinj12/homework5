if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, stringr, readxl, data.table, gdata)
library(tidyverse)
library(scales)
library(knitr)
library(rdrobust)
library(modelsummary)
library(AER)
library(fixest)
library(acs)

final.data<-read_tsv('data/output/acs_medicaid.txt')
colnames(final.data)

#1 
Q1<- final.data %>%
  group_by(year)%>%
  summarize(direct_insured=mean(ins_direct/adult_pop, na.rm=TRUE))%>%
  ggplot(aes(x=year, y=direct_insured))+geom_line() + theme_bw()+
labs(title = "Share of insured individuals with direct purchase health insurance from 2012-2019",
     x = "Year",
     y = "Share of individuals",) + theme_bw()
Q1

#2
#Because of the creation of the ACA marketplace there is a reduction in direct puchase health insurance for two reasons. One because of the ACA's regulations in making health insurance more equitable in terms of not price discriminating based on pre-exisiting health conditions. Because of this mandate some private health insurances had to increase their premimums to protect against this potential risk. In addition, another reason there may have been a decrease in direct purcahse health insurance is the fact that the ACA created a subsidezed market place for consumers to purchase helath insurance directly from private firms at a lower rate. Thus this alternatiev to direct purchase may be more cost effective for cosnumers and push them away from directly being from an insruance providerr and usingt he marketplace as a cheaper option.    
#3

Q3<- final.data %>%
  group_by(year)%>%
summarize(medicaid_insured=mean(ins_medicaid/adult_pop, na.rm=TRUE))%>%
  ggplot(aes(x=year, y=medicaid_insured))+geom_line() + geom_point() +
labs(title = "Share of insured individuals with medicaid from 2012-2019",
     x = "Year",
     y = "Share of individuals",) + theme_bw()
Q3

#4
not_expanded<-final.data%>%
  filter(year==2012)%>%
  filter(expand_ever==FALSE)%>%
  select(State)
not_expanded
expanded_states_2014<-final.data%>%
  filter(year==2012)%>%
  filter(! State %in% c("District of Columbia", "Puerto Rico"))%>%
  filter(expand_year==2014)%>%
  select(State)
view(expanded_states_2014)


### view(final.data %>% filter(!is.na(expand_year))) this tells me which states expanded 

figure_Q4 <- final.data %>% 
  filter(year >= 2012 & year <= 2019) %>%
  mutate(expanded = ifelse(State %in% not_expanded$State, "Not Expanded", 
                           ifelse(State %in% expanded_states_2014$State, "Expanded", NA))) %>%
  drop_na(expanded) %>%
  group_by(year, expanded) %>%
  summarize(avg_prop_uninsured = mean(uninsured / adult_pop, na.rm = TRUE)) %>%
  ggplot(aes(x = year, y = avg_prop_uninsured, color = expanded)) + 
  geom_line() +
  labs(title = "Average Proportion of Uninsured between expanded and not expanded states from 2012-2019",
       x = "Year",
       y = "Average Proportion of Uninsured") +
  geom_vline(xintercept = 2014, color = "red") +
  ylim(0, 0.25)
figure_Q4


#Q5  
colnames(final.data)
Q5 <- final.data %>%
  filter(! State %in% c("District of Columbia", "Puerto Rico"))%>%
  filter(!is.na(expand_ever) | expand_year==2014) %>%
  filter(year %in% c(2012, 2015)) %>%
  group_by(year, expand_ever) %>%
  summarize(avg_prop_uninsured = mean((uninsured / adult_pop)*100, na.rm = TRUE))

Q5_table<-Q5%>%
  pivot_wider(names_from = year, values_from = avg_prop_uninsured) %>%
  mutate(across(`2012`:`2015`, ~percent(.01 * .)), .keep = "unused") %>%
  rename("2012" = `2012`, "2015" = `2015`) %>%
  mutate(expand_ever = ifelse(expand_ever, "Expansion", "Non-Expansion")) %>%
  select(expand_ever, "2012", "2015")
#kable(Q5_table, digits = 3, caption = "Proportion of Uninusred in 2012 and 2015 for states that expanded and did not expanded")
Q5_table
#Q6

regression<-final.data%>%
  filter(State %in% c("Arizona", "Arkansas", "California", "Colorado", "Delaware", 
                      "District of Columbia", "Connecticut", "Hawaii", "Illinois", "Iowa", "Kentucky", 
                      "Maryland", "Massachusetts", "Michigan", "Minnesota", "Nevada", 
                      "New Hampshire", "New Jersey", "New Mexico", 
                      "New York", "North Dakota", 
                      "Ohio", "Oregon", "Rhode Island", "Vermont", "Washington", "West Virginia",
                      "Alabama", "Florida", "Georgia", "Kansas", "Mississippi", 
                      "North Carolina", "South Carolina", "South Dakota", "Tennessee",
                      "Texas", "Wisconsin", "Wyoming"))%>%
  mutate(group=ifelse(State %in% c("Arizona", "Arkansas", "California", "Colorado", "Delaware", 
                                   "District of Columbia", "Connecticut", "Hawaii", "Illinois", "Iowa", "Kentucky", 
                                   "Maryland", "Massachusetts", "Michigan", "Minnesota", "Nevada", 
                                   "New Hampshire", "New Jersey", "New Mexico","New York", "North Dakota", 
                                   "Ohio", "Oregon", "Rhode Island", "Vermont", "Washington", "West Virginia"), 
                      "Treatment", "Control"))%>%
  mutate(post=year>=2014) %>%
  mutate(prop_uninsured=uninsured/adult_pop)%>%
  mutate(treat=post*expand_ever)

Q6 <- lm(prop_uninsured ~post+group+post*group, data=regression)
modelsummary(Q6)

#############################################################
reg.data<-final.data %>%
  mutate(post=(year>=2014), 
         treat=post*expand_ever)%>%
  filter(is.na(expand_year) | expand_year==2014)

dd.est<-lm(p)


#Q7
Q7 <-  feols(prop_uninsured ~ treat | State + year, data=regression)
modelsummary(Q7)

#Q8
regression_all<-final.data%>%
  mutate(group=ifelse(State %in% c("Arizona", "Arkansas", "California", "Colorado", "Delaware", 
                                   "District of Columbia", "Connecticut", "Hawaii", "Illinois", "Iowa", "Kentucky", 
                                   "Maryland", "Massachusetts", "Michigan", "Minnesota", "Nevada", 
                                   "New Hampshire", "New Jersey", "New Mexico","New York", "North Dakota", 
                                   "Ohio", "Oregon", "Rhode Island", "Vermont", "Washington", "West Virginia"), 
                      "Treatment", "Control"))%>%
  mutate(post=year>=2014) %>%
  mutate(prop_uninsured=uninsured/adult_pop)%>%
  mutate(treat=post*expand_ever)
Q8<-  feols(prop_uninsured ~ treat | State + year, data=regression_all)
modelsummary(Q8)


#Q9

Q9 <- feols(prop_uninsured~i(year, expand_ever, ref=2013) | State + year,
            cluster=~State,
            data=regression)
modelsummary(Q9)

event.plot<-iplot(Q9, 
                  xlab='Time to treatment',
                  main='Event Study')

coef_df <- Q9 %>%
  tidy() %>%
  filter(term != "(Intercept)")
coef_df


Q9_figure<-ggplot(coef_df, aes(x = term, y = estimate, color = term)) +
  geom_errorbar(aes(ymin = as.numeric(estimate - std.error), ymax = as.numeric(estimate + std.error)), width = 0.2) +
  geom_line() +
  geom_point(aes(y = estimate))+
  scale_x_discrete(labels = c("2012", "2014", "2015", "2016", "2017", "2018", "2019")) +
  labs(x = "Year", y = "Coefficient Estimate & 95% CI", 
       title="Event Study", color = "Expansion Status") +
  theme_minimal()
Q9_figure


################# IN CLASS # 9 


#Q10
reg.data2<-final.data%>%
  mutate(prop_uninsured=uninsured/adult_pop)%>%
  mutate(time_to_treat=ifelse(expand_ever==TRUE, year-expand_year,-1),
         time_to_treat=ifelse(time_to_treat<=-4, -4, time_to_treat))
mod.twfe2<-feols(prop_uninsured~i(time_to_treat, expand_ever, ref=-1)| State + year, 
                 data=reg.data2)
Q10<-iplot(mod.twfe2, xlab='Time to treatment')

#MAKE THE MAIN THE FIG CAPTION 
out.width



# Q10 <- feols(prop_uninsured~i(year, expand_ever, ref=2013) | State + year,
#              cluster=~State,
#              data=regression_all)
# modelsummary(Q10)
# 
# coef_df_10 <- Q10 %>%
#   tidy() %>%
#   filter(term != "(Intercept)")
# 
# Q10_figure<-ggplot(coef_df_10, aes(x = term, y = estimate, color = term)) +
#   geom_errorbar(aes(ymin = as.numeric(estimate - std.error), ymax = as.numeric(estimate + std.error)), width = 0.2) +
#   geom_line() +
#   geom_point(aes(y = estimate))+
#   scale_x_discrete(labels = c("2012", "2014", "2015", "2016", "2017", "2018", "2019")) +
#   labs(x = "Year", y = "Coefficient Estimate & 95% CI", 
#        title="Event Study for all states", color = "Expansion Status") +
#   theme_minimal()
# Q10_figure


save.image("Hwk5_workspace_5_2.Rdata")

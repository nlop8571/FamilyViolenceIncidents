library(tidyr)
library(dplyr)
library(forcats)
library(car)
library(magrittr)
library(granova)
library(readxl)

#Family_Incidents <-read_excel(any path that connects to "Family_Incidents.xlsx")
Family_Incidents <- read_excel("~/UNI-recent/RMIT2021/Applied Analytics-MATH1324/assignment2/Family_Incidents.xlsx")

#Family_Incidents <-read_excel(any path that connects to "Family_Incidents.xlsx")
Family_Incidents <- read_excel("~/UNI-recent/RMIT2021/Applied Analytics-MATH1324/assignment2/Family_Incidents.xlsx")
# Filter the raw data for summary statistics to see what is happening before doing any analysis ##########

Family_Incidents %>%  filter(Year == "2019" | Year == "2020") %>%
group_by(Year) %>% summarise(Min = min(`Rate_per_100,000_population`, na.rm = TRUE),
                        Q1 = quantile(`Rate_per_100,000_population`,probs = .25,na.rm = TRUE),
                        Median = median(`Rate_per_100,000_population`, na.rm = TRUE),
                        Q3 = quantile(`Rate_per_100,000_population`,probs = .75,na.rm = TRUE),
                        Max = max(`Rate_per_100,000_population`,na.rm = TRUE),
                        Mean = mean(`Rate_per_100,000_population`, na.rm = TRUE),
                        SD = sd(`Rate_per_100,000_population`, na.rm = TRUE),
                        n = n(),
                        Missing = sum(is.na(`Rate_per_100,000_population`)))


# do a box plot to visualise 2019 and 2020, see outliers ##########

Family_Incidents %>% filter(Year == "2019" | Year == "2020") %>%
  boxplot(`Rate_per_100,000_population` ~ Year, data = ., main ="Family Incidents by 100,000 population by Year", ylab="Incidents", col="red") 



##To conduct the paired t-test ####

##### clean the data, create a data frame, filter out the Local government totals and capture years 2019 and 2020 in seperate columns########

##filter the years first###

incident <- Family_Incidents %>% filter(Year == "2019" | Year == "2020")
incident                 
 
## then create the data frame 


incident_clean <- subset(incident, `Local_Government_Area` != "Total")
incident_clean <- incident_clean[c(1,6)]
incident_clean <- incident_clean %>% group_by(Year) %>% mutate(id = row_number()) 

incident_clean <- incident_clean %>% spread(Year, `Rate_per_100,000_population`) %>% select(-id)


incident_df <- incident_clean %>% mutate(d = `2020` - `2019`)
incident_df 


### run summary statistics on the variance between 2019 and 2020 only using incident_df 2020 and incident_df 2019

## from now on, only use the incident df 
            
        incident_df %>%                                               
                       summarise(Min = min(d, na.rm =TRUE),
                        Q1 = quantile(d,probs = .25,na.rm = TRUE),
                        Median = median(d, na.rm = TRUE),
                        Q3 = quantile(d,probs = .75, na.rm = TRUE),
                        IQR = IQR(d, na.rm = TRUE),
                        Max = max(d,na.rm = TRUE),
                        Mean = mean(d, na.rm = TRUE),
                        SD =sd(d, na.rm = TRUE),
                        N = n(),
                        Missing = sum(is.na(d)))
        
        
     
        
## Run the qqPlot to check that the data is approximately normally distributed#####   
## some observations fall outside of the 
## a histogram helps visuals the variance distribution ####   
        
hist(incident_df$d)       

qqPlot(incident_df$d, dist ="norm") 


        
## use this to visualise the variance and outliers in a boxplot ######
##this can be changed to only caputre the years if your wnat##
        
        
boxplot(
        incident_df$d,
        ylab = "Incidents per 100,000 population",
        xlab = 'Variance between 2019 and 2020', col = "red",
        main = " Difference in Incidents per 100,000 population"
)
        axis(1, at = 1:1, labels = c("Variance"))       
        
        
 ## this graph will highlight the changes year on year. It slopes up in 2020       
        
        matplot(t(data.frame(incident_df$`2020`, incident_df$`2019`)),
                type = "b",
                pch = 16,
                col = 1,
                lty = 1,
                xlab = "Year",
                ylab = "Incidents",
                xaxt = "n",
                main = " Incidents per 100,000 pop"
        )
        axis(1, at = 1:2, labels = c("2020-During COVID", "2019-Before COVID 19"))       
        

        
### The paired t-test for normally distributed data     
        
        
t.test(incident_df$`2020`, incident_df$`2019`,
      paired = TRUE,
      alternative = "two.sided", conf.level = .95)  


#Man WHITNEY  test for data not normally distributed . I dont think we need this###

wilcox.test(incident_df$`2020`, incident_df$`2019`, conf.int = TRUE)


## the critical value of t-test, 2 tailed at a = .05 
##-1.990
### still working on this#
qt(p = 0.025, df = 78)   


#two tailed p-value p <1.999 
2*pt(q = 3.84, df = 78)   


## THESE TWO SHOULD BE COMPAREDn in our findings ###













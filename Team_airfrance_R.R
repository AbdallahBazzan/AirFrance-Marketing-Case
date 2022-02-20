# Calling libraries needed for script
library(readxl)
library(ggplot2)
library(plotly) 
library(data.table) 
library(dplyr)
library(readxl)
#install.packages("stringr") # Install stringr package
library(stringr)
#getting the dataset
Air_France_Case_Spreadsheet_Supplement <- read_excel("C:/Users/ABDALLAH BAZZAN/Desktop/R/Air France Case Spreadsheet Supplement.xlsx", 
                                                     sheet = "DoubleClick")
#View(Air_France_Case_Spreadsheet_Supplement)
air_france <- Air_France_Case_Spreadsheet_Supplement
# Viewing the dataset
#View(air_france)

#Looking at the first 5 rows of the dataset
head(air_france)

#Checking all the variable names
colnames(air_france)

#Exploring the summary statistics of each variable
summary(air_france)

##########################################----------------------------------------------------
########### Data Massaging ###############----------------------------------------------------
##########################################----------------------------------------------------

airfrance <- air_france

#Removing the columns that we don't need
colnames(airfrance)
remove_col <- c('Publisher ID', 'Keyword ID', 'Match Type',
                'Keyword Group', 'Category', 'Keyword Type', 'Status')

airfrance <- select(airfrance, -remove_col)
colnames(airfrance)

#Checking if any in large numbers or empty string is present in the data 
sum(airfrance== "")
sum(is.na(airfrance))

#using the sapply function to check for missing values source
sapply(airfrance, function(x) sum(is.na(x)))

colnames(airfrance)

#creating revenue variable
airfrance$net_income <- airfrance$Amount - airfrance$`Total Cost`

airfrance$net_income <- as.numeric(airfrance$net_income)
#View(airfrance)

airfrance$ROA <- as.numeric(airfrance$net_income / airfrance$`Total Cost`)
#Creating revenue/per booking variable 
airfrance$'Revenue per booking' <- round(airfrance$net_income/
                                      airfrance$`Total Volume of Bookings`,2)
airfrance$'Revenue per booking' <- as.numeric(airfrance$'Revenue per booking')
#Creating booking cost  
airfrance$'Cost per booking' <-as.numeric(airfrance$`Total Cost`/
                                   airfrance$`Total Volume of Bookings`)
#Creating booking probability
airfrance$'Probability of Booking' <- round((( airfrance$`Trans. Conv. %` * airfrance$`Engine Click Thru %`)/100),2)

#turning us-publishers into 1, globals = 0 
airfrance$'US Publisher' <- c()
airfrance$'US Publisher' <- str_detect(airfrance$`Publisher Name`, "US")
airfrance$'US Publisher' <- as.numeric(airfrance$'US Publisher')
#substituting errors in spelling
unique(airfrance$`Bid Strategy`)
airfrance$`Bid Strategy` <- gsub("Position 1 -2 Target", "Position 1-2 Target", 
                                 airfrance$`Bid Strategy`)
airfrance$`Bid Strategy` <- gsub("Postiion 1-4 Bid Strategy", "Position 1-4 Bid Strategy", 
                                 airfrance$`Bid Strategy`)


###################Descriptive statistics#################################

#Creating a new data frame 
air_df <- airfrance

summary(airfrance)

#desc_stats function
desc_stats <- function(x){
  my_min <- try(min(x, na.rm=TRUE))
  my_mean <- try(mean(x, na.rm=TRUE))
  my_sd <- try(sd(x, na.rm=TRUE))
  my_max <- try(max(x, na.rm=TRUE))
  return(c(my_min, my_mean, my_sd, my_max))
}#closing the function  

#Finding descriptive stat for the variables
airfrance_stat <- c("Min", "Mean", "SD", "Max")
impressions <- desc_stats(air_df$Impressions)
number_clicks <- desc_stats(air_df$Clicks)
total_amount <- desc_stats(air_df$Amount)
total_cost <- desc_stats(air_df$`Total Cost`)
net_income_ <- desc_stats(air_df$net_income)

airfrance_desc <- as.data.frame(cbind(airfrance_stat, impressions,
                                      number_clicks, total_amount,
                                      total_cost, net_income_))
airfrance_desc

#publishers for SEM for each company
publishers <- unique(air_df$`Publisher Name`)
publishers
#sales by publisher SEM
#starting with empty vector

total_rev <- c()
total_clicks <- c()
cost_total <- c()
total_book <- c()
total_net_income <- c()

#creating a for loop to compare publishers
for (i in 1:length(publishers)) {
  total_rev <- c(total_rev,
          sum(airfrance$Amount[which(airfrance[,1] == publishers[i])]))
  total_clicks <- c(total_clicks,
                  sum(airfrance$Clicks[which(airfrance[,1]==publishers[i])]))
  cost_total<- c(cost_total,
              sum(airfrance$`Total Cost`[which(airfrance[,1]==publishers[i])]))
  total_book <- c(total_book,
    sum(airfrance$`Total Volume of Bookings`[which(airfrance[,1]==publishers[i])]))
  total_net_income <- c(total_net_income,
            sum(airfrance$net_income[which(airfrance[,1]==publishers[i])]))
  
}#ending loop
airfrance$net_income
total_rev
total_clicks
cost_total
total_book
total_net_income
#combining Kayak with other publishers for a total amount 
all_publishers <- cbind(c(publishers,"Kayak-US"))
publisher_sales <- as.numeric(cbind(c(total_rev, "233694")))
publisher_clicks<- as.numeric(cbind(c(total_clicks, "2839")))
publisher_costs <- as.numeric(cbind(c(cost_total, "3567.13")))
publisher_books <- as.numeric(cbind(c(total_book, "208")))
publisher_net_income <- as.numeric(cbind(c(total_net_income, "230126.87")))

#creating a matrix for overall publishers
my_matrix<- matrix( c(all_publishers, publisher_sales, publisher_clicks,
                      publisher_costs, publisher_books,
                      publisher_net_income), ncol = 6, nrow = 8)
overall_publishers<- as.data.frame(my_matrix)
#changing the column names
colnames(x= overall_publishers) <- c("Publishers", "Total Sales",
                                      "Total Clicks", "Total Costs",
                                      "Total Bookings", "Net Income")

#Changing the type of the variables in the matrix 
overall_publishers$`Total Sales` <- round(as.numeric
                                        (overall_publishers$`Total Sales` ),2)
overall_publishers$`Total Clicks` <- round(as.numeric
                                      (overall_publishers$`Total Clicks`))
overall_publishers$`Total Costs` <- round(as.numeric
                                      (overall_publishers$`Total Costs`),2)
overall_publishers$`Total Bookings` <- round(as.numeric
                                      (overall_publishers$`Total Bookings`))
overall_publishers$`Net Income` <- round(as.numeric
                                        (overall_publishers$`Net Income`),2)

# Adding new columns with usefull information
overall_publishers$ROA <- round(as.numeric
  (overall_publishers$`Net Income` / overall_publishers$`Total Costs`),2)

overall_publishers$'Revenue per booking' <- round(as.numeric
(overall_publishers$`Net Income`/overall_publishers$`Total Bookings`))

overall_publishers$'Cost per booking' <- round(as.numeric
(overall_publishers$`Total Costs`/overall_publishers$`Total Bookings`))

overall_publishers$'ROA per booking' <- round(as.numeric
(overall_publishers$'Revenue per booking'/
    overall_publishers$'Cost per booking'), 2)

overall_publishers$'Avg. Cost per Click'<- round(as.numeric
                                (overall_publishers$'Total Costs'
                                        /overall_publishers$'Total Clicks'),2)

#printing overall publishers and their totals
overall_publishers

colnames(overall_publishers)
summary(overall_publishers)

#empty vector to create 1, 0 in the United States or not
overall_publishers$'US based' <- c()
overall_publishers$'US based' <- str_detect(overall_publishers$`Publishers`, "US")
overall_publishers$'US based' <- as.character(as.numeric(overall_publishers$'US based'))



#Ordering the values based on the ROA
overall_publishers[order(overall_publishers$ROA),]

#need to focus on something to better their market share in the states
air_regression <- airfrance

#initial logistic regression
my_logitair<- glm(airfrance$`US Publisher` ~ airfrance$net_income + 
                                airfrance$'Amount' +
                                airfrance$'Total Volume of Bookings'+
                                airfrance$'Clicks'+airfrance$'Impressions'+
                                airfrance$'Avg. Pos.', 
                          data=air_regression, family = "binomial") 
summary(my_logitair)

#Cleaned logistic regression 
cleaned_logitair<- glm(airfrance$`US Publisher` ~  airfrance$'Impressions'+airfrance$'Avg. Pos.' , 
                       data=air_regression, family = "binomial") 
summary(cleaned_logitair)

my_linear <- lm(net_income ~ airfrance$'Amount' +
                  airfrance$'Total Volume of Bookings'+
                  airfrance$'Clicks'+airfrance$'Impressions'+
                  airfrance$'Avg. Pos.' +airfrance$`Avg. Cost per Click`, 
                data= air_regression)
summary(my_linear)

## Working with Bid strategy
air_df <- airfrance
air_md <- air_df


#changing to NAs 
#WE NEED TO DO MORE WITH BIDSTRATEGY\
air_md$air_ROA <- as.numeric(air_md$net_income / air_md$`Total Cost`)
air_md[air_md=="Inf"] <- NA

air_md <- air_md[which(air_md$air_ROA > 0),]

air_md <- air_md[which(air_md$air_ROA != 'NA'),]

bid_strat <- unique(air_md$`Bid Strategy`)

bid_strat1 <- bid_strat
bid_strat1
#creating empty vectors for publishers
ROA_bid_strat <-c()
Yahoo_US_BS <- c()
MSN_Global_BS <- c()
MSN_US_BS <- c()
Google_Global_BS <- c()
Google_US_BS <- c()
Overture_Global_BS<- c()
Overture_US_BS <- c()




#creating empty vectors for average positioning
Yahoo_US_AP <- c()
MSN_Global_AP <- c()
MSN_US_AP <- c()
Google_Global_AP <- c()
Google_US_AP <- c()
Overture_Global_AP<- c()
Overture_US_AP <- c()
#looking at average positioning compared to bid strategy
for (i in 1:length(bid_strat)){
  Yahoo_US_AP <- c(Yahoo_US_AP, mean(air_md$`Avg. Pos.`[which(air_md$'Bid Strategy' == bid_strat[i] & 
                                                                air_md$'Publisher Name' == publishers[1])]))
  MSN_Global_AP <- c(MSN_Global_AP, mean(air_md$`Avg. Pos.`[which(air_md$'Bid Strategy' == bid_strat[i] & 
                                                                    air_md$'Publisher Name' == publishers[2])]))
  Google_Global_AP <- c(Google_Global_AP, mean(air_md$`Avg. Pos.`[which(air_md$'Bid Strategy' == bid_strat[i] & 
                                                                          air_md$'Publisher Name' == publishers[3])]))
  Overture_Global_AP <- c(Overture_Global_AP, mean(air_md$`Avg. Pos.`[which(air_md$'Bid Strategy' == bid_strat[i] & 
                                                                              air_md$'Publisher Name' == publishers[4])]))
  Google_US_AP <- c(Google_US_AP, mean(air_md$`Avg. Pos.`[which(air_md$'Bid Strategy' == bid_strat[i] & 
                                                                  air_md$'Publisher Name' == publishers[5])]))
  Overture_US_AP <- c(Overture_US_AP, mean(air_md$`Avg. Pos.`[which(air_md$'Bid Strategy' == bid_strat[i] & 
                                                                      air_md$'Publisher Name' == publishers[6])]))
  MSN_US_AP <- c(MSN_US_AP, mean(air_md$`Avg. Pos.`[which(air_md$'Bid Strategy' == bid_strat[i] & 
                                                            air_md$'Publisher Name' == publishers[7])]))
}

ROA_bid_strat <- cbind(bid_strat, Yahoo_US_BS,Yahoo_US_AP, MSN_Global_BS,MSN_Global_AP, 
                       Google_Global_BS, Google_Global_AP,
                       Overture_Global_BS, Overture_Global_AP, Google_US_BS, Google_US_AP,
                       Overture_US_AP, Overture_US_BS, MSN_US_AP, MSN_US_BS)
#View(ROA_bid_strat)
ROA_bid_strat <- cbind(bid_strat, Yahoo_US_BS,Yahoo_US_AP, MSN_Global_BS,MSN_Global_AP, 
                       Google_Global_BS, Google_Global_AP,
                       Overture_Global_BS, Overture_Global_AP, Google_US_BS, Google_US_AP,
                       Overture_US_AP, Overture_US_BS, MSN_US_AP, MSN_US_BS)
#View(ROA_bid_strat)

#creating a new data frame
air_md2 <- air_df

#Slice the data
air_md2$air_ROA <- as.numeric(air_md2$net_income / air_md2$`Total Cost`)
air_md2[air_md2=="Inf"] <- NA
air_md2 <- air_md2[which(air_md2$air_ROA != 'NA'),]
air_md2 <- air_md2[which(air_md2$`Bid Strategy`!= 'NA'),]
bid_strat <- unique(air_md2$`Bid Strategy`)
#creating a stacked bar chart to look at bid strategy vs revenue
#all of the global publishers have specific bid strategys, 
#google is around the board but all of their Us publishers dont really
#us comp need specific positions in order to target the market better
pBid <- ggplot() + geom_bar(aes(y = `net_income` , x = `Bid Strategy`, fill = `Publisher Name`), data = air_md2,
                          stat="identity")
pBid



pCampaignBook <- ggplot() + geom_bar(aes(y =`Total Volume of Bookings` , x = `Publisher Name`, fill = Campaign), data = air_md2,
                            stat="identity")


pCampaignBook

#pCampaignIncome <- ggplot() + geom_bar(aes(y = Clicks , x = `Publisher Name`, fill = Campaign), data = air_md2,
                                # stat="identity")

#pCampaignIncome

##### ROA chart
publ_name <- c(overall_publishers$publishers)
#where are the Ys coming from
ROA_publ <- c(overall_publishers$`ROA per booking`)
data <- data.frame(publ_name,ROA_publ)

x <- c(overall_publishers$Publishers)
y <- c(overall_publishers$ROA)
data <- data.frame(x, y)

pROA <- plot_ly(data, x= reorder(x,+y), y= ~y, type = "bar", name = "Return on Advertising", color = I("blue"), alpha = 0.5) %>%
  layout(title = "Return On Advertising",
         xaxis = list(title = ""),
         yaxis = list(title = ""))
pROA
#COMPARE THE TOP TWO Net_income
w <- c(overall_publishers$Publishers)
z <- c(overall_publishers$`Net Income`)
data2 <- data.frame(w, z)

#The alphabetic default order so we need to code the following:
data2$w <- factor(data2$w, levels = data2[["w"]])

pNet_incomee <- plot_ly(data2, x= reorder(w,+z), y= ~z, type = "bar", mode = "lines+markers+text", name = "Return on Advertising", color = I("blue"), alpha = 0.5, width = 0.5) %>%
  layout(title = "Net Income",
         xaxis = list(title = ""),
         yaxis = list(title = ""))
pNet_incomee

######## TOTAL VOLUME OF BOOOKS
w <- overall_publishers$Publishers
z <- overall_publishers$`Total Bookings`
data2 <- data.frame(w, z)

#The alphabetic default order so we need to code the following:
data2$w <- factor(data2$w, levels = data2[["w"]])

pTotal_bookings <- plot_ly(data2, x= reorder(w,+z), y= ~z, type = "bar", mode = "lines+markers+text", name = "Return on Advertising", color = I("blue"), alpha = 0.5, width = 0.5) %>%
  layout(title = "Total Bookings per Publisher",
         xaxis = list(title = ""),
         yaxis = list(title = ""))
pTotal_bookings
######## Net Income per booking
w <- overall_publishers$Publishers
z <- overall_publishers$`Revenue per booking`
data2 <- data.frame(w, z)

#The alphabetic default order so we need to code the following:
data2$w <- factor(data2$w, levels = data2[["w"]])

pNet_income <- plot_ly(data2, x= reorder(w,+z), y= ~z, type = "bar", mode = "lines+markers+text", name = "Return on Advertising", color = I("blue"), alpha = 0.5, width = 0.5) %>%
  layout(title = "Net Income per Booking",
         xaxis = list(title = ""),
         yaxis = list(title = ""))
pNet_income

######## Total COst per booking
w <- overall_publishers$Publishers
z <- overall_publishers$`Cost per booking`
data2 <- data.frame(w, z)

#The alphabetic default order so we need to code the following:
data2$w <- factor(data2$w, levels = data2[["w"]])

pCostperbook<- plot_ly(data2, x= reorder(w,+z), y= ~z, type = "bar", mode = "lines+markers+text", name = "Return on Advertising", color = I("blue"), alpha = 0.5, width = 0.5) %>%
  layout(title = "Costs Per Booking",
         xaxis = list(title = ""),
         yaxis = list(title = ""))
pCostperbook
######## ROA per booking
w <- overall_publishers$Publishers
z <- overall_publishers$`ROA per booking`
data2 <- data.frame(w, z)

#The alphabetic default order so we need to code the following:
data2$w <- factor(data2$w, levels = data2[["w"]])

pNet_income <- plot_ly(data2, x= reorder(w,+z), y= ~z, type = "bar", mode = "lines+markers+text", name = "Return on Advertising", color = I("blue"), alpha = 0.5, width = 0.5) %>%
  layout(title = "ROA per Booking",
         xaxis = list(title = ""),
         yaxis = list(title = ""))
pNet_income


#Focus more on Kayak because ROA is higher

#Pivot Table

air_piv2 <- overall_publishers %>% group_by(Publishers) %>% summarize(
  overall_records = n(),
  sum_tc = sum(`Total Costs`),
  avg_ROA = mean(ROA),
  avg_cpc = mean(`Avg. Cost per Click`),
  avg_prob = mean(`Total Bookings`)
)
summary(air_piv2 )
#creating a bubble Chart
#Bubble CHart

pBubble <- plot_ly(air_piv2, x = ~sum_tc, y = ~avg_prob,
             textposition = "auto",
             type = 'scatter', 
             mode = 'markers', 
             size = ~avg_cpc, 
             color = ~`Publishers`, 
             colors = 'Paired',
             marker = list(opacity = 0.8, sizemode = 'diameter')) %>%
  layout(title = 'Bookings Based on Clicks',
         xaxis = list(title = "Sum of Total Clicks", showgrid = TRUE),
         yaxis = list(title = "Average Amount of Bookings ", showgrid = TRUE),
         showlegend = TRUE)

pBubble

#logistic regression on total volume of book
table(airfrance$Keyword)
count_word <- airfrance %>%
  count(Keyword)
count_word

#Cheap International flight
#cheap international ticket
#cheapest flight to paris
#discount international airfare
#europe airline
#airfrance.com
#airfrance usa

####### KPIs
# Conversion rate = (Number of bookings/ total clicks)
#ROA
#######
sum(overall_publishers$`Total Costs`)
#Uniform Strategy

#combining Kayak with other publishers for a total amount 
all_publishers1 <- publishers
publisher_sales1 <- total_rev
publisher_clicks1<- total_clicks
publisher_costs1 <- cost_total
publisher_books1 <- total_book
publisher_net_income1 <- total_net_income

#creating a matrix for overall publishers
my_matrix1<- matrix( c(all_publishers1, publisher_sales1, publisher_clicks1,
                      publisher_costs1, publisher_books1,
                      publisher_net_income1), ncol = 6, nrow = 7)
overall_publishers1<- as.data.frame(my_matrix1)
#changing the column names
colnames(x= overall_publishers1) <- c("Publishers", "Total Sales",
                                     "Total Clicks", "Total Costs",
                                     "Total Bookings", "Net Income")

#Changing the type of the variables in the matrix 
overall_publishers1$`Total Sales` <- round(as.numeric
                                          (overall_publishers1$`Total Sales` ),2)
overall_publishers1$`Total Clicks` <- round(as.numeric
                                           (overall_publishers1$`Total Clicks`))
overall_publishers1$`Total Costs` <- round(as.numeric
                                          (overall_publishers1$`Total Costs`),2)
overall_publishers1$`Total Bookings` <- round(as.numeric
                                             (overall_publishers1$`Total Bookings`))
overall_publishers1$`Net Income` <- round(as.numeric
                                         (overall_publishers1$`Net Income`),2)

# Adding new columns with usefull information
overall_publishers1$ROA <- round(as.numeric
                                (overall_publishers1$`Net Income` / overall_publishers1$`Total Costs`),2)

overall_publishers1$'Revenue per booking' <- round(as.numeric
                                                  (overall_publishers1$`Net Income`/overall_publishers1$`Total Bookings`))

overall_publishers1$'Cost per booking' <- round(as.numeric
                                               (overall_publishers1$`Total Costs`/overall_publishers1$`Total Bookings`))

overall_publishers1$'ROA per booking' <- round(as.numeric
                                              (overall_publishers1$'Revenue per booking'/
                                                  overall_publishers1$'Cost per booking'), 2)

overall_publishers1$'Avg. Cost per Click'<- round(as.numeric
                                                 (overall_publishers1$'Total Costs'
                                                   /overall_publishers1$'Total Clicks'),2)

################
overall_publishers1$UniformCost <- as.numeric(
  sum(overall_publishers1$`Total Costs`)/7)

overall_publishers1$uniform_net_income <- 
  (overall_publishers1$UniformCost * overall_publishers1$ROA)

#### Highest ROA strategy
#overall_publishers1$Highest_ROA_Income <-
  #(sum(overall_publishers1$`Total Costs`)*(18.1))

#Scenario 2
actual_scenario <-
  sum(overall_publishers$`Net Income`)
scenario_2 <- 
  sum(overall_publishers1$UniformCost * overall_publishers1$ROA)
scenario_3 <-
  (sum(overall_publishers1$`Total Costs`)*(18.1))

scenarios <- c("Actual Scenario","Uniform Strategy Scenario","Highest ROA Scenario")

my_income_scenarios<- matrix(c(scenarios,actual_scenario,scenario_2,scenario_3),
                              ncol = 2, nrow = 3)
income_scenarios<- as.data.frame(my_income_scenarios)
colnames(x= income_scenarios) <- c("Income Scenarios", "Net Income")

my_income_scenarios

### Income Scenarios Chart
w <- income_scenarios$`Income Scenarios`
z <- income_scenarios$`Net Income`
data2 <- data.frame(w, z)

#The alphabetic default order so we need to code the following:
data2$w <- factor(data2$w, levels = data2[["w"]])

pIncome_scenarios <- plot_ly(data2, x=w, y= ~z, type = "bar",
                             mode = "lines+markers+text",
                             name = "Income Scenarios",
                             color = I("blue"), alpha = 1, width = 0.5) %>%
  layout(title = "Different Incomes per Scenarios",
         xaxis = list(title = ""),
         yaxis = list(title = ""))
pIncome_scenarios



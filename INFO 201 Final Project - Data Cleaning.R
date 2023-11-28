library(dplyr)

#join dataframes
df_1 <- read.csv("Impaired_Driving_Death_Rate__by_Age_and_Gender__2012___2014__All_States.csv")
df_2 <- read.csv("DUI.csv")

df <- merge(x = df_1, y = df_2, by = "State", all = TRUE)

#create at least one new categorical variable
region <- ""
for (i in 1:nrow(df)){
  if (df$State[i] == "Connecticut" | 
      df$State[i] == "Maine" | 
      df$State[i] == "Massachusetts" | 
      df$State[i] == "New Hampshire" | 
      df$State[i] == "Rhode Island" | 
      df$State[i] == "Vermont" |
      df$State[i] == "New Jersey" | 
      df$State[i] == "New York" | 
      df$State[i] == "Pennsylvania"
      ){
    region[i] <- "Northeast"
  } else if (df$State[i] == "Illinois" |
             df$State[i] == "Indiana" |
             df$State[i] == "Michigan" |
             df$State[i] == "Ohio" |
             df$State[i] == "Wisconsin" |
             df$State[i] == "Iowa" |
             df$State[i] == "Kansas" |
             df$State[i] == "Minnesota" |
             df$State[i] == "Missouri" |
             df$State[i] == "Nebraska" |
             df$State[i] == "North Dakota" |
             df$State[i] == "South Dakota"
             ){
    region[i] <- "Midwest"
  } else if (df$State[i] == "Delaware" |
             df$State[i] == "Florida" |
             df$State[i] == "Georgia" |
             df$State[i] == "Maryland" |
             df$State[i] == "North Carolina" |
             df$State[i] == "South Carolina" |
             df$State[i] == "Virginia" |
             df$State[i] == "District of Columbia" |
             df$State[i] == "West Virginia" |
             df$State[i] == "Alabama" |
             df$State[i] == "Kentucky" |
             df$State[i] == "Mississippi" |
             df$State[i] == "Tennessee" |
             df$State[i] == "Arkansas" |
             df$State[i] == "Louisiana" |
             df$State[i] == "Oklahoma" |
             df$State[i] == "Texas"
             ){
    region[i] <- "South"
  } else{
    region[i] <- "West"
  }
}
df$Region <- region

grocery_alcohol <- ""
for (i in 1:nrow(df)){
  if (df$State[i] == "Alabama" | 
      df$State[i] == "Arkansas" |
      df$State[i] == "Colorado" |
      df$State[i] == "Florida" |
      df$State[i] == "Georgia" |
      df$State[i] == "Idaho" |
      df$State[i] == "Montana" |
      df$State[i] == "New Hampshire" |
      df$State[i] == "North Carolina" |
      df$State[i] == "Ohio" |
      df$State[i] == "Oklahoma" |
      df$State[i] == "Oregon" |
      df$State[i] == "Pennsylvania" |
      df$State[i] == "South Carolina" |
      df$State[i] == "Tennessee" |
      df$State[i] == "Texas" |
      df$State[i] == "Vermont" |
      df$State[i] == "Virginia"
  ){
    grocery_alcohol[i] <- "Beer and Wine Only"
  } else if(df$State[i] == "Connecticut" |
            df$State[i] == "Kansas" |
            df$State[i] == "Kentucky" |
            df$State[i] == "Minnesota" |
            df$State[i] == "Mississippi" |
            df$State[i] == "New York" |
            df$State[i] == "Utah"
  ){
    grocery_alcohol[i] <- "Beer Only"
  } else if(df$State[i] == "Alaska" |
            df$State[i] == "Delaware" |
            df$State[i] == "Maryland" |
            df$State[i] == "Rhode Island"
  ){
    grocery_alcohol[i] <- "None"
  } else if(df$State[i] == "New Jersey"
  ){
    grocery_alcohol[i] <- "Varies"
  } else{
    grocery_alcohol[i] <- "All"
  }
}
df$Types.Of.Alcohol.Purchasable.In.Grocery.Stores <- grocery_alcohol

marijuana_legal <- ""
for (i in 1:nrow(df)){
  if (df$State[i] == "California" |	
      df$State[i] == "Alaska" |	
      df$State[i] == "Nevada" |	
      df$State[i] == "Oregon" |	
      df$State[i] == "Washington" |	
      df$State[i] == "Maine" |	
      df$State[i] == "Colorado" |	
      df$State[i] == "Montana" |	
      df$State[i] == "Vermont" |	
      df$State[i] == "Rhode Island" |	
      df$State[i] == "New Mexico" |	
      df$State[i] == "Michigan" |	
      df$State[i] == "Arizona" |	
      df$State[i] == "New Jersey" |	
      df$State[i] == "Delaware" |	
      df$State[i] == "Connecticut" |	
      df$State[i] == "Massachusetts" |	
      df$State[i] == "Illinois" |	
      df$State[i] == "Maryland" |	
      df$State[i] == "Minnesota" |	
      df$State[i] == "New York" |	
      df$State[i] == "Ohio" |	
      df$State[i] == "Missouri" |	
      df$State[i] == "Virginia" |	
      df$State[i] == "District of Columbia"
  ){
    marijuana_legal[i] = "Yes"
  } else{
    marijuana_legal[i] = "No"
  }
}
df$Marijuana.Legalized <- marijuana_legal

#create at least one new continuous/numerical variable
num_fatal_2012 <- 0
for(i in 1:nrow(df)){
  num_fatal_2012[i] <- round((df$All.Ages..2012[i] / 100000) * df$Population[i])
}
df$Actual.Number.Fatalities.2012 <- num_fatal_2012

num_fatal_2014 <- 0
for(i in 1:nrow(df)){
  num_fatal_2014[i] <- round((df$All.Ages..2014[i] / 100000) * df$Population[i])
}
df$Actual.Number.Fatalities.2014 <- num_fatal_2014

#create at least one summarization data frame
g_df <- filter(group_by(df, Region, Types.Of.Alcohol.Purchasable.In.Grocery.Stores), !is.na(Fatalities))
region_df <- summarize(g_df, avg_fatal = mean(Fatalities))
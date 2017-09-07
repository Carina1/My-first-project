pi
pi ^ 2
1 / 0
0.2 < 0.2
#when compare decimals comapres many decimal places so use commnad below to avoid strancge results).
all.equal(0.8 - 0.6, 0.7 - 0.5)
(0.8 - 0.6) == (0.7 - 0.5)
5 != 4
#floating point errors
(0.5 - 0.3) == (0.3 - 0.1)
# do a not equal to with a tolerance!all.equal(0.5 - 0.3, 0.3 - 0.1)
pi > 3
1:5 <= 3 #pairwise processing (comparison all 5 values)
sum(1:5 <= 3) #number of responses that are less than or equal to 3)
| #shift backslash called pipe is function for or
  colours <- c("red", "blue", "orange") # <- means make equal
tolower("ORANGE") %in% tolower(colours)
a %in% "LETTERS"
LETTERS #contains all the letters of alphabet
"a" %in% LETTERS
"a" %in% letters

#Find out which values in the sequence 1 to 10 are greater than or equal to 3 and less than 7

1:10 >= 3
1:10 < 7
1:10 >= 3 & 1:10 < 7

`1a` <- "a"
`1a` <- NULL #produce empty cell

TRUE = FALSE

myvar <-
  1 #store calculations in variables, see values in environment window
myvar <- 1:10

ls()
rm (" ")
rm (list = ls()) #clears environment
c(1:5, 9:20) #combines vectors/sequences of numbers)

1:50
c("red", "green")
c(1:50,
  c("red", "green"))

data.frame(1:26, letters)
largeABC <-
  data.frame(id = rep(1:26, 5), abc = letters) #name table and rep splits into 5
View(largeABC)

myframe <-
  data.frame(id = 1:50, frame = "red", "green") #left hand vector needs to be function of right as recycles and match
View(myframe)

1:20 * 5
1:20 * 1:3
1:20 * 1:3
data.frame (1:20, 1:3)

list (1:50, c("red", "hotels"))
origlist = list(words = c("red", "green"))
firstlist = list(id = 1:50, words2 = c("red", "green")) #name lists to differentiate

list (1:26, c = "letters")
alphabetlist = list(id = 1:26, words = c("letters"))

list (1)

class(c(1, TRUE))

is.integer(c(1, TRUE))
origVec <- as.integer(c(1, TRUE))
is.integer(as.integer(c(1, TRUE)))
length(origVec)

paste(1:30, 1:3)

concatData <- c(paste(1:30, sep = "-", collapse = ";"))
strsplit(concatData, split = ";")

write.csv()

rnorm(100, mean = 10, sd = 1)
runif(50, 1, 50)

install.packages("tidyverse")
library("tidyverse")

tidyverse::tidyverse_conflicts() #helps reduce ambiguity of code as default is last loaded - useful if only use rarely not if use alot

library(dplyr)

a <- 10:21
a <= 15
a[a <= 15]
a[a <= 15 | a > 19]

df <- data.frame(a = letters, b = 1:26)
df[]
df[, ]
df[1, ]
df[, 1]
df[1:5, 1:2]

df[-1, ] #minus first column
df[, -"a"] #doesn't work

df[, 2] <= "5" #column 2 less than or equal to 5 (check)
df[df$b <= 5,] #same as above
#check column names
colnames(df) <= "a"

df[, colnames(df) <= "a"]

letters < "x"

letters [letters < "x"]

View(iris)

iris[1, ]
iris[1:5, ]
iris[, 1:2]
iris [, "Sepal.Length"]
iris[, c("Sepal.Length", "Sepal.Width")] #for trimming down a dataset

letters > "g"
letters[letters > "g"]
iris["Sepal.Length" > 5.8]

iris[iris$Sepal.Length >= 5.8,] #row filter
avgSepalWidth <-
  mean(iris$Sepal.Width) #add brackets for it to show the mean value or look in Environment window that returns values
iris[iris$Sepal.Width < avgSepalWidth,] #can step into each of these functions

a[is.na(a)]
a[is.na(a)] <- mean(a, na.rm = TRUE)

iris [, 1:4]
iris [,-5] #remove column number
iris[, colnames(iris) != "Species"]

myIris <- iris [1:100, ]
myIris$Species <- "Unknown"
View (myIris)
myIris <- myIris [myIris$Sepal.Length <= 5, 5, ]

write.csv(iris, "iris.csv", row.names = FALSE) #saved in files tabs

library(haven)
colnames(iris) <- gsub(".", "", colnames(iris), fixed = TRUE)
haven::write_dta(iris, "iris.dta", version = 13)
read_csv("iris.csv",
         col_names = letters [1:5],
         skip = 1)

altIris <- read_csv("iris.csv",
                    col_names = letters [1:5],
                    skip = 1)
write_csv (altIris, "altIris.csv")

library(readxl)
cancerdata <- read_excel("adultcancersurvivalcorrectionfinal.xls")
cn <- "adultcancersurvivalcorrectionfinal.xls"
readxl::excel_sheets(cn)
cancerdata <- read_excel(cn,
                         sheet = "Table 5",
                         skip = 2,
                         na = ":")
cancerXL <- read_excel(cn,
                       sheet = "Table 5",
                       skip = 2,
                       na = ":")
library(writexl)
writexl::write_xlsx(cancerXL, "newACS.xlsx")

library(openxlsx)
openxlsx::write.xlsx()

openxlsx::write.xlsx(iris, "newIRISauto.xlsx",
                     colwidths = "auto")

#DATABASE
#auto log in
library(DBI)
library(odbc)
dbConn <- dbConnect(
  odbc(),
  driver = "SQL Server",
  server = "rea-inf-dsql08",
  database = "cancerstatsr",
  trusted_connection = TRUE
)

incidence <- dbGetQuery(
  dbConn,
  "select *       #select everything denoted by star
  from [national].incidence i         #metadata view of tables rather than detailed list view
  where baseyearkey=2016
  and incidenceyearkey>=2000"
)

betterTblsList <-
  dbGetQuery(dbConn, "SELECT * FROM information_schema.tables")
betterTblsList

cancercodes <- dbGetQuery(dbConn, "SELECT * FROM dim.cancersite")

baseyear <- 2016
sqltorun <-
  sqlInterpolate(
    dbConn,
    "select TOP 100* -- retrieve only 100 rows for example, this is sql comment (double hyphen with space within brackets of comment)
    from [national].incidence          -- need to add national in brackets otherwise return error
    where baseyearkey = ?baseyear",
    baseyear = baseyear
  )
newincidences <- dbGetQuery(dbConn, sqltorun)

#piping - drops left hand information needed ie data usually for fileter command

library(tidyverse)
incidence <- read_csv("data/incidences.csv")
incidence %>%
  filter(IncidenceYearKey == 2000)
View ()

#same as above
filter(incidence, IncidenceYearKey == 2000)

library (datasauRus)
library (ggplot2)
datasauRus::datasauRus_dozen %>%
  ggplot(aes(x = x, y = y)) +
  geom_point() +
  facet_wrap( ~ dataset)

incidence %>% #pipe into
  filter(IncidenceYearKey == 2000,
         GenderKey == "M") %>%
  select_if(is.numeric) %>%
  pairs()

#pipes enables step by step

iris %>%
  colnames () %>%
  toupper ()

#put letters in x position by using . and create new vector (not changing one that exists) by using sub
LETTERS %>%
  sub("S", "Z", .) ->
  LETTERZ

#FILTERING

incidence %>% #pipe into
  filter(IncidenceYearKey == 2000 | #Make an OR command
           GenderKey == "M" , # Use , for AND
         AgeRangeKey == "0509")

unique(incidence$CountryKey)

iris %>% #pipe into
  filter(SepalWidth < mean(SepalWidth) | #Make an OR command
           SepalLength < mean(SepalLength))

iris %>% #pipe into
  filter(Species != "setosa")

iris %>%
  filter(!(Species == "setosa"))

iris %>%
  select(-SepalLength)

starts_with #for prefixes
ends_with #for suffixes

iris %>%
  select (PetalWidth:Species) #between columns

iris %>%
  select (., sort(colnames(.))) #sort columns but full stop shows want whole dataset

#select function works only on columns
#filter function works only on rows
#ordering rows can use arrange but only order data when absolutely have to

iris %>%
  select(starts_with("S")) %>%
  colnames()

iris %>%
  select(starts_with("P"), everything())

#Select allows us to rename columns/cleaning

# also can use matches and also specify what interested in first and then everything else

#MUTATE

iris %>%
  mutate(SepalArea = SepalWidth * SepalLength) ->
  iris

#need to specify this is in iris

iris %>%
  rename (P = PetalWidth)

iris %>%
  mutate(SepalArea = SepalWidth * SepalLength,
         AvgSepalArea = mean(SepalArea))

iris %>%
  mutate(
    SepalWidth1 = SepalWidth / 2.5,
    SepalLength1 = SepalLength / 2.5,
    PetalLength1 = PetalLength / 2.5,
    PetalWidth1 = PetalWidth / 2.5,
    Species = toupper(Species)
  ) ->
  irisImperial

#use mutate_if so no need to write each (stretch content)

iris %>%
  mutate_if(is.numeric, ~ . / 2.5) %>%
  mutate_if(is.character, toupper)

#AGGREGATION

iris %>%
  summarise(TotalArea = sum(SepalLength * SepalWidth))

iris %>%
  # as_tibble for nice format for printing
  summarise(TotalArea = sum(SepalLength * SepalWidth))

iris %>%
  summarise(min(SepalLength,
                max(SepalLength)))

iris %>% #check brackets
  summarise(minSW = min(SepalWidth),
            maxSW = max(SepalWidth))

#group_by group first and then summarise and produces in long form. Use pivot function

iris %>%
  group_by (substr(Species, 1, 1)) %>% #for more than one level use substr
  summarise(TotalSA = sum(SepalLength * SepalWidth))

#group by mutates only those in group and summarise collapses the group


iris %>% ##can calculate which ones are outside confidence intervals
  group_by(Species) %>%
  mutate(AvgPW = mean(PetalWidth))

#stretch
iris %>%
  mutate (AvgPWDiff = PetalWidth < AvgPW) %>%
  group_by(Species, AvgPWDiff) %>%
  summarise_all(max)

#Solution to stretch Q
iris %>%
  mutate(bAvg = SepalLength < mean(SepalLength)) %>%
  group_by(Species, bAvg) %>%
  summarise_all(max)

iris %>%
  mutate(bAvg = SepalLength < mean(SepalLength)) %>%
  group_by_if( ~ !is.numeric(.)) %>%
  summarise_all(max)

incidence %>%
  filter(IncidenceYearKey == 2000) %>%
  group_by(GenderKey, CancerSiteKey) %>%
  summarise(IncidenceCount = sum(IncidenceCount)) %>%
  spread(GenderKey, IncidenceCount)

incidence %>%
  filter(IncidenceYearKey == max(IncidenceYearKey)) %>%
  group_by(AgeRangeKey, CountryKey) %>% #cat with fewer unique values across the top
  summarise(IncidenceCount = sum(IncidenceCount)) %>%
  spread(CountryKey, IncidenceCount)

incidence %>%
  filter(IncidenceYearKey == max(IncidenceYearKey)) %>% #must be equal to
  group_by(AgeRangeKey, CountryKey) %>% #sum incidences for every age and country combo
  summarise(IncidenceCount = sum(IncidenceCount)) %>% #proportion of total by country and age
  group_by(CountryKey) %>%
  mutate(Prop = IncidenceCount / sum(IncidenceCount)) %>%
  select(-IncidenceCount) %>%
  #mutate(Prop=round(Prop*100,2)) %>%
  mutate(Prop = scales::percent(Prop)) %>%
  spread(CountryKey, Prop)

incidence %>%
  filter(IncidenceYearKey == max(IncidenceYearKey)) %>%
  group_by(AgeRangeKey, GenderKey, CountryKey) %>%
  summarise(IncidenceCount = sum(IncidenceCount)) %>%
  unite("Gender-Age", c("GenderKey", "AgeRangeKey")) %>%
  spread(CountryKey, IncidenceCount) ->
  gender_age_country  #write to dataset

#unpivot table and specify new columns need
gender_age_country %>%
  gather(CountryKey, IncidenceCount,-'Gender-Age') %>%
  separate('Gender-Age', c("GenderKey", "AgeRangeKey"))

#if wish to format column names so that less likely to have case-sensitive problems
incidence %>%
  rename_all(tolower)
rename_all( ~ sub("key", "", .))

#View WHO dataset who
View(who)
View(population)

#pivot population data


population %>%
  spread(year, population)

who %>%
  gather(Measure, Value,-(country:year), #name of measure in a column and value in rows and in brackets further splitting out of that measure name
         na.rm = TRUE) #remove lots of NAs in cell
#separate(Measure, c("Type", "Something", GenderAge"), extra="merge") #extra merge removes separators between columns
#group_by(Measure)
#summarise(n()) %>%
#View()

#JOINS

incidence %>%
  inner_join(cancercodes, by = c("CancerSiteKey" = "CancerSiteKey")) ->
  incidence_cancers

#use equals to show which table heading want to join in both tables
#if not enough memory overwrite incidence, pereably not view in console

incidence %>%
  distinct(CancerSiteKey) %>%
  slice (1:5) ->
  exclusions

incidence %>%  #filter out those not matching
  anti_join(exclusions)

#join quicker than in filters (more succinct)

#PLOTS

library(datasauRus)
library(ggplot2)

#datasaurus_dozen %>%
#group_by(dataset) %>%
#summarise_all(mean) ->
#dd_summary

datasaurus_dozen %>%
  filter(dataset == "dino") %>% #add to get dino plot
  ggplot(aes(x = x, y = y, colour = dataset)) +
  geom_point() + #remember to add the plus sign to action next line
  #geom_point(data=datasetofchoice, colour="red") +
  #geom_path() + #plots in order of time in 2D
  #facet_wrap (~dataset) +
  theme_minimal() + #removes grey background
  theme(legend.position = "none") ->
  dd_plot

dd_plot %+% sample_n(datasaurus_dozen, 1000) #special plus avaialble in gg plots to add more data to chart (adding 1000 rows - can specify how much data add)

#adds some inaccuracy to stop data being identified ie for small numbers (or multiple points appear on same position)
ggplot(data = simpsons_paradox, aes(x = x, y = y, colour = dataset)) +
  geom_jitter(width = 5)

iris %>%
  ggplot(aes(x = SepalWidth, y = SepalLength, colour = Species)) +
  geom_point () +
  labs(
    x = "sepal width (cm)",
    y = "sepal length (cm)",
    title = "Size relationship",
    subtitle = paste("In cm,", nrow(iris), "observations, has been jittered")
  )

iris %>%
  ggplot(aes(x = SepalWidth, y = SepalLength, colour = Species)) +
  geom_jitter(width = 5)

ggsave("myfirstplot.pdf") #svg file format good for papers too but not avaial?

#DAY 2 RECAP

iris[1:5,-4]
letters[3:1] <-
  c("a", "b", "c") #transpose abc with cba in alphabet list
mylist <- list(a = "twitter", b = iris, who)
mylist[3] #to view who data
mylist["3"] #to view who data
mylist$b #to view who data

#read in file using readr?
cancer %>%
  count(CancerSiteLevel2Desc)

incidence %>%
  filter (IncidenceYearKey == max(IncidenceYearKey)) %>% #to get latest year of data
  inner_join(cancercodes, by = "CancerSiteKey") %>%
  group_by(CancerSiteLevel2Desc, GenderKey, AgeRangeKey, CountryKey) %>%
  summarise(IncidenceCount = sum(IncidenceCount)) %>%
  group_by(CancerSiteLevel2Desc) %>%
  mutate(Prop = scales::percent(IncidenceCount / sum(IncidenceCOunt))) %>%
  select(-IncidenceCount) %>%
  spread(AgeRangeKey, Prop) %>%
  View ()


incidence %>%
  filter (IncidenceYearKey == max(IncidenceYearKey)) %>%
  #to get latest year of data
  inner_join(cancer, by = "CancerSiteKey") %>%
  group_by(CancerSiteLevel2Desc, GenderKey, AgeRangeKey, CountryKey) %>%
  summarise(IncidenceCount = sum(IncidenceCount))
group_by(CancerSiteLevel2Desc) %>%
  mutate(Prop = scales::percent(IncidenceCount / sum(IncidenceCount))) %>%
  #mutate (Prop=ifelse(Prop=="NaN%, "-%", Prop)) %>% #replacing NaN with zero
  select(-IncidenceCount) %>%
  spread(AgeRangeKey, Prop) %>%
  View ()
#not recognising IncidenceYearKey. Why?

library(forcats)
#produce stacked bar chart
incidence %>%
  filter (IncidenceYearKey == max(IncidenceYearKey)) %>%
  inner_join(cancercodes, by = "CancerSiteKey") %>%
  group_by(CancerSiteLevel2Desc, GenderKey, AgeRangeKey, CountryKey) %>%
  summarise(IncidenceCount = sum(IncidenceCount)) %>%
  ggplot(aes(x = CancerSiteLevel2Desc, y = IncidenceCount)) +
  geom_col(position = "fill") +
  #geom_col(width = 1, fill=rgb(242, 76, 174, maxColorValue = 235)) +
  coord_flip()

#install plotly

ggplotly(myplot)

summarydata %>%
  plot_ly(x =  ~ AgeRangeKey, y =  ~ IncidenceCount(%>% ))
add_bars() %>%
  plotly::( ~ CancerSiteLevel2Desc)

#DAY 3 BASIC STATS

#rm("iris")

iris %>%
  lm(SepalLength ~ ., data = .) %>%
  #setosa is the ref group comparing to
  #summary()
  #stagnificance of model
  #coefficients()
  #predicted valuesrs show si
  fitted() ->
  lmfit

iris %>%
  ggplot(aes(x = 1:nrow(.), y = SepalLength)) +
  geom_point() +
  geom_point(aes(y = lmfit), colour = "red")

library (ggplot2)

library(tidyverse)
library(DBI)
library(odbc)
library(plotly)


iris %>%
  lm(SepalLength ~ ., data = .) %>%
  #plot()
  geom_histogram(aes(x = SepalWidth, y = SepalLength), colour = "red") #?

iris %>%
  lm(SepalLength ~ ., data = .) %>%
  predict(iris[5, ])

lmfit[5]

library(broom)

iris %>%
  lm(SepalLength ~ ., data = .) ->
  lmmodel

lmmodel %>%
  glance() %>%
  gather(Measure, Value)

lmmodel %>%
  augment() %>%
  ggplot(aes(x = .fitted, y = .std.resid)) +
  geom_point() +
  geom_smooth ()

library(modelr)

iris %>%
  sample_n(10) %>%
  modelr::add_predictions(model = lmmodel)

#Regressing incidnece data example, must use poisson not linear in reality


#LOGISTIC REGRESSION https://cruk3.lockedata.co.uk/logistic-regression



incidence %>%
  glm(
    IncidenceCount ~ CountryKey + GenderKey +
      AgeRangeKey + IncidenceYearKey,
    data = .,
    family = "quasipoisson"
  )

install.packages("optiRum")

library(optiRum)

logit.prob(1)
prob.logit(.5)
logit.odd(1)


incidence %>%
  glm(IncidenceCount ~ CountryKey + GenderKey +
        AgeRangeKey + IncidenceYearKey,
      data = .) %>%
  glance() ->
  modelB

incidence %>%
  mutate (LowIncidenceCount = as_factor(ifelse(IncidenceCount < 500,
                                               "low", "high"))) ->
  incidence

incidence %>%
  glm(
    IncidenceCount ~ CountryKey + GenderKey +
      AgeRangeKey + IncidenceYearKey,
    data = .,
    family = "binomial",
    model = FALSE,
    x = FALSE,
    y = FALSE
  ) ->
  glmmodel

#binomial not working as family to create glmmodel as glmmodel not recognised

library(tidyverse)
library (forcats)

library (broom)
library (modelr)
library (optiRum)

incidence %>%
  sample_n(10) %>%
  add_predictions(glmmodel) %>%
  mutate(prob = logit.prob(pred)) %>%
  select(pred, prob, everything())

#DECISION TREES

library (FFTrees)

library (tidyverse)

incidence %>%
  resample_partition(c("training" = 0.7,
                       "testing" = 0.3)) ->
  incidence2

#above made intermediary copy

incidence2$training %>%
  as_data_frame() ->
  train

incidence2$testing %>%
  as_data_frame() ->
  test

test %>%
  glm(
    IncidenceCount ~ CountryKey + GenderKey +
      AgeRangeKey + IncidenceYearKey,
    data = .,
    family = "poisson",
    model = FALSE,
    x = FALSE,
    y = FALSE
  ) ->
  glmmodel

#binomial not working as family to create glmmodel as glmmodel not recognised

test %>%
  add_predictions(glmmodel) %>%
  mutate(prob = logit.prob(pred)) %>%
  select(pred, prob, everything()) ->
  testresults

giniChart(testresults$pred, testresults$LowIncidenceCount)
#not able to complete the code in time

install.packages("caret") #can do without speech marks

library(caret)

confusionMatrix(ifelse(testresults$prob < .5,
                       "low", "high"),
                testresults$LowIncidenceCount)

lift(testresults$LowIncidenceCount ~ testresults)

#DATA CLEANING

summary(incidence)

featurePlot(test[, 2:6], test$IncidenceCount)

install.packages("PASWR")
library(PASWR)

titanic3() <- readr::read_csv
View(titanic3)
summary(titanic3)

titanic3 %>%
  skimr::skim()

titanic3 <- read_excel(titanic3)

install.packages("Hmisc")
library(Hmisc)

library(titanic)
library(forcats)

titanic3 %>%
  mutate(age = fct_explicit_na(cut2(age, g = 10))) %>%
  group_by(age) %>%
  filter(fare < 500) %>%
  summarise(n(), mean(survived)) %>%
  View()

titanic3 %>%
  mutate(age = fct_explicit_na(cut2(age, g = 10))) %>%
  group_by(age) %>%
  filter(fare < 500) ->
  titanic_raw

set.seed(98986)  #bootstrapping
titanic_raw %>%
  resample_partition(c("train" = .7, "test" = .3)) ->
  titanic_samples
#mutate(age=fct_explicit_na(cut2(fare, g = 10))) %>%
group_by(age) %>%
  filter(fare < 500)

titanic_sample$train %>%
  as_data_frame() %>%
  preProcess() ->
  scalingModel

titanic_sample$test %>%
  as_data_frame() %>%
  preProcess() ->
  scalingModel

titanic_sample$train %>%
  as_data_frame() %>%
  predict(scalingModel, .) ->
  training

titanic_sample$test %>%
  as_data_frame() %>%
  predict(scalingModel, .) ->
  testing

training %>%
  glm(survival ~ pclass + sibsp + parch + fare + sex + age,
      data = .,
      family = "binomial") ->
  titanic_model

titanic3 %>%
  mutate(age = fct_explicit_na(cut2(age, g = 10))) %>%
  #mutate survived to factor variables "D" "S" to do binomial regression
  filter(fare < 500) %>%
  mutate(survived = as_factor(ifelse(survivaed, "S", "D"))) ->
  titanic_raw

varImp(titanic_model)

training %>%
  glm(survival ~ pclass + sibsp + parch + fare + sex + age,
      data = .,
      family = "binomial") ->
  titanic_model

#predicting who died
titanic_model %>%
  augment() #gives logit resepectively if will survive
ggplot(aes(x = .fitted)) +
  geom_density() +
  facet_wrap( ~ survived) #view by variables

#areas overlap are areas with difficulty with prediction


#DAY 4 ADVANCED

letters[letters < LETTERS]

library(tidyverse)

#change rows to columns use mutate

mtcars %>%
  mutate(car = rownames(.)) %>%
  View()

#spread is to pivot and gather to unpivot but need to state columns and columns not want to pivot

who %>%
  gather (Measure, Value,-(country:year)) %>%
  group_by(country, Measure) %>%
  summarise(Avg = mean(Value, na.rm = TRUE)) %>%
  mutate(Avg = ifelse(is.nan(Avg),-99, Avg)) #remove NaN to NA

library(ggplot2movies)
library(modelr)

#rdrr.io to find which package code come from and run examples

movies %>%
  resample_partition(c(train = .7, test = .3)) ->
  movies_split

movies_split$train %>%
  as_data_frame() %>%
  #mutate(year=year-min(year)) %>%
  #mutate(year=year-min(year)/(2020-min(year))) %>% #comparing to the first year so don't need to add year 1900 to intercept
  lm(
    rating ~ year + length + Action + Animation + Comedy +
      Drama + Documentary + Romance + Short,
    data = .
  ) ->
  movies_ratings

library(ggplot2)
movies_split$test %>%
  as_data_frame() %>%
  #mutate(year=year-min(as_data_frame(movies_split$train)$year)) %>%
  add_predictions(model = movies_ratings) %>%
  mutate(residuals = rating - pred) %>%
  ggplot(aes(x = residuals)) +
  geom_density()

#see which rows are causing problems
movies_split$test %>%
  as_data_frame() %>%
  add_predictions(model = movies_ratings) %>%
  mutate(residuals = rating - pred) %>%
  filter(residuals < (-5)) %>%
  View()

#STRING MANIPULATION

library(stringr)

simple <- "This IS HOrribly typed! "
numbers<-c("02", "11","10","1")
str_to_lower(simple)
str_to_upper(simple)

sort(numbers)
str_sort(numbers,numeric = TRUE)
str_length(numbers)

str_split(simple," ")
str_split(simple,"i|I")
str_split(simple,"[iI]")
str_split(simple, boundary("word"))

#remove apostrophes
simple2<-"Thi's IS HOrrib1y typed!"
str_split(simple2,boundary("word"))

str_extract(simple,"r+")
str_extract(str_to_lower(simple),"is.*")
str_count(simple, "is | IS")
str_count(str_to_lower(simple), "is")

#standardised all text data (useful for survey data)
who %>%
  mutate_if(is_character, str_to_upper)

who %>%
  mutate_if(is_character, str_to_upper)

everyletterofthealphabet<-"the quick brown fox jumps over the lazy dog"
str_extract(str_to_upper,"the quick brown fox jumps over the lazy dog")
str_count(simple,"the quick brown fox jumps over the lazy dog")

"the quick brown fox jumps over the lazy dog" %>% 
  str_to_upper() %>% 
  str_split(boundary("word")) %>%
  .[[1]] %>%  #two sq brackets returns without list wrapping
  str_length()

library(forcats)
myFactor<-as.factor(c("red","blue","yellow",NA,"red"))
fct_count(myFactor)
myFactor %>% 
  fct_count()

myFactor %>% 
  fct_explicit_na() %>% 
  fct_lump(n=1) %>% 
  fct_count()

#show missing
myFactor %>% 
  fct_lump(n=1) %>% 
  fct_explicit_na() %>%
  fct_count()

gss_cat %>%
  count(year)

gss_cat %>%
  fct_count(as.factor(gss_cat$year))

#anonymise data so that can't see religion and race and can see if trying to find out
gss_cat %>%
  mutate(marital=fct_lump(marital,n=2),race=fct_anon(race),relig=fct_anon(relig))

#A package for sticking things together (glue and glue data)

library(glue)

age<-40
gender<-"Male"
location<-"England"
cancerlocation<-"Thyroid"
glue("The most common cancer for {ifelse(gender
     =='Female','women','men')} in {location} aged {round(age,-1)} to {round
     (age,-1)+10} is cancer of the {cancerlocation}.")

#or can specify age ranges using lower bound and upper bound
lb=round(age,-1),
ub=rounf(age,-1)+10)

cancercodes<-read_csv("data/cancer.csv")

cancercodes %>% 
  glue_data ("The code for {CancerSiteLevel6Desc} is {CAncerSiteKey}. {state}", state=":sadface:")

library(tidyverse)
library(DBI)
library(odbc)

incidence %>% 
  group_by(IncidenceYearKey, CancerSiteKey, CountryKey, AgeRangeKey, GenderKey) %>% 
  summarise(IncidenceCount=sum(IncidenceCount)) %>% 
  group_by(IncidenceYearKey,CountryKey,GenderKey) %>% 
  mutate(prop=IncidenceCount/sum(IncidenceCount)) %>% 
  glue_data("In {year}, for {GenderKey} aged 
            {AgeRangeKey} in {CountryKey} there were {format(IncidenceCount)} 
            incidences of cancer.  This is {scales::percent(prop)} of all cancers for {GenderKey} 
            aged {AgeRangeKey} in {CountryKey} that year."),
  year=IncidenceYearKey, 
  pNum=format(IncidenceCount, trim=TRUE), 
  per=scales::percent(prop)

library(pivottabler)

#create new one
pt <- PivotTable$new()
pt$addData(incidence)
pt$addRowDataGroups("AgeRangeKey")
pt$addColumnDataGroups("GenderKey")
                      pt$defineCalculation(calculationName="Incidence",summariseExpression="sum(IncidenceCount)")
                      pt$renderPivot()

#quick pivot but slower than if do it in dplayr)

qhpvt(dataFrame = incidence,
      rows = c("AgeRangeKey"),
      columns= c("CountryKey","GenderKey"),
      calculations = "sum(IncidenceCount)")
      #calculations = c(a="round(sum(IncidenceCount),-3)","a b"="sum(IncidenceCount)")

#pt$getLatex() gets file

writeLines(pt$getLatex(), "pivot.tex") #can view in Files tab or in project folder and open in nay text application

pt$theme<-"largeplain"
pt$renderPivot()
#can overwrite the theme repetitively
pt$renderPivot()

cruk <- list(
  headerBackgroundColor = "#f24cae",
  headerColor = "rgb(233,255,255)")

theme<- getSimpleColoredTheme(pt, "cruk", colors=cruk,fontName="Museo, Arial")
pt$theme<-theme
pt$renderPivot()

#name style

#conditional formatting

s <- PivotStyle$new(pt, styleName="cellHighlight",
declarations=list("color"="red"))
pt$renderPivot()

#needs attributes above, missing g and gc in order to apply some of below so can do sequence of RAG using conditional formatting
s <- PivotStyle$new(pt, styleName="cellBold",
  declarations=list("font-weight"="bold"))
g<-pt$findCells(minValue = 456500)
gc<-lapply(g, function(cell) {cell$style<- s})
  pt$renderPivot()






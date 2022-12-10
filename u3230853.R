
# Data Capture and Preparation Exam R Codes


library(tidyverse)

# Question no. 8

names <- c('Apple', 'carlos', 'maradona', 'Diego', 'Olives', 'Water', 'Africa', 'identitiy')
removing <- c()
for (x in names) {
  if(grepl('^[aeiouAEIOU].{5,}', x)){
    print(x)
  }
  else{
    removing <- append(removing, x)
  }
}
names <- names[!(names %in% removing)]
names

#Question no. 17

data <- tibble(
  name = c("Carl","Josh","Laura"),
  wt_Tuesday_5pm = c(100, 150, 140),
  wt_Wednesay_3pm = c(104, 155, 138),
  wt_Friday_9m = c(NA, 160, 142)
)

Average_Weight <- data %>% mutate_if(is.numeric, ~ . * 0.453592) %>% 
  summarize_if(is.numeric, sum, na.rm=TRUE) %>% 
  mutate(avg_wt = (wt_Tuesday_5pm + wt_Wednesay_3pm + wt_Friday_9m)/8) %>% 
  mutate(wt_Tuesday_5pm = NULL, wt_Wednesay_3pm=NULL, wt_Friday_9m=NULL)
Average_Weight


# Question number 18

data <-  read.csv("exam_data.csv")

library(dplyr)
#************ Exercise 1 **************

# 2. Store the link
url <- "https://extranet.who.int/tme/generateCSV.asp?ds=notifications"


# 3-5. Load data into R and convert to tibble
data <- as_tibble(read.csv(url,header = TRUE))

# using pipes
data <- url %>% read.csv(header = TRUE) %>% as_tibble()

# 6. Check out the data (use any of these)
print(data)
glimpse(data)

head(data)
tail(data)


#************ Exercise 2 **************

# 1. get the name of the seventh column
start <- colnames(data)[7]

# get the name of the last column
end <- colnames(data)[ncol(data)]

# 2-3. Convert from wide to long and fix warning
data1 <- data %>%
  gather(
    start:end, 
    key = "key",
    value = "cases",
    na.rm = TRUE
  )%>% 
  as_tibble()


#************ Exercise 3 **************

# 1. Count variables in key column
count_var <- data1 %>% count(key)

# 2. Order the count_var in descending order
ordered_count <- count_var %>% arrange(desc(n))

#3. get new url with data dictionary
url2 <- "https://extranet.who.int/tme/generateCSV.asp?ds=dictionary"

# 3. store data into a new tibble
dictionary <- url2 %>% read.csv(header = TRUE) %>% as_tibble()

# 4. have a glimpse into the data
glimpse(dictionary)

# 4. Find and print the meaning of the variables with more than 3500 counts
# 4.a. identify the name of those variables with counts > 3500 
# use the pull function to get the names of the variables as a vector instead of tibble
keys <- ordered_count %>% filter(n >3500) %>% pull(key)

# 4.b. print the definition for each variable with count >3500
# To compare multiple values in filter() use %in% instead of ==
dictionary %>% filter(variable_name %in% keys) %>% select(variable_name,definition) %>% print()




#************ Exercise 4 **************
#***** First Part *****
# 1-2. Fix incosistent name
data2 <- data1 %>%
  mutate(key = str_replace(key, "newrel", "new_rel"))


#***** Second Part *****
# 1-2. Focus on variables that have age (number)
data3 <- data2 %>% filter(str_detect(key,"[0-9]"))  %>% as_tibble()

# 3-4. Split the key column into three new variables
data4 <- data3 %>% separate(key, c("new", "type", "sexage"), sep="_")  %>% as_tibble()

# 5-6. Split the variable name "sexage" into two
data5 <- data4 %>% separate(sexage, c("sex","age"), sep=1)  %>% as_tibble()

#***** Third Part *****

# 1-2. Remove duplicates and constants
data6 <- data5 %>% select(-iso2, -iso3, -new)  %>% as_tibble()


x <- data6 %>% 
  group_by(country) %>% 
  summarise(cases = sum(cases))

y <- data6 %>% 
  group_by(year) %>% 
  summarise(cases = sum(cases))
z <- data6 %>% 
  group_by(sex) %>% 
  summarise(cases = sum(cases))

da <- data %>% filter(year >= 2015, sex=="m" | sex=="f" )

unique(da$year)
unique(da$sex)




# Question no. 21

Adi <- ""An apple weights 250 grms, a mango weights 525 grms, in total they weight 773 grms."
str_extract(Adi, "[a-zA-Z]+\\s(weights)\\s\\d+\\s?(grms)")







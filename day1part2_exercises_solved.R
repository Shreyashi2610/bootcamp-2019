install.packages("here")
here::here()

gapminder <- read.csv(here::here("data", "gapminder5.csv"))
str(gapminder) #Problem : Characters read in as Factors
head(gapminder)

gapminder$country = as.character(gapminder$country)
gapminder$continent = as.character(gapminder$continent)
str(gapminder)


##FOR LOOP

#Mean Life Expectancy by Country
mean(gapminder$lifeExp[gapminder$country == 'Afghanistan'])
mean(gapminder$lifeExp[gapminder$country == 'Albania'])
#Loop it!
obs <- 1:nrow(gapminder) #vector of iterator = each row

for (i in obs) {
    gapminder[i, "gdp"] <- gapminder[i, "pop"] * gapminder[i, "gdpPercap"]
}
head(gapminder)

#Creating new columns - log_gdpPerCap and log_pop that store logarithm of gdpPerCap and pop respectively
for (i in obs){
    gapminder[i, "log_gdpPercap"] <- log(gapminder[i, "gdpPercap"])
    gapminder[i, "log_pop"] <- log(gapminder[i, "pop"])
}
head(gapminder)

#If we want to avoid loops
gapminder$vec_log_pop <- log(gapminder$pop)
all(gapminder$vec_log_pop == gapminder$log_pop)


#Has life expectancy increased over time?
years <- unique(gapminder$year) #returns unique values of year in gapminder dataset

for (i in years) {
    mean_le <- mean(gapminder$lifeExp[gapminder$year == i], na.rm = TRUE)
    print(paste0(i, ": ", mean_le)) #paste0 - concatenates strings and variable values
}

#Mean life expectancy by Continent
cont <- unique(gapminder$continent)

for (i in cont) {
    mean_le <- mean(gapminder$lifeExp[gapminder$continent == i], na.rm = T)
    print(paste0(i, ": ", mean_le))
}

#Nested For Loops
#What is the mean life expectancy of each continent each year?
for (i in cont){
    print(paste0("Continent: ", i))
    for (j in years){
        mean_le <- mean(gapminder$lifeExp[gapminder$continent==i & gapminder$year==j], na.rm = T)
        print(paste0(j, ": ", mean_le))
    }
}

#Has the gap in life expectancy between countries on different continents narrowed over time?
for (i in cont){
    print(paste0("Continent: ", i))
    for (j in years){
        sd_le <- sd(gapminder$lifeExp[gapminder$continent==i & gapminder$year==j], na.rm = T)
        print(paste0(j, ": ", sd_le))
    }
}

##APPLY
vars <- gapminder[, c("lifeExp","pop","gdpPercap")]
apply(vars, 2, mean)

#lapply
lapply(gapminder, mean)
sapply(gapminder, mean)

sapply(years, function(x) mean(gapminder$lifeExp[gapminder$year == x]))


##WHILE LOOP
i <- 1952 #Initiate the iterator

while (i < 1987) {
    sd_le <- sd(gapminder$lifeExp[gapminder$year == i])
    print(paste0(i, ": ", sd_le))
    i <- i + 5 #increment the iterator inside the loop
}

#Std Dev for life expectancy of each year between 1987 and 2002 both inclusive
i <- 1987 #Initiate the iterator

while (i <= 2002) {
    sd_le <- sd(gapminder$lifeExp[gapminder$year == i])
    print(paste0(i, ": ", sd_le))
    i <- i + 5 #increment the iterator inside the loop
}


##IF/ELSE CONDITIONAL
set.seed(1)
random_year <- sample(years, 1)
random_year

if (random_year > 1995) {
print(random_year)
} else {
print("Sorry too old!")
}


#Which continents have a mean life expectancy greater than 70 yrs?
threshold <- 70

for (i in unique(gapminder$continent)) {
    tmp <- mean(gapminder$lifeExp[gapminder$continent == i])
    
    if (tmp < threshold) {
        print(paste("Mean life expectancy of ",i," is less than", threshold))
    } else {
        print(paste("Mean life expectancy of ",i," is greater than", threshold))
    }
}

#Write a for loop that reports the mean population for years greater than or equal to 1987.
#Print a message if condition is not met
for (i in unique(gapminder$year)) {
    #tmp <- mean(gapminder$pop[gapminder$year == i])
    if (i >= 1987) {
        print(paste("Mean Population for year ",i," is ", mean(gapminder$pop[gapminder$year == i])))
    } else {
        print("Year is before 1987")
    }
}



##FUNCTIONS
get_values <-
    function(df, variable = 'continent') {
        vals <- unique(df[[variable]])
        print(paste0(variable, ": ", vals))
    }

#Write a function that prints the mean and std dev for life expectancy for a given country
report_mean_sd <-
    function(df, variable, country) {
        cat("\nCountry: ", country,
            "\nMean Life Expectancy: ", mean(df[[variable]][df$country == country]),
            "\nStd Dev of Life Expectancy: ",sd(df[[variable]][df$country == country]))
    }

for(i in unique(gapminder$country)) {
    report_mean_sd(gapminder, "lifeExp", i)
}

#Write a function that reports mean, median, min, max for Life Expectancy of a continent in gapminder
report_stats <-
    function(df, variable, continent) {
        cat("\n\nContinent: ", continent,
            "\nMean Life Expectancy: ", mean(df[[variable]][df$continent == continent]),
            "\nMedian Life Expectancy: ", median(df[[variable]][df$continent == continent]),
            "\nMin Life Expectancy: ", min(df[[variable]][df$continent == continent]),
            "\nMax Life Expectancy: ", max(df[[variable]][df$continent == continent]))
    }

for(i in unique(gapminder$continent)) {
    report_stats(gapminder, "lifeExp", i)
}









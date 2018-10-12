HW3\_Markdown
================
Noah Kreski
October 12, 2018

Problem One
===========

``` r
#This imports the data and cleans the names.
BRFSS_Cleaned = janitor::clean_names(brfss_smart2010)%>%
                #The next two lines focus on overall health and entries with one of five relevant response levels.
                filter(topic == "Overall Health")%>%
                filter(response %in% c("Excellent", "Very good", "Good", "Fair", "Poor"))%>%
                #This creates an ordered factor of response levels.
                mutate(response = (factor (response, levels = c("Excellent", "Very good", "Good", "Fair", "Poor"))))
```

### In 2002, which states were observed at 7 locations?

``` r
#This gets the 2002 entries.
filter(BRFSS_Cleaned, year == 2002)%>%
#This includes a single datum for each location
distinct(locationdesc,.keep_all = TRUE)%>%
#These two lines tell us how many locations exist by state
group_by(locationabbr)%>%
summarize(n = n())
```

The states observed at 7 locations in 2002 are Connecticut, Florida, and North Carolina

### Make a “spaghetti plot” that shows the number of locations in each state from 2002 to 2010.

``` r
BRFSS_Spaghetti = BRFSS_Cleaned%>%
                  #This includes all unique locations by year
                  distinct(year,locationdesc,.keep_all = TRUE)%>%
                  #These two lines tell us how many locations exist by state and year
                  group_by(year,locationabbr)%>%
                  summarize(n = n())
ggplot(data = BRFSS_Spaghetti, aes(x=year, y=n, group=locationabbr, color = locationabbr)) + geom_line()
```

![](HW3_Markdown_files/figure-markdown_github/spaghetti-1.png) This spaghetti plot tracks the number of observations by year and state.

Problem Two
===========

### Write a short description of the dataset, noting the size and structure of the data, describing some key variables, and giving illstrative examples of observations

This data, with 1,384,617 rows and 15 columns, articulates information about grocery shopping behavior. Key variables include aisle, which describes the location of an item, and product\_id, which gives a numeric code for a purchased product. For instance, order\_id 1 included items such as Bulgarian Yogurt and Organic 4% Milk Fat Whole Milk, both in the department 'dairy eggs'. Additionally, I know the hour of day during this order was 10 am, that it has been 9 days since the last order, and that yogurt was added to the cart first.

### How many aisles are there, and which aisles are the most items ordered from?

``` r
instacart%>%
group_by(aisle)%>%
summarize(obs=n())


instacart%>%
group_by(aisle)%>%
summarize(obs=n())%>%
top_n(n=5)
```

This summarization code provides a summary with 134 rows, one for each aisle, so there are 134 aisles. The 5 most popular aisles are fresh fruits, fresh vegetables, packaged cheese, packages vegetables & fruits, and yogurt, the number one aisle being fresh vegetables.

### Make a plot that shows the number of items ordered in each aisle. Order aisles sensibly, and organize your plot so others can read it.

``` r
instacart_summary_data = instacart%>%
                         group_by(aisle_id)%>%
                         summarize(obs=n())
ggplot(instacart_summary_data, aes(x = aisle_id, y = obs)) + 
  geom_point() + labs(title = "Items Ordered per aisle", x = "Aisle", y = "Orders")
```

![](HW3_Markdown_files/figure-markdown_github/aisles%20plot-1.png)

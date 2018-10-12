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

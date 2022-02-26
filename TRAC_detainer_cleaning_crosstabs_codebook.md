---
title: 'TRAC detainers: cleaning, crosstabs, and codebook'
author: "Dashiell Nusbaum"
date: "9/29/2021"
output: 
  html_document:
    keep_md: true
---



# Libraries

```r
library(tidyverse) # general manipulation
library(data.table) # for loading in large data more quickly with fread rather than read_csv() from tidyverse (though both work, don't necessarily need this)
library(reactable) # for better looking table output
library(reactablefmtr) # for better looking table output
library(haven) # this package allows us to read in (and write to) data tables in a STATA file format (.dta)
library(usmap) # useful for getting fips codes for states
library(janitor) # gives us the tabyl() function, which allows us to make crosstabs
#library(writexl) # allows us to write the codebook to an excel file
```

# Importing data into R

```r
# the detainer datasets from https://www.dropbox.com/sh/epaluor9o05tx0c/AACXAmgdZOPat_GswK5LIKuEa?dl=0
detainer_all_1607 <- read_dta("./Detainerall_1607.dta")
detainer_all_1905 <- read_dta("./Detainerall_1905.dta")

# dataframe that has the county fips codes. skip the first 4 lines, since the variable names start in row 5. From: https://www.census.gov/geographies/reference-files/2020/demo/popest/2020-fips.html
census_county_fips_codes <- fread(file = "./census_county_fips_codes.csv", skip = 4) %>%
  filter(`County Code (FIPS)` != 0) # get rid of rows where the county code is 0, since those are rows that are duplicates, where the same county has multiple rows b/c there are diffent, say, city codes (or something else) within it. if we don't get rid of this, it will create duplicates when we left_join()

country_regions_base <- read_csv("https://raw.githubusercontent.com/lukes/ISO-3166-Countries-with-Regional-Codes/master/all/all.csv") # has country regions to join in
```

# Append the two datasets so that you have a single data file with all the detainer data.

```r
plyr::rbind.fill(detainer_all_1607, detainer_all_1905) -> # bind the two tables together by row (so add the 2nd one below the 1st), and do rbind.fill, so if a column doesn't exist in the other dataframe, it fills with NAs
  detainer_all # write it to a new df called detainer_all

detainer_all %>%
  mutate(citizenship_country = str_to_title(citizenship_country),
         birth_country = str_to_title(birth_country)) -> # change it to be as a title rather than all-caps
  detainer_all
```



# Pre-clean

## Adding my reactable theme

```r
# function for my reactable theme since using the same for every table

reactable_func <- function(df, width) { # function that takes dataframe & width
  
  df %>% # take df
    reactable(theme = cosmo(), # add theme
              outlined = TRUE, striped = TRUE, # outline and stripe table
              fullWidth = FALSE, width = width, # dont stretch df, set pxl wdth
              defaultColDef = colDef(
                align = "center", # align all columns in center
                style = color_scales(df, # color all rows...
                                     color_ref = "color")), # based on color var
              columns = list(color = colDef(show = FALSE))) # hide color ref var
}
```



## See if there are any typos and look for unexpected values for different varaibles

```r
detainer_all %>%
  distinct(state) %>% # get all distinct state values, see if any typos or unexpected
  arrange(state) %>% # arrange alphabetically
  mutate(color = case_when(TRUE == FALSE ~ "#a1d99b")) -> # don't want to color
  states_df

reactable_func(states_df, width = 150) # reactable of df to show user
```

```{=html}
<div id="htmlwidget-39f9abf69cda7885c261" class="reactable html-widget" style="width:150px;height:auto;"></div>
<script type="application/json" data-for="htmlwidget-39f9abf69cda7885c261">{"x":{"tag":{"name":"Reactable","attribs":{"data":{"state":["","AK","AL","AR","AZ","CA","CO","CT","DC","DE","FL","GA","GU","HI","IA","ID","IL","IN","KS","KY","LA","MA","MD","ME","MI","MN","MO","MP","MS","MT","NC","ND","NE","NH","NJ","NM","NV","NY","OH","OK","OR","PA","PR","RI","SC","SD","TN","TX","UN","UT","VA","VI","VT","WA","WI","WV","WY"],"color":[null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null]},"columns":[{"accessor":"state","name":"state","type":"character","align":"center","style":[{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"}]},{"accessor":"color","name":"color","type":"character","align":"center","style":[{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"}],"show":false}],"defaultPageSize":10,"paginationType":"numbers","showPageInfo":true,"minRows":1,"outlined":true,"striped":true,"inline":true,"width":"150px","theme":{"color":"#141415","backgroundColor":"#f8f9fa","borderColor":"#f8f9fa","borderWidth":"1px","stripedColor":"white","highlightColor":"white","cellPadding":6,"tableStyle":{"fontFamily":"Verdana","fontSize":14},"headerStyle":{"borderWidth":"2px","backgroundColor":"#373a3c","color":"#ffffff","transitionDuration":"0.5s","&:hover[aria-sort]":{"color":"#ffffff"},"&[aria-sort='ascending'], &[aria-sort='descending']":{"color":"#ffffff"},"fontSize":15,"fontFamily":"Verdana"},"groupHeaderStyle":{"&:not(:empty)":{"color":"#141415","fontSize":15,"fontFamily":"Verdana"},"&:hover":{"fontWeight":"bold","transitionDuration":"1s","transitionTimingFunction":"ease-out","color":"#111111"}},"rowSelectedStyle":{"backgroundColor":"#78c2ad","color":"#ffffff"},"inputStyle":{"backgroundColor":"#ffffff","color":"#9a9a9a"},"searchInputStyle":{"backgroundColor":"#ffffff","color":"#9a9a9a","borderColor":"#b3cecc","&:focus":{"color":"#888888"}},"selectStyle":{"backgroundColor":"#2780e3","color":"#ffffff","borderColor":"#ffffff","outlineColor":"#ffffff"},"pageButtonStyle":{"backgroundColor":"#ffffff","color":"#2780e3","&:hover":{"backgroundColor":"#2780e3","color":"#ffffff"}},"pageButtonHoverStyle":{"backgroundColor":"#2780e3","color":"#ffffff"},"pageButtonActiveStyle":{"backgroundColor":"#2780e3","color":"#ffffff"},"pageButtonCurrentStyle":{"backgroundColor":"#2780e3","color":"#ffffff"}},"dataKey":"aee382c8330d2e7761a8edcf2addafd8","key":"aee382c8330d2e7761a8edcf2addafd8"},"children":[]},"class":"reactR_markup"},"evals":[],"jsHooks":[]}</script>
```

```r
"There are 57 unique values in the state variable"
```

```
## [1] "There are 57 unique values in the state variable"
```

```r
# values of state where the state_fip were missing, and adding them if possible
# missing FIPS codes all available here: https://www.nrcs.usda.gov/wps/portal/nrcs/detail/?cid=nrcs143_013696
detainer_all %>%
  mutate(state_fip = fips(state = state)) %>%
  mutate(state_fip = as.numeric(state_fip)) %>% # change it from a character to a numeric
  distinct(state, state_fip) %>%
  arrange(state) %>%
  filter(is.na(state_fip) == TRUE) %>%
  mutate(state_fip = case_when(state == "AS" ~ 60, # if the state is "AS" (American Samoa), make the value in state_fips 60 (fips code = 60) (before this, it was NA)
                               state == "GU" ~ 66, # if the state is "GU", make the value in state_fips 66
                               state == "MP" ~ 69,
                               state == "PR" ~ 72,
                               state == "VI" ~ 78,
                               is.na(state_fip) == FALSE ~ state_fip # if the value in state_fip is not NA, then keep the value in state_fip the same
                               )) %>%
  mutate(color = case_when(TRUE == FALSE ~ "#a1d99b")) -> # don't want to color
  state_fips_df

reactable_func(state_fips_df, width = 250) # reactable of df to show user
```

```{=html}
<div id="htmlwidget-49b93e91014b0dd0ca20" class="reactable html-widget" style="width:250px;height:auto;"></div>
<script type="application/json" data-for="htmlwidget-49b93e91014b0dd0ca20">{"x":{"tag":{"name":"Reactable","attribs":{"data":{"state":["","GU","MP","PR","UN","VI"],"state_fip":["NA",66,69,72,"NA",78],"color":[null,null,null,null,null,null]},"columns":[{"accessor":"state","name":"state","type":"character","align":"center","style":[{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"}]},{"accessor":"state_fip","name":"state_fip","type":"numeric","align":"center","style":[{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"}]},{"accessor":"color","name":"color","type":"character","align":"center","style":[{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"}],"show":false}],"defaultPageSize":10,"paginationType":"numbers","showPageInfo":true,"minRows":1,"outlined":true,"striped":true,"inline":true,"width":"250px","theme":{"color":"#141415","backgroundColor":"#f8f9fa","borderColor":"#f8f9fa","borderWidth":"1px","stripedColor":"white","highlightColor":"white","cellPadding":6,"tableStyle":{"fontFamily":"Verdana","fontSize":14},"headerStyle":{"borderWidth":"2px","backgroundColor":"#373a3c","color":"#ffffff","transitionDuration":"0.5s","&:hover[aria-sort]":{"color":"#ffffff"},"&[aria-sort='ascending'], &[aria-sort='descending']":{"color":"#ffffff"},"fontSize":15,"fontFamily":"Verdana"},"groupHeaderStyle":{"&:not(:empty)":{"color":"#141415","fontSize":15,"fontFamily":"Verdana"},"&:hover":{"fontWeight":"bold","transitionDuration":"1s","transitionTimingFunction":"ease-out","color":"#111111"}},"rowSelectedStyle":{"backgroundColor":"#78c2ad","color":"#ffffff"},"inputStyle":{"backgroundColor":"#ffffff","color":"#9a9a9a"},"searchInputStyle":{"backgroundColor":"#ffffff","color":"#9a9a9a","borderColor":"#b3cecc","&:focus":{"color":"#888888"}},"selectStyle":{"backgroundColor":"#2780e3","color":"#ffffff","borderColor":"#ffffff","outlineColor":"#ffffff"},"pageButtonStyle":{"backgroundColor":"#ffffff","color":"#2780e3","&:hover":{"backgroundColor":"#2780e3","color":"#ffffff"}},"pageButtonHoverStyle":{"backgroundColor":"#2780e3","color":"#ffffff"},"pageButtonActiveStyle":{"backgroundColor":"#2780e3","color":"#ffffff"},"pageButtonCurrentStyle":{"backgroundColor":"#2780e3","color":"#ffffff"}},"dataKey":"cd164ac9d34994185cf5bf974138f2f0","key":"cd164ac9d34994185cf5bf974138f2f0"},"children":[]},"class":"reactR_markup"},"evals":[],"jsHooks":[]}</script>
```

```r
"There are two values of 'state' for which 'state_fip' is still NA: an empty/blank value; and 'UN', which I assume is Unknown given that the 'county' variable is 'Unknown County' for all rows where state = 'UN'"
```

```
## [1] "There are two values of 'state' for which 'state_fip' is still NA: an empty/blank value; and 'UN', which I assume is Unknown given that the 'county' variable is 'Unknown County' for all rows where state = 'UN'"
```

```r
# commented out below is another way to get the fips code of every state (including guam, peurto rico, etc). Not using it because A) the code isn't intuitive to anyone who doesn't know much about web scraping in R and B) if the link ever changes (less likely b/c it's a .gov(?), then scraping from that no longer possible)
#rvest::read_html("https://www.nrcs.usda.gov/wps/portal/nrcs/detail/?cid=nrcs143_013696") %>%
#  rvest::html_nodes(css = ".data :nth-child(1)") %>%
#  .[[1]] %>%
#  rvest::html_table() %>%
#  rename(state = `Postal Code`)

# could also download a dataset w/ the fips codes by state abbreviation and left_join() it, but ideally, we want the user to have to do as little as possible, which means don't want them to have to download and put in folder more files than necessary

detainer_all %>%
  distinct(gender) %>% # all values of gender in the df
  mutate(color = case_when(TRUE == FALSE ~ "#a1d99b")) -> # don't want to color
  gender_distinct_df

reactable_func(gender_distinct_df, width = 250) # reactable of df to show user
```

```{=html}
<div id="htmlwidget-a3dd7c8ec15ffdcd682a" class="reactable html-widget" style="width:250px;height:auto;"></div>
<script type="application/json" data-for="htmlwidget-a3dd7c8ec15ffdcd682a">{"x":{"tag":{"name":"Reactable","attribs":{"data":{"gender":["Male","Female","Unknown"],"color":[null,null,null]},"columns":[{"accessor":"gender","name":"gender","type":"character","align":"center","style":[{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"}]},{"accessor":"color","name":"color","type":"character","align":"center","style":[{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"}],"show":false}],"defaultPageSize":10,"paginationType":"numbers","showPageInfo":true,"minRows":1,"outlined":true,"striped":true,"inline":true,"width":"250px","theme":{"color":"#141415","backgroundColor":"#f8f9fa","borderColor":"#f8f9fa","borderWidth":"1px","stripedColor":"white","highlightColor":"white","cellPadding":6,"tableStyle":{"fontFamily":"Verdana","fontSize":14},"headerStyle":{"borderWidth":"2px","backgroundColor":"#373a3c","color":"#ffffff","transitionDuration":"0.5s","&:hover[aria-sort]":{"color":"#ffffff"},"&[aria-sort='ascending'], &[aria-sort='descending']":{"color":"#ffffff"},"fontSize":15,"fontFamily":"Verdana"},"groupHeaderStyle":{"&:not(:empty)":{"color":"#141415","fontSize":15,"fontFamily":"Verdana"},"&:hover":{"fontWeight":"bold","transitionDuration":"1s","transitionTimingFunction":"ease-out","color":"#111111"}},"rowSelectedStyle":{"backgroundColor":"#78c2ad","color":"#ffffff"},"inputStyle":{"backgroundColor":"#ffffff","color":"#9a9a9a"},"searchInputStyle":{"backgroundColor":"#ffffff","color":"#9a9a9a","borderColor":"#b3cecc","&:focus":{"color":"#888888"}},"selectStyle":{"backgroundColor":"#2780e3","color":"#ffffff","borderColor":"#ffffff","outlineColor":"#ffffff"},"pageButtonStyle":{"backgroundColor":"#ffffff","color":"#2780e3","&:hover":{"backgroundColor":"#2780e3","color":"#ffffff"}},"pageButtonHoverStyle":{"backgroundColor":"#2780e3","color":"#ffffff"},"pageButtonActiveStyle":{"backgroundColor":"#2780e3","color":"#ffffff"},"pageButtonCurrentStyle":{"backgroundColor":"#2780e3","color":"#ffffff"}},"dataKey":"b2784305d2256890714a5a8a7e4677fb","key":"b2784305d2256890714a5a8a7e4677fb"},"children":[]},"class":"reactR_markup"},"evals":[],"jsHooks":[]}</script>
```

```r
detainer_all %>%
  filter((citizenship_country %in% country_regions_base$name) == FALSE) %>% # which values of citizenship_country do not exist anywhere in the name column of country_regions_base df?
  distinct(citizenship_country) %>%
  arrange(citizenship_country) %>%
  mutate(color = case_when(TRUE == FALSE ~ "#a1d99b")) -> # don't want to color
  cit_missing_1

country_regions_base %>%
  filter((name %in% detainer_all$citizenship_country) == FALSE) %>% # which values of name do not exist anywhere in the citizenship_country column of detainer_all df?
  distinct(name) %>%
  arrange(name) %>%
  mutate(color = case_when(TRUE == FALSE ~ "#a1d99b")) -> # don't want to color
  cit_missing_2

"Values of citizenship_country that don't exist in names column of country_regions_base_df"
```

```
## [1] "Values of citizenship_country that don't exist in names column of country_regions_base_df"
```

```r
reactable_func(cit_missing_1, width = 300) # reactable of df to show user
```

```{=html}
<div id="htmlwidget-56778e5b5576ed826e8f" class="reactable html-widget" style="width:300px;height:auto;"></div>
<script type="application/json" data-for="htmlwidget-56778e5b5576ed826e8f">{"x":{"tag":{"name":"Reactable","attribs":{"data":{"citizenship_country":["","Antigua-Barbuda","Arabian Peninsula","Bolivia","Bosnia-Herzegovina","British Virgin Islands","Brunei","Burma","Cape Verde","China, Peoples Republic Of","Cocos Islands","Czech Republic","Czechoslovakia","Dem Rep Of The Congo","East Timor","Falkland Islands","Iran","Ivory Coast","Korea","Kosovo","Laos","Macau","Macedonia","Mariana Islands, Northern","Micronesia, Federated States Of","Moldova","Netherlands Antilles","Non-Quota Immigrant","North Korea","Reunion","Russia","Sao Tome And Principe","Serbia And Montenegro","South Korea","St. Kitts-Nevis","St. Lucia","St. Vincent-Grenadines","Stateless","Swaziland","Syria","Taiwan","Tanzania","Trinidad And Tobago","Turks And Caicos Islands","United Kingdom","United States","Unknown","Ussr","Venezuela","Vietnam","Virgin Islands, U.s.","Wallis And Futuna Islands","Yugoslavia"],"color":[null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null]},"columns":[{"accessor":"citizenship_country","name":"citizenship_country","type":"character","align":"center","style":[{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"}]},{"accessor":"color","name":"color","type":"character","align":"center","style":[{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"}],"show":false}],"defaultPageSize":10,"paginationType":"numbers","showPageInfo":true,"minRows":1,"outlined":true,"striped":true,"inline":true,"width":"300px","theme":{"color":"#141415","backgroundColor":"#f8f9fa","borderColor":"#f8f9fa","borderWidth":"1px","stripedColor":"white","highlightColor":"white","cellPadding":6,"tableStyle":{"fontFamily":"Verdana","fontSize":14},"headerStyle":{"borderWidth":"2px","backgroundColor":"#373a3c","color":"#ffffff","transitionDuration":"0.5s","&:hover[aria-sort]":{"color":"#ffffff"},"&[aria-sort='ascending'], &[aria-sort='descending']":{"color":"#ffffff"},"fontSize":15,"fontFamily":"Verdana"},"groupHeaderStyle":{"&:not(:empty)":{"color":"#141415","fontSize":15,"fontFamily":"Verdana"},"&:hover":{"fontWeight":"bold","transitionDuration":"1s","transitionTimingFunction":"ease-out","color":"#111111"}},"rowSelectedStyle":{"backgroundColor":"#78c2ad","color":"#ffffff"},"inputStyle":{"backgroundColor":"#ffffff","color":"#9a9a9a"},"searchInputStyle":{"backgroundColor":"#ffffff","color":"#9a9a9a","borderColor":"#b3cecc","&:focus":{"color":"#888888"}},"selectStyle":{"backgroundColor":"#2780e3","color":"#ffffff","borderColor":"#ffffff","outlineColor":"#ffffff"},"pageButtonStyle":{"backgroundColor":"#ffffff","color":"#2780e3","&:hover":{"backgroundColor":"#2780e3","color":"#ffffff"}},"pageButtonHoverStyle":{"backgroundColor":"#2780e3","color":"#ffffff"},"pageButtonActiveStyle":{"backgroundColor":"#2780e3","color":"#ffffff"},"pageButtonCurrentStyle":{"backgroundColor":"#2780e3","color":"#ffffff"}},"dataKey":"137a812fe25413034a961e309a67673e","key":"137a812fe25413034a961e309a67673e"},"children":[]},"class":"reactR_markup"},"evals":[],"jsHooks":[]}</script>
```

```r
"Values of name that don't exist in citizenship_country column of detainer_all"
```

```
## [1] "Values of name that don't exist in citizenship_country column of detainer_all"
```

```r
reactable_func(cit_missing_2, width = 300) # reactable of df to show user
```

```{=html}
<div id="htmlwidget-ff3d672d8ca2c3f8d74c" class="reactable html-widget" style="width:300px;height:auto;"></div>
<script type="application/json" data-for="htmlwidget-ff3d672d8ca2c3f8d74c">{"x":{"tag":{"name":"Reactable","attribs":{"data":{"name":["Åland Islands","Antarctica","Antigua and Barbuda","Bolivia (Plurinational State of)","Bonaire, Sint Eustatius and Saba","Bosnia and Herzegovina","Bouvet Island","British Indian Ocean Territory","Brunei Darussalam","Cabo Verde","China","Christmas Island","Cocos (Keeling) Islands","Congo, Democratic Republic of the","Côte d'Ivoire","Curaçao","Czechia","Falkland Islands (Malvinas)","Faroe Islands","French Southern Territories","Greenland","Guernsey","Heard Island and McDonald Islands","Holy See","Iran (Islamic Republic of)","Isle of Man","Jersey","Korea (Democratic People's Republic of)","Korea, Republic of","Lao People's Democratic Republic","Liechtenstein","Macao","Mayotte","Micronesia (Federated States of)","Moldova, Republic of","Myanmar","New Caledonia","Niue","Norfolk Island","Northern Mariana Islands","Palestine, State of","Pitcairn","Réunion","Russian Federation","Saint Barthélemy","Saint Helena, Ascension and Tristan da Cunha","Saint Kitts and Nevis","Saint Lucia","Saint Martin (French part)","Saint Pierre and Miquelon","Saint Vincent and the Grenadines","Sao Tome and Principe","Sint Maarten (Dutch part)","South Georgia and the South Sandwich Islands","Svalbard and Jan Mayen","Syrian Arab Republic","Taiwan, Province of China","Tanzania, United Republic of","Timor-Leste","Tokelau","Trinidad and Tobago","Turks and Caicos Islands","United Kingdom of Great Britain and Northern Ireland","United States Minor Outlying Islands","United States of America","Venezuela (Bolivarian Republic of)","Viet Nam","Virgin Islands (British)","Virgin Islands (U.S.)","Wallis and Futuna"],"color":[null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null]},"columns":[{"accessor":"name","name":"name","type":"character","align":"center","style":[{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"}]},{"accessor":"color","name":"color","type":"character","align":"center","style":[{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"}],"show":false}],"defaultPageSize":10,"paginationType":"numbers","showPageInfo":true,"minRows":1,"outlined":true,"striped":true,"inline":true,"width":"300px","theme":{"color":"#141415","backgroundColor":"#f8f9fa","borderColor":"#f8f9fa","borderWidth":"1px","stripedColor":"white","highlightColor":"white","cellPadding":6,"tableStyle":{"fontFamily":"Verdana","fontSize":14},"headerStyle":{"borderWidth":"2px","backgroundColor":"#373a3c","color":"#ffffff","transitionDuration":"0.5s","&:hover[aria-sort]":{"color":"#ffffff"},"&[aria-sort='ascending'], &[aria-sort='descending']":{"color":"#ffffff"},"fontSize":15,"fontFamily":"Verdana"},"groupHeaderStyle":{"&:not(:empty)":{"color":"#141415","fontSize":15,"fontFamily":"Verdana"},"&:hover":{"fontWeight":"bold","transitionDuration":"1s","transitionTimingFunction":"ease-out","color":"#111111"}},"rowSelectedStyle":{"backgroundColor":"#78c2ad","color":"#ffffff"},"inputStyle":{"backgroundColor":"#ffffff","color":"#9a9a9a"},"searchInputStyle":{"backgroundColor":"#ffffff","color":"#9a9a9a","borderColor":"#b3cecc","&:focus":{"color":"#888888"}},"selectStyle":{"backgroundColor":"#2780e3","color":"#ffffff","borderColor":"#ffffff","outlineColor":"#ffffff"},"pageButtonStyle":{"backgroundColor":"#ffffff","color":"#2780e3","&:hover":{"backgroundColor":"#2780e3","color":"#ffffff"}},"pageButtonHoverStyle":{"backgroundColor":"#2780e3","color":"#ffffff"},"pageButtonActiveStyle":{"backgroundColor":"#2780e3","color":"#ffffff"},"pageButtonCurrentStyle":{"backgroundColor":"#2780e3","color":"#ffffff"}},"dataKey":"6c89ca5e3f74482e6e0509f9cda0d0c1","key":"6c89ca5e3f74482e6e0509f9cda0d0c1"},"children":[]},"class":"reactR_markup"},"evals":[],"jsHooks":[]}</script>
```



### Fixing country_regions to make it joinable

```r
country_regions_base %>%
  
  # changing the names to match the coutry region dataset when there are discrepancies (however, only doing for non-other countries to save time)
  mutate(name = case_when(name == "Antigua and Barbuda" ~ "Antigua-Barbuda",
                          name == "Bolivia (Plurinational State of)" ~ "Bolivia",
                          name == "Virgin Islands (British)" ~ "British Virgin Islands",
                          name == "Virgin Islands (U.S.)" ~ "Virgin Islands, U.s.",
                          name == "Cocos (Keeling) Islands" ~ "Cocos Islands",
                          name == "Falkland Islands (Malvinas)" ~ "Falkland Islands",
                          name == "Saint Kitts and Nevis" ~ "St. Kitts-Nevis",
                          name == "Saint Lucia" ~ "St. Lucia",
                          name == "Saint Vincent and the Grenadines" ~ "St. Vincent-Grenadines",
                          name == "Trinidad and Tobago" ~ "Trinidad And Tobago",
                          name == "Turks and Caicos Islands" ~ "Turks And Caicos Islands",
                          name == "Venezuela (Bolivarian Republic of)" ~ "Venezuela",
                          name == "Northern Mariana Islands" ~ "Mariana Islands, Northern",
                          name == name ~ name)) %>% # for all others remaining that haven't yielded something in the case_when so far, keep them as is
  
  mutate(region_group = case_when(name == "Mexico" ~ "Mexico", # Mexico 1st, most specific, falls under another region (Central America in intermediate-region and Latin America and the Caribbean in sub-region)
                                  `intermediate-region` == "Caribbean" ~ "Caribbean",
                                  `intermediate-region` == "South America" ~ "South America",
                                  `intermediate-region` == "Central America" ~ "Central America",
                                  name == name ~ "Other" # for all others remaining that haven't yielded something in the case_when so far, make them other
  )) %>%
  select(name, region_group) ->
  country_regions
```



## Cleaning (transformations)

```r
detainer_all %>%
  
  # state
  rename(state_name = state) %>% # rename the state variable to be state_name so it's the same variable name as it is in the arrests1805 dataset
  
  mutate(state_fips = fips(state = state_name)) %>% # get the fips code for each state using fips()
  
  mutate(state_fips = as.numeric(state_fips)) %>% # change the fips code from a character to a numeric
  
  # adding missing fips codes where possible
  mutate(state_fips = case_when(state_name == "AS" ~ 60, # if the state is "AS" (American Samoa), make the value in state_fips 60 (fips code = 60) (before this, it was NA)
                                state_name == "GU" ~ 66, # if the state is "GU", make the value in state_fips 66
                                state_name == "MP" ~ 69,
                                state_name == "PR" ~ 72,
                                state_name == "VI" ~ 78,
                                is.na(state_fips) == FALSE ~ state_fips # if the value in state_fips is not NA, then keep it as whatever it currently is
                               )) %>%
  
  # county
  rename(county_name = county) %>% # rename county to instead be called county_name, so it has the same variable name as in the arrests1805 dataframe

  # census joining: the left_join, rename, and select code are the exact same as we used for TRAC_arrests_09_23_21
  left_join(., census_county_fips_codes, by = c("county_name" = "Area Name (including legal/statistical area description)", "state_fips" = "State Code (FIPS)")) %>%
  
   # join the above df (the detainer_all df with the all the mutations we've made to it) (".") to the df that has the county FIPS codes (census_county_fips_codes)
  # The join keys / the variables you are joining them by = are called:
  # A) "county_name" in the detainer_all df and is called "Area Name (including legal/statistical area description)" in the census_county_fips_codes df. Aka, this is the variable that has the same value in both
  # B) "state_fips" in the detainer_all df and "State Code (FIPS)" in the census_county_fips_codes df
  # "county_name" goes before the = since it's the variable in the existing df, whereas "Area Name (..." goes after the = since it's the variable in the df that you are joining into the arrests df.
  # This join adds a bunch of variables, but the only one we're interested in is "`County Code (FIPS)`"
  
  rename(county_fips = `County Code (FIPS)`) %>% # rename the `County Code (FIPS)` variable to be county_fips instead. Have to use `` (not ', but `) around the County Code variable name since there are spaces in it. Don't need to do as.numeric like did with states, since the variable type is already a numeric
  
  select(-contains("Code"), -"Summary Level") %>% # get rid of the "Summary Level" variable and any variables that have "Code" in their name, as those were all the variables that got added when we used left_join, which were either unneccessary or have already served their purpose and we're now done with (like County Code (FIPS), which we used to create the county variable but that we are now done with)
  
  # date
  mutate(year = str_sub(prepare_date, start = -2, end = -1)) %>% # get the 2nd and last characters in prepare_date, which is always last two digits of the year
  
  mutate(year = str_c("20", year)) %>% # we know that the first 2 digits of the year are always 20, all the data is from this century. combine "20" and the last 2 digits of the year (which are in year_yy variable) into one date, e.g. "20" and "12" combine to be "2012" using str_c()
  
  mutate(year = as.numeric(year)) %>% # change the year variable from a character to a numeric
  
  mutate(month = str_extract(prepare_date, "[A-Z]+")) %>% # extract (i.e., get) capital letters from prepare_date. [A-Z]+ says get capital letter, the + says get all the [A-Z] (all the capital letters) from the 1st time one shows up until there is no longer a capital letter in the string. aka get the first instance of capital letters in a row
  
  mutate(month = str_to_title(month)) %>% # e.g. change "JAN" to "Jan".
  
  mutate(month = match(month, month.abb)) %>% # match.abb already exists in R. what this line does is get the number associated with each month. Don't 100% understand why
  
  # sex / gender. code for this copy pasted from the arrests cleaning code
  mutate(male = factor(x = gender,
                       levels = c("Female", "Male", "Unknown"))) %>% # make a variable called male. this variable is a factor (so what Stata can read as something with a label and a nolabel). Want the first level, male = 1, to be "Female", and the 2nd level, male = 2, to be "Male", and 3rd level, male = 3, to be "Unknown". Ideally, it would be "Female" as 0 and "Male" as 1, and "Unknown" as 99 but there is no way to do this in R that translates correctly to Stata (there are many ways to do it in R, but none that translate correctly to Stata).
  # if you ever do want to see the label for a factor in R, you can use as.numeric() on the variable to see them
  
  # birth_country
  left_join(., country_regions, by = c("birth_country" = "name")) %>% # bring in the region
  
  rename(birth_country_region = region_group) %>% # rename it so it's a different variable name when later joining for citizenship
  
  mutate(birth_country_region = case_when(is.na(birth_country_region) == TRUE ~ "Other", # if it's na, make it "Other"
                                          is.na(birth_country_region) == FALSE ~ birth_country_region)) %>% # if it's not na, keep what is there
  
  mutate(birth_country_region = factor(x = birth_country_region, # making it a factor
                                       levels = c("Mexico", # 1
                                                  "Central America", # 2
                                                  "Caribbean", # 3
                                                  "South America", # 4
                                                  "Other"))) %>% # 5
  
  # citizenship_country
  left_join(., country_regions, by = c("citizenship_country" = "name")) %>% # bring in the region
  
  rename(citizenship_region = region_group) %>% # rename it so it's a different variable name than that for birth_country (distinguish between the two)
  
  mutate(citizenship_region = case_when(is.na(citizenship_region) == TRUE ~ "Other", # if it's na, make it "Other"
                                        is.na(citizenship_region) == FALSE ~ citizenship_region)) %>% # if it's not na, keep what is there
  
  mutate(citizenship_region = factor(x = citizenship_region, # making it a factor
                                     levels = c("Mexico", # 1
                                                "Central America", # 2
                                                "Caribbean", # 3
                                                "South America", # 4
                                                "Other"))) %>% # 5
  
  select(state_name, state_fips, county_name, county_fips, prepare_date, month, year, male, citizenship_region, birth_country_region, everything()) -> # putting the varaibles in the order listed in the email. everything() at the end of the list keeps all the other variables we haven't listed (such as age), and puts them after the ones we have listed out
  
  detainer_clean # new df name with cleaned data
```


```r
head(detainer_clean) %>% # getting first few rows to show user
  mutate(color = case_when(TRUE == FALSE ~ "#a1d99b")) -> # don't want to color
  detainer_clean_head

reactable_func(detainer_clean_head, width = 5000) # reactable of df to show user
```

```{=html}
<div id="htmlwidget-c1c2b85a9c038b112e6e" class="reactable html-widget" style="width:5000px;height:auto;"></div>
<script type="application/json" data-for="htmlwidget-c1c2b85a9c038b112e6e">{"x":{"tag":{"name":"Reactable","attribs":{"data":{"state_name":["CA","CA","CO","ID","CO","SC"],"state_fips":[6,6,8,16,8,45],"county_name":["Lassen County","Kern County","Garfield County","Ada County","Garfield County","Hampton County"],"county_fips":[35,29,45,1,45,49],"prepare_date":["12MAR08","11OCT07","04OCT12","04MAR08","04JAN13","28MAR08"],"month":[3,10,10,3,1,3],"year":[2008,2007,2012,2008,2013,2008],"male":["Male","Male","Male","Male","Male","Male"],"citizenship_region":["Mexico","Mexico","Other","Mexico","Mexico","Mexico"],"birth_country_region":["Mexico","Mexico","Other","Mexico","Mexico","Mexico"],"source":["A","A","B","A","B","A"],"unique_id":[1,1000,1000,1001,1001,1002],"detainer_type":["","","","","",""],"detainer_lift_reason":["Case Closed","Lifted","","Early Release","","Booked into Detention"],"gender":["Male","Male","Male","Male","Male","Male"],"citizenship_country":["Mexico","Mexico","Afghanistan","Mexico","Mexico","Mexico"],"age_at_detainer":[33,31,"NA",31,"NA",43],"detainer_detention_facility":["F.C.I. HERLONG","TAFT FED.CORR.INST.","1ST CHOICE INNS","IDAHO STATE PRISON, BOISE","1ST CHOICE INNS","ESTILL FED.CORR.INST."],"city":["HERLONG","TAFT","GLENWOOD SPRINGS","BOISE","GLENWOOD SPRINGS","ESTILL"],"detainer_facility_type":["Federal Facility","Federal Facility","Other Facility Type","State Facility","Other Facility Type","Federal Facility"],"mscc_grp":["3","1","1","1","2","1"],"crime_grp":["YY","YY","YY","YY","YY","YY"],"birth_country":["Mexico","Mexico","Afghanistan","Mexico","Mexico","Mexico"],"projected_release_date":["01JUN08","","","","","18JUN08"],"init_book_in_date":["","","12/16/1997","3/18/2004","4/1/1997",""],"departed_date":["6/11/2008 0:00","11/16/2007 0:00","","2/18/2010 0:00","","7/8/2008 0:00"],"color":[null,null,null,null,null,null]},"columns":[{"accessor":"state_name","name":"state_name","type":"character","align":"center","style":[{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"}]},{"accessor":"state_fips","name":"state_fips","type":"numeric","align":"center","style":[{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"}]},{"accessor":"county_name","name":"county_name","type":"character","align":"center","style":[{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"}]},{"accessor":"county_fips","name":"county_fips","type":"numeric","align":"center","style":[{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"}]},{"accessor":"prepare_date","name":"prepare_date","type":"character","align":"center","style":[{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"}]},{"accessor":"month","name":"month","type":"numeric","align":"center","style":[{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"}]},{"accessor":"year","name":"year","type":"numeric","align":"center","style":[{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"}]},{"accessor":"male","name":"male","type":"factor","align":"center","style":[{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"}]},{"accessor":"citizenship_region","name":"citizenship_region","type":"factor","align":"center","style":[{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"}]},{"accessor":"birth_country_region","name":"birth_country_region","type":"factor","align":"center","style":[{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"}]},{"accessor":"source","name":"source","type":"character","align":"center","style":[{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"}]},{"accessor":"unique_id","name":"unique_id","type":"numeric","align":"center","style":[{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"}]},{"accessor":"detainer_type","name":"detainer_type","type":"character","align":"center","style":[{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"}]},{"accessor":"detainer_lift_reason","name":"detainer_lift_reason","type":"character","align":"center","style":[{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"}]},{"accessor":"gender","name":"gender","type":"character","align":"center","style":[{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"}]},{"accessor":"citizenship_country","name":"citizenship_country","type":"character","align":"center","style":[{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"}]},{"accessor":"age_at_detainer","name":"age_at_detainer","type":"numeric","align":"center","style":[{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"}]},{"accessor":"detainer_detention_facility","name":"detainer_detention_facility","type":"character","align":"center","style":[{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"}]},{"accessor":"city","name":"city","type":"character","align":"center","style":[{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"}]},{"accessor":"detainer_facility_type","name":"detainer_facility_type","type":"character","align":"center","style":[{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"}]},{"accessor":"mscc_grp","name":"mscc_grp","type":"character","align":"center","style":[{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"}]},{"accessor":"crime_grp","name":"crime_grp","type":"character","align":"center","style":[{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"}]},{"accessor":"birth_country","name":"birth_country","type":"character","align":"center","style":[{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"}]},{"accessor":"projected_release_date","name":"projected_release_date","type":"character","align":"center","style":[{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"}]},{"accessor":"init_book_in_date","name":"init_book_in_date","type":"character","align":"center","style":[{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"}]},{"accessor":"departed_date","name":"departed_date","type":"character","align":"center","style":[{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"}]},{"accessor":"color","name":"color","type":"character","align":"center","style":[{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"},{"background":"#FFFFFF00","color":"black","fontWeight":"normal"}],"show":false}],"defaultPageSize":10,"paginationType":"numbers","showPageInfo":true,"minRows":1,"outlined":true,"striped":true,"inline":true,"width":"5000px","theme":{"color":"#141415","backgroundColor":"#f8f9fa","borderColor":"#f8f9fa","borderWidth":"1px","stripedColor":"white","highlightColor":"white","cellPadding":6,"tableStyle":{"fontFamily":"Verdana","fontSize":14},"headerStyle":{"borderWidth":"2px","backgroundColor":"#373a3c","color":"#ffffff","transitionDuration":"0.5s","&:hover[aria-sort]":{"color":"#ffffff"},"&[aria-sort='ascending'], &[aria-sort='descending']":{"color":"#ffffff"},"fontSize":15,"fontFamily":"Verdana"},"groupHeaderStyle":{"&:not(:empty)":{"color":"#141415","fontSize":15,"fontFamily":"Verdana"},"&:hover":{"fontWeight":"bold","transitionDuration":"1s","transitionTimingFunction":"ease-out","color":"#111111"}},"rowSelectedStyle":{"backgroundColor":"#78c2ad","color":"#ffffff"},"inputStyle":{"backgroundColor":"#ffffff","color":"#9a9a9a"},"searchInputStyle":{"backgroundColor":"#ffffff","color":"#9a9a9a","borderColor":"#b3cecc","&:focus":{"color":"#888888"}},"selectStyle":{"backgroundColor":"#2780e3","color":"#ffffff","borderColor":"#ffffff","outlineColor":"#ffffff"},"pageButtonStyle":{"backgroundColor":"#ffffff","color":"#2780e3","&:hover":{"backgroundColor":"#2780e3","color":"#ffffff"}},"pageButtonHoverStyle":{"backgroundColor":"#2780e3","color":"#ffffff"},"pageButtonActiveStyle":{"backgroundColor":"#2780e3","color":"#ffffff"},"pageButtonCurrentStyle":{"backgroundColor":"#2780e3","color":"#ffffff"}},"dataKey":"e900bff73a620941605b5224a190e988","key":"e900bff73a620941605b5224a190e988"},"children":[]},"class":"reactR_markup"},"evals":[],"jsHooks":[]}</script>
```

# Writing the clean dataset to .dta (STATA) and .csv

```r
write_dta(data = detainer_clean, # dataframe using
          path = "./detainer_clean.dta") # file writing to

write_csv(x = detainer_clean, # dataframe using
          file = "./detainer_clean.csv") # file writing to
```



# Reporting crosstabs & missing data
## Crosstabs: Pre-work to set up the for-loop that will create the crosstabs

```r
detainer_clean %>%
  select(state_name, month, year, male, citizenship_region, birth_country_region) -> # only get variables mentioned in email (don't need the _fips or counties)
  detainer_clean_for_crosstabs

detainer_var_names <- names(detainer_clean_for_crosstabs) # vector that has all of the variable names in the detainer_clean_for_crosstabs dataframe in order

detainer_clean_crosstabs_list <- list() # create an empty list (will use/populate/fill the list in the for-loop).
```

## Crosstabs: For-loop

```r
for(i in seq_along(detainer_var_names)) { # for each value in a sequence from 1 to n, where n is the number of variables in the dataframe:
  
  detainer_clean_for_crosstabs %>% # take the cleaned dataset (with the variables we want to do crosstabs for)
    
    tabyl(detainer_var_names[i]) %>% # tabyl creates a crosstab. This line of code creates a crosstab of the ith variable in the dataframe (so if the 1st variable in the dataframe (and therefore the first item in the detainer_var_names vector) is "state", the first time through this for-loop, i=1, so it will do tabyl(detainer_var_names[1]), which is tabyl(state). The 2nd time through the for-loop, it will pull the 2nd variable in the dataframe, so tabyl(detainer_var_names[2], which might be county))
    
    tibble() %>% # tibble() turns the crosstab into a manipulable dataframe
    
    mutate(percent = scales::percent(percent, accuracy = 0.1)) %>% # change to be written as a %
    
    mutate(color = case_when(TRUE == FALSE ~ "#a1d99b")) -> # don't want to color
    
    detainer_clean_crosstabs_list[[i]] # send each of the crosstabs to a list, so the ith object of the list is the crosstab for the ith variable in the detainer_clean_for_crosstabs dataframe. ex: if the 1st varaible in the detainer_clean_for_crosstabs dataframe is "state", then detainer_clean_crosstabs_list[[1]] will be the crosstab for state
  
}
```



## Crosstabs: list of all crosstabs (i.e. the result of the above for-loop)

```r
for (i in seq_along(detainer_clean_crosstabs_list)) { # for each of the dfs
  
}

# percent is the percent including NAs, valid_percent is the percent not including NAs.

# copying and pasting to a google sheets file: https://docs.google.com/spreadsheets/d/1HVVKwMDpvqqbEuQw-rx-8GzEYlMknkrw6a5SBNx_TSU/edit?usp=sharing.
```

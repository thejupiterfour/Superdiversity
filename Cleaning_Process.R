library(refsplitr)
library(tidyverse)
library(rorcid)
library(usethis)
library(anytime)
library(janitor)
library(httpuv)
library(DataEditR)
library(stringr)

rm(list = ls())
#Data 2015 ----------------
#import data from 2015 located in "./data/2015"
setwd("~/Goek_Analyses/Superdiversity")
#do this step only if first time
refs_2015 <- references_read(data = "./data/2015", 
                             dir=TRUE,
                             include_all = TRUE)

#save as .csv in output folder
write.csv(refs_2015,"./output/refs_2015.csv")

#split and match author and addresses, two lists: prelim and review 
clean_refs_2015 <- authors_clean(refs_2015)

#save as .csv in output folder
write.csv(clean_refs_2015$prelim,"./output/clean_refs_2015_prelim.csv")
write.csv(clean_refs_2015$review,"./output/clean_refs_2015review.csv")

#if previous steps have been done before, import saved files only
refs_2015 <- read_csv("output copy/refs_2015.csv")
clean_refs_2015_prelim <- read_csv("output copy/clean_refs_2015_prelim.csv")
clean_refs_2015_review <- read_csv("output copy/clean_refs_2015review.csv")

#save as a tibble
prelim_2015 <- as_tibble(clean_refs_2015_prelim)
review_2015 <- as_tibble(clean_refs_2015_review)

#checking matches and creates lists to review matches more easily
df_review_2015 <- df_for_review_check(review_2015)
check_pools_list <- check_pools(review_2015) #checks if diff authors were pooled in the same groupID
check_splits_list <- check_splits(review_2015) #checks if same authors were split among diff groupID

review_2015 <- read_csv("OneDrive_1_16-11-2022/output/20221118-data.csv")

#replace mismatches by changing "Author Name" and "AuthorID", respectively 
review_2015$groupID[review_2015$AF == "Bradley, J. Andrew" ] <- "15077"

#second option for fixing mismatches
data_edit(review_2015)

#save manually reviewed file 
write.csv(review_2015,"./output copy/m_review.csv")

save(refs_2015, file = "refs_2015.Rdata")

#accept results of authors_clean, ideally after checking review file
m_review_2015 <- read_csv("OneDrive_1_16-11-2022/output/m_review.csv")
m_review_2015 <- m_review |> select(-...1)

refined_2015 <- authors_refine(m_review_2015, prelim_2015)

#save file
write.csv(refined_2015, "./refined_2015.csv")

#read record's file, fread is faster than read_csv
refs_2015 <- data.table::fread("refs_2015 1.csv")

#load R file
load("refs_2015.Rdata")


#recover first names------
#orcid authentication
orcid_auth()

#save your orcid_token here by ORCID_TOKEN = "your-token"
usethis::edit_r_environ()

#find authors orcidID based on DOI of records
DOI_2015 <- refs_2015|>
  filter(DI != "NA")|>
  select(DI, refID)  

DOI_2015_find <- DOI_2015[["DI"]]

#search for orcids associated with DOIs
DOI_2015_auth <- orcid_doi(DOI_2015_find)

#save file as an Rdata file
save(DOI_2015_auth, file = "DOI_2015_auth.Rdata")

#find authors using orcid
#unnest list of information
DOI_2015_search <- plyr::ldply(DOI_2015_auth) 

DOI_2015_search <- DOI_2015_search|>
  unique()

#vector with orcid IDS found
DOI_2015_search_list <- DOI_2015_search$`orcid-identifier.path`

#find authors associated with these orcidsID
orcid_refs2015 <- orcid_person(DOI_2015_search_list) 

#save file
save(orcid_refs2015, file = "orcid_refs2015.Rdata")

#pluck info extracted into a df
orcid_data_2015 <- orcid_refs2015 %>% {
  dplyr::tibble(
    author_name_givenname = purrr::map_chr(., purrr::pluck, "name", "given-names", "value", .default=NA_character_),
    author_name_surname = purrr::map_chr(., purrr::pluck, "name", "family-name", "value", .default=NA_character_),
    OI = purrr::map_chr(., purrr::pluck, "name", "path", .default = NA_character_),
    other_names = purrr::map_chr(., pluck, "other-names", "other-name", "value", .default=NA_character_),
    
  )
} 

#standardize the file for later joining
orcid_data_2015 <- orcid_data_2015|>
  filter(author_name_givenname != "NA")|>
  mutate(author_name_surname = tolower(author_name_surname),
         author_name_givenname = tolower(author_name_givenname))

refined_2015 <- refined_2015|>
  mutate(author_name_surname = tolower(author_name_surname))

#remove possible missing names by matching these patterns
orcid_data_2015 <- orcid_data_2015 |>
  filter(author_name_givenname != "NA")|>
  mutate(Pat = str_replace_all(author_name_givenname, 
                               pattern = "^. [A-z].$", replacement = "NOFIRSTNAME"))|>
  subset(Pat!="NOFIRSTNAME")|>
  mutate(Pat = str_replace_all(author_name_givenname, 
                               pattern = "^[A-z]. $", replacement = "NOFIRSTNAME"))|>
  subset(Pat!="NOFIRSTNAME")|>
  unique()

#remove unwanted columns
orcid_data_2015 <- orcid_data_2015 |>
  select(-other_names, -Pat)

#save file
save(orcid_data_2015, file="orcid_data_2015.Rdata")

#join with refined df based on matching orcidIDs
refined_2015 <- refined_2015 |>
  separate(author_name, sep = ", ", into = c("author_name_surname", "author_name_givenname"), remove = FALSE) |>
  mutate(author_name_givenname_noabr=str_remove(author_name_givenname, pattern = "^[A-Za-z]+\\.$"),
         author_name_givenname_noabr=na_if(author_name_givenname_noabr, ""),
         has_given_name=case_when(is.na(author_name_givenname_noabr) ~"No",TRUE ~"Yes"),
         has_OI=case_when(is.na(OI) ~ "No",TRUE ~"Yes"))

#make all names lower case to standardize
refined_2015 <- refined_2015 |>
  mutate(author_name_surname = tolower(author_name_surname),
         author_name_givenname = tolower(author_name_givenname))

#change column name for joining
orcid_data_2015 <- orcid_data_2015 |>
  rename(author_first_name = author_name_givenname)

#join names found
refined_new <- left_join(x=refined_2015, y=orcid_data_2015)

#remove missing first names
refined_new <- refined_new|>
  filter(author_name_givenname != "NA")

#match all first names found
refined_new <- refined_new|>
  mutate(first_name = coalesce(author_first_name, author_name_givenname_noabr))

#weed out missing first names
refined_new <- refined_new |>
  mutate(first_name = str_replace_all(first_name, 
                                      pattern = "^[A-z]. .$", replacement = "NA"))|>
  subset(first_name != "NA")

#make all names lower case
refined_new <- refined_new |>
  select(-author_name_givenname, -author_name_givenname_noabr, -author_first_name, -has_OI, -has_given_name)|>
  mutate(author_name_surname = tolower(author_name_surname), first_name = tolower(first_name))

#change column order for ease of check
refined_new <- refined_new[, c(1, 2, 3, 4, 21, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19,
                               20)]

#rename column to merge to retrieve missing orcidIDs
orcid_data_2015<- orcid_data_2015 |>
  rename(first_name = author_first_name)

#join again to recover missing orcidIDS
refined_new <- left_join(x=refined_new, y = orcid_data_2015, by = c("first_name", "author_name_surname"))

#coalesce so remaining orcid IDs values are assigned
refined_new <- refined_new|>
  mutate(OI = coalesce(OI.y, OI.x))|>
  select(-OI.y, -OI.x)

#to be able to tally and keep track who has OI and/or first name
refined_new <- refined_new |>
  mutate(has_given_name=case_when(is.na(first_name) ~"No",TRUE ~"Yes"),
         has_OI=case_when(is.na(OI) ~ "No", TRUE ~ "Yes"))

refined_new <- refined_new|>
  filter(has_OI != "No")

refined_new$OI[refined_new$groupID == "65384"] <- "NA"

save(refined_new, file = "refined_new.Rdata")

#refine authors' countries ----
library(countrycode)

#separate authors and countries and unify UK
authors_country_2015new <- refined_new|>
  select(first_name, author_name_surname, country)|>
  unique()|>
  filter(country != "NA")

#rename countries with non standard names
authors_country_2015new <- authors_country_2015new |>
  mutate(country = case_when(
    country == "england" ~ "United Kingdom",
    country == "scotland" ~ "United Kingdom",
    country == "wales" ~ "United Kingdom",
    country == "north ireland" ~ "United Kingdom",
    country == "usa" ~ "United States of America",
    country == "papua n guinea" ~ "Papua and New Guinea",
    country == "cent afr republ" ~ "Central African Republic",
    country == "kosovo" ~ "Republic of Kosovo",
    country == "namibia" ~ "NB",
    TRUE ~ authors_country_2015new$country))

#get country name into iso2c codes, United Kingdom = UK
authors_country_2015new <- authors_country_2015new |>
  mutate(country = countrycode(authors_country_2015new$country, origin = "country.name", destination = "iso2c"))

#RK for Republic of Kosovo and NB for Namibia
authors_country_2015new <- authors_country_2015new |>
  mutate(country = case_when(
    country == "NA" ~ "RK",
    country == "NB" ~ "RB",
    TRUE ~ authors_country_2015new$country))
#remove empty country cells
authors_country_2015new <- authors_country_2015new |>
  filter(country != "NA")
#weed out remaining authors with no first name
authors_country_2015 <- authors_country_2015|>
  mutate(first_name = str_replace_all(first_name, 
                                      pattern = "^[A-z]. [A-z]-[A-z]$", replacement = "NA"))|>
  subset(first_name != "NA")
#save file as .csv
write.csv(authors_country_2015, "./Authors_Countries2015_30Dec.csv")

refined_2015|> 
  filter(has_OI == "No")

#find information on authors based on orcidID----------------
refined_new$OI[refined_new$OI == "Hosieni Tabatabaei"] <- "NA"

refined_new <- refined_new|>
  filter(OI != "NA")


#find information on works from authors
authors_work2 <- orcid_works(authors_orcid2)

authors_work1 <- orcid_works(authors_orcid1)


#join files
authors_work <- full_join(authors_work1, authors_work2)

#save as R file
save(authors_work2, file = "authors_work2.Rdata")

save(authors_work1, file = "authors_work1.Rdata")

#find information on education from authors
authors_edu1 <- orcid_educations(authors_orcid1)

authors_edu2 <- orcid_educations(authors_orcid2)

#join files
authors_edu <- full_join(authors_edu1, authors_edu2)

#Affiliation names for ROR-----
affiliation <- refined_new|>
  select(university)|>
  unique()

write_csv2(affiliation, file ="affiliation_names.csv")

#ROR matched file
affiliation_ROR <- read_csv("2023-01-09_search_results_affiliation_matching.csv")

affiliation_search <- affiliation_ROR|>
  select()

jsonROR <- fromJSON("v1.9-2022-10-11-ror-data.json")

save(jsonROR, file="jsonROR.Rdata")

ROR <- jsonlite::flatten(jsonROR)

ROR <- ROR|>
  select(id,  name, types, country.country_code, country.country_name, external_ids.ISNI.all,
         external_ids.FundRef.preferred, external_ids.FundRef.all, established, addresses)

ROR <- ROR|>
  unnest(addresses)

ROR <- ROR|>
  select(id, name, types, country_code, country_name, FundRef, ISNI)

ROR <- ROR|>
  unnest(FundRef, ISNI)|>
  unique()

#remove square brackets from full university names 
affiliation_ROR <- affiliation_ROR|>
  select(search_term, chosen_match_names)

affiliation_ROR$chosen_match_names[affiliation_ROR$chosen_match_names == "[]"] <- NA

affiliation_ROR <- affiliation_ROR|>
  rename(university = search_term)

affiliation_ROR$chosen_match_names[affiliation_ROR$university == "blaise pascal univ"] <- "Institut Pascal"


refined_new$university[refined_new$address == "Amgen Inc, deCODE Genet, Reykjavik, Iceland."] <- "amgen inc ice"

affiliation_ROR <- affiliation_ROR|>
  add_row(university = "amgen inc ice", chosen_match_names = "deCODE Genetics (Iceland)")

refined_new <- left_join(refined_new, affiliation_ROR)



#find pattern and remove it
refined_new <- refined_new|>
  mutate(name_fix =  gsub("\\['|\\']","", refined_new$chosen_match_names))

refined_new <- refined_new|>     
  mutate(name_fix = gsub("\\[|\\]", "", refined_new$chosen_match_names))

refined_new <- refined_new|>     
  mutate(name_fix = gsub('"', "", refined_new$name_fix))

refined_new <- ROR|>     
  mutate(name_fix = gsub("'", "", refined_new$name_fix))

test2<- ROR|>
  group_by(name_fix)|>
  mutate(tag = cur_group_id())

test<- test|>
  group_by(test)|>
  mutate(tag1 = cur_group_id())|>
  select(-tag)


test3 <- merge_plus(test,test2, unique_key_1 = "tag1", unique_key_2 = "tag", match_type = "fuzzy",
                    by.x="test", by.y="name_fix")

ROR <- ROR|>
  rename(name_fix = name)

refined_new <- refined_new|>
  select(-chosen_match_names)

refined_new <- left_join(refined_new, ROR, by="name_fix")

test <- refined_new|>
  select(university, name_fix, id)


#Web of Science categories----
#uunique identifier
refs_2015_1 <- refs_2015_1|>
  group_by(DI)|>
  mutate(uniqueID = cur_group_id())

#select references
CR <- refs_2015_1|>
  select(uniqueID, CR)
#select only CR
CR_unfolded <-CR$CR
#split the rows
CR_unfolded_df <- str_split(CR_unfolded, pattern = "\r\n")
#create tibble
CR_df <- as_tibble(unlist(CR_unfolded_df))
#separate into columns to get journal names
CR_references <- CR_df|>
  separate(value, sep = ", ", into=c("author", "year", "journal", "vol", "page", "doi"), remove = FALSE)

CR_references <- CR_references|>
  select(journal_fixed)|>
  unique()

CR_references <- CR_references|>
  mutate(journal =  gsub("\\[","", CR_references$journal))

CR_references <- CR_references|>
  mutate(journal_fixed = tolower(journal_fixed))

#files from web of science, 
wos1 <- read_csv("wos-core_SSCI 2023-January-20.csv")
wos2 <- read_csv("wos-core_SCIE 2023-January-20.csv")
wos3 <- read_csv("wos-core_ESCI 2023-January-20.csv")
wos4 <- read_csv("wos-core_AHCI 2023-January-20.csv")
wos_abb <- read_csv("wos-jcr 2022-June-28.csv")

#join all files
wos <- full_join(wos1, wos2)
wos <- full_join(wos, wos3)
wos <- full_join(wos, wos4)

#rename to join
wos <- wos|>
  rename(Title = `Journal title`)

#join both dataframes
wos_all <- left_join(wos, wos_abb)

wos_all <- wos_all|>
  mutate(Title20 = tolower(Title20))

CR_category <- CR_references|>
  rename(Title20 = journal_fixed)

CR_category <- left_join(CR_category, wos_all)

CR_category <- CR_category|>
  filter(Title20 != "NA")|>
  filter(Title != "NA")

save(wos_all, file="wos_all.Rdata")


#orcid ID retrieval from data dump-----
library(xml2)

o_ids <- refined_new|>
  select(OI)|>
  unique()

dump <- as_tibble(dump)

dump <- dump|>
  mutate(OI = str_remove(value, pattern = ".xml"))

OI_dump <- dump|>
  select(OI)

OI <- inner_join(o_ids, OI_dump)

#Data 2017 ----------
#import data from 2017 located in "./data/2015"
setwd("~/Library/CloudStorage/OneDrive-Personal/University of Strathclyde/Gok_data_analyses")
#do this step only if first time
refs_2017 <- references_read(data = "./2017", 
                             dir=TRUE,
                             include_all = TRUE)

#save as .csv in output folder
write.csv(refs_2017,"./refs_2017.csv")
#save as R.data for ease of sharing
save(refs_2017, file = "refs_2017.Rdata")

#split and match author and addresses, two lists: prelim and review 
clean_refs_2017 <- authors_clean(refs_2017)

#save as .Rdata in output folder
save(clean_refs_2017, file = "clean_refs_2017.Rdata")

#save as a tibble
prelim_2017 <- as_tibble(clean_refs_2017_prelim)
review_2017 <- as_tibble(clean_refs_2017_review)

#replace mismatches by changing "Author Name" and "AuthorID", respectively 
review_2017$groupID[review_2017$AF == "Bradley, J. Andrew" ] <- "15077"

#checking matches and creates lists to review matches more easily
df_review_2015 <- df_for_review_check(review_2015)
check_pools_list <- check_pools(review_2015) #checks if diff authors were pooled in the same groupID
check_splits_list <- check_splits(review_2015) #checks if same authors were split among diff groupID

#accept and refine results
refined_2017 <- authors_refine(review_2017, prelim_2017)

#find authors orcidID based on DOI of records
DOI_2017 <- refs_2017|>
  filter(DI != "NA")|>
  select(DI, refID)  

DOI_2017_find <- DOI_2017[["DI"]]

#search for orcids associated with DOIs
DOI_2017_auth <- orcid_doi(DOI_2017_find)

#save file as an Rdata file
save(DOI_2017_auth, file = "DOI_2017_auth.Rdata")

#find authors using orcid
#unnest list of information
DOI_2017_search <- plyr::ldply(DOI_2017_auth) 

DOI_2017_search <- DOI_2017_search|>
  unique()

#vector with orcid IDS found
DOI_2017_search_list <- DOI_2017_search$`orcid-identifier.path`

save(DOI_2017_search_list, file = "DOI_2017_search_list.Rdata")

#find authors associated with these orcidsID
orcid_refs2017 <- orcid_person(DOI_2017_search_list) 


#save file
save(orcid_refs2017, file = "orcid_refs2017.Rdata")



#pluck info extracted into a df
orcid_person_data_2017 <- orcids_ppl_2017 %>% {
  dplyr::tibble(
    author_name_givenname = purrr::map_chr(., purrr::pluck, "name", "given-names", "value", .default=NA_character_),
    author_name_surname = purrr::map_chr(., purrr::pluck, "name", "family-name", "value", .default=NA_character_),
    OI = purrr::map_chr(., purrr::pluck, "name", "path", .default = NA_character_),
    other_names = purrr::map_chr(., pluck, "other-names", "other-name", "value", .default=NA_character_),
    
  )
} 

#standardize the file for later joining
orcid_person_data_2017 <- orcid_person_data_2017|>
  filter(author_name_givenname != "NA")|>
  mutate(author_name_surname = tolower(author_name_surname),
         author_name_givenname = tolower(author_name_givenname))

refined_2017 <- refined_2017|>
  mutate(author_name_surname = tolower(author_name_surname))

#remove possible missing names by matching these patterns
orcid_person_data_2017 <- orcid_person_data_2017 |>
  filter(author_name_givenname != "NA")|>
  mutate(Pat = str_replace_all(author_name_givenname, 
                               pattern = "^[A-z]$", replacement = "NOFIRSTNAME"))|>

  subset(Pat!="NOFIRSTNAME")|>
  unique()

#remove unwanted columns
orcid_person_data_2017 <- orcid_person_data_2017 |>
  select(-other_names, -Pat)

#save file
save(orcid_person_data_2017, file = "orcid_person_data_2017.Rdata")

#join with refined df based on matching orcidIDs
refined_2017_new <- refined_2017 |>
  separate(author_name, sep = ", ", into = c("author_name_surname", "author_name_givenname"), remove = FALSE) |>
  mutate(author_name_givenname_noabr=str_remove(author_name_givenname, pattern = "^[A-Za-z]+\\.$"),
         author_name_givenname_noabr=na_if(author_name_givenname_noabr, ""),
         has_given_name=case_when(is.na(author_name_givenname_noabr) ~"No",TRUE ~"Yes"),
         has_OI=case_when(is.na(OI) ~ "No",TRUE ~"Yes"))

#make all names lower case to standardize
refined_2017_new <- refined_2017_new |>
  mutate(author_name_surname = tolower(author_name_surname),
         author_name_givenname = tolower(author_name_givenname))

#change column name for joining
orcid_person_data_2017 <- orcid_person_data_2017 |>
  rename(author_first_name = author_name_givenname)

refined_2017_new <- refined_2017_new |>
  rename(author_first_name = author_name_givenname)

#join names found
refined_2017_new <- left_join(x=refined_2017_new, y=orcid_person_data_2017)

#remove missing first names
refined_2017_new <- refined_2017_new|>
  filter(author_first_name != "NA")

#match all first names found
refined_2017_new <- refined_2017_new|>
  mutate(first_name = coalesce(author_first_name, author_name_givenname_noabr))

#weed out missing first names
refined_2017_new <- refined_2017_new |>
  mutate(first_name = str_replace_all(first_name, 
                                      pattern = "^'.$", replacement = "NA"))|>
  subset(first_name != "NA")

#make all names lower case
refined_2017_new <- refined_2017_new |>
  select(-author_first_name, -author_name_givenname_noabr, -has_OI, -has_given_name)|>
  mutate(author_name_surname = tolower(author_name_surname), first_name = tolower(first_name))

#change column order for ease of check
refined_2017_new <- refined_2017_new[, c(1, 2, 3, 4, 21, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19,
                               20)]


#rename column to merge to retrieve missing orcidIDs
orcid_person_data_2017 <- orcid_person_data_2017 |>
  rename(first_name = author_first_name)

#join again to recover missing orcidIDS
refined_2017_new <- left_join(x=refined_2017_new, y = orcid_person_data_2017, by = c("first_name", "author_name_surname"))

#coalesce so remaining orcid IDs values are assigned
refined_2017_new <- refined_2017_new|>
  mutate(OI = coalesce(OI.y, OI.x))|>
  select(-OI.y, -OI.x)

#to be able to tally and keep track who has OI and/or first name
refined_2017_new <- refined_2017_new |>
  mutate(has_given_name=case_when(is.na(first_name) ~"No",TRUE ~"Yes"),
         has_OI=case_when(is.na(OI) ~ "No", TRUE ~ "Yes"))

refined_2017_new <- refined_2017_new|>
  filter(OI != "NA")

refined_new_2017$OI[refined_new_2017$groupID == "65384"] <- "NA"

save(refined_2017_new, file = "refined_2017_new.Rdata")

#refine authors' countries ----
library(countrycode)

#separate authors and countries and unify UK
authors_country_2017new <- refined_2017_new|>
  select(first_name, author_name_surname, country)|>
  unique()|>
  filter(country != "NA")

#rename countries with non standard names
authors_country_2017new <- authors_country_2017new |>
  mutate(country = case_when(
    country == "england" ~ "United Kingdom",
    country == "scotland" ~ "United Kingdom",
    country == "wales" ~ "United Kingdom",
    country == "north ireland" ~ "United Kingdom",
    country == "usa" ~ "United States of America",
    country == "papua n guinea" ~ "Papua and New Guinea",
    country == "cent afr republ" ~ "Central African Republic",
    country == "kosovo" ~ "Repuplic of Kosovo",
    country == "namibia" ~ "Namibia",
    TRUE ~ authors_country_2017new$country))

#get country name into iso2c codes, United Kingdom = UK
authors_country_2017new <- authors_country_2017new |>
  mutate(country = countrycode(authors_country_2017new$country, 
                               origin = "country.name", destination = "iso2c"))

#RK for Republic of Kosovo and NB for Namibia
authors_country_2017new <- authors_country_2017new |>
  mutate(country = case_when(
    country == "NA" ~ "NB",
    country == "Kosovo" ~ "RK",
    TRUE ~ authors_country_2017new$country))
#remove empty country cells
authors_country_2017new <- authors_country_2017new |>
  filter(country != "NA")
#weed out remaining authors with no first name
authors_country_2017new <- authors_country_2017new|>
  mutate(first_name = str_replace_all(first_name, 
                                      pattern = "^[A-z]-[A-z]$", replacement = "NA"))|>
  subset(first_name != "NA")

authors_country_2017new <- authors_country_2017new|>
  unique()

#add year to identify the data
authors_country_2017new$Year <- "2017"

#save file as .csv
write.csv(authors_country_2017new, "./Authors_Countries2017.csv")

save(authors_country_2017new, file = "authors_country_2017.Rdata")

#Affiliation names for ROR, 2017 Data-----
affiliation_2017 <- refined_2017_new|>
  select(university)|>
  filter(university != "NA")|>
  unique()

affiliation_2018 <- affiliation_2018|>
  filter(university != "NA")

write_csv2(affiliation, file ="affiliation.csv")

affiliation <- full_join(affiliation, affiliation_2018)


#ROR matched file
affiliation_ROR <- read_csv("2023-01-09_search_results_affiliation_matching.csv")

affiliation_search <- affiliation_ROR|>
  select()

orcid_2017 <- refined_2017_new |>
  select(groupID, OI)|>
  unique()

jsonROR <- fromJSON("v1.9-2022-10-11-ror-data.json")

save(jsonROR, file="jsonROR.Rdata")

ROR <- jsonlite::flatten(jsonROR)

ROR <- ROR|>
  select(id,  name, types, country.country_code, country.country_name, external_ids.ISNI.all,
         external_ids.FundRef.preferred, external_ids.FundRef.all, established, addresses)

ROR <- ROR|>
  unnest(addresses)

ROR <- ROR|>
  select(id, name, types, country_code, country_name, FundRef, ISNI)

ROR <- ROR|>
  unnest(FundRef, ISNI)|>
  unique()

#remove square brackets from full university names 
affiliation_ROR <- affiliation_ROR|>
  select(search_term, chosen_match_names)

affiliation_ROR$chosen_match_names[affiliation_ROR$chosen_match_names == "[]"] <- NA

affiliation_ROR <- affiliation_ROR|>
  rename(university = search_term)

affiliation_ROR$chosen_match_names[affiliation_ROR$university == "blaise pascal univ"] <- "Institut Pascal"


refined_new$university[refined_new$address == "Amgen Inc, deCODE Genet, Reykjavik, Iceland."] <- "amgen inc ice"

affiliation_ROR <- affiliation_ROR|>
  add_row(university = "amgen inc ice", chosen_match_names = "deCODE Genetics (Iceland)")

refined_new <- left_join(refined_new, affiliation_ROR)



#find pattern and remove it
refined_new <- refined_new|>
  mutate(name_fix =  gsub("\\['|\\']","", refined_new$chosen_match_names))

refined_new <- refined_new|>     
  mutate(name_fix = gsub("\\[|\\]", "", refined_new$chosen_match_names))

refined_new <- refined_new|>     
  mutate(name_fix = gsub('"', "", refined_new$name_fix))

refined_new <- ROR|>     
  mutate(name_fix = gsub("'", "", refined_new$name_fix))

test2<- ROR|>
  group_by(name_fix)|>
  mutate(tag = cur_group_id())

test<- test|>
  group_by(test)|>
  mutate(tag1 = cur_group_id())|>
  select(-tag)


test3 <- merge_plus(test,test2, unique_key_1 = "tag1", unique_key_2 = "tag", match_type = "fuzzy",
                    by.x="test", by.y="name_fix")

ROR <- ROR|>
  rename(name_fix = name)

refined_new <- refined_new|>
  select(-chosen_match_names)

refined_new <- left_join(refined_new, ROR, by="name_fix")

test <- refined_new|>
  select(university, name_fix, id)


#ROR file
affiliation_2018_csv <- affiliation_2018_csv %>%
  filter(ror_id!= "NA")

#Web of Science categories 2016-2018----
#uunique identifier
refs_2017 <- refs_2017|>
  group_by(UT)|>
  mutate(uniqueID = cur_group_id())

#select references
CR_2017 <- refs_2017|>
  select(uniqueID, CR, UT)
#select only CR
CR_2017unfolded <-CR_2017$CR
#split the rows
CR_2017unfolded_df <- str_split(CR_2017unfolded, pattern = "\r\n")
#create tibble
CR_2017df <- as_tibble(unlist(CR_2017unfolded_df))
#separate into columns to get journal names
CR_2017references <- CR_2017df|>
  separate(value, sep = ", ", into=c("author", "year", "journal", "vol", "page", "doi"), remove = FALSE)

CR_2017references <- CR_2017references|>
  select(journal)

CR_2017references <- CR_2017references|>
  mutate(journal =  gsub("\\[","", CR_2017references$journal))

CR_2017references <- CR_2017references|>
  mutate(journal = tolower(journal))

save(CR_2017references, file = "CR_2017ref.Rdata")

#files from web of science, 
wos1 <- read_csv("wos-core_SSCI 2023-January-20.csv")
wos2 <- read_csv("wos-core_SCIE 2023-January-20.csv")
wos3 <- read_csv("wos-core_ESCI 2023-January-20.csv")
wos4 <- read_csv("wos-core_AHCI 2023-January-20.csv")
wos_abb <- read_csv("wos-jcr 2022-June-28.csv")

#join all files
wos <- full_join(wos1, wos2)
wos <- full_join(wos, wos3)
wos <- full_join(wos, wos4)

#rename to join
wos <- wos|>
  rename(Title = `Journal title`)

#join both dataframes
wos_all <- left_join(wos, wos_abb)

wos_all <- wos_all|>
  mutate(Title20 = tolower(Title20))

CR_2017category <- CR_2017references|>
  rename(Title20 = journal)

CR_2017category <- CR_2017category |>
  cbind(CR_2017category, CR_2017)

CR_2017category <- CR_2017category[, -2]

CR_2017category <- left_join(CR_2017category, wos_all)

CR_2017category <- CR_2017category|>
  filter(Title20 != "NA")|>
  filter(Title != "NA")


save(CR_2017category, file = "CR_2017wos.Rdata")

save(wos_all, file = "wos_all.Rdata")


#Data 2016 -----
#import data from 2016 located in "./2016"
setwd("~/Library/CloudStorage/OneDrive-Personal/University of Strathclyde/Gok_data_analyses")
#do this step only if first time
refs_2016 <- references_read(data = "./2016", 
                             dir=TRUE,
                             include_all = TRUE)

#save as .csv in output folder
write.csv(refs_2016,"./refs_2016.csv")
#save as R.data for ease of sharing
save(refs_2016, file = "refs_2016.Rdata")

#split and match author and addresses, two lists: prelim and review 
clean_refs_2016 <- authors_clean(refs_2016)

#save as .Rdata in output folder
save(clean_refs_2016, file = "clean_refs_2016.Rdata")

#save as a tibble
prelim_2016 <- as_tibble(clean_refs_2016$prelim)
review_2016 <- as_tibble(clean_refs_2016$review)

#replace mismatches by changing "Author Name" and "AuthorID", respectively 
review_2016$groupID[review_2016$AF == "McCarthy, Helen S." ] <- "9556"

#accept and refine results
refined_2016 <- authors_refine(review_2016, prelim_2016)

#find authors orcidID based on DOI of records
DOI_2016 <- refs_2016|>
  filter(DI != "NA")|>
  select(DI, refID)  

DOI_2016_find <- DOI_2016[["DI"]]

#search for orcids associated with DOIs
DOI_2016_auth <- orcid_doi(DOI_2016_find)


#save file as an Rdata file 
save(DOI_2016_auth, file = "DOI_2016_auth.Rdata")

#find authors using orcid
#unnest list of information
DOI_2016_search <- plyr::ldply(DOI_2016_auth) 

DOI_2016_search <- DOI_2016_search|>
  unique()

#vector with orcid IDS found
DOI_2016_search_list <- DOI_2016_search$`orcid-identifier.path`

save(DOI_2016_search_list, file = "DOI_2016_search_list.Rdata")


#pluck info extracted into a df
orcid_person_data_2016 <- orcids_ppl_2016 %>% {
  dplyr::tibble(
    author_name_givenname = purrr::map_chr(., purrr::pluck, "name", "given-names", "value", .default=NA_character_),
    author_name_surname = purrr::map_chr(., purrr::pluck, "name", "family-name", "value", .default=NA_character_),
    OI = purrr::map_chr(., purrr::pluck, "name", "path", .default = NA_character_),
    other_names = purrr::map_chr(., pluck, "other-names", "other-name", "value", .default=NA_character_),
    
  )
} 

#standardize the file for later joining
orcid_person_data_2016 <- orcid_person_data_2016|>
  filter(author_name_givenname != "NA")|>
  mutate(author_name_surname = tolower(author_name_surname),
         author_name_givenname = tolower(author_name_givenname))


#remove possible missing names by matching these patterns
orcid_person_data_2016 <- orcid_person_data_2016 |>
  filter(author_name_givenname != "NA")|>
  mutate(Pat = str_replace_all(author_name_givenname, 
                               pattern = "^[a-z].$", replacement = "NOFIRSTNAME"))|>
  
  subset(Pat!="NOFIRSTNAME")|>
  unique()#remove unwanted columns

orcid_person_data_2016 <- orcid_person_data_2016 |>
  select(-other_names, -Pat)

#save file
save(orcid_person_data_2016, file = "orcid_person_data_2016.Rdata")

#join with refined df based on matching orcidIDs
refined_2016_new <- refined_2016 |>
  separate(author_name, sep = ", ", into = c("author_name_surname", "author_name_givenname"), remove = FALSE) |>
  mutate(author_name_givenname_noabr=str_remove(author_name_givenname, pattern = "^[A-Za-z]+\\.$"),
         author_name_givenname_noabr=na_if(author_name_givenname_noabr, ""),
         has_given_name=case_when(is.na(author_name_givenname_noabr) ~"No",TRUE ~"Yes"),
         has_OI=case_when(is.na(OI) ~ "No",TRUE ~"Yes"))

refined_2016_new <- refined_2016_new|>
  mutate(author_name_surname = tolower(author_name_surname))

#make all names lower case to standardize
refined_2016_new <- refined_2016_new |>
  mutate(author_name_surname = tolower(author_name_surname),
         author_name_givenname = tolower(author_name_givenname))

#change column name for joining
orcid_person_data_2016 <- orcid_person_data_2016 |>
  rename(author_first_name = author_name_givenname)

refined_2016_new <- refined_2016_new |>
  rename(author_first_name = author_name_givenname)

#join names found
refined_2016_new <- left_join(x = refined_2016_new, y = orcid_person_data_2016)

#remove missing first names
refined_2016_new <- refined_2016_new|>
  filter(author_first_name != "NA")

#match all first names found
refined_2016_new <- refined_2016_new|>
  mutate(first_name = coalesce(author_first_name, author_name_givenname_noabr)) 

#weed out missing first names
refined_2016_new <- refined_2016_new |>
  mutate(first_name = str_replace_all(first_name, 
                                      pattern = "^[A-z]-[A-z]$", replacement = "NA"))|>
  subset(first_name != "NA")

#make all names lower case
refined_2016_new <- refined_2016_new |>
  select(-author_first_name, -author_name_givenname_noabr, -has_OI, -has_given_name)|>
  mutate(author_name_surname = tolower(author_name_surname), first_name = tolower(first_name))

#change column order for ease of check
refined_2016_new <- refined_2016_new[, c(1, 2, 3, 4, 21, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19,
                                         20)]


#rename column to merge to retrieve missing orcidIDs
orcid_person_data_2016 <- orcid_person_data_2016 |>
  rename(first_name = author_first_name)

#join again to recover missing orcidIDS
refined_2016_new <- left_join(x=refined_2016_new, y = orcid_person_data_2016, by = c("first_name", "author_name_surname"))

#coalesce so remaining orcid IDs values are assigned
refined_2016_new <- refined_2016_new|>
  mutate(OI = coalesce(OI.y, OI.x))|>
  select(-OI.y, -OI.x)

#to be able to tally and keep track who has OI and/or first name
refined_2016_new <- refined_2016_new |>
  mutate(has_given_name=case_when(is.na(first_name) ~"No",TRUE ~"Yes"),
         has_OI=case_when(is.na(OI) ~ "No", TRUE ~ "Yes"))

refined_2016_new <- refined_2016_new|>
  filter(OI != "NA")

refined_2016_new <- refined_2016_new%>%
  select(-has_given_name, -has_OI)

refined_new_2016$OI[refined_new_2016$groupID == "65384"] <- "NA"

save(refined_2016_new, file = "refined_2016_new.Rdata")

#refine authors' countries ----
library(countrycode)

#separate authors and countries and unify UK
authors_country_2016new <- refined_2016_new|>
  select(first_name, author_name_surname, country)|>
  unique()|>
  filter(country != "NA")

#rename countries with non standard names
authors_country_2016new <- authors_country_2016new |>
  mutate(country = case_when(
    country == "england" ~ "United Kingdom",
    country == "scotland" ~ "United Kingdom",
    country == "wales" ~ "United Kingdom",
    country == "north ireland" ~ "United Kingdom",
    country == "usa" ~ "United States of America",
    country == "papua n guinea" ~ "Papua and New Guinea",
    country == "cent afr republ" ~ "Central African Republic",
    country == "kosovo" ~ "Repuplic of Kosovo",
    country == "namibia" ~ "Namibia",
    TRUE ~ authors_country_2016new$country))

#get country name into iso2c codes, United Kingdom = UK
authors_country_2016new <- authors_country_2016new |>
  mutate(country = countrycode(authors_country_2016new$country, 
                               origin = "country.name", destination = "iso2c"))

#RK for Republic of Kosovo and NB for Namibia
authors_country_2016new <- authors_country_2016new |>
  mutate(country = case_when(
    country == "NA" ~ "NB",
    country == "Kosovo" ~ "RK",
    TRUE ~ authors_country_2016new$country))

#remove empty country cells
authors_country_2016new <- authors_country_2016new |>
  filter(country != "NA")

#weed out remaining authors with no first name
authors_country_2016new <- authors_country_2016new|>
  mutate(first_name = str_replace_all(first_name, 
                                      pattern = "^[A-z]-[A-z]$", replacement = "NA"))|>
  subset(first_name != "NA")

authors_country_2016new <- authors_country_2016new|>
  unique()

#add year to identify the data
authors_country_2016new$Year <- "2016"

#save file as .csv
write.csv(authors_country_2016new, "./Authors_Countries2016.csv")

save(authors_country_2016new, file = "authors_country_2016.Rdata")


#Data 2018 -------
#save as a tibble
refs_2018 <- references_read(data = "./data/2018", 
                             dir=TRUE,
                             include_all = TRUE)

save(refs_2018, file = "refs_2018.Rdata")

prelim_2018 <- as_tibble(clean_refs_2018$prelim)
review_2018 <- as_tibble(clean_refs_2018$review)

#replace mismatches by changing "Author Name" and "AuthorID", respectively 
review_2018$groupID[review_2018$AF == "Graham, Stewart F." ] <- "5968"

#accept and refine results
refined_2018 <- authors_refine(review_2018, prelim_2018)

#find authors orcidID based on DOI of records
DOI_2018 <- refs_2018|>
  filter(DI != "NA")|>
  select(DI, refID)  

DOI_2018_find <- DOI_2018[["DI"]]

#search for orcids associated with DOIs
DOI_2018_auth <- orcid_doi(DOI_2018_find)

#save file as an Rdata file
save(DOI_2018_auth, file = "DOI_2018_auth.Rdata")

#find authors using orcid
#unnest list of information
DOI_2018_search <- plyr::ldply(DOI_2018_auth) 

DOI_2018_search <- DOI_2018_search|>
  unique()

#vector with orcid IDS found
DOI_2018_search_list <- DOI_2018_search$`orcid-identifier.path`

save(DOI_2018_search_list, file = "DOI_2018_search_list.Rdata")


#pluck info extracted into a df
orcid_person_data_2018 <- orcids_ppl_2018 %>% {
  dplyr::tibble(
    author_name_givenname = purrr::map_chr(., purrr::pluck, "name", "given-names", "value", .default=NA_character_),
    author_name_surname = purrr::map_chr(., purrr::pluck, "name", "family-name", "value", .default=NA_character_),
    OI = purrr::map_chr(., purrr::pluck, "name", "path", .default = NA_character_),
    other_names = purrr::map_chr(., pluck, "other-names", "other-name", "value", .default=NA_character_),
    
  )
} 

#standardize the file for later joining
orcid_person_data_2018 <- orcid_person_data_2018|>
  filter(author_name_givenname != "NA")|>
  mutate(author_name_surname = tolower(author_name_surname),
         author_name_givenname = tolower(author_name_givenname))


#remove possible missing names by matching these patterns
orcid_person_data_2018 <- orcid_person_data_2018 |>
  filter(author_name_givenname != "NA")|>
  mutate(Pat = str_replace_all(author_name_givenname, 
                               pattern = "^[A-z]. [A-z].$", replacement = "NOFIRSTNAME"))|>
  
  subset(Pat!="NOFIRSTNAME")|>
  unique()

#remove unwanted columns
orcid_person_data_2018 <- orcid_person_data_2018 |>
  select(-other_names, -Pat)

#save file
save(orcid_person_data_2018, file = "orcid_person_data_2018.Rdata")

#join with refined df based on matching orcidIDs
refined_2018_new <- refined_2018 |>
  separate(author_name, sep = ", ", into = c("author_name_surname", "author_name_givenname"), remove = FALSE) |>
  mutate(author_name_givenname_noabr=str_remove(author_name_givenname, pattern = "^[A-Za-z]+\\.$"),
         author_name_givenname_noabr=na_if(author_name_givenname_noabr, ""),
         has_given_name=case_when(is.na(author_name_givenname_noabr) ~"No",TRUE ~"Yes"),
         has_OI=case_when(is.na(OI) ~ "No",TRUE ~"Yes"))

refined_2018_new <- refined_2018_new|>
  mutate(author_name_surname = tolower(author_name_surname))

#make all names lower case to standardize
refined_2018_new <- refined_2018_new |>
  mutate(author_name_surname = tolower(author_name_surname),
         author_name_givenname = tolower(author_name_givenname))

#change column name for joining
orcid_person_data_2018 <- orcid_person_data_2018 |>
  rename(author_first_name = author_name_givenname)

refined_2018_new <- refined_2018_new |>
  rename(author_first_name = author_name_givenname)

#join names found
refined_2018_new <- left_join(x = refined_2018_new, y = orcid_person_data_2018)

#remove missing first names
refined_2018_new <- refined_2018_new|>
  filter(author_first_name != "NA")

#match all first names found
refined_2018_new <- refined_2018_new|>
  mutate(first_name = coalesce(author_first_name, author_name_givenname_noabr)) 

#weed out missing first names
refined_2018_new <- refined_2018_new |>
  mutate(first_name = str_replace_all(first_name, 
                                      pattern = "^[A-z]. -[A-z].$", replacement = "NA"))|>
  subset(first_name != "NA")

#make all names lower case
refined_2018_new <- refined_2018_new |>
  select(-author_first_name, -author_name_givenname_noabr, -has_OI, -has_given_name)|>
  mutate(author_name_surname = tolower(author_name_surname), first_name = tolower(first_name))

#change column order for ease of check
refined_2018_new <- refined_2018_new[, c(1, 2, 3, 4, 21, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19,
                                         20)]


#rename column to merge to retrieve missing orcidIDs
orcid_person_data_2018 <- orcid_person_data_2018 |>
  rename(first_name = author_first_name)

#join again to recover missing orcidIDS
refined_2018_new <- left_join(x=refined_2018_new, y = orcid_person_data_2018, by = c("first_name", "author_name_surname"))

#coalesce so remaining orcid IDs values are assigned
refined_2018_new <- refined_2018_new|>
  mutate(OI = coalesce(OI.y, OI.x))|>
  select(-OI.y, -OI.x)

#to be able to tally and keep track who has OI and/or first name
refined_2018_new <- refined_2018_new |>
  mutate(has_given_name=case_when(is.na(first_name) ~"No",TRUE ~"Yes"),
         has_OI=case_when(is.na(OI) ~ "No", TRUE ~ "Yes"))

refined_2018_new <- refined_2018_new|>
  filter(OI != "NA")

refined_2018_new <- refined_2018_new%>%
  select(-has_given_name, -has_OI)

refined_new_2017$OI[refined_new_2017$groupID == "65384"] <- "NA"

save(refined_2018_new, file = "refined_2018_new.Rdata")

#refine authors' countries ----
library(countrycode)

#separate authors and countries and unify UK
authors_country_2018new <- refined_2018_new|>
  select(first_name, author_name_surname, country)|>
  unique()|>
  filter(country != "NA")

#rename countries with non standard names
authors_country_2018new <- authors_country_2018new |>
  mutate(country = case_when(
    country == "england" ~ "United Kingdom",
    country == "scotland" ~ "United Kingdom",
    country == "wales" ~ "United Kingdom",
    country == "north ireland" ~ "United Kingdom",
    country == "usa" ~ "United States of America",
    country == "papua n guinea" ~ "Papua and New Guinea",
    country == "cent afr republ" ~ "Central African Republic",
    country == "kosovo" ~ "Repuplic of Kosovo",
    country == "namibia" ~ "Namibia",
    TRUE ~ authors_country_2018new$country))

#get country name into iso2c codes, United Kingdom = UK
authors_country_2018new <- authors_country_2018new |>
  mutate(country = countrycode(authors_country_2018new$country, 
                               origin = "country.name", destination = "iso2c"))

#RK for Republic of Kosovo and NB for Namibia
authors_country_2018new <- authors_country_2018new |>
  mutate(country = case_when(
    country == "NA" ~ "NB",
    country == "Kosovo" ~ "RK",
    TRUE ~ authors_country_2018new$country))

#remove empty country cells
authors_country_2018new <- authors_country_2018new |>
  filter(country != "NA")

#weed out remaining authors with no first name
authors_country_2018new <- authors_country_2018new|>
  mutate(first_name = str_replace_all(first_name, 
                                      pattern = "^[A-z]-[A-z]$", replacement = "NA"))|>
  subset(first_name != "NA")

authors_country_2018new <- authors_country_2018new|>
  unique()

#add year to identify the data
authors_country_2018new$Year <- "2018"

#save file as .csv
write.csv(authors_country_2018new, "./Authors_Countries2018.csv")

save(authors_country_2018new, file = "authors_country_2018.Rdata")

save(authors_country_2015new, file = "authors_country_2015new.Rdata")

# Join all names for NAMSOR----- 
authors_country_2015new <- read_csv("SD_Science_Namsor_Input.csv")

authors_country_2015new$Year <- "2015"

#keep records in x without a match in y, anti_join
author_country <- anti_join(authors_country_2016new, authors_country_2015new)

author_country <- full_join(authors_country_2017new, author_country)

author_country <- full_join(authors_country_2018new, author_country)

author_country <- anti_join(author_country, authors_country_2015new, by = c("first_name", "author_name_surname"))

save(author_country, file = "Jun27_author_country_2016-18.Rdata")


#API affiliation ------
library(jsonlite)
library(httr)
library(tidyverse)

orcid_2018  <- GET(url = "https://api.sandbox.orcid.org/v3.0/0000-0001-7484-8889/works")

orcid_2018 <- rawToChar(orcid_2018$content)

orcid_2018 <- fromJSON(orcid_2018)

names(orcid_2018)
orcid_2018$items

orcid_2018 <- flatten(as.data.frame(orcid_2018))



library(httr)
library(jsonlite)
library(tidyverse)


ROR_API_ENDPOINT <- "https://api.dev.ror.org/organizations"

affiliation_names <- affiliation_2017$university

# Loop through each affiliation name and send API request
response_list2017 <- list()  # Create an empty list to store individual API responses

for (name in affiliation_names) {
  # Encode the affiliation name to handle special characters
  encoded_name <- URLencode(name)
  
  # Prepare the API request URL
  request_url <- paste0(ROR_API_ENDPOINT, "?affiliation=", paste(encoded_name, collapse = "+"))
  
  response <- GET(url = request_url)
  
  response <- rawToChar(response$content)
  
  # Use jsonlite::fromJSON to parse the JSON string and automatically flatten the result
  response_list2017[[name]] <- fromJSON(response, flatten = TRUE)
}


save(response_list, file = "response_list2018.Rdata")

test<- jsonlite::flatten(as.data.frame(response_list))

test <- test|>
  select(id,  name, types, country.country_code, country.country_name, external_ids.ISNI.all,
         external_ids.FundRef.preferred, external_ids.FundRef.all, established, addresses)

responses <- responses|>
  unnest(addresses)

responses <- responses|>
  select(id, name, types, country_code, country_name, FundRef, ISNI)

responses <- responses|>
  unnest(FundRef, ISNI)|>
  unique()

# API ORCID data ----- 
base_url <- "https://pub.orcid.org/v3.0/"

api_key <- "APP-UCB33ACDE2GMGTJ9"  # Replace this with the API key you obtained from ORCID

headers <- c(
  `Content-Type` = "application/json",
  `Authorization` = paste("409b38f2-298c-4cf7-b556-35794a7607dd", api_key)
)

orcid_id <- "0000-0001-7484-8889"  # Replace with the ORCID ID you want to access

# Construct the URL for the record request
record_url <- paste0(base_url, orcid_id, "/record")

# Make the GET request
record_response <- GET(record_url, add_headers(.headers = headers))

# Parse the JSON response
record_data <- fromJSON(content(record_response, "text"))

# Access the record information
print(record_data)


ORCID_API_ENDPOINT <- "https://pub.orcid.org/v3.0/"

ids_2015 <- refined_2015 %>% select(OI) 
ids_2016 <- refined_2016_new %>% select(OI) 
ids_2017 <- refined_2017_new %>% select(OI) 
ids_2018 <- refined_2018_new %>% select(OI) 


orcid_ids <- bind_rows(ids_2015, ids_2016, ids_2017, ids_2018)

orcid_ids <- orcid_ids %>%
  filter(OI != "NA") %>%
  unique
  
# Loop through each affiliation name and send API request
orcid_list2018 <- list()  # Create an empty list to store individual API responses

for (name in orcid_ids) {
  # Encode the affiliation name to handle special characters
  encoded_name <- URLencode(name)
  
  # Prepare the API request URL
  request_url <- paste0(ORCID_API_ENDPOINT, paste(encoded_name, collapse = "+"), "/works")
  
  response <- GET(url = request_url)
  
  if (http_status(response)$status_code == 200) {
    response <- rawToChar(response$content)
    # Use jsonlite::fromJSON to parse the JSON string and automatically flatten the result
    response_orcid[[name]] <- fromJSON(response, flatten = TRUE)
  } else {
    cat("Error occurred for ORCID ID:", name, "\n")
  }
}


save(response_list, file = "response_list2018.Rdata")

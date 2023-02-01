# Milestone 1: Data Cleaning and Visualization
# Note: We received about 87 responses on the survey we regulated. The survey
# is till open and we will continue receiving the responses, but following is 
# the standard cleaning and visualization code that we will continue to use. 

# Useful imports
library(tibble)
library(RCurl)
library(RJSONIO)
library(rvest)
library(igraph)
library(tokenizers)
library(tidyverse)
library(tm)
library(stringr)
library(SnowballC)
library(gtools)
library(stringr)
library(readr)


# Reading data from CSV file
data1 <- read.csv("form.csv")
data1$Your.Name...آپ.کا.نام..

print("Columns are: ")
colnames(data1)

# ---------------------- PEOPLE AND ORGANIZATIONS ----------------------------

# --- Data Cleaning ---

# Renaming the columns:
colnames(data1)[2] = "sub_date"
colnames(data1)[3] = "donor_name"
colnames(data1)[4] = "donor_age"
colnames(data1)[5] = "donor_profession"
colnames(data1)[6] = "donor_city"
colnames(data1)[7] = "donor_city_area"
colnames(data1)[8] = "donor_income_per_month"
colnames(data1)[9] = "amount_donated"
colnames(data1)[10] = "amount_donated_to"
colnames(data1)[11] = "donated_to_organization_name"
colnames(data1)[12] = "donated_to_individual_name"
colnames(data1)[13] = "when_donated"
colnames(data1)[14] = "consent"
colnames(data1)[15] = "donated_to_ngo_name"
colnames(data1)[16] = "place_donated_to"

# Capitalizing the letters
data1$donor_name = toupper(data1$donor_name)
data1$amount_donated_to = toupper(data1$amount_donated_to)
data1$donated_to_organization_name = toupper(data1$donated_to_organization_name)
data1$donated_to_individual_name = toupper(data1$donated_to_individual_name)
data1$donated_to_ngo_name = toupper(data1$donated_to_ngo_name)
data1$place_donated_to = toupper(data1$place_donated_to)

# Creating a list of people and ngos where they donated.

# converting donation amount
amounts <- unique(data1$amount_donated)
amounts_w <- c(1, 2, 3, 4, 5, 6)

people_nodes_ngo <- c()
ngo_amount <- c()
ngo <- c()
for (i in 1:87){
  if (grepl("NGO",data1$amount_donated_to[i], fixed=T)){
    people_nodes_ngo <- append(people_nodes_ngo, data1$alias[i])
    ngo <- append(ngo, gsub("-", "", gsub(" ", "", data1$donated_to_ngo_name[i], fixed=T)))
    a <- which(data1$amount_donated[i] %in% amounts)
    ngo_amount <- append(ngo_amount, amounts_w[a])

  }
}

# Creating a list of people and organizations where they donated.

people_nodes_organization <- c()
organization_amount <- c()
organization <- c()
for (i in 1:87){
  if (grepl("ORGANIZATION",data1$amount_donated_to[i], fixed=T)){
    people_nodes_organization <- append(people_nodes_organization, data1$alias[i])
    organization <- append(organization, gsub(" ", "", data1$donated_to_organization_name[i], fixed=T))
    a <- which(data1$amount_donated[i] %in% amounts)
    organization_amount <- append(organization_amount, amounts_w[a])
  }
}

# Some people put NGOs in the organization category. Removing those from organization list
# and placing them in NGOs list. 
for (i in 1:length(organization)){
  if (grepl("ALKHIDMAT", organization[i])){
    organization[i] <- "ALKHIDMATFOUNDATION"
  }
}
tempList <- list(which(organization %in% ngo))
for (i in 1:length(tempList)){
  people_nodes_ngo <- append(people_nodes_ngo, people_nodes_organization[tempList[[i]]])
  ngo_amount <- append(ngo_amount, organization_amount[tempList[[i]]])
  
  ngo <- append(ngo, organization[tempList[[i]]])
}

orgs <- c()
ppl <- c()
amnts <- c()
for (i in 1:length(organization)){
  if (!(i %in% tempList[[1]])){
    orgs <- append(orgs, organization[i])
    ppl <- append(ppl, people_nodes_organization[i])
    amnts <- append(amnts, organization_amount[i])
  }
}

organization <- orgs
people_nodes_organization <- ppl
organization_amount <- amnts

# Removing people who selected organization but upon asking the organization name, they 
# responded things like "Don't know" etc. 

organization <- organization[-1]
people_nodes_organization <- people_nodes_organization[-1]
organization_amount <- organization_amount[-1]
organization <- organization[-2]
people_nodes_organization <- people_nodes_organization[-2]
organization_amount <- organization_amount[-2]

people_nodes_individual <- c()
individual_amount <- c()
individual <- c()
for (i in 1:87){
  if (grepl("INDIVIDUAL",data1$amount_donated_to[i], fixed=T)){
    people_nodes_individual <- append(people_nodes_individual, data1$alias[i])
    individual <- append(individual, gsub(" ", "", data1$donated_to_individual_name[i], fixed=T))
    a <- which(data1$amount_donated[i] %in% amounts)
    individual_amount <- append(individual_amount, amounts_w[a])
  }
}

# Removing NA
individual <- individual[-17]
people_nodes_individual <- people_nodes_individual[-17]
individual_amount <- individual_amount[-17]
individual <- individual[-19]
people_nodes_individual <- people_nodes_individual[-19]
individual_amount <- individual_amount[-19]
individual <- individual[-2]
people_nodes_individual <- people_nodes_individual[-2]
individual_amount <- individual_amount[-2]
individual <- individual[-21]
people_nodes_individual <- people_nodes_individual[-21]
individual_amount <- individual_amount[-21]
individual <- individual[-22]
people_nodes_individual <- people_nodes_individual[-22]
individual_amount <- individual_amount[-22]
individual <- individual[-27]
people_nodes_individual <- people_nodes_individual[-27]
individual_amount <- individual_amount[-27]
individual <- individual[-3]
people_nodes_individual <- people_nodes_individual[-3]
individual_amount <- individual_amount[-3]

# an entry in individuals said that he/she donated to someone claiming they came
# from JDC. Shifting that to ngo
people_nodes_ngo <- append(people_nodes_ngo, people_nodes_individual[7])
ngo <- append(ngo, "JDC")
ngo_amount <- append(ngo_amount, individual_amount[7])
individual <- individual[-7]
people_nodes_individual <- people_nodes_individual[-7]
individual_amount <- individual_amount[-7]



# Creating final list combining organizations, ngos where people donated.

people <- c()
donated_to <- c()
amount_final <- c()
types <- c()

## Splitting entries on "," and "\n". 
split_cleaning <- function(people_list, other_list){
  total_orgs <- c()
  total_people <- c()
  for (i in 1:length(other_list)){
    if (grepl(",", other_list[i])){
      firstSplit <- strsplit(other_list[i], split=",")
      n <- other_list[i]
      p <- people_list[i]
      l1 <- length(firstSplit[[1]])
      for (k in 1:l1){
        if (grepl("\n", firstSplit[[1]][k])){
          secondSplit <- strsplit(firstSplit[[1]][k], split="\n")
          l2 <- length(secondSplit[[1]])
          if (l2 > 1){
            for (m in 1:l2){
              total_orgs <- append(total_orgs, secondSplit[[1]][m])
              total_people <- append(total_people, p)
            }
          }
          else
          {
            total_orgs <- append(total_orgs, secondSplit[[1]][1])
            total_people <- append(total_people, p)
          }
        } else {
          total_orgs <- append(total_orgs, firstSplit[[1]][k])
          total_people <- append(total_people, p)
        }
      }
    }
    else if (grepl("\n", other_list[i])){
      firstSplit <- strsplit(other_list[i], split="\n")
      print(other_list[i])
      n <- other_list[i]
      p <- people_list[i]
      l1 <- length(firstSplit[[1]])
      if (l1 > 1){
        for (j in 1:l1){
          print(firstSplit[[1]][j])
          if (grepl(",", firstSplit[[1]][j])){
            secondSplit <- strsplit(firstSplit[[1]][j], split=",")
            l2 <- length(secondSplit[[1]])
            if (l2 > 1){
              for (k in 1:l2){
                total_orgs <- append(total_orgs, secondSplit[[1]][k])
                total_people <- append(total_people, p)
              }
            }
            else
            {
              total_orgs <- append(total_orgs, secondSplit[[1]][1])
              total_people <- append(total_people, p)
            }
          }
          else
          {
            total_orgs <- append(total_orgs, firstSplit[[1]][j])
            total_people <- append(total_people, p)
          }
        }
        
      }
      else
      {
        total_orgs <- append(total_orgs, firstSplit[[1]][1])
        total_people <- append(total_people, p)
      }
    }
    else
    {
      total_orgs <- append(total_orgs, other_list[i])
      total_people <- append(total_people, people_list[i])
    }
  }
  return(list(total_people, total_orgs))
}

## Combining people who donated to NGOs and Organizations to one list.

total = split_cleaning(people_nodes_ngo, ngo)
people <- total[[1]]
len_ngo <- length(total[[1]])
donated_to <- total[[2]]

# ngo
for (i in 1:len_ngo){
  types <- append(types, "NGO")
}

total = split_cleaning(people_nodes_organization, organization)
people <- append(people, total[[1]])
donated_to <- append(donated_to, total[[2]])
len_org <- length(total[[1]])
# organization
for (i in 1:len_org){
  types <- append(types, "ORGANIZATION")
}

# team balochistan and dost foundation
people <- append(people, people[13])
donated_to <- append(donated_to, "TEAMBALOCHISTAN")
types <- append(types, "NGO")
people <- append(people, people[13])
donated_to <- append(donated_to, "DOSTFOUNDATION")
types <- append(types, "NGO")
people <- people[-13]
donated_to <- donated_to[-13]
types <- types[-13]

#  kaarachi



check <- data.frame(from=people, to=donated_to)
check <- filter(check, !duplicated(check))
g <- graph_from_data_frame(check) %>% 
  set_vertex_attr(name="types", value=names(V(.))%in% check$from)
g <- g %>% set_vertex_attr(name="type", value = as.logical(V(.)$types))
col <- c("steelblue", "red")
V(g)$size <- degree(g, mode="in")
plot(g, vertex.label=NA,
     vertex.color= col[as.numeric(V(g)$type)+1], edge.arrow.size=.4)

#ind <- data.frame(from=people_nodes_individual, to=individual)
#plot(g, vertex.label=NA, vertex.size=4, edge.arrow.size=.4)

g <- graph_from_data_frame(check)
plot(g, vertex.color="red", vertex.label=NA, edge.arrow.size=.4, vertex.size=3)

# ---------------

people_nodes_others <- c()
others <- c()
for (i in 1:87){
  if ((grepl("INDIVIDUAL",data1$amount_donated_to[i], fixed=T) == F) &
      (grepl("ORGANIZATION",data1$amount_donated_to[i], fixed=T) == F)&
      (grepl("NGO",data1$amount_donated_to[i], fixed=T) == F)){
    people_nodes_others <- append(people_nodes_others, data1$Alias[i])
    others <- append(others, gsub(" ", "", data1$amount_donated_to[i], fixed=T))
  }
}

# --------------------------- PEOPLE AND PLACES ---------------------------
places <- c()
people_places <- c()
for (i in 1:87){
  people_places <- append(people_places, data1$alias[i])
  places <- append(places,  gsub(" ", "", data1$place_donated_to[i], fixed=T))
}

# Removing those people who didn't mention places they donated to. 
new_places <- c()
new_people <- c()
for (i in 1:length(places)){
  if (places[i] != ""){
    new_places <- append(new_places, places[i])
    new_people <- append(new_people, people_places[i])
  }
}
places <- new_places
people_places <- new_people

# Filtering all the cities as places
# 1, 6, 8, 9, 12, 14, 16, 17, 19, 20, 23, 24, 31, 33, 36, 39, 41, 43, 44, 46, 47
actual_places <- c()
people_actual_places <- c()
indexes <- c(1, 2, 7, 9, 10, 13, 15, 17, 18, 20, 21, 24, 32, 37, 40, 42, 44, 45, 47) 
for (i in 1:length(indexes)){
  actual_places <- append(actual_places, places[indexes[i]])
  people_actual_places <- append(people_actual_places, people_places[i])
}

# Cleaning the list by splitting multiple entries.
cleaned_places <- c()
cleaned_people <- c()
for (i in 1:length(actual_places)){
  if ((grepl(",", actual_places[i], fixed=T)) & (grepl("AND", actual_places[i]))){
    temp <- strsplit(actual_places[i], split=",")
    for (j in 1:length(temp[[1]])){
      cleaned_places <- append(cleaned_places, temp[[1]][j])
      cleaned_people <- append(cleaned_people, people_actual_places[i])
    }
  }
  else if (grepl(",", actual_places[i], fixed=T)){
    temp <- strsplit(actual_places[i], split=",")
    for (j in 1:length(temp[[1]])){
      cleaned_places <- append(cleaned_places, temp[[1]][j])
      cleaned_people <- append(cleaned_people, people_actual_places[i])
    }
  } 
  else {
    cleaned_places <- append(cleaned_places, actual_places[i])
    cleaned_people <- append(cleaned_people, people_actual_places[i])
  }
}

# Manual Cleaning
cleaned_people <- append(cleaned_people, cleaned_people[3])
cleaned_people <- append(cleaned_people, cleaned_people[3])
cleaned_places <- append(cleaned_places, "BALOCHISTAN")
cleaned_places <- append(cleaned_places, "SINDH")
cleaned_people <- append(cleaned_people, cleaned_people[10])
cleaned_people <- append(cleaned_people, cleaned_people[10])
cleaned_places <- append(cleaned_places, "TANDOAZAM")
cleaned_places <- append(cleaned_places, "MIRPURKHAS")

cleaned_people <- cleaned_people[-3]
cleaned_places <- cleaned_places[-3]
cleaned_people <- cleaned_people[-10]
cleaned_places <- cleaned_places[-10]

cleaned_people <- cleaned_people[-9]
cleaned_places <- cleaned_places[-9]



# Converting cities to provinces
cleaned_places[1] <- "SINDH"
cleaned_places[2] <- "SINDH"
cleaned_places[3] <- "SINDH"
cleaned_places[5] <- "SINDH"
cleaned_places[6] <- "SINDH"
cleaned_places[7] <- "SINDH"
cleaned_places[8] <- "PUNJAB"
cleaned_places[9]<- "SINDH"
cleaned_places[12] <- "SINDH"
cleaned_places[13] <- "PUNJAB"
cleaned_places[14] <- "GILGIT"
cleaned_places[15] <- "GILGIT"
cleaned_places[17] <- "SINDH"
cleaned_places[18] <- "SINDH"
cleaned_places[20] <- "SINDH"
cleaned_places[23] <- "SINDH"
cleaned_places[24] <- "SINDH"


cleaned_places <- cleaned_places[-16]
cleaned_people <- cleaned_people[-16]

places_province_df <- data.frame(from=cleaned_people, to=cleaned_places)
places_province_df <- filter(places_province_df, !duplicated(places_province_df))
g2 <- graph_from_data_frame(places_province_df) %>% 
  set_vertex_attr(name="types", value=names(V(.))%in% places_province_df$from)
g2 <- g2 %>% set_vertex_attr(name="type", value = as.logical(V(.)$types))
col <- c("steelblue", "red")
plot(g2,
     vertex.color= col[as.numeric(V(g2)$type)+1], edge.arrow.size=.4)
title(main="People who donated to provinces")

# ------------ Converting graphs to unipartite
############ PLACES

# ONLY PROVINCE
from2 <- c()
to2 <- c()

nodes2 <- c()
for (i in 1:length(cleaned_people)){
  if (!(cleaned_people[i] %in% nodes)){
    nodes2 <- append(nodes2, cleaned_people[i])
  }
}

for (i in 1:length(nodes2)){
  x <- list(which(cleaned_people == nodes2[i]))[[1]]
  person <- nodes2[i]
  for (j in 1:length(x)){
    p <- cleaned_places[x[j]]
    for (k in 1:length(cleaned_people)){
      if (cleaned_people[k] != person){
        if (cleaned_places[k] == p){
          from2 <- append(from2, person)
          to2 <- append(to2, cleaned_people[k])
        }
      }
    }
  }
}

n1 <- c()
n2 <- c()
compare <- c()
for (i in 1:length(from2)){
  str1 <- from2[i]
  str2 <- to2[i]
  flag <- T
  if (length(compare) > 0){
    for (j in 1:length(compare)){
      ele1 <- compare[[j]][1]
      ele2 <- compare[[j]][2]
      if (((str1 == ele1) & (str2 == ele2)) | ((str1 == ele2) & (str2 == ele1))){
        flag <- F
      }
    }
    if (flag == T){
      compare[[length(compare)+1]] <- list(str1, str2)
      index <- index + 1
      n1 <- append(n1, str1)
      n2 <- append(n2, str2)
    }
  }
  else {
    compare[[length(compare)+1]] <- list(str1, str2)
    index <- index + 1
    n1 <- append(n1, str1)
    n2 <- append(n2, str2)
  }
}
places_uni_df_province <- data.frame(from=n1, to=n2)

g <- graph_from_data_frame(places_uni_df_province, directed=F)
#V(g)$color <- betweenness(g)
plot(g, vertex.label=NA, vertex.color="black", vertex.size=3)
title(main="Unipartite network of people who donated to provinces.")

#_____________________ people
# from, donated_to
nodes_people <- c()
income <- c()
for (i in 1:length(people)){
  if (!(people[i] %in% nodes_people)){
    nodes_people <- append(nodes_people, people[i])
    x <- which(data1$alias == people[i])
    income <- append(income, data1$amount_donated[x])
  }
}
uni_income <- c(1, 2, 3, 4, 5, 6)
lst_in <- list(unique(income))
c <- c()
for (i in 1:length(nodes_people)){
  if (income[i] == "Less than 5,000" ){
    c <- append(c, 1)
  }
  else if (income[i] == "5,000 - 15,000" ){
    c <- append(c, 2)
  }
  else if (income[i] == "15,000 - 30,000"){
    c <- append(c, 3)
  }
  else if (income[i] == "30,000 - 50,000" ){
    c <- append(c, 4)
  }
  else if (income[i] == "50,000 - 100,000"){
    c <- append(c, 5)
  }
  else if (income[i] == "More than 100,000 (1 lac)"){
    c <- append(c, 6)
  }
}

from_ppl <- c()
to_ppl <- c()


for (i in 1:length(nodes_people)){
  x <- list(which(people == nodes_people[i]))[[1]]
  person <- nodes_people[i]
  for (j in 1:length(x)){
    p <- donated_to[x[j]]
    for (k in 1:length(people)){
      if (people[k] != person){
        if (donated_to[k] == p){
          from_ppl <- append(from_ppl, person)
          to_ppl <- append(to_ppl, people[k])
        }
      }
    }
  }
}

n1 <- c()
n2 <- c()
compare <- c()
for (i in 1:length(from_ppl)){
  str1 <- from_ppl[i]
  str2 <- to_ppl[i]
  flag <- T
  if (length(compare) > 0){
    for (j in 1:length(compare)){
      ele1 <- compare[[j]][1]
      ele2 <- compare[[j]][2]
      if (((str1 == ele1) & (str2 == ele2)) | ((str1 == ele2) & (str2 == ele1))){
        flag <- F
      }
    }
    if (flag == T){
      compare[[length(compare)+1]] <- list(str1, str2)
      index <- index + 1
      n1 <- append(n1, str1)
      n2 <- append(n2, str2)
    }
  }
  else {
    compare[[length(compare)+1]] <- list(str1, str2)
    index <- index + 1
    n1 <- append(n1, str1)
    n2 <- append(n2, str2)
  }
}

final_ppl <- append(n1, people_nodes_individual)
final_2 <- append(n2, individual)


people_uni_df <- data.frame(from=final_ppl, to=final_2)
color_df <- data.frame(n=nodes_people, colors=c)
g3 <- graph_from_data_frame(people_uni_df, directed=F)
test.layout <- layout_(g3,with_dh(weight.edge.lengths = edge_density(g3)/1000))
#V(g3)$size <- degree(g3)
plot(g3, layout=test.layout, vertex.label=NA, vertex.size=3, vertex.color="black")
#title(main="Unipartite graph of people who donated in organizations, NGOs, and individuals")


for (i in 1:87){
  if (grepl("OTHER",data1$amount_donated_to[i], fixed=T)){
    people_nodes_ngo <- append(people_nodes_ngo, data1$alias[i])
    ngo <- append(ngo, gsub("-", "", gsub(" ", "", data1$donated_to_ngo_name[i], fixed=T)))
    a <- which(data1$amount_donated[i] %in% amounts)
    ngo_amount <- append(ngo_amount, amounts_w[a])
    
  }
}
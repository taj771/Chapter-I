# This code generate the sumarry of primary studies included in meta analysis that report in Table 1

# clear memory
rm(list = ls())
library(dplyr)


# load data - for processd data for 250 meters - use for mean elasticity
df_wf <- read.csv("./metadata/meta_data_distance_250m.csv")
# load data - for processd data for 750 meters- use for mean elasticity
df_nwf <- read.csv("./metadata/meta_data_distance_750m.csv")

df <- rbind(df_wf,df_nwf)%>%
  rename("elast"="y_value")

# load data - for other study characteristcis
df1 <- read.csv("./metadata/Meta_dataset_water_clarity_TJ.csv")


region <- df1%>%
  select(studyname,state)

samplesize <- df%>%
  select(study_name,sampsize)%>%
  group_by(study_name)%>%
  mutate(min_sam = min(sampsize))%>%
  mutate(max_sam = max(sampsize))%>%
  mutate(d = max_sam-min_sam)%>%
  mutate(c = paste(min_sam, max_sam, sep='-'))%>%
  distinct(study_name,min_sam,max_sam, .keep_all = T)%>%
  mutate(sample_n = case_when(d==0~paste0(sampsize),
                              d > 0 ~ paste0(c)))%>%
  select(study_name,sample_n)

mu_e <- df%>%
  ungroup()%>%
  select(study_name, elast)%>%
  group_by(study_name)%>%
  mutate(mu_e = mean(elast))%>%
  distinct(study_name, mu_e)


function_form <- df%>%
  select(study_name,funcform)%>%
  group_by(study_name,funcform)%>%
  distinct(study_name,funcform)

region <- df%>%
  select(study_name,west,midwest,south,northeast,multireg)%>%
  group_by(study_name)%>%
  distinct()%>%
  mutate(region = case_when(west ==1 ~ "West",
                            midwest==1 ~ "midwest",
                            south ==1 ~ "south",
                            northeast ==1 ~ "Nort East",
                            multireg ==1 ~ "Multi Regional"))%>%
  select(study_name,region)%>%
  mutate(region = ifelse(study_name == "Clapper & Caudill 2014", "Canada", region))%>%
  mutate(region = ifelse(study_name == "Calder & Arrieta 2019", "Canada", region))%>%
  mutate(region = ifelse(study_name == "TJ, Pat, Richard", "Canada", region))


state <- df1%>%
  select(studyname,state)%>%
  group_by(studyname,state)%>%
  rename("study_name"="studyname")%>%
  distinct()


table <- df%>%
  group_by(study_name)%>%
  summarise( n = n())%>%
  left_join(state)%>%
  left_join(samplesize)%>%
  left_join(function_form)%>%
  left_join(region)%>%
  left_join(mu_e)%>%
  distinct(study_name, .keep_all = T)

library(xtable)

print(xtable(table, type = "latex"), file = "./results/Tables/Table1.tex")


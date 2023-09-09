# clear memory
rm(list = ls())

df <- read.csv("./metadata/meta_dataset_water_clarity_TJ.csv")


df <- df%>%
  mutate("Lake or Resevoir" = case_when(wbtype == "LakeRes"~1,
                                        wbtype=="Estuary"~0))%>%
  mutate("Estuary" = case_when(wbtype=="LakeRes"~0,
                               wbtype=="Estuary"~1))%>%
  mutate("Funform:linlog" = case_when(funcform=="lin-log"~1,
                                      funcform=="linear"~ 0,
                                      funcform=="log-lin"~ 0,
                                      funcform=="double-log"~0))%>%
  mutate("Funform:linear" = case_when(funcform=="lin-log"~0,
                                      funcform=="linear"~ 1,
                                      funcform=="log-lin"~ 0,
                                      funcform=="double-log"~0))%>%
  mutate("Funform:loglin" = case_when(funcform=="lin-log"~0,
                                      funcform=="linear"~ 0,
                                      funcform=="log-lin"~ 1,
                                      funcform=="double-log"~0))%>%
  mutate("Funform:double_log" = case_when(funcform=="lin-log"~0,
                                          funcform=="linear"~ 0,
                                          funcform=="log-lin"~ 0,
                                          funcform=="double-log"~1))%>%
  mutate("waterbody size (Sq.Kilometers)" = wbsize/100000)
  
#if we want to discriminate it by wf and nwf
#waterfront
#df_waterfront <- df%>%
  #subset(distbuf == 1)


summary <- df %>%
  select(wqelast,avgwqvar, `Lake or Resevoir`, Estuary, `waterbody size (Sq.Kilometers)`,
         `Funform:linear`,`Funform:linlog`,`Funform:loglin`,`Funform:double_log`,
         west, midwest, northeast, south, multireg,canada) %>% # select variables to summarise
  rename("Elasticity" = "wqelast",
         "Average SD" = "avgwqvar",
         "Lake or Resevoir" = "Lake or Resevoir",
         "Estuary" = "Estuary",
         "Funfrom Linear" = "Funform:linear",
         "Funfrom Lin-log" = "Funform:linlog",
         "Funfrom Log-lin" = "Funform:loglin",
         "Funfrom Double-log" = "Funform:double_log",
         "West" = "west",
         "Midwest" = "midwest",
         "Northeast" = "northeast",
         "South" = "south",
         "Multi regional" = "multireg",
         "Canada" = "canada")%>%
  summarise_each(funs(mean = mean(., na.rm = T),
                      sd = sd(., na.rm = T),
                      median = median(., na.rm = T),
                      min = min(., na.rm = T), 
                      max = max(., na.rm = T)
  ))

df.stats.tidy <- summary %>% 
  gather(stat, val) %>%
  separate(stat, into = c("var", "stat"), sep = "_") %>%
  spread(stat, val) %>%
  select(var, mean, sd, median, min, max) # reorder columns

datasummary_df(df.stats.tidy, output = "./results/summary_table.docx")

xtable(df.stats.tidy)



print.xtable(xtable(df.stats.tidy.wf), file = "./results/summary_wf.tex")



#non-waterfront
df_nonwaterfront <- df%>%
  subset(distbuf == 2)


summary_nwf <- df_nonwaterfront %>%
  select(wqelast,avgwqvar, `Lake or Resevoir`, Estuary, `waterbody size (Sq.Kilometers)`,
         `Funform:linear`,`Funform:linlog`,`Funform:loglin`,`Funform:double_log`,
         west, midwest, northeast, south, multireg,canada) %>% # select variables to summarise
  rename("Elasticity" = "wqelast",
         "Average SD" = "avgwqvar",
         "Lake or Resevoir" = "Lake or Resevoir",
         "Estuary" = "Estuary",
         "Funfrom Linear" = "Funform:linear",
         "Funfrom Lin-log" = "Funform:linlog",
         "Funfrom Log-lin" = "Funform:loglin",
         "Funfrom Double-log" = "Funform:double_log",
         "West" = "west",
         "Midwest" = "midwest",
         "Northeast" = "northeast",
         "South" = "south",
         "Multi regional" = "multireg",
         "Canada" = "canada")%>%
  summarise_each(funs(mean = mean(., na.rm = T),
                      sd = sd(., na.rm = T),
                      median = median(., na.rm = T),
                      min = min(., na.rm = T), 
                      max = max(., na.rm = T)
  ))

df.stats.tidy.nwf <- summary_nwf %>% 
  gather(stat, val) %>%
  separate(stat, into = c("var", "stat"), sep = "_") %>%
  spread(stat, val) %>%
  select(var, mean, sd, median, min, max) # reorder columns

xtable(df.stats.tidy.nwf)


print.xtable(xtable(df.stats.tidy), file = "./results/summary_wf.tex")


print.xtable(xtable(df.stats.tidy), file = "./results/summary_nwf.tex")












##############################################################################
# 2nd option
library(vtable)

base_year <- min(df$pubyear)


df1<- df%>%
  mutate(vi = elast_sim_se^2)%>%
  mutate(invvariance = 1/vi)%>%
  mutate(wf = case_when(distbuf=="1"~"Waterfront",
                        distbuf=="2"~"Nonwaterfront"))%>%
  rename("Elasticity" = elast_sim)%>%
  mutate("Time trend" = pubyear-base_year)%>%
  mutate("Lake or Resevoir" = case_when(wbtype == "LakeRes"~1,
                             wbtype=="Estuary"~0))%>%
  mutate("Estuary" = case_when(wbtype=="LakeRes"~0,
                             wbtype=="Estuary"~1))%>%
  mutate("Funform:linlog" = case_when(funcform=="lin-log"~1,
                            funcform=="linear"~ 0,
                            funcform=="log-lin"~ 0,
                            funcform=="double-log"~0))%>%
  mutate("Funform:linear" = case_when(funcform=="lin-log"~0,
                            funcform=="linear"~ 1,
                            funcform=="log-lin"~ 0,
                            funcform=="double-log"~0))%>%
  mutate("Funform:loglin" = case_when(funcform=="lin-log"~0,
                            funcform=="linear"~ 0,
                            funcform=="log-lin"~ 1,
                            funcform=="double-log"~0))%>%
  mutate("Funform:double_log" = case_when(funcform=="lin-log"~0,
                            funcform=="linear"~ 0,
                            funcform=="log-lin"~ 0,
                            funcform=="double-log"~1))%>%
  mutate("waterbody size (Sq.Kilometers)" = wbsize/100000)%>%
  rename("West" = west)%>%
  rename("Mid West" = midwest)%>%
  rename("North East" = northeast)%>%
  rename("South" = south)%>%
  rename("Multi regional" = multireg)%>%
  rename("Canada" = canada)%>%
  rename("Average water clarity" = avgwqvar)%>%
  select(distbuf, "Elasticity", "Lake or Resevoir","Time trend","Lake or Resevoir","Estuary","Funform:linlog" ,
         "Funform:linear","Funform:loglin","Funform:double_log","Average water clarity","waterbody size (Sq.Kilometers)","West","Mid West",
         "North East", "South", "Multi regional", "Canada")


st(df1, group = 'distbuf', group.long = T)

st(df1, group = 'distbuf', group.long =  T, out = "latex", file = "./results/table2.tex")

df2 <- df%>%
  group_by(distbuf)%>%
  tally()

datasummary_df(df1)

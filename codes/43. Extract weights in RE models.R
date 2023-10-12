# load data - with outliers 
df <- read.csv("./metadata/meta_dataset_water_clarity_TJ.csv")

# waterfront
df <- df%>%
  subset(distbuf ==1)%>%
  select(obsid,studyid,geog,wqelast,sampsize)%>%
  drop_na(sampsize)%>%
  drop_na(wqelast)%>%
  mutate(vi = log(sampsize))%>%
  group_by(studyid, geog)%>%
  mutate(cluster = cur_group_id())%>%
  ungroup()%>%
  mutate(id = row_number())

wf.re <- rma.mv(yi = wqelast, vi, random = ~ 1 | cluster/obsid, data=df)

re_weight <- weights(wf.re, type="rowsum")[1:403]%>%
  as.data.frame()%>%
  mutate(id = row_number())%>%
  rename(re_w = ".")

write_csv(re_weight, "./metadata/RE_weight.csv")

# Nonwaterfront

# waterfront
df <- df%>%
  subset(distbuf ==2)%>%
  select(obsid,studyid,geog,wqelast,sampsize)%>%
  drop_na(sampsize)%>%
  drop_na(wqelast)%>%
  mutate(vi = log(sampsize))%>%
  group_by(studyid, geog)%>%
  mutate(cluster = cur_group_id())%>%
  ungroup()%>%
  mutate(id = row_number())

wf.re <- rma.mv(yi = wqelast, vi, random = ~ 1 | cluster/obsid, data=df)

re_weight <- weights(wf.re, type="rowsum")[1:240]%>%
  as.data.frame()%>%
  mutate(id = row_number())%>%
  rename(re_w = ".")

write_csv(re_weight, "./metadata/RE_weight_nwf.csv")

##########################################################################

# weight without discriminating distacne bins to caluclate transfer error 

# load data - with outliers 
df <- read.csv("./metadata/meta_dataset_water_clarity_TJ.csv")


df <- df%>%
  select(obsid,studyid,geog,wqelast,sampsize)%>%
  drop_na(sampsize)%>%
  drop_na(wqelast)%>%
  mutate(vi = log(sampsize))%>%
  group_by(studyid, geog)%>%
  mutate(cluster = cur_group_id())%>%
  ungroup()%>%
  mutate(id = row_number())

re <- rma.mv(yi = wqelast, vi, random = ~ 1 | cluster/obsid, data=df)

re_weight <- weights(re, type="rowsum")[1:643]%>%
  as.data.frame()%>%
  mutate(id = row_number())%>%
  rename(re_w = ".")

write_csv(re_weight, "./metadata/RE_weight_withoutdiscrim_wf_nwf.csv")

##########################################################################







sum(re_weight$re_w * df$wqelast) / sum(re_weight$re_w)

t <- data.frame(k = c(table(df$cluster)),
           weight = tapply(re_weight, df$cluster, sum))







w.wf <- data.frame(k=c(table(df$obsid)),
                   weight = tapply(wi.wf,df.wf.re$obsid, sum))


library(tibble)
w.wf <- rownames_to_column(w.wf, var="obsid")
w.wf$obsid <- as.numeric(w.wf$obsid)

# join weights

df.wf.re1 <- df.wf.re%>%
  select(elast_sim,elast_sim_se,cluster,studyid,obsid,geog,vi)%>%
  left_join(w.wf)%>%
  mutate(weighted_elas = weight*elast_sim)%>%
  group_by(studyid, geog)%>%
  mutate(w_elas = sum(weighted_elas))%>%
  distinct(cluster, .keep_all = T)

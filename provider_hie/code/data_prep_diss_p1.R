#### State HIE Consent Policies and Amount of Health Information Exchange #####
# Code for data setup and analysis
# Nate Apathy
# November 20, 2019
# Abstract submitted to AcademyHealth ARM January 2020
# Meaningful Use data, PQRS physician data, state HIE consent policy data

# load libraries
library(pacman)
p_load(RSocrata,skimr,readxl,tidyverse,stargazer,tableone,expss,doBy,truncreg,VGAM,naniar)


#########################################
########## MU DATA
# Step 1: Download and Merge Meaningful Use Files #####
# 1a: Downloading
# files downloaded from:
# https://www.cms.gov/Regulations-and-Guidance/Legislation/EHRIncentivePrograms/PUF.html
# turned into XLSX documents from binary files for import
# don't need unmodified, not for comparison
# unmod_s2_oct17 <- read_excel("~/Documents/RProjects/mu_eval/mu_files/Oct17EP_S2_Un.xlsx")
mod1_s2_oct17 <- read_excel("~/Documents/RProjects/mu_eval/mu_files/Oct17EP_S2_M1.xlsx")
mod2_s2_oct17 <- read_excel("~/Documents/RProjects/mu_eval/mu_files/Oct17EP_S2_M2.xlsx")
mod3_s2_oct17 <- read_excel("~/Documents/RProjects/mu_eval/mu_files/Oct17EP_S2_M3.xlsx")
mod4_s2_oct17 <- read_excel("~/Documents/RProjects/mu_eval/mu_files/Oct17EP_S2_M4.xlsx")

# 1b: Trim to variables of interest
# note: we are only using the MU Stgae 2 modified reporting data, published Oct 2017
# this will limit us to 2015 and 2016 by default
colnames(mod1_s2_oct17)
# want 1:5 and all columns of OBEP69 78:81

colnames(mod1_s2_oct17)[1:5]
# only difference is the payment year "number" in the unmodified file

mod_s2_obep69 <- mod1_s2_oct17[,c(1:5,78:81)] %>% 
  rbind(mod2_s2_oct17[,c(1:5,78:81)]) %>%
  rbind(mod3_s2_oct17[,c(1:5,78:81)]) %>%
  rbind(mod4_s2_oct17[,c(1:5,78:81)])

# rename the fields
colnames(mod_s2_obep69) <- c("npi","prog_yr","pmt_yr","att_succ_dt","cehrt_num","numerator","denom","pct","exclusion")
mod_s2_obep69$modflag <- 1
# modflag = 1 means the calculations are for OBEP69
interop_mu_df <- mod_s2_obep69
interop_mu_df$npi <- as.integer(interop_mu_df$npi)

# 1c: Trimming to 2016 only
interop_mu_df %>% filter(prog_yr==2016) %>%
  mutate(pmt_yr=as.integer(pmt_yr),
         modflag=as.factor(modflag)) -> interop_mu_df
# 191866 obs of 10 vars
# note exclusion flag
table(interop_mu_df$exclusion)
# 155,167 are excluded
# only 36,699 are not in 2016

interop_mu_df %>% filter(exclusion=="N") %>% pull(npi) %>% n_distinct()
# 36,669 without exclusions, so no dupes

#############################################
#### physician compare metadata from end of year 2016
physcomp_meta <- read_csv("~/Documents/RProjects/mu_eval/dissertation1/physiciancompare2016.csv")

colnames(physcomp_meta) <- c("npi", "pac_id", "prof_enrl_id", "last_nm", 
                             "first_nm", "mid_nm", "suffix", "gender", "cred", 
                             "med_school", "md_grad_yr", "pri_spec", "sec_spec1", 
                             "sec_spec2", "sec_spec3", "sec_spec4", "all_sec_specs",
                             "org_legal_name", "group_pac_id", "num_group_mems", 
                             "addr_1", "addr_2", "addr_2_suppr", "city", "state", "zip", 
                             "phone_num", "hosp_affil_ccn1", "hosp_affil_lbn1", 
                             "hosp_affil_ccn2", "hosp_affil_lbn2", "hosp_affil_ccn3", 
                             "hosp_affil_lbn3", "hosp_affil_ccn4", "hosp_affil_lbn4", 
                             "hosp_affil_ccn5", "hosp_affil_lbn5", "prof_acc_medicare_assign", 
                             "rep_qual_meas", "used_ehr", "mill_hearts")

physcomp_meta %>% distinct(npi,group_pac_id)
# 1,124,460 unique npi-pac_id-group_pac_id sets?
# what is the diff btwpac id and group pac id...?
# pac_id is just a proxy for npi, their pecos ID
# individual providers who do not have a group affiliation have NA for group_pac_id
physcomp_meta$group_flag <- if_else(is.na(physcomp_meta$group_pac_id)==TRUE,0,1)

# note: there are NO duplicate providers in those that are NOT excluded
# so we need not do any de-duping
# we only need their npi that appears in the MU file
# but we don't know which practice that is affiliated with for those with multiple entries
# there are no group IDs in the MU file
# we're gonna have to see how many are across state lines and have ambiguous affiliations

############################
#### fix the zip codes and counties
physcomp_meta %>% mutate(zipchars=nchar(zip)) %>% group_by(zipchars) %>%
  summarize(count=n())
# these are much cleaner
# only 5 or 9 digit zips
# just trip the 9s to fives
physcomp_meta$zipfix5 <- substr(physcomp_meta$zip,1,5)
# just take the first 5 digits no big deal


### check for npi matches and provider counts
physcomp_meta %>% distinct(npi,group_pac_id,zipfix5)
# 1,331,626 unique provider/orgs

physcomp_meta %>% distinct(npi) %>% unlist() %>% as.integer() -> pc_npis
# 958,328 unique NPIs in the PC data

interop_mu_df %>% filter(exclusion=="N") %>% distinct(npi) -> mu_npis

pc_npis %in% interop_mu_df$npi %>% table()
# 189,757 of the providers are in the MU files

mu_npis %>% unlist() %in% pc_npis %>% table()
# 36,443 of the MU non-excluded providers are in the PC data
# 36443/36699 = 99%, nice

## match zip codes to counties
zip_county_q416 <- read_csv("~/Documents/RProjects/mu_eval/zip_county_q416.csv")

# add counties
# only want to match based on business address ratio of >50% of addresses in the ZIP being in that county
zip_county_q416 %>% filter(BUS_RATIO>0.5) %>% distinct() -> zip_county_biz_unique

physcomp_meta$zipfix5 %in% zip_county_biz_unique$ZIP %>% table()

physcomp_meta$county <- vlookup(physcomp_meta$zipfix5,zip_county_biz_unique,
                                result_column = "COUNTY",lookup_column = "ZIP")

##########################
####### MU VENDOR
# Step 3: Download Meaningful Use Vendors file
# pull in vendor data
# need the full file from here:
# https://dashboard.healthit.gov/datadashboard/documentation/ehr-products-mu-attestation-data-documentation.php
# filter to EPs attestation year==2016
mu_vendor_16 <- read_csv("~/Documents/RProjects/mu_eval/dissertation1/mu_vendors.csv") %>%
  filter(Provider_Type=="EP" & Program_Year==2016 & Provider_Stage_Number=="Stage 2")

mu_vendor_16 %>%
  group_by(Provider_Type,Provider_Stage_Number,Program_Year,Attestation_Year) %>% summarize(count=n())
# looks good
mu_vendor_16 %>% distinct(NPI) -> mu_vend_npis

unlist(mu_npis) %in% unlist(mu_vend_npis) %>% table()
# they're all there... which is good.

# we can match the interop data to the vendor data using "CEHRT_NUM" which is unique to a product combination
# this can at least narrow the list of vendors

interop_mu_df$cehrt_num %in% mu_vendor_16$EHR_Certification_Number %>% table()
# all of them are there

mu_vendor_16 %>% filter(EHR_Certification_Number %in% interop_mu_df$cehrt_num) %>%
  filter(Product_Setting=="Ambulatory") %>%
  group_by(EHR_Certification_Number) %>% 
  summarize(count=n_distinct(Vendor_Name)) %>% 
  arrange(desc(count)) %>% # what is 1314E01ROIP8EAD
  group_by(count) %>% summarize(count2=n())

# cehrts don't necessarily tell us the vendor for 528+73+10+4 of the CEHRTS
# because they have multiple vendor names under them
# clean this up later, leave out for now

mu_vendor_16 %>% filter(EHR_Certification_Number=="1314E01ROIP8EAD") %>% View()

mu_vendor_16 %>% filter(EHR_Certification_Number %in% interop_mu_df$cehrt_num) %>%
  filter(Product_Setting=="Ambulatory") %>%
  group_by(Vendor_Name) %>% 
  summarize(count=n_distinct(NPI)) %>% 
  arrange(desc(count)) %>% ungroup() %>%
  mutate(pct=count/sum(count),
         cumpct=cumsum(pct))

### overall ranking
# epic, allscripts, eclinwx, athena, nextgen, ge, cerner, greenway, eMDs, integrated practice solutions
# cover half with top 5, group the rest into "other"?
# cover 60% with top 6, and have the 10k count cutoff
# i'm not opposed to that
mu_vendor_16 %>% filter(EHR_Certification_Number %in% interop_mu_df$cehrt_num) %>%
  filter(Product_Setting=="Ambulatory") %>%
  group_by(Vendor_Name) %>% 
  summarize(count=n_distinct(NPI)) %>% 
  arrange(desc(count)) %>% top_n(6,count) %>% pull(Vendor_Name) -> top6_vendors

interop_mu_df$vendor <- vlookup(interop_mu_df$cehrt_num,mu_vendor_16,
                                lookup_column = "EHR_Certification_Number",
                                result_column = "Vendor_Name")

interop_mu_df %>% group_by(vendor) %>% 
  summarize(count=n_distinct(npi)) %>% 
  arrange(desc(count)) %>% ungroup() %>%
  mutate(pct=count/sum(count),
         runpct=cumsum(pct)) %>% 
  top_n(8,count) %>% pull(vendor) -> top8_vendors

# top vendors change once we merge them
# top 8 vendors cover 72% and are all over 5k providers
# that seems better

interop_mu_df$vendor2 <- if_else(interop_mu_df$vendor %in% top8_vendors,interop_mu_df$vendor,"other")

table(interop_mu_df$vendor2)

# columns we need for analysis:
# npi, grad year, pri spec, med school, num_org_mem, zip code, gender, pqrs, credential, state


################### AHRF 2016
# yikes... this is going to be a pain to get apparently for past years
# we have 2017-2018 ahrf so we will use that in the meantime

# see ahrf2018.R
# ahrf_18_cut is the data frame we want, which has been saved as ahrf_18_cut.Rdata
load(file="~/Documents/RProjects/mu_eval/ahrf_18_cut.Rdata")
head(ahrf_18_cut)

# most of these variables are character variables
# so we have to modify them to the appropriate type for analysis

# Variables
# F00011 State FIPS (for matching)
# F00012 County FIPS (for matching)
# F00023 Federal Region Code
# F00020-13 Rural/Urban continuum codes from 2013
# F09787-16 HPSA Code (1=whole county, 2=part of county)
# F08868-16 Total # of Hospitals
# F08905-16 # of hospitals with medicare certification
# F08869-16 # of short term general hospitals (coded 10-1 ????)
# F11386-16 - F11390-16, # of short term general hospitals by bed size
# F11984-16 population estimate
# F14083-16 population estimate >65
# F13191-16 # eligible for medicare
# F13192-16 # medicare advantage enrollees
# F13193-16 % medicare advantage penetration
# F09781-16 per capita personal income, 2016
# F13226-16 median household income, 2016
# F13321-16 percent of persons in poverty, 2016

# create five digit county fips code for merging with PQRS data
ahrf_18_cut$ctyfips <- as.numeric(paste0(ahrf_18_cut$F00011,ahrf_18_cut$F00012))
head(ahrf_18_cut)

# rename these variables so they are easier to work with
colnames(ahrf_18_cut) <- c("stfips2dig","ctyfips2dig","fedreg","rur_urb","hpsacode","num_hosp","num_sh_trm_gen_hosps","num_hosps_medicare","num_st_gh_6-49","num_st_gh_50-99","num_st_gh_100-199","num_st_gh_200-299","num_st_gh_>300","pop_est","pop_est_>65","num_medicare_elig","num_med_adv_enrol","pct_ma_penet","pers_inc_percapita","med_hshold_inc","pct_pers_poverty","ctyfips5")

# variables we definitely want to use, have no "options"
# fedreg, federal region code, this is a character var so its fine to treat it as that; NO  CHANGE
# rural urban continuum code, this is character var but needs to be collapsed into metro or non-metro (01-03=Metro,the rest=nonMetro)
# hpsacode, for provider shortage areas, this is an integer and needs to be a factor variable
# num_hosps_medicare for the number of medicare hospitals in the county, this is char and needs to be integer
# num_medicare_elig for medicare eligible population, this is char needs to be numeric
# med_hsold_inc for income levels, this is char and needs to be numeric (integer)
# pct_pers_poverty for poverty levels, this is is chr and needs to be a numeric, can be whole number to get the outcome variable and this variable on the same scale (for interpretation)

# testing out how to fix the numeric variable needs for percent poverty
# the others will be much more straightforward to convert into numeric
povdectest <- as.numeric(ahrf_18_cut$pct_pers_poverty)
povdectest
as.numeric(paste0(substr(povdectest,1,nchar(povdectest)-1),".",substr(povdectest,nchar(povdectest),nchar(povdectest))))

ahrf_18_cut$pct_pers_poverty_num <- as.numeric(paste0(substr(ahrf_18_cut$pct_pers_poverty,1,
                                                             nchar(ahrf_18_cut$pct_pers_poverty)-1),".",
                                                      substr(ahrf_18_cut$pct_pers_poverty,
                                                             nchar(ahrf_18_cut$pct_pers_poverty),
                                                             nchar(ahrf_18_cut$pct_pers_poverty))))

ahrf_18_cut %>% mutate(hpsacode_fct = as.factor(hpsacode),
                       num_hosps_medicare_int = as.integer(num_hosps_medicare),
                       num_medicare_elig_int = as.integer(num_medicare_elig),
                       med_hshold_inc_int = as.integer(med_hshold_inc),
                       rur_urb_di = if_else(rur_urb %in% c("01","02","03"),"Metro","non-Metro")) -> ahrf

# this file is ready to merge now


###################
###### State Consent Policy
# pulls in from comreg project for consistency
laws16 <- read_excel("~/Documents/RProjects/aha_comreg/data/laws/State_HIE_Laws_2016_xc.xlsx")

laws16 %>% filter(Pilot==FALSE) %>% 
  dplyr::select(c("State","Incentives","G_Transparancy_Eff",
                  "G_Oversight_Stand","G_Transparancy_Eff_Rep",
                  "G_Transparancy_Eff_Aud","Data_Security",
                  "Data_Privacy_Consent","Data_Privacy_PtRestrictions")) -> laws16

laws16 %>% mutate_at(c("Incentives",
                       "G_Oversight_Stand","G_Transparancy_Eff",
                       "G_Transparancy_Eff_Rep",
                       "G_Transparancy_Eff_Aud","Data_Security",
                       "Data_Privacy_Consent"),as.factor) %>%
  mutate(incent=factor(Incentives,
                       labels =  c("Fin","NonFin","Both","None")),
         # IV2, technical standards
         tech_stds=factor(case_when(G_Oversight_Stand==1~"Yes",
                                    G_Oversight_Stand==2~"No")),
         eff_eval=factor(G_Transparancy_Eff,
                         labels=c("Yes","No")),
         audit_authority=factor(G_Transparancy_Eff_Aud,
                                labels = c("Yes","No","Missing")),
         # control outcome, effectiveness reports
         eff_eval_reports=factor(case_when(G_Transparancy_Eff_Rep==1~"Yes",
                                           G_Transparancy_Eff_Rep==2~"No",
                                           G_Transparancy_Eff_Rep==5~NA_character_)),
         data_sec=factor(Data_Security,
                         labels = c("Yes","No","Ambiguous","Missing")),
         # IV 1, consent requirements
         consent_req=factor(case_when(Data_Privacy_Consent==1~"OptIn",
                                      Data_Privacy_Consent==2~"OptOut",
                                      Data_Privacy_Consent %in% c(3,4,5)~"Other",
                                      Data_Privacy_Consent==6~NA_character_),
                            labels = c("OptIn","OptOut",
                                       "Other"))) %>%
  dplyr::select(c("State","incent","tech_stds","eff_eval","audit_authority",
                  "eff_eval_reports","data_sec","consent_req")) %>%
  replace_with_na(replace = list(audit_authority="Missing",
                                 data_sec="Missing")) -> laws16
laws16$state <- laws16$State

########################################3

###############################
########### MERGE FILES

# vendor is already in the MU file
# need to link MU data to the phycomp_meta

analysis_dat <- left_join(physcomp_meta,interop_mu_df)
# excellent. doesn't add any records
analysis_dat$pracyrs <- 2016-analysis_dat$md_grad_yr

# now add the laws in

analysis_dat$consent <- vlookup(analysis_dat$state,laws16,
                                lookup_column = "state",result_column = "consent_req")

analysis_dat %>% group_by(state,consent) %>% summarize(count=n()) %>% View()
# that worked

analysis_dat <- left_join(analysis_dat,ahrf,by=c("county"="ctyfips5"))

# select down justwhat we need
colnames(analysis_dat)

analysis_dat %>% select(c(1:20,25,26,38:57,60,79:84)) -> analysis_dat
# 2,247,729 obs of 49 variables
# this is ALL medicare providers in the 2016 year end file

####### format variables
### for analysis, set reference groups, alternative specifications

# outcome variable: % eSCR
# need as proportion, whole number, flag for hi performers (above 80 and above 75th percentile)

quantile(na.omit(analysis_dat$pct),0.75)
# 0.7288136

analysis_dat %>% mutate(pct_whole = pct*100,
                        hiperf80 = case_when(pct>=.80~"hiperf",
                                             pct<.80~"nothiperf",
                                             TRUE~NA_character_),
                        hiperf75th = case_when(pct>=0.7288136~"hiperf",
                                               pct<0.7288136~"nothiperf",
                                               TRUE~NA_character_),
                        hiperf80 = relevel(factor(hiperf80),ref = "nothiperf"),
                        hiperf75th = relevel(factor(hiperf75th),ref = "nothiperf"),
                        pct_group=case_when(pct>0&pct<=0.1~"0-10%",
                                            pct>0.1&pct<=0.2~"11-20%",
                                            pct>0.2&pct<=0.3~"21-30%",
                                            pct>0.3&pct<=0.4~"31-40%",
                                            pct>0.4&pct<=0.5~"41-50%",
                                            pct>0.5&pct<=0.6~"51-60%",
                                            pct>0.6&pct<=0.7~"61-70%",
                                            pct>0.7&pct<=0.8~"71-80%",
                                            pct>0.8&pct<=0.9~"81-90%",
                                            pct>0.9&pct<=1~"90-100%")) -> analysis_dat


# consent policy variable needs to have reference of opt-out
analysis_dat$consent <- factor(analysis_dat$consent,levels=c("OptOut","OptIn","Other"))

# consent policy as defined or undefined
analysis_dat$consent_def <- if_else(analysis_dat$consent %in% c("OptOut","OptIn","Other"),"Defined","Undefined")

# payment year is numeric, need to make 99s --> NA for heckman model
analysis_dat$pmt_yr2 <- if_else(analysis_dat$pmt_yr==99,NA_integer_,analysis_dat$pmt_yr)

table(analysis_dat$pmt_yr,analysis_dat$pmt_yr2,exclude = F)

# gender is character, with Female as reference

# practice size
## need to convert into group sizes
analysis_dat %>%
  mutate(practice_size=factor(case_when(num_group_mems==1~"Solo",
                                 num_group_mems>1 & num_group_mems<=5~"2to5",
                                 num_group_mems>5 & num_group_mems<=10~"6to10",
                                 num_group_mems>10 & num_group_mems<=50~"11to50",
                                 num_group_mems>50~"morethan50",
                                 TRUE~NA_character_)),
         practice_size=relevel(practice_size,ref="2to5")) -> analysis_dat

# fix practice size for solo practices with no org_pac_id but a pac_id that is a proxy for npi
analysis_dat %>%
  group_by(group_pac_id) %>%
  mutate(grp_pac_npis=n_distinct(npi)) %>%
  # filter(is.na(num_group_mems)==T & is.na(group_pac_id)==F) %>% 
  # (above) looking at providers with non-missing group pac_ids but no group size (~3200 of them)
  # (below) 
  filter(is.na(group_pac_id)==F) %>% 
  group_by(grp_pac_npis) %>% summarize(count=n()) %>% arrange(desc(grp_pac_npis))
# this tells us that every pac_id where group_pac_id are missing, it is a solo practice
# and where both are present, it is still a solo practice

analysis_dat %>% mutate(group_pac_id2=case_when(is.na(group_pac_id)==T~pac_id,
                                               is.na(group_pac_id)==F~group_pac_id),
                        practice_size2=case_when(is.na(group_pac_id)==T&is.na(pac_id)==F~"Solo",
                                                is.na(group_pac_id)==F~as.character(practice_size)),
                        group_pac_id=group_pac_id2,
                        practice_size3=factor(practice_size2,
                                             levels = c("2to5","Solo",
                                                        "6to10","11to50","morethan50"))) -> analysis_dat

# practice years is numeric, OK as is

# specialty code - this is text, we will filter to family practice and pcps only
# filter to primary care providers only
# those with primary specialties of Internal Med, Family Med, general practice

analysis_dat %>% group_by(pri_spec) %>% filter(mu_provider==1) %>%
  summarize(count=n()) %>% 
  arrange(desc(count)) %>% ungroup() %>%
  mutate(pct=round(count/sum(count)*100,2),
         cumpct=cumsum(pct)) %>% top_n(15,count) %>% pull(pri_spec) -> top15_mu_specs

primary_care_specs <- c("FAMILY PRACTICE","INTERNAL MEDICINE","GENERAL PRACTICE")

# the top 15 specialities in the MU provider sample cover:
# 75% of all providers
# no speciality makes up more than 2% of providers after that
# no speciality includes 10k providers after that cutoff

analysis_dat$pcp_flag <- if_else(analysis_dat$pri_spec %in% primary_care_specs,1,0)

analysis_dat$spec_code <- if_else(analysis_dat$pri_spec %in% top15_mu_specs,analysis_dat$pri_spec,"Other")

# vendor - need to make reference group correct
analysis_dat$vendor3 <- relevel(factor(analysis_dat$vendor2),ref="other")

# hpsa code - already a factor, leave as is

# number of medicare hospitals, eligible persons, household income, poverty - leave as-is

# create a meaningful use participating flag
# these are going to be the providers with non-missing MU fields
analysis_dat$mu_provider <- if_else(is.na(analysis_dat$prog_yr)==TRUE,0,1)

################################################
### medicare payments for HHI calculation
# https://www.cms.gov/Research-Statistics-Data-and-Systems/Statistics-Trends-and-Reports/Medicare-Provider-Charge-Data/Downloads/Medicare-Physician-and-Other-Supplier-PUF-Methodology.pdf
# https://www.cms.gov/Research-Statistics-Data-and-Systems/Statistics-Trends-and-Reports/Medicare-Provider-Charge-Data/Physician-and-Other-Supplier2016
# this file got moved
# https://data.cms.gov/Medicare-Physician-Supplier/Medicare-Physician-and-Other-Supplier-National-Pro/85jw-maq9
med_pmts <- read_delim("~/Documents/RProjects/mu_eval/dissertation1/Medicare_Physician_and_Other_Supplier_NPI_Aggregate_CY2016.txt", 
                       "\t", escape_double = FALSE, trim_ws = TRUE)

# assign PCSA via PCSA crosswalk
# get PCSAs
load("~/Documents/RProjects/hhi/ziptopcsa.Rdata")

analysis_dat$pcsa <- vlookup(analysis_dat$zipfix5,zip_to_pcsa,
                             lookup_column = "zip5",result_column = "PCSA")

analysis_dat$pmts2016 <- vlookup(analysis_dat$npi,med_pmts,
                                 lookup_column = "NPI",result_column = "TOTAL_MEDICARE_STND_AMT")
# standardized amount that medicare pays, which accounts for geographic differences in payment rates

length(which(is.na(analysis_dat$pmts2016)==TRUE))
#240k missings

# calculate HHI for PCSAs among primary care providers
## calculate HHI
analysis_dat %>% filter(pcp_flag==1) %>%
  group_by(pcsa,npi) %>%
  summarize(med_pmts=sum(na.omit(pmts2016))) %>%
  mutate(cum_pmts=cumsum(med_pmts),
         tot_pmts=max(cum_pmts),
         mktshare=med_pmts/tot_pmts,
         mktinf=mktshare*100,
         sqmktinf=mktinf^2,
         hhi=sum(sqmktinf)) %>%
  group_by(pcsa) %>% summarize(hhi=mean(hhi)) %>%
  mutate(hhi_cat = case_when(hhi<1500~"unconcentrated",
                             hhi>=1500 & hhi <2500~"moderate_conc",
                             hhi>=2500~"highly_conc")) -> pcsa_pcp_hhi

## calculate market shares for each provider
analysis_dat %>% filter(pcp_flag==1) %>%
  group_by(pcsa,npi) %>%
  summarize(med_pmts=sum(na.omit(pmts2016))) %>%
  mutate(cum_pmts=cumsum(med_pmts),
         tot_pmts=max(cum_pmts),
         mktshare=med_pmts/tot_pmts,
         mktinf=mktshare*100,
         sqmktinf=mktinf^2,
         hhi=sum(sqmktinf)) -> pcsa_npi_mkt_share

# at the group level
# looks drastically different
# almost all PCSAs are 10,000
analysis_dat %>% filter(pcp_flag==1) %>%
  group_by(pcsa,group_pac_id) %>% # could be imperfect for solo practitioners?
  summarize(med_pmts=sum(na.omit(pmts2016))) %>%
  mutate(cum_pmts=cumsum(med_pmts),
         tot_pmts=max(cum_pmts),
         mktshare=med_pmts/tot_pmts,
         mktinf=mktshare*100,
         sqmktinf=mktinf^2,
         hhi=sum(sqmktinf)) %>%
  group_by(pcsa) %>% summarize(hhi=mean(hhi)) %>% ggplot() + geom_histogram(aes(x=hhi))

# merge this back to the data
analysis_dat$pcsa_pcp_pmts_hhi <- vlookup(analysis_dat$pcsa,pcsa_pcp_hhi,
                                          lookup_column = "pcsa",
                                          result_column = "hhi")

analysis_dat$pcsa_pcp_pmts_hhi_cat <- vlookup(analysis_dat$pcsa,pcsa_pcp_hhi,
                                          lookup_column = "pcsa",
                                          result_column = "hhi_cat")

analysis_dat$pcsa_pcp_pmts_hhi_cat <- relevel(factor(analysis_dat$pcsa_pcp_pmts_hhi_cat),ref="unconcentrated")

analysis_dat$pcp_mkt_share <- vlookup(analysis_dat$npi,pcsa_npi_mkt_share,
                                              lookup_column = "npi",
                                              result_column = "mktinf")

#########################
##### provider data from the payments file
# options
# credential - maybe its more reliable here
# race and age breakdowns - can compare mix? but these are just counts, not percentages
# total_med_unique_benes – Total Medicare beneficiaries receiving medical (non-ASP) services.
# Beneficiary_Average_Risk_Score – Average Hierarchical Condition Category (HCC) risk score of beneficiaries. Please refer to the “Additional Information” section of this document for more details on HCC risk scores.
# they have percents of beneficiaries for tons of different chronic condition flags
# this data is only their Part B covered patients
# traditional medicare only, no MA
# not risk adjusted, but do have risk scores

analysis_dat$cred_pmts <- vlookup(analysis_dat$npi,med_pmts,
                                              lookup_column = "NPI",
                                              result_column = "NPPES_CREDENTIALS")

# credential field has 15k unique credentials... probably not worth cleaning up

analysis_dat$avg_risk_score <- vlookup(analysis_dat$npi,med_pmts,
                                       lookup_column = "NPI",
                                       result_column = "BENEFICIARY_AVERAGE_RISK_SCORE")

analysis_dat$avg_age <- vlookup(analysis_dat$npi,med_pmts,
                                       lookup_column = "NPI",
                                       result_column = "BENEFICIARY_AVERAGE_AGE")

analysis_dat$asthma_pct <- vlookup(analysis_dat$npi,med_pmts,
                                lookup_column = "NPI",
                                result_column = "BENEFICIARY_CC_ASTHMA_PERCENT")

analysis_dat$chf_pct <- vlookup(analysis_dat$npi,med_pmts,
                                   lookup_column = "NPI",
                                   result_column = "BENEFICIARY_CC_CHF_PERCENT")

analysis_dat$copd_pct <- vlookup(analysis_dat$npi,med_pmts,
                                lookup_column = "NPI",
                                result_column = "BENEFICIARY_CC_COPD_PERCENT")

analysis_dat$ckd_pct <- vlookup(analysis_dat$npi,med_pmts,
                                lookup_column = "NPI",
                                result_column = "BENEFICIARY_CC_CKD_PERCENT")

analysis_dat$diab_pct <- vlookup(analysis_dat$npi,med_pmts,
                                lookup_column = "NPI",
                                result_column = "BENEFICIARY_CC_DIAB_PERCENT")

analysis_dat$hypt_pct <- vlookup(analysis_dat$npi,med_pmts,
                                lookup_column = "NPI",
                                result_column = "BENEFICIARY_CC_HYPERT_PERCENT")

## data from the AHRQ health systems compendium
# https://www.ahrq.gov/chsp/data-resources/compendium-2016.html

# read in file
grp_link <- read_csv("~/Documents/RProjects/mu_eval/dissertation1/2016-compendium-files/group-practice-linkage-public-file.csv")
unique(analysis_dat$group_pac_id) %in% unique(grp_link$pecos_pac_ids) %>% table()
# 148,044 not in the group linkage file
# 30,553 have matches (not necessarily affiliated with systems, just present in file)

analysis_dat$sys_memb <- vlookup(analysis_dat$group_pac_id,grp_link,
                                 lookup_column = "pecos_pac_ids",
                                 result_column = "health_sys_id")

analysis_dat$sys_memb_flag <- factor(if_else(is.na(analysis_dat$sys_memb)==FALSE,1,0),levels = c(0,1))

table(analysis_dat$sys_memb_flag,analysis_dat$practice_size,exclude = F)
# limitation: these are only group practices affiliated with systems
# no solo practices (they excluded these for some reason)

analysis_dat %>% filter(mu_provider==1 & exclusion=="N" & pcp_flag==1 & used_ehr=="Y") %>%
  group_by(sys_memb_flag,practice_size) %>% summarize(count=n_distinct(npi))

###### number of exchange partners in ... HSA? HRR? PCSA? what market def?
#### ideally would be a zip code radius of drive time but alas...
# count of specialists who reported to MU on this measure, indicating ability to recieve eSCR
# is this in the raw MU data?

# which specialties are viable exchange partners
# e.g. not anesthesiologists or nurse practitioners

analysis_dat %>% group_by(pcsa) %>% 
  summarize(n_spec=uniqueN(npi[pcp_flag==0 & used_ehr=="Y"])) %>%
  mutate(tot=sum(n_spec)) -> pcsa_ehr_specialists



analysis_dat$n_spec_ehr_pcsa <- vlookup(analysis_dat$pcsa,pcsa_ehr_specialists,
                                        lookup_column = "pcsa",
                                        result_column = "n_spec")

analysis_dat %>% filter(pcp_flag==0) %>% group_by(pri_spec) %>% summarize(count=n()) %>% arrange(desc(count))

### recode specialty to higher order groups
# used in HHI calculation
analysis_dat %>% group_by(pri_spec) %>% 
  summarize(count=n_distinct(npi,pac_id)) %>% 
  arrange(desc(count)) %>% 
  ungroup() %>% mutate(pct=count/sum(count)*100,
                       cumpct=cumsum(pct))
# copy out to data google sheet for recodes
# https://docs.google.com/spreadsheets/d/1KweB-5DY_o3RUClbSnIlfOpwVN7iyFiW-f7Ll08vp5A/edit?usp=sharing

spec_recodes <- data.frame(stringsAsFactors=FALSE,
                      pri_spec = c("INTERNAL MEDICINE", "NURSE PRACTITIONER",
                                   "FAMILY PRACTICE", "PHYSICAL THERAPY",
                                   "CHIROPRACTIC",
                                   "CERTIFIED REGISTERED NURSE ANESTHETIST", "ANESTHESIOLOGY",
                                   "CLINICAL SOCIAL WORKER", "OBSTETRICS/GYNECOLOGY", "OPTOMETRY",
                                   "DIAGNOSTIC RADIOLOGY", "CLINICAL PSYCHOLOGIST",
                                   "PSYCHIATRY", "ORTHOPEDIC SURGERY",
                                   "CARDIOVASCULAR DISEASE (CARDIOLOGY)",
                                   "GENERAL SURGERY", "OPHTHALMOLOGY", "EMERGENCY MEDICINE",
                                   "PODIATRY", "NEUROLOGY", "GASTROENTEROLOGY",
                                   "PATHOLOGY", "DERMATOLOGY", "PULMONARY DISEASE",
                                   "UROLOGY", "OTOLARYNGOLOGY",
                                   "HEMATOLOGY/ONCOLOGY", "NEPHROLOGY", "PEDIATRIC MEDICINE",
                                   "PHYSICAL MEDICINE AND REHABILITATION",
                                   "AUDIOLOGIST", "GENERAL PRACTICE", "OCCUPATIONAL THERAPY",
                                   "ENDOCRINOLOGY", "INFECTIOUS DISEASE",
                                   "PLASTIC AND RECONSTRUCTIVE SURGERY", "NEUROSURGERY",
                                   "RHEUMATOLOGY", "RADIATION ONCOLOGY",
                                   "ALLERGY/IMMUNOLOGY", "CRITICAL CARE (INTENSIVISTS)",
                                   "VASCULAR SURGERY", "MEDICAL ONCOLOGY",
                                   "REGISTERED DIETITIAN OR NUTRITION PROFESSIONAL",
                                   "CERTIFIED NURSE MIDWIFE",
                                   "INTERVENTIONAL CARDIOLOGY", "CLINICAL NURSE SPECIALIST",
                                   "THORACIC SURGERY", "PAIN MANAGEMENT",
                                   "CARDIAC ELECTROPHYSIOLOGY", "SPEECH LANGUAGE PATHOLOGIST",
                                   "ORAL SURGERY (DENTIST ONLY)",
                                   "GERIATRIC MEDICINE", "INTERVENTIONAL PAIN MANAGEMENT",
                                   "ANESTHESIOLOGY ASSISTANT", "CARDIAC SURGERY",
                                   "INTERVENTIONAL RADIOLOGY",
                                   "COLORECTAL SURGERY (PROCTOLOGY)", "HAND SURGERY",
                                   "PHYSICIAN ASSISTANT", "MAXILLOFACIAL SURGERY",
                                   "SPORTS MEDICINE", "HOSPICE/PALLIATIVE CARE",
                                   "GYNECOLOGICAL ONCOLOGY", "SURGICAL ONCOLOGY", "HEMATOLOGY",
                                   "OSTEOPATHIC MANIPULATIVE MEDICINE",
                                   "NUCLEAR MEDICINE", "PREVENTATIVE MEDICINE",
                                   "SLEEP LABORATORY/MEDICINE",
                                   "UNDEFINED PHYSICIAN TYPE (SPECIFY)", "ADDICTION MEDICINE",
                                   "GERIATRIC PSYCHIATRY", "NEUROPSYCHIATRY",
                                   "PERIPHERAL VASCULAR DISEASE",
                                   "UNDEFINED NON-PHYSICIAN TYPE (SPECIFY)", NA),
                         count = c(101678, 94679, 85322, 49497, 41597, 39257, 37728, 36469,
                                   32088, 30933, 30064, 26326, 23676, 22174,
                                   21106, 20788, 18083, 17257, 14840, 14414, 13531,
                                   11711, 11531, 10025, 9348, 9312, 8681, 8607,
                                   7783, 7731, 6636, 5959, 5910, 5840, 5822,
                                   4866, 4818, 4603, 4520, 3600, 3517, 3190, 3139,
                                   3139, 3045, 2678, 2308, 2281, 2123, 1970, 1916,
                                   1902, 1721, 1710, 1709, 1570, 1557, 1467,
                                   1435, 1236, 1110, 1042, 1001, 1000, 932, 766, 652,
                                   579, 468, 368, 351, 208, 207, 132, 71, 7, 3),
                           pct = c(10.6, 9.8, 8.9, 5.1, 4.3, 4.1, 3.9, 3.8, 3.3, 3.2, 3.1, 2.7,
                                   2.5, 2.3, 2.2, 2.2, 1.9, 1.8, 1.5, 1.5, 1.4,
                                   1.2, 1.2, 1, 1, 1, 0.9, 0.9, 0.8, 0.8, 0.7,
                                   0.6, 0.6, 0.6, 0.6, 0.5, 0.5, 0.5, 0.5, 0.4,
                                   0.4, 0.3, 0.3, 0.3, 0.3, 0.3, 0.2, 0.2, 0.2,
                                   0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.1,
                                   0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1,
                                   0, 0, 0, 0, 0, 0, 0, 0, 0),
                        cumpct = c(10.6, 20.4, 29.3, 34.5, 38.8, 42.9, 46.8, 50.6, 53.9, 57.1,
                                   60.3, 63, 65.5, 67.8, 70, 72.1, 74, 75.8,
                                   77.3, 78.8, 80.3, 81.5, 82.7, 83.7, 84.7, 85.7,
                                   86.6, 87.5, 88.3, 89.1, 89.8, 90.4, 91, 91.6,
                                   92.2, 92.7, 93.2, 93.7, 94.2, 94.5, 94.9,
                                   95.2, 95.6, 95.9, 96.2, 96.5, 96.7, 97, 97.2,
                                   97.4, 97.6, 97.8, 98, 98.1, 98.3, 98.5, 98.6,
                                   98.8, 98.9, 99.1, 99.2, 99.3, 99.4, 99.5, 99.6,
                                   99.7, 99.8, 99.8, 99.9, 99.9, 99.9, 100, 100,
                                   100, 100, 100, 100),
                   spec_recode = c("PRIMARY_CARE", "PRIMARY_CARE", "PRIMARY_CARE",
                                   "REHAB_THERAPY", "REHAB_THERAPY",
                                   "ANESTHESIA", "ANESTHESIA", "SOCIAL_WORK", "OB_GYN",
                                   "OPTOMETRY", "RADIOLOGY", "BEHAVIORAL_HEALTH",
                                   "BEHAVIORAL_HEALTH", "ORTHO_SURG", "CARDIOLOGY",
                                   "SURGERY", "OPHTHALMOLOGY", "EMERGENCY",
                                   "PODIATRY", "NEUROLOGY", "GASTROENTEROLOGY",
                                   "PATHOLOGY", "DERMATOLOGY", "PULMONOLOGY",
                                   "UROLOGY", "ENT", "HEMATOLOGY", "NEPHROLOGY",
                                   "PEDIATRICS", "REHAB_THERAPY", "AUDIOLOGY",
                                   "PRIMARY_CARE", "REHAB_THERAPY", "ENDOCRINOLOGY",
                                   "INFECTIOUS_DISEASE", "SURGERY", "SURGERY",
                                   "RHEUMATOLOGY", "ONCOLOGY", "IMMUNOLOGY",
                                   "CRITICAL_CARE", "SURGERY", "ONCOLOGY", "DIETETICS",
                                   "OB_GYN", "CARDIOLOGY", "PRIMARY_CARE", "SURGERY",
                                   "PAIN_MGT", "CARDIOLOGY", "REHAB_THERAPY",
                                   "SURGERY", "GERIATRICS", "PAIN_MGT",
                                   "ANESTHESIA", "SURGERY", "RADIOLOGY", "SURGERY",
                                   "SURGERY", "PRIMARY_CARE", "SURGERY", "SPORTS_MED",
                                   "PALLIATIVE", "ONCOLOGY", "SURGERY",
                                   "HEMATOLOGY", "REHAB_THERAPY", "NUCLEAR_MED",
                                   "PREVENTIVE", "SLEEP_MED", "OTHER", "ADDICTION_MED",
                                   "BEHAVIORAL_HEALTH", "NEUROLOGY", "VASCULAR",
                                   "OTHER", "OTHER")
                )

analysis_dat$spec_recode <- vlookup(analysis_dat$pri_spec,spec_recodes,
                                    lookup_column = "pri_spec",
                                    result_column = "spec_recode")

########### all providers analysis variable check
#### 

# outcome: pct
# practice size groups
# vendor
# system membership
# three chronic disease %s, CKD, diabetes, hypertension
# average bene age
# average hcc risk score
### mkt factors
# number of ehr-enabled partners (below)
# hhi - HSA level, into categories - below
# market share - for all providers - below
# hpsa (county)
# medicare hospitals
# median household income
# percent in poverty
# metro/nonmetro
# state HIE consent policy (with missings --> "none") --> recode below
### controls
# years in practice
# gender
# specialty (may need recode)

##################### HSA matching and HHI
zip_to_hsa <- read_excel("Documents/RProjects/hhi/ZipHsaHrr16.xls", 
                         col_types = c("text", "numeric", "text", 
                                       "text", "numeric", "text", "text"))
zip_to_hsa %>% mutate(zipchars=nchar(zipcode16)) %>% group_by(zipchars) %>%
  summarize(count=n()) # just need to add leading zeroes

zip_to_hsa %>% mutate(zipfix5=case_when(nchar(zipcode16)==3~paste0("00",zipcode16),
                                        nchar(zipcode16)==4~paste0("0",zipcode16),
                                        TRUE~zipcode16)) -> zip_to_hsa

analysis_dat$hsanum <- vlookup(analysis_dat$zipfix5,zip_to_hsa,
                               lookup_column = "zipfix5",
                               result_column = "hsanum")


## calc HHI for HSAs by 
# calculate HHI for HSAs among all providers within specialty
## calculate HHI
analysis_dat %>%
  group_by(hsanum,spec_recode,npi) %>%
  summarize(med_pmts=sum(na.omit(pmts2016))) %>% 
  #group_by(hsanum,pri_spec) %>%
  mutate(cum_pmts=cumsum(med_pmts),
         tot_pmts=max(cum_pmts),
         mktshare=med_pmts/tot_pmts,
         mktinf=mktshare*100,
         sqmktinf=mktinf^2,
         hhi=sum(sqmktinf)) %>% 
  group_by(hsanum,spec_recode) %>% summarize(hhi=mean(hhi)) %>%
  mutate(hhi_cat = case_when(hhi<1500~"unconcentrated",
                             hhi>=1500 & hhi <2500~"moderate_conc",
                             hhi>=2500~"highly_conc")) -> hsa_prov_spec_hhi

## calculate market shares for each provider
analysis_dat %>%
  group_by(hsanum,spec_recode,npi) %>%
  summarize(med_pmts=sum(na.omit(pmts2016))) %>%
  mutate(cum_pmts=cumsum(med_pmts),
         tot_pmts=max(cum_pmts),
         mktshare=med_pmts/tot_pmts,
         mktinf=mktshare*100,
         sqmktinf=mktinf^2,
         hhi=sum(sqmktinf)) -> hsa_npi_spec_mkt_share

# merge this back to the data
analysis_dat %>% left_join(hsa_prov_spec_hhi,by=c("hsanum","spec_recode")) -> analysis_dat

analysis_dat$hsa_prov_pmts_hhi_cat <- relevel(factor(analysis_dat$hhi_cat),ref="unconcentrated")

analysis_dat$prov_mkt_share <- vlookup(analysis_dat$npi,hsa_npi_spec_mkt_share,
                                      lookup_column = "npi",
                                      result_column = "mktinf")

##### recode the consent policy variable
analysis_dat %>% mutate(consent2=if_else(is.na(consent)==T,"NoPolicy",paste0(consent))) -> analysis_dat

## number of ehr-capable providers in hsa
analysis_dat %>% group_by(hsanum) %>% 
  summarize(n_ehr_prov=uniqueN(npi[pcp_flag==0 & used_ehr=="Y"])) %>%
  mutate(tot=sum(n_ehr_prov)) -> hsa_ehr_specialists

analysis_dat$n_prov_ehr_hsa <- vlookup(analysis_dat$hsanum,hsa_ehr_specialists,
                                        lookup_column = "hsanum",
                                        result_column = "n_ehr_prov")

# all done, save it off TO BOX ONLY
#save(analysis_dat,file="dissertation1/analytic_file.Rdata")
#load(file="dissertation1/analytic_file.Rdata")
# also save to box folder for good measure
#save(analysis_dat,
   #  file="~/Box/Apathy_Dissertation/P1_PC_HIE_Usage_Consent_Policies/Data/AnalysisFiles/analytic_file.Rdata")
# this box filepath no longer exists (as of Aug 2020)
# source of truth is local now:
save(analysis_dat,
     file="~/Documents/RProjects/mu_eval/dissertation1/analytic_file.Rdata")

############################
#### end of file









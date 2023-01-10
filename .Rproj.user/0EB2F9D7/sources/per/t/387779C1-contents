library(tidyverse)
library(janitor)


veg_summaries <- data.frame(matrix(vector(),ncol=8))
colnames(veg_summaries) <-c("loc","wis","wwis","c","sdi","n","e","fqi")

wis_unit_conv <- wis_unit_conv %>% 
  mutate(w_i_s = `Wetland. Ind.`) %>% 
  select(-`Wetland. Ind.`)

veg_master22_clean <- veg_master_22 %>% 
  clean_names() %>% 
  select(code,w_i_s,native_exotic,growth_habit,coefficient_of_conservatism_c) %>% 
  mutate(species_ground = code,
         c_score = as.numeric(coefficient_of_conservatism_c)) %>% 
  select(-code,-coefficient_of_conservatism_c) %>% 
  merge(wis_unit_conv, by = "w_i_s", all = TRUE) %>% 
  mutate(species_ground = tolower(species_ground))


#Now on to quad_analysis5

# quad_analysis4 <- function(x, y){
#   x %>% 
#     merge(veg_master22_clean, by = "species_ground") %>% 
#     mutate(zero = 0)%>%
#     mutate(percent_G = case_when(growth_habit == "Gramminoid" ~ ave_cover, 
#                                  TRUE ~ zero),
#            percent_F = case_when(growth_habit == "Forb" ~ ave_cover, 
#                                  TRUE ~ zero),
#            percent_S = case_when(growth_habit == "Shrub" ~ ave_cover, 
#                                  TRUE ~ zero),
#            percent_T = case_when(growth_habit == "Tree" ~ ave_cover, 
#                                  TRUE ~ zero),
#            percent_V = case_when(native_exotic == "Vine" ~ ave_cover, 
#                                  TRUE ~ zero),
#            wwis = wis*ave_cover,
#            wc = c_score*ave_cover,
#            pi = ave_cover/sum(ave_cover,na.rm = TRUE),
#            pi2 = (ave_cover/sum(ave_cover,na.rm = TRUE))^2,
#            ave_wis_cover = ifelse(wis == "NA",0,ave_cover)) %>% 
#     summarise(wis = mean(wis, na.rm = TRUE),
#               wwis = sum(wwis, na.rm = TRUE)/sum(ave_wis_cover, na.rm = TRUE),
#               c = mean(c_score,na.rm = TRUE),
#               sdi = sum(pi)-sum(pi2),
#               n = length(which(native_exotic == "N")),
#               e = length(which(native_exotic == "E"|native_exotic == "B")),
#               fqi = sum(c_score,na.rm = TRUE)/sqrt(n),
#               loc = tolower(unique(transect_id)))
# }





###############First 2022 test- already processed data


# ep2_clean <- clean_names(ep2_test) %>% 
#   mutate(ave_cover = (rowSums(.[8:17],na.rm = TRUE)/10))
# 
# 
# test <- ep2_clean %>% 
#   merge(veg_master22_clean, by = "species_ground") %>% 
#   mutate(zero = 0)%>%
#   mutate(percent_G = case_when(growth_habit == "Gramminoid" ~ ave_cover, 
#                                TRUE ~ zero),
#          percent_F = case_when(growth_habit == "Forb" ~ ave_cover, 
#                                TRUE ~ zero),
#          percent_S = case_when(growth_habit == "Shrub" ~ ave_cover, 
#                                TRUE ~ zero),
#          percent_T = case_when(growth_habit == "Tree" ~ ave_cover, 
#                                TRUE ~ zero),
#          percent_V = case_when(native_exotic == "Vine" ~ ave_cover, 
#                                TRUE ~ zero),
#          wwis = wis*ave_cover,
#          wc = c_score*ave_cover,
#          pi = ave_cover/sum(ave_cover,na.rm = TRUE),
#          pi2 = (ave_cover/sum(ave_cover,na.rm = TRUE))^2) %>% 
#   summarise(wis = mean(wis, na.rm = TRUE),
#             wwis = sum(wwis, na.rm = TRUE)/sum(ave_cover, na.rm = TRUE),
#             c = mean(c_score,na.rm = TRUE),
#             sdi = sum(pi)-sum(pi2),
#             n = length(which(native_exotic == "N")),
#             e = length(which(native_exotic == "E"|native_exotic == "B")),
#             fqi = sum(c_score,na.rm = TRUE)/sqrt(n))





hh1_clean <- clean_names(hh1) %>% 
  mutate(ave_cover = (rowSums(.[6:15],na.rm = TRUE)/10))


nem3_clean <- clean_names(nem3) %>% 
  mutate(species_ground = tolower(species_ground),
         ave_cover = (rowSums(.[6:15],na.rm = TRUE)/10))

quad_analysis4(nem3_clean,veg_master22_clean)

################################# Processing new sites

af3_clean <- clean_names(af3) %>% 
  mutate(ave_cover = (rowSums(.[6:15],na.rm = TRUE)/10))

veg_summaries <- rbind(veg_summaries,(quad_analysis4(af3_clean,veg_master22_clean)))


bs1_clean <- clean_names(bs1) %>% 
  mutate(ave_cover = (rowSums(.[6:15],na.rm = TRUE)/10))

veg_summaries <- rbind(veg_summaries,(quad_analysis4(bs1_clean,veg_master22_clean)))


op2_clean <- clean_names(op2) %>% 
  mutate(species_ground = tolower(species_ground),
         ave_cover = (rowSums(.[6:15],na.rm = TRUE)/10))

veg_summaries <- rbind(veg_summaries,(quad_analysis4(op2_clean,veg_master22_clean)))


pd2_clean <- clean_names(pd2) %>% 
  mutate(species_ground = tolower(species_ground),
         ave_cover = (rowSums(.[6:15],na.rm = TRUE)/10))

veg_summaries <- rbind(veg_summaries,(quad_analysis4(pd2_clean,veg_master22_clean)))


r3_clean <- clean_names(r3) %>% 
  mutate(species_ground = tolower(species_ground),
         ave_cover = (rowSums(.[6:15],na.rm = TRUE)/10))

veg_summaries <- rbind(veg_summaries,(quad_analysis4(r3_clean,veg_master22_clean)))


sem4_clean <- clean_names(sem4) %>% 
  mutate(species_ground = tolower(species_ground),
         ave_cover = (rowSums(.[6:15],na.rm = TRUE)/10))

veg_summaries <- rbind(veg_summaries,(quad_analysis4(sem4_clean,veg_master22_clean)))


swm1_clean <- clean_names(swm1) %>% 
  mutate(species_ground = tolower(species_ground),
         ave_cover = (rowSums(.[6:15],na.rm = TRUE)/10))

veg_summaries <- rbind(veg_summaries,(quad_analysis4(swm1_clean,veg_master22_clean)))


swm2_clean <- clean_names(swm2) %>% 
  mutate(species_ground = tolower(species_ground),
         ave_cover = (rowSums(.[6:15],na.rm = TRUE)/10))

veg_summaries <- rbind(veg_summaries,(quad_analysis4(swm2_clean,veg_master22_clean)))


bin2_clean <- clean_names(bin2) %>% 
  mutate(species_ground = tolower(species_ground),
         ave_cover = (rowSums(.[6:15],na.rm = TRUE)/10))

veg_summaries <- rbind(veg_summaries,(quad_analysis4(bin2_clean,veg_master22_clean)))


bin4_clean <- clean_names(bin4) %>% 
  mutate(species_ground = tolower(species_ground),
         ave_cover = (rowSums(.[6:15],na.rm = TRUE)/10))

veg_summaries <- rbind(veg_summaries,(quad_analysis4(bin4_clean,veg_master22_clean)))


cav1_clean <- clean_names(cav1) %>% 
  mutate(species_ground = tolower(species_ground),
         ave_cover = (rowSums(.[6:15],na.rm = TRUE)/10))

veg_summaries <- rbind(veg_summaries,(quad_analysis4(cav1_clean,veg_master22_clean)))


cav3_clean <- clean_names(cav3) %>% 
  mutate(species_ground = tolower(species_ground),
         ave_cover = (rowSums(.[6:15],na.rm = TRUE)/10))

veg_summaries <- rbind(veg_summaries,(quad_analysis4(cav3_clean,veg_master22_clean)))


cwr1_clean <- clean_names(cwr1) %>% 
  mutate(species_ground = tolower(species_ground),
         ave_cover = (rowSums(.[6:15],na.rm = TRUE)/10))

veg_summaries <- rbind(veg_summaries,(quad_analysis4(cwr1_clean,veg_master22_clean)))


cwr2_clean <- clean_names(cwr2) %>% 
  mutate(species_ground = tolower(species_ground),
         ave_cover = (rowSums(.[6:15],na.rm = TRUE)/10))

veg_summaries <- rbind(veg_summaries,(quad_analysis4(cwr2_clean,veg_master22_clean)))


de1_clean <- clean_names(de1) %>% 
  mutate(species_ground = tolower(species_ground),
         ave_cover = (rowSums(.[6:15],na.rm = TRUE)/10))

veg_summaries <- rbind(veg_summaries,(quad_analysis4(de1_clean,veg_master22_clean)))


de2_clean <- clean_names(de2) %>% 
  mutate(species_ground = tolower(species_ground),
         ave_cover = (rowSums(.[6:15],na.rm = TRUE)/10))

veg_summaries <- rbind(veg_summaries,(quad_analysis4(de2_clean,veg_master22_clean)))


jt2_clean <- clean_names(jt2) %>% 
  mutate(species_ground = tolower(species_ground),
         ave_cover = (rowSums(.[6:15],na.rm = TRUE)/10))

veg_summaries <- rbind(veg_summaries,(quad_analysis4(jt2_clean,veg_master22_clean)))


mt1_clean <- clean_names(mt1) %>% 
  mutate(species_ground = tolower(species_ground),
         ave_cover = (rowSums(.[6:15],na.rm = TRUE)/10))

veg_summaries <- rbind(veg_summaries,(quad_analysis4(mt1_clean,veg_master22_clean)))


mt2_clean <- clean_names(mt2) %>% 
  mutate(species_ground = tolower(species_ground),
         ave_cover = (rowSums(.[6:15],na.rm = TRUE)/10))

veg_summaries <- rbind(veg_summaries,(quad_analysis4(mt2_clean,veg_master22_clean)))


npe_clean <- clean_names(npe) %>% 
  mutate(species_ground = tolower(species_ground),
         ave_cover = (rowSums(.[6:15],na.rm = TRUE)/10))

veg_summaries <- rbind(veg_summaries,(quad_analysis4(npe_clean,veg_master22_clean)))


npw_clean <- clean_names(npw) %>% 
  mutate(species_ground = tolower(species_ground),
         ave_cover = (rowSums(.[6:15],na.rm = TRUE)/10))

veg_summaries <- rbind(veg_summaries,(quad_analysis4(npw_clean,veg_master22_clean)))


sw1_clean <- clean_names(sw1) %>% 
  mutate(species_ground = tolower(species_ground),
         ave_cover = (rowSums(.[6:15],na.rm = TRUE)/10))

veg_summaries <- rbind(veg_summaries,(quad_analysis4(sw1_clean,veg_master22_clean)))


vor_clean <- clean_names(vor) %>% 
  mutate(species_ground = tolower(species_ground),
         ave_cover = (rowSums(.[6:15],na.rm = TRUE)/10))

veg_summaries <- rbind(veg_summaries,(quad_analysis4(vor_clean,veg_master22_clean)))


vou_clean <- clean_names(vou) %>% 
  mutate(species_ground = tolower(species_ground),
         ave_cover = (rowSums(.[6:15],na.rm = TRUE)/10))

veg_summaries <- rbind(veg_summaries,(quad_analysis4(vou_clean,veg_master22_clean)))


wm1_clean <- clean_names(wm1) %>% 
  mutate(species_ground = tolower(species_ground),
         ave_cover = (rowSums(.[6:15],na.rm = TRUE)/10))

veg_summaries <- rbind(veg_summaries,(quad_analysis4(wm1_clean,veg_master22_clean)))

#####################################2020 and 2021 data

af2_clean <- clean_names(af2) %>% 
  mutate(species_ground = tolower(species_ground),
         ave_cover = (rowSums(.[6:15],na.rm = TRUE)/10))

veg_summaries <- rbind(veg_summaries,(quad_analysis4(af2_clean,veg_master22_clean)))


bin1_clean <- clean_names(bin1) %>% 
  mutate(species_ground = tolower(species_ground),
         ave_cover = (rowSums(.[6:15],na.rm = TRUE)/10))

veg_summaries <- rbind(veg_summaries,(quad_analysis4(bin1_clean,veg_master22_clean)))


bin3_clean <- clean_names(bin3) %>% 
  mutate(species_ground = tolower(species_ground),
         ave_cover = (rowSums(.[6:15],na.rm = TRUE)/10))

veg_summaries <- rbind(veg_summaries,(quad_analysis4(bin3_clean,veg_master22_clean)))


bin5_clean <- clean_names(bin5) %>% 
  mutate(species_ground = tolower(species_ground),
         ave_cover = (rowSums(.[6:15],na.rm = TRUE)/10))

veg_summaries <- rbind(veg_summaries,(quad_analysis4(bin5_clean,veg_master22_clean)))


bin6_clean <- clean_names(bin6) %>% 
  mutate(species_ground = tolower(species_ground),
         ave_cover = (rowSums(.[6:15],na.rm = TRUE)/10))

veg_summaries <- rbind(veg_summaries,(quad_analysis4(bin6_clean,veg_master22_clean)))


cav2_clean <- clean_names(cav2) %>% 
  mutate(species_ground = tolower(species_ground),
         ave_cover = (rowSums(.[6:15],na.rm = TRUE)/10))

veg_summaries <- rbind(veg_summaries,(quad_analysis4(cav2_clean,veg_master22_clean)))


dw1_clean <- clean_names(dw1) %>% 
  mutate(species_ground = tolower(species_ground),
         ave_cover = (rowSums(.[6:15],na.rm = TRUE)/10))

veg_summaries <- rbind(veg_summaries,(quad_analysis4(dw1_clean,veg_master22_clean)))

ep2_clean <- clean_names(ep2) %>% 
  mutate(species_ground = tolower(species_ground),
         ave_cover = (rowSums(.[6:15],na.rm = TRUE)/10))

veg_summaries <- rbind(veg_summaries,(quad_analysis4(ep2_clean,veg_master22_clean)))


ht1_clean <- clean_names(ht1) %>% 
  mutate(species_ground = tolower(species_ground),
         ave_cover = (rowSums(.[6:15],na.rm = TRUE)/10))

veg_summaries <- rbind(veg_summaries,(quad_analysis4(ht1_clean,veg_master22_clean)))


jt1_clean <- clean_names(jt1) %>% 
  mutate(species_ground = tolower(species_ground),
         ave_cover = (rowSums(.[6:15],na.rm = TRUE)/10))

veg_summaries <- rbind(veg_summaries,(quad_analysis4(jt1_clean,veg_master22_clean)))


ph_clean <- clean_names(ph) %>% 
  mutate(species_ground = tolower(species_ground),
         ave_cover = (rowSums(.[6:15],na.rm = TRUE)/10))

veg_summaries <- rbind(veg_summaries,(quad_analysis4(ph_clean,veg_master22_clean)))


pr1_clean <- clean_names(pr1) %>% 
  mutate(species_ground = tolower(species_ground),
         ave_cover = (rowSums(.[6:15],na.rm = TRUE)/10))

veg_summaries <- rbind(veg_summaries,(quad_analysis4(pr1_clean,veg_master22_clean)))


spi1_clean <- clean_names(spi1) %>% 
  mutate(species_ground = tolower(species_ground),
         ave_cover = (rowSums(.[6:15],na.rm = TRUE)/10))

veg_summaries <- rbind(veg_summaries,(quad_analysis4(spi1_clean,veg_master22_clean)))


urep_clean <- clean_names(urep) %>% 
  mutate(species_ground = tolower(species_ground),
         ave_cover = (rowSums(.[6:15],na.rm = TRUE)/10))

veg_summaries <- rbind(veg_summaries,(quad_analysis4(urep_clean,veg_master22_clean)))


urr1_clean <- clean_names(urr1) %>% 
  mutate(species_ground = tolower(species_ground),
         ave_cover = (rowSums(.[6:15],na.rm = TRUE)/10))

veg_summaries <- rbind(veg_summaries,(quad_analysis4(urr1_clean,veg_master22_clean)))


ursp1_clean <- clean_names(ursp1) %>% 
  mutate(species_ground = tolower(species_ground),
         ave_cover = (rowSums(.[6:15],na.rm = TRUE)/10))

veg_summaries <- rbind(veg_summaries,(quad_analysis4(ursp1_clean,veg_master22_clean)))


ype_clean <- clean_names(ype) %>% 
  mutate(species_ground = tolower(species_ground),
         ave_cover = (rowSums(.[6:15],na.rm = TRUE)/10))

veg_summaries <- rbind(veg_summaries,(quad_analysis4(ype_clean,veg_master22_clean)))


ypw_clean <- clean_names(ypw) %>% 
  mutate(species_ground = tolower(species_ground),
         ave_cover = (rowSums(.[6:15],na.rm = TRUE)/10))

veg_summaries <- rbind(veg_summaries,(quad_analysis4(ypw_clean,veg_master22_clean)))


yps_clean <- clean_names(yps) %>% 
  mutate(species_ground = tolower(species_ground),
         ave_cover = (rowSums(.[6:15],na.rm = TRUE)/10))

veg_summaries <- rbind(veg_summaries,(quad_analysis4(yps_clean,veg_master22_clean)))


nem2_clean <- clean_names(nem2) %>% 
  mutate(species_ground = tolower(species_ground),
         ave_cover = (rowSums(.[6:15],na.rm = TRUE)/10))

veg_summaries <- rbind(veg_summaries,(quad_analysis4(nem2_clean,veg_master22_clean)))


phm_clean <- clean_names(phm) %>% 
  mutate(species_ground = tolower(species_ground),
         ave_cover = (rowSums(.[6:15],na.rm = TRUE)/10))

veg_summaries <- rbind(veg_summaries,(quad_analysis4(phm_clean,veg_master22_clean)))


prs1_clean <- clean_names(prs1) %>% 
  mutate(species_ground = tolower(species_ground),
         ave_cover = (rowSums(.[6:15],na.rm = TRUE)/10))

veg_summaries <- rbind(veg_summaries,(quad_analysis4(prs1_clean,veg_master22_clean)))


spi2_clean <- clean_names(spi2) %>% 
  mutate(species_ground = tolower(species_ground),
         ave_cover = (rowSums(.[6:15],na.rm = TRUE)/10))

veg_summaries <- rbind(veg_summaries,(quad_analysis4(spi2_clean,veg_master22_clean)))


urr2_clean <- clean_names(urr2) %>% 
  mutate(species_ground = tolower(species_ground),
         ave_cover = (rowSums(.[6:15],na.rm = TRUE)/10))

veg_summaries <- rbind(veg_summaries,(quad_analysis4(urr2_clean,veg_master22_clean)))


ursp2_clean <- clean_names(ursp2) %>% 
  mutate(species_ground = tolower(species_ground),
         ave_cover = (rowSums(.[6:15],na.rm = TRUE)/10))

veg_summaries <- rbind(veg_summaries,(quad_analysis4(ursp2_clean,veg_master22_clean)))


wrp1_clean <- clean_names(wrp1) %>% 
  mutate(species_ground = tolower(species_ground),
         ave_cover = (rowSums(.[6:15],na.rm = TRUE)/10))

veg_summaries <- rbind(veg_summaries,(quad_analysis4(wrp1_clean,veg_master22_clean)))


wrpw_clean <- clean_names(wrpw) %>% 
  mutate(species_ground = tolower(species_ground),
         ave_cover = (rowSums(.[6:15],na.rm = TRUE)/10))

veg_summaries <- rbind(veg_summaries,(quad_analysis4(wrpw_clean,veg_master22_clean)))


mor3_clean <- clean_names(mor3) %>% 
  mutate(species_ground = tolower(species_ground),
         ave_cover = (rowSums(.[6:15],na.rm = TRUE)/10))

veg_summaries <- rbind(veg_summaries,(quad_analysis4(mor3_clean,veg_master22_clean)))

nwm3_clean <- clean_names(nwm3) %>% 
  mutate(species_ground = tolower(species_ground),
         ave_cover = (rowSums(.[6:15],na.rm = TRUE)/10))

veg_summaries <- rbind(veg_summaries,(quad_analysis4(nwm3_clean,veg_master22_clean)))

#Block of code used to test if all rows are being paired with master veg document. If number of rows differ, check documents.
test <- nwm3_clean %>%
  merge(veg_master22_clean, by = "species_ground") %>%
  mutate(zero = 0)%>%
  mutate(percent_G = case_when(growth_habit == "Gramminoid" ~ ave_cover,
                               TRUE ~ zero),
         percent_F = case_when(growth_habit == "Forb" ~ ave_cover,
                               TRUE ~ zero),
         percent_S = case_when(growth_habit == "Shrub" ~ ave_cover,
                               TRUE ~ zero),
         percent_T = case_when(growth_habit == "Tree" ~ ave_cover,
                               TRUE ~ zero),
         percent_V = case_when(native_exotic == "Vine" ~ ave_cover,
                               TRUE ~ zero),
         wwis = wis*ave_cover,
         wc = c_score*ave_cover,
         pi = ave_cover/sum(ave_cover,na.rm = TRUE),
         pi2 = (ave_cover/sum(ave_cover,na.rm = TRUE))^2,
         ave_wis_cover = ifelse(wis == "NA",0,ave_cover))


veg_summaries_full <- veg_summaries %>% 
  rbind(veg_scores)

write.csv(veg_summaries_full, "veg_summaries_full.csv", row.names = FALSE)






quad_analysis5 <- function(x, y){
  x %>% 
  mutate(species_ground=tolower(species_ground)) %>% 
  merge(y, by = "species_ground") %>% 
  mutate(zero = 0)%>%
  mutate(percent_G = case_when(growth_habit == "Graminoid" ~ ave_cover, 
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
         ave_wis_cover = ifelse(wis == "NA",0,ave_cover),
         bg=ifelse(species_ground=="bg",ave_cover,0),
         ld=ifelse(species_ground=="ld",ave_cover,0)) %>% 
  summarise(wis = mean(wis, na.rm = TRUE),
            wwis = sum(wwis, na.rm = TRUE)/sum(ave_wis_cover, na.rm = TRUE),
            c = mean(c_score,na.rm = TRUE),
            wc=mean(wc,na.rm = TRUE),
            sdi = sum(pi)-sum(pi2),
            n = length(which(native_exotic == "N")),
            e = length(which(native_exotic == "E"|native_exotic == "B")),
            fqi = sum(c_score,na.rm = TRUE)/sqrt(n),
            loc = tolower(unique(transect_id)),
            graminoid=sum(growth_habit=="Graminoid",na.rm = TRUE),
            forb=sum(growth_habit=="Forb",na.rm = TRUE),
            tree=sum(growth_habit=="Tree",na.rm = TRUE),
            shrub=sum(growth_habit=="Shrub",na.rm = TRUE),
            vine=sum(growth_habit=="Vine",na.rm = TRUE),
            p_graminoid = sum(percent_G,na.rm = TRUE),
            p_forb=sum(percent_F,na.rm = TRUE),
            p_shrub=sum(percent_S,na.rm = TRUE),
            p_tree=sum(percent_T,na.rm = TRUE),
            p_vine=sum(percent_V,na.rm = TRUE),
            ave_cover=sum(ave_cover,na.rm = TRUE),
            bg=sum(bg),
            ld=sum(ld))
}



# 
# test <-  af3_clean%>% 
#   mutate(species_ground=tolower(species_ground)) %>% 
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
#          pi2 = (ave_cover/sum(ave_cover,na.rm = TRUE))^2,
#          ave_wis_cover = ifelse(wis == "NA",0,ave_cover)) %>% 
#   summarise(wis = mean(wis, na.rm = TRUE),
#             wwis = sum(wwis, na.rm = TRUE)/sum(ave_wis_cover, na.rm = TRUE),
#             c = mean(c_score,na.rm = TRUE),
#             wc=mean(wc,na.rm = TRUE),
#             sdi = sum(pi)-sum(pi2),
#             n = length(which(native_exotic == "N")),
#             e = length(which(native_exotic == "E"|native_exotic == "B")),
#             fqi = sum(c_score,na.rm = TRUE)/sqrt(n),
#             loc = tolower(unique(transect_id)),
#             gramminoid = sum(percent_G),
#             forb=sum(percent_F),
#             shrub=sum(percent_S),
#             tree=sum(percent_T),
#             vine=sum(percent_V))



veg_summaries_new <- data.frame(matrix(vector(),ncol=22))
colnames(veg_summaries_new) <-c("loc","wis","wwis","c","wc","sdi","n","e","fqi",
                                "gramminoid","forb","shrub","tree","vine",
                                "p_gramminoid","p_forb","p_shrub","p_tree","p_vine",
                                "ave_cover","bg","ld")



hh1_clean <- clean_names(hh1) %>% 
  mutate(ave_cover = (rowSums(.[6:15],na.rm = TRUE)/10))


nem3_clean <- clean_names(nem3) %>% 
  mutate(species_ground = tolower(species_ground),
         ave_cover = (rowSums(.[6:15],na.rm = TRUE)/10))

quad_analysis5(nem3_clean,veg_master22_clean)

################################# Processing new sites

af3_clean <- clean_names(af3) %>% 
  mutate(ave_cover = (rowSums(.[6:15],na.rm = TRUE)/10),
         species_ground=tolower(species_ground))

veg_summaries_new <- rbind(veg_summaries_new,(quad_analysis5(af3_clean,veg_master22_clean)))


bs1_clean <- clean_names(bs1) %>% 
  mutate(ave_cover = (rowSums(.[6:15],na.rm = TRUE)/10))

veg_summaries_new <- rbind(veg_summaries_new,(quad_analysis5(bs1_clean,veg_master22_clean)))


op2_clean <- clean_names(op2) %>% 
  mutate(species_ground = tolower(species_ground),
         ave_cover = (rowSums(.[6:15],na.rm = TRUE)/10))

veg_summaries_new <- rbind(veg_summaries_new,(quad_analysis5(op2_clean,veg_master22_clean)))


pd2_clean <- clean_names(pd2) %>% 
  mutate(species_ground = tolower(species_ground),
         ave_cover = (rowSums(.[6:15],na.rm = TRUE)/10))

veg_summaries_new <- rbind(veg_summaries_new,(quad_analysis5(pd2_clean,veg_master22_clean)))


r3_clean <- clean_names(r3) %>% 
  mutate(species_ground = tolower(species_ground),
         ave_cover = (rowSums(.[6:15],na.rm = TRUE)/10))

veg_summaries_new <- rbind(veg_summaries_new,(quad_analysis5(r3_clean,veg_master22_clean)))


sem4_clean <- clean_names(sem4) %>% 
  mutate(species_ground = tolower(species_ground),
         ave_cover = (rowSums(.[6:15],na.rm = TRUE)/10))

veg_summaries_new <- rbind(veg_summaries_new,(quad_analysis5(sem4_clean,veg_master22_clean)))


swm1_clean <- clean_names(swm1) %>% 
  mutate(species_ground = tolower(species_ground),
         ave_cover = (rowSums(.[6:15],na.rm = TRUE)/10))

veg_summaries_new <- rbind(veg_summaries_new,(quad_analysis5(swm1_clean,veg_master22_clean)))


swm2_clean <- clean_names(swm2) %>% 
  mutate(species_ground = tolower(species_ground),
         ave_cover = (rowSums(.[6:15],na.rm = TRUE)/10))

veg_summaries_new <- rbind(veg_summaries_new,(quad_analysis5(swm2_clean,veg_master22_clean)))


bin2_clean <- clean_names(bin2) %>% 
  mutate(species_ground = tolower(species_ground),
         ave_cover = (rowSums(.[6:15],na.rm = TRUE)/10))

veg_summaries_new <- rbind(veg_summaries_new,(quad_analysis5(bin2_clean,veg_master22_clean)))


bin4_clean <- clean_names(bin4) %>% 
  mutate(species_ground = tolower(species_ground),
         ave_cover = (rowSums(.[6:15],na.rm = TRUE)/10))

veg_summaries_new <- rbind(veg_summaries_new,(quad_analysis5(bin4_clean,veg_master22_clean)))


cav1_clean <- clean_names(cav1) %>% 
  mutate(species_ground = tolower(species_ground),
         ave_cover = (rowSums(.[6:15],na.rm = TRUE)/10))

veg_summaries_new <- rbind(veg_summaries_new,(quad_analysis5(cav1_clean,veg_master22_clean)))


cav3_clean <- clean_names(cav3) %>% 
  mutate(species_ground = tolower(species_ground),
         ave_cover = (rowSums(.[6:15],na.rm = TRUE)/10))

veg_summaries_new <- rbind(veg_summaries_new,(quad_analysis5(cav3_clean,veg_master22_clean)))


cwr1_clean <- clean_names(cwr1) %>% 
  mutate(species_ground = tolower(species_ground),
         ave_cover = (rowSums(.[6:15],na.rm = TRUE)/10))

veg_summaries_new <- rbind(veg_summaries_new,(quad_analysis5(cwr1_clean,veg_master22_clean)))


cwr2_clean <- clean_names(cwr2) %>% 
  mutate(species_ground = tolower(species_ground),
         ave_cover = (rowSums(.[6:15],na.rm = TRUE)/10))

veg_summaries_new <- rbind(veg_summaries_new,(quad_analysis5(cwr2_clean,veg_master22_clean)))


de1_clean <- clean_names(de1) %>% 
  mutate(species_ground = tolower(species_ground),
         ave_cover = (rowSums(.[6:15],na.rm = TRUE)/10))

veg_summaries_new <- rbind(veg_summaries_new,(quad_analysis5(de1_clean,veg_master22_clean)))


de2_clean <- clean_names(de2) %>% 
  mutate(species_ground = tolower(species_ground),
         ave_cover = (rowSums(.[6:15],na.rm = TRUE)/10))

veg_summaries_new <- rbind(veg_summaries_new,(quad_analysis5(de2_clean,veg_master22_clean)))


jt2_clean <- clean_names(jt2) %>% 
  mutate(species_ground = tolower(species_ground),
         ave_cover = (rowSums(.[6:15],na.rm = TRUE)/10))

veg_summaries_new <- rbind(veg_summaries_new,(quad_analysis5(jt2_clean,veg_master22_clean)))


mt1_clean <- clean_names(mt1) %>% 
  mutate(species_ground = tolower(species_ground),
         ave_cover = (rowSums(.[6:15],na.rm = TRUE)/10))

veg_summaries_new <- rbind(veg_summaries_new,(quad_analysis5(mt1_clean,veg_master22_clean)))


mt2_clean <- clean_names(mt2) %>% 
  mutate(species_ground = tolower(species_ground),
         ave_cover = (rowSums(.[6:15],na.rm = TRUE)/10))

veg_summaries_new <- rbind(veg_summaries_new,(quad_analysis5(mt2_clean,veg_master22_clean)))


npe_clean <- clean_names(npe) %>% 
  mutate(species_ground = tolower(species_ground),
         ave_cover = (rowSums(.[6:15],na.rm = TRUE)/10))

veg_summaries_new <- rbind(veg_summaries_new,(quad_analysis5(npe_clean,veg_master22_clean)))


npw_clean <- clean_names(npw) %>% 
  mutate(species_ground = tolower(species_ground),
         ave_cover = (rowSums(.[6:15],na.rm = TRUE)/10))

veg_summaries_new <- rbind(veg_summaries_new,(quad_analysis5(npw_clean,veg_master22_clean)))


sw1_clean <- clean_names(sw1) %>% 
  mutate(species_ground = tolower(species_ground),
         ave_cover = (rowSums(.[6:15],na.rm = TRUE)/10))

veg_summaries_new <- rbind(veg_summaries_new,(quad_analysis5(sw1_clean,veg_master22_clean)))


vor_clean <- clean_names(vor) %>% 
  mutate(species_ground = tolower(species_ground),
         ave_cover = (rowSums(.[6:15],na.rm = TRUE)/10))

veg_summaries_new <- rbind(veg_summaries_new,(quad_analysis5(vor_clean,veg_master22_clean)))


vou_clean <- clean_names(vou) %>% 
  mutate(species_ground = tolower(species_ground),
         ave_cover = (rowSums(.[6:15],na.rm = TRUE)/10))

veg_summaries_new <- rbind(veg_summaries_new,(quad_analysis5(vou_clean,veg_master22_clean)))


wm1_clean <- clean_names(wm1) %>% 
  mutate(species_ground = tolower(species_ground),
         ave_cover = (rowSums(.[6:15],na.rm = TRUE)/10))

veg_summaries_new <- rbind(veg_summaries_new,(quad_analysis5(wm1_clean,veg_master22_clean)))

#####################################2020 and 2021 data

af2_clean <- clean_names(af2) %>% 
  mutate(species_ground = tolower(species_ground),
         ave_cover = (rowSums(.[6:15],na.rm = TRUE)/10))

veg_summaries_new <- rbind(veg_summaries_new,(quad_analysis5(af2_clean,veg_master22_clean)))


bin1_clean <- clean_names(bin1) %>% 
  mutate(species_ground = tolower(species_ground),
         ave_cover = (rowSums(.[6:15],na.rm = TRUE)/10))

veg_summaries_new <- rbind(veg_summaries_new,(quad_analysis5(bin1_clean,veg_master22_clean)))


bin3_clean <- clean_names(bin3) %>% 
  mutate(species_ground = tolower(species_ground),
         ave_cover = (rowSums(.[6:15],na.rm = TRUE)/10))

veg_summaries_new <- rbind(veg_summaries_new,(quad_analysis5(bin3_clean,veg_master22_clean)))


bin5_clean <- clean_names(bin5) %>% 
  mutate(species_ground = tolower(species_ground),
         ave_cover = (rowSums(.[6:15],na.rm = TRUE)/10))

veg_summaries_new <- rbind(veg_summaries_new,(quad_analysis5(bin5_clean,veg_master22_clean)))

bin5_clean<-bin5_clean %>% 
  select(1:16)



bin6_clean <- clean_names(bin6) %>% 
  mutate(species_ground = tolower(species_ground),
         ave_cover = (rowSums(.[6:15],na.rm = TRUE)/10))

veg_summaries_new <- rbind(veg_summaries_new,(quad_analysis5(bin6_clean,veg_master22_clean)))


cav2_clean <- clean_names(cav2) %>% 
  mutate(species_ground = tolower(species_ground),
         ave_cover = (rowSums(.[6:15],na.rm = TRUE)/10))

veg_summaries_new <- rbind(veg_summaries_new,(quad_analysis5(cav2_clean,veg_master22_clean)))

cav2_clean<-cav2_clean %>% 
  select(1:16)


dw1_clean <- clean_names(dw1) %>% 
  mutate(species_ground = tolower(species_ground),
         ave_cover = (rowSums(.[6:15],na.rm = TRUE)/10))

veg_summaries_new <- rbind(veg_summaries_new,(quad_analysis5(dw1_clean,veg_master22_clean)))

ep2_clean <- clean_names(ep2) %>% 
  mutate(species_ground = tolower(species_ground),
         ave_cover = (rowSums(.[6:15],na.rm = TRUE)/10))

veg_summaries_new <- rbind(veg_summaries_new,(quad_analysis5(ep2_clean,veg_master22_clean)))


ht1_clean <- clean_names(ht1) %>% 
  mutate(species_ground = tolower(species_ground),
         ave_cover = (rowSums(.[6:15],na.rm = TRUE)/10))

veg_summaries_new <- rbind(veg_summaries_new,(quad_analysis5(ht1_clean,veg_master22_clean)))


jt1_clean <- clean_names(jt1) %>% 
  mutate(species_ground = tolower(species_ground),
         ave_cover = (rowSums(.[6:15],na.rm = TRUE)/10))

veg_summaries_new <- rbind(veg_summaries_new,(quad_analysis5(jt1_clean,veg_master22_clean)))


jt1_clean <- jt1_clean %>% 
  select(1:16)




ph_clean <- clean_names(ph) %>% 
  mutate(species_ground = tolower(species_ground),
         ave_cover = (rowSums(.[6:15],na.rm = TRUE)/10))

veg_summaries_new <- rbind(veg_summaries_new,(quad_analysis5(ph_clean,veg_master22_clean)))


pr1_clean <- clean_names(pr1) %>% 
  mutate(species_ground = tolower(species_ground),
         ave_cover = (rowSums(.[6:15],na.rm = TRUE)/10))

veg_summaries_new <- rbind(veg_summaries_new,(quad_analysis5(pr1_clean,veg_master22_clean)))

pr1_clean<-pr1_clean %>% 
  select(1:16)



spi1_clean <- clean_names(spi1) %>% 
  mutate(species_ground = tolower(species_ground),
         ave_cover = (rowSums(.[6:15],na.rm = TRUE)/10))

veg_summaries_new <- rbind(veg_summaries_new,(quad_analysis5(spi1_clean,veg_master22_clean)))

spi1_clean<-spi1_clean %>% 
  select(1:16)



urep_clean <- clean_names(urep) %>% 
  mutate(species_ground = tolower(species_ground),
         ave_cover = (rowSums(.[6:15],na.rm = TRUE)/10))

veg_summaries_new <- rbind(veg_summaries_new,(quad_analysis5(urep_clean,veg_master22_clean)))

urep_clean<-urep_clean %>% 
  select(1:16)



urr1_clean <- clean_names(urr1) %>% 
  mutate(species_ground = tolower(species_ground),
         ave_cover = (rowSums(.[6:15],na.rm = TRUE)/10))

veg_summaries_new <- rbind(veg_summaries_new,(quad_analysis5(urr1_clean,veg_master22_clean)))


ursp1_clean <- clean_names(ursp1) %>% 
  mutate(species_ground = tolower(species_ground),
         ave_cover = (rowSums(.[6:15],na.rm = TRUE)/10))

veg_summaries_new <- rbind(veg_summaries_new,(quad_analysis5(ursp1_clean,veg_master22_clean)))

ursp1_clean<-ursp1_clean %>% 
  select(1:16)



ype_clean <- clean_names(ype) %>% 
  mutate(species_ground = tolower(species_ground),
         ave_cover = (rowSums(.[6:15],na.rm = TRUE)/10))

veg_summaries_new <- rbind(veg_summaries_new,(quad_analysis5(ype_clean,veg_master22_clean)))


ypw_clean <- clean_names(ypw) %>% 
  mutate(species_ground = tolower(species_ground),
         ave_cover = (rowSums(.[6:15],na.rm = TRUE)/10))

veg_summaries_new <- rbind(veg_summaries_new,(quad_analysis5(ypw_clean,veg_master22_clean)))


yps_clean <- clean_names(yps) %>% 
  mutate(species_ground = tolower(species_ground),
         ave_cover = (rowSums(.[6:15],na.rm = TRUE)/10),
         transect_id=ifelse(transect_id=="YPW","YPS",transect_id))

veg_summaries_new <- rbind(veg_summaries_new,(quad_analysis5(yps_clean,veg_master22_clean)))

yps_clean<-yps_clean %>% 
  select(1:16)



nem2_clean <- clean_names(nem2) %>% 
  mutate(species_ground = tolower(species_ground),
         ave_cover = (rowSums(.[6:15],na.rm = TRUE)/10))

veg_summaries_new <- rbind(veg_summaries_new,(quad_analysis5(nem2_clean,veg_master22_clean)))


phm_clean <- clean_names(phm) %>% 
  mutate(species_ground = tolower(species_ground),
         ave_cover = (rowSums(.[6:15],na.rm = TRUE)/10))

veg_summaries_new <- rbind(veg_summaries_new,(quad_analysis5(phm_clean,veg_master22_clean)))


prs1_clean <- clean_names(prs1) %>% 
  mutate(species_ground = tolower(species_ground),
         ave_cover = (rowSums(.[6:15],na.rm = TRUE)/10))

veg_summaries_new <- rbind(veg_summaries_new,(quad_analysis5(prs1_clean,veg_master22_clean)))

prs1_clean<-prs1_clean %>% 
  select(1:16)


spi2_clean <- clean_names(spi2) %>% 
  mutate(species_ground = tolower(species_ground),
         ave_cover = (rowSums(.[6:15],na.rm = TRUE)/10))

veg_summaries_new <- rbind(veg_summaries_new,(quad_analysis5(spi2_clean,veg_master22_clean)))


urr2_clean <- clean_names(urr2) %>% 
  mutate(species_ground = tolower(species_ground),
         ave_cover = (rowSums(.[6:15],na.rm = TRUE)/10),
         transect_id="URR2")

veg_summaries_new <- rbind(veg_summaries_new,(quad_analysis5(urr2_clean,veg_master22_clean)))


ursp2_clean <- clean_names(ursp2) %>% 
  mutate(species_ground = tolower(species_ground),
         ave_cover = (rowSums(.[6:15],na.rm = TRUE)/10))

veg_summaries_new <- rbind(veg_summaries_new,(quad_analysis5(ursp2_clean,veg_master22_clean)))


wrp1_clean <- clean_names(wrp1) %>% 
  mutate(species_ground = tolower(species_ground),
         ave_cover = (rowSums(.[6:15],na.rm = TRUE)/10))

veg_summaries_new <- rbind(veg_summaries_new,(quad_analysis5(wrp1_clean,veg_master22_clean)))


wrpw_clean <- clean_names(wrpw) %>% 
  mutate(species_ground = tolower(species_ground),
         ave_cover = (rowSums(.[6:15],na.rm = TRUE)/10))

veg_summaries_new <- rbind(veg_summaries_new,(quad_analysis5(wrpw_clean,veg_master22_clean)))


mor3_clean <- clean_names(mor3) %>% 
  mutate(species_ground = tolower(species_ground),
         ave_cover = (rowSums(.[6:15],na.rm = TRUE)/10))

veg_summaries_new <- rbind(veg_summaries_new,(quad_analysis5(mor3_clean,veg_master22_clean)))

nwm3_clean <- clean_names(nwm3) %>% 
  mutate(species_ground = tolower(species_ground),
         ave_cover = (rowSums(.[6:15],na.rm = TRUE)/10))

veg_summaries_new <- rbind(veg_summaries_new,(quad_analysis5(nwm3_clean,veg_master22_clean)))



###############################################################Previously veg scores

hh1_clean <- clean_names(hh1) %>% 
  mutate(species_ground=tolower(species_ground),
         ave_cover = (rowSums(.[6:15],na.rm = TRUE)/10))

veg_summaries_new <- rbind(veg_summaries_new,(quad_analysis5(hh1_clean,veg_master22_clean)))


hh2_clean <- clean_names(hh2) %>% 
  mutate(species_ground=tolower(species_ground),
         ave_cover = (rowSums(.[6:15],na.rm = TRUE)/10))

veg_summaries_new <- rbind(veg_summaries_new,(quad_analysis5(hh2_clean,veg_master22_clean)))


hm1_clean <- clean_names(hm1) %>% 
  mutate(species_ground=tolower(species_ground),
         ave_cover = (rowSums(.[6:15],na.rm = TRUE)/10))

veg_summaries_new <- rbind(veg_summaries_new,(quad_analysis5(hm1_clean,veg_master22_clean)))


hm1_clean <- hm1_clean %>%
  mutate(ave_cover=avg_cover) %>% 
  select(-avg_cover)




m1_clean <- clean_names(m1) %>% 
  mutate(species_ground=tolower(species_ground),
         ave_cover = (rowSums(.[6:15],na.rm = TRUE)/10))

veg_summaries_new <- rbind(veg_summaries_new,(quad_analysis5(m1_clean,veg_master22_clean)))


mm1_clean <- clean_names(mm1) %>% 
  mutate(species_ground=tolower(species_ground),
         ave_cover = (rowSums(.[6:15],na.rm = TRUE)/10))

veg_summaries_new <- rbind(veg_summaries_new,(quad_analysis5(mm1_clean,veg_master22_clean)))


mor1_clean <- clean_names(mor1) %>% 
  mutate(species_ground=tolower(species_ground),
         ave_cover = (rowSums(.[6:15],na.rm = TRUE)/10))

veg_summaries_new <- rbind(veg_summaries_new,(quad_analysis5(mor1_clean,veg_master22_clean)))


mr1_clean <- clean_names(mr1) %>% 
  mutate(species_ground=tolower(species_ground),
         ave_cover = (rowSums(.[6:15],na.rm = TRUE)/10))

veg_summaries_new <- rbind(veg_summaries_new,(quad_analysis5(mr1_clean,veg_master22_clean)))


n1_clean <- clean_names(n1) %>% 
  mutate(species_ground=tolower(species_ground),
         ave_cover = (rowSums(.[6:15],na.rm = TRUE)/10))

veg_summaries_new <- rbind(veg_summaries_new,(quad_analysis5(n1_clean,veg_master22_clean)))


nem1_clean <- clean_names(nem1) %>% 
  mutate(species_ground=tolower(species_ground),
         ave_cover = (rowSums(.[6:15],na.rm = TRUE)/10))

veg_summaries_new <- rbind(veg_summaries_new,(quad_analysis5(nem1_clean,veg_master22_clean)))


nem3_clean <- clean_names(nem3) %>% 
  mutate(species_ground = tolower(species_ground),
         ave_cover = (rowSums(.[6:15],na.rm = TRUE)/10))

veg_summaries_new <- rbind(veg_summaries_new,(quad_analysis5(nem3_clean,veg_master22_clean)))


nm2_clean <- clean_names(nm2) %>% 
  mutate(species_ground=tolower(species_ground),
         ave_cover = (rowSums(.[6:15],na.rm = TRUE)/10))

veg_summaries_new <- rbind(veg_summaries_new,(quad_analysis5(nm2_clean,veg_master22_clean)))


nwm1_clean <- clean_names(nwm1) %>% 
  mutate(species_ground=tolower(species_ground),
         ave_cover = (rowSums(.[6:15],na.rm = TRUE)/10))

veg_summaries_new <- rbind(veg_summaries_new,(quad_analysis5(nwm1_clean,veg_master22_clean)))


nwm2_clean <- clean_names(nwm2) %>% 
  mutate(species_ground=tolower(species_ground),
         ave_cover = (rowSums(.[6:15],na.rm = TRUE)/10))

veg_summaries_new <- rbind(veg_summaries_new,(quad_analysis5(nwm2_clean,veg_master22_clean)))


nwm4_clean <- clean_names(nwm4) %>% 
  mutate(species_ground=tolower(species_ground),
         ave_cover = (rowSums(.[6:15],na.rm = TRUE)/10),
         transect_id="NWM4")

veg_summaries_new <- rbind(veg_summaries_new,(quad_analysis5(nwm4_clean,veg_master22_clean)))


nwm5_clean <- clean_names(nwm5) %>% 
  mutate(species_ground=tolower(species_ground),
         ave_cover = (rowSums(.[6:15],na.rm = TRUE)/10))

veg_summaries_new <- rbind(veg_summaries_new,(quad_analysis5(nwm5_clean,veg_master22_clean)))


op1_clean <- clean_names(op1) %>% 
  mutate(species_ground=tolower(species_ground),
         ave_cover = (rowSums(.[6:15],na.rm = TRUE)/10))

veg_summaries_new <- rbind(veg_summaries_new,(quad_analysis5(op1_clean,veg_master22_clean)))


pd1_clean <- clean_names(pd1) %>% 
  mutate(species_ground=tolower(species_ground),
         ave_cover = (rowSums(.[6:15],na.rm = TRUE)/10))

veg_summaries_new <- rbind(veg_summaries_new,(quad_analysis5(pd1_clean,veg_master22_clean)))


r5_clean <- clean_names(r5) %>% 
  mutate(species_ground=tolower(species_ground),
         ave_cover = (rowSums(.[6:15],na.rm = TRUE)/10))

veg_summaries_new <- rbind(veg_summaries_new,(quad_analysis5(r5_clean,veg_master22_clean)))


sem3_clean <- clean_names(sem3) %>% 
  mutate(species_ground=tolower(species_ground),
         ave_cover = (rowSums(.[6:15],na.rm = TRUE)/10))

veg_summaries_new <- rbind(veg_summaries_new,(quad_analysis5(sem3_clean,veg_master22_clean)))


si1_clean <- clean_names(si1) %>% 
  mutate(species_ground=tolower(species_ground),
         ave_cover = (rowSums(.[6:15],na.rm = TRUE)/10))

veg_summaries_new <- rbind(veg_summaries_new,(quad_analysis5(si1_clean,veg_master22_clean)))


sm1_clean <- clean_names(sm1) %>% 
  mutate(species_ground=tolower(species_ground),
         ave_cover = (rowSums(.[6:15],na.rm = TRUE)/10))

veg_summaries_new <- rbind(veg_summaries_new,(quad_analysis5(sm1_clean,veg_master22_clean)))


tnc1_clean <- clean_names(tnc1) %>% 
  mutate(species_ground=tolower(species_ground),
         ave_cover = (rowSums(.[6:15],na.rm = TRUE)/10))

veg_summaries_new <- rbind(veg_summaries_new,(quad_analysis5(tnc1_clean,veg_master22_clean)))


wm2_clean <- clean_names(wm2) %>% 
  mutate(species_ground=tolower(species_ground),
         ave_cover = (rowSums(.[6:15],na.rm = TRUE)/10))

veg_summaries_new <- rbind(veg_summaries_new,(quad_analysis5(wm2_clean,veg_master22_clean)))


wm4_clean <- clean_names(wm4) %>% 
  mutate(species_ground=tolower(species_ground),
         ave_cover = (rowSums(.[6:15],na.rm = TRUE)/10))

veg_summaries_new <- rbind(veg_summaries_new,(quad_analysis5(wm4_clean,veg_master22_clean)))


wr1_clean <- clean_names(wr1) %>% 
  mutate(species_ground=tolower(species_ground),
         ave_cover = (rowSums(.[6:15],na.rm = TRUE)/10))

veg_summaries_new <- rbind(veg_summaries_new,(quad_analysis5(wr1_clean,veg_master22_clean)))

wr1_clean<-wr1_clean %>% 
  select(1:16)



veg_summaries_new <- veg_summaries_new %>% 
  mutate(loc=ifelse(loc=="wrp1","wrpe",loc),
         loc=ifelse(loc=="si1","si",loc))


write.csv(veg_summaries_new, "veg_summaries_new.csv", row.names = FALSE)



  
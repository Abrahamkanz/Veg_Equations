
veg_quads_bind <- af2_clean %>% 
  rbind(af3_clean,bin1_clean,bin2_clean,bin3_clean,bin4_clean,bin5_clean,bin6_clean,
        bs1_clean,cav1_clean,cav2_clean,cav3_clean,cwr1_clean,cwr2_clean,de1_clean,
        de2_clean,dw1_clean,ep2_clean,hh1_clean,hh2_clean,hm1_clean,ht1_clean,
        jt1_clean,jt2_clean,m1_clean,mm1_clean,mor1_clean,mor3_clean,mr1_clean,
        mt1_clean,mt2_clean,n1_clean,nem1_clean,nem2_clean,nem3_clean,nm2_clean,
        npe_clean,npw_clean,nwm1_clean,nwm2_clean,nwm3_clean,nwm4_clean,nwm5_clean,
        op1_clean,op2_clean,pd1_clean,pd2_clean,ph_clean,phm_clean,pr1_clean,prs1_clean,
        r3_clean,r5_clean,sem3_clean,sem4_clean,si1_clean,sm1_clean,spi1_clean,
        spi2_clean,sw1_clean,swm1_clean,swm2_clean,tnc1_clean,urep_clean,urr1_clean,
        urr2_clean,ursp1_clean,ursp2_clean,vor_clean,vou_clean,wm1_clean,wm2_clean,
        wm4_clean,wr1_clean,wrp1_clean,wrpw_clean,ype_clean,ypw_clean,yps_clean) %>% 
  select(transect_id,species_ground,ave_cover) %>%
  mutate(species_ground=tolower(species_ground),
         species_ground=ifelse(species_ground=="asclepias sp.","asclepias sp",species_ground),
         species_ground=ifelse(species_ground=="carex sp.","carex sp",species_ground),
         species_ground=ifelse(species_ground=="carex spp.","carex sp",species_ground),
         species_ground=ifelse(species_ground=="carex spp","carex sp",species_ground),
         species_ground=ifelse(species_ground=="eleocharis sp.","eleocharis sp",species_ground),
         species_ground=ifelse(species_ground=="eleocharis spp","eleocharis sp",species_ground),
         species_ground=ifelse(species_ground=="eleocharis spp.","eleocharis sp",species_ground),
         species_ground=ifelse(species_ground=="ele sp.","eleocharis sp",species_ground),
         species_ground=ifelse(species_ground=="cargran","cargra",species_ground),
         species_ground=ifelse(species_ground=="plantago sp.","plantago sp",species_ground),
         species_ground=ifelse(species_ground=="poa spp.","poa sp.",species_ground),
         species_ground=ifelse(species_ground=="potentilla sp.","potentilla sp",species_ground)) %>% 
  spread(key = species_ground,value = ave_cover)

veg_quads_bind[is.na(veg_quads_bind)] <- 0


write.csv(veg_quads_bind, "veg_quads_bind.csv", row.names = FALSE)


















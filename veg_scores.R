
#Calculating scores using output of quad analysis function
#Table to bind values in.

veg_scores_table <- setNames(data.frame(matrix(ncol = 12, nrow = 0)), c(colnames(yps_scores)))

# Used to quickly get column names
# yps_scores <- YPS_Rowe_07212021_rev %>%
#  mutate(c_mod = as.numeric(c)) %>%
#  mutate_at(33, ~replace_na(., 0)) %>%
#  summarise(sum_exotics = sum(native_exotic_adv_n_e_a == "E"),
#            sum_natives = sum(native_exotic_adv_n_e_a == "N"),
#            sum_species = sum(sum_exotics,sum_natives),
#            sum_pi = sum(pi),
#            sum_pi2 = sum(pi2),
#            sdi = sum_pi-sum_pi2,
#            wis = mean(wis),
#            sum_cover = sum(ave_cover[species_ground != "LD"& species_ground != "BG"]),
#            wis_sum = sum_cover - sum(ave_cover[species_ground == "NA"]),
#            fqi = sum(c_mod)/sqrt(sum_natives)) %>%
#  mutate(site = "yps",
#         year = "2021")

# 
# veg_scores_table <- YPS_Rowe_07212021_rev %>%
#   mutate(c_mod = as.numeric(c)) %>%
#   mutate_at(33, ~replace_na(., 0)) %>%
#   summarise(sum_exotics = sum(native_exotic_adv_n_e_a == "E"),
#             sum_natives = sum(native_exotic_adv_n_e_a == "N"),
#             sum_species = sum(sum_exotics,sum_natives),
#             sum_pi = sum(pi),
#             sum_pi2 = sum(pi2),
#             sdi = sum_pi-sum_pi2,
#             wis = mean(wis),
#             sum_cover = sum(ave_cover[species_ground != "LD"& species_ground != "BG"]),
#             wis_sum = sum_cover - sum(ave_cover[species_ground == "NA"]),
#             fqi = sum(c_mod)/sqrt(sum_natives)) %>%
#   mutate(site = "yps",
#          year = "2021") %>%
#   rbind(veg_scores_table)


#Function for calculations
veg_scores_calc<- function(x,y,z){
 z %>%
    mutate(c_mod = as.numeric(c)) %>%
    mutate_if(is.numeric, ~replace_na(., 0))  %>%
    summarise(sum_exotics = sum(native_exotic_adv_n_e_a == "E"),
              sum_natives = sum(native_exotic_adv_n_e_a == "N"),
              sum_species = sum(sum_exotics,sum_natives),
              sum_pi = sum(pi),
              sum_pi2 = sum(pi2),
              sdi = sum_pi-sum_pi2,
              wis = mean(wis),
              sum_cover = sum(ave_cover[species_ground != "LD"& species_ground != "BG"]),
              wis_sum = sum_cover - sum(ave_cover[species_ground == "NA"]),
              fqi = sum(c_mod)/sqrt(sum_natives)) %>%
    mutate(site = x,
           year = y) %>%
    rbind(veg_scores_table)
}


veg_scores_table <- veg_scores_calc("yps",2021,YPS_Rowe_07212021_rev)

veg_scores_table <- veg_scores_calc("af2",2021,AF2_VMDB_8_5_2021_output_rev)




veg_scores_calc2<- function(x,y,z){
  z %>%
    mutate(c_mod = as.numeric(c)) %>%
    mutate_if(is.numeric, ~replace_na(., 0))  %>%
    summarise(sum_exotics = sum(native_exotic_adv_n_e_a == "E"),
              sum_natives = sum(native_exotic_adv_n_e_a == "N"),
              sum_species = sum(sum_exotics,sum_natives),
              sum_pi = sum(pi),
              sum_pi2 = sum(pi2),
              sdi = sum_pi-sum_pi2,
              wis = mean(wis),
              sum_cover = sum(ave_cover[species_ground != "LD"& species_ground != "BG"]),
              wis_sum = sum_cover - sum(ave_cover[species_ground == "NA"]),
              fqi = sum(c_mod)/sqrt(sum_natives)) %>%
    mutate(site = x,
           year = y) %>%
    rbind(veg_scores_table)
}










library(tidyverse)
library(janitor)

#This process will not work perfectly due to 
#differences between the Crane Trust and NNHP lists. I modified the NNHP list to present
#genus/species in the same way as the crane trust, but I was not sure what to do in situations 
#where the scientific name included "var.____" which means that those species will still need 
#data entered by hand. In the Crane Trust document, I removed extra abbreviations in the "code"
#column of the species page (i.e. "AGRSTO (AGROSTO)" became "AGRSTO"). I noticed in the island
#comparison page that variants of the same species are included. If I knew more about which row is 
#used in those situations, I could further improve the efficiency of this process. 

#Put data into one table and select the relevant columns. 

NNHP_state_list_2013_by_family_mod2 <- NNHP_state_list_2013_by_family_mod %>% 
  mutate(P = `P-symbol`) %>% 
  select("C","Wetness","Grank","Srank","P",Genus_species)

veg_master <- Vegetation_MASTER_SpeciesList_CraneTrust_ACJW_Current_08262019_mod %>% 
  left_join(island_comp, by = "Code") %>%
  select("Code", "Wetland. Ind.", "Native/Exotic/Adv. (N/E/A)","growth_hab","Genus_species") %>% 
  left_join(lists_mismatch) %>% 
  mutate(Genus_species = ifelse(is.na(`Scientific Name`),Genus_species,`Scientific Name`)) %>% 
  select("Code", "Wetland. Ind.", "Native/Exotic/Adv. (N/E/A)","growth_hab","Genus_species") %>% 
  left_join(NNHP_state_list_2013_by_family_mod2, by = "Genus_species") %>% 
  select("Code", "Wetland. Ind.", "Native/Exotic/Adv. (N/E/A)", "C", "growth_hab") %>% 
  left_join(growth_hab_table, by = "growth_hab") %>% 
  left_join(wis_unit_conv, by = "Wetland. Ind.") %>% 
  mutate("Species/Ground" = Code) %>%
  select(-"Code", -"Wetland. Ind.", -"growth_hab")

#"Cleaning" column names since spaces and other punctuation were giving me grief in the coding process. 

veg_master_clean <- clean_names(veg_master)

veg_master_clean %>% 
  merge()

HM1_VMDB_6_23_2020_mod_clean <- clean_names(HM1_VMDB_6_23_2020_mod)

#Combine cleaned data and calculate variables

test2 <- HM1_VMDB_6_23_2020_mod_clean %>% 
  left_join(test_clean, by = "species_ground") %>% 
  mutate(c = na_if(c, "*")) %>% 
  mutate_if(is.numeric, ~replace_na(., 0))%>% 
  mutate(c = as.numeric(c)) %>% 
  mutate(zero = 0)%>% 
  mutate(percent_G = case_when(growth_hab_abbr == "G" ~ avg_cover, 
                          TRUE ~ zero),
         percent_F = case_when(growth_hab_abbr == "F" ~ avg_cover, 
                               TRUE ~ zero),
         percent_S = case_when(growth_hab_abbr == "S" ~ avg_cover, 
                               TRUE ~ zero),
         percent_T = case_when(growth_hab_abbr == "T" ~ avg_cover, 
                               TRUE ~ zero),
         percent_E = case_when(native_exotic_adv_n_e_a == "E" ~ avg_cover, 
                               TRUE ~ zero),
         pi = avg_cover/(sum(avg_cover)),
         pi2 = pi^2,
         wwis = wis*avg_cover,
         wc = c*avg_cover) %>% 
  write.csv(file = "test2.csv", row.names = F)




quad_analysis <- function(x, y){
  x %>% 
    left_join(y, by = "species_ground") %>% 
    mutate(c = na_if(c, "*")) %>% 
    replace(is.na(.), 0) %>% 
    mutate(c = as.numeric(c)) %>% 
    mutate(zero = 0)%>% 
    mutate(percent_G = case_when(growth_hab_abbr == "G" ~ avg_cover, 
                                 TRUE ~ zero),
           percent_F = case_when(growth_hab_abbr == "F" ~ avg_cover, 
                                 TRUE ~ zero),
           percent_S = case_when(growth_hab_abbr == "S" ~ avg_cover, 
                                 TRUE ~ zero),
           percent_T = case_when(growth_hab_abbr == "T" ~ avg_cover, 
                                 TRUE ~ zero),
           percent_E = case_when(native_exotic_adv_n_e_a == "E" ~ avg_cover, 
                                 TRUE ~ zero),
           pi = avg_cover/(sum(avg_cover)),
           pi2 = pi^2,
           wwis = wis*avg_cover,
           wc = c*avg_cover) %>% 
    write.csv(file = "output.csv", row.names = F) 
}

#At this point, all we should need to do for future is import the data, clean it, and run the function.
#This process will only require a name replacement within the code below. 

HM1_VMDB_6_23_2020_mod_clean <- clean_names(HM1_VMDB_6_23_2020_mod)

test_clean <- clean_names(test)

quad_analysis(HM1_VMDB_6_23_2020_mod_clean, test_clean)

#You will have to change the name of the file wherever it is saved because I don't know how to get 
#the "output.csv" to change with the name of the input. 

quad_analysis2 <- function(x, y){
  x %>% 
    left_join(y, by = "species_ground") %>% 
    mutate(c = na_if(c, "*")) %>% 
    replace(is.na(.), 0) %>% 
    mutate(c = as.numeric(c)) %>% 
    mutate(zero = 0)%>% 
    mutate(percent_G = case_when(growth_hab_abbr == "G" ~ ave_cover, 
                                 TRUE ~ zero),
           percent_F = case_when(growth_hab_abbr == "F" ~ ave_cover, 
                                 TRUE ~ zero),
           percent_S = case_when(growth_hab_abbr == "S" ~ ave_cover, 
                                 TRUE ~ zero),
           percent_T = case_when(growth_hab_abbr == "T" ~ ave_cover, 
                                 TRUE ~ zero),
           percent_E = case_when(native_exotic_adv_n_e_a == "E" ~ ave_cover, 
                                 TRUE ~ zero),
           pi = ave_cover/(sum(ave_cover)),
           pi2 = pi^2,
           wwis = wis*ave_cover,
           wc = c*ave_cover) %>% 
    write.csv(file = "output.csv", row.names = F) 
}

M1_VMDB_7_21_2020_clean <- clean_names(M1_VMDB_7_21_2020)

quad_analysis2(M1_VMDB_7_21_2020_clean, test_clean)


MOR1_VMDB_6_29_2020_clean <- clean_names(MOR1_VMDB_6_29_2020)

quad_analysis2(MOR1_VMDB_6_29_2020_clean, test_clean)


NEM2_VMDB_8_5_2020_clean <- clean_names(NEM2_VMDB_8_5_2020)

quad_analysis2(NEM2_VMDB_8_5_2020_clean, test_clean)


NEM3_VMDB_8_10_2020_clean <- clean_names(NEM3_VMDB_8_10_2020)

quad_analysis2(NEM3_VMDB_8_10_2020_clean, test_clean)



NWM2_VMDB_7_16_2019_clean <- clean_names(NWM2_VMDB_7_16_2019)

quad_analysis2(NWM2_VMDB_7_16_2019_clean, test_clean)



NWM5_VMDB_6_17_2020_clean <- clean_names(NWM5_VMDB_6_17_2020)

quad_analysis2(NWM5_VMDB_6_17_2020_clean, test_clean)



OP2_VMDB_7_24_2020_clean <- clean_names(OP2_VMDB_7_24_2020)

quad_analysis2(OP2_VMDB_7_24_2020_clean, test_clean)


#Didn't run correctly due to different naming convention (use of lowercase). Excel sheet modified.
PHM_Rowe_07112019_mod_clean <- clean_names(PHM_Rowe_07112019_mod)

quad_analysis2(PHM_Rowe_07112019_mod_clean, test_clean)



PRS1_Rowe_07222020_clean <- clean_names(PRS1_Rowe_07222020)

quad_analysis2(PRS1_Rowe_07222020_clean, test_clean)


SM1_VMDB_7_26_2019_clean <- clean_names(SM1_VMDB_7_26_2019)

quad_analysis2(SM1_VMDB_7_26_2019_clean, test_clean)



SPI2_PRRIP_08062020_mod_clean <- clean_names(SPI2_PRRIP_08062020_mod)

quad_analysis2(SPI2_PRRIP_08062020_mod_clean, test_clean)



URR2_TNC_08102020_mod_clean <- clean_names(URR2_TNC_08102020_mod)

quad_analysis2(URR2_TNC_08102020_mod_clean, test_clean)



URSP2_TNC_07292020_mod_clean <- clean_names(URSP2_TNC_07292020_mod)

quad_analysis2(URSP2_TNC_07292020_mod_clean, test_clean)



WRP1_Rowe_07112019_clean <- clean_names(WRP1_Rowe_07112019)

quad_analysis2(WRP1_Rowe_07112019_clean, test_clean)



WRPW_Rowe_07152020_mod_clean <- clean_names(WRPW_Rowe_07152020_mod)

quad_analysis2(WRPW_Rowe_07152020_mod_clean, test_clean)



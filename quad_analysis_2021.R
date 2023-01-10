
#2021 Quad Analysis

#Quad analyses 1 and 2 no longer function as they attempt to place 0's into character data.
#My best guess is that one of the functions updated and caused errors. 

# quad_analysis3 <- function(x, y){
#   x %>% 
#     left_join(y, by = "species_ground") %>% 
#     mutate(c = na_if(c, "*")) %>% 
#     mutate_if(is.numeric, ~replace_na(., 0)) %>% 
#     mutate(c = as.numeric(c)) %>% 
#     mutate(zero = 0)%>% 
#     mutate(percent_G = case_when(growth_hab_abbr == "G" ~ ave_cover, 
#                                  TRUE ~ zero),
#            percent_F = case_when(growth_hab_abbr == "F" ~ ave_cover, 
#                                  TRUE ~ zero),
#            percent_S = case_when(growth_hab_abbr == "S" ~ ave_cover, 
#                                  TRUE ~ zero),
#            percent_T = case_when(growth_hab_abbr == "T" ~ ave_cover, 
#                                  TRUE ~ zero),
#            percent_E = case_when(native_exotic_adv_n_e_a == "E" ~ ave_cover, 
#                                  TRUE ~ zero),
#            wwis = wis*ave_cover,
#            wc = c*ave_cover) %>% 
#     write.csv(file = "output.csv", row.names = F) 
# }


#AF2_VMDB_8_5_2021_mod_clean <- clean_names(AF2_VMDB_8_5_2021_mod)
#Mod actually not required after fixing function. 

AF2_VMDB_8_5_2021_clean <- clean_names(AF2_VMDB_8_5_2021)

quad_analysis3(AF2_VMDB_8_5_2021_clean, test_clean)

#BIN1 already complete?
BIN1_PRRIP_06302020_clean <- clean_names(BIN1_PRRIP_06302020)

quad_analysis3(BIN1_PRRIP_06302020_clean, test_clean)


#Parts of BIN3 are missing, not sure why
BIN3_PRRIP_07062020_mod_clean <- clean_names(BIN3_PRRIP_07062020_mod)

quad_analysis3(BIN3_PRRIP_07062020_mod_clean, test_clean)


BIN5_PRRIP_07142021_clean <- clean_names(BIN5_PRRIP_07142021)

quad_analysis3(BIN5_PRRIP_07142021_clean, test_clean)


BIN6_PRIP_06302021_mod_clean <- clean_names(BIN6_PRIP_06302021_mod)

quad_analysis3(BIN6_PRIP_06302021_mod_clean, test_clean)


CAV2_TNC_06232021_clean <- clean_names(CAV2_TNC_06232021)

quad_analysis3(CAV2_TNC_06232021_clean, test_clean)


DW1_PRIP_06162021_mod_clean <- clean_names(DW1_PRIP_06162021_mod)

quad_analysis3(DW1_PRIP_06162021_mod_clean, test_clean)


EP2_VMDB_7_19_2021_clean <- clean_names(EP2_VMDB_7_19_2021)

quad_analysis3(EP2_VMDB_7_19_2021_clean, test_clean)


HH1_VMDB_7_27_2021_clean <- clean_names(HH1_VMDB_7_27_2021)

quad_analysis3(HH1_VMDB_7_27_2021_clean, test_clean)


HH2_VMDB_07_1_2021_clean <- clean_names(HH2_VMDB_07_1_2021)

quad_analysis3(HH2_VMDB_07_1_2021_clean, test_clean)


HH3_VMDB_7_29_2020_clean <- clean_names(HH3_VMDB_7_29_2020)

quad_analysis3(HH3_VMDB_7_29_2020_clean, test_clean)


HT1_PRRIP_08092021_clean <- clean_names(HT1_PRRIP_08092021)

quad_analysis3(HT1_PRRIP_08092021_clean, test_clean)


JT1_PRRIP_06092021_clean <- clean_names(JT1_PRRIP_06092021)

quad_analysis3(JT1_PRRIP_06092021_clean, test_clean)


MM1_VMDB_08_04_2021_clean <- clean_names(MM1_VMDB_08_04_2021)

quad_analysis3(MM1_VMDB_08_04_2021_clean, test_clean)


MM3_VMDB_07_26_2021_clean <- clean_names(MM3_VMDB_07_26_2021)

quad_analysis3(MM3_VMDB_07_26_2021_clean, test_clean)


MR1_VMDB_8_17_2021_clean <- clean_names(MR1_VMDB_8_17_2021)

quad_analysis3(MR1_VMDB_8_17_2021_clean, test_clean)


N1_VMDB_8_20_2021_clean <- clean_names(N1_VMDB_8_20_2021)

quad_analysis3(N1_VMDB_8_20_2021_clean, test_clean)


NM1_VMDB_06_23_2021_clean <- clean_names(NM1_VMDB_06_23_2021)

quad_analysis3(NM1_VMDB_06_23_2021_clean, test_clean)


NM2_VMDB_6_22_2021_clean <- clean_names(NM2_VMDB_6_22_2021)

quad_analysis3(NM2_VMDB_6_22_2021_clean, test_clean)


NWM1_VMDB_6_28_2021_clean <- clean_names(NWM1_VMDB_6_28_2021)

quad_analysis3(NWM1_VMDB_6_28_2021_clean, test_clean)



NWM2_VMDB_7_12_2021_clean <- clean_names(NWM2_VMDB_7_12_2021)

quad_analysis3(NWM2_VMDB_7_12_2021_clean, test_clean)


NWM4_VMDB_7_20_2021_clean <- clean_names(NWM4_VMDB_7_20_2021)

quad_analysis3(NWM4_VMDB_7_20_2021_clean, test_clean)


OP1_VMDB_7_30_2021_clean <- clean_names(OP1_VMDB_7_30_2021)

quad_analysis3(OP1_VMDB_7_30_2021_clean, test_clean)


PD1_VMDB_7_29_2021_clean <- clean_names(PD1_VMDB_7_29_2021)

quad_analysis3(PD1_VMDB_7_29_2021_clean, test_clean)



PH_Rowe_06092021_clean <- clean_names(PH_Rowe_06092021)

quad_analysis3(PH_Rowe_06092021_clean, test_clean)


PR1_Rowe_08102021_clean <- clean_names(PR1_Rowe_08102021)

quad_analysis3(PR1_Rowe_08102021_clean, test_clean)


R2_VMDB_8_2_2021_clean <- clean_names(R2_VMDB_8_2_2021)

quad_analysis3(R2_VMDB_8_2_2021_clean, test_clean)


R4_VMDB_7_22_2021_clean <- clean_names(R4_VMDB_7_22_2021)

quad_analysis3(R4_VMDB_7_22_2021_clean, test_clean)


R5_VMDB_7_15_2021_clean <- clean_names(R5_VMDB_7_15_2021)

quad_analysis3(R5_VMDB_7_15_2021_clean, test_clean)


SEM1_VMDB_7_23_2021_clean <- clean_names(SEM1_VMDB_7_23_2021)

quad_analysis3(SEM1_VMDB_7_23_2021_clean, test_clean)


SEM3_VMDB_7_21_2021_clean <- clean_names(SEM3_VMDB_7_21_2021)

quad_analysis3(SEM3_VMDB_7_21_2021_clean, test_clean)


SI1_VMDB_9_01_2021_clean <- clean_names(SI1_VMDB_9_01_2021)

quad_analysis3(SI1_VMDB_9_01_2021_clean, test_clean)


SM1_VMDB_8_2_2021_clean <- clean_names(SM1_VMDB_8_2_2021)

quad_analysis3(SM1_VMDB_8_2_2021_clean, test_clean)


SPI1_PRRIP_07212021_clean <- clean_names(SPI1_PRRIP_07212021)

quad_analysis3(SPI1_PRRIP_07212021_clean, test_clean)



SWM3_VMDB_8_11_2021_clean <- clean_names(SWM3_VMDB_8_11_2021)

quad_analysis3(SWM3_VMDB_8_11_2021_clean, test_clean)


TNC1_VMDB_7_22_2021_clean <- clean_names(TNC1_VMDB_7_22_2021)

quad_analysis3(TNC1_VMDB_7_22_2021_clean, test_clean)


UREP_TNC_07292021_clean <- clean_names(UREP_TNC_07292021)

quad_analysis3(UREP_TNC_07292021_clean, test_clean)


URR1_TNC_05262021_clean <- clean_names(URR1_TNC_05262021)

quad_analysis3(URR1_TNC_05262021_clean, test_clean)


URSP1_TNC_06032021_clean <- clean_names(URSP1_TNC_06032021)

quad_analysis3(URSP1_TNC_06032021_clean, test_clean)


VC1_VMDB_8_4_2021_clean <- clean_names(VC1_VMDB_8_4_2021)

quad_analysis3(VC1_VMDB_8_4_2021_clean, test_clean)


VC2_VMDB_8_3_2021_clean <- clean_names(VC2_VMDB_8_3_2021)

quad_analysis3(VC2_VMDB_8_3_2021_clean, test_clean)


WM2_VMDB_7_8_2021_clean <- clean_names(WM2_VMDB_7_8_2021)

quad_analysis3(WM2_VMDB_7_8_2021_clean, test_clean)


WM4_VMDB_6_16_2021_clean <- clean_names(WM4_VMDB_6_16_2021)

quad_analysis3(WM4_VMDB_6_16_2021_clean, test_clean)


WR1_VMDB_7_7_2021_clean <- clean_names(WR1_VMDB_7_7_2021)

quad_analysis3(WR1_VMDB_7_7_2021_clean, test_clean)


YPE_Rowe_07072021_clean <- clean_names(YPE_Rowe_07072021)

quad_analysis3(YPE_Rowe_07072021_clean, test_clean)


YPS_Rowe_07212021_clean <- clean_names(YPS_Rowe_07212021)

quad_analysis3(YPS_Rowe_07212021_clean, test_clean)


YPW_Rowe_07072021_clean <- clean_names(YPW_Rowe_07072021)

quad_analysis3(YPW_Rowe_07072021_clean, test_clean)



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






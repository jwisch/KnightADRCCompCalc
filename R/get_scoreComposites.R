

#' Takes in dataframe of normed scores
#' Returns Global Composite
#' @param df dataframe containing normed scores
#' @param id_col_name character string of ID column name
#' @param visit_date_col_name character string of visit date column name
#' @param craftdel_col_name character string of craftdel column name
#' @param asscmem_col_name character string of associated memory column name (WMS Associate Learning (summary score)	Num 	0-21	High score = good)
#' @param srtfree_col_name character string of srtfree column name
#' @param animals_col_name character string of animals column name  (CATEGORY FLUENCY - ANIMALS	Num 	0 and above	High score = good)
#' @param mlnt_col_name character string of multilingual naming test column name  (Multilingual Naming Test (MINT) - Total Score	Num 	0-32	High Score = good)
#' @param veg_col_name character string of veg column name (CATEGORY FLUENCY - VEGETABLES	Num 	0 and above	High score = good)
#' @param tma_col_name character string of trails a column name (High score = bad)
#' @param tmb_col_name character string of trails b column name (High score = bad)
#' @param digsym_col_name character string of digit symbol column name (WAIS-R Digit Symbol	Num 	0-93	High score = good)
#' @param switch_col_name character string of switch column name
#' @param digb_col_name character string of digb column name (WMS-R DIGIT SPAN BACKWARD (total number of trials)	Num 	0-12	High score = good)
#' @param digf_col_name character string of digf column name #WMS-R DIGIT SPAN FORWARD (total number of trials)	Num 	0-12	High score = good
#' @param lettnum_col_name character string of lettnum column name #WMS-III letter-Number sequencing	Num 	0-21	High score = good
#' @return dataframe containing participant id, visit date, the global composite score and the number of nas in the visit that went into the score. It is up to user discretion in terms of how many tests to require
#' @export

get_GlobalComposite <- function(df, id_col_name = "ID", visit_date_col_name = "TESTDATE",
                                craftdel_col_name = "z_craftdre", 
                                asscmem_col_name = "z_asscmem", 
                                srtfree_col_name = "z_srtfree",
                                animals_col_name = "z_animals", 
                                mlnt_col_name = "z_mlnt_eq", 
                                veg_col_name = "z_veg",
                                tma_col_name = "z_TrailA_eq", 
                                tmb_col_name = "z_TrailB_eq", 
                                digsym_col_name = "z_digsym",
                                switch_col_name = "z_switchmixed", 
                                digb_col_name = "z_DIGIB_eq", 
                                digf_col_name = "z_DIGIF_eq",
                                lettnum_col_name = "z_lettnum"){
  if (mean(abs(df[, craftdel_col_name]), na.rm= TRUE) > 3 ){
    message("Hey! The values for craftdel look weird. Did you norm them?")
  }
  if (mean(abs(df[, asscmem_col_name]), na.rm= TRUE) > 3 ){
    message("Hey! The values for asscmem look weird. Did you norm them?")
  }
  if (mean(abs(df[, srtfree_col_name]), na.rm= TRUE) > 3 ){
    message("Hey! The values for srtfree look weird. Did you norm them?")
  }
  if (mean(abs(df[, animals_col_name]), na.rm= TRUE) > 3 ){
    message("Hey! The values for animals look weird. Did you norm them?")
  }
  if (mean(abs(df[, mlnt_col_name]), na.rm= TRUE) > 3 ){
    message("Hey! The values for mlnt look weird. Did you norm them?")
  }
  if (mean(abs(df[, veg_col_name]), na.rm= TRUE) > 3 ){
    message("Hey! The values for veg look weird. Did you norm them?")
  }
  if (mean(abs(df[, tma_col_name]), na.rm= TRUE) > 3 ){
    message("Hey! The values for Trails A look weird. Did you norm them?")
  }
  if (mean(abs(df[, tmb_col_name]), na.rm= TRUE) > 3 ){
    message("Hey! The values for Trails B look weird. Did you norm them?")
  }
  if (mean(abs(df[, digsym_col_name]), na.rm= TRUE) > 3 ){
    message("Hey! The values for digsym look weird. Did you norm them?")
  }
  if (mean(abs(df[, digb_col_name]), na.rm= TRUE) > 3 ){
    message("Hey! The values for digb look weird. Did you norm them?")
  }
  if (mean(abs(df[, switch_col_name]), na.rm= TRUE) > 3 ){
    message("Hey! The values for switch look weird. Did you norm them?")
  }
  if (mean(abs(df[, digf_col_name]), na.rm= TRUE) > 3 ){
    message("Hey! The values for digf look weird. Did you norm them?")
  }
  if (mean(abs(df[, lettnum_col_name]), na.rm= TRUE) > 3 ){
    message("Hey! The values for lettnum look weird. Did you norm them?")
  }

  Global_composite <- rowMeans(df[, c(craftdel_col_name, asscmem_col_name, srtfree_col_name,
                                           animals_col_name, mlnt_col_name, veg_col_name,
                                           tma_col_name, tmb_col_name, digsym_col_name,
                                           switch_col_name, digb_col_name, digf_col_name,
                                           lettnum_col_name)], na.rm = TRUE)  
  NA_count <- rowSums(is.na(df[, c(craftdel_col_name, asscmem_col_name, srtfree_col_name,
                                   animals_col_name, mlnt_col_name, veg_col_name,
                                   tma_col_name, tmb_col_name, digsym_col_name,
                                   switch_col_name, digb_col_name, digf_col_name,
                                   lettnum_col_name)]))
  return(data.frame(id_col_name = as.vector(df[, id_col_name]),
                    visit_date_col_name = as.vector(df[, visit_date_col_name]),
                    "Composite_Global" = Global_composite,
                    "NA_in_Composite_Global" = NA_count))
}


#' Takes in dataframe of normed scores
#' Returns Episodic Memory Composite
#' @param df dataframe containing normed scores
#' @param id_col_name character string of ID column name
#' @param visit_date_col_name character string of visit date column name
#' @param craftdel_col_name character string of craftdel column name
#' @param asscmem_col_name character string of associated memory column name
#' @param srtfree_col_name character string of srtfree column name
#' @return dataframe containing participant id, visit date, the episodic memory composite score and the number of nas in the visit that went into the score. It is up to user discretion in terms of how many tests to require
#' @export


get_EpisodicMemoryComposite <- function(df, id_col_name = "ID", visit_date_col_name = "TESTDATE",
                                craftdel_col_name = "z_craftdre", asscmem_col_name = "z_asscmem", 
                                srtfree_col_name = "z_srtfree"){
  if (mean(abs(df[, craftdel_col_name]), na.rm= TRUE) > 3 ){
    message("Hey! The values for craftdel look weird. Did you norm them?")
  }
  if (mean(abs(df[, asscmem_col_name]), na.rm= TRUE) > 3 ){
    message("Hey! The values for asscmem look weird. Did you norm them?")
  }
  if (mean(abs(df[, srtfree_col_name]), na.rm= TRUE) > 3 ){
    message("Hey! The values for srtfree look weird. Did you norm them?")
  }

  EM_composite <- rowMeans(df[, c(craftdel_col_name, asscmem_col_name, srtfree_col_name)], na.rm = TRUE)  
  NA_count <- rowSums(is.na(df[, c(craftdel_col_name, asscmem_col_name, srtfree_col_name)]))
  return(data.frame(id_col_name = as.vector(df[, id_col_name]),
                    visit_date_col_name = as.vector(df[, visit_date_col_name]),
                    "Composite_EpisodicMemory" = EM_composite,
                    "NA_in_Composite_EM" = NA_count))
}



#' Takes in dataframe of normed scores
#' Returns Semantic Memory
#' index_animals,index_mint,index_veg
#' @param df dataframe containing normed scores
#' @param id_col_name character string of ID column name
#' @param visit_date_col_name character string of visit date column name
#' @param animals_col_name character string of animals column name
#' @param mlnt_col_name character string of multilingual naming test column name
#' @param veg_col_name character string of veg column name
#' @return dataframe containing participant id, visit date, the semantic memory composite score and the number of nas in the visit that went into the score. It is up to user discretion in terms of how many tests to require
#' @export


get_SemanticMemoryComposite <- function(df, id_col_name = "ID", visit_date_col_name = "TESTDATE",
                                animals_col_name = "z_animals", mlnt_col_name = "z_mlnt_eq", 
                                veg_col_name  = "z_veg"){
  if (mean(abs(df[, animals_col_name]), na.rm= TRUE) > 3 ){
    message("Hey! The values for animals look weird. Did you norm them?")
  }
  if (mean(abs(df[, mlnt_col_name]), na.rm= TRUE) > 3 ){
    message("Hey! The values for mlnt look weird. Did you norm them?")
  }
  if (mean(abs(df[, veg_col_name]), na.rm= TRUE) > 3 ){
    message("Hey! The values for veg look weird. Did you norm them?")
  }
  SemanticMemory_composite <- rowMeans(df[, c(animals_col_name, mlnt_col_name, veg_col_name)], na.rm = TRUE)  
  NA_count <- rowSums(is.na(df[, c(animals_col_name, mlnt_col_name, veg_col_name)]))
  return(data.frame(id_col_name = as.vector(df[, id_col_name]),
                    visit_date_col_name = as.vector(df[, visit_date_col_name]),
                    "Composite_SemanticMemory" = SemanticMemory_composite,
                    "NA_in_Composite_SM" = NA_count))
}




#' Takes in dataframe of normed scores
#' Returns Attention Processing Composite
#' index_tma,index_tmb,index_digsym, index_switch
#' @param df dataframe containing normed scores
#' @param id_col_name character string of ID column name
#' @param visit_date_col_name character string of visit date column name
#' @param tma_col_name character string of trails a column name
#' @param tmb_col_name character string of trails b column name
#' @param digsym_col_name character string of digit symbol column name
#' @param switch_col_name character string of switch column name
#' @return dataframe containing participant id, visit date, the attention processing composite score and the number of nas in the visit that went into the score. It is up to user discretion in terms of how many tests to require
#' @export


get_AttentionComposite <- function(df, id_col_name = "ID", visit_date_col_name = "TESTDATE",
                                   tma_col_name = "z_TrailA_eq", tmb_col_name = "z_TrailB_eq", 
                                   digsym_col_name = "z_digsym",
                                switch_col_name = "z_switchmixed"){
  if (mean(abs(df[, tma_col_name]), na.rm= TRUE) > 3 ){
    message("Hey! The values for Trails A look weird. Did you norm them?")
  }
  if (mean(abs(df[, tmb_col_name]), na.rm= TRUE) > 3 ){
    message("Hey! The values for Trails B look weird. Did you norm them?")
  }
  if (mean(abs(df[, digsym_col_name]), na.rm= TRUE) > 3 ){
    message("Hey! The values for digsym look weird. Did you norm them?")
  }
  if (mean(abs(df[, switch_col_name]), na.rm= TRUE) > 3 ){
    message("Hey! The values for switch look weird. Did you norm them?")
  }
 
  Attention_composite <- rowMeans(df[, c(tma_col_name, tmb_col_name, digsym_col_name,
                                      switch_col_name)], na.rm = TRUE)  
  NA_count <- rowSums(is.na(df[, c(tma_col_name, tmb_col_name, digsym_col_name,
                                   switch_col_name)]))
  return(data.frame(id_col_name = as.vector(df[, id_col_name]),
                    visit_date_col_name = as.vector(df[, visit_date_col_name]),
                    "Composite_Attention" = Attention_composite,
                    "NA_in_Composite_Attention" = NA_count))
}



#' Takes in dataframe of normed scores
#' Returns Working Memory Composite
#' index_digb,index_digf,index_lettnum
#' @param df dataframe containing normed scores
#' @param id_col_name character string of ID column name
#' @param visit_date_col_name character string of visit date column name
#' @param digb_col_name character string of digb column name
#' @param digf_col_name character string of digf column name
#' @param lettnum_col_name character string of lettnum column name
#' @return dataframe containing participant id, visit date, the global composite score and the number of nas in the visit that went into the score. It is up to user discretion in terms of how many tests to require
#' @export

get_WorkingMemoryComposite <- function(df, id_col_name = "ID", visit_date_col_name = "TESTDATE",
                                digb_col_name = "z_DIGIB_eq", digf_col_name = "z_DIGIF_eq",
                                lettnum_col_name = "z_lettnum"){
  if (mean(abs(df[, digb_col_name]), na.rm= TRUE) > 3 ){
    message("Hey! The values for digb look weird. Did you norm them?")
  }

  if (mean(abs(df[, digf_col_name]), na.rm= TRUE) > 3 ){
    message("Hey! The values for digf look weird. Did you norm them?")
  }
  if (mean(abs(df[, lettnum_col_name]), na.rm= TRUE) > 3 ){
    message("Hey! The values for lettnum look weird. Did you norm them?")
  }
  
  WM_composite <- rowMeans(df[, c( digb_col_name, digf_col_name,
                                      lettnum_col_name)], na.rm = TRUE)  
  NA_count <- rowSums(is.na(df[, c(digb_col_name, digf_col_name,
                                   lettnum_col_name)]))
  return(data.frame(id_col_name = as.vector(df[, id_col_name]),
                    visit_date_col_name = as.vector(df[, visit_date_col_name]),
                    "Composite_WorkingMemory" = WM_composite,
                    "NA_in_Composite_Global" = NA_count))
}



#' Takes in dataframe of normed scores
#' Returns Knight PACC
#' index_srtfree,index_animals,index_digsym, index_tmb
#' @param df dataframe containing normed scores
#' @param id_col_name character string of ID column name
#' @param visit_date_col_name character string of visit date column name
#' @param srtfree_col_name character string of srtfree column name
#' @param animals_col_name character string of animals column name
#' @param tmb_col_name character string of trails b column name
#' @param digsym_col_name character string of digit symbol column name
#' @return dataframe containing participant id, visit date, the global composite score and the number of nas in the visit that went into the score. It is up to user discretion in terms of how many tests to require
#' @export

get_KnightPACC <- function(df, id_col_name = "ID", visit_date_col_name = "TESTDATE",
                                srtfree_col_name = "z_srtfree",
                                animals_col_name = "z_animals", tmb_col_name = "z_TrailB_eq",
                               digsym_col_name = "z_digsym"){
  if (mean(abs(df[, srtfree_col_name]), na.rm= TRUE) > 3 ){
    message("Hey! The values for srtfree look weird. Did you norm them?")
  }
  if (mean(abs(df[, animals_col_name]), na.rm= TRUE) > 3 ){
    message("Hey! The values for animals look weird. Did you norm them?")
  }
  if (mean(abs(df[, tmb_col_name]), na.rm= TRUE) > 3 ){
    message("Hey! The values for Trails B look weird. Did you norm them?")
  }
  if (mean(abs(df[, digsym_col_name]), na.rm= TRUE) > 3 ){
    message("Hey! The values for digsym look weird. Did you norm them?")
  }

  KnightPACC <- rowMeans(df[, c(srtfree_col_name,
                                animals_col_name, tmb_col_name, 
                                digsym_col_name)], na.rm = TRUE)  
  NA_count <- rowSums(is.na(df[, c(srtfree_col_name,
                                   animals_col_name, tmb_col_name, 
                                   digsym_col_name)]))
  return(data.frame(id_col_name = as.vector(df[, id_col_name]),
                    visit_date_col_name = as.vector(df[, visit_date_col_name]),
                    "KnightPACC" = KnightPACC,
                    "NA_in_KnightPACC" = NA_count))
}


#' Takes in dataframe of normed scores
#' Returns ADCS PACC
#' index_srtfree,index_craftdel,index_digsym, index_mmse
#' @param df dataframe containing normed scores
#' @param id_col_name character string of ID column name
#' @param visit_date_col_name character string of visit date column name
#' @param srtfree_col_name character string of srtfree column name
#' @param digsym_col_name character string of digit symbol column name
#' @return dataframe containing participant id, visit date, the global composite score and the number of nas in the visit that went into the score. It is up to user discretion in terms of how many tests to require
#' @export

get_ADCSPACC <- function(df, id_col_name = "ID", visit_date_col_name = "TESTDATE",
                           srtfree_col_name = "z_srtfree",
                         craftdel_col_name = "z_craftdre", mmse_col_name = "z_MMSE_eq", digsym_col_name = "z_digsym"){
  if (mean(abs(df[, srtfree_col_name]), na.rm= TRUE) > 3 ){
    message("Hey! The values for srtfree look weird. Did you norm them?")
  }
  if (mean(abs(df[, craftdel_col_name]), na.rm= TRUE) > 3 ){
    message("Hey! The values for craftdel look weird. Did you norm them?")
  }
  if (mean(abs(df[, digsym_col_name]), na.rm= TRUE) > 3 ){
    message("Hey! The values for digsym look weird. Did you norm them?")
  }
  if (mean(abs(df[, mmse_col_name]), na.rm= TRUE) > 3 ){
    message("Hey! The values for MMSE look weird. Did you norm them?")
  }

  
  ADCSPACC <- rowMeans(df[, c(srtfree_col_name,
                                craftdel_col_name, digsym_col_name, 
                                mmse_col_name)], na.rm = TRUE)  
  NA_count <- rowSums(is.na(df[, c(srtfree_col_name,
                                   craftdel_col_name, digsym_col_name, 
                                   mmse_col_name)]))
  return(data.frame(id_col_name = as.vector(df[, id_col_name]),
                    visit_date_col_name = as.vector(df[, visit_date_col_name]),
                    "ADCSPACC" = ADCSPACC,
                    "NA_in_ADCSPACC" = NA_count))
}

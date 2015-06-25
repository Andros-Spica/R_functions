CAMOTECCERDATA1 <- function(data, complete=TRUE, asNa=c("NULL"), method="random", outliers=c("outlier"), hist=FALSE){
  
  # "data" is a data frame
  # "complete" is the boolean indicating if output needs to have the petrological characterization complete
  # "hist" is the boolean indicating if histograms should be created
  
  source(file="functions/histThemAll.R")
  source(file="functions/replaceNas.R")
  
  # numerical data
  for (v in 122:147){
    data[,v] <- as.numeric(as.character(data[,v]))
  }
  
  # filter incomplete characterization
  if(complete==TRUE){
    data <- subset(data,data$CHARAC=="complete")
  }
  
  # filter outliers
  for (i in 1:length(outliers)){
    data <- subset(data, row.names(data)!=outliers[i])
  }
  
  # replace Nas
  ## categorical data
  for (v in 1:121){
    for (i in 1:length(asNa)){
      data[,v] <- replaceNas(data[,v], type="categorical", asNa=asNa[i], method=method)
    }
  }
  
  ## numerical data
  for (v in 122:147){
    data[,v] <- replaceNas(data[,v], type="numerical", method=method)
  }
  
  # filter constant variables
  out <- vector()
  for (v in 1:ncol(data)){
    if (sd(data[,v],na.rm = TRUE)==0){
      out <- c(out,v)
    }
  }
  data <- data[,-out]
  
  # take out "COAR_R_EVAP"
  data <- data[,names(data)!="COAR_R_EVAP"]
  # take out "COAR_R_EVAP"
  data <- data[,names(data)!="COAR_R_CONGBREC"]
  # take out "COAR_R_EVAP"
  data <- data[,names(data)!="COAR_R_SERP"]
  
  if (hist==TRUE){
    if (version==1){
      # v1
      data2 <- data[,c(-1)]
    } else if (version==2){
      # v2
      data2 <- data[,c(-1,-2,-3)]
    } else if (version==3){
      data2 <- data
    }
    # plots
    histThemAll(data2, outType="png")
  }
  
  return(data)
}

CAMOTECCERDATA2 <- function(data){
  
  # generate tags 1:(site x period x function) 2:(FABRIC x function)
  tag1 <- vector()
  tag2 <- vector()
  for (i in 1:nrow(data)){
    tag1[i] <- paste(as.character(data$Site_Name[i]),
                     as.character(data$PERIOD_NAME[i]), sep=", ")
    tag2[i] <- paste(as.character(data$FABRIC[i]),
                     as.character(data$FUNCTION_CAT[i]), sep=", ")
  }
  
  ord.tag1 <- c("BDN-Pompeu Fabra, Early Roman Empire",
                "Can Feu, Early Roman Empire",
                "El Morè, Early Roman Empire",
                "El Vilarenc, Early Roman Empire",
                "Fenals, Early Roman Empire",
                "L'Aumedina, Early Roman Empire",
                "La Salut, Early Roman Empire",
                "Llafranc, Early Roman Empire",
                "Sant Boi Historic Centre, Early Roman Empire",
                "Kampyr Tepe, Hellenistic Period",
                "Termez - Tchingiz Tepe, Yuezhi Period",
                "Termez - Kara Tepe, Kushan Period",
                "Termez - Tchingiz Tepe, Kushan-Sassanid Period",
                "Termez - Kara Tepe, Kushan-Sassanid Period",
                "Termez - Ancient Quarters, Pre-Mongol Period - Turkic Dynasties")
  
  data$tag1 <- factor(tag1, levels=ord.tag1)
  data$tag2 <- factor(tag2)
  
  # Generate code to label points and classes
  label <- vector()
  star <- vector()
  star2 <- vector()
  star3 <- vector()
  for (i in 1:nrow(data)){
    if (data$FUNCTION_CAT[i]=="transport storage vessel"){
      star2[i] <- "AMP"
      star3[i] <- ""
      if (data$Site_Name[i]=="BDN-Pompeu Fabra"){
        label[i] <- "a"
        star[i] <- "BIF"
      } else if (data$Site_Name[i]=="Can Feu"){
        label[i] <- "b"
        star[i] <- "FEU"
      } else if (data$Site_Name[i]=="El Morè"){
        label[i] <- "c"
        star[i] <- "MOR"
      } else if (data$Site_Name[i]=="El Vilarenc"){
        label[i] <- "d"
        star[i] <- "ELV"
      } else if (data$Site_Name[i]=="Fenals"){
        label[i] <- "e"
        star[i] <- "FEN"
      } else if (data$Site_Name[i]=="L'Aumedina"){
        label[i] <- "f"
        star[i] <- "AUM"
      } else if (data$Site_Name[i]=="La Salut"){
        label[i] <- "g"
        star[i] <- "SAL"
      } else if (data$Site_Name[i]=="Llafranc"){
        label[i] <- "h"
        star[i] <- "LLA"
      } else if (data$Site_Name[i]=="Sant Boi Historic Centre"){
        label[i] <- "i"
        star[i] <- "SBL"
      } 
    } else {
      star[i] <- "UZB"
      if (data$FUNCTION_CAT[i]=="table ware"){
        star2[i] <- "TW|CW"
        if (data$Site_Name[i]=="Kampyr Tepe"){
          label[i] <- "1"
          star3[i] <- "KPT-H"
        } else if (data$Site_Name[i]=="Termez - Tchingiz Tepe"){
          if (data$PERIOD_NAME[i]=="Yuezhi Period"){
            label[i] <- "2"
            star3[i] <- "TT-Y"
          } else if (data$PERIOD_NAME[i]=="Kushan Period"){
            label[i] <- "3"
            star3[i] <- "TT-K"
          } else {
            label[i] <- "4"
            star3[i] <- "TT-KS"
          }
        } else if (data$Site_Name[i]=="Termez - Kara Tepe"){
          label[i] <- "5"
          star3[i] <- "TRZ-KS"
        } else if (data$Site_Name[i]=="Termez - Ancient Quarters"){
          label[i] <- "6"
          star3[i] <- "TRZ-PM"
        }
      } else if (data$FUNCTION_CAT[i]=="common ware"){
        label[i] <- "x"
        star2[i] <- "TW|CW"
        if (data$Site_Name[i]=="Kampyr Tepe"){
          star3[i] <- "KPT-H"
        } else if (data$Site_Name[i]=="Termez - Tchingiz Tepe"){
          if (data$PERIOD_NAME[i]=="Yuezhi Period"){
            star3[i] <- "TT-Y"
          } else if (data$PERIOD_NAME[i]=="Kushan Period"){
            star3[i] <- "TT-K"
          } else {
            star3[i] <- "TT-KS"
          }
        } else if (data$Site_Name[i]=="Termez - Kara Tepe"){
          star3[i] <- "TRZ-KS"
        } else if (data$Site_Name[i]=="Termez - Ancient Quarters"){
          star3[i] <- "TRZ-PM"
        }
      } else if (data$FUNCTION_CAT[i]=="cooking ware"){
        label[i] <- "y"
        star2[i] <- "CKW"
      } else if (data$FUNCTION_CAT[i]=="storage vessel"){
        label[i] <- "w"
        star2[i] <- "TW|CW"
        star3[i] <- "TRZ-KS"
        
      } else {
        label[i] <- "z"
        star2[i] <- "TW|CW"
        star3[i] <- "TT-Y"
      }
    }
  }
  data$label <- factor(label)
  data$star <- factor(star)
  data$star2 <- factor(star2)
  data$star3 <- factor(star3)
  
  return(data)
}

CAMOTECCERDATA3 <- function(data){
  
  ### put values in proper order
  ord.per = c("Early Roman Empire" ,
              "Hellenistic Period",
              "Yuezhi Period",
              "Kushan Period",
              "Kushan-Sassanid Period",
              "Pre-Mongol Period - Turkic Dynasties"
  )
  ord.fctcat <- c("table ware","common ware","cooking ware", "storage vessel","transport storage vessel", "illumination")
  
  ord.yni<-c("yes", "no")
  ord.clay<-c("Ca-rich", "Fe to Ca-rich", "Fe-rich")
  ord.formtech<-c("wheel", "mould", "hand-made clay colid")
  
  ord.inclorient<-c("parallel", "slightly parallel", "unparallel", "none")
  ord.incldistrib<-c("well", "moderately to well", "moderately", "poorly to moderately", "poorly", "none")
  ord.temp<-c("unfired", "700-800ºC", "800-900ºC", "900-1000ºC", "1000-1100ºC")
  ord.comp<-c("none", "few", "common", "frequent", "dominant", "predominant")
  ord.comp2<-c("none", "few", "frequent", "predominant")
  ord.freq<-c("none", "few", "common", "abundant", "very abundant")
  ord.grain<-c("none","very fine","very fine to fine","fine","fine to medium","medium","medium to coarse","coarse","coarse to very coarse","very coarse")
  ord.round<-c("angular","angular to subangular", "subangular","subangular to subrounded","subrounded","subrounded to rounded","rounded","none")
  ord.form<-c("elongate","elongate to equidimensional","equidimensional","equidimensional to laminar","laminar", "none")
  ord.spac<-c("single-spaced","single to double-spaced","double-spaced","double to open-spaced","open-spaced","none")
  ord.sort<-c("well-sorted","moderately to well-sorted","moderately-sorted","poorly to moderately-sorted","poorly-sorted", "none")
  ord.grain2<-c("none","very fine silt","very fine to fine silt","fine silt","fine to medium silt","medium silt","medium to coarse silt","coarse silt","coarse silt to very fine sand")
  
  if ("PERIOD_NAME" %in% names(data)){
    data$PERIOD_NAME <-factor(data$PERIOD_NAME, levels=ord.per)
  }
  if ("FUNCTION_CAT" %in% names(data)){
    data$FUNCTION_CAT <-factor(data$FUNCTION_CAT, levels=ord.fctcat)
  }
  
  if ("CLAY" %in% names(data)){
    data$CLAY <-factor(data$CLAY, levels=ord.clay)
  }
  if ("TEMPER" %in% names(data)){
    data$TEMPER <-factor(data$TEMPER, levels=ord.yni)
  }
  if ("INCLUSIONS" %in% names(data)){
    data$INCLUSIONS <-factor(data$INCLUSIONS, levels=ord.yni)
  }
  if ("CLAY_MIX" %in% names(data)){
    data$CLAY_MIX <-factor(data$CLAY_MIX, levels=ord.yni)
  }
  if ("CLAY_TEXT_FEAT" %in% names(data)){
    data$CLAY_TEXT_FEAT <-factor(data$CLAY_TEXT_FEAT, levels=ord.yni)
  }
  
  data$INCLUS_DISTRIB <- factor(data$INCLUS_DISTRIB, levels=ord.incldistrib)
  data$INCLUS_ORIENT <- factor(data$INCLUS_ORIENT, levels=ord.inclorient)
  data$TEMP <- factor(data$TEMP, levels=ord.temp)
  
  data$VOID_OVERALL <- factor(data$VOID_OVERALL, levels=ord.freq)
  if ("VOID_VESIC_MEGA" %in% names(data)){
    data$VOID_VESIC_MEGA <-factor(data$VOID_VESIC_MEGA, levels=ord.comp)
  }
  if ("VOID_VESIC_MACRO" %in% names(data)){
    data$VOID_VESIC_MACRO <-factor(data$VOID_VESIC_MACRO, levels=ord.comp)
  }
  if ("VOID_VESIC_MESO" %in% names(data)){
    data$VOID_VESIC_MESO <-factor(data$VOID_VESIC_MESO, levels=ord.comp)
  }
  if ("VOID_VESIC_MICRO" %in% names(data)){
    data$VOID_VESIC_MICRO <-factor(data$VOID_VESIC_MICRO, levels=ord.comp)
  }
  if ("VOID_VUGH_MEGA" %in% names(data)){
    data$VOID_VUGH_MEGA <-factor(data$VOID_VUGH_MEGA, levels=ord.comp)
  }
  if ("VOID_VUGH_MACRO" %in% names(data)){
    data$VOID_VUGH_MACRO <-factor(data$VOID_VUGH_MACRO, levels=ord.comp)
  }
  if ("VOID_VUGH_MESO" %in% names(data)){
    data$VOID_VUGH_MESO <-factor(data$VOID_VUGH_MESO, levels=ord.comp)
  }
  if ("VOID_VUGH_MICRO" %in% names(data)){
    data$VOID_VUGH_MICRO <-factor(data$VOID_VUGH_MICRO, levels=ord.comp)
  }
  if ("VOID_CHAN_MEGA" %in% names(data)){
    data$VOID_CHAN_MEGA <-factor(data$VOID_CHAN_MEGA, levels=ord.comp)
  }
  if ("VOID_CHAN_MACRO" %in% names(data)){
    data$VOID_CHAN_MACRO <-factor(data$VOID_CHAN_MACRO, levels=ord.comp)
  }
  if ("VOID_CHAN_MESO" %in% names(data)){
    data$VOID_CHAN_MESO <-factor(data$VOID_CHAN_MESO, levels=ord.comp)
  }
  if ("VOID_CHAN_MICRO" %in% names(data)){
    data$VOID_CHAN_MICRO <-factor(data$VOID_CHAN_MICRO, levels=ord.comp)
  }
  if ("VOID_PLAN_MEGA" %in% names(data)){
    data$VOID_PLAN_MEGA <-factor(data$VOID_PLAN_MEGA, levels=ord.comp)
  }
  if ("VOID_PLAN_MACRO" %in% names(data)){
    data$VOID_PLAN_MACRO <-factor(data$VOID_PLAN_MACRO, levels=ord.comp)
  }
  if ("VOID_PLAN_MESO" %in% names(data)){
    data$VOID_PLAN_MESO <-factor(data$VOID_PLAN_MESO, levels=ord.comp)
  }
  if ("VOID_PLAN_MICRO" %in% names(data)){
    data$VOID_PLAN_MICRO <-factor(data$VOID_PLAN_MICRO, levels=ord.comp)
  }
  
  data$COAR_GRAINSIZE <- factor(data$COAR_GRAINSIZE, levels=ord.grain)
  data$COAR_FREQ <- factor(data$COAR_FREQ, levels=ord.freq)
  data$COAR_ROUNDNESS <- factor(data$COAR_ROUNDNESS, levels=ord.round)
  data$COAR_FORM <- factor(data$COAR_FORM, levels=ord.form)
  data$COAR_SPACING <- factor(data$COAR_SPACING, levels=ord.spac)
  data$COAR_SORTING <- factor(data$COAR_SORTING, levels=ord.sort)
  
  if ("COAR_R_GRANIT" %in% names(data)){
    data$COAR_R_GRANIT <-factor(data$COAR_R_GRANIT, levels=ord.comp)    
  }
  if ("COAR_R_RHYOL" %in% names(data)){
    data$COAR_R_RHYOL <-factor(data$COAR_R_RHYOL, levels=ord.comp)    
  }
  if ("COAR_R_DIOR" %in% names(data)){
    data$COAR_R_DIOR <-factor(data$COAR_R_DIOR, levels=ord.comp)
  }
  if ("COAR_R_ANDES" %in% names(data)){
    data$COAR_R_ANDES <-factor(data$COAR_R_ANDES, levels=ord.comp)
  }
  if ("COAR_R_GABBRO" %in% names(data)){
    data$COAR_R_GABBRO <-factor(data$COAR_R_GABBRO, levels=ord.comp)
  }
  if ("COAR_R_BASALT" %in% names(data)){
    data$COAR_R_BASALT <-factor(data$COAR_R_BASALT, levels=ord.comp)
  }
  if ("COAR_R_SYEN" %in% names(data)){
    data$COAR_R_SYEN <-factor(data$COAR_R_SYEN, levels=ord.comp)
  }
  if ("COAR_R_TRACHY" %in% names(data)){
    data$COAR_R_TRACHY <-factor(data$COAR_R_TRACHY, levels=ord.comp)
  }
  if ("COAR_R_CONGBREC" %in% names(data)){
    data$COAR_R_CONGBREC <-factor(data$COAR_R_CONGBREC, levels=ord.comp)
  }
  if ("COAR_R_QTZSANDST" %in% names(data)){
    data$COAR_R_QTZSANDST <-factor(data$COAR_R_QTZSANDST, levels=ord.comp)
  }
  if ("COAR_R_FELDSANDST" %in% names(data)){
    data$COAR_R_FELDSANDST <-factor(data$COAR_R_FELDSANDST, levels=ord.comp)
  }
  if ("COAR_R_LITSANDST" %in% names(data)){
    data$COAR_R_LITSANDST <-factor(data$COAR_R_LITSANDST, levels=ord.comp)
  }
  if ("COAR_R_CASILTST" %in% names(data)){
    data$COAR_R_CASILTST <-factor(data$COAR_R_CASILTST, levels=ord.comp)
  }
  if ("COAR_R_FESILTST" %in% names(data)){
    data$COAR_R_FESILTST <-factor(data$COAR_R_FESILTST, levels=ord.comp)
  }
  if ("COAR_R_CAMUDST" %in% names(data)){
    data$COAR_R_CAMUDST <-factor(data$COAR_R_CAMUDST, levels=ord.comp)
  }
  if ("COAR_R_FEMUDST" %in% names(data)){
    data$COAR_R_FEMUDST <-factor(data$COAR_R_FEMUDST, levels=ord.comp)
  }
  if ("COAR_R_CLAYST" %in% names(data)){
    data$COAR_R_CLAYST <-factor(data$COAR_R_CLAYST, levels=ord.comp)
  }
  if ("COAR_R_LIMEST" %in% names(data)){
    data$COAR_R_LIMEST <-factor(data$COAR_R_LIMEST, levels=ord.comp)
  }
  if ("COAR_R_CALS" %in% names(data)){
    data$COAR_R_CALS <-factor(data$COAR_R_CALS, levels=ord.comp)
  }
  if ("COAR_R_DOLOM" %in% names(data)){
    data$COAR_R_DOLOM <-factor(data$COAR_R_DOLOM, levels=ord.comp)
  }
  if ("COAR_R_CALM" %in% names(data)){
    data$COAR_R_CALM <-factor(data$COAR_R_CALM, levels=ord.comp)
  }
  if ("COAR_R_SPELEO" %in% names(data)){
    data$COAR_R_SPELEO <-factor(data$COAR_R_SPELEO, levels=ord.comp)
  }
  if ("COAR_R_CAL.FOS" %in% names(data)){
    data$COAR_R_CAL.FOS <-factor(data$COAR_R_CAL.FOS, levels=ord.comp)
  }
  if ("COAR_R_BIVAL" %in% names(data)){
    data$COAR_R_BIVAL <-factor(data$COAR_R_BIVAL, levels=ord.comp)
  }
  if ("COAR_R_TRAV" %in% names(data)){
    data$COAR_R_TRAV <-factor(data$COAR_R_TRAV, levels=ord.comp)
  }
  if ("COAR_R_EVAP" %in% names(data)){
    data$COAR_R_EVAP <-factor(data$COAR_R_EVAP, levels=ord.comp)
  }
  if ("COAR_R_CHERT" %in% names(data)){
    data$COAR_R_CHERT <-factor(data$COAR_R_CHERT, levels=ord.comp)
  }
  if ("COAR_R_RADIO" %in% names(data)){
    data$COAR_R_RADIO <-factor(data$COAR_R_RADIO, levels=ord.comp)
  }
  if ("COAR_R_SLATE" %in% names(data)){
    data$COAR_R_SLATE <-factor(data$COAR_R_SLATE, levels=ord.comp)
  }
  if ("COAR_R_PHYLL" %in% names(data)){
    data$COAR_R_PHYLL <-factor(data$COAR_R_PHYLL, levels=ord.comp)
  }
  if ("COAR_R_SCHIST" %in% names(data)){
    data$COAR_R_SCHIST <-factor(data$COAR_R_SCHIST, levels=ord.comp)
  }
  if ("COAR_R_GNEISS" %in% names(data)){
    data$COAR_R_GNEISS <-factor(data$COAR_R_GNEISS, levels=ord.comp)
  }
  if ("COAR_R_QUARTZ" %in% names(data)){
    data$COAR_R_QUARTZ <-factor(data$COAR_R_QUARTZ, levels=ord.comp)
  }
  if ("COAR_R_MARBLE" %in% names(data)){
    data$COAR_R_MARBLE <-factor(data$COAR_R_MARBLE, levels=ord.comp)
  }
  if ("COAR_R_AMP" %in% names(data)){
    data$COAR_R_AMP <-factor(data$COAR_R_AMP, levels=ord.comp)
  }
  if ("COAR_R_SERP" %in% names(data)){
    data$COAR_R_SERP <-factor(data$COAR_R_SERP, levels=ord.comp)
  }
  if ("COAR_C_QTZ" %in% names(data)){
    data$COAR_C_QTZ <-factor(data$COAR_C_QTZ, levels=ord.comp)
  }
  if ("COAR_C_PL" %in% names(data)){
    data$COAR_C_PL <-factor(data$COAR_C_PL, levels=ord.comp)
  }
  if ("COAR_C_KFS" %in% names(data)){
    data$COAR_C_KFS <-factor(data$COAR_C_KFS, levels=ord.comp)
  }
  if ("COAR_C_SA" %in% names(data)){
    data$COAR_C_SA <-factor(data$COAR_C_SA, levels=ord.comp)
  }
  if ("COAR_C_MS" %in% names(data)){
    data$COAR_C_MS <-factor(data$COAR_C_MS, levels=ord.comp)
  }
  if ("COAR_C_BT" %in% names(data)){
    data$COAR_C_BT <-factor(data$COAR_C_BT, levels=ord.comp)
  }
  if ("COAR_C_SRP" %in% names(data)){
    data$COAR_C_SRP <-factor(data$COAR_C_SRP, levels=ord.comp)
  }
  if ("COAR_C_OP" %in% names(data)){
    data$COAR_C_OP <-factor(data$COAR_C_OP, levels=ord.comp)
  }
  if ("COAR_C_RT" %in% names(data)){
    data$COAR_C_RT <-factor(data$COAR_C_RT, levels=ord.comp)
  }
  if ("COAR_C_SPL" %in% names(data)){
    data$COAR_C_SPL <-factor(data$COAR_C_SPL, levels=ord.comp)
  }
  if ("COAR_C_EP" %in% names(data)){
    data$COAR_C_EP <-factor(data$COAR_C_EP, levels=ord.comp)
  }
  if ("COAR_C_AM" %in% names(data)){
    data$COAR_C_AM <-factor(data$COAR_C_AM, levels=ord.comp)
  }
  if ("COAR_C_CPX" %in% names(data)){
    data$COAR_C_CPX <-factor(data$COAR_C_CPX, levels=ord.comp)
  }
  if ("COAR_C_OPX" %in% names(data)){
    data$COAR_C_OPX <-factor(data$COAR_C_OPX, levels=ord.comp)
  }
  if ("COAR_C_OL" %in% names(data)){
    data$COAR_C_OL <-factor(data$COAR_C_OL, levels=ord.comp)
  }
  if ("COAR_C_GRT" %in% names(data)){
    data$COAR_C_GRT <-factor(data$COAR_C_GRT, levels=ord.comp)
  }
  if ("COAR_C_SIL" %in% names(data)){
    data$COAR_C_SIL <-factor(data$COAR_C_SIL, levels=ord.comp)
  }
  if ("COAR_C_ST" %in% names(data)){
    data$COAR_C_ST <-factor(data$COAR_C_ST, levels=ord.comp)
  }
  if ("COAR_C_TTN" %in% names(data)){
    data$COAR_C_TTN <-factor(data$COAR_C_TTN, levels=ord.comp)
  }
  if ("COAR_C_ZRN" %in% names(data)){
    data$COAR_C_ZRN <-factor(data$COAR_C_ZRN, levels=ord.comp)
  }
  if ("COAR_C_GR" %in% names(data)){
    data$COAR_C_GR <-factor(data$COAR_C_GR, levels=ord.comp)
  }
  if ("COAR_C_PY" %in% names(data)){
    data$COAR_C_PY <-factor(data$COAR_C_PY, levels=ord.comp)
  }
  
  data$FINE_FREQ <- factor(data$FINE_FREQ, levels=ord.freq)
  data$FINE_GRAINSIZE <- factor(data$FINE_GRAINSIZE, levels=ord.grain2)
  data$FINE_FORM <- factor(data$FINE_FORM, levels=ord.form)
  
  if ("FINE_C_CAL" %in% names(data)){
    data$FINE_C_CAL <- factor(data$FINE_C_CAL, levels=ord.comp2)
  }
  if ("FINE_C_CAL.FOS" %in% names(data)){
    data$FINE_C_CAL.FOS <- factor(data$FINE_C_CAL.FOS, levels=ord.comp2)
  }
  if ("FINE_C_QTZ" %in% names(data)){
    data$FINE_C_QTZ <- factor(data$FINE_C_QTZ, levels=ord.comp2)
  }
  if ("FINE_C_PL" %in% names(data)){
    data$FINE_C_PL <- factor(data$FINE_C_PL, levels=ord.comp2)
  }
  if ("FINE_C_KFS" %in% names(data)){
    data$FINE_C_KFS <- factor(data$FINE_C_KFS, levels=ord.comp2)
  }
  if ("FINE_C_SA" %in% names(data)){
    data$FINE_C_SA <- factor(data$FINE_C_SA, levels=ord.comp2)
  }
  if ("FINE_C_MS" %in% names(data)){
    data$FINE_C_MS <- factor(data$FINE_C_MS, levels=ord.comp2)
  }
  if ("FINE_C_BT" %in% names(data)){
    data$FINE_C_BT <- factor(data$FINE_C_BT, levels=ord.comp2)
  }
  if ("FINE_C_SRP" %in% names(data)){
    data$FINE_C_SRP <- factor(data$FINE_C_SRP, levels=ord.comp2)
  }
  if ("FINE_C_OP" %in% names(data)){
    data$FINE_C_OP <- factor(data$FINE_C_OP, levels=ord.comp2)
  }
  if ("FINE_C_RT" %in% names(data)){
    data$FINE_C_RT <- factor(data$FINE_C_RT, levels=ord.comp2)
  }
  if ("FINE_C_EP" %in% names(data)){
    data$FINE_C_EP <- factor(data$FINE_C_EP, levels=ord.comp2)
  }
  if ("FINE_C_AM" %in% names(data)){
    data$FINE_C_AM <- factor(data$FINE_C_AM, levels=ord.comp2)
  }
  if ("FINE_C_CPX" %in% names(data)){
    data$FINE_C_CPX <- factor(data$FINE_C_CPX, levels=ord.comp2)
  }
  if ("FINE_C_OPX" %in% names(data)){
    data$FINE_C_OPX <- factor(data$FINE_C_OPX, levels=ord.comp2)
  }
  if ("FINE_C_OL" %in% names(data)){
    data$FINE_C_OL <- factor(data$FINE_C_OL, levels=ord.comp2)
  }
  if ("FINE_C_GRT" %in% names(data)){
    data$FINE_C_GRT <- factor(data$FINE_C_GRT, levels=ord.comp2)
  }
  if ("FINE_C_ZRN" %in% names(data)){
    data$FINE_C_ZRN <- factor(data$FINE_C_ZRN, levels=ord.comp2)
  }
  
  return(data)
}
    
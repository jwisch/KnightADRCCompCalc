#Crosswalks from Monsell et al, 2016 DOI: 10.1097/WAD.0000000000000111


#' convert Moca scores to mmse
#' Crosswalks from Monsell et al, 2016 DOI: 10.1097/WAD.0000000000000111
#' @param MoCA MoCa scores as vector
#' @return equivalent MMSE scores as vector
#' @export
#' @examples
#' MoCA2MMSE(c(23,124,53))

MoCA2MMSE <- function(MoCA){
  data(moca_MMSE)
  df <- data.frame("Education_adjusted_moca" = MoCA)
  df$Education_adjusted_moca <- as.numeric(df$Education_adjusted_moca)
  df[is.na(df$Education_adjusted_moca) | df$Education_adjusted_moca > 30 | df$Education_adjusted_moca < 0, "Education_adjusted_moca"] <- "out of range"
  df$row_num <- seq(from = 1, to = nrow(df))
  df <- merge(df, moca_MMSE, by = "Education_adjusted_moca", all.x = TRUE, all.y = FALSE)
  df <- df[order(df$row_num),]
  return(df$MMSE_equivalent)
}


#' convert multilingual naming test to boston naming test equivalent
#' Crosswalks from Monsell et al, 2016 DOI: 10.1097/WAD.0000000000000111
#' @param mlnttots multilingual naming scores as vector
#' @return boston naming test scores as vector
#' @export
#' @examples
#' minttots2BNT(c(23,124,53))

mlnt2BNT <- function(mlnttots){
  data(MINT_bostonNaming)
  df <- data.frame("MINT" = mlnttots)
  df$MINT <- as.numeric(df$MINT)
  df[is.na(df$MINT) | df$MINT > 32 | df$MINT < 0, "MINT"] <- "out of range"
  df$row_num <- seq(from = 1, to = nrow(df))
  df <- merge(df, MINT_bostonNaming, by = "MINT", all.x = TRUE)
  df <- df[order(df$row_num),]
  return(df$Boston_Naming)
}

#' convert Craft to logical memory iia
#' Crosswalks from Monsell et al, 2016 DOI: 10.1097/WAD.0000000000000111
#' @param mlnttots multilingual naming scores as vector
#' @return boston naming test scores as vector
#' @export
#' @examples
#' craft2MEMUNITS(c(23,124,53))

craft2MEMUNITS <- function(craft){
  data(Craft_LogMem)
  df <- data.frame("Craft" = craft)
  df$Craft <- as.numeric(df$Craft)
  df[is.na(df$Craft) | df$Craft > 25 | df$Craft < 0, "Craft"] <- "out of range"
  df$row_num <- seq(from = 1, to = nrow(df))
  df <- merge(df, Craft_LogMem, by = "Craft", all.x = TRUE)
  df <- df[order(df$row_num),]
  return(df$LogicalMemory1A)
}


#' convert digforct to DIGIF
#' Crosswalks from Monsell et al, 2016 DOI: 10.1097/WAD.0000000000000111
#' @param digforct WMS Digit Span Forward	Num 	0-14	High score = good
#' @return DIGIF #WMS-R DIGIT SPAN FORWARD (total number of trials)	Num 	0-12	High score = good
#' @export
#' @examples
#' digforct2DIGIF(c(0,7,53))

digforct2DIGIF <- function(digforct){
  data(trials_digSpan)
  df <- data.frame("trials" = digforct)
  df$trials <- as.numeric(df$trials)
  df[is.na(df$trials) | df$trials > 14 | df$trials < 0, "trials"] <- "out of range"
  df$row_num <- seq(from = 1, to = nrow(df))
  df <- merge(df, trials_digSpan, by = "trials", all.x = TRUE)
  df <- df[order(df$row_num),]
  return(df$DigitSpan)
}


#' convert digbacct to DIGIB
#' Crosswalks from Monsell et al, 2016 DOI: 10.1097/WAD.0000000000000111
#' @param digbacct 	0-14	High score = good
#' @return DIGIB #WMS-R DIGIT SPAN BACKWARD (total number of trials)	Num 	0-12	High score = good
#' @export
#' @examples
#' digbacct2DIGIB(c(0,7,53))

digbacct2DIGIB <- function(digbacct){
  data(NumSpanBkwd_DigSpanBkwd)
  df <- data.frame("NumberSpanBackward" = digbacct)
  df$NumberSpanBackward <- as.numeric(df$NumberSpanBackward)
  df[is.na(df$NumberSpanBackward) | df$NumberSpanBackward > 14 | df$NumberSpanBackward < 0, "NumberSpanBackward"] <- "out of range"
  df$row_num <- seq(from = 1, to = nrow(df))
  df <- merge(df, NumSpanBkwd_DigSpanBkwd, by = "NumberSpanBackward", all.x = TRUE)
  df <- df[order(df$row_num),]
  return(df$DigitSpanBackward)
}

#' combine two vectors that contain Trails A data to get a single trails A vector
#'
#' @param tma trails a (number of seconds)
#' @param TRAILA TRAILMAKING A (number of seconds)	Num 	0-150
#' @return TRAILA A combination of the two columns
#' @export
#' @examples
#' get_TRAILSA(c(NA,7,53), c(300, NA, NA))

get_TRAILSA <- function(tma, TRAILA){
  TRAILA <- as.numeric(as.character(TRAILA))
  TRAILA[TRAILA > 150] <- 150 #if trails A is greater than 150, set to 150
  tma[tma > 150] <- 150 #if tma is greater than 150, set to 150
  TRAILA[TRAILA < 0] <- NA #if trails A is negative, set to NA
  tma[tma < 0] <- NA #if tma is negative, set to NA
  TRAILA <- rowMeans(data.frame("TRAILA" = TRAILA, "tma" = tma), na.rm = TRUE)
  TRAILA[is.nan(TRAILA)] <- NA

  return(TRAILA)
}

#' combine two vectors that contain Trails B data to get a single trails B vector
#'
#' @param tmb trails b (number of seconds)
#' @param TRAILB TRAILMAKING B (number of seconds)	Num 	0-300
#' @return TRAILB A combination of the two columns
#' @export
#' @examples
#' get_TRAILSB(c(NA,507,53), c(300, NA, NA))

get_TRAILSB <- function(tmb, TRAILB){
  TRAILB <- as.numeric(as.character(TRAILB))
  TRAILB[TRAILB > 300] <- 300 #if trails B is greater than 300, set to 300
  tmb[tmb > 300] <- 300 #if tmb is greater than 300, set to 300
  TRAILB[TRAILB < 0] <- NA #if trails A is negative, set to NA
  tmb[tmb < 0] <- NA #if tma is negative, set to NA
  TRAILB <- rowMeans(data.frame("TRAILB" = TRAILB, "tmb" = tmb), na.rm = TRUE)
  TRAILB[is.nan(TRAILB)] <- NA

  return(TRAILB)
}



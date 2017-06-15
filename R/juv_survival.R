#' Inverse logit link, transforms variables on logit scale to real scale.
#' The inverse logit function (called either the inverse logit or the logistic
#' function) transforms a real number (usually the logarithm of the odds) to a
#' value (usually probability p) in the interval [0,1]. The invlogit function is
#' 1/ (1 + exp(-x)).
#' @name inv.logit
#' @param eta logit value to be back transformed to real scale

### inverse logit link function
inv.logit <- function(eta) {
  return(1 / (1 + exp(-eta)))
}


#' Determines the size specific survival rates of juveniles rearing in-stream 
#' for each watershed/bypass. Output is a 31x4 matrix. Each row represents each 
#' watershed/bypass, and the columns represent each size class from smallest to largest.
#' @name Juv.IC.S
#' @param maxT25 Boolean: if 1 average monthly stream temperature > 25degC during rearing. 
#' A 1x31 vector, where each element represents a watershed/bypass.
#' @param aveT20 Boolean: if 1 average monthly stream temperature > 20degC during rearing. 
#' A 1x31 vector, where each element represents a watershed/bypass.
#' @param high.pred Probability of high predation. A 1x31 vector, where each element represents a watershed/bypass.
#' @param no.con.pts Number of predator "hot spots" in each watershed. A 1x31 vector, where each element represents a watershed/bypass.
#' @param prop.div Proportion of total flow diverted in each watershed. A 1x31 vector, where each element represents a watershed/bypass.
#' @param tot.div Total amount of water diverted in each watershed/bypass. A 1x31 vector, where each element represents a watershed/bypass
#' @param strand Probability of juveniles becoming stranded. A 1x31 vector, where each element represents a watershed/bypass.
#' @example 
#' Juv.IC.S(maxT25 = c(0, 1, 0, 0, 0), aveT20 = c(0, 0, 1, 0, 0), 
#' high.pred = c(0, 0, 0, 1, 0), no.con.pts = rep(10, 5), prop.div = rep(0, 5), 
#' tot.div = rep(0, 5), strand = c(0, 0, 0, 0, 1))

# maxT25 = 0
# aveT20 = 0
# high.pred = .33
# no.con.pts = 197
# prop.div = .1
# tot.div = 200
# strand = .16

Juv.IC.S <- function(maxT25, aveT20, high.pred, no.con.pts, prop.div, tot.div, strand) {
  
  B0 <- -0.75
  
  s <-inv.logit(B0 + -0.717 * aveT20 + 
                  -0.122 * high.pred + -0.189 * no.con.pts * 0.0358 * high.pred + -3.51 * prop.div * 0.05 + 
                  -0.0021 * tot.div * 0.215 + -1.939 * strand) * (1 - maxT25) + maxT25 * 0.1
  
  m <- inv.logit(B0 + 1.48 + -0.717 * aveT20 + 
                   -0.122 * high.pred + -0.189 * no.con.pts * 0.0358 * high.pred + -3.51 * prop.div * 0.05 + 
                   -0.0021 * tot.div * 0.215) * (1 - maxT25) + maxT25 * 0.1
  
  l <- inv.logit(B0 + 2.223 + -0.717 * aveT20 + 
                   -0.122 * high.pred + -0.189 * no.con.pts * 0.0358 * high.pred + -3.51 * prop.div * 0.05 + 
                   -0.0021 * tot.div * 0.215) * (1 - maxT25) + maxT25 * 0.1  
  t(rbind(s, m, l, 1))
}

# a <- Juv.IC.S(maxT25, aveT20, high.pred, no.con.pts, prop.div, tot.div, strand)


# refactored --------------------------------------------------------------------
# Juv.IC.S <- function(maxT25, aveT20, high.pred, no.con.pts, prop.div, tot.div, strand) {
#   beta <- c(-0.75, -0.717, -0.122, -0.0067662, -0.1755, -0.0004515)
#   
#   score <- beta[1] + 
#     beta[2] * aveT20 + 
#     beta[3] * high.pred + 
#     beta[4] * no.con.pts * high.pred + 
#     beta[5] * prop.div +
#     beta[6] * tot.div 
#   
#   score_s <- score + (-1.939 * strand)
#   score_m <- score + 1.48
#   score_l <- score + 2.223
#   
#   s <- inv.logit(score_s) * (1 - maxT25) + maxT25 * 0.1
#   m <- inv.logit(score_m) * (1 - maxT25) + maxT25 * 0.1
#   l <- inv.logit(score_l) * (1 - maxT25) + maxT25 * 0.1  
#   
#   return(cbind(s, m, l, 1))
# }

#' Determines the size specific survival rates of juveniles rearing in-stream for each watershed/bypass.  
#' Output is a 31x4 matrix.  Each row represents each watershed/bypass, and the columns represent each 
#' size class from smallest to largest.
#' @name Juv.FP.S
#' @param maxT25 Boolean: if 1 average monthly stream temperature > 25degC during rearing. 
#' A 1x31 vector, where each element represents a watershed/bypass.
#' @param aveT20 Boolean: if 1 average monthly stream temperature > 20degC during rearing. 
#' A 1x31 vector, where each element represents a watershed/bypass.
#' @param high.pred Probability of high predation. 
#' A 1x31 vector, where each element represents a watershed/bypass.
#' @param no.con.pts Number of predator "hot spots" in each watershed. 
#' A 1x31 vector, where each element represents a watershed/bypass.
#' @param prop.div Proportion of total flow diverted in each watershed. 
#' A 1x31 vector, where each element represents a watershed/bypass.
#' @param tot.div Total amount of water diverted in each watershed/bypass. 
#' A 1x31 vector, where each element represents a watershed/bypass
#' @param strand Probability of juveniles becoming stranded. 
#' A 1x31 vector, where each element represents a watershed/bypass.
#' @param wks.fld Number of weeks that the floodplains are inundated.  Defaults to 2.
#' @example Juv.FP.S(maxT25 = c(0, 1, 0, 0, 0), aveT20 = c(0, 0, 1, 0, 0), 
#' high.pred = c(0, 0, 0, 1, 0), no.con.pts = rep(10, 5), prop.div = rep(0, 5), 
#' tot.div = rep(0, 5), strand = c(0, 0, 0, 0, 1))

Juv.FP.S <- function(maxT25, aveT20, high.pred, no.con.pts, prop.div, tot.div, strand, wks.fld = 2) {
  
  if(wks.fld > 0) in_channel <- (4-wks.fld) / 4 else in_channel = 1
  in_floodplain <- 1 - in_channel
  
  B0 <- -0.75
  
  s_in_channel <- (inv.logit(B0 + -0.717 * aveT20 + 
                               -0.122 * high.pred + -0.189 * no.con.pts * 0.0358 * high.pred + -3.51 * prop.div * 0.05 + 
                               -0.0021 * tot.div * 0.215 + -1.939 * strand) * (1 - maxT25) + maxT25 * 0.1) ^ in_channel
  
  m_in_channel <- (inv.logit(B0 + 1.48 + -0.717 * aveT20 + 
                               -0.122 * high.pred + -0.189 * no.con.pts * 0.0358 * high.pred + -3.51 * prop.div * 0.05 + 
                               -0.0021 * tot.div * 0.215) * (1 - maxT25) + maxT25 * 0.1) ^ in_channel
  
  l_in_channel <- (inv.logit(B0 + 2.223 + -0.717 * aveT20 + 
                               -0.122 * high.pred + -0.189 * no.con.pts * 0.0358 * high.pred + -3.51 * prop.div * 0.05 + 
                               -0.0021 * tot.div * 0.215) * (1 - maxT25) + maxT25 * 0.1) ^ in_channel
  
  s_in_floodplain <- (inv.logit(B0 + -0.717 * aveT20 + 0.47 +
                                  -0.122 * high.pred) * (1 - maxT25) + maxT25 * 0.1) ^ in_floodplain
  
  m_in_floodplain <- (inv.logit(B0 + 1.48 + -0.717 * aveT20 + 0.47 +
                                  -0.122 * high.pred) * (1 - maxT25) + maxT25 * 0.1) ^ in_floodplain
  
  l_in_floodplain <- (inv.logit(B0 + 2.223 + -0.717 * aveT20 + 0.47 +
                                  -0.122 * high.pred) * (1 - maxT25) + maxT25 * 0.1) ^ in_floodplain
  
  t(rbind(s_in_channel * s_in_floodplain, 
          m_in_channel * m_in_floodplain, 
          l_in_channel * l_in_floodplain, 1))
}

# refactored---------------------------------------------------------------------
# Juv.FP.S <- function(maxT25, aveT20, high.pred, no.con.pts, prop.div, tot.div, strand, wks.fld = 2) {
#   
#   if (wks.fld > 0) in_channel <- (4 - wks.fld) / 4 else in_channel = 1
#   in_floodplain <- 1 - in_channel
#   
#   beta <- c(-0.75, -0.717, -0.122, -0.0067662, -0.1755, -0.0004515)
#   
#   in_channel_surv <- Juv.IC.S(maxT25, aveT20, high.pred, no.con.pts, prop.div, tot.div, strand)
#   
#   s_in_channel <- in_channel_surv[ ,1] ^ in_channel
#   m_in_channel <- in_channel_surv[ ,2] ^ in_channel
#   l_in_channel <- in_channel_surv[ ,3] ^ in_channel
#   
#   score_fp <- beta[1] +
#     beta[2] * aveT20 +
#     beta[3] * high.pred
#   
#   s_fp <- score_fp + 0.47 
#   m_fp <- score_fp + 1.95
#   l_fp <- score_fp + 2.693
#   
#   s_in_floodplain <- (inv.logit(s_fp) * (1 - maxT25) + maxT25 * 0.1) ^ in_floodplain
#   m_in_floodplain <- (inv.logit(m_fp) * (1 - maxT25) + maxT25 * 0.1) ^ in_floodplain
#   l_in_floodplain <- (inv.logit(l_fp) * (1 - maxT25) + maxT25 * 0.1) ^ in_floodplain
#   
#   cbind(s_in_channel * s_in_floodplain, 
#         m_in_channel * m_in_floodplain, 
#         l_in_channel * l_in_floodplain, 1)
# }

#' Produces size specific estimates of out-migrant survival for juveniles 
#' migrating out of one of the Sacramento mainstem areas (UM, LM, LL). Output is
#' a 1x4 vectors where each element represents a size class from smallest to largest. 
#' @name Juv.OUTM.S
#' @param Q.cms Flow in cms of the upper Sacramento mainstem (scalar).
#' @param aveT Average temperature during juvenile outmigration.
#' @param tot.div Total amount of water diverted. 
#' @param prop.div Proportion of total flow diverted.
#' @example Juv.OUTM.S(Q.cms = 1010, aveT = 5, tot.div = 0, prop.div = 0) ^ 0.5

Juv.OUTM.S <- function(Q.cms, aveT, tot.div, prop.div) {
  
  B0 <- -7.35
  
  s <- inv.logit(B0 + 0.0092 * Q.cms - 3.51 * prop.div * 0.05 + -0.0021 * tot.div * 0.215) * .5 + 
    inv.logit(B0 + 0.554 * aveT- 3.51 * prop.div * 0.05 + -0.0021 * tot.div * 0.215) * .5
  
  m <- inv.logit(B0+ 0.48 + 0.0092 * Q.cms - 3.51 * prop.div * 0.05 + -0.0021 * tot.div * 0.215) * .5 + 
    inv.logit(B0+ 1.48 + 0.554 * aveT- 3.51 * prop.div * 0.05 + -0.0021 * tot.div * 0.215) * .5
  
  l <- vl <- inv.logit(B0+ 1.223 + 0.0092 * Q.cms - 3.51 * prop.div * 0.05 + -0.0021 * tot.div * 0.215) * .5 + 
    inv.logit(B0+ 2.223 + 0.554 * aveT- 3.51 * prop.div * 0.05 + -0.0021 * tot.div * 0.215) * .5
  
  t(rbind(s, m, l, vl))
}


# refactored--------------------------------------------------------------------
# Juv.OUTM.S <- function(Q.cms, aveT, tot.div, prop.div) {
#   
#   beta <- c(-7.35, 0.0092, 0.554, -0.1755, -0.0004515)
# 
#   score <- beta[1] + 
#     beta[4] * prop.div + 
#     beta[5] * tot.div  
#   
#   score_s1 <- score + beta[2] * Q.cms  
#   score_s2 <- score + beta[3] * aveT
#   
#   score_m1 <- score_s1 + 0.48
#   score_l1 <- score_s1 + 1.223
#   score_m2 <- score_s2 + 1.48
#   score_l2 <- score_s2 + 2.223
#   
#   s <- (inv.logit(score_s1) + inv.logit(score_s2)) * 0.5
#   m <- (inv.logit(score_m1) + inv.logit(score_m2)) * 0.5
#   l <- vl <- (inv.logit(score_l1) + inv.logit(score_l2)) * 0.5
#   
#   return(cbind(s, m, l, vl))
# }

#' Produces size specific estimates of out-migrant survival for juveniles 
#' migrating out of the Delta.  Output is a 2x4 matrix where the rows represent 
#' the north delta (row 1) and south delta (row 2). The columns represent each 
#' size class from smallest to largest.
#' @param Q.cms Flow in cms of the delta mainstem
#' @param pctdiv The percentage of flow being diverted from the delta
#' @param aveT Average temperature of the Delta.

JuvD.OUTM.S <- function(Q.cms, pctdiv, aveT) {
  
  B0 <- -3.15
  
  s <- inv.logit(B0 + 0.0013 * Q.cms) * 0.333 + inv.logit(B0 + 0.386 * aveT) * 0.333 + 
    inv.logit(B0 + -0.033 * pctdiv) * 0.333
  
  m <- inv.logit(B0 + 0.48 + 0.0013 * Q.cms) * 0.333 + inv.logit(B0 + 0.48 + 0.386 * aveT) * 0.333 + 
    inv.logit(B0 + 0.48 + -0.033 * pctdiv) * 0.333 
  
  l <- inv.logit(B0 + 1.223 + 0.0013 * Q.cms) * 0.333 + inv.logit(B0 + 1.223 + 0.386 * aveT) * 0.333 + 
    inv.logit(B0 + 1.223 + -0.033 * pctdiv) * 0.333 
  
  t(rbind(s,m,l,l))
}

# refactored -------------------------------------------------------------------
# JuvD.OUTM.Ss <- function(Q.cms, pctdiv, aveT) {
#   
#   beta <- c(-3.15, 0.0013, 0.386, -0.033)
#   
#   score_s1 <- beta[1] + beta[2] * Q.cms
#   score_s2 <- beta[1] + beta[3] * aveT
#   score_s3 <- beta[1] + beta[4] * pctdiv
#   
#   s <- (inv.logit(score_s1) + inv.logit(score_s2) + inv.logit(score_s3)) * 0.333
#   m <- (inv.logit(score_s1 + 0.48) + inv.logit(score_s2 + 0.48) + inv.logit(score_s3 + 0.48)) * 0.333
#   l <- (inv.logit(score_s1 + 1.223) + inv.logit(score_s2 + 1.223) + inv.logit(score_s3 + 1.223)) * 0.333
#   
#   return(cbind(s,m,l,l))
# }
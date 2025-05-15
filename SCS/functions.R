empty_df = function(r, c, cn) {
  df = data.frame(matrix(NA_real_, nrow = r, ncol = c))
  colnames(df) = cn
  return(df)
}

arraybox = function(i, class, area) {
  shinydashboard::box(
    solidHeader = TRUE, width = "100%",
    col_10(
      p(class[i])
    ),
    col_2(
      p(area[i])
    )
  )
}

calculate_CurveNumber = function(sclass, pp, lu_class, lu_area, SCS_CN_values) {
  df = data.frame(
    LU = paste0(substr(lu_class, start = 1, stop = 15), "..."),
    A_LU = lu_area,
    A_LU_p = lu_area/sum(lu_area) * 100
  )
  
  df$A = lu_area/sum(lu_area) * pp
  df$CN = sapply(lu_class, FUN = function(lu) {
    SCS_CN_values[which(SCS_CN_values$use == lu), sclass]
  })
  df$ACN = df[,4]/100 * df$CN
  
  colnames(df)[1] = paste0("Soil class ", sclass, " (", pp, "%)")
  
  return(df)
}

sumrow = function(df, sclass) {
  
  csum = colSums(df[,2:6])
  
  df[,3] = round(df[,3], 2)
  df[,4] = round(df[,4], 2)
  df[,5] = as.integer(df[,5])
  df[,6] = round(df[,6], 2)
  
  df[nrow(df)+1,] = c("Sum:" , csum[1:3], "-", round(csum[5], 2))
  
  colnames(df)[2:6] = c(
    "A_LU [ha]",
    "A_LU [%]",
    "A [%]",
    "CN",
    "A*CN"
  )
    
  return(df)
  
}

col_1 = function(...) {
  shiny::column(1, ...)
}

col_2 = function(...) {
  shiny::column(2, ...)
}

col_3 = function(...) {
  shiny::column(3, ...)
}

col_4 = function(...) {
  shiny::column(4, ...)
}

col_6 = function(...) {
  shiny::column(6, ...)
}

col_8 = function(...) {
  shiny::column(8, ...)
}

col_9 = function(...) {
  shiny::column(9, ...)
}

col_10 = function(...) {
  shiny::column(10, ...)
}

col_12 = function(...) {
  shiny::column(12, ...)
}







P_eff_dist = function(p, t, I_a, S) {
  
  print(I_a)
  print(S)
  
  df = data.frame(
    i = 1:length(t),
    t = t,
    P = p
  )
  
  df$P_cum = cumsum(df$P)
  df$I_a = rep(I_a, nrow(df))
  df$Fi = S * (df$P_cum - I_a)/(df$P_cum - I_a + S )
  df$Fi[df$P_cum<I_a] = 0
  df$P_effcum = df$P_cum-I_a-df$Fi
  df$P_eff = c(0, diff(df$P_effcum))
  
  return(df)
  
}

PeffPlot = function(df) {
  df$PminusPeff = df$P-df$P_eff
  
  iarunner = unique(df$I_a)
  wi = c()

  for (i in 1:nrow(df)){
    if (iarunner/df$P[i] >= 1) {
      iarunner = iarunner - df$P[i]
      wi = c(wi, 1)
    } else if (iarunner/df$P[i] < 1){
      wi = c(wi, iarunner/df$P[i])
      iarunner = 0
    } else (
      wi = c(wi, 0)
    )
    print(wi)
  }
  
  par(mar = c(1, 4, 4, 2) + 0.1)
  barplot(height = df$P, space = 0, col = '#0000ff', main = "P over t", adj = 0)
  barplot(height = df$PminusPeff, space = 0, col="#92d050", add=T)
  barplot(height = df$P, width = wi, space = 0, col="#ff0000", border = NA, add=T)
  barplot(height = df$P, space = 0, col = 'transparent', add=T)
  
  
}



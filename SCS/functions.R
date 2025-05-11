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

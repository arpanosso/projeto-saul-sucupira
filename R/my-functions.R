estat_names <- c("Min","Q1","Med","Media","Q3",
                 "Max","DP","CV","Skn","Krt")
estat_desc <- function(x){
  x<-na.omit(x)
  m <- mean(x,na.rm = TRUE)
  md <- median(x)
  mini <- min(x,na.rm = TRUE)
  q1 <- quantile(x,.25)
  q3 <- quantile(x,.75)
  maxi <- max(x,na.rm = TRUE)
  dp <- sd(x,na.rm = TRUE)
  cv <- 100*dp/m
  ass <- agricolae::skewness(x)
  curt <- agricolae::kurtosis(x)
  c(mini,q1,md,m,q3,maxi,dp,cv,ass,curt)
}

plot_my_models <- function(modelo_1,modelo_2,modelo_3){
  sqr.f1<-round(attr(modelo_1, "SSErr"),4);
  c01<-round(modelo_1$psill[[1]],4); c0_c11<-round(sum(modelo_1$psill),4);
  a1<-round(modelo_1$range[[2]],2)
  sqr.f2<-round(attr(modelo_2, "SSErr"),4);
  c02<-round(modelo_2$psill[[1]],4); c0_c12<-round(sum(modelo_2$psill),4);
  a2<-round(3*modelo_2$range[[2]],2)
  sqr.f3<-round(attr(modelo_3, "SSErr"),4);
  c03<-round(modelo_3$psill[[1]],4); c0_c13<-round(sum(modelo_3$psill),4);
  a3<-round(modelo_3$range[[2]]*(3^.5),2)

  df_aux <- vari_exp |>
    mutate(
      gamma_m1 = ifelse(dist <= a1, c01 + (c0_c11-c01)*(3/2*(dist/a1)-1/2*(dist/a1)^3),c0_c11),
      gamma_m2 = c02 + (c0_c12-c02)*(1-exp(-3*(dist/a2))),
      gamma_m3 = c03 + (c0_c13-c03)*(1-exp(-(dist*dist/a3/a3))),
      residuo_total = (gamma-mean(gamma))^2,
      residuo_mod_1 = (gamma - gamma_m1)^2,
      residuo_mod_2 = (gamma - gamma_m2)^2,
      residuo_mod_3 = (gamma - gamma_m3)^2
    ) |>
    summarise(
      r2_1=(sum(residuo_total) - sum(residuo_mod_1))/sum(residuo_total),
      r2_2=(sum(residuo_total) - sum(residuo_mod_2))/sum(residuo_total),
      r2_3=(sum(residuo_total) - sum(residuo_mod_3))/sum(residuo_total),
    )
  r21<-as.vector(round(df_aux[1],4))
  r22<-as.vector(round(df_aux[2],4))
  r23<-as.vector(round(df_aux[3],4))
  print(plot(vari_exp,
             model=modelo_1,
             col=1,pl=F,
             pch=16,
             cex=1.2,cex.main=7,
             ylab=list("Semivariância",cex=1.3),
             xlab=list("Distância de Separação h (m)",cex=1.3),
             main =paste("Esf(C0= ",c01,"; C0+C1= ",
                         c0_c11, "; a= ", a1,"; r2 = ",
                         r21,")",sep="")))
  modelo_3$range[[2]] <- a3
  print(plot(vari_exp,model=modelo_2, col=1,pl=F,pch=16,cex=1.2,cex.main=7,ylab=list("Semivariância",cex=1.3),xlab=list("Distância de Separação h (m)",cex=1.3),main =paste("Exp(C0= ",c02,"; C0+C1= ", c0_c12, "; a= ", a2,"; r2 = ", r22,")",sep="")))
  print(plot(vari_exp,model=modelo_3, col=1,pl=F,pch=16,cex=1.2,cex.main=7,ylab=list("Semivariância",cex=1.3),xlab=list("Distância de Separação h (m)",cex=1.3),main =paste("Gau(C0= ",c03,"; C0+C1= ", c0_c13, "; a= ", a3,"; r2 = ", r23,")",sep="")))
}

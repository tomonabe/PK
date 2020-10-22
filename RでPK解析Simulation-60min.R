install.packages("mrgsolve")
library(mrgsolve)

#1-コンパートメントモデル経口投与
code<-" 
$PARAM
Ke = 0.05, Vd = 50, Ka = 0.1
$INIT
Xa=100, X=0
$ODE
dxdt_Xa = -Ka*Xa;
dxdt_X = Ka*Xa - Ke*X;
$CAPTURE
C = X/Vd; "

#グラフを描く
mod<-mcode("oral", code) %>% update(end = 60, delta = 0.1)
mod %>% mrgsim %>% plot

#Kaを変化させてみる
idata <- expand.idata(Ka = c(0.05, 0.1, 0.2))
out <- 
  mod %>% 
  idata_set(idata) %>% 
  mrgsim(end=60)
plot(out, C~.)

#Keを変化させてみる
idatake <- expand.idata(Ke = c(0.025, 0.05, 0.1))
out <- 
  mod %>% 
  idata_set(idatake) %>% 
  mrgsim(end=60)
plot(out, C~.)

#Vdを変化させてみる
idatavd <- expand.idata(Vd = c(25, 50, 100))
out <- 
  mod %>% 
  idata_set(idatavd) %>% 
  mrgsim(end=60)
plot(out, C~.)


#繰り返し投与
code<-" 
$PARAM
Ke = 0.05, Vd = 50, Ka = 0.1
$INIT
Xa=0, X=0
$ODE
dxdt_Xa = -Ka*Xa;
dxdt_X = Ka*Xa - Ke*X;
$CAPTURE
C = X/Vd; "
mod<-mcode("oral", code) %>% update(end = 60, delta = 0.1)

#Keが異なる患者へ24時間ごと100mg繰り返し投与7日間
idatake <- expand.idata(Ke = c(0.025, 0.05, 0.1))
mod %>% 
  ev_rx("100 q 24 x 7") %>%
  idata_set(idatake) %>% 
  mrgsim(end = 180, delta = 0.1) %>% 
  plot(C~time)

#Keが異なる患者へ100mgを24時間ごと14日間つぎに48時間ごと14日間繰り返し投与
idatake <- expand.idata(Ke = c(0.025, 0.05, 0.1))
mod %>% 
  ev_rx("100 q 24 x 14 then 100 q 48 x 14") %>%
  idata_set(idatake) %>% 
  mrgsim(end = 1200, delta = 0.1) %>% 
  plot(C~time)

#Kaが異なる患者へ24時間ごと100mg繰り返し投与7日間
idata <- expand.idata(Ka = c(0.05, 0.1, 0.2))
mod %>% 
  ev_rx("100 q 24 x 7") %>%
  idata_set(idata) %>% 
  mrgsim(end = 180, delta = 0.1) %>% 
  plot(C~time)

#Vdが異なる患者へ24時間ごと100mg繰り返し投与7日間
idatavd <- expand.idata(Vd = c(25, 50, 100))
mod %>% 
  ev_rx("100 q 24 x 7") %>%
  idata_set(idatavd) %>% 
  mrgsim(end = 180, delta = 0.1) %>% 
  plot(C~time)



#1-コンパートメントモデル急速静注
codeiv<-" 
$PARAM
Ke = 0.05, Vd = 50
$INIT
X=100
$ODE
dxdt_X = - Ke*X;
$CAPTURE
C = X/Vd; "

#グラフを描く
modiv<-mcode("iv", codeiv) %>% update(end = 60, delta = 0.1)
modiv %>% mrgsim %>% plot


#1-コンパートメントモデル急速静注繰り返し投与
codeiv<-" 
$PARAM
Ke = 0.05, Vd = 50
$INIT
X=0
$ODE
dxdt_X = - Ke*X;
$CAPTURE
C = X/Vd; "
modiv<-mcode("iv", codeiv) %>% update(end = 60, delta = 0.1)

#Keが異なる患者へ24時間ごと100mg繰り返し投与7日間
idatake <- expand.idata(Ke = c(0.025, 0.05, 0.1))
modiv %>%
  ev_rx("100 q 24 x 7") %>%
  idata_set(idatake) %>% 
  mrgsim(end = 180, delta = 0.1) %>% 
  plot(C~time)



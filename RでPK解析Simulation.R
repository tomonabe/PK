# mrgsolveパッケージをインストール
install.packages("mrgsolve")
library(mrgsolve)

#1-コンパートメントモデル経口投与
code<-" 
$PARAM
Ke = 0.15, Vd = 10, Ka = 0.8
$INIT
Xa=10, X=0
$ODE
dxdt_Xa = -Ka*Xa;
dxdt_X = Ka*Xa - Ke*X;
$CAPTURE
C = X/Vd; "

#グラフを描く
mod<-mcode("oral", code) %>% update(end = 24, delta = 0.1)
mod %>% mrgsim %>% plot

#Kaを変化させてみる
idata <- expand.idata(Ka = c(0.4, 0.8, 1.6))
out <- 
  mod %>% 
  idata_set(idata) %>% 
  mrgsim(end=24)
plot(out, C~.)

#Keを変化させてみる
idatake <- expand.idata(Ke = c(0.075, 0.15, 0.3))
out <- 
  mod %>% 
  idata_set(idatake) %>% 
  mrgsim(end=24)
plot(out, C~.)

#Vdを変化させてみる
idatavd <- expand.idata(Vd = c(5, 10, 20))
out <- 
  mod %>% 
  idata_set(idatavd) %>% 
  mrgsim(end=24)
plot(out, C~.)


#1-コンパートメントモデル経口投与　血中濃度を対数
code2<-" 
$PARAM
Ke = 0.15, Vd = 10, Ka = 0.8
$INIT
Xa=10, X=0
$ODE
dxdt_Xa = -Ka*Xa;
dxdt_X = Ka*Xa - Ke*X;
$CAPTURE
lnC = log(X/Vd); "

#グラフを描く
mod2<-mcode("oral2", code2) %>% update(end = 24, delta = 0.1)
mod2 %>% mrgsim %>% plot

#Kaを変化させてみる　時間を48時間まで
idata <- expand.idata(Ka = c(0.4, 0.8, 1.6))
out <- 
  mod2 %>% 
  idata_set(idata) %>% 
  mrgsim(end=48)
plot(out, lnC~.)

#Keを変化させてみる　時間を48時間まで
idatake <- expand.idata(Ke = c(0.075, 0.15, 0.3))
out <- 
  mod2 %>% 
  idata_set(idatake) %>% 
  mrgsim(end=48)
plot(out, lnC~.)

#Vdを変化させてみる　時間を48時間まで
idatavd <- expand.idata(Vd= c(5, 10, 20))
out <- 
  mod2 %>% 
  idata_set(idatavd) %>% 
  mrgsim(end=48)
plot(out, lnC~.)


#Keが異なる患者へ12時間ごと10 mgを繰り返し投与3日間
idatake <- expand.idata(Ke = c(0.075, 0.15, 0.3))
mod %>% init(Xa=0) %>%
  ev_rx("10 q 12 x 6") %>%
  idata_set(idatake) %>% 
  mrgsim(end = 96, delta = 0.1) %>% 
  plot(C~time)


#繰り返し投与
code<-" 
$PARAM
Ke = 0.15, Vd = 10, Ka = 0.8
$CMT
Xa, X
$ODE
dxdt_Xa = -Ka*Xa;
dxdt_X = Ka*Xa - Ke*X;
$CAPTURE
C = X/Vd; "
mod<-mcode("oral", code) %>% update(end = 24, delta = 0.1)

#Keが異なる患者へ12時間ごと10mg繰り返し投与3日間
idatake <- expand.idata(Ke = c(0.01, 0.05, 0.25))
mod %>%
  ev_rx("10 q 12 x 6") %>%
  idata_set(idatake) %>% 
  mrgsim(end = 72, delta = 0.1) %>% 
  plot(C~time)

#Keが異なる患者へ24時間ごと4mg繰り返し投与30日間
idatake <- expand.idata(Ke = c(0.01, 0.05, 0.25))
mod %>%
  ev_rx("4 q 24 x 30") %>%
  idata_set(idatake) %>% 
  mrgsim(end = 720, delta = 0.1) %>% 
  plot(C~time)



#Keが異なる患者へ12時間ごと10mg繰り返し投与3日間
idatake <- expand.idata(Ke = c(0.075, 0.15, 0.3))
mod %>%
  ev_rx("10 q 12 x 6") %>%
  idata_set(idatake) %>% 
  mrgsim(end = 96, delta = 0.1) %>% 
  plot(C~time)

#Keが異なる患者へ10mgを12時間ごと3日間つぎに24時間ごと4日間繰り返し投与
idatake <- expand.idata(Ke = c(0.075, 0.15, 0.3))
mod %>% 
  ev_rx("10 q 12 x 6 then 10 q 24 x 4") %>%
  idata_set(idatake) %>% 
  mrgsim(end = 192, delta = 0.1) %>% 
  plot(C~time)

#Kaが異なる患者へ12時間ごと10mg繰り返し投与3日間
idata <- expand.idata(Ka = c(0.4, 0.8, 1.6))
mod %>% 
  ev_rx("10 q 12 x 6") %>%
  idata_set(idata) %>% 
  mrgsim(end = 96, delta = 0.1) %>% 
  plot(C~time)

#Vdが異なる患者へ12時間ごと10mg繰り返し投与3日間
idatavd <- expand.idata(Vd = c(5, 10, 20))
mod %>% 
  ev_rx("10 q 12 x 6") %>%
  idata_set(idatavd) %>% 
  mrgsim(end = 96, delta = 0.1) %>% 
  plot(C~time)



#1-コンパートメントモデル急速静注
codeiv<-" 
$PARAM
Ke = 0.15, Vd = 10
$INIT
X=10
$ODE
dxdt_X = - Ke*X;
$CAPTURE
C = X/Vd; "

#グラフを描く
modiv<-mcode("iv", codeiv) %>% update(end = 60, delta = 0.1)
modiv %>% mrgsim %>% plot

#Keが異なる患者へ12時間ごと10mg繰り返し投与3日間
idatake <- expand.idata(Ke = c(0.075, 0.15, 0.3))
modiv %>%
  ev_rx("10 q 12 x 6") %>%
  idata_set(idatake) %>% 
  mrgsim(end = 96, delta = 0.1) %>% 
  plot(C~time)



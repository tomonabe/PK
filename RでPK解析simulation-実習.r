# mrgsolveパッケージをインストール
install.packages("mrgsolve")
library(mrgsolve)
library(tidyverse)

#1-コンパートメントモデル経口投与
code<-" 
$PARAM
Ke = 0.05, Vd = 50, Ka = 0.08333
$INIT
Xa=1000, X=0
$ODE
dxdt_Xa = -Ka*Xa;
dxdt_X = Ka*Xa - Ke*X;
$CAPTURE
C = X/Vd; "

#グラフを描く
mod<-mcode("oral", code) %>% update(end = 60, delta = 0.1)
mod %>% mrgsim %>% plot

#血中濃度のグラフを描く
out <- 
  mod %>% 
  mrgsim(end=60)
plot(out, C~time)

#縦軸が片対数の血中濃度のグラフを描く
plot(out, C~time, logy="true", ylim=c(1,10))

#0.5分までのデータを見る
head(out)

#2から60分のデータを見る
filter(out, time %in% c(2,5,10,15,20,30,40,50,60))

#Vdが違っていた場合の血中濃度変化
code2<-" 
$PARAM
Ke = 0.05556, Vd = 45, Ka = 0.08333
$INIT
Xa=1000, X=0
$ODE
dxdt_Xa = -Ka*Xa;
dxdt_X = Ka*Xa - Ke*X;
$CAPTURE
C = X/Vd; "
mod2<-mcode("oral2", code2) %>% update(end = 60, delta = 0.5)
out2 <- mod2 %>% mrgsim(end=60)
plot(out2, C~time, logy="true", ylim=c(1,10))

#2から60分のデータを見る
filter(out2, time %in% c(2,5,10,20,30,40,50,60))

#流速が2.0mL/minだった場合の血中濃度変化
code3<-" 
$PARAM
Ke = 0.04, Vd = 50, Ka = 0.06667
$INIT
Xa=1000, X=0
$ODE
dxdt_Xa = -Ka*Xa;
dxdt_X = Ka*Xa - Ke*X;
$CAPTURE
C = X/Vd; "
mod3<-mcode("oral3", code3) %>% update(end = 60, delta = 0.5)
out3 <- mod3 %>% mrgsim(end=60)
plot(out3, C~time, logy="true", ylim=c(1,10))

#2から60分のデータを見る
filter(out3, time %in% c(2,5,10,15,20,30,40,50,60))

#投与量が違っていた場合の血中濃度変化
code4<-" 
$PARAM
Ke = 0.05, Vd = 50, Ka = 0.08333
$INIT
Xa=900, X=0
$ODE
dxdt_Xa = -Ka*Xa;
dxdt_X = Ka*Xa - Ke*X;
$CAPTURE
C = X/Vd; "
mod4<-mcode("oral4", code4) %>% update(end = 60, delta = 0.5)
out4 <- mod4 %>% mrgsim(end=60)
plot(out4, C~time, logy="true", ylim=c(1,10))

#2から60分のデータを見る
filter(out4, time %in% c(2,5,10,15,20,30,40,50,60))



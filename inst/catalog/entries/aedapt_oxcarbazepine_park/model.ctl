$PROBLEM Oxcarbazepine active-metabolite population PK - Park
$SUBROUTINES ADVAN2 TRANS2
$INPUT ID TIME EVID AMT RATE II SS CMT DV MDV WT EIAED
$DATA data.csv IGNORE=@
$THETA
 (0, 2.13, 2130) ; CL/F
 (0, 0.666, 666) ; weight exponent
 (0, 0.598, 598) ; KA
 (0, 49, 49000) ; V/F
 (0, 0.312, 312) ; enzyme-inducer effect
$OMEGA
 0.0767
$SIGMA
 0.0571
 2.83
$PK
CL=THETA(1)*(WT/62.8)^THETA(2)*(1+THETA(5)*EIAED)*exp(ETA(1))
KA=THETA(3)
V=THETA(4)*(WT/62.8)
S2=V
$ERROR
Y=F*(1+ERR(1))+ERR(2)
$TABLE ID TIME DV PRED IPRED CWRES NOPRINT ONEHEADER

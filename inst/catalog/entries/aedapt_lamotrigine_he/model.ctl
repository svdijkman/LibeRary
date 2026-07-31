$PROBLEM Lamotrigine - He population PK
$SUBROUTINES ADVAN2 TRANS2
$INPUT ID TIME EVID AMT RATE II SS CMT DV MDV WT COMED_VPA COMED_CBZ COMED_PHB
$DATA data.csv IGNORE=@
$THETA
 (0, 1.01, 1010) ; base CL/F
 (0, 0.635, 635) ; weight exponent
 (-7.53, -0.753, 0) ; valproate log effect
 (0, 0.868, 868) ; carbamazepine log effect
 (0, 0.633, 633) ; phenobarbital log effect
 (0, 16.7, 16700) ; V/F at 27.87 kg
 (0, 1, 1000) ; KA
$OMEGA
 0.067
$SIGMA
 0.045
$PK
CL=THETA(1)*(WT/27.87)^THETA(2)*exp(THETA(3)*COMED_VPA+THETA(4)*COMED_CBZ+THETA(5)*COMED_PHB+ETA(1))
V=THETA(6)*(WT/27.87)
KA=THETA(7)
S2=V
$ERROR
Y=F*(1+ERR(1))
$TABLE ID TIME DV PRED IPRED CWRES NOPRINT ONEHEADER

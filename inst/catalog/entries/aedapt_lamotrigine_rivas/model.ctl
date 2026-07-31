$PROBLEM Lamotrigine - Rivas population PK
$SUBROUTINES ADVAN2 TRANS2
$INPUT ID TIME EVID AMT RATE II SS CMT DV MDV WT COMED_VPA COMED_PHT COMED_PHB COMED_CBZ
$DATA data.csv IGNORE=@
$THETA
 (0, 0.028, 28) ; CL/F per kg
 (-7.13, -0.713, 0) ; valproate log effect
 (0, 0.663, 663) ; phenytoin log effect
 (0, 0.588, 588) ; phenobarbital log effect
 (0, 0.467, 467) ; carbamazepine log effect
 (0, 0.864, 864) ; multiple-inducer log effect
 (0, 1.5, 1500) ; V/F per kg
 (0, 1.3, 1300) ; KA
$OMEGA
 0.07285
$SIGMA
 1.25
$PK
NIND=COMED_PHT+COMED_PHB+COMED_CBZ
IND=ifelse(NIND>1,1,0)
CL=THETA(1)*WT*exp(THETA(2)*COMED_VPA+THETA(3)*COMED_PHT+THETA(4)*COMED_PHB+THETA(5)*COMED_CBZ+THETA(6)*IND+ETA(1))
V=THETA(7)*WT
KA=THETA(8)
S2=V
$ERROR
Y=F+ERR(1)
$TABLE ID TIME DV PRED IPRED CWRES NOPRINT ONEHEADER

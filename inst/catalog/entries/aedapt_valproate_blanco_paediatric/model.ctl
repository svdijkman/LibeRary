$PROBLEM Valproate - Blanco paediatric population PK
$SUBROUTINES ADVAN2 TRANS2
$INPUT ID TIME EVID AMT RATE II SS CMT DV MDV WT DOSE_MG_KG_DAY COMED_CBZ
$DATA data.csv IGNORE=@
$THETA
 (0, 0.012, 12) ; base CL/F
 (0, 0.715, 715) ; weight exponent
 (0, 0.306, 306) ; dose exponent
 (0, 1.359, 1359) ; carbamazepine factor
 (0, 1.9, 1900) ; KA
 (0, 0.24, 240) ; V/F per kg
$OMEGA
 0.046255
$SIGMA
 0.1556
$PK
CL=THETA(1)*(WT^THETA(2))*(DOSE_MG_KG_DAY^THETA(3))*(THETA(4)^COMED_CBZ)*exp(ETA(1))
KA=THETA(5)
V=THETA(6)*WT
S2=V
$ERROR
Y=F*(1+ERR(1))
$TABLE ID TIME DV PRED IPRED CWRES NOPRINT ONEHEADER

$PROBLEM Carbamazepine - Delgado population PK
$SUBROUTINES ADVAN2 TRANS2
$INPUT ID TIME EVID AMT RATE II SS CMT DV MDV WT AGE DOSE_MG_KG_DAY COMED_PHB
$DATA data.csv IGNORE=@
$THETA
 (0, 0.0122, 12.2) ; weight coefficient
 (0, 0.0467, 46.7) ; dose coefficient
 (0, 0.331, 331) ; age exponent
 (0, 0.289, 289) ; phenobarbital effect
 (0, 0.65, 650) ; KA
 (0, 1.5, 1500) ; V/F per kg
 (0, 0.85, 850) ; F1
$OMEGA
 0.118
$SIGMA
 1.52
$PK
CL=(THETA(1)*WT+THETA(2)*DOSE_MG_KG_DAY)*(AGE^THETA(3))*(1+THETA(4)*COMED_PHB)*exp(ETA(1))
KA=THETA(5)
V=THETA(6)*WT
F1=THETA(7)
S2=V
$ERROR
Y=F+ERR(1)
$TABLE ID TIME DV PRED IPRED CWRES NOPRINT ONEHEADER

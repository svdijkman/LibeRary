$PROBLEM Zonisamide - Okada population PK
$SUBROUTINES ADVAN2 TRANS2
$INPUT ID TIME EVID AMT RATE II SS CMT DV MDV WT DAILY_DOSE CYP2C19_HET CYP2C19_PM COMED_CBZ COMED_PHT COMED_PHB
$DATA data.csv IGNORE=@
$THETA
 (0, 1.22, 1220) ; base CL/F
 (0, 0.77, 770) ; weight exponent
 (-1.7, -0.17, 0) ; daily-dose exponent
 (0, 0.84, 840) ; CYP2C19 heterozygote factor
 (0, 0.7, 700) ; CYP2C19 poor-metaboliser factor
 (0, 1.24, 1240) ; carbamazepine factor
 (0, 1.28, 1280) ; phenytoin factor
 (0, 1.29, 1290) ; phenobarbital factor
 (0, 1.23, 1230) FIX ; V/F per kg
 (0, 2, 2000) FIX ; KA
$OMEGA
 0.076
$SIGMA
 0.05
$PK
CL=THETA(1)*(WT/44)^THETA(2)*(DAILY_DOSE^THETA(3))*(THETA(4)^CYP2C19_HET)*(THETA(5)^CYP2C19_PM)*(THETA(6)^COMED_CBZ)*(THETA(7)^COMED_PHT)*(THETA(8)^COMED_PHB)*exp(ETA(1))
V=THETA(9)*WT
KA=THETA(10)
S2=V
$ERROR
Y=F*(1+ERR(1))
$TABLE ID TIME DV PRED IPRED CWRES NOPRINT ONEHEADER

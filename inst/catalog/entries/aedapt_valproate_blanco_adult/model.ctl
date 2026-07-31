$PROBLEM Valproate - Blanco adult population PK
$SUBROUTINES ADVAN2 TRANS2
$INPUT ID TIME EVID AMT RATE II SS CMT DV MDV WT DOSE_MG_KG_DAY COMED_CBZ COMED_PHT COMED_PHB
$DATA data.csv IGNORE=@
$THETA
 (0, 0.004, 4) ; base CL/F per kg
 (0, 0.304, 304) ; dose exponent
 (0, 1.363, 1363) ; carbamazepine factor
 (0, 1.541, 1541) ; phenytoin factor
 (0, 1.397, 1397) ; phenobarbital factor
 (0, 1.2, 1200) ; KA
 (0, 0.2, 200) ; V/F per kg
$OMEGA
 0.0547
$SIGMA
 11.36
$PK
CL=THETA(1)*WT*(DOSE_MG_KG_DAY^THETA(2))*(THETA(3)^COMED_CBZ)*(THETA(4)^COMED_PHT)*(THETA(5)^COMED_PHB)*(1+ETA(1))
KA=THETA(6)
V=THETA(7)*WT
S2=V
$ERROR
Y=F+ERR(1)
$TABLE ID TIME DV PRED IPRED CWRES NOPRINT ONEHEADER

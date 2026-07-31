$PROBLEM Carbamazepine - Jiao population PK
$SUBROUTINES ADVAN2 TRANS2
$INPUT ID TIME EVID AMT RATE II SS CMT DV MDV WT AGE DOSE_MG_KG_DAY COMED_PHT COMED_PHB COMED_VPA_HIGH
$DATA data.csv IGNORE=@
$THETA
 (0, 0.0734, 73.4) ; base CL/F
 (0, 0.406, 406) ; dose exponent
 (0, 0.694, 694) ; weight exponent
 (0, 1.45, 1450) ; phenytoin factor
 (0, 1.17, 1170) ; phenobarbital factor
 (0, 1.21, 1210) ; high-dose valproate factor
 (0, 1.91, 1910) ; age over 65 factor
 (0, 0.849, 849) ; V/F per kg
 (0, 1.2, 1200) ; KA
$OMEGA
 0.0254
 0.01
$SIGMA
 0.975
$PK
ELDERLY=ifelse(AGE>65,1,0)
CL=THETA(1)*(DOSE_MG_KG_DAY^THETA(2))*(WT^THETA(3))*(THETA(4)^COMED_PHT)*(THETA(5)^COMED_PHB)*(THETA(6)^COMED_VPA_HIGH)*(THETA(7)^ELDERLY)*exp(ETA(1))
V=THETA(8)*WT*exp(ETA(2))
KA=THETA(9)
S2=V
$ERROR
Y=F+ERR(1)
$TABLE ID TIME DV PRED IPRED CWRES NOPRINT ONEHEADER

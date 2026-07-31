$PROBLEM Clobazam - Saruwatari population PK
$SUBROUTINES ADVAN2 TRANS2
$INPUT ID TIME EVID AMT RATE II SS CMT DV MDV WT COMED_ZNS COMED_PHB COMED_PHT CYP2C19_HET CYP2C19_PM POR28_CT POR28_TT
$DATA data.csv IGNORE=@
$THETA
 (0, 0.0594, 59.4) ; KA
 (0, 13.3, 13300) ; V/F coefficient
 (0, 0.136, 136) ; V/F weight exponent
 (0, 0.511, 511) ; CL/F coefficient
 (0, 0.54, 540) ; CL/F weight exponent
 (0, 0.484, 484) ; zonisamide factor
 (0, 1.66, 1660) ; phenobarbital factor
 (0, 1.93, 1930) ; phenytoin factor
 (0, 0.944, 944) ; CYP2C19 heterozygote factor
 (0, 0.819, 819) ; CYP2C19 poor-metaboliser factor
 (0, 1.02, 1020) ; POR*28 CT factor
 (0, 1.44, 1440) ; POR*28 TT factor
$OMEGA
 1.91e-05
 0.0669
 3.6e-09
$SIGMA
 0.107
$PK
KA=THETA(1)*exp(ETA(1))
CL=THETA(4)*(WT^THETA(5))*(THETA(6)^COMED_ZNS)*(THETA(7)^COMED_PHB)*(THETA(8)^COMED_PHT)*(THETA(9)^CYP2C19_HET)*(THETA(10)^CYP2C19_PM)*(THETA(11)^POR28_CT)*(THETA(12)^POR28_TT)*exp(ETA(2))
V=THETA(2)*(WT^THETA(3))*exp(ETA(3))
S2=V*0.001
$ERROR
Y=F*(1+ERR(1))
$TABLE ID TIME DV PRED IPRED CWRES NOPRINT ONEHEADER

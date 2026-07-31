$PROBLEM Phenobarbital - Goto population PK
$SUBROUTINES ADVAN2 TRANS2
$INPUT ID TIME EVID AMT RATE II SS CMT DV MDV WT CYP2C9_13 COMED_VPA COMED_PHT SEVERE_MID
$DATA data.csv IGNORE=@
$THETA
 (0, 14.78, 14780) ; V/F
 (0, 0.23, 230) ; CL/F at 40 kg
 (0, 0.21, 210) ; weight exponent
 (0, 0.53, 530) ; CYP2C9*1/*3 factor
 (0, 0.68, 680) ; valproate factor
 (0, 0.85, 850) ; phenytoin factor
 (0, 0.85, 850) ; severe mental/intellectual disability factor
 (0, 2, 2000) FIX ; KA
$OMEGA
 2.93
 0.03
$SIGMA
 12.2
$PK
V=THETA(1)*(1+ETA(1))
CL=THETA(2)*(WT/40)^THETA(3)*(THETA(4)^CYP2C9_13)*(THETA(5)^COMED_VPA)*(THETA(6)^COMED_PHT)*(THETA(7)^SEVERE_MID)*(1+ETA(2))
KA=THETA(8)
S2=V
$ERROR
Y=F+ERR(1)
$TABLE ID TIME DV PRED IPRED CWRES NOPRINT ONEHEADER

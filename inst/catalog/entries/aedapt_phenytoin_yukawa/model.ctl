$PROBLEM Phenytoin - Yukawa nonlinear population PK
$SUBROUTINES ADVAN13
$INPUT ID TIME EVID AMT RATE II SS CMT DV MDV WT AGE COMED_AED POWDER_FORMULATION DAILY_DOSE
$DATA data.csv IGNORE=@
$MODEL
  COMP=(DEPOT,DEFDOSE)
  COMP=(CENTRAL,DEFOBSERVATION)
$THETA
 (0, 325, 325000) ; VM at 60 kg per day
 (0, 2.41, 2410) ; KM
 (0, 0.737, 737) ; weight exponent
 (0, 0.752, 752) ; age over 15 factor
 (0, 1.08, 1080) ; comedication VM factor
 (0, 1.32, 1320) ; comedication KM factor
 (0, 9.92, 9920) ; powder bioavailability coefficient
 (0, 1.23, 1230) FIX ; V per kg
 (0, 10, 10000) FIX ; KA
$OMEGA
 0.0366
 0.335
 0.206
$SIGMA
 0.0106
$PK
AGE15=ifelse(AGE>15,1,0)
KM=THETA(2)*(THETA(4)^AGE15)*(THETA(6)^COMED_AED)*exp(ETA(1))
VM=THETA(1)*(WT/60)^THETA(3)*(THETA(5)^COMED_AED)*exp(ETA(2))
V=THETA(8)*WT*exp(ETA(3))
KA=THETA(9)
F1=1-POWDER_FORMULATION*exp(-THETA(7)/DAILY_DOSE)
S2=V
$DES
DADT(1)=-KA*A(1)
C=A(2)/V
DADT(2)=KA*A(1)-(VM/24)*C/(KM+C)
$ERROR
Y=F*(1+ERR(1))
$TABLE ID TIME DV PRED IPRED CWRES NOPRINT ONEHEADER

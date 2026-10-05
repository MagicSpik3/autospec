* Encoding: windows-1252.
********************************************************************************************************
*
*
*   RELATIONSHIPS AND EDUCATION Base Checks 
*
********************************************************************************************************



***************************************************************************************
*
*   STAGE 1 - BASE / ROUTING CHECKS
*
*   a)   Open the classificatory file
*   b)   Check individual outcome codes
*   c)   Check missing and changed values for var_b03
*   d)   Check Age / birthdate values
*   e)   Check var_a07 populated for all 16+
*   f)   Check consistency of MarSta and var_a07
*   g)   Check MarBef asked for all those who are married and living with spouse
*   h)   Check var_a08 asked for all those aged 16+ not married / Civil Partners in
*        households greater than 1 var_b02 in size
*   i)   Check var_b07 variable is populated properly
*   j)   Check PartNum is populated when var_b07 = Yes
*   k)   Check that SSPartner and SSPNo match var_b07 and PartNum
*   l)   var_b09 should by populated for all those who have partners in the household
*   m)   var_b10 should be consistent with the responses given at var_b07 and var_b09
*   n)   Check HHldr is populated for all cases
*   o)   Save the file 
*
***************************************************************************************



SORT CASES BY var_b01 var_b02.


*   b)   Recalculate var_a01, var_a02, var_a03, var_a04, var_a05 and var_a06 to account for any changes to var_b04 made during linkage work.
*            Amended ages in var_a06 derivation to match retirement ages for R9 period.

FRE VAR var_a01.
DO IF RANGE(var_b04,0,24).
   COMPUTE var_a01 = 1.
ELSE IF RANGE(var_b04,25,64).
   COMPUTE var_a01 = 2.
ELSE IF RANGE(var_b04,65,74).
   COMPUTE var_a01 = 3.
ELSE IF var_b04 >= 75.
   COMPUTE var_a01 = 4.
END IF.
FRE VAR var_a01.

FRE VAR var_a02.
DO IF RANGE(var_b04,0,15).
   COMPUTE var_a02 = 1.
ELSE IF RANGE(var_b04,16,34).
   COMPUTE var_a02 = 2.
ELSE IF RANGE(var_b04,35,54).
   COMPUTE var_a02 = 3.
ELSE IF RANGE(var_b04,55,74).
   COMPUTE var_a02 = 4.
ELSE IF var_b04 >= 75.
   COMPUTE var_a02 = 5.
END IF.
FRE VAR var_a02.

FRE VAR var_a03.
DO IF RANGE(var_b04,0,15).
   COMPUTE var_a03 = 1.
ELSE IF RANGE(var_b04,16,24).
   COMPUTE var_a03 = 2.
ELSE IF RANGE(var_b04,25,44).
   COMPUTE var_a03 = 3.
ELSE IF RANGE(var_b04,45,64).
   COMPUTE var_a03 = 4.
ELSE IF RANGE(var_b04,65,74).
   COMPUTE var_a03 = 5.
ELSE IF var_b04 >= 75.
   COMPUTE var_a03 = 6.
END IF.
FRE VAR var_a03.

FRE VAR var_a04.
DO IF RANGE(var_b04,0,15).
   COMPUTE var_a04 = 1.
ELSE IF RANGE(var_b04,16,24).
   COMPUTE var_a04 = 2.
ELSE IF RANGE(var_b04,25,34).
   COMPUTE var_a04 = 3.
ELSE IF RANGE(var_b04,35,44).
   COMPUTE var_a04 = 4.
ELSE IF RANGE(var_b04,45,54).
   COMPUTE var_a04 = 5.
ELSE IF RANGE(var_b04,55,64).
   COMPUTE var_a04 = 6.
ELSE IF RANGE(var_b04,65,74).
   COMPUTE var_a04 = 7.
ELSE IF RANGE(var_b04,75,84).
   COMPUTE var_a04 = 8.
ELSE IF var_b04 >= 85.
   COMPUTE var_a04 = 9.
END IF.
FRE VAR var_a04.

FRE VAR var_a05.
DO IF RANGE(var_b04,0,4).
   COMPUTE var_a05 = 1.
ELSE IF RANGE(var_b04,5,10).
   COMPUTE var_a05 = 2.
ELSE IF RANGE(var_b04,11,15).
   COMPUTE var_a05 = 3.
ELSE IF RANGE(var_b04,16,19).
   COMPUTE var_a05 = 4.
ELSE IF RANGE(var_b04,20,24).
   COMPUTE var_a05 = 5.
ELSE IF RANGE(var_b04,25,29).
   COMPUTE var_a05 = 6.
ELSE IF RANGE(var_b04,30,34).
   COMPUTE var_a05 = 7.
ELSE IF RANGE(var_b04,35,39).
   COMPUTE var_a05 = 8.
ELSE IF RANGE(var_b04,40,44).
   COMPUTE var_a05 = 9.
ELSE IF RANGE(var_b04,45,49).
   COMPUTE var_a05 = 10.
ELSE IF RANGE(var_b04,50,54).
   COMPUTE var_a05 = 11.
ELSE IF RANGE(var_b04,55,59).
   COMPUTE var_a05 = 12.
ELSE IF RANGE(var_b04,60,64).
   COMPUTE var_a05 = 13.
ELSE IF RANGE(var_b04,65,69).
   COMPUTE var_a05 = 14.
ELSE IF RANGE(var_b04,70,74).
   COMPUTE var_a05 = 15.
ELSE IF RANGE(var_b04,75,79).
   COMPUTE var_a05 = 16.
ELSE IF RANGE(var_b04,80,84).
   COMPUTE var_a05 = 17.
ELSE IF var_b04 >= 85.
   COMPUTE var_a05 = 18.
END IF.
FRE VAR var_a05.

FRE VAR var_a06.
DO IF (var_b03 = 1).
   RECODE
      var_b04
         (0 thru 15 = 1)
         (16 thru 65 = 2)
         (66 thru HI = 3) INTO var_a06.
ELSE IF (var_b03 = 2).
   RECODE
      var_b04
         (0 thru 15 = 1)
         (16 thru 65 = 2)
         (66 thru HI = 3) INTO var_a06.
END IF.
FRE VAR var_a06.



*   f)   Rederive var_a09 to account for any changes above to var_b04, var_a07 and var_a08.
*           Requires recode of var_a07 before running. "R9 Response Recodes" syntax.

FRE VAR var_a09.
DO IF (var_a07 = 2).
   COMPUTE var_a09 = 1.
ELSE IF (var_a07 = 6).
   COMPUTE var_a09 = 8.
ELSE IF (var_a08 = 1).
   COMPUTE var_a09 = 2.
ELSE IF (var_b04 < 16).
   COMPUTE var_a09 = 3.
ELSE.
   RECODE
      var_a07
         (1 = 3)
         (4 = 5)         
         (5 = 4)
         (3 = 6)
         (7,8,9 = 9)
         (ELSE = Copy) INTO var_a09.
END IF.
FRE VAR var_a09.



*   k)   Produce aggregated household dataset which totals the number of people who are married;
*        the number of people who are cohabiting; the number of people in same-var_b03 couples and the
*        the number of people in Civil Partnerships. Match this information onto the file.
*        No changes.

COMPUTE var_c01 = 0.
COMPUTE var_c02 = 0.
COMPUTE var_c03 = 0.
COMPUTE var_c04 = 0.

DO IF (var_a09 = 1).
   COMPUTE var_c01 = 1.
ELSE IF (var_a09 = 2).
   COMPUTE var_c02 = 2.
ELSE IF (var_a09 = 7).
   COMPUTE var_c03 = 1.
ELSE IF (var_a09 = 8).
   COMPUTE var_c04 = 1.
END IF.
EXE.

AGGREGATE 
   /OUTFILE = * MODE = ADDVARIABLES
   /BREAK = var_b01
   /var_c05 = SUM(var_c01)
   /var_c06 = SUM(var_c02)
   /var_c07 = SUM(var_c03)
   /var_c08 = SUM(var_c04).

 

* Checks for odd numbers of individuals stated as couples.

TEMPORARY.
SELECT IF ANY(var_c05,1,3,5,7,9,11,13,15).
LIST var_b01 var_b02 var_b03 var_b04 var_c01 var_c05 var_a07 var_a09 var_a08 var_b07 var_b08 var_r01 to var_r10.

TEMPORARY.
SELECT IF ANY(var_c06,1,3,5,7,9,11,13,15).
LIST var_b01 var_b02 var_b03 var_b04 var_c02 var_c06 var_a07 var_a09 var_a08 var_b07 var_b08 var_r01 to var_r10.

TEMPORARY.
SELECT IF ANY(var_c07,1,3,5,7,9,11,13,15).
LIST var_b01 var_b02 var_b03 var_b04 var_c03 var_c07 var_a07 var_a09 var_a08 var_b07 var_b08 var_r01 to var_r10.

TEMPORARY.    
SELECT IF ANY(var_c08,1,3,5,7,9,11,13,15) OR (var_a07 = -8).
LIST var_b01 var_b02 var_b03 var_b04 var_c04 var_c08 var_a07 var_a09 var_a08 var_b07 var_b08 var_r01 to var_r10.


*   l)   Check cases where var_a09 = 1,2,8 and var_b07 <> 1, and var_a09 <> 1,2,8 and var_b07 = 2.
*        No changes.

TEMP.
SELECT IF ((ANY(var_a09,1,2,8) AND var_b07 <> 1) OR (NOT(ANY(var_a09,1,2,8)) AND var_b07 <> 2)).
LIST VAR var_b01 var_b02 var_a07 var_a09 var_b07 var_b08.


*   m)   Check PartNum has value 1-16 if var_b07 = 1 and 17 if var_b07 = 2.
*        No changes.

TEMP.
SELECT IF ((var_b07 = 1 AND NOT(RANGE(var_b08,1,16))) OR (var_b07 = 2 AND var_b08 <> 17)).
LIST VAR var_b01 var_b02 var_b04 var_b12 var_b13 var_b07 var_b08.



************************
*** Stop here. ***
************************



*   n)   Check that var_b09 is populated for all those who have partners in the household (for discrepancies, you can use var_b08
         to check find their var_b07 in the dataset and check their details).
*        No changes.

FRE VAR var_b09.

DO IF var_b07 = 1.
   DO IF ((var_b01 = LAG(var_b01,1)) AND (var_b08 = LAG(var_b02,1))).
      DO IF var_b03 = LAG(var_b03,1).
         COMPUTE var_b09 = 1.
      ELSE.
         COMPUTE var_b09 = 2.
      END IF.
   ELSE IF ((var_b01 = LAG(var_b01,2)) AND (var_b08 = LAG(var_b02,2))).
      DO IF var_b03 = LAG(var_b03,2).
         COMPUTE var_b09 = 1.
      ELSE.
         COMPUTE var_b09 = 2.
      END IF.
   ELSE IF ((var_b01 = LAG(var_b01,3)) AND (var_b08 = LAG(var_b02,3))).
      DO IF var_b03 = LAG(var_b03,3).
         COMPUTE var_b09 = 1.
      ELSE.
         COMPUTE var_b09 = 2.
      END IF.
   ELSE IF ((var_b01 = LAG(var_b01,4)) AND (var_b08 = LAG(var_b02,4))).
      DO IF var_b03 = LAG(var_b03,4).
         COMPUTE var_b09 = 1.
      ELSE.
         COMPUTE var_b09 = 2.
      END IF.
   ELSE IF ((var_b01 = LAG(var_b01,5)) AND (var_b08 = LAG(var_b02,5))).
      DO IF var_b03 = LAG(var_b03,5).
         COMPUTE var_b09 = 1.
      ELSE.
         COMPUTE var_b09 = 2.
      END IF.
   ELSE IF ((var_b01 = LAG(var_b01,6)) AND (var_b08 = LAG(var_b02,6))).
      DO IF var_b03 = LAG(var_b03,6).
         COMPUTE var_b09 = 1.
      ELSE.
         COMPUTE var_b09 = 2.
      END IF.
   ELSE IF ((var_b01 = LAG(var_b01,7)) AND (var_b08 = LAG(var_b02,7))).
      DO IF var_b03 = LAG(var_b03,7).
         COMPUTE var_b09 = 1.
      ELSE.
         COMPUTE var_b09 = 2.
      END IF.
   ELSE IF ((var_b01 = LAG(var_b01,8)) AND (var_b08 = LAG(var_b02,8))).
      DO IF var_b03 = LAG(var_b03,8).
         COMPUTE var_b09 = 1.
      ELSE.
         COMPUTE var_b09 = 2.
      END IF.
   ELSE IF ((var_b01 = LAG(var_b01,9)) AND (var_b08 = LAG(var_b02,9))).
      DO IF var_b03 = LAG(var_b03,9).
         COMPUTE var_b09 = 1.
      ELSE.
         COMPUTE var_b09 = 2.
      END IF.
   ELSE IF ((var_b01 = LAG(var_b01,10)) AND (var_b08 = LAG(var_b02,10))).
      DO IF var_b03 = LAG(var_b03,10).
         COMPUTE var_b09 = 1.
      ELSE.
         COMPUTE var_b09 = 2.
      END IF.
   ELSE IF ((var_b01 = LAG(var_b01,11)) AND (var_b08 = LAG(var_b02,11))).
      DO IF var_b03 = LAG(var_b03,11).
         COMPUTE var_b09 = 1.
      ELSE.
         COMPUTE var_b09 = 2.
      END IF.
   ELSE IF ((var_b01 = LAG(var_b01,12)) AND (var_b08 = LAG(var_b02,12))).
      DO IF var_b03 = LAG(var_b03,12).
         COMPUTE var_b09 = 1.
      ELSE.
         COMPUTE var_b09 = 2.
      END IF.
   ELSE IF ((var_b01 = LAG(var_b01,13)) AND (var_b08 = LAG(var_b02,13))).
      DO IF var_b03 = LAG(var_b03,13).
         COMPUTE var_b09 = 1.
      ELSE.
         COMPUTE var_b09 = 2.
      END IF.
   ELSE IF ((var_b01 = LAG(var_b01,14)) AND (var_b08 = LAG(var_b02,14))).
      DO IF var_b03 = LAG(var_b03,14).
         COMPUTE var_b09 = 1.
      ELSE.
         COMPUTE var_b09 = 2.
      END IF.
   ELSE IF ((var_b01 = LAG(var_b01,15)) AND (var_b08 = LAG(var_b02,15))).
      DO IF var_b03 = LAG(var_b03,15).
         COMPUTE var_b09 = 1.
      ELSE.
         COMPUTE var_b09 = 2.
      END IF.
   ELSE IF ((var_b01 = LAG(var_b01,16)) AND (var_b08 = LAG(var_b02,16))).
      DO IF var_b03 = LAG(var_b03,16).
         COMPUTE var_b09 = 1.
      ELSE.
         COMPUTE var_b09 = 2.
      END IF.
   END IF.
ELSE.
   COMPUTE var_b09 = -9.
END IF.
SORT CASES BY var_b01 var_b02(D).
DO IF var_b07 = 1.
   DO IF ((var_b01 = LAG(var_b01,1)) AND (var_b08 = LAG(var_b02,1))).
      DO IF var_b03 = LAG(var_b03,1).
         COMPUTE var_b09 = 1.
      ELSE.
         COMPUTE var_b09 = 2.
      END IF.
   ELSE IF ((var_b01 = LAG(var_b01,2)) AND (var_b08 = LAG(var_b02,2))).
      DO IF var_b03 = LAG(var_b03,2).
         COMPUTE var_b09 = 1.
      ELSE.
         COMPUTE var_b09 = 2.
      END IF.
   ELSE IF ((var_b01 = LAG(var_b01,3)) AND (var_b08 = LAG(var_b02,3))).
      DO IF var_b03 = LAG(var_b03,3).
         COMPUTE var_b09 = 1.
      ELSE.
         COMPUTE var_b09 = 2.
      END IF.
   ELSE IF ((var_b01 = LAG(var_b01,4)) AND (var_b08 = LAG(var_b02,4))).
      DO IF var_b03 = LAG(var_b03,4).
         COMPUTE var_b09 = 1.
      ELSE.
         COMPUTE var_b09 = 2.
      END IF.
   ELSE IF ((var_b01 = LAG(var_b01,5)) AND (var_b08 = LAG(var_b02,5))).
      DO IF var_b03 = LAG(var_b03,5).
         COMPUTE var_b09 = 1.
      ELSE.
         COMPUTE var_b09 = 2.
      END IF.
   ELSE IF ((var_b01 = LAG(var_b01,6)) AND (var_b08 = LAG(var_b02,6))).
      DO IF var_b03 = LAG(var_b03,6).
         COMPUTE var_b09 = 1.
      ELSE.
         COMPUTE var_b09 = 2.
      END IF.
   ELSE IF ((var_b01 = LAG(var_b01,7)) AND (var_b08 = LAG(var_b02,7))).
      DO IF var_b03 = LAG(var_b03,7).
         COMPUTE var_b09 = 1.
      ELSE.
         COMPUTE var_b09 = 2.
      END IF.
   ELSE IF ((var_b01 = LAG(var_b01,8)) AND (var_b08 = LAG(var_b02,8))).
      DO IF var_b03 = LAG(var_b03,8).
         COMPUTE var_b09 = 1.
      ELSE.
         COMPUTE var_b09 = 2.
      END IF.
   ELSE IF ((var_b01 = LAG(var_b01,9)) AND (var_b08 = LAG(var_b02,9))).
      DO IF var_b03 = LAG(var_b03,9).
         COMPUTE var_b09 = 1.
      ELSE.
         COMPUTE var_b09 = 2.
      END IF.
   ELSE IF ((var_b01 = LAG(var_b01,10)) AND (var_b08 = LAG(var_b02,10))).
      DO IF var_b03 = LAG(var_b03,10).
         COMPUTE var_b09 = 1.
      ELSE.
         COMPUTE var_b09 = 2.
      END IF.
   ELSE IF ((var_b01 = LAG(var_b01,11)) AND (var_b08 = LAG(var_b02,11))).
      DO IF var_b03 = LAG(var_b03,11).
         COMPUTE var_b09 = 1.
      ELSE.
         COMPUTE var_b09 = 2.
      END IF.
   ELSE IF ((var_b01 = LAG(var_b01,12)) AND (var_b08 = LAG(var_b02,12))).
      DO IF var_b03 = LAG(var_b03,12).
         COMPUTE var_b09 = 1.
      ELSE.
         COMPUTE var_b09 = 2.
      END IF.
   ELSE IF ((var_b01 = LAG(var_b01,13)) AND (var_b08 = LAG(var_b02,13))).
      DO IF var_b03 = LAG(var_b03,13).
         COMPUTE var_b09 = 1.
      ELSE.
         COMPUTE var_b09 = 2.
      END IF.
   ELSE IF ((var_b01 = LAG(var_b01,14)) AND (var_b08 = LAG(var_b02,14))).
      DO IF var_b03 = LAG(var_b03,14).
         COMPUTE var_b09 = 1.
      ELSE.
         COMPUTE var_b09 = 2.
      END IF.
   ELSE IF ((var_b01 = LAG(var_b01,15)) AND (var_b08 = LAG(var_b02,15))).
      DO IF var_b03 = LAG(var_b03,15).
         COMPUTE var_b09 = 1.
      ELSE.
         COMPUTE var_b09 = 2.
      END IF.
   ELSE IF ((var_b01 = LAG(var_b01,16)) AND (var_b08 = LAG(var_b02,16))).
      DO IF var_b03 = LAG(var_b03,16).
         COMPUTE var_b09 = 1.
      ELSE.
         COMPUTE var_b09 = 2.
      END IF.
   END IF.
ELSE.
   COMPUTE var_b09 = -9.
END IF.


SORT CASES BY var_b01 var_b02.

FRE VAR var_b09.



*   o)   Check that var_b10 (type of partnership) = 1 when var_b09 = Yes and var_b03 = Male, var_b10 = 2 when var_b09 = Yes and var_b03 = Female,
         var_b10 = 3 when var_b09 = No.
*        No changes.

FRE VAR var_b10.
DO IF var_b07 = 1.
   DO IF (var_b09 = 1).
      DO IF (var_b03 = 1).
         COMPUTE var_b10 = 1.
      ELSE IF (var_b03 = 2).
         COMPUTE var_b10 = 2.
      END IF.
   ELSE.
      COMPUTE var_b10 = 3.
   END IF.
ELSE.
   COMPUTE var_b10 = -9.
END IF.
FRE VAR var_b10.



*   p)   Check that var_b11 is populated for all cases - manually check whether there are any unlabelled values
*        and try to assign these to categories - may take work in CaseBook.
*        No changes.

FRE VAR var_b11.
RECODE
   var_b11
      (-9 = -8).
FRE VAR var_b11.



*   q)   Compute variable for HH respondent's var_b05, var_a07 and var_a08.
*        Ameneded variable names as still using R7 suffix


COMPUTE var_d01 = 0.
COMPUTE var_d02 = 0.
COMPUTE var_d03 = 0.
DO IF (var_b02 = var_b06).
   COMPUTE var_d01 = var_b05.
   COMPUTE var_d02 = var_a07.
   COMPUTE var_d03 = var_a08.
END IF.


AGGREGATE
   /OUTFILE = * MODE = ADDVARIABLES
   /BREAK = var_b01
   /var_d04 = SUM(var_d01)
   /var_d05 = SUM(var_d02)
   /var_d06 = SUM(var_d03).
EXE.



FORMATS
   var_d04 var_d05 var_d06 (F2).









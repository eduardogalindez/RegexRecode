#This R program will take 2 arrays(one with regular expressions and the other with a replacement for that regular expression), and it send these two arrays
#to a function that iterates all the regular expressions through a vector and replaces the ones that match with their respective replacement.
#In this program, I applied it to mortality codes and recoded them so that they where easier to recognize.

#Array with all the regular expressions
RExp <-c('A1[6-9].+','A5[0-3].?','B2[0-4]','C16.?','C1[8-9].?|C2[0-1].?','C25.?',
         'C3[3-4].?','C50.?','C5[3-6].?','C61.?','C6[4-8].?','C8[2-5].?','C9[1-5].?',
       'C0[0-9].?|C1[0-5].?|C17.?|C2[2-4].?|C2[6-9].?|C3[0-2].?|C3[7-9].?|C4[0-9].?|C5[1-2].?|C5[7-9].?|C60.?|C6[2-3].?|C69.?|C7[0-9].?|C8[0-1].?|C88.?|C90.?|C9[6-7].?'
       ,'C3[5-6].?|C8[6-7].?','E1[0-4].?','G30.?','I11.?|I13.?','I2[0-5].?'
       ,'I0[0-9].?|I2[6-9].?|I[3-4][0-9].?|I5[0-1].?', 'I10.?|I12.?','I6[0-9].?'
       ,'I70.?','I7[1-8].?','I12.?|I1[4-9].?|I5[2-9].?','J1[0-8].?','J4[0-7].?'
       ,'K2[5-8].?','K70.?|K7[3-4].?','N0[0-7].?|N1[7-9].?|N2[5-7].?','O[0-9][0-9].?'
       ,'P[0-8][0-9].?|P9[0-6].?','Q[0-9][0-9].?','R95.?','R[0-8][0-9].?|R9[0-4].?|R9[6-9].?',
       'A0[0-9].?|A[2-4][0-9].?|A5[4-9].?|A[6-9][0-9].?|B[0-1][0-9].?|B2[5-9].?|B[3-9][0-9].?|D[0-9][0-9].?|E0[0-7].?|E1[5-9].?|E[2-9][0-9].?|F[0-9][0-9].?|G[0-1][0-9].?|G2[0-5].?|G3[1-9].?|G[4-9][0-9].?|H[0-8][0-9].?|H9[0-3].?|I[8-9][0-9].?|J0[0-6].?|J[2-3][0-9].?|J[6-9][0-9].?|K[0-1][0-9].?|K2[0-2].?|K29.?|K[3-5][0-9].?|K6[0-6].?|K7[1-2].?|K7[5-9].?|K[8-9][0-9].?|M[0-9][0-9].?|N1[0-5].?|N2[0-3].?|N2[8-9].?|N[3-8][0-9].?|N9[0-8].?'
       ,'V0[2-4].?|V090.?|V1[2-4].?|V19[0-2].?|V19[4-6].?|V[2-7][0-9].?|V80[3-5].?|V81[0-1].?|V82[0-1]|V8[3-6].?|V87[0-8].?|V88[0-8].?|V890.?|V892.?'
       ,'V01.?|V0[5-6].?|V091.?|V09[3-9].?|V1[0-1].?|V1[5-8].?|V193.?|V19[8-9].?|V80[0-2].?|V80[6-9].?|V81[2-9].?|V82[2-9].?|V879.?|V889.?|V891.?|V893.?|V899.?|V9[0-9].?|W[0-9][0-9].?|X[0-5][0-9].?|Y[4-7][0-9].?|Y8[0-6].?|Y88.?'
       ,'U03.?|X[6-7][0-9].?|X8[0-4].?|Y87.0.?','U0[1-2].?|X8[5-9].?|X9[0-9].?|Y0[0-9].?|Y871.?',
       'Y[1-2][0-9].?|Y3[0-6].?|Y872.?|Y89.?','[S-T][0-9][0-9].?'
       )
#Replacements
Recode <- c('001','002','003','005','006','007',
          '008','009','010','011','012','013',
          '014','015','004','016','017','020',
          '021','022','023','024','025','026',
          '018','027','028','029','030','031',
          '032','033','034','035','036','037',
          '038','039','040','041','042','039')
#This part can be omited since it is related to the work I was doing
load("~/Desktop/mortality/2009morterPR.RData")
#Make a vector off of the table I had and make a new shorter one after that.
econd <- mortdataPR$econd1
econdnew<- econd[1:200]

#Function that takes two arrays and a vector as parameters, the first array is made of regular expressions
#the second array is made of the replacements for the regular expressions and the vector is where the changes will be made.
#It works by calling a for function through the first array and calling the sub function as many times as there are elements on the array's.
#It returns a vector with the changes that where made.
CodeToRecode <- function(RegularExp,CodeReplacement , CodeList){
for(i in 1:length(RegularExp))
      CodeList<-sub(RegularExp[i],CodeReplacement[i],CodeList, perl = TRUE)
return (CodeList)
}
#Called the vector, then the function and called the vector again to see the changes.
econdnew
econdnew <- CodeToRecode(RExp, Recode, econdnew)
econdnew
   
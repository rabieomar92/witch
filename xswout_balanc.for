!     --------------------------------------------------------
!     THIS FORTRAN SOURCE FILE WAS GENERATED USING FORTRAN-IT.
!     --------------------------------------------------------
!     Generation time: 10/10/2017 9:49:12 AM
!     Author         : 
!     Device Name    : RABIEMSI
!     Project Name   : XSWOUT.fip
!     --------------------------------------------------------
      
      
!     Begin your code here:
      
      SUBROUTINE BALANC(NM,N,A,LOW,IGH,SCALE)                           EILI 116
C                                                                       EILI 117
      INTEGER I,J,K,L,M,N,JJ,NM,IGH,LOW,IEXC                            EILI 118
      DOUBLE PRECISION A(NM,N),SCALE(N)                                 EILI 119
      DOUBLE PRECISION C,F,G,R,S,B2,RADIX                               EILI 120
      LOGICAL NOCONV                                                    EILI 121
C                                                                       EILI 122
C     THIS SUBROUTINE IS A TRANSLATION OF THE ALGOL PROCEDURE BALANCE,  EILI 123
C     NUM. MATH. 13, 293-304(1969) BY PARLETT AND REINSCH.              EILI 124
C     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 315-326(1971).   EILI 125
C                                                                       EILI 126
C     THIS SUBROUTINE BALANCES A REAL MATRIX AND ISOLATES               EILI 127
C     EIGENVALUES WHENEVER POSSIBLE.                                    EILI 128
C                                                                       EILI 129
C     ON INPUT                                                          EILI 130
C                                                                       EILI 131
C        NM MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL         EILI 132
C          ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM          EILI 133
C          DIMENSION STATEMENT.                                         EILI 134
C                                                                       EILI 135
C        N IS THE ORDER OF THE MATRIX.                                  EILI 136
C                                                                       EILI 137
C        A CONTAINS THE INPUT MATRIX TO BE BALANCED.                    EILI 138
C                                                                       EILI 139
C     ON OUTPUT                                                         EILI 140
C                                                                       EILI 141
C        A CONTAINS THE BALANCED MATRIX.                                EILI 142
C                                                                       EILI 143
C        LOW AND IGH ARE TWO INTEGERS SUCH THAT A(I,J)                  EILI 144
C          IS EQUAL TO ZERO IF                                          EILI 145
C           (1) I IS GREATER THAN J AND                                 EILI 146
C           (2) J=1,...,LOW-1 OR I=IGH+1,...,N.                         EILI 147
C                                                                       EILI 148
C        SCALE CONTAINS INFORMATION DETERMINING THE                     EILI 149
C           PERMUTATIONS AND SCALING FACTORS USED.                      EILI 150
C                                                                       EILI 151
C     SUPPOSE THAT THE PRINCIPAL SUBMATRIX IN ROWS LOW THROUGH IGH      EILI 152
C     HAS BEEN BALANCED, THAT P(J) DENOTES THE INDEX INTERCHANGED       EILI 153
C     WITH J DURING THE PERMUTATION STEP, AND THAT THE ELEMENTS         EILI 154
C     OF THE DIAGONAL MATRIX USED ARE DENOTED BY D(I,J).  THEN          EILI 155
C        SCALE(J) = P(J),    FOR J = 1,...,LOW-1                        EILI 156
C                 = D(J,J),      J = LOW,...,IGH                        EILI 157
C                 = P(J)         J = IGH+1,...,N.                       EILI 158
C     THE ORDER IN WHICH THE INTERCHANGES ARE MADE IS N TO IGH+1,       EILI 159
C     THEN 1 TO LOW-1.                                                  EILI 160
C                                                                       EILI 161
C     NOTE THAT 1 IS RETURNED FOR IGH IF IGH IS ZERO FORMALLY.          EILI 162
C                                                                       EILI 163
C     THE ALGOL PROCEDURE EXC CONTAINED IN BALANCE APPEARS IN           EILI 164
C     BALANC  IN LINE.  (NOTE THAT THE ALGOL ROLES OF IDENTIFIERS       EILI 165
C     K,L HAVE BEEN REVERSED.)                                          EILI 166
C                                                                       EILI 167
C     QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO BURTON S. GARBOW,    EILI 168
C     MATHEMATICS AND COMPUTER SCIENCE DIV, ARGONNE NATIONAL LABORATORY EILI 169
C                                                                       EILI 170
C     THIS VERSION DATED AUGUST 1983.                                   EILI 171
C                                                                       EILI 172
C     ------------------------------------------------------------------EILI 173
C                                                                       EILI 174
      RADIX = 16.0D0                                                    EILI 175
C                                                                       EILI 176
      B2 = RADIX * RADIX                                                EILI 177
      K = 1                                                             EILI 178
      L = N                                                             EILI 179
      GO TO 100                                                         EILI 180
C     .......... IN-LINE PROCEDURE FOR ROW AND                          EILI 181
C                COLUMN EXCHANGE ..........                             EILI 182
   20 SCALE(M) = J                                                      EILI 183
      IF (J .EQ. M) GO TO 50                                            EILI 184
C                                                                       EILI 185
      DO 30 I = 1, L                                                    EILI 186
         F = A(I,J)                                                     EILI 187
         A(I,J) = A(I,M)                                                EILI 188
         A(I,M) = F                                                     EILI 189
   30 CONTINUE                                                          EILI 190
C                                                                       EILI 191
      DO 40 I = K, N                                                    EILI 192
         F = A(J,I)                                                     EILI 193
         A(J,I) = A(M,I)                                                EILI 194
         A(M,I) = F                                                     EILI 195
   40 CONTINUE                                                          EILI 196
C                                                                       EILI 197
   50 GO TO (80,130), IEXC                                              EILI 198
C     .......... SEARCH FOR ROWS ISOLATING AN EIGENVALUE                EILI 199
C                AND PUSH THEM DOWN ..........                          EILI 200
   80 IF (L .EQ. 1) GO TO 280                                           EILI 201
      L = L - 1                                                         EILI 202
C     .......... FOR J=L STEP -1 UNTIL 1 DO -- ..........               EILI 203
  100 DO 120 JJ = 1, L                                                  EILI 204
         J = L + 1 - JJ                                                 EILI 205
C                                                                       EILI 206
         DO 110 I = 1, L                                                EILI 207
            IF (I .EQ. J) GO TO 110                                     EILI 208
            IF (A(J,I) .NE. 0.0D0) GO TO 120                            EILI 209
  110    CONTINUE                                                       EILI 210
C                                                                       EILI 211
         M = L                                                          EILI 212
         IEXC = 1                                                       EILI 213
         GO TO 20                                                       EILI 214
  120 CONTINUE                                                          EILI 215
C                                                                       EILI 216
      GO TO 140                                                         EILI 217
C     .......... SEARCH FOR COLUMNS ISOLATING AN EIGENVALUE             EILI 218
C                AND PUSH THEM LEFT ..........                          EILI 219
  130 K = K + 1                                                         EILI 220
C                                                                       EILI 221
  140 DO 170 J = K, L                                                   EILI 222
C                                                                       EILI 223
         DO 150 I = K, L                                                EILI 224
            IF (I .EQ. J) GO TO 150                                     EILI 225
            IF (A(I,J) .NE. 0.0D0) GO TO 170                            EILI 226
  150    CONTINUE                                                       EILI 227
C                                                                       EILI 228
         M = K                                                          EILI 229
         IEXC = 2                                                       EILI 230
         GO TO 20                                                       EILI 231
  170 CONTINUE                                                          EILI 232
C     .......... NOW BALANCE THE SUBMATRIX IN ROWS K TO L ..........    EILI 233
      DO 180 I = K, L                                                   EILI 234
  180 SCALE(I) = 1.0D0                                                  EILI 235
C     .......... ITERATIVE LOOP FOR NORM REDUCTION ..........           EILI 236
  190 NOCONV = .FALSE.                                                  EILI 237
C                                                                       EILI 238
      DO 270 I = K, L                                                   EILI 239
         C = 0.0D0                                                      EILI 240
         R = 0.0D0                                                      EILI 241
C                                                                       EILI 242
         DO 200 J = K, L                                                EILI 243
            IF (J .EQ. I) GO TO 200                                     EILI 244
            C = C + DABS(A(J,I))                                        EILI 245
            R = R + DABS(A(I,J))                                        EILI 246
  200    CONTINUE                                                       EILI 247
C     .......... GUARD AGAINST ZERO C OR R DUE TO UNDERFLOW ..........  EILI 248
         IF (C .EQ. 0.0D0 .OR. R .EQ. 0.0D0) GO TO 270                  EILI 249
         G = R / RADIX                                                  EILI 250
         F = 1.0D0                                                      EILI 251
         S = C + R                                                      EILI 252
  210    IF (C .GE. G) GO TO 220                                        EILI 253
         F = F * RADIX                                                  EILI 254
         C = C * B2                                                     EILI 255
         GO TO 210                                                      EILI 256
  220    G = R * RADIX                                                  EILI 257
  230    IF (C .LT. G) GO TO 240                                        EILI 258
         F = F / RADIX                                                  EILI 259
         C = C / B2                                                     EILI 260
         GO TO 230                                                      EILI 261
C     .......... NOW BALANCE ..........                                 EILI 262
  240    IF ((C + R) / F .GE. 0.95D0 * S) GO TO 270                     EILI 263
         G = 1.0D0 / F                                                  EILI 264
         SCALE(I) = SCALE(I) * F                                        EILI 265
         NOCONV = .TRUE.                                                EILI 266
C                                                                       EILI 267
         DO 250 J = K, N                                                EILI 268
  250    A(I,J) = A(I,J) * G                                            EILI 269
C                                                                       EILI 270
         DO 260 J = 1, L                                                EILI 271
  260    A(J,I) = A(J,I) * F                                            EILI 272
C                                                                       EILI 273
  270 CONTINUE                                                          EILI 274
C                                                                       EILI 275
      IF (NOCONV) GO TO 190                                             EILI 276
C                                                                       EILI 277
  280 LOW = K                                                           EILI 278
      IGH = L                                                           EILI 279
      RETURN                                                            EILI 280
      END               
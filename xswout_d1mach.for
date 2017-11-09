!     --------------------------------------------------------
!     THIS FORTRAN SOURCE FILE WAS GENERATED USING FORTRAN-IT.
!     --------------------------------------------------------
!     Generation time: 10/10/2017 1:41:06 AM
!     Author         : 
!     Device Name    : RABIEMSI
!     Project Name   : XSWOUT.fip
!     --------------------------------------------------------
      
      
!     Begin your code here:
      
      DOUBLE PRECISION FUNCTION D1MACH(I)                               EILI2185
C***BEGIN PROLOGUE  D1MACH                                              EILI2186
C***DATE WRITTEN   750101   (YYMMDD)                                    EILI2187
C***REVISION DATE  860501   (YYMMDD)                                    EILI2188
C***CATEGORY NO.  R1                                                    EILI2189
C***KEYWORDS  MACHINE CONSTANTS                                         EILI2190
C***AUTHOR  FOX, P. A., (BELL LABS)                                     EILI2191
C           HALL, A. D., (BELL LABS)                                    EILI2192
C           SCHRYER, N. L., (BELL LABS)                                 EILI2193
C***PURPOSE  RETURN DOUBLE PRECISION MACHINE DEPENDENT CONSTANTS.       EILI2194
C***DESCRIPTION                                                         EILI2195
C                                                                       EILI2196
C     D1MACH CAN BE USED TO OBTAIN MACHINE-DEPENDENT PARAMETERS         EILI2197
C     FOR THE LOCAL MACHINE ENVIRONMENT.  IT IS A FUNCTION              EILI2198
C     SUBPROGRAM WITH ONE (INPUT) ARGUMENT, AND CAN BE CALLED           EILI2199
C     AS FOLLOWS, FOR EXAMPLE                                           EILI2200
C                                                                       EILI2201
C          D = D1MACH(I)                                                EILI2202
C                                                                       EILI2203
C     WHERE I=1,...,5.  THE (OUTPUT) VALUE OF D ABOVE IS                EILI2204
C     DETERMINED BY THE (INPUT) VALUE OF I.  THE RESULTS FOR            EILI2205
C     VARIOUS VALUES OF I ARE DISCUSSED BELOW.                          EILI2206
C                                                                       EILI2207
C  DOUBLE-PRECISION MACHINE CONSTANTS                                   EILI2208
C  D1MACH( 1) = B**(EMIN-1), THE SMALLEST POSITIVE MAGNITUDE.           EILI2209
C  D1MACH( 2) = B**EMAX*(1 - B**(-T)), THE LARGEST MAGNITUDE.           EILI2210
C  D1MACH( 3) = B**(-T), THE SMALLEST RELATIVE SPACING.                 EILI2211
C  D1MACH( 4) = B**(1-T), THE LARGEST RELATIVE SPACING.                 EILI2212
C  D1MACH( 5) = LOG10(B)                                                EILI2213
C***REFERENCES  FOX P.A., HALL A.D., SCHRYER N.L.,*FRAMEWORK FOR A      EILI2214
C                 PORTABLE LIBRARY*, ACM TRANSACTIONS ON MATHEMATICAL   EILI2215
C                 SOFTWARE, VOL. 4, NO. 2, JUNE 1978, PP. 177-188.      EILI2216
C***ROUTINES CALLED  XERROR                                             EILI2217
C***END PROLOGUE  D1MACH                                                EILI2218
C                                                                       EILI2219
      INTEGER SMALL(4)                                                  EILI2220
      INTEGER LARGE(4)                                                  EILI2221
      INTEGER RIGHT(4)                                                  EILI2222
      INTEGER DIVER(4)                                                  EILI2223
      INTEGER LOG10(4)                                                  EILI2224
C                                                                       EILI2225
      DOUBLE PRECISION DMACH(5)                                         EILI2226
C                                                                       EILI2227
      EQUIVALENCE (DMACH(1),SMALL(1))                                   EILI2228
      EQUIVALENCE (DMACH(2),LARGE(1))                                   EILI2229
      EQUIVALENCE (DMACH(3),RIGHT(1))                                   EILI2230
      EQUIVALENCE (DMACH(4),DIVER(1))                                   EILI2231
      EQUIVALENCE (DMACH(5),LOG10(1))                                   EILI2232
C                                                                       EILI2233
C     MACHINE CONSTANTS FOR THE BURROUGHS 1700 SYSTEM.                  EILI2234
C                                                                       EILI2235
C     DATA SMALL(1) / ZC00800000 /                                      EILI2236
C     DATA SMALL(2) / Z000000000 /                                      EILI2237
C                                                                       EILI2238
C     DATA LARGE(1) / ZDFFFFFFFF /                                      EILI2239
C     DATA LARGE(2) / ZFFFFFFFFF /                                      EILI2240
C                                                                       EILI2241
C     DATA RIGHT(1) / ZCC5800000 /                                      EILI2242
C     DATA RIGHT(2) / Z000000000 /                                      EILI2243
C                                                                       EILI2244
C     DATA DIVER(1) / ZCC6800000 /                                      EILI2245
C     DATA DIVER(2) / Z000000000 /                                      EILI2246
C                                                                       EILI2247
C     DATA LOG10(1) / ZD00E730E7 /                                      EILI2248
C     DATA LOG10(2) / ZC77800DC0 /                                      EILI2249
C                                                                       EILI2250
C     MACHINE CONSTANTS FOR THE BURROUGHS 5700 SYSTEM.                  EILI2251
C                                                                       EILI2252
C     DATA SMALL(1) / O1771000000000000 /                               EILI2253
C     DATA SMALL(2) / O0000000000000000 /                               EILI2254
C                                                                       EILI2255
C     DATA LARGE(1) / O0777777777777777 /                               EILI2256
C     DATA LARGE(2) / O0007777777777777 /                               EILI2257
C                                                                       EILI2258
C     DATA RIGHT(1) / O1461000000000000 /                               EILI2259
C     DATA RIGHT(2) / O0000000000000000 /                               EILI2260
C                                                                       EILI2261
C     DATA DIVER(1) / O1451000000000000 /                               EILI2262
C     DATA DIVER(2) / O0000000000000000 /                               EILI2263
C                                                                       EILI2264
C     DATA LOG10(1) / O1157163034761674 /                               EILI2265
C     DATA LOG10(2) / O0006677466732724 /                               EILI2266
C                                                                       EILI2267
C     MACHINE CONSTANTS FOR THE BURROUGHS 6700/7700 SYSTEMS.            EILI2268
C                                                                       EILI2269
C     DATA SMALL(1) / O1771000000000000 /                               EILI2270
C     DATA SMALL(2) / O7770000000000000 /                               EILI2271
C                                                                       EILI2272
C     DATA LARGE(1) / O0777777777777777 /                               EILI2273
C     DATA LARGE(2) / O7777777777777777 /                               EILI2274
C                                                                       EILI2275
C     DATA RIGHT(1) / O1461000000000000 /                               EILI2276
C     DATA RIGHT(2) / O0000000000000000 /                               EILI2277
C                                                                       EILI2278
C     DATA DIVER(1) / O1451000000000000 /                               EILI2279
C     DATA DIVER(2) / O0000000000000000 /                               EILI2280
C                                                                       EILI2281
C     DATA LOG10(1) / O1157163034761674 /                               EILI2282
C     DATA LOG10(2) / O0006677466732724 /                               EILI2283
C                                                                       EILI2284
C     MACHINE CONSTANTS FOR THE CDC 6000/7000 SERIES.                   EILI2285
C     FOR FTN4                                                          EILI2286
C                                                                       EILI2287
C     DATA SMALL(1) / 00564000000000000000B /                           EILI2288
C     DATA SMALL(2) / 00000000000000000000B /                           EILI2289
C                                                                       EILI2290
C     DATA LARGE(1) / 37757777777777777777B /                           EILI2291
C     DATA LARGE(2) / 37157777777777777777B /                           EILI2292
C                                                                       EILI2293
C     DATA RIGHT(1) / 15624000000000000000B /                           EILI2294
C     DATA RIGHT(2) / 00000000000000000000B /                           EILI2295
C                                                                       EILI2296
C     DATA DIVER(1) / 15634000000000000000B /                           EILI2297
C     DATA DIVER(2) / 00000000000000000000B /                           EILI2298
C                                                                       EILI2299
C     DATA LOG10(1) / 17164642023241175717B /                           EILI2300
C     DATA LOG10(2) / 16367571421742254654B /                           EILI2301
C                                                                       EILI2302
C     MACHINE CONSTANTS FOR THE CDC 6000/7000 SERIES.                   EILI2303
C     FOR FTN5                                                          EILI2304
C                                                                       EILI2305
C     DATA SMALL(1) / O"00564000000000000000" /                         EILI2306
C     DATA SMALL(2) / O"00000000000000000000" /                         EILI2307
C                                                                       EILI2308
C     DATA LARGE(1) / O"37757777777777777777" /                         EILI2309
C     DATA LARGE(2) / O"37157777777777777777" /                         EILI2310
C                                                                       EILI2311
C     DATA RIGHT(1) / O"15624000000000000000" /                         EILI2312
C     DATA RIGHT(2) / O"00000000000000000000" /                         EILI2313
C                                                                       EILI2314
C     DATA DIVER(1) / O"15634000000000000000" /                         EILI2315
C     DATA DIVER(2) / O"00000000000000000000" /                         EILI2316
C                                                                       EILI2317
C     DATA LOG10(1) / O"17164642023241175717" /                         EILI2318
C     DATA LOG10(2) / O"16367571421742254654" /                         EILI2319
C                                                                       EILI2320
C     MACHINE CONSTANTS FOR THE CRAY 1                                  EILI2321
C                                                                       EILI2322
C     DATA SMALL(1) / 201354000000000000000B /                          EILI2323
C     DATA SMALL(2) / 000000000000000000000B /                          EILI2324
C                                                                       EILI2325
C     DATA LARGE(1) / 577767777777777777777B /                          EILI2326
C     DATA LARGE(2) / 000007777777777777774B /                          EILI2327
C                                                                       EILI2328
C     DATA RIGHT(1) / 376434000000000000000B /                          EILI2329
C     DATA RIGHT(2) / 000000000000000000000B /                          EILI2330
C                                                                       EILI2331
C     DATA DIVER(1) / 376444000000000000000B /                          EILI2332
C     DATA DIVER(2) / 000000000000000000000B /                          EILI2333
C                                                                       EILI2334
C     DATA LOG10(1) / 377774642023241175717B /                          EILI2335
C     DATA LOG10(2) / 000007571421742254654B /                          EILI2336
C                                                                       EILI2337
C     MACHINE CONSTANTS FOR THE DATA GENERAL ECLIPSE S/200              EILI2338
C                                                                       EILI2339
C     NOTE - IT MAY BE APPROPRIATE TO INCLUDE THE FOLLOWING CARD -      EILI2340
C     STATIC DMACH(5)                                                   EILI2341
C                                                                       EILI2342
C     DATA SMALL/20K,3*0/,LARGE/77777K,3*177777K/                       EILI2343
C     DATA RIGHT/31420K,3*0/,DIVER/32020K,3*0/                          EILI2344
C     DATA LOG10/40423K,42023K,50237K,74776K/                           EILI2345
C                                                                       EILI2346
C     MACHINE CONSTANTS FOR THE HARRIS 220                              EILI2347
C                                                                       EILI2348
C     DATA SMALL(1), SMALL(2) / '20000000, '00000201 /                  EILI2349
C     DATA LARGE(1), LARGE(2) / '37777777, '37777577 /                  EILI2350
C     DATA RIGHT(1), RIGHT(2) / '20000000, '00000333 /                  EILI2351
C     DATA DIVER(1), DIVER(2) / '20000000, '00000334 /                  EILI2352
C     DATA LOG10(1), LOG10(2) / '23210115, '10237777 /                  EILI2353
C                                                                       EILI2354
C     MACHINE CONSTANTS FOR THE HONEYWELL 600/6000 SERIES.              EILI2355
C                                                                       EILI2356
C     DATA SMALL(1), SMALL(2) / O402400000000, O000000000000 /          EILI2357
C     DATA LARGE(1), LARGE(2) / O376777777777, O777777777777 /          EILI2358
C     DATA RIGHT(1), RIGHT(2) / O604400000000, O000000000000 /          EILI2359
C     DATA DIVER(1), DIVER(2) / O606400000000, O000000000000 /          EILI2360
C     DATA LOG10(1), LOG10(2) / O776464202324, O117571775714 /          EILI2361
C                                                                       EILI2362
C     MACHINE CONSTANTS FOR THE HP 2100                                 EILI2363
C     THREE WORD DOUBLE PRECISION OPTION WITH FTN4                      EILI2364
C                                                                       EILI2365
C     DATA SMALL(1), SMALL(2), SMALL(3) / 40000B,       0,       1 /    EILI2366
C     DATA LARGE(1), LARGE(2), LARGE(3) / 77777B, 177777B, 177776B /    EILI2367
C     DATA RIGHT(1), RIGHT(2), RIGHT(3) / 40000B,       0,    265B /    EILI2368
C     DATA DIVER(1), DIVER(2), DIVER(3) / 40000B,       0,    276B /    EILI2369
C     DATA LOG10(1), LOG10(2), LOG10(3) / 46420B,  46502B,  77777B /    EILI2370
C                                                                       EILI2371
C     MACHINE CONSTANTS FOR THE HP 2100                                 EILI2372
C     FOUR WORD DOUBLE PRECISION OPTION WITH FTN4                       EILI2373
C                                                                       EILI2374
C     DATA SMALL(1), SMALL(2) /  40000B,       0 /                      EILI2375
C     DATA SMALL(3), SMALL(4) /       0,       1 /                      EILI2376
C     DATA LARGE(1), LARGE(2) /  77777B, 177777B /                      EILI2377
C     DATA LARGE(3), LARGE(4) / 177777B, 177776B /                      EILI2378
C     DATA RIGHT(1), RIGHT(2) /  40000B,       0 /                      EILI2379
C     DATA RIGHT(3), RIGHT(4) /       0,    225B /                      EILI2380
C     DATA DIVER(1), DIVER(2) /  40000B,       0 /                      EILI2381
C     DATA DIVER(3), DIVER(4) /       0,    227B /                      EILI2382
C     DATA LOG10(1), LOG10(2) /  46420B,  46502B /                      EILI2383
C     DATA LOG10(3), LOG10(4) /  76747B, 176377B /                      EILI2384
C                                                                       EILI2385
C     MACHINE CONSTANTS FOR THE HP 9000                                 EILI2386
C                                                                       EILI2387
C     D1MACH(1) = 2.8480954D-306                                        EILI2388
C     D1MACH(2) = 1.40444776D+306                                       EILI2389
C     D1MACH(3) = 2.22044605D-16                                        EILI2390
C     D1MACH(4) = 4.44089210D-16                                        EILI2391
C     D1MACH(5) = 3.01029996D-1                                         EILI2392
C                                                                       EILI2393
C     DATA SMALL(1), SMALL(2) / 00040000000B, 00000000000B /            EILI2394
C     DATA LARGE(1), LARGE(2) / 17737777777B, 37777777777B /            EILI2395
C     DATA RIGHT(1), RIGHT(2) / 07454000000B, 00000000000B /            EILI2396
C     DATA DIVER(1), DIVER(2) / 07460000000B, 00000000000B /            EILI2397
C     DATA LOG10(1), LOG10(2) / 07764642023B, 12047674777B /            EILI2398
C                                                                       EILI2399
C     MACHINE CONSTANTS FOR THE IBM 360/370 SERIES,                     EILI2400
C     THE XEROX SIGMA 5/7/9, THE SEL SYSTEMS 85/86, AND                 EILI2401
C     THE PERKIN ELMER (INTERDATA) 7/32.                                EILI2402
C                                                                       EILI2403
C     DATA SMALL(1), SMALL(2) / Z00100000, Z00000000 /                  EILI2404
C     DATA LARGE(1), LARGE(2) / Z7FFFFFFF, ZFFFFFFFF /                  EILI2405
C     DATA RIGHT(1), RIGHT(2) / Z33100000, Z00000000 /                  EILI2406
C     DATA DIVER(1), DIVER(2) / Z34100000, Z00000000 /                  EILI2407
C     DATA LOG10(1), LOG10(2) / Z41134413, Z509F79FF /                  EILI2408
C                                                                       EILI2409
C     MACHINE CONSTANTS FOR THE PDP-10 (KA PROCESSOR).                  EILI2410
C                                                                       EILI2411
C     DATA SMALL(1), SMALL(2) / "033400000000, "000000000000 /          EILI2412
C     DATA LARGE(1), LARGE(2) / "377777777777, "344777777777 /          EILI2413
C     DATA RIGHT(1), RIGHT(2) / "113400000000, "000000000000 /          EILI2414
C     DATA DIVER(1), DIVER(2) / "114400000000, "000000000000 /          EILI2415
C     DATA LOG10(1), LOG10(2) / "177464202324, "144117571776 /          EILI2416
C                                                                       EILI2417
C     MACHINE CONSTANTS FOR THE PDP-10 (KI PROCESSOR).                  EILI2418
C                                                                       EILI2419
C     DATA SMALL(1), SMALL(2) / "000400000000, "000000000000 /          EILI2420
C     DATA LARGE(1), LARGE(2) / "377777777777, "377777777777 /          EILI2421
C     DATA RIGHT(1), RIGHT(2) / "103400000000, "000000000000 /          EILI2422
C     DATA DIVER(1), DIVER(2) / "104400000000, "000000000000 /          EILI2423
C     DATA LOG10(1), LOG10(2) / "177464202324, "476747767461 /          EILI2424
C                                                                       EILI2425
C     MACHINE CONSTANTS FOR PDP-11 FORTRAN SUPPORTING                   EILI2426
C     32-BIT INTEGERS (EXPRESSED IN INTEGER AND OCTAL).                 EILI2427
C                                                                       EILI2428
C     DATA SMALL(1), SMALL(2) /    8388608,           0 /               EILI2429
C     DATA LARGE(1), LARGE(2) / 2147483647,          -1 /               EILI2430
C     DATA RIGHT(1), RIGHT(2) /  612368384,           0 /               EILI2431
C     DATA DIVER(1), DIVER(2) /  620756992,           0 /               EILI2432
C     DATA LOG10(1), LOG10(2) / 1067065498, -2063872008 /               EILI2433
C                                                                       EILI2434
C     DATA SMALL(1), SMALL(2) / O00040000000, O00000000000 /            EILI2435
C     DATA LARGE(1), LARGE(2) / O17777777777, O37777777777 /            EILI2436
C     DATA RIGHT(1), RIGHT(2) / O04440000000, O00000000000 /            EILI2437
C     DATA DIVER(1), DIVER(2) / O04500000000, O00000000000 /            EILI2438
C     DATA LOG10(1), LOG10(2) / O07746420232, O20476747770 /            EILI2439
C                                                                       EILI2440
C     MACHINE CONSTANTS FOR PDP-11 FORTRAN SUPPORTING                   EILI2441
C     16-BIT INTEGERS (EXPRESSED IN INTEGER AND OCTAL).                 EILI2442
C                                                                       EILI2443
C     DATA SMALL(1), SMALL(2) /    128,      0 /                        EILI2444
C     DATA SMALL(3), SMALL(4) /      0,      0 /                        EILI2445
C                                                                       EILI2446
C     DATA LARGE(1), LARGE(2) /  32767,     -1 /                        EILI2447
C     DATA LARGE(3), LARGE(4) /     -1,     -1 /                        EILI2448
C                                                                       EILI2449
C     DATA RIGHT(1), RIGHT(2) /   9344,      0 /                        EILI2450
C     DATA RIGHT(3), RIGHT(4) /      0,      0 /                        EILI2451
C                                                                       EILI2452
C     DATA DIVER(1), DIVER(2) /   9472,      0 /                        EILI2453
C     DATA DIVER(3), DIVER(4) /      0,      0 /                        EILI2454
C                                                                       EILI2455
C     DATA LOG10(1), LOG10(2) /  16282,   8346 /                        EILI2456
C     DATA LOG10(3), LOG10(4) / -31493, -12296 /                        EILI2457
C                                                                       EILI2458
C     DATA SMALL(1), SMALL(2) / O000200, O000000 /                      EILI2459
C     DATA SMALL(3), SMALL(4) / O000000, O000000 /                      EILI2460
C                                                                       EILI2461
C     DATA LARGE(1), LARGE(2) / O077777, O177777 /                      EILI2462
C     DATA LARGE(3), LARGE(4) / O177777, O177777 /                      EILI2463
C                                                                       EILI2464
C     DATA RIGHT(1), RIGHT(2) / O022200, O000000 /                      EILI2465
C     DATA RIGHT(3), RIGHT(4) / O000000, O000000 /                      EILI2466
C                                                                       EILI2467
C     DATA DIVER(1), DIVER(2) / O022400, O000000 /                      EILI2468
C     DATA DIVER(3), DIVER(4) / O000000, O000000 /                      EILI2469
C                                                                       EILI2470
C     DATA LOG10(1), LOG10(2) / O037632, O020232 /                      EILI2471
C     DATA LOG10(3), LOG10(4) / O102373, O147770 /                      EILI2472
C                                                                       EILI2473
C     MACHINE CONSTANTS FOR THE UNIVAC 1100 SERIES. FTN COMPILER        EILI2474
C                                                                       EILI2475
C     DATA SMALL(1), SMALL(2) / O000040000000, O000000000000 /          EILI2476
C     DATA LARGE(1), LARGE(2) / O377777777777, O777777777777 /          EILI2477
C     DATA RIGHT(1), RIGHT(2) / O170540000000, O000000000000 /          EILI2478
C     DATA DIVER(1), DIVER(2) / O170640000000, O000000000000 /          EILI2479
C     DATA LOG10(1), LOG10(2) / O177746420232, O411757177572 /          EILI2480
C                                                                       EILI2481
C     MACHINE CONSTANTS FOR VAX 11/780                                  EILI2482
C     (EXPRESSED IN INTEGER AND HEXADECIMAL)                            EILI2483
C     ***THE HEX FORMAT BELOW MAY NOT BE SUITABLE FOR UNIX SYSYEMS***   EILI2484
C     *** THE INTEGER FORMAT SHOULD BE OK FOR UNIX SYSTEMS***           EILI2485
C                                                                       EILI2486
c      DATA SMALL(1), SMALL(2) /        128,           0 /              EILI2487
c      DATA LARGE(1), LARGE(2) /     -32769,          -1 /              EILI2488
c      DATA RIGHT(1), RIGHT(2) /       9344,           0 /              EILI2489
c      DATA DIVER(1), DIVER(2) /       9472,           0 /              EILI2490
c      DATA LOG10(1), LOG10(2) /  546979738,  -805796613 /              EILI2491
C                                                                       EILI2492
C***** VXD                                                              EILI2493
C     DATA SMALL(1), SMALL(2) / Z00000080, Z00000000 /                  EILI2494
C     DATA LARGE(1), LARGE(2) / ZFFFF7FFF, ZFFFFFFFF /                  EILI2495
C     DATA RIGHT(1), RIGHT(2) / Z00002480, Z00000000 /                  EILI2496
C     DATA DIVER(1), DIVER(2) / Z00002500, Z00000000 /                  EILI2497
C     DATA LOG10(1), LOG10(2) / Z209A3F9A, ZCFF884FB /                  EILI2498
C***** VXD                                                              EILI2499
C                                                                       EILI2500
C     MACHINE CONSTANTS FOR VAX 11/780 (G-FLOATING)                     EILI2501
C     (EXPRESSED IN INTEGER AND HEXADECIMAL)                            EILI2502
C     ***THE HEX FORMAT BELOW MAY NOT BE SUITABLE FOR UNIX SYSYEMS***   EILI2503
C     *** THE INTEGER FORMAT SHOULD BE OK FOR UNIX SYSTEMS***           EILI2504
C                                                                       EILI2505
C     DATA SMALL(1), SMALL(2) /         16,           0 /               EILI2506
C     DATA LARGE(1), LARGE(2) /     -32769,          -1 /               EILI2507
C     DATA RIGHT(1), RIGHT(2) /      15552,           0 /               EILI2508
C     DATA DIVER(1), DIVER(2) /      15568,           0 /               EILI2509
C     DATA LOG10(1), LOG10(2) /  1142112243, 2046775455 /               EILI2510
C                                                                       EILI2511
C***** VXG                                                              EILI2512
C     DATA SMALL(1), SMALL(2) / Z00000010, Z00000000 /                  EILI2513
C     DATA LARGE(1), LARGE(2) / ZFFFF7FFF, ZFFFFFFFF /                  EILI2514
C     DATA RIGHT(1), RIGHT(2) / Z00003CC0, Z00000000 /                  EILI2515
C     DATA DIVER(1), DIVER(2) / Z00003CD0, Z00000000 /                  EILI2516
C     DATA LOG10(1), LOG10(2) / Z44133FF3, Z79FF509F /                  EILI2517
C***** VXG                                                              EILI2518
C                                                                       EILI2519
C     MACHINE CONSTANTS FOR THE ELXSI 6400                              EILI2520
C     (ASSUMING REAL*8 IS THE DEFAULT DOUBLE PRECISION)                 EILI2521
C                                                                       EILI2522
C     DATA SMALL(1), SMALL(2) / '00100000'X,'00000000'X /               EILI2523
C     DATA LARGE(1), LARGE(2) / '7FEFFFFF'X,'FFFFFFFF'X /               EILI2524
C     DATA RIGHT(1), RIGHT(2) / '3CB00000'X,'00000000'X /               EILI2525
C     DATA DIVER(1), DIVER(2) / '3CC00000'X,'00000000'X /               EILI2526
C     DATA LOG10(1), DIVER(2) / '3FD34413'X,'509F79FF'X /               EILI2527
C                                                                       EILI2528
C     MACHINE CONSTANTS FOR THE IBM PC - MICROSOFT FORTRAN              EILI2529
C                                                                       EILI2530
C***** PCMICROSOFT                                                      EILI2531
C     DATA SMALL(1), SMALL(2) / #00000000, #00100000 /                  EILI2532
C     DATA LARGE(1), LARGE(2) / #FFFFFFFF, #7FEFFFFF /                  EILI2533
C     DATA RIGHT(1), RIGHT(2) / #00000000, #3CA00000 /                  EILI2534
C     DATA DIVER(1), DIVER(2) / #00000000, #3CB00000 /                  EILI2535
C     DATA LOG10(1), LOG10(2) / #509F79FF, #3FD34413 /                  EILI2536
C***** PCMICROSOFT                                                      EILI2537
C                                                                       EILI2538
C     MACHINE CONSTANTS FOR THE IBM PC - PROFESSIONAL FORTRAN           EILI2539
C     AND LAHEY FORTRAN                                                 EILI2540
C                                                                       EILI2541
C***** PCNDP                                                            EILI2542
      DATA SMALL(1), SMALL(2) / Z'00000000', Z'00100000' /              EILI2543
      DATA LARGE(1), LARGE(2) / Z'FFFFFFFF', Z'7FEFFFFF' /              EILI2544
      DATA RIGHT(1), RIGHT(2) / Z'00000000', Z'3CA00000' /              EILI2545
      DATA DIVER(1), DIVER(2) / Z'00000000', Z'3CB00000' /              EILI2546
      DATA LOG10(1), LOG10(2) / Z'509F79FF', Z'3FD34413' /              EILI2547
C***** PCNDP                                                            EILI2548
C                                                                       EILI2549
C***FIRST EXECUTABLE STATEMENT  D1MACH                                  EILI2550
      IF (I .LT. 1  .OR.  I .GT. 5)                                     EILI2551
     1   CALL XERROR( 'D1MACH -- I OUT OF BOUNDS',25,1,2)               EILI2552
C                                                                       EILI2553
      D1MACH = DMACH(I)                                                 EILI2554
      RETURN                                                            EILI2555
C                                                                       EILI2556
      END                                  
!     --------------------------------------------------------
!     THIS FORTRAN SOURCE FILE WAS GENERATED USING FORTRAN-IT.
!     --------------------------------------------------------
!     Generation time: 10/10/2017 9:45:46 AM
!     Author         : 
!     Device Name    : RABIEMSI
!     Project Name   : XSWOUT.fip
!     --------------------------------------------------------
      
      
!     Begin your code here:
      
      INTEGER FUNCTION I1MACH(I)                                        EILI1637
C***BEGIN PROLOGUE  I1MACH                                              EILI1638
C***DATE WRITTEN   750101   (YYMMDD)                                    EILI1639
C***REVISION DATE  860501   (YYMMDD)                                    EILI1640
C***CATEGORY NO.  R1                                                    EILI1641
C***KEYWORDS  MACHINE CONSTANTS                                         EILI1642
C***AUTHOR  FOX, P. A., (BELL LABS)                                     EILI1643
C           HALL, A. D., (BELL LABS)                                    EILI1644
C           SCHRYER, N. L., (BELL LABS)                                 EILI1645
C***PURPOSE  RETURN INTEGER MACHINE DEPENDENT CONSTANTS.                EILI1646
C***DESCRIPTION                                                         EILI1647
C                                                                       EILI1648
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * *             EILI1649
C   THESE MACHINE CONSTANT ROUTINES MUST BE ACTIVATED FOR               EILI1650
C   A PARTICULAR ENVIRONMENT.                                           EILI1651
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * *             EILI1652
C                                                                       EILI1653
C     I1MACH CAN BE USED TO OBTAIN MACHINE-DEPENDENT PARAMETERS         EILI1654
C     FOR THE LOCAL MACHINE ENVIRONMENT.  IT IS A FUNCTION              EILI1655
C     SUBROUTINE WITH ONE (INPUT) ARGUMENT, AND CAN BE CALLED           EILI1656
C     AS FOLLOWS, FOR EXAMPLE                                           EILI1657
C                                                                       EILI1658
C          K = I1MACH(I)                                                EILI1659
C                                                                       EILI1660
C     WHERE I=1,...,16.  THE (OUTPUT) VALUE OF K ABOVE IS               EILI1661
C     DETERMINED BY THE (INPUT) VALUE OF I.  THE RESULTS FOR            EILI1662
C     VARIOUS VALUES OF I ARE DISCUSSED BELOW.                          EILI1663
C                                                                       EILI1664
C  I/O UNIT NUMBERS.                                                    EILI1665
C    I1MACH( 1) = THE STANDARD INPUT UNIT.                              EILI1666
C    I1MACH( 2) = THE STANDARD OUTPUT UNIT.                             EILI1667
C    I1MACH( 3) = THE STANDARD PUNCH UNIT.                              EILI1668
C    I1MACH( 4) = THE STANDARD ERROR MESSAGE UNIT.                      EILI1669
C                                                                       EILI1670
C  WORDS.                                                               EILI1671
C    I1MACH( 5) = THE NUMBER OF BITS PER INTEGER STORAGE UNIT.          EILI1672
C    I1MACH( 6) = THE NUMBER OF CHARACTERS PER INTEGER STORAGE UNIT.    EILI1673
C                                                                       EILI1674
C  INTEGERS.                                                            EILI1675
C    ASSUME INTEGERS ARE REPRESENTED IN THE S-DIGIT, BASE-A FORM        EILI1676
C                                                                       EILI1677
C               SIGN ( X(S-1)*A**(S-1) + ... + X(1)*A + X(0) )          EILI1678
C                                                                       EILI1679
C               WHERE 0 .LE. X(I) .LT. A FOR I=0,...,S-1.               EILI1680
C    I1MACH( 7) = A, THE BASE.                                          EILI1681
C    I1MACH( 8) = S, THE NUMBER OF BASE-A DIGITS.                       EILI1682
C    I1MACH( 9) = A**S - 1, THE LARGEST MAGNITUDE.                      EILI1683
C                                                                       EILI1684
C  FLOATING-POINT NUMBERS.                                              EILI1685
C    ASSUME FLOATING-POINT NUMBERS ARE REPRESENTED IN THE T-DIGIT,      EILI1686
C    BASE-B FORM                                                        EILI1687
C               SIGN (B**E)*( (X(1)/B) + ... + (X(T)/B**T) )            EILI1688
C                                                                       EILI1689
C               WHERE 0 .LE. X(I) .LT. B FOR I=1,...,T,                 EILI1690
C               0 .LT. X(1), AND EMIN .LE. E .LE. EMAX.                 EILI1691
C    I1MACH(10) = B, THE BASE.                                          EILI1692
C                                                                       EILI1693
C  SINGLE-PRECISION                                                     EILI1694
C    I1MACH(11) = T, THE NUMBER OF BASE-B DIGITS.                       EILI1695
C    I1MACH(12) = EMIN, THE SMALLEST EXPONENT E.                        EILI1696
C    I1MACH(13) = EMAX, THE LARGEST EXPONENT E.                         EILI1697
C                                                                       EILI1698
C  DOUBLE-PRECISION                                                     EILI1699
C    I1MACH(14) = T, THE NUMBER OF BASE-B DIGITS.                       EILI1700
C    I1MACH(15) = EMIN, THE SMALLEST EXPONENT E.                        EILI1701
C    I1MACH(16) = EMAX, THE LARGEST EXPONENT E.                         EILI1702
C                                                                       EILI1703
C  TO ALTER THIS FUNCTION FOR A PARTICULAR ENVIRONMENT,                 EILI1704
C  THE DESIRED SET OF DATA STATEMENTS SHOULD BE ACTIVATED BY            EILI1705
C  REMOVING THE C FROM COLUMN 1.  ALSO, THE VALUES OF                   EILI1706
C  I1MACH(1) - I1MACH(4) SHOULD BE CHECKED FOR CONSISTENCY              EILI1707
C  WITH THE LOCAL OPERATING SYSTEM.                                     EILI1708
C***REFERENCES  FOX P.A., HALL A.D., SCHRYER N.L.,*FRAMEWORK FOR A      EILI1709
C                 PORTABLE LIBRARY*, ACM TRANSACTIONS ON MATHEMATICAL   EILI1710
C                 SOFTWARE, VOL. 4, NO. 2, JUNE 1978, PP. 177-188.      EILI1711
C***ROUTINES CALLED  (NONE)                                             EILI1712
C***END PROLOGUE  I1MACH                                                EILI1713
C                                                                       EILI1714
      INTEGER IMACH(16),OUTPUT                                          EILI1715
      EQUIVALENCE (IMACH(4),OUTPUT)                                     EILI1716
C                                                                       EILI1717
C     MACHINE CONSTANTS FOR THE BURROUGHS 1700 SYSTEM.                  EILI1718
C                                                                       EILI1719
C     DATA IMACH( 1) /    7 /                                           EILI1720
C     DATA IMACH( 2) /    2 /                                           EILI1721
C     DATA IMACH( 3) /    2 /                                           EILI1722
C     DATA IMACH( 4) /    2 /                                           EILI1723
C     DATA IMACH( 5) /   36 /                                           EILI1724
C     DATA IMACH( 6) /    4 /                                           EILI1725
C     DATA IMACH( 7) /    2 /                                           EILI1726
C     DATA IMACH( 8) /   33 /                                           EILI1727
C     DATA IMACH( 9) / Z1FFFFFFFF /                                     EILI1728
C     DATA IMACH(10) /    2 /                                           EILI1729
C     DATA IMACH(11) /   24 /                                           EILI1730
C     DATA IMACH(12) / -256 /                                           EILI1731
C     DATA IMACH(13) /  255 /                                           EILI1732
C     DATA IMACH(14) /   60 /                                           EILI1733
C     DATA IMACH(15) / -256 /                                           EILI1734
C     DATA IMACH(16) /  255 /                                           EILI1735
C                                                                       EILI1736
C     MACHINE CONSTANTS FOR THE BURROUGHS 5700 SYSTEM.                  EILI1737
C                                                                       EILI1738
C     DATA IMACH( 1) /   5 /                                            EILI1739
C     DATA IMACH( 2) /   6 /                                            EILI1740
C     DATA IMACH( 3) /   7 /                                            EILI1741
C     DATA IMACH( 4) /   6 /                                            EILI1742
C     DATA IMACH( 5) /  48 /                                            EILI1743
C     DATA IMACH( 6) /   6 /                                            EILI1744
C     DATA IMACH( 7) /   2 /                                            EILI1745
C     DATA IMACH( 8) /  39 /                                            EILI1746
C     DATA IMACH( 9) / O0007777777777777 /                              EILI1747
C     DATA IMACH(10) /   8 /                                            EILI1748
C     DATA IMACH(11) /  13 /                                            EILI1749
C     DATA IMACH(12) / -50 /                                            EILI1750
C     DATA IMACH(13) /  76 /                                            EILI1751
C     DATA IMACH(14) /  26 /                                            EILI1752
C     DATA IMACH(15) / -50 /                                            EILI1753
C     DATA IMACH(16) /  76 /                                            EILI1754
C                                                                       EILI1755
C     MACHINE CONSTANTS FOR THE BURROUGHS 6700/7700 SYSTEMS.            EILI1756
C                                                                       EILI1757
C     DATA IMACH( 1) /      5 /                                         EILI1758
C     DATA IMACH( 2) /      6 /                                         EILI1759
C     DATA IMACH( 3) /      7 /                                         EILI1760
C     DATA IMACH( 4) /      6 /                                         EILI1761
C     DATA IMACH( 5) /     48 /                                         EILI1762
C     DATA IMACH( 6) /      6 /                                         EILI1763
C     DATA IMACH( 7) /      2 /                                         EILI1764
C     DATA IMACH( 8) /     39 /                                         EILI1765
C     DATA IMACH( 9) / O0007777777777777 /                              EILI1766
C     DATA IMACH(10) /      8 /                                         EILI1767
C     DATA IMACH(11) /     13 /                                         EILI1768
C     DATA IMACH(12) /    -50 /                                         EILI1769
C     DATA IMACH(13) /     76 /                                         EILI1770
C     DATA IMACH(14) /     26 /                                         EILI1771
C     DATA IMACH(15) / -32754 /                                         EILI1772
C     DATA IMACH(16) /  32780 /                                         EILI1773
C                                                                       EILI1774
C     MACHINE CONSTANTS FOR THE CDC 6000/7000 SERIES.                   EILI1775
C     FOR FTN4                                                          EILI1776
C                                                                       EILI1777
C     DATA IMACH( 1) /    5 /                                           EILI1778
C     DATA IMACH( 2) /    6 /                                           EILI1779
C     DATA IMACH( 3) /    7 /                                           EILI1780
C     DATA IMACH( 4) / 6LOUTPUT /                                       EILI1781
C     DATA IMACH( 5) /   60 /                                           EILI1782
C     DATA IMACH( 6) /   10 /                                           EILI1783
C     DATA IMACH( 7) /    2 /                                           EILI1784
C     DATA IMACH( 8) /   48 /                                           EILI1785
C     DATA IMACH( 9) / 00007777777777777777B /                          EILI1786
C     DATA IMACH(10) /    2 /                                           EILI1787
C     DATA IMACH(11) /   47 /                                           EILI1788
C     DATA IMACH(12) / -929 /                                           EILI1789
C     DATA IMACH(13) / 1070 /                                           EILI1790
C     DATA IMACH(14) /   94 /                                           EILI1791
C     DATA IMACH(15) / -929 /                                           EILI1792
C     DATA IMACH(16) / 1069 /                                           EILI1793
C                                                                       EILI1794
C     MACHINE CONSTANTS FOR THE CDC 6000/7000 SERIES.                   EILI1795
C     FOR FTN5                                                          EILI1796
C                                                                       EILI1797
C     DATA IMACH( 1) /    5 /                                           EILI1798
C     DATA IMACH( 2) /    6 /                                           EILI1799
C     DATA IMACH( 3) /    7 /                                           EILI1800
C     DATA IMACH( 4) / L"OUTPUT" /                                      EILI1801
C     DATA IMACH( 5) /   60 /                                           EILI1802
C     DATA IMACH( 6) /   10 /                                           EILI1803
C     DATA IMACH( 7) /    2 /                                           EILI1804
C     DATA IMACH( 8) /   48 /                                           EILI1805
C     DATA IMACH( 9) / O"00007777777777777777" /                        EILI1806
C     DATA IMACH(10) /    2 /                                           EILI1807
C     DATA IMACH(11) /   47 /                                           EILI1808
C     DATA IMACH(12) / -929 /                                           EILI1809
C     DATA IMACH(13) / 1070 /                                           EILI1810
C     DATA IMACH(14) /   94 /                                           EILI1811
C     DATA IMACH(15) / -929 /                                           EILI1812
C     DATA IMACH(16) / 1069 /                                           EILI1813
C                                                                       EILI1814
C     MACHINE CONSTANTS FOR THE CRAY 1                                  EILI1815
C                                                                       EILI1816
C     DATA IMACH( 1) /   100 /                                          EILI1817
C     DATA IMACH( 2) /   101 /                                          EILI1818
C     DATA IMACH( 3) /   102 /                                          EILI1819
C     DATA IMACH( 4) /   101 /                                          EILI1820
C     DATA IMACH( 5) /    64 /                                          EILI1821
C     DATA IMACH( 6) /     8 /                                          EILI1822
C     DATA IMACH( 7) /     2 /                                          EILI1823
C     DATA IMACH( 8) /    63 /                                          EILI1824
C     DATA IMACH( 9) / 777777777777777777777B /                         EILI1825
C     DATA IMACH(10) /     2 /                                          EILI1826
C     DATA IMACH(11) /    47 /                                          EILI1827
C     DATA IMACH(12) / -8189 /                                          EILI1828
C     DATA IMACH(13) /  8190 /                                          EILI1829
C     DATA IMACH(14) /    94 /                                          EILI1830
C     DATA IMACH(15) / -8099 /                                          EILI1831
C     DATA IMACH(16) /  8190 /                                          EILI1832
C                                                                       EILI1833
C     MACHINE CONSTANTS FOR THE DATA GENERAL ECLIPSE S/200              EILI1834
C                                                                       EILI1835
C     DATA IMACH( 1) /  11 /                                            EILI1836
C     DATA IMACH( 2) /  12 /                                            EILI1837
C     DATA IMACH( 3) /   8 /                                            EILI1838
C     DATA IMACH( 4) /  10 /                                            EILI1839
C     DATA IMACH( 5) /  16 /                                            EILI1840
C     DATA IMACH( 6) /   2 /                                            EILI1841
C     DATA IMACH( 7) /   2 /                                            EILI1842
C     DATA IMACH( 8) /  15 /                                            EILI1843
C     DATA IMACH( 9) / 32767 /                                          EILI1844
C     DATA IMACH(10) /  16 /                                            EILI1845
C     DATA IMACH(11) /   6 /                                            EILI1846
C     DATA IMACH(12) / -64 /                                            EILI1847
C     DATA IMACH(13) /  63 /                                            EILI1848
C     DATA IMACH(14) /  14 /                                            EILI1849
C     DATA IMACH(15) / -64 /                                            EILI1850
C     DATA IMACH(16) /  63 /                                            EILI1851
C                                                                       EILI1852
C     MACHINE CONSTANTS FOR THE HARRIS 220                              EILI1853
C                                                                       EILI1854
C     DATA IMACH( 1) /    5 /                                           EILI1855
C     DATA IMACH( 2) /    6 /                                           EILI1856
C     DATA IMACH( 3) /    0 /                                           EILI1857
C     DATA IMACH( 4) /    6 /                                           EILI1858
C     DATA IMACH( 5) /   24 /                                           EILI1859
C     DATA IMACH( 6) /    3 /                                           EILI1860
C     DATA IMACH( 7) /    2 /                                           EILI1861
C     DATA IMACH( 8) /   23 /                                           EILI1862
C     DATA IMACH( 9) / 8388607 /                                        EILI1863
C     DATA IMACH(10) /    2 /                                           EILI1864
C     DATA IMACH(11) /   23 /                                           EILI1865
C     DATA IMACH(12) / -127 /                                           EILI1866
C     DATA IMACH(13) /  127 /                                           EILI1867
C     DATA IMACH(14) /   38 /                                           EILI1868
C     DATA IMACH(15) / -127 /                                           EILI1869
C     DATA IMACH(16) /  127 /                                           EILI1870
C                                                                       EILI1871
C     MACHINE CONSTANTS FOR THE HONEYWELL 600/6000 SERIES.              EILI1872
C                                                                       EILI1873
C     DATA IMACH( 1) /    5 /                                           EILI1874
C     DATA IMACH( 2) /    6 /                                           EILI1875
C     DATA IMACH( 3) /   43 /                                           EILI1876
C     DATA IMACH( 4) /    6 /                                           EILI1877
C     DATA IMACH( 5) /   36 /                                           EILI1878
C     DATA IMACH( 6) /    6 /                                           EILI1879
C     DATA IMACH( 7) /    2 /                                           EILI1880
C     DATA IMACH( 8) /   35 /                                           EILI1881
C     DATA IMACH( 9) / O377777777777 /                                  EILI1882
C     DATA IMACH(10) /    2 /                                           EILI1883
C     DATA IMACH(11) /   27 /                                           EILI1884
C     DATA IMACH(12) / -127 /                                           EILI1885
C     DATA IMACH(13) /  127 /                                           EILI1886
C     DATA IMACH(14) /   63 /                                           EILI1887
C     DATA IMACH(15) / -127 /                                           EILI1888
C     DATA IMACH(16) /  127 /                                           EILI1889
C                                                                       EILI1890
C     MACHINE CONSTANTS FOR THE HP 2100                                 EILI1891
C     3 WORD DOUBLE PRECISION OPTION WITH FTN4                          EILI1892
C                                                                       EILI1893
C     DATA IMACH( 1) /    5 /                                           EILI1894
C     DATA IMACH( 2) /    6 /                                           EILI1895
C     DATA IMACH( 3) /    4 /                                           EILI1896
C     DATA IMACH( 4) /    1 /                                           EILI1897
C     DATA IMACH( 5) /   16 /                                           EILI1898
C     DATA IMACH( 6) /    2 /                                           EILI1899
C     DATA IMACH( 7) /    2 /                                           EILI1900
C     DATA IMACH( 8) /   15 /                                           EILI1901
C     DATA IMACH( 9) / 32767 /                                          EILI1902
C     DATA IMACH(10) /    2 /                                           EILI1903
C     DATA IMACH(11) /   23 /                                           EILI1904
C     DATA IMACH(12) / -128 /                                           EILI1905
C     DATA IMACH(13) /  127 /                                           EILI1906
C     DATA IMACH(14) /   39 /                                           EILI1907
C     DATA IMACH(15) / -128 /                                           EILI1908
C     DATA IMACH(16) /  127 /                                           EILI1909
C                                                                       EILI1910
C     MACHINE CONSTANTS FOR THE HP 2100                                 EILI1911
C     4 WORD DOUBLE PRECISION OPTION WITH FTN4                          EILI1912
C                                                                       EILI1913
C     DATA IMACH( 1) /    5 /                                           EILI1914
C     DATA IMACH( 2) /    6 /                                           EILI1915
C     DATA IMACH( 3) /    4 /                                           EILI1916
C     DATA IMACH( 4) /    1 /                                           EILI1917
C     DATA IMACH( 5) /   16 /                                           EILI1918
C     DATA IMACH( 6) /    2 /                                           EILI1919
C     DATA IMACH( 7) /    2 /                                           EILI1920
C     DATA IMACH( 8) /   15 /                                           EILI1921
C     DATA IMACH( 9) / 32767 /                                          EILI1922
C     DATA IMACH(10) /    2 /                                           EILI1923
C     DATA IMACH(11) /   23 /                                           EILI1924
C     DATA IMACH(12) / -128 /                                           EILI1925
C     DATA IMACH(13) /  127 /                                           EILI1926
C     DATA IMACH(14) /   55 /                                           EILI1927
C     DATA IMACH(15) / -128 /                                           EILI1928
C     DATA IMACH(16) /  127 /                                           EILI1929
C                                                                       EILI1930
C     MACHINE CONSTANTS FOR THE HP 9000                                 EILI1931
C                                                                       EILI1932
C     DATA IMACH( 1) /     5 /                                          EILI1933
C     DATA IMACH( 2) /     6 /                                          EILI1934
C     DATA IMACH( 3) /     6 /                                          EILI1935
C     DATA IMACH( 4) /     7 /                                          EILI1936
C     DATA IMACH( 5) /    32 /                                          EILI1937
C     DATA IMACH( 6) /     4 /                                          EILI1938
C     DATA IMACH( 7) /     2 /                                          EILI1939
C     DATA IMACH( 8) /    32 /                                          EILI1940
C     DATA IMACH( 9) / 2147483647 /                                     EILI1941
C     DATA IMACH(10) /     2 /                                          EILI1942
C     DATA IMACH(11) /    24 /                                          EILI1943
C     DATA IMACH(12) /  -126 /                                          EILI1944
C     DATA IMACH(13) /   127 /                                          EILI1945
C     DATA IMACH(14) /    53 /                                          EILI1946
C     DATA IMACH(15) / -1015 /                                          EILI1947
C     DATA IMACH(16) /  1017 /                                          EILI1948
C                                                                       EILI1949
C     MACHINE CONSTANTS FOR THE IBM 360/370 SERIES,                     EILI1950
C     THE XEROX SIGMA 5/7/9, THE SEL SYSTEMS 85/86, AND                 EILI1951
C     THE PERKIN ELMER (INTERDATA) 7/32.                                EILI1952
C                                                                       EILI1953
C     DATA IMACH( 1) /   5 /                                            EILI1954
C     DATA IMACH( 2) /   6 /                                            EILI1955
C     DATA IMACH( 3) /   7 /                                            EILI1956
C     DATA IMACH( 4) /   6 /                                            EILI1957
C     DATA IMACH( 5) /  32 /                                            EILI1958
C     DATA IMACH( 6) /   4 /                                            EILI1959
C     DATA IMACH( 7) /  16 /                                            EILI1960
C     DATA IMACH( 8) /  31 /                                            EILI1961
C     DATA IMACH( 9) / Z7FFFFFFF /                                      EILI1962
C     DATA IMACH(10) /  16 /                                            EILI1963
C     DATA IMACH(11) /   6 /                                            EILI1964
C     DATA IMACH(12) / -64 /                                            EILI1965
C     DATA IMACH(13) /  63 /                                            EILI1966
C     DATA IMACH(14) /  14 /                                            EILI1967
C     DATA IMACH(15) / -64 /                                            EILI1968
C     DATA IMACH(16) /  63 /                                            EILI1969
C                                                                       EILI1970
C     MACHINE CONSTANTS FOR THE PDP-10 (KA PROCESSOR).                  EILI1971
C                                                                       EILI1972
C     DATA IMACH( 1) /    5 /                                           EILI1973
C     DATA IMACH( 2) /    6 /                                           EILI1974
C     DATA IMACH( 3) /    5 /                                           EILI1975
C     DATA IMACH( 4) /    6 /                                           EILI1976
C     DATA IMACH( 5) /   36 /                                           EILI1977
C     DATA IMACH( 6) /    5 /                                           EILI1978
C     DATA IMACH( 7) /    2 /                                           EILI1979
C     DATA IMACH( 8) /   35 /                                           EILI1980
C     DATA IMACH( 9) / "377777777777 /                                  EILI1981
C     DATA IMACH(10) /    2 /                                           EILI1982
C     DATA IMACH(11) /   27 /                                           EILI1983
C     DATA IMACH(12) / -128 /                                           EILI1984
C     DATA IMACH(13) /  127 /                                           EILI1985
C     DATA IMACH(14) /   54 /                                           EILI1986
C     DATA IMACH(15) / -101 /                                           EILI1987
C     DATA IMACH(16) /  127 /                                           EILI1988
C                                                                       EILI1989
C     MACHINE CONSTANTS FOR THE PDP-10 (KI PROCESSOR).                  EILI1990
C                                                                       EILI1991
C     DATA IMACH( 1) /    5 /                                           EILI1992
C     DATA IMACH( 2) /    6 /                                           EILI1993
C     DATA IMACH( 3) /    5 /                                           EILI1994
C     DATA IMACH( 4) /    6 /                                           EILI1995
C     DATA IMACH( 5) /   36 /                                           EILI1996
C     DATA IMACH( 6) /    5 /                                           EILI1997
C     DATA IMACH( 7) /    2 /                                           EILI1998
C     DATA IMACH( 8) /   35 /                                           EILI1999
C     DATA IMACH( 9) / "377777777777 /                                  EILI2000
C     DATA IMACH(10) /    2 /                                           EILI2001
C     DATA IMACH(11) /   27 /                                           EILI2002
C     DATA IMACH(12) / -128 /                                           EILI2003
C     DATA IMACH(13) /  127 /                                           EILI2004
C     DATA IMACH(14) /   62 /                                           EILI2005
C     DATA IMACH(15) / -128 /                                           EILI2006
C     DATA IMACH(16) /  127 /                                           EILI2007
C                                                                       EILI2008
C     MACHINE CONSTANTS FOR PDP-11 FORTRAN SUPPORTING                   EILI2009
C     32-BIT INTEGER ARITHMETIC.                                        EILI2010
C                                                                       EILI2011
C     DATA IMACH( 1) /    5 /                                           EILI2012
C     DATA IMACH( 2) /    6 /                                           EILI2013
C     DATA IMACH( 3) /    5 /                                           EILI2014
C     DATA IMACH( 4) /    6 /                                           EILI2015
C     DATA IMACH( 5) /   32 /                                           EILI2016
C     DATA IMACH( 6) /    4 /                                           EILI2017
C     DATA IMACH( 7) /    2 /                                           EILI2018
C     DATA IMACH( 8) /   31 /                                           EILI2019
C     DATA IMACH( 9) / 2147483647 /                                     EILI2020
C     DATA IMACH(10) /    2 /                                           EILI2021
C     DATA IMACH(11) /   24 /                                           EILI2022
C     DATA IMACH(12) / -127 /                                           EILI2023
C     DATA IMACH(13) /  127 /                                           EILI2024
C     DATA IMACH(14) /   56 /                                           EILI2025
C     DATA IMACH(15) / -127 /                                           EILI2026
C     DATA IMACH(16) /  127 /                                           EILI2027
C                                                                       EILI2028
C     MACHINE CONSTANTS FOR PDP-11 FORTRAN SUPPORTING                   EILI2029
C     16-BIT INTEGER ARITHMETIC.                                        EILI2030
C                                                                       EILI2031
C     DATA IMACH( 1) /    5 /                                           EILI2032
C     DATA IMACH( 2) /    6 /                                           EILI2033
C     DATA IMACH( 3) /    5 /                                           EILI2034
C     DATA IMACH( 4) /    6 /                                           EILI2035
C     DATA IMACH( 5) /   16 /                                           EILI2036
C     DATA IMACH( 6) /    2 /                                           EILI2037
C     DATA IMACH( 7) /    2 /                                           EILI2038
C     DATA IMACH( 8) /   15 /                                           EILI2039
C     DATA IMACH( 9) / 32767 /                                          EILI2040
C     DATA IMACH(10) /    2 /                                           EILI2041
C     DATA IMACH(11) /   24 /                                           EILI2042
C     DATA IMACH(12) / -127 /                                           EILI2043
C     DATA IMACH(13) /  127 /                                           EILI2044
C     DATA IMACH(14) /   56 /                                           EILI2045
C     DATA IMACH(15) / -127 /                                           EILI2046
C     DATA IMACH(16) /  127 /                                           EILI2047
C                                                                       EILI2048
C     MACHINE CONSTANTS FOR THE UNIVAC 1100 SERIES. FTN COMPILER        EILI2049
C                                                                       EILI2050
C     DATA IMACH( 1) /     5 /                                          EILI2051
C     DATA IMACH( 2) /     6 /                                          EILI2052
C     DATA IMACH( 3) /     1 /                                          EILI2053
C     DATA IMACH( 4) /     6 /                                          EILI2054
C     DATA IMACH( 5) /    36 /                                          EILI2055
C     DATA IMACH( 6) /     4 /                                          EILI2056
C     DATA IMACH( 7) /     2 /                                          EILI2057
C     DATA IMACH( 8) /    35 /                                          EILI2058
C     DATA IMACH( 9) / O377777777777 /                                  EILI2059
C     DATA IMACH(10) /     2 /                                          EILI2060
C     DATA IMACH(11) /    27 /                                          EILI2061
C     DATA IMACH(12) /  -128 /                                          EILI2062
C     DATA IMACH(13) /   127 /                                          EILI2063
C     DATA IMACH(14) /    60 /                                          EILI2064
C     DATA IMACH(15) / -1024 /                                          EILI2065
C     DATA IMACH(16) /  1023 /                                          EILI2066
C                                                                       EILI2067
C     MACHINE CONSTANTS FOR THE VAX 11/780                              EILI2068
C                                                                       EILI2069
C***** VAX                                                              EILI2070
C     DATA IMACH( 1) /    5 /                                           EILI2071
C     DATA IMACH( 2) /    6 /                                           EILI2072
C     DATA IMACH( 3) /    5 /                                           EILI2073
C     DATA IMACH( 4) /    6 /                                           EILI2074
C     DATA IMACH( 5) /   32 /                                           EILI2075
C     DATA IMACH( 6) /    4 /                                           EILI2076
C     DATA IMACH( 7) /    2 /                                           EILI2077
C     DATA IMACH( 8) /   31 /                                           EILI2078
C     DATA IMACH( 9) / 2147483647 /                                     EILI2079
C     DATA IMACH(10) /    2 /                                           EILI2080
C     DATA IMACH(11) /   24 /                                           EILI2081
C     DATA IMACH(12) / -127 /                                           EILI2082
C     DATA IMACH(13) /  127 /                                           EILI2083
C     DATA IMACH(14) /   56 /                                           EILI2084
C     DATA IMACH(15) / -127 /                                           EILI2085
C     DATA IMACH(16) /  127 /                                           EILI2086
C***** VAX                                                              EILI2087
C                                                                       EILI2088
C     MACHINE CONSTANTS FOR THE ELXSI 6400                              EILI2089
C                                                                       EILI2090
C     DATA IMACH( 1) /     5 /                                          EILI2091
C     DATA IMACH( 2) /     6 /                                          EILI2092
C     DATA IMACH( 3) /     6 /                                          EILI2093
C     DATA IMACH( 4) /     6 /                                          EILI2094
C     DATA IMACH( 5) /    32 /                                          EILI2095
C     DATA IMACH( 6) /     4 /                                          EILI2096
C     DATA IMACH( 7) /     2 /                                          EILI2097
C     DATA IMACH( 8) /    32 /                                          EILI2098
C     DATA IMACH( 9) / 2147483647 /                                     EILI2099
C     DATA IMACH(10) /     2 /                                          EILI2100
C     DATA IMACH(11) /    24 /                                          EILI2101
C     DATA IMACH(12) /  -126 /                                          EILI2102
C     DATA IMACH(13) /   127 /                                          EILI2103
C     DATA IMACH(14) /    53 /                                          EILI2104
C     DATA IMACH(15) / -1022 /                                          EILI2105
C     DATA IMACH(16) /  1023 /                                          EILI2106
C                                                                       EILI2107
C     MACHINE CONSTANTS FOR THE Z80 MICROPROCESSOR                      EILI2108
C                                                                       EILI2109
C     DATA IMACH( 1) /    1 /                                           EILI2110
C     DATA IMACH( 2) /    1 /                                           EILI2111
C     DATA IMACH( 3) /    0 /                                           EILI2112
C     DATA IMACH( 4) /    1 /                                           EILI2113
C     DATA IMACH( 5) /   16 /                                           EILI2114
C     DATA IMACH( 6) /    2 /                                           EILI2115
C     DATA IMACH( 7) /    2 /                                           EILI2116
C     DATA IMACH( 8) /   15 /                                           EILI2117
C     DATA IMACH( 9) / 32767 /                                          EILI2118
C     DATA IMACH(10) /    2 /                                           EILI2119
C     DATA IMACH(11) /   24 /                                           EILI2120
C     DATA IMACH(12) / -127 /                                           EILI2121
C     DATA IMACH(13) /  127 /                                           EILI2122
C     DATA IMACH(14) /   56 /                                           EILI2123
C     DATA IMACH(15) / -127 /                                           EILI2124
C     DATA IMACH(16) /  127 /                                           EILI2125
C                                                                       EILI2126
C     MACHINE CONSTANTS FOR THE IBM PC - MICROSOFT FORTRAN              EILI2127
C                                                                       EILI2128
C***** PCMICROSOFT                                                      EILI2129
C     DATA IMACH( 1) /     5 /                                          EILI2130
C     DATA IMACH( 2) /     6 /                                          EILI2131
C     DATA IMACH( 3) /     6 /                                          EILI2132
C     DATA IMACH( 4) /     0 /                                          EILI2133
C     DATA IMACH( 5) /    32 /                                          EILI2134
C     DATA IMACH( 6) /     4 /                                          EILI2135
C     DATA IMACH( 7) /     2 /                                          EILI2136
C     DATA IMACH( 8) /    31 /                                          EILI2137
C     DATA IMACH( 9) / 2147483647 /                                     EILI2138
C     DATA IMACH(10) /     2 /                                          EILI2139
C     DATA IMACH(11) /    24 /                                          EILI2140
C     DATA IMACH(12) /  -126 /                                          EILI2141
C     DATA IMACH(13) /   127 /                                          EILI2142
C     DATA IMACH(14) /    53 /                                          EILI2143
C     DATA IMACH(15) / -1022 /                                          EILI2144
C     DATA IMACH(16) /  1023 /                                          EILI2145
C***** PCMICROSOFT                                                      EILI2146
C                                                                       EILI2147
C     MACHINE CONSTANTS FOR THE IBM PC - PROFESSIONAL FORTRAN           EILI2148
C     AND LAHEY FORTRAN                                                 EILI2149
C                                                                       EILI2150
C***** PCNDP                                                            EILI2151
      DATA IMACH( 1) /     4 /                                          EILI2152
      DATA IMACH( 2) /     7 /                                          EILI2153
      DATA IMACH( 3) /     7 /                                          EILI2154
      DATA IMACH( 4) /     0 /                                          EILI2155
      DATA IMACH( 5) /    32 /                                          EILI2156
      DATA IMACH( 6) /     4 /                                          EILI2157
      DATA IMACH( 7) /     2 /                                          EILI2158
      DATA IMACH( 8) /    31 /                                          EILI2159
      DATA IMACH( 9) / 2147483647 /                                     EILI2160
      DATA IMACH(10) /     2 /                                          EILI2161
      DATA IMACH(11) /    24 /                                          EILI2162
      DATA IMACH(12) /  -126 /                                          EILI2163
      DATA IMACH(13) /   127 /                                          EILI2164
      DATA IMACH(14) /    53 /                                          EILI2165
      DATA IMACH(15) / -1022 /                                          EILI2166
      DATA IMACH(16) /  1023 /                                          EILI2167
C***** PCNDP                                                            EILI2168
C                                                                       EILI2169
C***FIRST EXECUTABLE STATEMENT  I1MACH                                  EILI2170
      IF (I .LT. 1  .OR.  I .GT. 16) GO TO 10                           EILI2171
C                                                                       EILI2172
      I1MACH=IMACH(I)                                                   EILI2173
      RETURN                                                            EILI2174
C                                                                       EILI2175
   10 CONTINUE                                                          EILI2176
      WRITE(OUTPUT,9000)                                                EILI2177
9000  FORMAT('1ERROR    1 IN I1MACH - I OUT OF BOUNDS ')                EILI2178
C                                                                       EILI2179
C     CALL FDUMP                                                        EILI2180
C                                                                       EILI2181
C                                                                       EILI2182
      STOP                                                              EILI2183
      END      
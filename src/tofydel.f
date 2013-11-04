      subroutine TOFYDEL(y, smu, sig, snu, n, k )
      integer iy, n, k
      double precision y(n), smu(n), sig(n), snu(n), dum, tofyn(k), sumT  
      do 100 i = 1, n
      iy = y(i)+1
      tofyn(1) = smu(i)*snu(i)+smu(i)*(1-snu(i))/(1+smu(i)*sig(i)*
     1           (1-snu(i))) 
      sumT = 0
      do 110 j = 2, iy 
      dum = 1+1/(smu(i)*sig(i)*(1-snu(i)))
      tofyn(j) =  (y(i)+smu(i)*snu(i)+(1/(sig(i)*(1-snu(i))))-
     1             (smu(i)*snu(i)*y(i))/tofyn(j-1))/dum
 110     sumT = sumT+log(tofyn(j-1))      
      y(i) = tofyn(iy)
 100  smu(i) = sumT      
      end

      subroutine TOFYSIN(y, smu, sig, snu, slbes, sc, n, k )
      integer iy, n, k
      double precision y(n), smu(n), sig(n), snu(n), slbes(n), 
     1    tofyn(k), sumT, salpha, sc(n) 
      do 100 i = 1, n
      iy = y(i)+1
      tofyn(1) = (smu(i)/sc(i))*((1+2*sig(i)*(smu(i)/sc(i)))**(-0.5))
     1 *exp(slbes(i)) 
      sumT = 0
      salpha = sqrt(1+2*sig(i)*(smu(i)/sc(i)))/sig(i)
      do 110 j = 2, iy 
      tofyn(j) =  (sc(i)*sig(i)*(2*(j-1+snu(i))/smu(i))+(1/tofyn(j-1)))
     1             *(smu(i)/(sig(i)*salpha*sc(i)))**2 
 110     sumT = sumT+log(tofyn(j-1))      
      y(i) = tofyn(iy)
 100  smu(i) = sumT      
      end

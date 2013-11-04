      subroutine TOFYS(y, smu, sig, snu, slbes, n, k )
      integer iy, n, k
      double precision y(n), smu(n), sig(n), snu(n), slbes(n),
     +  tofyn(k), sumT, salpha
      do 100 i = 1, n
      iy = y(i)+1
      tofyn(1) = smu(i)*((1+2*sig(i)*smu(i))**(-0.5))*exp(slbes(i)) 
      sumT = 0
      salpha = sqrt(1+2*sig(i)*smu(i))/sig(i)
      do 110 j = 2, iy 
      tofyn(j) =   (sig(i)*(2*(j-1+snu(i))/smu(i))+(1/tofyn(j-1)))
     1             *(smu(i)/(sig(i)*salpha))**2 
 110     sumT = sumT+log(tofyn(j-1))      
      y(i) = tofyn(iy)
 100  smu(i) = sumT      
      end

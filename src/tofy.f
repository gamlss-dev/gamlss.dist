      subroutine TOFY(y, smu, sig, n, k )
      integer iy, n, k
      double precision y(n), smu(n), sig(n),  tofyn(k), sumT 
      do 10 i = 1, n
      iy = y(i)+1
      tofyn(1) = smu(i)*((1+2*sig(i)*smu(i))**(-0.5))
      sumT = 0
      do 11 j = 2, iy 
      tofyn(j) =  ((sig(i)*(2*(j-1)-1)/smu(i))+(1/tofyn(j-1)))*
     1             (tofyn(1))**2
 11     sumT = sumT+log(tofyn(j-1))      
      y(i) = tofyn(iy)
 10     smu(i) = sumT      
      end

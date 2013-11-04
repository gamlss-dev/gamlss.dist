      subroutine CDFSICHEL(y, mu, sigma, nu, alpha, c, tynew1, 
     + lpnew1, n, k )
      integer iy, n, k
      double precision y(n), mu(n), sigma(n), nu(n), alpha(n), 
     +    tynew1(n), lpnew1(n), tynew(k), lpnew(k), cdf, c(n) 
      do 100 i = 1, n
      iy = y(i)+1 
       tynew(1) = tynew1(i)
       lpnew(1) = lpnew1(i)
       cdf = exp(lpnew(1))
       do 110 j = 2, iy 
      tynew(j) = (c(i)*sigma(i)*(2*(j-1+nu(i))/mu(i))+(1/
     + tynew(j-1)))*(mu(i)/(sigma(i)*alpha(i)*c(i)))**2
      lpnew(j) = lpnew(j-1) + log(tynew(j-1)) - log(real(j)-1)
      cdf = cdf+exp(lpnew(j))      
 110  continue  
       y(i) = cdf
 100  continue      
      end


 
  

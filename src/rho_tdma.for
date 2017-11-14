
***********************************************
*   tdma to solve the vapor density profile   *
***********************************************

	  subroutine rho_tdma(arhow,arhoe,arhop,arhos,
     *                      n,alpha1,beta1,rhov3)
	  implicit doubleprecision (a-h,o-z)
	  integer n
	  doubleprecision arhow(n),arhoe(n),arhop(n),arhos(n)
	  doubleprecision alpha1(n),beta1(n),rhov3(n)

	   alpha1(1)=arhoe(1)/arhop(1)
	   beta1(1)=arhos(1)/arhop(1)

	  do i=2,n-1
	
	   alpha1(i)=arhoe(i)/(arhop(i)-arhow(i)*alpha1(i-1))
	   beta1(i)=(arhos(i)+arhow(i)*beta1(i-1))/(arhop(i)-
     *               arhow(i)*alpha1(i-1))

	  end do

	   alpha1(n)=0
	   beta1(n)=(arhos(n)+arhow(n)*beta1(n-1))/(arhop(n)-
     *               arhow(n)*alpha1(n-1))

	   rhov3(n)=beta1(n)
	
	  do i=n-1,1,-1

	   rhov3(i)=alpha1(i)*rhov3(i+1)+beta1(i)

	  end do

      return
	  end    
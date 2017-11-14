*****************************************************
*          tdma for temperature profile             *
*****************************************************

	  subroutine temp_tdma(atw,ate,atp,ats,n,alpha,beta,temp3)
	  implicit doubleprecision (a-h,o-z)
	  integer n
	  doubleprecision atw(n),ate(n),atp(n),ats(n)
	  doubleprecision alpha(n),beta(n),temp3(n)

	   alpha(1)=ate(1)/atp(1)
	   beta(1)=ats(1)/atp(1)

	  do i=2,n-1

	   alpha(i)=ate(i)/(atp(i)-atw(i)*alpha(i-1))
	   beta(i)=(ats(i)+atw(i)*beta(i-1))/(atp(i)-atw(i)*
     *              alpha(i-1))

	  end do

	   alpha(n)=0
	   beta(n)=(ats(n)+atw(n)*beta(n-1))/(atp(n)-atw(n)*
     *              alpha(n-1))

	   temp3(n)=beta(n)

	  do i=n-1,1,-1
	
           temp3(i)=alpha(i)*temp3(i+1)+beta(i)

	  end do

	  return
	  end
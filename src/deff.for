*******************************************
*  calculation of the deffe coefficient   *
*******************************************

	subroutine deff(deffe,deff2,deffw,n,
     *					  ratio_p,ratio_e,ratio_q,ratio_w)

	implicit doubleprecision (a-h,o-z)

	integer n
	doubleprecision deff2(n),deffe(n),deffw(n)
	doubleprecision ratio_p,ratio_q,ratio_w,ratio_e

	   deffe(1)=deff2(1)*deff2(2)/(ratio_p*deff2(2)+ratio_e*
     *              deff2(1))
	

	do i=2,n-2
		deffe(i)=2*deff2(i)*deff2(i+1)/(deff2(i)+deff2(i+1))
	end do
	   
	deffe(n-1)=deff2(n)*deff2(n-1)/(ratio_w*deff2(n)+ratio_q*
     *           deff2(n-1))
	deffe(n)=0

	  deffw(1)=0

	  do i=2,n
	  deffw(i)=deffe(i-1)
	  end do

	return
	end   
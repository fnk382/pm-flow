********************************************
*  calculation of the keff coefficient	 *
********************************************

	subroutine keff(keffe,keff2,n,ratio_p,ratio_e,
     *						ratio_w,ratio_q,keffw)

	implicit doubleprecision (a-h,o-z)

	integer n
	doubleprecision keff2(n),keffe(n),keffw(n)
	doubleprecision ratio_p,ratio_q,ratio_e,ratio_w
 

	
c		space average conductivity

	   keffe(1)=keff2(1)*keff2(2)/(ratio_p*keff2(2)+ratio_e*
     *              keff2(1))

	do i=2,n-2
  
	   keffe(i)=2*keff2(i)*keff2(i+1)/(keff2(i)+keff2(i+1))

	end do
	   keffe(n-1)=keff2(n-1)*keff2(n)/(ratio_q*keff2(n-1)+
     *                ratio_w*keff2(n))
	   keffe(n)=0
	
       keffw(1)=0

	do i=2,n

	   keffw(i)=keffe(i-1)

	end do

	  return
	  end
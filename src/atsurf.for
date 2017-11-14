******************************************************
*    calculation of the surface grid area coefficient*
******************************************************

	  subroutine atsurf(rhot1,cpt1,spec1,temp1,dx,dt,mdot2,had2,
     *                     ds,k_air,k_potash_0,n,h_air,temp_amb,ddx,
     *                     temp_p,delta,ats)

	  implicit doubleprecision (a-h,o-z)
	  integer n
	  doubleprecision rhot1(n),cpt1(n),temp1(n),dx(n),spec1(n)
	  doubleprecision mdot2(n),had2(n),ats(n)
	  doubleprecision dt,h_air,temp_amb,ddx,temp_p,delta
	  doubleprecision k_air,k_potash_0

	   ats(1)=(ds+ddx)*spec1(1)*temp1(1)/dt+h_air*temp_amb-
     *	          mdot2(1)*had2(1)*(ddx+ds)

	  do i=2,n-1
	
	   ats(i)=rhot1(i)*cpt1(i)*temp1(i)*dx(i)/dt-mdot2(i)*had2(i)
     *            *dx(i) 

	  end do

	   ats(n)=rhot1(n)*cpt1(n)*temp1(n)*ddx/dt+k_potash_0/delta*
     *            temp_p-2*mdot2(n)*ddx*had2(n)
            

	  return
	  end
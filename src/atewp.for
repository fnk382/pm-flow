*************************************************************
*    calculation of the coefficient east and west face areas*
*************************************************************

	subroutine atewp(keffe,keffw,n,dxe,dxw,ate,atw,atp,
     *					  ddx,dx,dt,rhot2,cpt2,spec2,h_air,ds,
     *						delta,k_air,k_potash_0)

	implicit doubleprecision (a-h,o-z)

	integer n
	doubleprecision keffe(n),dxe(n)
	doubleprecision keffw(n),dxw(n)
	doubleprecision ate(n),atw(n),atp(n)
 	doubleprecision rhot2(n),cpt2(n),dx(n),spec2(n)
	doubleprecision dt,h_air,ds,delta,k_air,ddx,k_potash_0


	ate(1)=keffe(1)/dxe(1)	

	do i=2,n-2
	   ate(i)=keffe(i)/dxe(i)
	end do
	   ate(n-1)=keffe(n-1)/dxe(n-1)
	   ate(n)=0

	atw(1)=0
	atw(2)=keffw(2)/dxw(2)	

	do i=3,n-1
	   atw(i)=keffw(i)/dxw(i)
	end do
	atw(n)=keffw(n)/dxw(n)

	   atp(1)=ate(1)+h_air+(ds+ddx)*spec2(1)/dt

	do i=2,n-1
	   atp(i)=ate(i)+atw(i)+rhot2(i)*cpt2(i)*dx(i)/dt
	end do

	   atp(n)=atw(n)+rhot2(n)*cpt2(n)*ddx/dt+k_potash_0/delta

		return
		end
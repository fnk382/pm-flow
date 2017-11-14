*************************************************************
*   calculation of the mass of vapor phase change in the    *
*						middle nodes			          *
*************************************************************

	subroutine mass_middle(deffe,deffw,rhov3_i,rhov3_j,rhov3_k,
     *                         epg3,epg1,rhov1,dt,dx,dxw,dxe,mdot3)

	implicit doubleprecision (a-h,o-z)
	doubleprecision deffe,deffw,rhov3_i,rhov3_j,rhov3_k
	doubleprecision epg3,epg1,rhov1,mdot3
	doubleprecision dt,dx,dxw,dxe

	mdot3=(epg3*dx/dt+deffe/dxe+deffw/dxw)*(rhov3_j/dx)-(deffe/dxe)
     *        *(rhov3_k/dx)-(deffw/dxw)*(rhov3_i/dx)-epg1*rhov1/dt

	return
	end